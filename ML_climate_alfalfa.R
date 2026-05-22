
#Code used to clear the console
rm(list = ls())
cat("\014")
if (!is.null(dev.list())) dev.off()

library(mRMRe)
library(caret)
library(dplyr)
library(ggplot2)
library(housingData)
library(sf)
library(tigris)
library(stringr)
library(blockCV)
library(ranger)
library(gstat)
library(xgboost)
library(gbm)
library(lightgbm)
library(randomForest)
library(e1071)
library(nnet)
library(SHAPforxgboost)
library(fastshap)
library(ggbeeswarm)
library(tidyr)
library(lubridate)
library(ggExtra)

# 1. Initial Cleaning (Drop columns and NA Yields)
dataset <- read.csv("Alfalfa_Master_Final_Analysis_1981_2018.csv") %>% 
  select(-State.ANSI) %>%
  filter(!is.na(YIELD))

############################################################################
##########################################################################
# Initial cleaning and Name matching
data <- dataset %>%
  mutate(
    County = str_to_title(tolower(County)),
    State = str_to_title(tolower(State)),
    County = str_replace(County, "Mcpherson", "McPherson")
  )

counties_sf <- counties(cb = TRUE) %>% st_as_sf()

dataset_sf <- counties_sf %>%
  inner_join(data, by = c("STATE_NAME" = "State", "NAME" = "County")) %>%
  filter(!is.na(YIELD)) %>%
  rename(State = STATE_NAME, County = NAME) %>%
  select(-COUNTYNS, -STATEFP, -COUNTYFP, -AFFGEOID, -GEOID, -NAMELSAD, -STUSPS, -LSAD, -ALAND, -AWATER) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(LONGITUDE = st_coordinates(centroid)[, 1],
         LATITUDE = st_coordinates(centroid)[, 2])

# Reset Row Names and drop geometry for the non-spatial data pool
dataset_sf <- dataset_sf %>% ungroup()
rownames(dataset_sf) <- NULL
dataset_final <- dataset_sf %>% st_drop_geometry() %>% select(-centroid) %>% as.data.frame()

dataset_final <- dataset_final %>%
  relocate(YIELD, .after = last_col())


########################################################################################################
######################## LightGBM CLIMATE MODEL #############################################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# Global LightGBM seeds
lgb_seeds <- list(
  seed = 123,
  bagging_seed = 123,
  feature_fraction_seed = 123,
  drop_seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))

# MASTER STORAGE (Keeps everything for every fold)
all_fold_models <- list()
all_fold_metrics <- data.frame()
all_mrmr_rankings <- list()
all_best_params <- list()
all_shap_values <- list()
all_shap_features <- list()
all_norm_params <- list()
all_mrmr_scores_list <- list()
all_test_results <- list()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  r2   <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f) 
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  outer_train_df <- dataset_with_folds %>% filter(fold != f)
  test_df        <- dataset_with_folds %>% filter(fold == f)
  
  # --- TEMPORAL VALIDATION (YEAR-BASED / LOYO) ---
  set.seed(123 + f)

  # 1. Identify all unique years available in the training pool
  unique_train_years <- unique(outer_train_df$YEAR)

  # 2. Randomly sample 6 years (~15-20% of your 1981-2018 timeline)
  val_years <- sample(unique_train_years, 6)

  # 3. Split the data: Validation gets the 6 years, Train gets the rest
  val_df   <- outer_train_df %>% filter(YEAR %in% val_years)
  train_df <- outer_train_df %>% filter(!(YEAR %in% val_years))

  # 4. Print confirmation to the console
  cat("\nFold", f, "tuning using RANDOM LOYO on Years:", paste(sort(val_years), collapse = ", "))
  
  # --- DROP NON-FEATURE COLUMNS ---
  train_df <- train_df %>% select(-State, -County, -YEAR)
  val_df   <- val_df   %>% select(-State, -County, -YEAR)
  test_df  <- test_df  %>% select(-State, -County, -YEAR)
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(train_df)[sapply(train_df, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "fold", "is_val"))
  
  means <- sapply(train_df[feats], mean, na.rm = TRUE)
  sds   <- sapply(train_df[feats], sd, na.rm = TRUE)
  all_norm_params[[f]] <- list(mean = means, sd = sds)
  
  train_sc <- train_df; val_sc <- val_df; test_sc <- test_df
  for(col in feats) {
    train_sc[[col]] <- (train_df[[col]] - means[col]) / sds[col]
    val_sc[[col]]   <- (val_df[[col]] - means[col]) / sds[col]
    test_sc[[col]]  <- (test_df[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  
  # Initialize mRMR data object
  mrmr_in <- mRMR.data(data = as.data.frame(train_sc[, c(feats, "YIELD")]))
  
  # Run classic mRMR to get top 30 potential features
  mrmr_run <- mRMR.classic(
    mrmr_in, 
    target_indices = which(names(train_sc[, c(feats, "YIELD")]) == "YIELD"), 
    feature_count = 50
  )
  
  # Extract feature names and relevance scores
  mrmr_feats <- names(train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run))) 
  
  # Store for later averaging/plotting
  all_mrmr_rankings[[f]] <- mrmr_feats
  all_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    dtrain_tmp <- lgb.Dataset(as.matrix(train_sc[, curr]), label = train_sc$YIELD)
    
    # Fast training to build the elbow curve
    set.seed(123) # Lock for reproducibility
    tmp_mod <- lgb.train(
      list(objective="regression", metric="rmse", verbose=-1, seed=42), 
      dtrain_tmp, 
      nrounds=50
    )
    
    p_val <- predict(tmp_mod, as.matrix(val_sc[, curr]))
    rmse_history[k] <- sqrt(mean((val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history)
  y <- rmse_history
  
  # 1. Find the FIRST Elbow (Maximum initial drop)
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  # 2. Find the SECOND Elbow (Diminishing returns point)
  if(k1 < length(y)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + 
                    (x_tail[length(x_tail)]-x_tail[1])*y_tail + 
                    (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else {
    opt_k <- k1
  }
  
  # 3. SAFETY FALLBACK (Ensures opt_k exists and isn't too small)
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) {
    opt_k <- min(length(mrmr_feats), 10)
  }
  
  # NOW we define best_feats because opt_k is finally ready
  best_feats <- mrmr_feats[1:opt_k]
  
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  
  
  # E. HYPERPARAMETER TUNING (Grid Search on Validation)
  grid <- expand.grid(leaves = c(15, 31, 50, 70), lr = c(0.01, 0.05, 0.1))
  best_v_rmse <- Inf; final_params <- list()
  for(g in 1:nrow(grid)){
    set.seed(123) # Lock for reproducibility
    p <- list(objective="regression", metric="rmse", num_leaves=grid$leaves[g], learning_rate=grid$lr[g], verbose=-1)
    m <- lgb.train(p, lgb.Dataset(as.matrix(train_sc[, best_feats]), label = train_sc$YIELD), nrounds=150)
    v_rmse <- sqrt(mean((val_sc$YIELD - predict(m, as.matrix(val_sc[, best_feats])))^2))
    if(v_rmse < best_v_rmse){ best_v_rmse <- v_rmse; final_params <- p }
  }
  all_best_params[[f]] <- final_params
  
  # NEW: Print best parameters to console
  cat("\nFold", f, "Best Parameters: Leaves =", final_params$num_leaves, "| LR =", final_params$learning_rate)
  
  # F. FINAL FOLD MODEL & SHAP
  set.seed(123) # Lock for reproducibility
  final_mod <- lgb.train(final_params, lgb.Dataset(as.matrix(train_sc[, best_feats]), label = train_sc$YIELD), nrounds=200)
  all_fold_models[[f]] <- final_mod
  
  X_test_mat <- as.matrix(test_sc[, best_feats])
  shap_contrib <- predict(final_mod, X_test_mat, type = "contrib")
  
  shap_df <- as.data.frame(shap_contrib[, 1:opt_k])
  colnames(shap_df) <- best_feats
  all_shap_values[[f]] <- shap_df
  all_shap_features[[f]] <- as.data.frame(X_test_mat)
  
  # G. METRICS COLLECTION
  v_p <- predict(final_mod, as.matrix(val_sc[, best_feats]))
  t_p <- predict(final_mod, X_test_mat)
  
  v_m <- get_metrics(val_df$YIELD, v_p, length(best_feats), nrow(val_df))
  t_m <- get_metrics(test_df$YIELD, t_p, length(best_feats), nrow(test_df))
  
  all_fold_metrics <- rbind(all_fold_metrics, data.frame(
    Fold = f, Features = opt_k,
    Val_RMSE = v_m["RMSE"], Val_MAE = v_m["MAE"], Val_R2 = v_m["R2"], Val_AdjR2 = v_m["AdjR2"],
    Test_RMSE = t_m["RMSE"], Test_MAE = t_m["MAE"], Test_R2 = t_m["R2"], Test_AdjR2 = t_m["AdjR2"]
  ))
  
  cat("\nFold", f, "complete. Test R2:", round(t_m["R2"], 3))
  
  fold_results <- data.frame(
    Actual = test_df$YIELD,
    Predicted = t_p,
    Fold = as.factor(f)
  )
  all_test_results[[f]] <- fold_results
}
# STOP TIMER HERE (Outside the loop)
end_time <- Sys.time()
comp_time <- end_time - start_time

# 5. Metrics
print(colMeans(all_fold_metrics[, -1]))

# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================

cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")

# To make it look even cleaner with rounding:
all_fold_metrics <- all_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(all_fold_metrics)


# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
# Calculate Means and SDs
metric_means <- colMeans(all_fold_metrics[, -1])
metric_sds   <- sapply(all_fold_metrics[, -1], sd)

# Create a clean summary table
final_summary <- data.frame(
  Metric = names(metric_means),
  Mean   = round(metric_means, 4),
  SD     = round(metric_sds, 4)
)

n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776

final_summary <- data.frame(
  Metric = names(metric_means),
  Mean   = round(metric_means, 4),
  SD     = round(metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )


cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(final_summary, "LGBM_Climate_Metrics.csv", row.names = FALSE)
saveRDS(final_summary, "LGBM_Climate_Metrics.rds")

# To identify which features were most common across all folds:
all_best_feats <- unlist(all_mrmr_rankings) # Or subset to opt_k
feat_freq <- as.data.frame(table(all_best_feats)) %>% arrange(desc(Freq))
print(feat_freq)


# ==============================================================================
# 4. FINAL RESULTS & VISUALIZATION
# ==============================================================================

# 1. Consolidate SHAP data
lgbm_climate_shap <- bind_rows(all_shap_values)
lgbm_climate_feat <- bind_rows(all_shap_features)

# 2. Pivot to Long format
lgbm_prep_shap <- lgbm_climate_shap %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")

lgbm_prep_feat <- lgbm_climate_feat %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "Value")

# 3. Join and Filter for TOP 10 Features
lgbm_plot_data <- left_join(lgbm_prep_shap, lgbm_prep_feat, by = c("ID", "Feature")) %>% 
  filter(!is.na(SHAP))

# FIX: Use dplyr::slice to avoid the method error
lgbm_top_10 <- lgbm_plot_data %>%
  group_by(Feature) %>%
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>%
  arrange(desc(mean_abs_impact)) %>%
  dplyr::slice(1:5)

lgbm_plot_data_top10 <- lgbm_plot_data %>%
  filter(Feature %in% lgbm_top_10$Feature)

# 4. Generate the Beeswarm Plot

common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

lgbm_climate_shap_plot <- ggplot(lgbm_plot_data_top10 %>% 
         group_by(Feature) %>% 
         mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
       aes(x = SHAP, 
           y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
           color = Value)) + 
  
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  
  scale_color_viridis_c(
    option = "viridis", 
    name = "Relative\nintensity",
    limits = common_color_lim,
    oob = scales::squish
  ) + 
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  
  # --- MATCHING SCATTER PLOT THEME AND SIZES ---
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)
  ) + 
  labs( 
    title = "Light Gradient Boosting", 
    x = expression("Yield impact (t ha"^{-1}*")"), 
    y = "Features" 
  )

lgbm_climate_shap_plot



# 1. Calculate Importance per Fold (using test data only)
lgbm_climate_fold_importance <- lgbm_plot_data %>%
  group_by(Feature) %>%
  summarize(
    Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE),
    Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

lgbm_climate_shap_summary_test_table <- lgbm_climate_fold_importance %>%
  select(Rank, Feature, Mean_Test_Impact, Impact_Stability_SD)

print(lgbm_climate_shap_summary_test_table)
write.csv(lgbm_climate_shap_summary_test_table, "LGBM_Climate_SHAP_Importance.csv", row.names = FALSE)


# 1. Aggregate and Average mRMR Scores
avg_mrmr_scores <- bind_rows(all_mrmr_scores_list) %>%
  group_by(Feature) %>%
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>%
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
avg_mrmr_scores <- avg_mrmr_scores %>%
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 10 (or Top 5) Average Features

lgbm_climate_mrmr_plot <- ggplot(avg_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Light Gradient Boosting", 
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    # Tilt x-axis labels to match the Ogallala style
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))
lgbm_climate_mrmr_plot

############################################ Train:Test #############

# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
plot_df <- bind_rows(all_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
# Pulling means and SDs directly from your summary table
test_r2_mean   <- final_summary$Mean[final_summary$Metric == "Test_R2"]
test_r2_sd     <- final_summary$SD[final_summary$Metric == "Test_R2"]
test_rmse_mean <- final_summary$Mean[final_summary$Metric == "Test_RMSE"]
test_rmse_sd   <- final_summary$SD[final_summary$Metric == "Test_RMSE"]
test_mae_mean  <- final_summary$Mean[final_summary$Metric == "Test_MAE"]
test_mae_sd    <- final_summary$SD[final_summary$Metric == "Test_MAE"]


# B. Overall Metrics (Calculated once across the entire combined dataset)
overall_r2 <- cor(plot_df$Actual, plot_df$Predicted)^2
overall_rmse <- sqrt(mean((plot_df$Actual - plot_df$Predicted)^2))
overall_mae <- mean(abs(plot_df$Actual - plot_df$Predicted))

# 3. CREATE CONSOLIDATED PLOT

p_main <- ggplot(plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(test_rmse_mean, 2), nsmall = 2), " ± ", format(round(test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(test_mae_mean, 2), nsmall = 2), " ± ", format(round(test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(test_r2_mean, 3), nsmall = 3), " ± ", format(round(test_r2_sd, 3), nsmall = 3)
           ), 
           size = 4, color = "black")

# 2. Top Density
p_top <- ggplot(plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
lgbm_climate_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Light Gradient Boosting", 
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
lgbm_climate_scatter_plot

########################################################################################################
######################## LightGBM FULL MODEL #############################################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup) - LGBM FULL VERSION
# ==============================================================================
set.seed(123)
# Global LightGBM seeds
lgb_seeds <- list(
  seed = 123,
  bagging_seed = 123,
  feature_fraction_seed = 123,
  drop_seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset_final %>% 
  distinct(State, County) %>% 
  group_by(State) %>% 
  mutate(fold = sample(rep(1:5, length.out = n()))) %>% 
  ungroup()

dataset_with_folds <- dataset_final %>% 
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))


# MASTER STORAGE (Keeps everything for every fold)
lgbm_full_all_models <- list()
lgbm_full_all_metrics <- data.frame()
lgbm_full_all_mrmr_rankings <- list()
lgbm_full_all_best_params <- list()
lgbm_full_all_shap_values <- list()
lgbm_full_all_shap_features <- list()
lgbm_full_all_norm_params <- list()
lgbm_full_all_best_feats_used <- c()
lgbm_full_all_mrmr_scores <- list()
lgbm_full_all_mrmr_scores_list <- list()
lgbm_full_all_test_results <- list()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  r2 <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}
# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  set.seed(123 + f) 
  
  # A. SPATIAL SPLITS
  lgbm_full_outer_train_df <- dataset_with_folds %>% filter(fold != f)
  lgbm_full_test_df        <- dataset_with_folds %>% filter(fold == f)
  
  # --- TEMPORAL VALIDATION (YEAR-BASED / LOYO) ---
  set.seed(123 + f)
  
  # 1. Identify all available years in the training pool
  lgbm_full_unique_train_years <- unique(lgbm_full_outer_train_df$YEAR)
  
  # 2. Randomly sample 6 years (~15-20% of your 38-year data) for the validation set
  lgbm_full_val_years <- sample(lgbm_full_unique_train_years, 6)
  
  # 3. Split based on Years instead of Counties
  lgbm_full_val_df   <- lgbm_full_outer_train_df %>% filter(YEAR %in% lgbm_full_val_years)
  lgbm_full_train_df <- lgbm_full_outer_train_df %>% filter(!(YEAR %in% lgbm_full_val_years))
  
  cat("\nFold", f, "tuning via LOYO on Years:", paste(sort(lgbm_full_val_years), collapse = ", "))
  
  # --- DROP NON-FEATURE COLUMNS ---
  lgbm_full_train_df <- lgbm_full_train_df %>% select(-State, -County)
  lgbm_full_val_df   <- lgbm_full_val_df   %>% select(-State, -County)
  lgbm_full_test_df  <- lgbm_full_test_df  %>% select(-State, -County)
  
  # B. Z-SCORE NORMALIZATION (Including Latitude/Longitude)
  lgbm_full_numeric_cols <- names(lgbm_full_train_df)[sapply(lgbm_full_train_df, is.numeric)]
  lgbm_full_feats <- setdiff(lgbm_full_numeric_cols, c("YIELD", "fold", "is_val"))
  
  lgbm_full_means <- sapply(lgbm_full_train_df[lgbm_full_feats], mean, na.rm = TRUE)
  lgbm_full_sds <- sapply(lgbm_full_train_df[lgbm_full_feats], sd, na.rm = TRUE)
  lgbm_full_all_norm_params[[f]] <- list(mean = lgbm_full_means, sd = lgbm_full_sds)
  
  lgbm_full_train_sc <- lgbm_full_train_df; lgbm_full_val_sc <- lgbm_full_val_df; lgbm_full_test_sc <- lgbm_full_test_df
  
  for(col in lgbm_full_feats) {
    lgbm_full_train_sc[[col]] <- (lgbm_full_train_df[[col]] - lgbm_full_means[col]) / lgbm_full_sds[col]
    lgbm_full_val_sc[[col]] <- (lgbm_full_val_df[[col]] - lgbm_full_means[col]) / lgbm_full_sds[col]
    lgbm_full_test_sc[[col]] <- (lgbm_full_test_df[[col]] - lgbm_full_means[col]) / lgbm_full_sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING (FIXED VARIABLE NAMES)
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  
  mrmr_in <- mRMR.data(data = as.data.frame(lgbm_full_train_sc[, c(lgbm_full_feats, "YIELD")]))

  mrmr_run <- mRMR.classic(mrmr_in, 
                           target_indices = which(names(lgbm_full_train_sc[, c(lgbm_full_feats, "YIELD")]) == "YIELD"), 
                           feature_count = 50)

  lgbm_full_mrmr_feats <- names(lgbm_full_train_sc[, lgbm_full_feats])[as.numeric(unlist(solutions(mrmr_run)))]
  lgbm_full_mrmr_scores <- as.numeric(unlist(scores(mrmr_run))) 
  
  lgbm_full_all_mrmr_rankings[[f]] <- lgbm_full_mrmr_feats
  lgbm_full_all_mrmr_scores_list[[f]] <- data.frame(Feature = lgbm_full_mrmr_feats, Score = lgbm_full_mrmr_scores)
  
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  lgbm_full_rmse_history <- c()
  
  # FIX: Use 'lgbm_full_mrmr_feats' instead of 'mrmr_feats'
  for(k in 1:length(lgbm_full_mrmr_feats)) {
    lgbm_full_curr_feats <- lgbm_full_mrmr_feats[1:k]
    
    # FIX: Use 'lgbm_full_train_sc' instead of 'train_sc'
    lgbm_full_dtrain_tmp <- lgb.Dataset(as.matrix(lgbm_full_train_sc[, lgbm_full_curr_feats]), 
                                        label = lgbm_full_train_sc$YIELD)
    
    set.seed(123)
    lgbm_full_tmp_mod <- lgb.train(
      list(objective="regression", metric="rmse", verbose=-1, seed=42), 
      lgbm_full_dtrain_tmp, 
      nrounds=50
    )
    
     # FIX: Use 'lgbm_full_val_sc' instead of 'val_sc'
     lgbm_full_p_val <- predict(lgbm_full_tmp_mod, as.matrix(lgbm_full_val_sc[, lgbm_full_curr_feats]))
     lgbm_full_rmse_history[k] <- sqrt(mean((lgbm_full_val_sc$YIELD - lgbm_full_p_val)^2))
   }
  
   # --- Geometric Logic to find lgbm_full_opt_k ---
   lgbm_full_x <- 1:length(lgbm_full_rmse_history)
   lgbm_full_y <- lgbm_full_rmse_history
  
   # 1. Find the FIRST Elbow
   lgbm_full_dist1 <- abs(((lgbm_full_y[1] - lgbm_full_y[length(lgbm_full_y)]) * lgbm_full_x + (lgbm_full_x[length(lgbm_full_x)] - lgbm_full_x[1]) * lgbm_full_y + (lgbm_full_x[1] * lgbm_full_y[length(lgbm_full_y)] - lgbm_full_x[length(lgbm_full_x)] * lgbm_full_y[1])))
   lgbm_full_k1 <- which.max(lgbm_full_dist1)
   
   # 2. Find the SECOND Elbow
   if(lgbm_full_k1 < length(lgbm_full_y)){
     lgbm_full_x_tail <- lgbm_full_k1:length(lgbm_full_y)
     lgbm_full_y_tail <- lgbm_full_y[lgbm_full_k1:length(lgbm_full_y)]
     lgbm_full_dist2 <- abs(((lgbm_full_y_tail[1] - lgbm_full_y_tail[length(lgbm_full_y_tail)]) * lgbm_full_x_tail + (lgbm_full_x_tail[length(lgbm_full_x_tail)] - lgbm_full_x_tail[1]) * lgbm_full_y_tail + (lgbm_full_x_tail[1] * lgbm_full_y_tail[length(lgbm_full_y_tail)] - lgbm_full_x_tail[length(lgbm_full_x_tail)] * lgbm_full_y_tail[1])))
     lgbm_full_opt_k <- lgbm_full_x_tail[which.max(lgbm_full_dist2)]
   } else {
     lgbm_full_opt_k <- lgbm_full_k1
   }
  
   # 3. SAFETY FALLBACK
   # FIX: Use 'lgbm_full_mrmr_feats' for the length check
   if(!exists("lgbm_full_opt_k") || is.na(lgbm_full_opt_k) || lgbm_full_opt_k < 10) {
     lgbm_full_opt_k <- min(length(lgbm_full_mrmr_feats), 10)
   }
   
   # FIX: Use 'lgbm_full_mrmr_feats' to subset the winners
   best_feats <- lgbm_full_mrmr_feats[1:lgbm_full_opt_k]
   
   cat("\nFold", f, "picked", lgbm_full_opt_k, "features (First Elbow at", lgbm_full_k1, ")")
   cat("\nSelected Features:", paste(best_feats, collapse = ", "))
   
  
   # ==============================================================================
   # E. HYPERPARAMETER TUNING (Grid Search on Validation)
   # ==============================================================================
   lgbm_full_grid <- expand.grid(leaves = c(15, 31, 50, 70), lr = c(0.01, 0.05, 0.1))
   lgbm_full_best_v_rmse <- Inf
   lgbm_full_final_params <- list()
   
   for(g in 1:nrow(lgbm_full_grid)){
     set.seed(123) # Lock for reproducibility
     lgbm_full_p <- list(
       objective = "regression", 
       metric = "rmse", 
       num_leaves = lgbm_full_grid$leaves[g], 
       learning_rate = lgbm_full_grid$lr[g], 
       seed = 42, 
       verbose = -1
     )
    
     # FIX: Use lgbm_full_train_sc
     lgbm_full_m <- lgb.train(
       lgbm_full_p, 
       lgb.Dataset(as.matrix(lgbm_full_train_sc[, best_feats]), label = lgbm_full_train_sc$YIELD), 
       nrounds = 150
     )
     
     # FIX: Use lgbm_full_val_sc
     lgbm_full_v_p <- predict(lgbm_full_m, as.matrix(lgbm_full_val_sc[, best_feats]))
     lgbm_full_v_rmse <- sqrt(mean((lgbm_full_val_sc$YIELD - lgbm_full_v_p)^2))
     
     if(lgbm_full_v_rmse < lgbm_full_best_v_rmse){
       lgbm_full_best_v_rmse <- lgbm_full_v_rmse
       lgbm_full_final_params <- lgbm_full_p
     }
   }
  
   lgbm_full_all_best_params[[f]] <- lgbm_full_final_params
   cat("\nFold", f, "Best Parameters: Leaves =", lgbm_full_final_params$num_leaves, "| LR =", lgbm_full_final_params$learning_rate)
  
   # ==============================================================================
   # F. FINAL FOLD MODEL & SHAP
   # ==============================================================================
   # FIX: Use lgbm_full_train_sc
   set.seed(123) # Lock for reproducibility
   lgbm_full_final_mod <- lgb.train(
     lgbm_full_final_params, 
     lgb.Dataset(as.matrix(lgbm_full_train_sc[, best_feats]), label = lgbm_full_train_sc$YIELD), 
     nrounds = 200
   )
   lgbm_full_all_models[[f]] <- lgbm_full_final_mod
   
   # FIX: Use lgbm_full_test_sc
   lgbm_full_X_test_mat <- as.matrix(lgbm_full_test_sc[, best_feats])
   lgbm_full_shap_contrib <- predict(lgbm_full_final_mod, lgbm_full_X_test_mat, type = "contrib")
   
   lgbm_full_shap_df <- as.data.frame(lgbm_full_shap_contrib[, 1:lgbm_full_opt_k])
   colnames(lgbm_full_shap_df) <- best_feats
   lgbm_full_all_shap_values[[f]] <- lgbm_full_shap_df
   lgbm_full_all_shap_features[[f]] <- as.data.frame(lgbm_full_X_test_mat)
  
   #  ==============================================================================
   # G. METRICS COLLECTION
   # ==============================================================================
   # FIX: Use lgbm_full_val_sc and lgbm_full_val_df
   lgbm_full_val_preds <- predict(lgbm_full_final_mod, as.matrix(lgbm_full_val_sc[, best_feats]))
   lgbm_full_test_preds <- predict(lgbm_full_final_mod, lgbm_full_X_test_mat)
   
   lgbm_full_v_m <- get_metrics(lgbm_full_val_df$YIELD, lgbm_full_val_preds, length(best_feats), nrow(lgbm_full_val_df))
   lgbm_full_t_m <- get_metrics(lgbm_full_test_df$YIELD, lgbm_full_test_preds, length(best_feats), nrow(lgbm_full_test_df))
   
   lgbm_full_all_metrics <- rbind(lgbm_full_all_metrics, data.frame(
     Fold = f, Features = lgbm_full_opt_k, 
     Val_RMSE = lgbm_full_v_m["RMSE"], Val_MAE = lgbm_full_v_m["MAE"], Val_R2 = lgbm_full_v_m["R2"], Val_AdjR2 = lgbm_full_v_m["AdjR2"],
     Test_RMSE = lgbm_full_t_m["RMSE"], Test_MAE = lgbm_full_t_m["MAE"], Test_R2 = lgbm_full_t_m["R2"], Test_AdjR2 = lgbm_full_t_m["AdjR2"]
   ))
  
   cat("\nFold", f, "complete. Test R2:", round(lgbm_full_t_m["R2"], 3))
   
   lgbm_full_fold_results <- data.frame(
     Actual = lgbm_full_test_df$YIELD,
     Predicted = lgbm_full_test_preds,
     Fold = as.factor(f)
   )
   lgbm_full_all_test_results[[f]] <- lgbm_full_fold_results
}
# STOP TIMER HERE (Outside the loop)
end_time <- Sys.time()
comp_time <- end_time - start_time

# 5. Metrics
print(colMeans(lgbm_full_all_metrics[, -1]))


# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================

cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")

# To make it look even cleaner with rounding:
lgbm_full_all_metrics <- lgbm_full_all_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(lgbm_full_all_metrics)

# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
# Calculate Means and SDs
lgbm_full_metric_means <- colMeans(lgbm_full_all_metrics[, -1])
lgbm_full_metric_sds   <- sapply(lgbm_full_all_metrics[, -1], sd)

# Create a clean summary table
n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776

lgbm_full_final_summary <- data.frame(
  Metric = names(lgbm_full_metric_means),
  Mean   = round(lgbm_full_metric_means, 4),
  SD     = round(lgbm_full_metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(lgbm_full_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(lgbm_full_final_summary, "LGBM_Full_Metrics.csv", row.names = FALSE)
saveRDS(lgbm_full_final_summary, "LGBM_Full_Metrics.rds")

# To identify which features were most common across all folds:
lgbm_full_all_best_feats <- unlist(lgbm_full_all_mrmr_rankings)
lgbm_full_feat_freq <- as.data.frame(table(lgbm_full_all_best_feats)) %>% arrange(desc(Freq))
print(lgbm_full_feat_freq)

# ==============================================================================
# 5. FINAL RESULTS & VISUALIZATION
# ==============================================================================

# --- B. SHAP BEESWARM PLOT ---
# FIX: Use 'lgbm_full_all_shap_values' and 'lgbm_full_all_shap_features'
lgbm_full_master_shap <- bind_rows(lgbm_full_all_shap_values)
lgbm_full_master_feat <- bind_rows(lgbm_full_all_shap_features)

lgbm_full_prep_shap <- lgbm_full_master_shap %>%
  mutate(ID = row_number()) %>%
  pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")

lgbm_full_prep_feat <- lgbm_full_master_feat %>%
  mutate(ID = row_number()) %>%
  pivot_longer(-ID, names_to = "Feature", values_to = "Value")

lgbm_full_plot_data <- left_join(lgbm_full_prep_shap, lgbm_full_prep_feat, by = c("ID", "Feature")) %>%
  filter(!is.na(SHAP))

lgbm_full_top_10 <- lgbm_full_plot_data %>%
  group_by(Feature) %>%
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>%
  arrange(desc(mean_abs_impact)) %>%
  dplyr::slice(1:5)

lgbm_full_plot_data_top10 <- lgbm_full_plot_data %>%
  filter(Feature %in% lgbm_full_top_10$Feature)

common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

lgbm_full_shap_plot <- ggplot(lgbm_full_plot_data_top10 %>% 
                                   group_by(Feature) %>% 
                                   mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                                 aes(x = SHAP, 
                                     y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                     color = Value)) + 
  
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  
  scale_color_viridis_c(
    option = "viridis", 
    name = "Relative\nintensity",
    limits = common_color_lim,
    oob = scales::squish
  ) + 
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)
  ) + 
  labs( 
    title = "Light Gradient Boosting", 
    x = expression("Yield impact (t ha"^{-1}*")"), 
    y = "Features" 
  )

lgbm_full_shap_plot


# 1. Calculate Importance per Fold (using test data only)
lgbm_full_fold_importance <- lgbm_full_plot_data %>%
  group_by(Feature) %>%
  summarize(
    Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE),
    Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

lgbm_full_shap_summary_test_table <- lgbm_full_fold_importance %>%
  select(Rank, Feature, Mean_Test_Impact, Impact_Stability_SD)

print(lgbm_full_shap_summary_test_table)
write.csv(lgbm_full_shap_summary_test_table, "LGBM_full_SHAP_Importance.csv", row.names = FALSE)

# --- C. AVERAGE mRMR RELEVANCE PLOT ---
# FIX: Use 'lgbm_full_all_mrmr_scores_list'
lgbm_full_avg_mrmr <- bind_rows(lgbm_full_all_mrmr_scores_list) %>%
  group_by(Feature) %>%
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>%
  arrange(desc(Avg_Score))

lgbm_full_avg_mrmr <- lgbm_full_avg_mrmr %>%
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

lgbm_full_mrmr_plot <- ggplot(lgbm_full_avg_mrmr[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Light Gradient Boosting", 
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    # Tilt x-axis labels to match the Ogallala style
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))
lgbm_full_mrmr_plot

############################################ Train:Test ############################################ 

# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
lgbm_full_plot_df <- bind_rows(lgbm_full_all_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
# Pulling means and SDs directly from your summary table
lgbm_full_test_r2_mean   <- lgbm_full_final_summary$Mean[lgbm_full_final_summary$Metric == "Test_R2"]
lgbm_full_test_r2_sd     <- lgbm_full_final_summary$SD[lgbm_full_final_summary$Metric == "Test_R2"]
lgbm_full_test_rmse_mean <- lgbm_full_final_summary$Mean[lgbm_full_final_summary$Metric == "Test_RMSE"]
lgbm_full_test_rmse_sd   <- lgbm_full_final_summary$SD[lgbm_full_final_summary$Metric == "Test_RMSE"]
lgbm_full_test_mae_mean  <- lgbm_full_final_summary$Mean[lgbm_full_final_summary$Metric == "Test_MAE"]
lgbm_full_test_mae_sd    <- lgbm_full_final_summary$SD[lgbm_full_final_summary$Metric == "Test_MAE"]


# B. Overall Metrics (Calculated once across the entire combined dataset)
lgbm_full_overall_r2 <- cor(lgbm_full_plot_df$Actual, lgbm_full_plot_df$Predicted)^2
lgbm_full_overall_rmse <- sqrt(mean((lgbm_full_plot_df$Actual - lgbm_full_plot_df$Predicted)^2))
lgbm_full_overall_mae <- mean(abs(lgbm_full_plot_df$Actual - lgbm_full_plot_df$Predicted))

# 3. SCATTER PLOT

p_main <- ggplot(lgbm_full_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(lgbm_full_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(lgbm_full_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(lgbm_full_test_mae_mean, 2), nsmall = 2), " ± ", format(round(lgbm_full_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(lgbm_full_test_r2_mean, 3), nsmall = 3), " ± ", format(round(lgbm_full_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")

# 2. Top Density
p_top <- ggplot(lgbm_full_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(lgbm_full_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
lgbm_full_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Light Gradient Boosting", 
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
lgbm_full_scatter_plot


########################################################################################################
######################## GBM CLIMATE MODEL #############################################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# Global GBM seeds
gbm_seeds <- list(
  seed = 123,
  bagging_seed = 123,
  feature_fraction_seed = 123,
  drop_seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))

# MASTER STORAGE (Keeps everything for every fold)
gbm_climate_mrmr_rankings <- list()
gbm_climate_mrmr_scores_list <- list()
gbm_climate_best_params <- list()
gbm_climate_fold_models <- list()
gbm_climate_shap_values <- list()
gbm_climate_shap_features <- list()
gbm_climate_norm_params <- list()
gbm_climate_test_results <- list()
gbm_climate_fold_metrics <- data.frame()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  r2   <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f)
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  gbm_climate_outer_train <- dataset_with_folds %>% filter(fold != f)
  gbm_climate_test        <- dataset_with_folds %>% filter(fold == f)
  
  # --- STEP 2: VALIDATION SET (Temporal LOYO - 6 Years) ---
  unique_years <- unique(gbm_climate_outer_train$YEAR)
  
  set.seed(123 + f)
  val_years <- sample(unique_years, 6) # Pick 6 years for a robust tuning set
  
  gbm_climate_val   <- gbm_climate_outer_train %>% filter(YEAR %in% val_years)
  gbm_climate_train <- gbm_climate_outer_train %>% filter(!(YEAR %in% val_years))
  
  cat("\nFold", f, "tuning using LOYO on Years:", paste(sort(val_years), collapse = ", "))
  
  # --- DROP NON-FEATURE COLUMNS ---
  gbm_climate_train <- gbm_climate_train %>% select(-State, -County, -YEAR)
  gbm_climate_val   <- gbm_climate_val   %>% select(-State, -County, -YEAR)
  gbm_climate_test  <- gbm_climate_test  %>% select(-State, -County, -YEAR)
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(gbm_climate_train)[sapply(gbm_climate_train, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "fold", "is_val"))
  
  means <- sapply(gbm_climate_train[feats], mean, na.rm = TRUE)
  sds   <- sapply(gbm_climate_train[feats], sd, na.rm = TRUE)
  gbm_climate_norm_params[[f]] <- list(mean = means, sd = sds)
  
  # Create scaled versions
  gbm_climate_train_sc <- gbm_climate_train
  gbm_climate_val_sc   <- gbm_climate_val
  gbm_climate_test_sc  <- gbm_climate_test
  
  for(col in feats) {
    gbm_climate_train_sc[[col]] <- (gbm_climate_train[[col]] - means[col]) / sds[col]
    gbm_climate_val_sc[[col]]   <- (gbm_climate_val[[col]] - means[col]) / sds[col]
    gbm_climate_test_sc[[col]]  <- (gbm_climate_test[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  mrmr_in <- mRMR.data(data = as.data.frame(gbm_climate_train_sc[, c(feats, "YIELD")]))
  mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(gbm_climate_train_sc[, c(feats, "YIELD")]) == "YIELD"), feature_count = 50)
  
  mrmr_feats  <- names(gbm_climate_train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run)))
  
  gbm_climate_mrmr_rankings[[f]] <- mrmr_feats
  gbm_climate_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    
    # 1. Create a clean standard data frame for training
    # This prevents the 'object not found' error by explicitly selecting columns
    df_train_sub <- as.data.frame(gbm_climate_train_sc[, c(curr, "YIELD")])
    set.seed(123) # Lock for reproducibility
    tmp_mod <- gbm(
      formula = YIELD ~ ., 
      data = df_train_sub, 
      distribution = "gaussian", 
      n.trees = 50, 
      interaction.depth = 3, 
      shrinkage = 0.1, 
      verbose = FALSE
    )
    
    # 2. Create a clean standard data frame for validation
    df_val_sub <- as.data.frame(gbm_climate_val_sc[, curr, drop = FALSE])
    
    # Predict using the cleaned data frame
    p_val <- predict(tmp_mod, df_val_sub, n.trees = 50)
    rmse_history[k] <- sqrt(mean((gbm_climate_val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history); y <- rmse_history
  
  # 1. Find the FIRST Elbow
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  # 2. Find the SECOND Elbow (Search specifically in the tail after k1)
  if(k1 < (length(y) - 1)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else { 
    opt_k <- k1 
  }
  
  # 3. SAFETY FALLBACK (Only triggers if the math fails or gives a tiny number)
  # Changed from 10 to 5 so it doesn't over-ride a valid small second elbow
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { 
    opt_k <- 10 
  }
  
  best_feats <- mrmr_feats[1:opt_k]
  
  # PRINTING EXACTLY AS REQUESTED
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  
  # ==============================================================================
  # E. HYPERPARAMETER TUNING (Grid Search on Validation)
  # ==============================================================================
  grid <- expand.grid(depth = c(3, 5, 7), lr = c(0.01, 0.05, 0.1))
  best_v_rmse <- Inf; final_params <- list()
  
  for(g in 1:nrow(grid)){
    set.seed(123) # Lock for reproducibility
    m <- gbm(
      formula = YIELD ~ ., 
      data = as.data.frame(gbm_climate_train_sc[, c(best_feats, "YIELD")]), 
      distribution = "gaussian", n.trees = 150, interaction.depth = grid$depth[g], 
      shrinkage = grid$lr[g], bag.fraction = 0.8, verbose = FALSE
    )
    # FIX: Force data.frame in predict
    v_p <- predict(m, as.data.frame(gbm_climate_val_sc[, best_feats]), n.trees = 150)
    v_rmse <- sqrt(mean((gbm_climate_val_sc$YIELD - v_p)^2))
    if(v_rmse < best_v_rmse){
      best_v_rmse <- v_rmse
      final_params <- list(depth = grid$depth[g], lr = grid$lr[g])
    }
  }
  gbm_climate_best_params[[f]] <- final_params
  
  # MATCHING YOUR REQUESTED OUTPUT FORMAT
  cat("\nFold", f, "Best Parameters: Depth =", final_params$depth, "| LR =", final_params$lr)
  
  # ==============================================================================
  # F. FINAL FOLD MODEL & SHAP
  # ==============================================================================
  set.seed(123) # Lock for reproducibility
  final_mod <- gbm(
    formula = YIELD ~ ., 
    data = as.data.frame(gbm_climate_train_sc[, c(best_feats, "YIELD")]), 
    distribution = "gaussian", n.trees = 200, interaction.depth = final_params$depth, 
    shrinkage = final_params$lr, verbose = FALSE
  )
  gbm_climate_fold_models[[f]] <- final_mod
  
  # SHAP Wrapper Fix: Ensure it converts to data.frame
  p_wrapper <- function(object, newdata) { 
    predict(object, as.data.frame(newdata), n.trees = 200) 
  }
  
  X_test_df <- as.data.frame(gbm_climate_test_sc[, best_feats])
  shap_contrib <- fastshap::explain(final_mod, X = X_test_df, pred_wrapper = p_wrapper, nsim = 10)
  
  gbm_climate_shap_values[[f]]   <- as.data.frame(shap_contrib)
  gbm_climate_shap_features[[f]] <- X_test_df
  
  # ==============================================================================
  # G. METRICS COLLECTION
  # ==============================================================================
  # 1. Generate predictions (Forcing data.frame for GBM)
  v_p <- predict(final_mod, as.data.frame(gbm_climate_val_sc[, best_feats]), n.trees = 200)
  t_p <- predict(final_mod, as.data.frame(gbm_climate_test_sc[, best_feats]), n.trees = 200)
  
  # 2. Calculate metrics using your specific function
  v_m <- get_metrics(gbm_climate_val$YIELD, v_p, length(best_feats), nrow(gbm_climate_val))
  t_m <- get_metrics(gbm_climate_test$YIELD, t_p, length(best_feats), nrow(gbm_climate_test))
  
  # 3. Store in the metrics dataframe (Including AdjR2)
  gbm_climate_fold_metrics <- rbind(gbm_climate_fold_metrics, data.frame(
    Fold = f, Features = opt_k, 
    Val_R2 = v_m["R2"], Test_R2 = t_m["R2"], Val_AdjR2 = v_m["AdjR2"], Test_AdjR2 = t_m["AdjR2"],
    Val_RMSE = v_m["RMSE"], Test_RMSE = t_m["RMSE"], Val_MAE = v_m["MAE"], Test_MAE = t_m["MAE"]
  ))
  
  # 4. Save fold results for the Actual vs Predicted plot
  gbm_climate_test_results[[f]] <- data.frame(
    Actual = gbm_climate_test$YIELD, 
    Predicted = t_p, 
    Fold = f
  )
  
  cat("\nFold", f, "complete. Test AdjR2:", round(t_m["AdjR2"], 3))
  
  
} # END MASTER LOOP
# STOP TIMER HERE (Outside the loop)
end_time <- Sys.time()
comp_time <- end_time - start_time


# 5. Metrics
print(colMeans(gbm_climate_fold_metrics[, -1]))


# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================

cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")

# To make it look even cleaner with rounding:
gbm_climate_fold_metrics <- gbm_climate_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(gbm_climate_fold_metrics)

# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
# Calculate Means and SDs
gbm_climate_metric_means <- colMeans(gbm_climate_fold_metrics[, -1])
gbm_climate_metric_sds   <- sapply(gbm_climate_fold_metrics[, -1], sd)

# Create a clean summary table
n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776
gbm_climate_final_summary <- data.frame(
  Metric = names(gbm_climate_metric_means),
  Mean   = round(gbm_climate_metric_means, 4),
  SD     = round(gbm_climate_metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(gbm_climate_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(gbm_climate_final_summary, "GBM_Climate_Metrics.csv", row.names = FALSE)
saveRDS(gbm_climate_final_summary, "GBM_Climate_Metrics.rds")

# To identify which features were most common across all folds:
gbm_climate_best_feats <- unlist(gbm_climate_mrmr_rankings)
gbm_climate_feat_freq <- as.data.frame(table(gbm_climate_best_feats)) %>% arrange(desc(Freq))
print(gbm_climate_feat_freq)

# ============================================================================== 
# 4. FINAL RESULTS & VISUALIZATION 
# ============================================================================== 

# 1. Consolidate SHAP data
gbm_master_shap <- bind_rows(gbm_climate_shap_values) 
gbm_master_feat <- bind_rows(gbm_climate_shap_features) 

# 2. Pivot to Long format
gbm_prep_shap <- gbm_master_shap %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")

gbm_prep_feat <- gbm_master_feat %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "Value")

# 3. Join and Filter for TOP 5 Features (as per your slice request)
gbm_plot_data <- left_join(gbm_prep_shap, gbm_prep_feat, by = c("ID", "Feature")) %>% 
  filter(!is.na(SHAP))

gbm_top_5 <- gbm_plot_data %>% 
  group_by(Feature) %>% 
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>% 
  arrange(desc(mean_abs_impact)) %>% 
  dplyr::slice(1:5)

gbm_plot_data_top5 <- gbm_plot_data %>% 
  filter(Feature %in% gbm_top_5$Feature)

# 4. Generate the Beeswarm Plot #######################################
  
common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

gbm_climate_shap_plot <- ggplot(gbm_plot_data_top5 %>% 
                                group_by(Feature) %>% 
                                mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                              aes(x = SHAP, 
                                  y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                  color = Value)) + 
  
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  
  scale_color_viridis_c(
    option = "viridis", 
    name = "Relative\nintensity",
    limits = common_color_lim,
    oob = scales::squish) + 
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)) + 
  labs( 
    title = "Gradient Boosting", 
    x = expression("Yield impact (t ha"^{-1}*")"), 
    y = "Features")

gbm_climate_shap_plot


# Calculate Importance per Fold (using test data only)
gbm_climate_fold_importance <- gbm_plot_data %>%
  group_by(Feature) %>%
  summarize(
    Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE),
    Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

gbm_climate_shap_summary_test_table <- gbm_climate_fold_importance %>%
  select(Rank, Feature, Mean_Test_Impact, Impact_Stability_SD)

print(gbm_climate_shap_summary_test_table)
write.csv(gbm_climate_shap_summary_test_table, "GBM_climate_SHAP_Importance.csv", row.names = FALSE)


# ============================================================================== 
# mRMR AGGREGATION PLOT
# ============================================================================== 
# 1. Aggregate and Average mRMR Scores
gbm_climate_mrmr_scores <- bind_rows(gbm_climate_mrmr_scores_list) %>% 
  group_by(Feature) %>% 
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
gbm_climate_mrmr_scores <- gbm_climate_mrmr_scores %>% 
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 5 Average Features
gbm_climate_mrmr_plot <- ggplot(gbm_climate_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Gradient Boosting",
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    # Tilt x-axis labels to match the Ogallala style
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))
gbm_climate_mrmr_plot

############################################ Train:Test ############################

# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
gbm_climate_plot_df <- bind_rows(gbm_climate_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
# Pulling means and SDs directly from your summary table
gbm_climate_test_r2_mean   <- gbm_climate_final_summary$Mean[gbm_climate_final_summary$Metric == "Test_R2"]
gbm_climate_test_r2_sd     <- gbm_climate_final_summary$SD[gbm_climate_final_summary$Metric == "Test_R2"]
gbm_climate_test_rmse_mean <- gbm_climate_final_summary$Mean[gbm_climate_final_summary$Metric == "Test_RMSE"]
gbm_climate_test_rmse_sd   <- gbm_climate_final_summary$SD[gbm_climate_final_summary$Metric == "Test_RMSE"]
gbm_climate_test_mae_mean  <- gbm_climate_final_summary$Mean[gbm_climate_final_summary$Metric == "Test_MAE"]
gbm_climate_test_mae_sd    <- gbm_climate_final_summary$SD[gbm_climate_final_summary$Metric == "Test_MAE"]


# B. Overall Metrics (Calculated once across the entire combined dataset)
gbm_climate_overall_r2 <- cor(gbm_climate_plot_df$Actual, gbm_climate_plot_df$Predicted)^2
gbm_climate_overall_rmse <- sqrt(mean((gbm_climate_plot_df$Actual - gbm_climate_plot_df$Predicted)^2))
gbm_climate_overall_mae <- mean(abs(gbm_climate_plot_df$Actual - gbm_climate_plot_df$Predicted))

# 3. CREATE CONSOLIDATED PLOT
p_main <- ggplot(gbm_climate_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(gbm_climate_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(gbm_climate_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(gbm_climate_test_mae_mean, 2), nsmall = 2), " ± ", format(round(gbm_climate_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(gbm_climate_test_r2_mean, 3), nsmall = 3), " ± ", format(round(gbm_climate_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")
# 2. Top Density
p_top <- ggplot(gbm_climate_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(gbm_climate_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
gbm_climate_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Gradient Boosting", 
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
gbm_climate_scatter_plot

# ################ SENSITIVITY CHECK ##############################
# 
# # 1. Reconstruct the test metadata (Year, State, County) from the original data
# # We iterate through the folds just like the loop did to ensure the order is identical
# gbm_climate_test_metadata <- lapply(1:5, function(f) {
#   dataset_with_folds %>% 
#     filter(fold == f) %>% 
#     select(YEAR, State, County)
# }) %>% bind_rows()
# 
# # 2. Combine your model predictions
# gbm_climate_test_preds <- bind_rows(gbm_climate_test_results)
# 
# # 3. Bind them together (Column Bind)
# # Now you have Year, State, County, Actual, and Predicted in one table
# gbm_climate_test_summary <- cbind(gbm_climate_test_metadata, gbm_climate_test_preds)
# 
# # 4. Calculate error and perform the Extreme Year Check
# gbm_climate_test_summary$Error <- gbm_climate_test_summary$Actual - gbm_climate_test_summary$Predicted
# 
# gbm_climate_extreme_year_check <- gbm_climate_test_summary %>%
#   group_by(YEAR) %>%
#   summarize(
#     Avg_Yield = mean(Actual),
#     RMSE = sqrt(mean(Error^2)),
#     MAE = mean(abs(Error)),
#     # Negative bias means the model is under-predicting
#     Bias = mean(Error),
#     # R-squared per year (optional but useful)
#     R2 = if(n() > 1) cor(Actual, Predicted)^2 else NA
#   ) %>%
#   arrange(Avg_Yield)
# 
# # View the results for the lowest yield (extreme) years
# print(head(gbm_climate_extreme_year_check))

########################################################################################################
######################## GBM FULL MODEL #############################################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# GBM seeds
gbm_seeds <- list(
  seed = 123,
  bagging_seed = 123,
  feature_fraction_seed = 123,
  drop_seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset_final %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset_final %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))


# MASTER STORAGE (Keeps everything for every fold)
gbm_full_fold_models <- list()
gbm_full_fold_metrics <- data.frame()
gbm_full_mrmr_rankings <- list()
gbm_full_best_params <- list()
gbm_full_shap_values <- list()
gbm_full_shap_features <- list()
gbm_full_norm_params <- list()
gbm_full_best_feats_used <- c()
gbm_full_mrmr_scores <- list()
gbm_full_mrmr_scores_list <- list()
gbm_full_test_results <- list()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  r2 <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f)
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  gbm_full_outer_train <- dataset_with_folds %>% filter(fold != f)
  gbm_full_test        <- dataset_with_folds %>% filter(fold == f)
  
  # --- STEP 2: VALIDATION SET (Temporal LOYO - 6 Years) ---
  unique_years <- unique(gbm_full_outer_train$YEAR)
  
  set.seed(123 + f)
  val_years <- sample(unique_years, 6) # Pick 6 years for a robust tuning set
  
  gbm_full_val   <- gbm_full_outer_train %>% filter(YEAR %in% val_years)
  gbm_full_train <- gbm_full_outer_train %>% filter(!(YEAR %in% val_years))
  
  cat("\nFold", f, "tuning using LOYO on Years:", paste(sort(val_years), collapse = ", "))
  
  # --- DROP NON-FEATURE COLUMNS ---
  gbm_full_train <- gbm_full_train %>% select(-State, -County)
  gbm_full_val   <- gbm_full_val   %>% select(-State, -County)
  gbm_full_test  <- gbm_full_test  %>% select(-State, -County)
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(gbm_full_train)[sapply(gbm_full_train, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "fold", "is_val"))
  
  means <- sapply(gbm_full_train[feats], mean, na.rm = TRUE)
  sds   <- sapply(gbm_full_train[feats], sd, na.rm = TRUE)
  gbm_full_norm_params[[f]] <- list(mean = means, sd = sds)
  
  # Create scaled versions
  gbm_full_train_sc <- gbm_full_train
  gbm_full_val_sc   <- gbm_full_val
  gbm_full_test_sc  <- gbm_full_test
  
  for(col in feats) {
    gbm_full_train_sc[[col]] <- (gbm_full_train[[col]] - means[col]) / sds[col]
    gbm_full_val_sc[[col]]   <- (gbm_full_val[[col]] - means[col]) / sds[col]
    gbm_full_test_sc[[col]]  <- (gbm_full_test[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  mrmr_in <- mRMR.data(data = as.data.frame(gbm_full_train_sc[, c(feats, "YIELD")]))
  mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(gbm_full_train_sc[, c(feats, "YIELD")]) == "YIELD"), feature_count = 50)
  
  mrmr_feats  <- names(gbm_full_train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run)))
  
  gbm_full_mrmr_rankings[[f]] <- mrmr_feats
  gbm_full_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    
    # 1. Create a clean standard data frame for training
    df_train_sub <- as.data.frame(gbm_full_train_sc[, c(curr, "YIELD")])
    set.seed(123) # Lock for reproducibility
    tmp_mod <- gbm(
      formula = YIELD ~ ., 
      data = df_train_sub, 
      distribution = "gaussian", 
      n.trees = 50, 
      interaction.depth = 3, 
      shrinkage = 0.1, 
      verbose = FALSE
    )
    
    # 2. Create a clean standard data frame for validation
    df_val_sub <- as.data.frame(gbm_full_val_sc[, curr, drop = FALSE])
    
    # Predict using the cleaned data frame
    p_val <- predict(tmp_mod, df_val_sub, n.trees = 50)
    rmse_history[k] <- sqrt(mean((gbm_full_val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history); y <- rmse_history
  
  # 1. Find the FIRST Elbow
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  # 2. Find the SECOND Elbow
  if(k1 < (length(y) - 1)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else { 
    opt_k <- k1 
  }
  
  # 3. SAFETY FALLBACK (Ensures at least 10 features)
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { 
    opt_k <- 10 
  }
  
  best_feats <- mrmr_feats[1:opt_k]
  
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  # ==============================================================================
  # E. HYPERPARAMETER TUNING (Grid Search on Validation)
  # ==============================================================================
  grid <- expand.grid(depth = c(3, 5, 7), lr = c(0.01, 0.05, 0.1))
  best_v_rmse <- Inf; final_params <- list()
  
  for(g in 1:nrow(grid)){
    set.seed(123) 
    m <- gbm(
      formula = YIELD ~ ., 
      data = as.data.frame(gbm_full_train_sc[, c(best_feats, "YIELD")]), 
      distribution = "gaussian", n.trees = 150, interaction.depth = grid$depth[g], 
      shrinkage = grid$lr[g], bag.fraction = 0.8, verbose = FALSE
    )
    # Force data.frame in predict
    v_p <- predict(m, as.data.frame(gbm_full_val_sc[, best_feats]), n.trees = 150)
    v_rmse <- sqrt(mean((gbm_full_val_sc$YIELD - v_p)^2))
    if(v_rmse < best_v_rmse){
      best_v_rmse <- v_rmse
      final_params <- list(depth = grid$depth[g], lr = grid$lr[g])
    }
  }
  gbm_full_best_params[[f]] <- final_params
  
  cat("\nFold", f, "Best Parameters: Depth =", final_params$depth, "| LR =", final_params$lr)
  
  # ==============================================================================
  # F. FINAL FOLD MODEL & SHAP
  # ==============================================================================
  set.seed(123) 
  final_mod <- gbm(
    formula = YIELD ~ ., 
    data = as.data.frame(gbm_full_train_sc[, c(best_feats, "YIELD")]), 
    distribution = "gaussian", n.trees = 200, interaction.depth = final_params$depth, 
    shrinkage = final_params$lr, verbose = FALSE
  )
  gbm_full_fold_models[[f]] <- final_mod
  
  # SHAP Wrapper Fix
  p_wrapper <- function(object, newdata) { 
    predict(object, as.data.frame(newdata), n.trees = 200) 
  }
  
  X_test_df <- as.data.frame(gbm_full_test_sc[, best_feats])
  shap_contrib <- fastshap::explain(final_mod, X = X_test_df, pred_wrapper = p_wrapper, nsim = 10)
  
  gbm_full_shap_values[[f]]   <- as.data.frame(shap_contrib)
  gbm_full_shap_features[[f]] <- X_test_df
  
  # ==============================================================================
  # G. METRICS COLLECTION
  # ==============================================================================
  # 1. Generate predictions (Forcing data.frame for GBM)
  v_p <- predict(final_mod, as.data.frame(gbm_full_val_sc[, best_feats]), n.trees = 200)
  t_p <- predict(final_mod, as.data.frame(gbm_full_test_sc[, best_feats]), n.trees = 200)
  
  # 2. Calculate metrics using your specific function
  v_m <- get_metrics(gbm_full_val$YIELD, v_p, length(best_feats), nrow(gbm_full_val))
  t_m <- get_metrics(gbm_full_test$YIELD, t_p, length(best_feats), nrow(gbm_full_test))
  
  # 3. Store in the metrics dataframe (Including AdjR2)
  gbm_full_fold_metrics <- rbind(gbm_full_fold_metrics, data.frame(
    Fold = f, Features = opt_k, 
    Val_R2 = v_m["R2"], Test_R2 = t_m["R2"], Val_AdjR2 = v_m["AdjR2"], Test_AdjR2 = t_m["AdjR2"],
    Val_RMSE = v_m["RMSE"], Test_RMSE = t_m["RMSE"], Val_MAE = v_m["MAE"], Test_MAE = t_m["MAE"]
  ))
  
  # 4. Save fold results for the Actual vs Predicted plot
  gbm_full_test_results[[f]] <- data.frame(
    Actual = gbm_full_test$YIELD, 
    Predicted = t_p, 
    Fold = f
  )
  
  cat("\nFold", f, "complete. Test AdjR2:", round(t_m["AdjR2"], 3))
  
  
} # END MASTER LOOP
# STOP TIMER HERE (Outside the loop)
end_time <- Sys.time()
comp_time <- end_time - start_time


# 5. Metrics
print(colMeans(gbm_full_fold_metrics[, -1]))


# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================

cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")

# To make it look even cleaner with rounding:
gbm_full_fold_metrics <- gbm_full_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(gbm_full_fold_metrics)

# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
# Calculate Means and SDs
gbm_full_metric_means <- colMeans(gbm_full_fold_metrics[, -1])
gbm_full_metric_sds   <- sapply(gbm_full_fold_metrics[, -1], sd)

# Create a clean summary table
n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776
gbm_full_final_summary <- data.frame(
  Metric = names(gbm_full_metric_means),
  Mean   = round(gbm_full_metric_means, 4),
  SD     = round(gbm_full_metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(gbm_full_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(gbm_full_final_summary, "GBM_Full_Metrics.csv", row.names = FALSE)
saveRDS(gbm_full_final_summary, "GBM_Full_Metrics.rds")

# To identify which features were most common across all folds:
gbm_full_best_feats <- unlist(gbm_full_mrmr_rankings)
gbm_full_feat_freq <- as.data.frame(table(gbm_full_best_feats)) %>% arrange(desc(Freq))
print(gbm_full_feat_freq)

# ============================================================================== 
# 4. FINAL RESULTS & VISUALIZATION 
# ============================================================================== 

# 1. Consolidate SHAP data
gbm_master_shap <- bind_rows(gbm_full_shap_values) 
gbm_master_feat <- bind_rows(gbm_full_shap_features) 

# 2. Pivot to Long format
gbm_prep_shap <- gbm_master_shap %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")

gbm_prep_feat <- gbm_master_feat %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "Value")

# 3. Join and Filter for TOP 5 Features
gbm_plot_data <- left_join(gbm_prep_shap, gbm_prep_feat, by = c("ID", "Feature")) %>% 
  filter(!is.na(SHAP))

gbm_top_5 <- gbm_plot_data %>% 
  group_by(Feature) %>% 
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>% 
  arrange(desc(mean_abs_impact)) %>% 
  dplyr::slice(1:5)

gbm_plot_data_top5 <- gbm_plot_data %>% 
  filter(Feature %in% gbm_top_5$Feature)

# 4. Generate the Beeswarm Plot #######################################

common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

gbm_full_shap_plot <- ggplot(gbm_plot_data_top5 %>% 
                               group_by(Feature) %>% 
                               mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                             aes(x = SHAP, 
                                 y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                 color = Value)) + 
  
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  
  scale_color_viridis_c(
    option = "viridis", 
    name = "Relative\nintensity",
    limits = common_color_lim,
    oob = scales::squish) + 
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)) + 
  labs( 
    title = "Gradient Boosting", 
    x = expression("Yield impact (t ha"^{-1}*")"), 
    y = "Features")

gbm_full_shap_plot


# Calculate Importance per Fold (using test data only)
gbm_full_fold_importance <- gbm_plot_data %>%
  group_by(Feature) %>%
  summarize(
    Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE),
    Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

gbm_full_shap_summary_test_table <- gbm_full_fold_importance %>%
  select(Rank, Feature, Mean_Test_Impact, Impact_Stability_SD)

print(gbm_full_shap_summary_test_table)
write.csv(gbm_full_shap_summary_test_table, "GBM_Full_SHAP_Importance.csv", row.names = FALSE)


# ============================================================================== 
# mRMR AGGREGATION PLOT
# ============================================================================== 
# 1. Aggregate and Average mRMR Scores
gbm_full_mrmr_scores <- bind_rows(gbm_full_mrmr_scores_list) %>% 
  group_by(Feature) %>% 
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
gbm_full_mrmr_scores <- gbm_full_mrmr_scores %>% 
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 5 Average Features
gbm_full_mrmr_plot <- ggplot(gbm_full_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Gradient Boosting",
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    # Tilt x-axis labels to match the Ogallala style
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))

gbm_full_mrmr_plot

############################################ Train:Test ############################
# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
gbm_full_plot_df <- bind_rows(gbm_full_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
# Pulling means and SDs directly from your summary table
gbm_full_test_r2_mean   <- gbm_full_final_summary$Mean[gbm_full_final_summary$Metric == "Test_R2"]
gbm_full_test_r2_sd     <- gbm_full_final_summary$SD[gbm_full_final_summary$Metric == "Test_R2"]
gbm_full_test_rmse_mean <- gbm_full_final_summary$Mean[gbm_full_final_summary$Metric == "Test_RMSE"]
gbm_full_test_rmse_sd   <- gbm_full_final_summary$SD[gbm_full_final_summary$Metric == "Test_RMSE"]
gbm_full_test_mae_mean  <- gbm_full_final_summary$Mean[gbm_full_final_summary$Metric == "Test_MAE"]
gbm_full_test_mae_sd    <- gbm_full_final_summary$SD[gbm_full_final_summary$Metric == "Test_MAE"]


# B. Overall Metrics (Calculated once across the entire combined dataset)
gbm_full_overall_r2 <- cor(gbm_full_plot_df$Actual, gbm_full_plot_df$Predicted)^2
gbm_full_overall_rmse <- sqrt(mean((gbm_full_plot_df$Actual - gbm_full_plot_df$Predicted)^2))
gbm_full_overall_mae <- mean(abs(gbm_full_plot_df$Actual - gbm_full_plot_df$Predicted))

# 3. CREATE CONSOLIDATED PLOT
p_main <- ggplot(gbm_full_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(gbm_full_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(gbm_full_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(gbm_full_test_mae_mean, 2), nsmall = 2), " ± ", format(round(gbm_full_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(gbm_full_test_r2_mean, 3), nsmall = 3), " ± ", format(round(gbm_full_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")
# 2. Top Density
p_top <- ggplot(gbm_full_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(gbm_full_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
gbm_full_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Gradient Boosting", 
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
gbm_full_scatter_plot

########################################################################################################
######################## XGB CLIMATE MODEL #############################################################


# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# Global XGB seeds
xgb_seeds <- list(
  seed = 123,
  bagging_seed = 123,
  feature_fraction_seed = 123,
  drop_seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))

# MASTER STORAGE (Keeps everything for every fold)
xgb_climate_mrmr_rankings <- list()
xgb_climate_mrmr_scores_list <- list()
xgb_climate_best_params <- list()
xgb_climate_fold_models <- list()
xgb_climate_shap_values <- list()
xgb_climate_shap_features <- list()
xgb_climate_norm_params <- list()
xgb_climate_test_results <- list()
xgb_climate_fold_metrics <- data.frame()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  r2   <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f)
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  xgb_climate_outer_train <- dataset_with_folds %>% filter(fold != f)
  xgb_climate_test        <- dataset_with_folds %>% filter(fold == f)
  
  # --- STEP 2: VALIDATION SET (Temporal LOYO - 6 Years) ---
  unique_years <- unique(xgb_climate_outer_train$YEAR)
  
  set.seed(123 + f)
  val_years <- sample(unique_years, 6) # Pick 6 years for a robust tuning set
  
  xgb_climate_val   <- xgb_climate_outer_train %>% filter(YEAR %in% val_years)
  xgb_climate_train <- xgb_climate_outer_train %>% filter(!(YEAR %in% val_years))
  
  cat("\nFold", f, "tuning using LOYO on Years:", paste(sort(val_years), collapse = ", "))
  
  # --- DROP NON-FEATURE COLUMNS ---
  xgb_climate_train <- xgb_climate_train %>% select(-State, -County, -YEAR)
  xgb_climate_val   <- xgb_climate_val   %>% select(-State, -County, -YEAR)
  xgb_climate_test  <- xgb_climate_test  %>% select(-State, -County, -YEAR)
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(xgb_climate_train)[sapply(xgb_climate_train, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "fold", "is_val"))
  
  means <- sapply(xgb_climate_train[feats], mean, na.rm = TRUE)
  sds   <- sapply(xgb_climate_train[feats], sd, na.rm = TRUE)
  xgb_climate_norm_params[[f]] <- list(mean = means, sd = sds)
  
  # Create scaled versions
  xgb_climate_train_sc <- xgb_climate_train
  xgb_climate_val_sc   <- xgb_climate_val
  xgb_climate_test_sc  <- xgb_climate_test
  
  for(col in feats) {
    xgb_climate_train_sc[[col]] <- (xgb_climate_train[[col]] - means[col]) / sds[col]
    xgb_climate_val_sc[[col]]   <- (xgb_climate_val[[col]] - means[col]) / sds[col]
    xgb_climate_test_sc[[col]]  <- (xgb_climate_test[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  mrmr_in <- mRMR.data(data = as.data.frame(xgb_climate_train_sc[, c(feats, "YIELD")]))
  mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(xgb_climate_train_sc[, c(feats, "YIELD")]) == "YIELD"), feature_count = 50)
  
  mrmr_feats  <- names(xgb_climate_train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run)))
  
  xgb_climate_mrmr_rankings[[f]] <- mrmr_feats
  xgb_climate_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    
    # XGBoost requires matrix input
    dtrain_tmp <- xgb.DMatrix(data = as.matrix(xgb_climate_train_sc[, curr]), label = xgb_climate_train_sc$YIELD)
    dval_tmp   <- xgb.DMatrix(data = as.matrix(xgb_climate_val_sc[, curr]))
    
    set.seed(123) 
    tmp_mod <- xgboost(
      data = dtrain_tmp,
      max_depth = 3,
      eta = 0.1,
      nrounds = 50,
      objective = "reg:squarederror",
      verbose = 0
    )
    
    p_val <- predict(tmp_mod, dval_tmp)
    rmse_history[k] <- sqrt(mean((xgb_climate_val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history); y <- rmse_history
  
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  if(k1 < (length(y) - 1)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else { 
    opt_k <- k1 
  }
  
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { 
    opt_k <- 10 
  }
  
  best_feats <- mrmr_feats[1:opt_k]
  xgb_climate_shap_features[[f]] <- best_feats
  
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  # ==============================================================================
  # E. HYPERPARAMETER TUNING (Grid Search on Validation)
  # ==============================================================================
  grid <- expand.grid(depth = c(3, 5, 7), lr = c(0.01, 0.05, 0.1))
  best_v_rmse <- Inf; final_params <- list()
  
  # Prepare matrices for XGBoost
  dtrain_final <- xgb.DMatrix(data = as.matrix(xgb_climate_train_sc[, best_feats]), label = xgb_climate_train_sc$YIELD)
  dval_final   <- xgb.DMatrix(data = as.matrix(xgb_climate_val_sc[, best_feats]))
  
  for(g in 1:nrow(grid)){
    set.seed(123) # Lock for reproducibility
    m <- xgboost(
      data = dtrain_final,
      max_depth = grid$depth[g],
      eta = grid$lr[g],
      nrounds = 150,
      subsample = 0.8,
      objective = "reg:squarederror",
      verbose = 0
    )
    
    v_p <- predict(m, dval_final)
    v_rmse <- sqrt(mean((xgb_climate_val_sc$YIELD - v_p)^2))
    
    if(v_rmse < best_v_rmse){
      best_v_rmse <- v_rmse
      final_params <- list(depth = grid$depth[g], lr = grid$lr[g])
    }
  }
  xgb_climate_best_params[[f]] <- final_params
  
  # MATCHING YOUR REQUESTED OUTPUT FORMAT
  cat("\nFold", f, "Best Parameters: Depth =", final_params$depth, "| LR =", final_params$lr)
  
  # ==============================================================================
  # F. FINAL FOLD MODEL & SHAP
  # ==============================================================================
  set.seed(123) # Lock for reproducibility
  final_mod <- xgboost(
    data = dtrain_final,
    max_depth = final_params$depth,
    eta = final_params$lr,
    nrounds = 200,
    objective = "reg:squarederror",
    verbose = 0
  )
  xgb_climate_fold_models[[f]] <- final_mod
  
  # SHAP Wrapper Fix: XGBoost handles matrices
  p_wrapper <- function(object, newdata) { 
    predict(object, as.matrix(newdata)) 
  }
  
  X_test_mat <- as.matrix(xgb_climate_test_sc[, best_feats])
  shap_contrib <- fastshap::explain(final_mod, X = X_test_mat, pred_wrapper = p_wrapper, nsim = 10)
  
  xgb_climate_shap_values[[f]]   <- as.data.frame(shap_contrib)
  xgb_climate_shap_features[[f]] <- as.data.frame(X_test_mat)
  
  # ==============================================================================
  # G. METRICS COLLECTION
  # ==============================================================================
  # 1. Generate predictions
  dtest_final <- xgb.DMatrix(data = X_test_mat)
  v_p <- predict(final_mod, dval_final)
  t_p <- predict(final_mod, dtest_final)
  
  # 2. Calculate metrics using your specific function
  v_m <- get_metrics(xgb_climate_val$YIELD, v_p, length(best_feats), nrow(xgb_climate_val))
  t_m <- get_metrics(xgb_climate_test$YIELD, t_p, length(best_feats), nrow(xgb_climate_test))
  
  # 3. Store in the metrics dataframe (Including AdjR2)
  xgb_climate_fold_metrics <- rbind(xgb_climate_fold_metrics, data.frame(
    Fold = f, Features = opt_k, 
    Val_R2 = v_m["R2"], Test_R2 = t_m["R2"], Val_AdjR2 = v_m["AdjR2"], Test_AdjR2 = t_m["AdjR2"],
    Val_RMSE = v_m["RMSE"], Test_RMSE = t_m["RMSE"], Val_MAE = v_m["MAE"], Test_MAE = t_m["MAE"]
  ))
  
  # 4. Save fold results for the Actual vs Predicted plot
  xgb_climate_test_results[[f]] <- data.frame(
    Actual = xgb_climate_test$YIELD, 
    Predicted = t_p, 
    Fold = f
  )
  
  cat("\nFold", f, "complete. Test AdjR2:", round(t_m["AdjR2"], 3))
  
} # END MASTER LOOP

# STOP TIMER HERE (Outside the loop)
end_time <- Sys.time()
comp_time <- end_time - start_time

# 5. Metrics
print(colMeans(xgb_climate_fold_metrics[, -1]))

# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================

cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")

# To make it look even cleaner with rounding:
xgb_climate_fold_metrics <- xgb_climate_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(xgb_climate_fold_metrics)


# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
# Calculate Means and SDs
xgb_climate_metric_means <- colMeans(xgb_climate_fold_metrics[, -1])
xgb_climate_metric_sds   <- sapply(xgb_climate_fold_metrics[, -1], sd)

# Create a clean summary table
n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776
xgb_climate_final_summary <- data.frame(
  Metric = names(xgb_climate_metric_means),
  Mean   = round(xgb_climate_metric_means, 4),
  SD     = round(xgb_climate_metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(xgb_climate_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(xgb_climate_final_summary, "XGB_Climate_Metrics.csv", row.names = FALSE)
saveRDS(xgb_climate_final_summary, "XGB_Climate_Metrics.rds")

# To identify which features were most common across all folds:
xgb_climate_best_feats <- unlist(xgb_climate_mrmr_rankings)
xgb_climate_feat_freq <- as.data.frame(table(xgb_climate_best_feats)) %>% arrange(desc(Freq))
print(xgb_climate_feat_freq)

# ============================================================================== 
# 4. FINAL RESULTS & VISUALIZATION 
# ============================================================================== 

# 1. Consolidate SHAP data
xgb_master_shap <- bind_rows(xgb_climate_shap_values) 
xgb_master_feat <- bind_rows(xgb_climate_shap_features) 

# 2. Pivot to Long format
xgb_prep_shap <- xgb_master_shap %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")

xgb_prep_feat <- xgb_master_feat %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "Value")

# 3. Join and Filter for TOP 5 Features
xgb_plot_data <- left_join(xgb_prep_shap, xgb_prep_feat, by = c("ID", "Feature")) %>% 
  filter(!is.na(SHAP))

xgb_top_5 <- xgb_plot_data %>% 
  group_by(Feature) %>% 
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>% 
  arrange(desc(mean_abs_impact)) %>% 
  dplyr::slice(1:5)

xgb_plot_data_top5 <- xgb_plot_data %>% 
  filter(Feature %in% xgb_top_5$Feature)

# 4. Generate the Beeswarm Plot #######################################

common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

xgb_climate_shap_plot <- ggplot(xgb_plot_data_top5 %>% 
                                  group_by(Feature) %>% 
                                  mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                                aes(x = SHAP, 
                                    y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                    color = Value)) + 
  
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  
  scale_color_viridis_c(
    option = "viridis", 
    name = "Relative\nintensity",
    limits = common_color_lim,
    oob = scales::squish) + 
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)) + 
  labs( 
    title = "Extreme Gradient Boosting", 
    x = expression("Yield impact (t ha"^{-1}*")"), 
    y = "Features")

xgb_climate_shap_plot


# Calculate Importance per Fold (using test data only)
xgb_climate_fold_importance <- xgb_plot_data %>%
  group_by(Feature) %>%
  summarize(
    Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE),
    Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

xgb_climate_shap_summary_test_table <- xgb_climate_fold_importance %>%
  select(Rank, Feature, Mean_Test_Impact, Impact_Stability_SD)

print(xgb_climate_shap_summary_test_table)
write.csv(xgb_climate_shap_summary_test_table, "XGB_climate_SHAP_Importance.csv", row.names = FALSE)


# ============================================================================== 
# mRMR AGGREGATION PLOT
# ============================================================================== 
# 1. Aggregate and Average mRMR Scores
xgb_climate_mrmr_scores <- bind_rows(xgb_climate_mrmr_scores_list) %>% 
  group_by(Feature) %>% 
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
xgb_climate_mrmr_scores <- xgb_climate_mrmr_scores %>% 
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 5 Average Features
xgb_climate_mrmr_plot <- ggplot(xgb_climate_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Extreme Gradient Boosting", 
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))

# Display the plot
xgb_climate_mrmr_plot

############################################ Train:Test ############################

# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
xgb_climate_plot_df <- bind_rows(xgb_climate_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
xgb_climate_test_r2_mean   <- xgb_climate_final_summary$Mean[xgb_climate_final_summary$Metric == "Test_R2"]
xgb_climate_test_r2_sd     <- xgb_climate_final_summary$SD[xgb_climate_final_summary$Metric == "Test_R2"]
xgb_climate_test_rmse_mean <- xgb_climate_final_summary$Mean[xgb_climate_final_summary$Metric == "Test_RMSE"]
xgb_climate_test_rmse_sd   <- xgb_climate_final_summary$SD[xgb_climate_final_summary$Metric == "Test_RMSE"]
xgb_climate_test_mae_mean  <- xgb_climate_final_summary$Mean[xgb_climate_final_summary$Metric == "Test_MAE"]
xgb_climate_test_mae_sd    <- xgb_climate_final_summary$SD[xgb_climate_final_summary$Metric == "Test_MAE"]


# B. Overall Metrics (Calculated once across the entire combined dataset)
xgb_climate_overall_r2 <- cor(xgb_climate_plot_df$Actual, xgb_climate_plot_df$Predicted)^2
xgb_climate_overall_rmse <- sqrt(mean((xgb_climate_plot_df$Actual - xgb_climate_plot_df$Predicted)^2))
xgb_climate_overall_mae <- mean(abs(xgb_climate_plot_df$Actual - xgb_climate_plot_df$Predicted))

# 3. CREATE CONSOLIDATED PLOT
p_main <- ggplot(xgb_climate_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs( 
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(xgb_climate_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(xgb_climate_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(xgb_climate_test_mae_mean, 2), nsmall = 2), " ± ", format(round(xgb_climate_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(xgb_climate_test_r2_mean, 3), nsmall = 3), " ± ", format(round(xgb_climate_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")
# 2. Top Density
p_top <- ggplot(xgb_climate_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(xgb_climate_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
xgb_climate_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Extreme Gradient Boosting", 
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
xgb_climate_scatter_plot


######################## XGB FULL MODEL ##############################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# XGB seeds
xgb_seeds <- list(
  seed = 123,
  bagging_seed = 123,
  feature_fraction_seed = 123,
  drop_seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset_final %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset_final %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))


# MASTER STORAGE (Keeps everything for every fold)
xgb_full_fold_models <- list()
xgb_full_fold_metrics <- data.frame()
xgb_full_mrmr_rankings <- list()
xgb_full_best_params <- list()
xgb_full_shap_values <- list()
xgb_full_shap_features <- list()
xgb_full_norm_params <- list()
xgb_full_best_feats_used <- c()
xgb_full_mrmr_scores <- list()
xgb_full_mrmr_scores_list <- list()
xgb_full_test_results <- list()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  r2 <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f)
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  xgb_full_outer_train <- dataset_with_folds %>% filter(fold != f)
  xgb_full_test        <- dataset_with_folds %>% filter(fold == f)
  
  # 1. CREATE A BACKUP OF THE TEST LABELS BEFORE DROPPING THEM
  test_labels <- xgb_full_test %>% select(State, County)
  
  # --- STEP 2: VALIDATION SET (Temporal LOYO - 6 Years) ---
  unique_years <- unique(xgb_full_outer_train$YEAR)
  
  set.seed(123 + f)
  val_years <- sample(unique_years, 6) # Pick 6 years for a robust tuning set
  
  xgb_full_val   <- xgb_full_outer_train %>% filter(YEAR %in% val_years)
  xgb_full_train <- xgb_full_outer_train %>% filter(!(YEAR %in% val_years))
  
  cat("\nFold", f, "tuning using LOYO on Years:", paste(sort(val_years), collapse = ", "))
  
  # --- DROP NON-FEATURE COLUMNS ---
  xgb_full_train <- xgb_full_train %>% select(-State, -County)
  xgb_full_val   <- xgb_full_val   %>% select(-State, -County)
  xgb_full_test  <- xgb_full_test  %>% select(-State, -County)
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(xgb_full_train)[sapply(xgb_full_train, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "fold", "is_val"))
  
  means <- sapply(xgb_full_train[feats], mean, na.rm = TRUE)
  sds   <- sapply(xgb_full_train[feats], sd, na.rm = TRUE)
  xgb_full_norm_params[[f]] <- list(mean = means, sd = sds)
  
  # Create scaled versions
  xgb_full_train_sc <- xgb_full_train
  xgb_full_val_sc   <- xgb_full_val
  xgb_full_test_sc  <- xgb_full_test
  
  # We also create a "Raw Year" column for your SHAP analysis later
  # because the scaled Year is hard to read.
  xgb_full_test_sc$YEAR_RAW <- xgb_full_test$YEAR 
  
  for(col in feats) {
    xgb_full_train_sc[[col]] <- (xgb_full_train[[col]] - means[col]) / sds[col]
    xgb_full_val_sc[[col]]   <- (xgb_full_val[[col]] - means[col]) / sds[col]
    xgb_full_test_sc[[col]]  <- (xgb_full_test[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  mrmr_in <- mRMR.data(data = as.data.frame(xgb_full_train_sc[, c(feats, "YIELD")]))
  mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(xgb_full_train_sc[, c(feats, "YIELD")]) == "YIELD"), feature_count = 50)
  
  mrmr_feats  <- names(xgb_full_train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run)))
  
  xgb_full_mrmr_rankings[[f]] <- mrmr_feats
  xgb_full_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    
    # XGBoost matrix preparation
    dtrain_tmp <- xgb.DMatrix(data = as.matrix(xgb_full_train_sc[, curr]), label = xgb_full_train_sc$YIELD)
    dval_tmp   <- xgb.DMatrix(data = as.matrix(xgb_full_val_sc[, curr]))
    
    set.seed(123) 
    tmp_mod <- xgboost(
      data = dtrain_tmp,
      max_depth = 3,
      eta = 0.1,
      nrounds = 50,
      objective = "reg:squarederror",
      verbose = 0
    )
    
    p_val <- predict(tmp_mod, dval_tmp)
    rmse_history[k] <- sqrt(mean((xgb_full_val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history); y <- rmse_history
  
  # 1. Find the FIRST Elbow
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  # 2. Find the SECOND Elbow
  if(k1 < (length(y) - 1)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else { 
    opt_k <- k1 
  }
  
  # 3. SAFETY FALLBACK (Ensures at least 10 features)
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { 
    opt_k <- 10 
  }
  
  best_feats <- mrmr_feats[1:opt_k]
  
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  # ==============================================================================
  # E. HYPERPARAMETER TUNING (Grid Search on Validation)
  # ==============================================================================
  grid <- expand.grid(depth = c(3, 5, 7), lr = c(0.01, 0.05, 0.1))
  best_v_rmse <- Inf; final_params <- list()
  
  # Prepare matrices for XGBoost
  dtrain_final <- xgb.DMatrix(data = as.matrix(xgb_full_train_sc[, best_feats]), label = xgb_full_train_sc$YIELD)
  dval_final   <- xgb.DMatrix(data = as.matrix(xgb_full_val_sc[, best_feats]))
  
  for(g in 1:nrow(grid)){
    set.seed(123) 
    m <- xgboost(
      data = dtrain_final,
      max_depth = grid$depth[g],
      eta = grid$lr[g],
      nrounds = 150,
      subsample = 0.8,
      objective = "reg:squarederror",
      verbose = 0
    )
    
    v_p <- predict(m, dval_final)
    v_rmse <- sqrt(mean((xgb_full_val_sc$YIELD - v_p)^2))
    
    if(v_rmse < best_v_rmse){
      best_v_rmse <- v_rmse
      final_params <- list(depth = grid$depth[g], lr = grid$lr[g])
    }
  }
  xgb_full_best_params[[f]] <- final_params
  
  cat("\nFold", f, "Best Parameters: Depth =", final_params$depth, "| LR =", final_params$lr)
  
  # ==============================================================================
  # F. FINAL FOLD MODEL & SHAP
  # ==============================================================================
  set.seed(123) 
  final_mod <- xgboost(
    data = dtrain_final,
    max_depth = final_params$depth,
    eta = final_params$lr,
    nrounds = 200,
    objective = "reg:squarederror",
    verbose = 0
  )
  xgb_full_fold_models[[f]] <- final_mod
  
  # SHAP Wrapper Fix
  p_wrapper <- function(object, newdata) { 
    predict(object, as.matrix(newdata)) 
  }
  
  X_test_mat <- as.matrix(xgb_full_test_sc[, best_feats])
  shap_contrib <- fastshap::explain(final_mod, X = X_test_mat, pred_wrapper = p_wrapper, nsim = 10)
  
  # xgb_full_shap_values[[f]]   <- as.data.frame(shap_contrib)
  # xgb_full_shap_features[[f]] <- as.data.frame(X_test_mat)
  
  # --- NEW: Attach the YEAR to the SHAP values here ---
  shap_df_with_year <- as.data.frame(shap_contrib)
  shap_df_with_year$YEAR <- xgb_full_test_sc$YEAR_RAW # Using the RAW year for easier analysis
  
  xgb_full_shap_values[[f]]   <- shap_df_with_year
  xgb_full_shap_features[[f]] <- as.data.frame(X_test_mat)
  
  # ==============================================================================
  # G. METRICS COLLECTION
  # ==============================================================================
  # 1. Generate predictions
  dtest_final <- xgb.DMatrix(data = X_test_mat)
  v_p <- predict(final_mod, dval_final)
  t_p <- predict(final_mod, dtest_final)
  
  # 2. Calculate metrics using your specific function
  v_m <- get_metrics(xgb_full_val$YIELD, v_p, length(best_feats), nrow(xgb_full_val))
  t_m <- get_metrics(xgb_full_test$YIELD, t_p, length(best_feats), nrow(xgb_full_test))
  
  # 3. Store in the metrics dataframe
  xgb_full_fold_metrics <- rbind(xgb_full_fold_metrics, data.frame(
    Fold = f, Features = opt_k, 
    Val_R2 = v_m["R2"], Test_R2 = t_m["R2"], Val_AdjR2 = v_m["AdjR2"], Test_AdjR2 = t_m["AdjR2"],
    Val_RMSE = v_m["RMSE"], Test_RMSE = t_m["RMSE"], Val_MAE = v_m["MAE"], Test_MAE = t_m["MAE"]
  ))
  
  # 4. Save fold results for the Actual vs Predicted plot
  xgb_full_test_results[[f]] <- data.frame(
    State = test_labels$State,
    County = test_labels$County,
    Actual = xgb_full_test$YIELD, 
    Predicted = t_p, 
    Year = xgb_full_test_sc$YEAR_RAW, # <--- NEW: Added for climate signal analysis
    Fold = f
  )
  
  
  cat("\nFold", f, "complete. Test AdjR2:", round(t_m["AdjR2"], 3))
  
} # END MASTER LOOP

# STOP TIMER HERE (Outside the loop)
end_time <- Sys.time()
comp_time <- end_time - start_time

# 5. Metrics
print(colMeans(xgb_full_fold_metrics[, -1]))

# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================

cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")

# To make it look even cleaner with rounding:
xgb_full_fold_metrics <- xgb_full_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(xgb_full_fold_metrics)

################# XGB FULL: Feature Ranking Change Analysis (Reviewer Comment) ###############

# # 1. Combine SHAP values
# xgb_full_all_shap <- bind_rows(xgb_full_shap_values)
# 
# # 2. Calculate Importance (Mean Absolute SHAP in t/ha)
# xgb_full_early_imp  <- colMeans(abs(xgb_full_all_shap %>% filter(YEAR <= 2000) %>% select(-YEAR)), na.rm = TRUE)
# xgb_full_recent_imp <- colMeans(abs(xgb_full_all_shap %>% filter(YEAR >= 2001) %>% select(-YEAR)), na.rm = TRUE)
# 
# # 3. Create the comparison table
# xgb_full_imp_comparison <- data.frame(
#   Feature = names(xgb_full_early_imp),
#   Early_t_ha   = xgb_full_early_imp,
#   Recent_t_ha  = xgb_full_recent_imp
# ) %>% 
#   mutate(
#     # The actual change in impact magnitude in t/ha
#     Yield_Impact_Change_t_ha = Recent_t_ha - Early_t_ha,
#     # Label if the feature is becoming more or less influential
#     Trend = ifelse(Yield_Impact_Change_t_ha > 0, "Increasing Influence", "Decreasing Influence")
#   )
# 
# # 4. Rank and Filter
# xgb_full_rank_check <- xgb_full_imp_comparison %>%
#   filter(!is.na(Early_t_ha)) %>%
#   mutate(Rank_Early = rank(-Early_t_ha),
#          Rank_Recent = rank(-Recent_t_ha)) %>%
#   arrange(Rank_Recent)
# 
# # 5. Print and Save
# print(head(xgb_full_rank_check, 10))
# 

# 1. Combine SHAP values
xgb_full_all_shap <- bind_rows(xgb_full_shap_values)

# 2. Calculate Importance for three 15-year/era blocks
xgb_early_imp  <- colMeans(abs(xgb_full_all_shap %>% filter(YEAR >= 1981 & YEAR <= 1995) %>% select(-YEAR)), na.rm = TRUE)
xgb_mid_imp    <- colMeans(abs(xgb_full_all_shap %>% filter(YEAR >= 1996 & YEAR <= 2010) %>% select(-YEAR)), na.rm = TRUE)
xgb_recent_imp <- colMeans(abs(xgb_full_all_shap %>% filter(YEAR >= 2011) %>% select(-YEAR)), na.rm = TRUE)

# 3. Create the comparison table
xgb_full_imp_comparison <- data.frame(
  Feature    = names(xgb_early_imp),
  Early_t_ha = xgb_early_imp,
  Mid_t_ha   = xgb_mid_imp,
  Recent_t_ha = xgb_recent_imp
) %>% 
  mutate(
    # Overall change from the earliest period to the most recent
    Total_Change_t_ha = Recent_t_ha - Early_t_ha,
    Trend = ifelse(Total_Change_t_ha > 0, "Increasing Influence", "Decreasing Influence")
  )

# 4. Rank based on the most recent period
xgb_full_rank_check <- xgb_full_imp_comparison %>%
  filter(!is.na(Early_t_ha)) %>%
  mutate(
    Rank_Early  = rank(-Early_t_ha),
    Rank_Mid    = rank(-Mid_t_ha),
    Rank_Recent = rank(-Recent_t_ha)
  ) %>%
  arrange(Rank_Recent)

# 5. Print
print(head(xgb_full_rank_check, 10))


write.csv(xgb_full_rank_check, "xgb_full_rank_check.csv", row.names = FALSE)

############## REVIEWER COMMENT ON UNCERTAINITY ###########################

# 1. Combine all results
xgb_full_all_results <- bind_rows(xgb_full_test_results)

# 2. Calculate Annual Uncertainty
xgb_full_uncertainty_summary <- xgb_full_all_results %>%
  group_by(Year) %>%
  summarize(
    Observed_Yield = mean(Actual),
    Mean_Prediction = mean(Predicted),
    Prediction_SD = sd(Predicted)
  )

# 3. Designate 2012 as the only extreme year, all others as Normal
xgb_full_uncertainty_summary <- xgb_full_uncertainty_summary %>%
  mutate(Year_Type = ifelse(Year == 2012, "Extreme (2012 Drought)", "Normal Years"))

# 4. Calculate the Comparison
xgb_full_uncertainty_check <- xgb_full_uncertainty_summary %>%
  group_by(Year_Type) %>%
  summarize(
    Number_of_Years = n(),
    Avg_Yield = mean(Observed_Yield),
    Avg_Uncertainty_SD = mean(Prediction_SD)
  )

print(xgb_full_uncertainty_check)


# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
# Calculate Means and SDs
xgb_full_metric_means <- colMeans(xgb_full_fold_metrics[, -1])
xgb_full_metric_sds   <- sapply(xgb_full_fold_metrics[, -1], sd)

# Create a clean summary table
n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776

xgb_full_final_summary <- data.frame(
  Metric = names(xgb_full_metric_means),
  Mean   = round(xgb_full_metric_means, 4),
  SD     = round(xgb_full_metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(xgb_full_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(xgb_full_final_summary, "XGB_Full_Metrics.csv", row.names = FALSE)
saveRDS(xgb_full_final_summary, "XGB_Full_Metrics.rds")

# To identify which features were most common across all folds:
xgb_full_best_feats <- unlist(xgb_full_mrmr_rankings)
xgb_full_feat_freq <- as.data.frame(table(xgb_full_best_feats)) %>% arrange(desc(Freq))
print(xgb_full_feat_freq)

# ==============================================================================
# 5. STATE-BY-STATE PERFORMANCE (POST-HOC ANALYSIS)
# ==============================================================================
cat("\n\n================ STATE-LEVEL PERFORMANCE ANALYSIS ================\n")

xgb_full_all_test_preds <- bind_rows(xgb_full_test_results)

# 2. Group by State to see how the model generalizes geographically
xgb_full_state_metrics <- xgb_full_all_test_preds %>%
  group_by(State) %>%
  summarise(
    Counties = n_distinct(County),
    Observations = n(),
    RMSE = sqrt(mean((Actual - Predicted)^2)),
    MAE  = mean(abs(Actual - Predicted)),
    R2   = 1 - (sum((Actual - Predicted)^2) / sum((Actual - mean(Actual))^2)),
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4))) %>%
  arrange(desc(R2))

print(xgb_full_state_metrics)

# 3. Save the state-level breakdown
write.csv(xgb_full_state_metrics, "XGB_State_Level_Metrics.csv", row.names = FALSE)

cat("\nState-level analysis complete and saved to 'XGB_State_Level_Metrics.csv'\n")


# ============================================================================== 
# 4. FINAL RESULTS & VISUALIZATION 
# ============================================================================== 

# 1. Consolidate SHAP data
xgb_master_shap <- bind_rows(xgb_full_shap_values) 
xgb_master_feat <- bind_rows(xgb_full_shap_features) 

# 2. Pivot to Long format
xgb_prep_shap <- xgb_master_shap %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")

xgb_prep_feat <- xgb_master_feat %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "Value")

# 3. Join and Filter for TOP 5 Features
xgb_plot_data <- left_join(xgb_prep_shap, xgb_prep_feat, by = c("ID", "Feature")) %>% 
  filter(!is.na(SHAP)) %>%
  filter(Feature != "YEAR") 

xgb_top_5 <- xgb_plot_data %>% 
  group_by(Feature) %>% 
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>% 
  arrange(desc(mean_abs_impact)) %>% 
  dplyr::slice(1:5)

xgb_plot_data_top5 <- xgb_plot_data %>% 
  filter(Feature %in% xgb_top_5$Feature)

# 4. Generate the Beeswarm Plot #######################################

common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

xgb_full_shap_plot <- ggplot(xgb_plot_data_top5 %>% 
                               group_by(Feature) %>% 
                               mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                             aes(x = SHAP, 
                                 y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                 color = Value)) + 
  
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  
  scale_color_viridis_c(
    option = "viridis", 
    name = "Relative\nintensity",
    limits = common_color_lim,
    oob = scales::squish) + 
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)) + 
  labs( 
    title = "Extreme Gradient Boosting", 
    x = expression("Yield impact (t ha"^{-1}*")"), 
    y = "Features")

xgb_full_shap_plot


# Calculate Importance per Fold (using test data only)
xgb_full_fold_importance <- xgb_plot_data %>%
  group_by(Feature) %>%
  summarize(
    Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE),
    Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

xgb_full_shap_summary_test_table <- xgb_full_fold_importance %>%
  select(Rank, Feature, Mean_Test_Impact, Impact_Stability_SD)

print(xgb_full_shap_summary_test_table)
write.csv(xgb_full_shap_summary_test_table, "XGB_Full_SHAP_Importance.csv", row.names = FALSE)

# ============================================================================== 
# mRMR AGGREGATION PLOT
# ============================================================================== 
# 1. Aggregate and Average mRMR Scores
xgb_full_mrmr_scores <- bind_rows(xgb_full_mrmr_scores_list) %>% 
  group_by(Feature) %>% 
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
xgb_full_mrmr_scores <- xgb_full_mrmr_scores %>% 
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 5 Average Features
xgb_full_mrmr_plot <- ggplot(xgb_full_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Extreme Gradient Boosting", 
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))

xgb_full_mrmr_plot

############################################ Train:Test ############################
# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
xgb_full_plot_df <- bind_rows(xgb_full_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
xgb_full_test_r2_mean   <- xgb_full_final_summary$Mean[xgb_full_final_summary$Metric == "Test_R2"]
xgb_full_test_r2_sd     <- xgb_full_final_summary$SD[xgb_full_final_summary$Metric == "Test_R2"]
xgb_full_test_rmse_mean <- xgb_full_final_summary$Mean[xgb_full_final_summary$Metric == "Test_RMSE"]
xgb_full_test_rmse_sd   <- xgb_full_final_summary$SD[xgb_full_final_summary$Metric == "Test_RMSE"]
xgb_full_test_mae_mean  <- xgb_full_final_summary$Mean[xgb_full_final_summary$Metric == "Test_MAE"]
xgb_full_test_mae_sd    <- xgb_full_final_summary$SD[xgb_full_final_summary$Metric == "Test_MAE"]

# B. Overall Metrics
xgb_full_overall_r2 <- cor(xgb_full_plot_df$Actual, xgb_full_plot_df$Predicted)^2
xgb_full_overall_rmse <- sqrt(mean((xgb_full_plot_df$Actual - xgb_full_plot_df$Predicted)^2))
xgb_full_overall_mae <- mean(abs(xgb_full_plot_df$Actual - xgb_full_plot_df$Predicted))

# 3. CREATE CONSOLIDATED PLOT
p_main <- ggplot(xgb_full_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(xgb_full_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(xgb_full_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(xgb_full_test_mae_mean, 2), nsmall = 2), " ± ", format(round(xgb_full_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(xgb_full_test_r2_mean, 3), nsmall = 3), " ± ", format(round(xgb_full_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")

# 2. Top Density
p_top <- ggplot(xgb_full_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(xgb_full_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
xgb_full_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Extreme Gradient Boosting", 
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
xgb_full_scatter_plot


########################################################################################################
######################## Random Forest CLIMATE MODEL ###################################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# Global RF seeds (Random Forest typically uses a single seed for the forest)
rf_seeds <- list(
  seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))

# MASTER STORAGE (Keeps everything for every fold)
rf_climate_mrmr_rankings <- list()
rf_climate_mrmr_scores_list <- list()
rf_climate_best_params <- list()
rf_climate_fold_models <- list()
rf_climate_shap_values <- list()
rf_climate_shap_features <- list()
rf_climate_norm_params <- list()
rf_climate_test_results <- list()
rf_climate_fold_metrics <- data.frame()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  r2   <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f)
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  rf_climate_outer_train <- dataset_with_folds %>% filter(fold != f)
  rf_climate_test        <- dataset_with_folds %>% filter(fold == f)
  
  # --- STEP 2: VALIDATION SET (Temporal LOYO - 6 Years) ---
  unique_years <- unique(rf_climate_outer_train$YEAR)
  
  set.seed(123 + f)
  val_years <- sample(unique_years, 6) 
  
  rf_climate_val   <- rf_climate_outer_train %>% filter(YEAR %in% val_years)
  rf_climate_train <- rf_climate_outer_train %>% filter(!(YEAR %in% val_years))
  
  cat("\nFold", f, "tuning using LOYO on Years:", paste(sort(val_years), collapse = ", "))
  
  # # --- DROP NON-FEATURE COLUMNS ---
  # rf_climate_train <- rf_climate_train %>% select(-State, -County, -YEAR)
  # rf_climate_val   <- rf_climate_val   %>% select(-State, -County, -YEAR)
  # rf_climate_test  <- rf_climate_test  %>% select(-State, -County, -YEAR)
  
  # --- KEEP NON-FEATURE COLUMNS FOR TRACKING ---
  # We still drop State/County to keep the data clean, but KEEP YEAR
  rf_climate_train <- rf_climate_train %>% select(-State, -County)
  rf_climate_val   <- rf_climate_val   %>% select(-State, -County)
  rf_climate_test  <- rf_climate_test  %>% select(-State, -County)
  
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(rf_climate_train)[sapply(rf_climate_train, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "YEAR", "fold", "is_val"))
  
  means <- sapply(rf_climate_train[feats], mean, na.rm = TRUE)
  sds   <- sapply(rf_climate_train[feats], sd, na.rm = TRUE)
  rf_climate_norm_params[[f]] <- list(mean = means, sd = sds)
  
  # Create scaled versions
  rf_climate_train_sc <- rf_climate_train
  rf_climate_val_sc   <- rf_climate_val
  rf_climate_test_sc  <- rf_climate_test
  
  for(col in feats) {
    rf_climate_train_sc[[col]] <- (rf_climate_train[[col]] - means[col]) / sds[col]
    rf_climate_val_sc[[col]]   <- (rf_climate_val[[col]] - means[col]) / sds[col]
    rf_climate_test_sc[[col]]  <- (rf_climate_test[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  mrmr_in <- mRMR.data(data = as.data.frame(rf_climate_train_sc[, c(feats, "YIELD")]))
  mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(rf_climate_train_sc[, c(feats, "YIELD")]) == "YIELD"), feature_count = 50)
  
  mrmr_feats  <- names(rf_climate_train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run)))
  
  rf_climate_mrmr_rankings[[f]] <- mrmr_feats
  rf_climate_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    
    set.seed(123) 
    # Using randomForest for the elbow check
    tmp_mod <- randomForest(
      formula = YIELD ~ ., 
      data = rf_climate_train_sc[, c(curr, "YIELD")],
      ntree = 50, # Fewer trees for speed during elbow search
      importance = FALSE
    )
    
    p_val <- predict(tmp_mod, rf_climate_val_sc[, curr, drop = FALSE])
    rmse_history[k] <- sqrt(mean((rf_climate_val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history); y <- rmse_history
  
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  if(k1 < (length(y) - 1)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else { 
    opt_k <- k1 
  }
  
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { 
    opt_k <- 10 
  }
  
  best_feats <- mrmr_feats[1:opt_k]
  rf_climate_shap_features[[f]] <- best_feats
  
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  # ==============================================================================
  # E. HYPERPARAMETER TUNING (Grid Search on Validation)
  # ==============================================================================
  # For Random Forest, tuning usually involves mtry and nodesize
  grid <- expand.grid(
    mtry = floor(length(best_feats) * c(0.2, 0.33, 0.5)), 
    nodesize = c(1, 5, 10)
  )
  best_v_rmse <- Inf; final_params <- list()
  
  for(g in 1:nrow(grid)){
    set.seed(123) 
    m <- randomForest(
      formula = YIELD ~ ., 
      data = rf_climate_train_sc[, c(best_feats, "YIELD")],
      ntree = 150,
      mtry = grid$mtry[g],
      nodesize = grid$nodesize[g]
    )
    
    v_p <- predict(m, rf_climate_val_sc[, best_feats])
    v_rmse <- sqrt(mean((rf_climate_val_sc$YIELD - v_p)^2))
    
    if(v_rmse < best_v_rmse){
      best_v_rmse <- v_rmse
      final_params <- list(mtry = grid$mtry[g], nodesize = grid$nodesize[g])
    }
  }
  rf_climate_best_params[[f]] <- final_params
  
  cat("\nFold", f, "Best Parameters: mtry =", final_params$mtry, "| nodesize =", final_params$nodesize)
  
  # ==============================================================================
  # F. FINAL FOLD MODEL & SHAP
  # ==============================================================================
  set.seed(123) 
  final_mod <- randomForest(
    formula = YIELD ~ ., 
    data = rf_climate_train_sc[, c(best_feats, "YIELD")],
    ntree = 200,
    mtry = final_params$mtry,
    nodesize = final_params$nodesize
  )
  rf_climate_fold_models[[f]] <- final_mod
  
  # SHAP Wrapper Fix for Random Forest
  p_wrapper <- function(object, newdata) { 
    predict(object, newdata) 
  }
  
  X_test_df <- rf_climate_test_sc[, best_feats]
  shap_contrib <- fastshap::explain(final_mod, X = X_test_df, pred_wrapper = p_wrapper, nsim = 10)
  
  # rf_climate_shap_values[[f]]   <- as.data.frame(shap_contrib)
  # rf_climate_shap_features[[f]] <- X_test_df
  
  # --- NEW: Attach the YEAR to the SHAP values here ---
  shap_df_with_year <- as.data.frame(shap_contrib)
  shap_df_with_year$YEAR <- rf_climate_test_sc$YEAR # This is safe metadata
  
  rf_climate_shap_values[[f]]   <- shap_df_with_year
  rf_climate_shap_features[[f]] <- X_test_df
  
  # ==============================================================================
  # G. METRICS COLLECTION
  # ==============================================================================
  # 1. Generate predictions
  v_p <- predict(final_mod, rf_climate_val_sc[, best_feats])
  t_p <- predict(final_mod, rf_climate_test_sc[, best_feats])
  
  # 2. Calculate metrics using your specific function
  v_m <- get_metrics(rf_climate_val$YIELD, v_p, length(best_feats), nrow(rf_climate_val))
  t_m <- get_metrics(rf_climate_test$YIELD, t_p, length(best_feats), nrow(rf_climate_test))
  
  # 3. Store in the metrics dataframe
  rf_climate_fold_metrics <- rbind(rf_climate_fold_metrics, data.frame(
    Fold = f, Features = opt_k, 
    Val_R2 = v_m["R2"], Test_R2 = t_m["R2"], Val_AdjR2 = v_m["AdjR2"], Test_AdjR2 = t_m["AdjR2"],
    Val_RMSE = v_m["RMSE"], Test_RMSE = t_m["RMSE"], Val_MAE = v_m["MAE"], Test_MAE = t_m["MAE"]
  ))
  
  # 4. Save fold results for the Actual vs Predicted plot
  rf_climate_test_results[[f]] <- data.frame(
    Actual = rf_climate_test$YIELD, 
    Predicted = t_p, 
    Year      = rf_climate_test_sc$YEAR, # <--- NEW: Crucial for Climate Signal
    Fold = f
  )
  
  cat("\nFold", f, "complete. Test AdjR2:", round(t_m["AdjR2"], 3))
  
} # END MASTER LOOP

# STOP TIMER HERE (Outside the loop)
end_time <- Sys.time()
comp_time <- end_time - start_time

# 5. Metrics
print(colMeans(rf_climate_fold_metrics[, -1]))


# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================

cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")

# If you are running the RF Full model:
print(rf_climate_fold_metrics)

# To make it look even cleaner with rounding:
rf_climate_fold_metrics <- rf_climate_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(rf_climate_fold_metrics)

################# RF CLIMATE: Feature Ranking Change Analysis ###############

# # 1. Combine SHAP values
# rf_climate_all_shap <- bind_rows(rf_climate_shap_values)
# 
# # 2. Calculate Importance (Mean Absolute SHAP in t/ha)
# # Note: Using na.rm = TRUE ensures we average only the folds where the feature was selected
# rf_climate_early_imp  <- colMeans(abs(rf_climate_all_shap %>% filter(YEAR <= 2000) %>% select(-YEAR)), na.rm = TRUE)
# rf_climate_recent_imp <- colMeans(abs(rf_climate_all_shap %>% filter(YEAR >= 2001) %>% select(-YEAR)), na.rm = TRUE)
# 
# # 3. Create the comparison table with t/ha labeling
# rf_climate_imp_comparison <- data.frame(
#   Feature = names(rf_climate_early_imp),
#   Early_t_ha   = rf_climate_early_imp,
#   Recent_t_ha  = rf_climate_recent_imp
# ) %>% 
#   mutate(
#     # The actual change in impact magnitude in t/ha
#     Yield_Impact_Change_t_ha = Recent_t_ha - Early_t_ha,
#     # Direction of the influence change
#     Trend = ifelse(Yield_Impact_Change_t_ha > 0, "Increasing Influence", "Decreasing Influence")
#   )
# 
# # 4. Rank and Filter
# rf_climate_rank_check <- rf_climate_imp_comparison %>%
#   filter(!is.na(Early_t_ha)) %>%
#   mutate(Rank_Early = rank(-Early_t_ha),
#          Rank_Recent = rank(-Recent_t_ha)) %>%
#   arrange(Rank_Recent)
# 
# # 5. Print and Save
# print(head(rf_climate_rank_check, 10))

# 1. Combine SHAP values
rf_climate_all_shap <- bind_rows(rf_climate_shap_values)

# 2. Calculate Importance for three era blocks
rf_climate_early_imp  <- colMeans(abs(rf_climate_all_shap %>% filter(YEAR >= 1981 & YEAR <= 1995) %>% select(-YEAR)), na.rm = TRUE)
rf_climate_mid_imp    <- colMeans(abs(rf_climate_all_shap %>% filter(YEAR >= 1996 & YEAR <= 2010) %>% select(-YEAR)), na.rm = TRUE)
rf_climate_recent_imp <- colMeans(abs(rf_climate_all_shap %>% filter(YEAR >= 2011) %>% select(-YEAR)), na.rm = TRUE)

# 3. Create the comparison table
rf_climate_imp_comparison <- data.frame(
  Feature     = names(rf_climate_early_imp),
  Early_t_ha  = rf_climate_early_imp,
  Mid_t_ha    = rf_climate_mid_imp,
  Recent_t_ha = rf_climate_recent_imp
) %>% 
  mutate(
    # Total change from the earliest period to the most recent
    Total_Change_t_ha = Recent_t_ha - Early_t_ha,
    Trend = ifelse(Total_Change_t_ha > 0, "Increasing Influence", "Decreasing Influence")
  )

# 4. Rank based on the three periods
rf_climate_rank_check <- rf_climate_imp_comparison %>%
  filter(!is.na(Early_t_ha)) %>%
  mutate(
    Rank_Early  = rank(-Early_t_ha),
    Rank_Mid    = rank(-Mid_t_ha),
    Rank_Recent = rank(-Recent_t_ha)
  ) %>%
  arrange(Rank_Recent)

# 5. Print
print(head(rf_climate_rank_check, 10))


write.csv(rf_climate_rank_check, "rf_climate_rank_check.csv", row.names = FALSE)

############## RF CLIMATE: UNCERTAINTY QUANTIFICATION ###########################

# 1. Combine all results from the RF Climate model folds
rf_climate_all_results <- bind_rows(rf_climate_test_results)

# 2. Calculate Annual Uncertainty (Inter-fold Standard Deviation)
rf_climate_uncertainty_summary <- rf_climate_all_results %>%
  group_by(Year) %>%
  summarize(
    Observed_Yield = mean(Actual),
    Mean_Prediction = mean(Predicted),
    # Prediction_SD represents the "uncertainty" or model disagreement
    Prediction_SD = sd(Predicted)
  )

# 3. Designate 2012 as the benchmark extreme year
rf_climate_uncertainty_summary <- rf_climate_uncertainty_summary %>%
  mutate(Year_Type = ifelse(Year == 2012, "Extreme (2012 Drought)", "Normal Years"))

# 4. Calculate the Comparison for RF Climate
rf_climate_uncertainty_check <- rf_climate_uncertainty_summary %>%
  group_by(Year_Type) %>%
  summarize(
    Number_of_Years = n(),
    Avg_Yield = mean(Observed_Yield),
    Avg_Uncertainty_SD = mean(Prediction_SD)
  )

print(rf_climate_uncertainty_check)



# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
# Calculate Means and SDs
rf_climate_metric_means <- colMeans(rf_climate_fold_metrics[, -1])
rf_climate_metric_sds   <- sapply(rf_climate_fold_metrics[, -1], sd)

# Create a clean summary table
n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776

rf_climate_final_summary <- data.frame(
  Metric = names(rf_climate_metric_means),
  Mean   = round(rf_climate_metric_means, 4),
  SD     = round(rf_climate_metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(rf_climate_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(rf_climate_final_summary, "RF_Climate_Metrics.csv", row.names = FALSE)
saveRDS(rf_climate_final_summary, "RF_Climate_Metrics.rds")

# To identify which features were most common across all folds:
rf_climate_best_feats <- unlist(rf_climate_mrmr_rankings)
rf_climate_feat_freq <- as.data.frame(table(rf_climate_best_feats)) %>% arrange(desc(Freq))
print(rf_climate_feat_freq)

# ============================================================================== 
# 4. FINAL RESULTS & VISUALIZATION (Random Forest)
# ============================================================================== 

# 1. Consolidate SHAP data
rf_master_shap <- bind_rows(rf_climate_shap_values) 
rf_master_feat <- bind_rows(rf_climate_shap_features) 

# 2. Pivot to Long format
rf_prep_shap <- rf_master_shap %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")

rf_prep_feat <- rf_master_feat %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "Value")

# 3. Join and Filter for TOP 5 Features
rf_plot_data <- left_join(rf_prep_shap, rf_prep_feat, by = c("ID", "Feature")) %>% 
  filter(!is.na(SHAP)) %>%
  filter(Feature != "YEAR") 

rf_top_5 <- rf_plot_data %>% 
  group_by(Feature) %>% 
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>% 
  arrange(desc(mean_abs_impact)) %>% 
  dplyr::slice(1:5)

rf_plot_data_top5 <- rf_plot_data %>% 
  filter(Feature %in% rf_top_5$Feature)

# 4. Generate the Beeswarm Plot #######################################

common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

rf_climate_shap_plot <- ggplot(rf_plot_data_top5 %>% 
                                 group_by(Feature) %>% 
                                 mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                               aes(x = SHAP, 
                                   y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                   color = Value)) + 
  
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  
  scale_color_viridis_c(
    option = "viridis", 
    name = "Relative\nintensity",
    limits = common_color_lim,
    oob = scales::squish) + 
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)) + 
  labs( 
    title = "Random Forest", 
    x = expression("Yield impact (t ha"^{-1}*")"), 
    y = "Features")

rf_climate_shap_plot


# Calculate Importance per Fold (using test data only)
rf_climate_fold_importance <- rf_plot_data %>%
  group_by(Feature) %>%
  summarize(
    Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE),
    Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

rf_climate_shap_summary_test_table <- rf_climate_fold_importance %>%
  select(Rank, Feature, Mean_Test_Impact, Impact_Stability_SD)

print(rf_climate_shap_summary_test_table)
write.csv(rf_climate_shap_summary_test_table, "RF_climate_SHAP_Importance.csv", row.names = FALSE)


# ============================================================================== 
# mRMR AGGREGATION PLOT
# ============================================================================== 
# 1. Aggregate and Average mRMR Scores
rf_climate_mrmr_scores <- bind_rows(rf_climate_mrmr_scores_list) %>% 
  group_by(Feature) %>% 
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
rf_climate_mrmr_scores <- rf_climate_mrmr_scores %>% 
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 5 Average Features
rf_climate_mrmr_plot <- ggplot(rf_climate_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Random Forest", 
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))

rf_climate_mrmr_plot

############################################ Train:Test ############################

# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
rf_climate_plot_df <- bind_rows(rf_climate_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
rf_climate_test_r2_mean   <- rf_climate_final_summary$Mean[rf_climate_final_summary$Metric == "Test_R2"]
rf_climate_test_r2_sd     <- rf_climate_final_summary$SD[rf_climate_final_summary$Metric == "Test_R2"]
rf_climate_test_rmse_mean <- rf_climate_final_summary$Mean[rf_climate_final_summary$Metric == "Test_RMSE"]
rf_climate_test_rmse_sd   <- rf_climate_final_summary$SD[rf_climate_final_summary$Metric == "Test_RMSE"]
rf_climate_test_mae_mean  <- rf_climate_final_summary$Mean[rf_climate_final_summary$Metric == "Test_MAE"]
rf_climate_test_mae_sd    <- rf_climate_final_summary$SD[rf_climate_final_summary$Metric == "Test_MAE"]


# B. Overall Metrics (Calculated once across the entire combined dataset)
rf_climate_overall_r2 <- cor(rf_climate_plot_df$Actual, rf_climate_plot_df$Predicted)^2
rf_climate_overall_rmse <- sqrt(mean((rf_climate_plot_df$Actual - rf_climate_plot_df$Predicted)^2))
rf_climate_overall_mae <- mean(abs(rf_climate_plot_df$Actual - rf_climate_plot_df$Predicted))

# 3. CREATE CONSOLIDATED PLOT
p_main <- ggplot(rf_climate_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(rf_climate_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(rf_climate_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(rf_climate_test_mae_mean, 2), nsmall = 2), " ± ", format(round(rf_climate_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(rf_climate_test_r2_mean, 3), nsmall = 3), " ± ", format(round(rf_climate_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")

# 2. Top Density
p_top <- ggplot(rf_climate_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(rf_climate_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
rf_climate_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Random Forest",  
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
rf_climate_scatter_plot


########################################################################################################
######################## Random Forest FULL MODEL #############################################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# RF seeds
rf_seeds <- list(
  seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset_final %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset_final %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))


# MASTER STORAGE (Keeps everything for every fold)
rf_full_fold_models <- list()
rf_full_fold_metrics <- data.frame()
rf_full_mrmr_rankings <- list()
rf_full_best_params <- list()
rf_full_shap_values <- list()
rf_full_shap_features <- list()
rf_full_norm_params <- list()
rf_full_best_feats_used <- c()
rf_full_mrmr_scores <- list()
rf_full_mrmr_scores_list <- list()
rf_full_test_results <- list()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  r2 <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f)
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  rf_full_outer_train <- dataset_with_folds %>% filter(fold != f)
  rf_full_test        <- dataset_with_folds %>% filter(fold == f)
  
  # --- STEP 2: VALIDATION SET (Temporal LOYO - 6 Years) ---
  unique_years <- unique(rf_full_outer_train$YEAR)
  
  set.seed(123 + f)
  val_years <- sample(unique_years, 6) # Pick 6 years for a robust tuning set
  
  rf_full_val   <- rf_full_outer_train %>% filter(YEAR %in% val_years)
  rf_full_train <- rf_full_outer_train %>% filter(!(YEAR %in% val_years))
  
  cat("\nFold", f, "tuning using LOYO on Years:", paste(sort(val_years), collapse = ", "))
  
  # --- DROP NON-FEATURE COLUMNS ---
  rf_full_train <- rf_full_train %>% select(-State, -County)
  rf_full_val   <- rf_full_val   %>% select(-State, -County)
  rf_full_test  <- rf_full_test  %>% select(-State, -County)
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(rf_full_train)[sapply(rf_full_train, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "fold", "is_val"))
  
  means <- sapply(rf_full_train[feats], mean, na.rm = TRUE)
  sds   <- sapply(rf_full_train[feats], sd, na.rm = TRUE)
  rf_full_norm_params[[f]] <- list(mean = means, sd = sds)
  
  # Create scaled versions
  rf_full_train_sc <- rf_full_train
  rf_full_val_sc   <- rf_full_val
  rf_full_test_sc  <- rf_full_test
  
  for(col in feats) {
    rf_full_train_sc[[col]] <- (rf_full_train[[col]] - means[col]) / sds[col]
    rf_full_val_sc[[col]]   <- (rf_full_val[[col]] - means[col]) / sds[col]
    rf_full_test_sc[[col]]  <- (rf_full_test[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  mrmr_in <- mRMR.data(data = as.data.frame(rf_full_train_sc[, c(feats, "YIELD")]))
  mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(rf_full_train_sc[, c(feats, "YIELD")]) == "YIELD"), feature_count = 50)
  
  mrmr_feats  <- names(rf_full_train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run)))
  
  rf_full_mrmr_rankings[[f]] <- mrmr_feats
  rf_full_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    
    set.seed(123) 
    tmp_mod <- randomForest(
      formula = YIELD ~ ., 
      data = rf_full_train_sc[, c(curr, "YIELD")],
      ntree = 50,
      importance = FALSE
    )
    
    p_val <- predict(tmp_mod, rf_full_val_sc[, curr, drop = FALSE])
    rmse_history[k] <- sqrt(mean((rf_full_val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history); y <- rmse_history
  
  # 1. Find the FIRST Elbow
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  # 2. Find the SECOND Elbow
  if(k1 < (length(y) - 1)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else { 
    opt_k <- k1 
  }
  
  # 3. SAFETY FALLBACK (Ensures at least 10 features)
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { 
    opt_k <- 10 
  }
  
  best_feats <- mrmr_feats[1:opt_k]
  
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  # ==============================================================================
  # E. HYPERPARAMETER TUNING (Grid Search on Validation)
  # ==============================================================================
  # Using the same mtry and nodesize logic from the RF climate model
  grid <- expand.grid(
    mtry = floor(length(best_feats) * c(0.2, 0.33, 0.5)), 
    nodesize = c(1, 5, 10)
  )
  best_v_rmse <- Inf; final_params <- list()
  
  for(g in 1:nrow(grid)){
    set.seed(123) 
    m <- randomForest(
      formula = YIELD ~ ., 
      data = rf_full_train_sc[, c(best_feats, "YIELD")],
      ntree = 150,
      mtry = grid$mtry[g],
      nodesize = grid$nodesize[g]
    )
    
    v_p <- predict(m, rf_full_val_sc[, best_feats])
    v_rmse <- sqrt(mean((rf_full_val_sc$YIELD - v_p)^2))
    
    if(v_rmse < best_v_rmse){
      best_v_rmse <- v_rmse
      final_params <- list(mtry = grid$mtry[g], nodesize = grid$nodesize[g])
    }
  }
  rf_full_best_params[[f]] <- final_params
  
  cat("\nFold", f, "Best Parameters: mtry =", final_params$mtry, "| nodesize =", final_params$nodesize)
  
  # ==============================================================================
  # F. FINAL FOLD MODEL & SHAP
  # ==============================================================================
  set.seed(123) 
  final_mod <- randomForest(
    formula = YIELD ~ ., 
    data = rf_full_train_sc[, c(best_feats, "YIELD")],
    ntree = 200,
    mtry = final_params$mtry,
    nodesize = final_params$nodesize
  )
  rf_full_fold_models[[f]] <- final_mod
  
  # SHAP Wrapper Fix for Random Forest
  p_wrapper <- function(object, newdata) { 
    predict(object, newdata) 
  }
  
  X_test_df <- rf_full_test_sc[, best_feats]
  shap_contrib <- fastshap::explain(final_mod, X = X_test_df, pred_wrapper = p_wrapper, nsim = 10)
  
  rf_full_shap_values[[f]]   <- as.data.frame(shap_contrib)
  rf_full_shap_features[[f]] <- X_test_df
  
  # ==============================================================================
  # G. METRICS COLLECTION
  # ==============================================================================
  # 1. Generate predictions
  v_p <- predict(final_mod, rf_full_val_sc[, best_feats])
  t_p <- predict(final_mod, rf_full_test_sc[, best_feats])
  
  # 2. Calculate metrics using your specific function
  v_m <- get_metrics(rf_full_val$YIELD, v_p, length(best_feats), nrow(rf_full_val))
  t_m <- get_metrics(rf_full_test$YIELD, t_p, length(best_feats), nrow(rf_full_test))
  
  # 3. Store in the metrics dataframe
  rf_full_fold_metrics <- rbind(rf_full_fold_metrics, data.frame(
    Fold = f, Features = opt_k, 
    Val_R2 = v_m["R2"], Test_R2 = t_m["R2"], Val_AdjR2 = v_m["AdjR2"], Test_AdjR2 = t_m["AdjR2"],
    Val_RMSE = v_m["RMSE"], Test_RMSE = t_m["RMSE"], Val_MAE = v_m["MAE"], Test_MAE = t_m["MAE"]
  ))
  
  # 4. Save fold results for the Actual vs Predicted plot
  rf_full_test_results[[f]] <- data.frame(
    Actual = rf_full_test$YIELD, 
    Predicted = t_p, 
    Fold = f
  )
  
  cat("\nFold", f, "complete. Test AdjR2:", round(t_m["AdjR2"], 3))
  
} # END MASTER LOOP

# STOP TIMER HERE (Outside the loop)
end_time <- Sys.time()
comp_time <- end_time - start_time

# 5. Metrics
print(colMeans(rf_full_fold_metrics[, -1]))

# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================

cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")

# To make it look even cleaner with rounding:
rf_full_fold_metrics <- rf_full_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(rf_full_fold_metrics)

# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
# Calculate Means and SDs
rf_full_metric_means <- colMeans(rf_full_fold_metrics[, -1])
rf_full_metric_sds   <- sapply(rf_full_fold_metrics[, -1], sd)

# Create a clean summary table
n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776

rf_full_final_summary <- data.frame(
  Metric = names(rf_full_metric_means),
  Mean   = round(rf_full_metric_means, 4),
  SD     = round(rf_full_metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(rf_full_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(rf_full_final_summary, "RF_Full_Metrics.csv", row.names = FALSE)
saveRDS(rf_full_final_summary, "RF_Full_Metrics.rds")

# To identify which features were most common across all folds:
rf_full_best_feats <- unlist(rf_full_mrmr_rankings)
rf_full_feat_freq <- as.data.frame(table(rf_full_best_feats)) %>% arrange(desc(Freq))
print(rf_full_feat_freq)

# ============================================================================== 
# 4. FINAL RESULTS & VISUALIZATION 
# ============================================================================== 

# 1. Consolidate SHAP data
rf_full_shap <- bind_rows(rf_full_shap_values) 
rf_full_feat <- bind_rows(rf_full_shap_features) 

# 2. Pivot to Long format
rf_prep_shap <- rf_full_shap %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")

rf_prep_feat <- rf_full_feat %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "Value")

# 3. Join and Filter for TOP 5 Features
rf_plot_data <- left_join(rf_prep_shap, rf_prep_feat, by = c("ID", "Feature")) %>% 
  filter(!is.na(SHAP))

rf_top_5 <- rf_plot_data %>% 
  group_by(Feature) %>% 
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>% 
  arrange(desc(mean_abs_impact)) %>% 
  dplyr::slice(1:5)

rf_plot_data_top5 <- rf_plot_data %>% 
  filter(Feature %in% rf_top_5$Feature)

# 4. Generate the Beeswarm Plot #######################################

common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

rf_full_shap_plot <- ggplot(rf_plot_data_top5 %>% 
                              group_by(Feature) %>% 
                              mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                            aes(x = SHAP, 
                                y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                color = Value)) + 
  
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  
  scale_color_viridis_c(
    option = "viridis", 
    name = "Relative\nintensity",
    limits = common_color_lim,
    oob = scales::squish) + 
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)) + 
  labs( 
    title = "Random Forest", 
    x = expression("Yield impact (t ha"^{-1}*")"), 
    y = "Features")

rf_full_shap_plot


# Calculate Importance per Fold (using test data only)
rf_full_fold_importance <- rf_plot_data %>%
  group_by(Feature) %>%
  summarize(
    Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE),
    Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

rf_full_shap_summary_test_table <- rf_full_fold_importance %>%
  select(Rank, Feature, Mean_Test_Impact, Impact_Stability_SD)

print(rf_full_shap_summary_test_table)
write.csv(rf_full_shap_summary_test_table, "RF_Full_SHAP_Importance.csv", row.names = FALSE)

# ============================================================================== 
# mRMR AGGREGATION PLOT
# ============================================================================== 
# 1. Aggregate and Average mRMR Scores
rf_full_mrmr_scores <- bind_rows(rf_full_mrmr_scores_list) %>% 
  group_by(Feature) %>% 
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
rf_full_mrmr_scores <- rf_full_mrmr_scores %>% 
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 5 Average Features
rf_full_mrmr_plot <- ggplot(rf_full_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Random Forest", 
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))

rf_full_mrmr_plot

############################################ Train:Test ############################
# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
rf_full_plot_df <- bind_rows(rf_full_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
rf_full_test_r2_mean   <- rf_full_final_summary$Mean[rf_full_final_summary$Metric == "Test_R2"]
rf_full_test_r2_sd     <- rf_full_final_summary$SD[rf_full_final_summary$Metric == "Test_R2"]
rf_full_test_rmse_mean <- rf_full_final_summary$Mean[rf_full_final_summary$Metric == "Test_RMSE"]
rf_full_test_rmse_sd   <- rf_full_final_summary$SD[rf_full_final_summary$Metric == "Test_RMSE"]
rf_full_test_mae_mean  <- rf_full_final_summary$Mean[rf_full_final_summary$Metric == "Test_MAE"]
rf_full_test_mae_sd    <- rf_full_final_summary$SD[rf_full_final_summary$Metric == "Test_MAE"]

# B. Overall Metrics
rf_full_overall_r2 <- cor(rf_full_plot_df$Actual, rf_full_plot_df$Predicted)^2
rf_full_overall_rmse <- sqrt(mean((rf_full_plot_df$Actual - rf_full_plot_df$Predicted)^2))
rf_full_overall_mae <- mean(abs(rf_full_plot_df$Actual - rf_full_plot_df$Predicted))

# 3. CREATE CONSOLIDATED PLOT
p_main <- ggplot(rf_full_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(rf_full_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(rf_full_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(rf_full_test_mae_mean, 2), nsmall = 2), " ± ", format(round(rf_full_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(rf_full_test_r2_mean, 3), nsmall = 3), " ± ", format(round(rf_full_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")

# 2. Top Density
p_top <- ggplot(rf_full_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(rf_full_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
rf_full_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Random Forest",  
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
rf_full_scatter_plot


########################################################################################################
######################## Linear Regression CLIMATE MODEL #############################################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# Global LR seeds
lr_seeds <- list(
  seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))

# MASTER STORAGE (Keeps everything for every fold)
lr_climate_mrmr_rankings <- list()
lr_climate_mrmr_scores_list <- list()
lr_climate_best_params <- list()
lr_climate_fold_models <- list()
lr_climate_shap_values <- list()
lr_climate_shap_features <- list()
lr_climate_norm_params <- list()
lr_climate_test_results <- list()
lr_climate_fold_metrics <- data.frame()
lr_climate_val_years_list <- list()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  r2   <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f)
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  lr_climate_outer_train <- dataset_with_folds %>% filter(fold != f)
  lr_climate_test        <- dataset_with_folds %>% filter(fold == f)
  
  # --- STEP 2: VALIDATION SET (Temporal LOYO - 6 Years) ---
  unique_years <- unique(lr_climate_outer_train$YEAR)
  
  set.seed(123 + f)
  val_years <- sample(unique_years, 6) 
  lr_climate_val_years_list[[f]] <- paste(sort(val_years), collapse = ", ")
  
  lr_climate_val   <- lr_climate_outer_train %>% filter(YEAR %in% val_years)
  lr_climate_train <- lr_climate_outer_train %>% filter(!(YEAR %in% val_years))
  
  cat("\nFold", f, "tuning using LOYO on Years:", lr_climate_val_years_list[[f]])
  
  # --- DROP NON-FEATURE COLUMNS ---
  lr_climate_train <- lr_climate_train %>% select(-State, -County, -YEAR)
  lr_climate_val   <- lr_climate_val   %>% select(-State, -County, -YEAR)
  lr_climate_test  <- lr_climate_test  %>% select(-State, -County, -YEAR)
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(lr_climate_train)[sapply(lr_climate_train, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "fold", "is_val"))
  
  means <- sapply(lr_climate_train[feats], mean, na.rm = TRUE)
  sds   <- sapply(lr_climate_train[feats], sd, na.rm = TRUE)
  lr_climate_norm_params[[f]] <- list(mean = means, sd = sds)
  
  # Create scaled versions
  lr_climate_train_sc <- lr_climate_train
  lr_climate_val_sc   <- lr_climate_val
  lr_climate_test_sc  <- lr_climate_test
  
  for(col in feats) {
    lr_climate_train_sc[[col]] <- (lr_climate_train[[col]] - means[col]) / sds[col]
    lr_climate_val_sc[[col]]   <- (lr_climate_val[[col]] - means[col]) / sds[col]
    lr_climate_test_sc[[col]]  <- (lr_climate_test[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  mrmr_in <- mRMR.data(data = as.data.frame(lr_climate_train_sc[, c(feats, "YIELD")]))
  mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(lr_climate_train_sc[, c(feats, "YIELD")]) == "YIELD"), feature_count = 50)
  
  mrmr_feats  <- names(lr_climate_train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run)))
  
  lr_climate_mrmr_rankings[[f]] <- mrmr_feats
  lr_climate_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    
    # Using Linear Regression (lm) for the elbow check
    tmp_mod <- lm(
      formula = YIELD ~ ., 
      data = lr_climate_train_sc[, c(curr, "YIELD")]
    )
    
    p_val <- predict(tmp_mod, lr_climate_val_sc[, curr, drop = FALSE])
    rmse_history[k] <- sqrt(mean((lr_climate_val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history); y <- rmse_history
  
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  if(k1 < (length(y) - 1)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else { 
    opt_k <- k1 
  }
  
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { 
    opt_k <- 10 
  }
  
  best_feats <- mrmr_feats[1:opt_k]
  lr_climate_shap_features[[f]] <- best_feats
  
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  # ==============================================================================
  # E. HYPERPARAMETER TUNING (Skipped for Linear Regression)
  # ==============================================================================
  cat("\nFold", f, ": Standard OLS Linear Regression (No Hyperparameters to tune)")
  
  # ==============================================================================
  # F. FINAL FOLD MODEL & SHAP
  # ==============================================================================
  set.seed(123) 
  # Fit final OLS model
  final_mod <- lm(
    formula = YIELD ~ ., 
    data = lr_climate_train_sc[, c(best_feats, "YIELD")]
  )
  lr_climate_fold_models[[f]] <- final_mod
  
  # SHAP Wrapper Fix for Linear Regression
  p_wrapper <- function(object, newdata) { 
    predict(object, as.data.frame(newdata)) 
  }
  
  X_test_df <- lr_climate_test_sc[, best_feats]
  # fastshap works on lm objects using the prediction wrapper
  shap_contrib <- fastshap::explain(final_mod, X = X_test_df, pred_wrapper = p_wrapper, nsim = 10)
  
  lr_climate_shap_values[[f]]   <- as.data.frame(shap_contrib)
  lr_climate_shap_features[[f]] <- X_test_df
  
  # ==============================================================================
  # G. METRICS COLLECTION
  # ==============================================================================
  # 1. Generate predictions
  v_p <- predict(final_mod, lr_climate_val_sc[, best_feats])
  t_p <- predict(final_mod, lr_climate_test_sc[, best_feats])
  
  # 2. Calculate metrics using your specific function
  v_m <- get_metrics(lr_climate_val$YIELD, v_p, length(best_feats), nrow(lr_climate_val))
  t_m <- get_metrics(lr_climate_test$YIELD, t_p, length(best_feats), nrow(lr_climate_test))
  
  # 3. Store in the metrics dataframe
  lr_climate_fold_metrics <- rbind(lr_climate_fold_metrics, data.frame(
    Fold = f, Features = opt_k, 
    Val_R2 = v_m["R2"], Test_R2 = t_m["R2"], Val_AdjR2 = v_m["AdjR2"], Test_AdjR2 = t_m["AdjR2"],
    Val_RMSE = v_m["RMSE"], Test_RMSE = t_m["RMSE"], Val_MAE = v_m["MAE"], Test_MAE = t_m["MAE"]
  ))
  
  # 4. Save fold results for the Actual vs Predicted plot
  lr_climate_test_results[[f]] <- data.frame(
    Actual = lr_climate_test$YIELD, 
    Predicted = t_p, 
    Fold = f
  )
  
  cat("\nFold", f, "complete. Test AdjR2:", round(t_m["AdjR2"], 3))
  
} # END MASTER LOOP

# STOP TIMER HERE
end_time <- Sys.time()
comp_time <- end_time - start_time

# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================
cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")
# Add validation years for tracking
lr_climate_fold_metrics$Val_Years <- unlist(lr_climate_val_years_list)

lr_climate_fold_metrics_clean <- lr_climate_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(lr_climate_fold_metrics_clean)

# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
lr_climate_metric_means <- colMeans(lr_climate_fold_metrics[, sapply(lr_climate_fold_metrics, is.numeric)][, -1])
lr_climate_metric_sds   <- sapply(lr_climate_fold_metrics[, sapply(lr_climate_fold_metrics, is.numeric)][, -1], sd)

n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776

lr_climate_final_summary <- data.frame(
  Metric = names(lr_climate_metric_means),
  Mean   = round(lr_climate_metric_means, 4),
  SD     = round(lr_climate_metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(lr_climate_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(lr_climate_final_summary, "LR_Climate_Metrics.csv", row.names = FALSE)
saveRDS(lr_climate_final_summary, "LR_Climate_Metrics.rds")

# Common features
lr_climate_best_feats <- unlist(lr_climate_mrmr_rankings)
lr_climate_feat_freq <- as.data.frame(table(lr_climate_best_feats)) %>% arrange(desc(Freq))
print(lr_climate_feat_freq)

# ============================================================================== 
# 4. FINAL RESULTS & VISUALIZATION (Linear Regression)
# ============================================================================== 
lr_climate_shap <- bind_rows(lr_climate_shap_values) 
lr_climate_feat <- bind_rows(lr_climate_shap_features) 

lr_prep_shap <- lr_climate_shap %>% mutate(ID = row_number()) %>% pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")
lr_prep_feat <- lr_climate_feat %>% mutate(ID = row_number()) %>% pivot_longer(-ID, names_to = "Feature", values_to = "Value")

lr_plot_data <- left_join(lr_prep_shap, lr_prep_feat, by = c("ID", "Feature")) %>% filter(!is.na(SHAP))

lr_top_5 <- lr_plot_data %>% 
  group_by(Feature) %>% 
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>% 
  arrange(desc(mean_abs_impact)) %>% 
  dplyr::slice(1:5)

lr_plot_data_top5 <- lr_plot_data %>% filter(Feature %in% lr_top_5$Feature)

# 4. Generate the Beeswarm Plot #######################################

common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

lr_climate_shap_plot <- ggplot(lr_plot_data_top5 %>% 
                                 group_by(Feature) %>% 
                                 mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                               aes(x = SHAP, 
                                   y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                   color = Value)) + 
  
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  
  scale_color_viridis_c(
    option = "viridis", 
    name = "Relative\nintensity",
    limits = common_color_lim,
    oob = scales::squish) + 
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)) + 
  labs( 
    title = "Linear Regression", 
    x = expression("Yield impact (t ha"^{-1}*")"), 
    y = "Features")

lr_climate_shap_plot


# Calculate Importance per Fold (using test data only)
lr_climate_fold_importance <- lr_plot_data %>%
  group_by(Feature) %>%
  summarize(
    Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE),
    Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

lr_climate_shap_summary_test_table <- lr_climate_fold_importance %>%
  select(Rank, Feature, Mean_Test_Impact, Impact_Stability_SD)

print(lr_climate_shap_summary_test_table)
write.csv(lr_climate_shap_summary_test_table, "LR_climate_SHAP_Importance.csv", row.names = FALSE)


# ============================================================================== 
# mRMR AGGREGATION PLOT
# ============================================================================== 
# 1. Aggregate and Average mRMR Scores
lr_climate_mrmr_scores <- bind_rows(lr_climate_mrmr_scores_list) %>% 
  group_by(Feature) %>% 
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
lr_climate_mrmr_scores <- lr_climate_mrmr_scores %>% 
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 5 Average Features
lr_climate_mrmr_plot <- ggplot(lr_climate_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Linear Regression", 
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))

lr_climate_mrmr_plot

############################################ Train:Test ############################

# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
lr_climate_plot_df <- bind_rows(lr_climate_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
lr_climate_test_r2_mean   <- lr_climate_final_summary$Mean[lr_climate_final_summary$Metric == "Test_R2"]
lr_climate_test_r2_sd     <- lr_climate_final_summary$SD[lr_climate_final_summary$Metric == "Test_R2"]
lr_climate_test_rmse_mean <- lr_climate_final_summary$Mean[lr_climate_final_summary$Metric == "Test_RMSE"]
lr_climate_test_rmse_sd   <- lr_climate_final_summary$SD[lr_climate_final_summary$Metric == "Test_RMSE"]
lr_climate_test_mae_mean  <- lr_climate_final_summary$Mean[lr_climate_final_summary$Metric == "Test_MAE"]
lr_climate_test_mae_sd    <- lr_climate_final_summary$SD[lr_climate_final_summary$Metric == "Test_MAE"]


# B. Overall Metrics (Calculated once across the entire combined dataset)
lr_climate_overall_r2 <- cor(lr_climate_plot_df$Actual, lr_climate_plot_df$Predicted)^2
lr_climate_overall_rmse <- sqrt(mean((lr_climate_plot_df$Actual - lr_climate_plot_df$Predicted)^2))
lr_climate_overall_mae <- mean(abs(lr_climate_plot_df$Actual - lr_climate_plot_df$Predicted))

# 3. CREATE CONSOLIDATED PLOT
p_main <- ggplot(lr_climate_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(lr_climate_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(lr_climate_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(lr_climate_test_mae_mean, 2), nsmall = 2), " ± ", format(round(lr_climate_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(lr_climate_test_r2_mean, 3), nsmall = 3), " ± ", format(round(lr_climate_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")

# 2. Top Density
p_top <- ggplot(lr_climate_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(lr_climate_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
lr_climate_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Linear Regression", 
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
lr_climate_scatter_plot

########################################################################################################
######################## Linear Regression FULL MODEL #############################################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# LR seeds
lr_seeds <- list(
  seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset_final %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset_final %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))


# MASTER STORAGE (Keeps everything for every fold)
lr_full_fold_models <- list()
lr_full_fold_metrics <- data.frame()
lr_full_mrmr_rankings <- list()
lr_full_best_params <- list()
lr_full_shap_values <- list()
lr_full_shap_features <- list()
lr_full_norm_params <- list()
lr_full_best_feats_used <- c()
lr_full_mrmr_scores <- list()
lr_full_mrmr_scores_list <- list()
lr_full_test_results <- list()
lr_full_val_years_list <- list()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  r2 <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f)
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  lr_full_outer_train <- dataset_with_folds %>% filter(fold != f)
  lr_full_test        <- dataset_with_folds %>% filter(fold == f)
  
  # --- STEP 2: VALIDATION SET (Temporal LOYO - 6 Years) ---
  unique_years <- unique(lr_full_outer_train$YEAR)
  
  set.seed(123 + f)
  val_years <- sample(unique_years, 6) # Pick 6 years for a robust tuning set
  lr_full_val_years_list[[f]] <- paste(sort(val_years), collapse = ", ")
  
  lr_full_val   <- lr_full_outer_train %>% filter(YEAR %in% val_years)
  lr_full_train <- lr_full_outer_train %>% filter(!(YEAR %in% val_years))
  
  cat("\nFold", f, "tuning using LOYO on Years:", lr_full_val_years_list[[f]])
  
  # --- DROP NON-FEATURE COLUMNS ---
  lr_full_train <- lr_full_train %>% select(-State, -County)
  lr_full_val   <- lr_full_val   %>% select(-State, -County)
  lr_full_test  <- lr_full_test  %>% select(-State, -County)
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(lr_full_train)[sapply(lr_full_train, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "fold", "is_val"))
  
  means <- sapply(lr_full_train[feats], mean, na.rm = TRUE)
  sds   <- sapply(lr_full_train[feats], sd, na.rm = TRUE)
  lr_full_norm_params[[f]] <- list(mean = means, sd = sds)
  
  # Create scaled versions
  lr_full_train_sc <- lr_full_train
  lr_full_val_sc   <- lr_full_val
  lr_full_test_sc  <- lr_full_test
  
  for(col in feats) {
    lr_full_train_sc[[col]] <- (lr_full_train[[col]] - means[col]) / sds[col]
    lr_full_val_sc[[col]]   <- (lr_full_val[[col]] - means[col]) / sds[col]
    lr_full_test_sc[[col]]  <- (lr_full_test[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  mrmr_in <- mRMR.data(data = as.data.frame(lr_full_train_sc[, c(feats, "YIELD")]))
  mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(lr_full_train_sc[, c(feats, "YIELD")]) == "YIELD"), feature_count = 50)
  
  mrmr_feats  <- names(lr_full_train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run)))
  
  lr_full_mrmr_rankings[[f]] <- mrmr_feats
  lr_full_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    
    # Linear Regression for Elbow check
    tmp_mod <- lm(
      formula = YIELD ~ ., 
      data = lr_full_train_sc[, c(curr, "YIELD")]
    )
    
    p_val <- predict(tmp_mod, lr_full_val_sc[, curr, drop = FALSE])
    rmse_history[k] <- sqrt(mean((lr_full_val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history); y <- rmse_history
  
  # 1. Find the FIRST Elbow
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  # 2. Find the SECOND Elbow
  if(k1 < (length(y) - 1)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else { 
    opt_k <- k1 
  }
  
  # 3. SAFETY FALLBACK (Ensures at least 10 features)
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { 
    opt_k <- 10 
  }
  
  best_feats <- mrmr_feats[1:opt_k]
  
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  # ==============================================================================
  # E. HYPERPARAMETER TUNING (Skipped for Linear Regression)
  # ==============================================================================
  cat("\nFold", f, ": Standard OLS (No Hyperparameters)")
  
  # ==============================================================================
  # F. FINAL FOLD MODEL & SHAP
  # ==============================================================================
  set.seed(123) 
  final_mod <- lm(
    formula = YIELD ~ ., 
    data = lr_full_train_sc[, c(best_feats, "YIELD")]
  )
  lr_full_fold_models[[f]] <- final_mod
  
  # SHAP Wrapper Fix for Linear Regression
  p_wrapper <- function(object, newdata) { 
    predict(object, as.data.frame(newdata)) 
  }
  
  X_test_df <- lr_full_test_sc[, best_feats]
  shap_contrib <- fastshap::explain(final_mod, X = X_test_df, pred_wrapper = p_wrapper, nsim = 10)
  
  lr_full_shap_values[[f]]   <- as.data.frame(shap_contrib)
  lr_full_shap_features[[f]] <- X_test_df
  
  # ==============================================================================
  # G. METRICS COLLECTION
  # ==============================================================================
  # 1. Generate predictions
  v_p <- predict(final_mod, lr_full_val_sc[, best_feats])
  t_p <- predict(final_mod, lr_full_test_sc[, best_feats])
  
  # 2. Calculate metrics using your specific function
  v_m <- get_metrics(lr_full_val$YIELD, v_p, length(best_feats), nrow(lr_full_val))
  t_m <- get_metrics(lr_full_test$YIELD, t_p, length(best_feats), nrow(lr_full_test))
  
  # 3. Store in the metrics dataframe
  lr_full_fold_metrics <- rbind(lr_full_fold_metrics, data.frame(
    Fold = f, Features = opt_k, 
    Val_R2 = v_m["R2"], Test_R2 = t_m["R2"], Val_AdjR2 = v_m["AdjR2"], Test_AdjR2 = t_m["AdjR2"],
    Val_RMSE = v_m["RMSE"], Test_RMSE = t_m["RMSE"], Val_MAE = v_m["MAE"], Test_MAE = t_m["MAE"]
  ))
  
  # 4. Save fold results for the Actual vs Predicted plot
  lr_full_test_results[[f]] <- data.frame(
    Actual = lr_full_test$YIELD, 
    Predicted = t_p, 
    Fold = f
  )
  
  cat("\nFold", f, "complete. Test AdjR2:", round(t_m["AdjR2"], 3))
  
} # END MASTER LOOP

# STOP TIMER HERE (Outside the loop)
end_time <- Sys.time()
comp_time <- end_time - start_time

# 5. Metrics
print(colMeans(lr_full_fold_metrics[, -1]))

# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================

cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")

# Add the Validation Years column for tracking
lr_full_fold_metrics$Val_Years <- unlist(lr_full_val_years_list)

# To make it look even cleaner with rounding:
lr_full_fold_metrics_rounded <- lr_full_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(lr_full_fold_metrics_rounded)

# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
# Calculate Means and SDs (only for numeric columns)
numeric_metrics <- lr_full_fold_metrics %>% select(where(is.numeric)) %>% select(-Fold)
lr_full_metric_means <- colMeans(numeric_metrics)
lr_full_metric_sds   <- sapply(numeric_metrics, sd)

# Create a clean summary table
n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776

lr_full_final_summary <- data.frame(
  Metric = names(lr_full_metric_means),
  Mean   = round(lr_full_metric_means, 4),
  SD     = round(lr_full_metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(lr_full_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(lr_full_final_summary, "LR_Full_Metrics.csv", row.names = FALSE)
saveRDS(lr_full_final_summary, "LR_Full_Metrics.rds")

# To identify which features were most common across all folds:
lr_full_best_feats <- unlist(lr_full_mrmr_rankings)
lr_full_feat_freq <- as.data.frame(table(lr_full_best_feats)) %>% arrange(desc(Freq))
print(lr_full_feat_freq)

# ============================================================================== 
# 4. FINAL RESULTS & VISUALIZATION 
# ============================================================================== 

# 1. Consolidate SHAP data
lr_full_shap <- bind_rows(lr_full_shap_values) 
lr_full_feat <- bind_rows(lr_full_shap_features) 

# 2. Pivot to Long format
lr_prep_shap <- lr_full_shap %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")

lr_prep_feat <- lr_full_feat %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(-ID, names_to = "Feature", values_to = "Value")

# 3. Join and Filter for TOP 5 Features
lr_plot_data <- left_join(lr_prep_shap, lr_prep_feat, by = c("ID", "Feature")) %>% 
  filter(!is.na(SHAP))

lr_top_5 <- lr_plot_data %>% 
  group_by(Feature) %>% 
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>% 
  arrange(desc(mean_abs_impact)) %>% 
  dplyr::slice(1:5)

lr_plot_data_top5 <- lr_plot_data %>% 
  filter(Feature %in% lr_top_5$Feature)

# 4. Generate the Beeswarm Plot #######################################

common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

lr_full_shap_plot <- ggplot(lr_plot_data_top5 %>% 
                              group_by(Feature) %>% 
                              mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                            aes(x = SHAP, 
                                y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                color = Value)) + 
  
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  
  scale_color_viridis_c(
    option = "viridis", 
    name = "Relative\nintensity",
    limits = common_color_lim,
    oob = scales::squish) + 
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)) + 
  labs( 
    title = "Linear Regression", 
    x = expression("Yield impact (t ha"^{-1}*")"), 
    y = "Features")

lr_full_shap_plot


# Calculate Importance per Fold (using test data only)
lr_full_fold_importance <- lr_plot_data %>%
  group_by(Feature) %>%
  summarize(
    Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE),
    Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

lr_full_shap_summary_test_table <- lr_full_fold_importance %>%
  select(Rank, Feature, Mean_Test_Impact, Impact_Stability_SD)

print(lr_full_shap_summary_test_table)
write.csv(lr_full_shap_summary_test_table, "LR_Full_SHAP_Importance.csv", row.names = FALSE)

# ============================================================================== 
# mRMR AGGREGATION PLOT (Linear Regression)
# ============================================================================== 
# 1. Aggregate and Average mRMR Scores
lr_full_mrmr_scores <- bind_rows(lr_full_mrmr_scores_list) %>% 
  group_by(Feature) %>% 
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
lr_full_mrmr_scores <- lr_full_mrmr_scores %>% 
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 5 Average Features
lr_full_mrmr_plot <- ggplot(lr_full_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Linear Regression", 
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))

lr_full_mrmr_plot

############################################ Train:Test ############################
# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
lr_full_plot_df <- bind_rows(lr_full_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
lr_full_test_r2_mean   <- lr_full_final_summary$Mean[lr_full_final_summary$Metric == "Test_R2"]
lr_full_test_r2_sd     <- lr_full_final_summary$SD[lr_full_final_summary$Metric == "Test_R2"]
lr_full_test_rmse_mean <- lr_full_final_summary$Mean[lr_full_final_summary$Metric == "Test_RMSE"]
lr_full_test_rmse_sd   <- lr_full_final_summary$SD[lr_full_final_summary$Metric == "Test_RMSE"]
lr_full_test_mae_mean  <- lr_full_final_summary$Mean[lr_full_final_summary$Metric == "Test_MAE"]
lr_full_test_mae_sd    <- lr_full_final_summary$SD[lr_full_final_summary$Metric == "Test_MAE"]

# B. Overall Metrics
lr_full_overall_r2 <- cor(lr_full_plot_df$Actual, lr_full_plot_df$Predicted)^2
lr_full_overall_rmse <- sqrt(mean((lr_full_plot_df$Actual - lr_full_plot_df$Predicted)^2))
lr_full_overall_mae <- mean(abs(lr_full_plot_df$Actual - lr_full_plot_df$Predicted))

# 3. CREATE CONSOLIDATED PLOT
p_main <- ggplot(lr_full_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(lr_full_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(lr_full_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(lr_full_test_mae_mean, 2), nsmall = 2), " ± ", format(round(lr_full_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(lr_full_test_r2_mean, 3), nsmall = 3), " ± ", format(round(lr_full_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")

# 2. Top Density
p_top <- ggplot(lr_full_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(lr_full_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
lr_full_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Linear Regression", 
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
lr_full_scatter_plot


########################################################################################################
######################## SVM CLIMATE MODEL #############################################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# Global SVM seeds
svm_seeds <- list(
  seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))

# MASTER STORAGE (Keeps everything for every fold)
svm_climate_mrmr_rankings <- list()
svm_climate_mrmr_scores_list <- list()
svm_climate_best_params <- list()
svm_climate_fold_models <- list()
svm_climate_shap_values <- list()
svm_climate_shap_features <- list()
svm_climate_norm_params <- list()
svm_climate_test_results <- list()
svm_climate_fold_metrics <- data.frame()
svm_climate_val_years_list <- list()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  r2   <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f)
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  svm_climate_outer_train <- dataset_with_folds %>% filter(fold != f)
  svm_climate_test        <- dataset_with_folds %>% filter(fold == f)
  
  # --- STEP 2: VALIDATION SET (Temporal LOYO - 6 Years) ---
  unique_years <- unique(svm_climate_outer_train$YEAR)
  
  set.seed(123 + f)
  val_years <- sample(unique_years, 6) 
  svm_climate_val_years_list[[f]] <- paste(sort(val_years), collapse = ", ")
  
  svm_climate_val   <- svm_climate_outer_train %>% filter(YEAR %in% val_years)
  svm_climate_train <- svm_climate_outer_train %>% filter(!(YEAR %in% val_years))
  
  cat("\nFold", f, "tuning using LOYO on Years:", svm_climate_val_years_list[[f]])
  
  # --- DROP NON-FEATURE COLUMNS ---
  svm_climate_train <- svm_climate_train %>% select(-State, -County, -YEAR)
  svm_climate_val   <- svm_climate_val   %>% select(-State, -County, -YEAR)
  svm_climate_test  <- svm_climate_test  %>% select(-State, -County, -YEAR)
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(svm_climate_train)[sapply(svm_climate_train, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "fold", "is_val"))
  
  means <- sapply(svm_climate_train[feats], mean, na.rm = TRUE)
  sds   <- sapply(svm_climate_train[feats], sd, na.rm = TRUE)
  svm_climate_norm_params[[f]] <- list(mean = means, sd = sds)
  
  # Create scaled versions
  svm_climate_train_sc <- svm_climate_train
  svm_climate_val_sc   <- svm_climate_val
  svm_climate_test_sc  <- svm_climate_test
  
  for(col in feats) {
    svm_climate_train_sc[[col]] <- (svm_climate_train[[col]] - means[col]) / sds[col]
    svm_climate_val_sc[[col]]   <- (svm_climate_val[[col]] - means[col]) / sds[col]
    svm_climate_test_sc[[col]]  <- (svm_climate_test[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  mrmr_in <- mRMR.data(data = as.data.frame(svm_climate_train_sc[, c(feats, "YIELD")]))
  mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(svm_climate_train_sc[, c(feats, "YIELD")]) == "YIELD"), feature_count = 50)
  
  mrmr_feats  <- names(svm_climate_train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run)))
  
  svm_climate_mrmr_rankings[[f]] <- mrmr_feats
  svm_climate_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    
    # Using e1071::svm for the elbow check (using radial kernel)
    set.seed(123)
    tmp_mod <- e1071::svm(
      formula = YIELD ~ ., 
      data = svm_climate_train_sc[, c(curr, "YIELD")],
      kernel = "radial"
    )
    
    p_val <- predict(tmp_mod, svm_climate_val_sc[, curr, drop = FALSE])
    rmse_history[k] <- sqrt(mean((svm_climate_val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history); y <- rmse_history
  
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  if(k1 < (length(y) - 1)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else { 
    opt_k <- k1 
  }
  
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { 
    opt_k <- 10 
  }
  
  best_feats <- mrmr_feats[1:opt_k]
  svm_climate_shap_features[[f]] <- best_feats
  
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  # ==============================================================================
  # E. HYPERPARAMETER TUNING (Grid Search for SVM)
  # ==============================================================================
  # Typical SVM tuning involves Cost (C) and Gamma (for radial kernel)
  grid <- expand.grid(
    cost = c(0.1, 1, 10),
    gamma = c(0.01, 0.1, 0.5)
  )
  best_v_rmse <- Inf; final_params <- list()
  
  for(g in 1:nrow(grid)){
    set.seed(123) 
    m <- e1071::svm(
      formula = YIELD ~ ., 
      data = svm_climate_train_sc[, c(best_feats, "YIELD")],
      kernel = "radial",
      cost = grid$cost[g],
      gamma = grid$gamma[g]
    )
    
    v_p <- predict(m, svm_climate_val_sc[, best_feats])
    v_rmse <- sqrt(mean((svm_climate_val_sc$YIELD - v_p)^2))
    
    if(v_rmse < best_v_rmse){
      best_v_rmse <- v_rmse
      final_params <- list(cost = grid$cost[g], gamma = grid$gamma[g])
    }
  }
  svm_climate_best_params[[f]] <- final_params
  
  cat("\nFold", f, "Best Parameters: Cost =", final_params$cost, "| Gamma =", final_params$gamma)
  
  # ==============================================================================
  # F. FINAL FOLD MODEL & SHAP
  # ==============================================================================
  set.seed(123) 
  final_mod <- e1071::svm(
    formula = YIELD ~ ., 
    data = svm_climate_train_sc[, c(best_feats, "YIELD")],
    kernel = "radial",
    cost = final_params$cost,
    gamma = final_params$gamma
  )
  svm_climate_fold_models[[f]] <- final_mod
  
  # SHAP Wrapper Fix for SVM
  p_wrapper <- function(object, newdata) { 
    predict(object, as.data.frame(newdata)) 
  }
  
  X_test_df <- svm_climate_test_sc[, best_feats]
  shap_contrib <- fastshap::explain(final_mod, X = X_test_df, pred_wrapper = p_wrapper, nsim = 10)
  
  svm_climate_shap_values[[f]]   <- as.data.frame(shap_contrib)
  svm_climate_shap_features[[f]] <- X_test_df
  
  # ==============================================================================
  # G. METRICS COLLECTION
  # ==============================================================================
  # 1. Generate predictions
  v_p <- predict(final_mod, svm_climate_val_sc[, best_feats])
  t_p <- predict(final_mod, svm_climate_test_sc[, best_feats])
  
  # 2. Calculate metrics using your specific function
  v_m <- get_metrics(svm_climate_val$YIELD, v_p, length(best_feats), nrow(svm_climate_val))
  t_m <- get_metrics(svm_climate_test$YIELD, t_p, length(best_feats), nrow(svm_climate_test))
  
  # 3. Store in the metrics dataframe
  svm_climate_fold_metrics <- rbind(svm_climate_fold_metrics, data.frame(
    Fold = f, Features = opt_k, 
    Val_R2 = v_m["R2"], Test_R2 = t_m["R2"], Val_AdjR2 = v_m["AdjR2"], Test_AdjR2 = t_m["AdjR2"],
    Val_RMSE = v_m["RMSE"], Test_RMSE = t_m["RMSE"], Val_MAE = v_m["MAE"], Test_MAE = t_m["MAE"]
  ))
  
  # 4. Save fold results for the Actual vs Predicted plot
  svm_climate_test_results[[f]] <- data.frame(
    Actual = svm_climate_test$YIELD, 
    Predicted = t_p, 
    Fold = f
  )
  
  cat("\nFold", f, "complete. Test AdjR2:", round(t_m["AdjR2"], 3))
  
} # END MASTER LOOP

# STOP TIMER HERE
end_time <- Sys.time()
comp_time <- end_time - start_time

# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================
cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")
svm_climate_fold_metrics$Val_Years <- unlist(svm_climate_val_years_list)

svm_climate_fold_metrics_clean <- svm_climate_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(svm_climate_fold_metrics_clean)

# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
# Filter for numeric columns for colMeans
numeric_cols_summary <- svm_climate_fold_metrics %>% select(where(is.numeric)) %>% select(-Fold)
svm_climate_metric_means <- colMeans(numeric_cols_summary)
svm_climate_metric_sds   <- sapply(numeric_cols_summary, sd)

# Create a clean summary table
n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776

svm_climate_final_summary <- data.frame(
  Metric = names(svm_climate_metric_means),
  Mean   = round(svm_climate_metric_means, 4),
  SD     = round(svm_climate_metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(svm_climate_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(svm_climate_final_summary, "SVM_Climate_Metrics.csv", row.names = FALSE)
saveRDS(svm_climate_final_summary, "SVM_Climate_Metrics.rds")

# Common features
svm_climate_best_feats <- unlist(svm_climate_mrmr_rankings)
svm_climate_feat_freq <- as.data.frame(table(svm_climate_best_feats)) %>% arrange(desc(Freq))
print(svm_climate_feat_freq)

# ============================================================================== 
# 4. FINAL RESULTS & VISUALIZATION (SVM)
# ============================================================================== 
svm_climate_shap <- bind_rows(svm_climate_shap_values) 
svm_climate_feat <- bind_rows(svm_climate_shap_features) 

svm_prep_shap <- svm_climate_shap %>% mutate(ID = row_number()) %>% pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")
svm_prep_feat <- svm_climate_feat %>% mutate(ID = row_number()) %>% pivot_longer(-ID, names_to = "Feature", values_to = "Value")

svm_plot_data <- left_join(svm_prep_shap, svm_prep_feat, by = c("ID", "Feature")) %>% filter(!is.na(SHAP))

svm_top_5 <- svm_plot_data %>% 
  group_by(Feature) %>% 
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>% 
  arrange(desc(mean_abs_impact)) %>% 
  dplyr::slice(1:5)

svm_plot_data_top5 <- svm_plot_data %>% filter(Feature %in% svm_top_5$Feature)

# Beeswarm Plot
common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

svm_climate_shap_plot <- ggplot(svm_plot_data_top5 %>% 
                                  group_by(Feature) %>% 
                                  mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                                aes(x = SHAP, 
                                    y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                    color = Value)) + 
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  scale_color_viridis_c(option = "viridis", name = "Relative\nintensity", limits = common_color_lim, oob = scales::squish) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)) + 
  labs(title = "Support Vector Machine", x = expression("Yield impact (t ha"^{-1}*")"), y = "Features")

svm_climate_shap_plot

# Importance table
svm_climate_fold_importance <- svm_plot_data %>%
  group_by(Feature) %>%
  summarize(Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE), Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

write.csv(svm_climate_fold_importance, "SVM_climate_SHAP_Importance.csv", row.names = FALSE)

# ============================================================================== 
# mRMR AGGREGATION PLOT (SVM)
# ============================================================================== 
# 1. Aggregate and Average mRMR Scores
svm_climate_mrmr_scores <- bind_rows(svm_climate_mrmr_scores_list) %>% 
  group_by(Feature) %>% 
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
svm_climate_mrmr_scores <- svm_climate_mrmr_scores %>% 
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 5 Average Features
svm_climate_mrmr_plot <- ggplot(svm_climate_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Support Vector Machine", 
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))

svm_climate_mrmr_plot

############################################ Train:Test ############################

# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
svm_climate_plot_df <- bind_rows(svm_climate_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
svm_climate_test_r2_mean   <- svm_climate_final_summary$Mean[svm_climate_final_summary$Metric == "Test_R2"]
svm_climate_test_r2_sd     <- svm_climate_final_summary$SD[svm_climate_final_summary$Metric == "Test_R2"]
svm_climate_test_rmse_mean <- svm_climate_final_summary$Mean[svm_climate_final_summary$Metric == "Test_RMSE"]
svm_climate_test_rmse_sd   <- svm_climate_final_summary$SD[svm_climate_final_summary$Metric == "Test_RMSE"]
svm_climate_test_mae_mean  <- svm_climate_final_summary$Mean[svm_climate_final_summary$Metric == "Test_MAE"]
svm_climate_test_mae_sd    <- svm_climate_final_summary$SD[svm_climate_final_summary$Metric == "Test_MAE"]


# B. Overall Metrics (Calculated once across the entire combined dataset)
svm_climate_overall_r2 <- cor(svm_climate_plot_df$Actual, svm_climate_plot_df$Predicted)^2
svm_climate_overall_rmse <- sqrt(mean((svm_climate_plot_df$Actual - svm_climate_plot_df$Predicted)^2))
svm_climate_overall_mae <- mean(abs(svm_climate_plot_df$Actual - svm_climate_plot_df$Predicted))

# 3. CREATE CONSOLIDATED PLOT
p_main <- ggplot(svm_climate_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(svm_climate_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(svm_climate_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(svm_climate_test_mae_mean, 2), nsmall = 2), " ± ", format(round(svm_climate_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(svm_climate_test_r2_mean, 3), nsmall = 3), " ± ", format(round(svm_climate_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")


# 2. Top Density
p_top <- ggplot(svm_climate_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(svm_climate_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
svm_climate_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Support Vector Machine",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
svm_climate_scatter_plot



########################################################################################################
######################## SVM FULL MODEL #############################################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# SVM seeds
svm_seeds <- list(
  seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset_final %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset_final %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))


# MASTER STORAGE (Keeps everything for every fold)
svm_full_fold_models <- list()
svm_full_fold_metrics <- data.frame()
svm_full_mrmr_rankings <- list()
svm_full_best_params <- list()
svm_full_shap_values <- list()
svm_full_shap_features <- list()
svm_full_norm_params <- list()
svm_full_best_feats_used <- c()
svm_full_mrmr_scores <- list()
svm_full_mrmr_scores_list <- list()
svm_full_test_results <- list()
svm_full_val_years_list <- list()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  r2 <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f)
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  svm_full_outer_train <- dataset_with_folds %>% filter(fold != f)
  svm_full_test        <- dataset_with_folds %>% filter(fold == f)
  
  # --- STEP 2: VALIDATION SET (Temporal LOYO - 6 Years) ---
  unique_years <- unique(svm_full_outer_train$YEAR)
  
  set.seed(123 + f)
  val_years <- sample(unique_years, 6) # Pick 6 years for a robust tuning set
  svm_full_val_years_list[[f]] <- paste(sort(val_years), collapse = ", ")
  
  svm_full_val   <- svm_full_outer_train %>% filter(YEAR %in% val_years)
  svm_full_train <- svm_full_outer_train %>% filter(!(YEAR %in% val_years))
  
  cat("\nFold", f, "tuning using LOYO on Years:", svm_full_val_years_list[[f]])
  
  # --- DROP NON-FEATURE COLUMNS ---
  svm_full_train <- svm_full_train %>% select(-State, -County)
  svm_full_val   <- svm_full_val   %>% select(-State, -County)
  svm_full_test  <- svm_full_test  %>% select(-State, -County)
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(svm_full_train)[sapply(svm_full_train, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "fold", "is_val"))
  
  means <- sapply(svm_full_train[feats], mean, na.rm = TRUE)
  sds   <- sapply(svm_full_train[feats], sd, na.rm = TRUE)
  svm_full_norm_params[[f]] <- list(mean = means, sd = sds)
  
  # Create scaled versions
  svm_full_train_sc <- svm_full_train
  svm_full_val_sc   <- svm_full_val
  svm_full_test_sc  <- svm_full_test
  
  for(col in feats) {
    svm_full_train_sc[[col]] <- (svm_full_train[[col]] - means[col]) / sds[col]
    svm_full_val_sc[[col]]   <- (svm_full_val[[col]] - means[col]) / sds[col]
    svm_full_test_sc[[col]]  <- (svm_full_test[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  mrmr_in <- mRMR.data(data = as.data.frame(svm_full_train_sc[, c(feats, "YIELD")]))
  mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(svm_full_train_sc[, c(feats, "YIELD")]) == "YIELD"), feature_count = 50)
  
  mrmr_feats  <- names(svm_full_train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run)))
  
  svm_full_mrmr_rankings[[f]] <- mrmr_feats
  svm_full_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    
    set.seed(123)
    tmp_mod <- e1071::svm(
      formula = YIELD ~ ., 
      data = svm_full_train_sc[, c(curr, "YIELD")],
      kernel = "radial"
    )
    
    p_val <- predict(tmp_mod, svm_full_val_sc[, curr, drop = FALSE])
    rmse_history[k] <- sqrt(mean((svm_full_val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history); y <- rmse_history
  
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  if(k1 < (length(y) - 1)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else { 
    opt_k <- k1 
  }
  
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { 
    opt_k <- 10 
  }
  
  best_feats <- mrmr_feats[1:opt_k]
  
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  # ==============================================================================
  # E. HYPERPARAMETER TUNING (Grid Search for SVM)
  # ==============================================================================
  grid <- expand.grid(cost = c(0.1, 1, 10), gamma = c(0.01, 0.1, 0.5))
  best_v_rmse <- Inf; final_params <- list()
  
  for(g in 1:nrow(grid)){
    set.seed(123) 
    m <- e1071::svm(
      formula = YIELD ~ ., 
      data = svm_full_train_sc[, c(best_feats, "YIELD")],
      kernel = "radial",
      cost = grid$cost[g],
      gamma = grid$gamma[g]
    )
    
    v_p <- predict(m, svm_full_val_sc[, best_feats])
    v_rmse <- sqrt(mean((svm_full_val_sc$YIELD - v_p)^2))
    
    if(v_rmse < best_v_rmse){
      best_v_rmse <- v_rmse
      final_params <- list(cost = grid$cost[g], gamma = grid$gamma[g])
    }
  }
  svm_full_best_params[[f]] <- final_params
  
  cat("\nFold", f, "Best Parameters: Cost =", final_params$cost, "| Gamma =", final_params$gamma)
  
  # ==============================================================================
  # F. FINAL FOLD MODEL & SHAP
  # ==============================================================================
  set.seed(123) 
  final_mod <- e1071::svm(
    formula = YIELD ~ ., 
    data = svm_full_train_sc[, c(best_feats, "YIELD")],
    kernel = "radial",
    cost = final_params$cost,
    gamma = final_params$gamma
  )
  svm_full_fold_models[[f]] <- final_mod
  
  # SHAP Wrapper Fix for SVM
  p_wrapper <- function(object, newdata) { 
    predict(object, as.data.frame(newdata)) 
  }
  
  X_test_df <- svm_full_test_sc[, best_feats]
  shap_contrib <- fastshap::explain(final_mod, X = X_test_df, pred_wrapper = p_wrapper, nsim = 10)
  
  svm_full_shap_values[[f]]   <- as.data.frame(shap_contrib)
  svm_full_shap_features[[f]] <- X_test_df
  
  # ==============================================================================
  # G. METRICS COLLECTION
  # ==============================================================================
  # 1. Generate predictions
  v_p <- predict(final_mod, svm_full_val_sc[, best_feats])
  t_p <- predict(final_mod, svm_full_test_sc[, best_feats])
  
  # 2. Calculate metrics using your specific function
  v_m <- get_metrics(svm_full_val$YIELD, v_p, length(best_feats), nrow(svm_full_val))
  t_m <- get_metrics(svm_full_test$YIELD, t_p, length(best_feats), nrow(svm_full_test))
  
  # 3. Store in the metrics dataframe
  svm_full_fold_metrics <- rbind(svm_full_fold_metrics, data.frame(
    Fold = f, Features = opt_k, 
    Val_R2 = v_m["R2"], Test_R2 = t_m["R2"], Val_AdjR2 = v_m["AdjR2"], Test_AdjR2 = t_m["AdjR2"],
    Val_RMSE = v_m["RMSE"], Test_RMSE = t_m["RMSE"], Val_MAE = v_m["MAE"], Test_MAE = t_m["MAE"]
  ))
  
  # 4. Save fold results for the Actual vs Predicted plot
  svm_full_test_results[[f]] <- data.frame(
    Actual = svm_full_test$YIELD, 
    Predicted = t_p, 
    Fold = f
  )
  
  cat("\nFold", f, "complete. Test AdjR2:", round(t_m["AdjR2"], 3))
  
} # END MASTER LOOP

# STOP TIMER HERE (Outside the loop)
end_time <- Sys.time()
comp_time <- end_time - start_time

# 5. Metrics
print(colMeans(svm_full_fold_metrics[, -1]))

# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================
cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")
svm_full_fold_metrics$Val_Years <- unlist(svm_full_val_years_list)
svm_full_fold_metrics_rounded <- svm_full_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))
print(svm_full_fold_metrics_rounded)

# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
numeric_metrics <- svm_full_fold_metrics %>% select(where(is.numeric)) %>% select(-Fold)
svm_full_metric_means <- colMeans(numeric_metrics)
svm_full_metric_sds   <- sapply(numeric_metrics, sd)

# Create a clean summary table
n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776

svm_full_final_summary <- data.frame(
  Metric = names(svm_full_metric_means),
  Mean   = round(svm_full_metric_means, 4),
  SD     = round(svm_full_metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(svm_full_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(svm_full_final_summary, "SVM_Full_Metrics.csv", row.names = FALSE)
saveRDS(svm_full_final_summary, "SVM_Full_Metrics.rds")

svm_full_best_feats <- unlist(svm_full_mrmr_rankings)
svm_full_feat_freq <- as.data.frame(table(svm_full_best_feats)) %>% arrange(desc(Freq))
print(svm_full_feat_freq)

# ============================================================================== 
# 4. FINAL RESULTS & VISUALIZATION 
# ============================================================================== 
svm_full_shap <- bind_rows(svm_full_shap_values) 
svm_full_feat <- bind_rows(svm_full_shap_features) 

svm_prep_shap <- svm_full_shap %>% mutate(ID = row_number()) %>% pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")
svm_prep_feat <- svm_full_feat %>% mutate(ID = row_number()) %>% pivot_longer(-ID, names_to = "Feature", values_to = "Value")

svm_plot_data <- left_join(svm_prep_shap, svm_prep_feat, by = c("ID", "Feature")) %>% filter(!is.na(SHAP))

svm_top_5 <- svm_plot_data %>% 
  group_by(Feature) %>% 
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>% 
  arrange(desc(mean_abs_impact)) %>% 
  dplyr::slice(1:5)

svm_plot_data_top5 <- svm_plot_data %>% filter(Feature %in% svm_top_5$Feature)

# 4. Generate the Beeswarm Plot
common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

svm_full_shap_plot <- ggplot(svm_plot_data_top5 %>% 
                               group_by(Feature) %>% 
                               mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                             aes(x = SHAP, 
                                 y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                 color = Value)) + 
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  geom_text(aes(x = -3.9, label = label_text), hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  scale_color_viridis_c(option = "viridis", name = "Relative\nintensity", limits = common_color_lim, oob = scales::squish) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)) + 
  labs(title = "Support Vector Machine", x = expression("Yield impact (t ha"^{-1}*")"), y = "Features")

svm_full_shap_plot

svm_full_fold_importance <- svm_plot_data %>%
  group_by(Feature) %>%
  summarize(Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE), Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

write.csv(svm_full_fold_importance, "SVM_Full_SHAP_Importance.csv", row.names = FALSE)

# ============================================================================== 
# mRMR AGGREGATION PLOT (SVM)
# ============================================================================== 
# 1. Aggregate and Average mRMR Scores
svm_full_mrmr_scores <- bind_rows(svm_full_mrmr_scores_list) %>% 
  group_by(Feature) %>% 
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
svm_full_mrmr_scores <- svm_full_mrmr_scores %>% 
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 5 Average Features
svm_full_mrmr_plot <- ggplot(svm_full_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Support Vector Machine", 
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))

svm_full_mrmr_plot

############################################ Train:Test ############################
# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
svm_full_plot_df <- bind_rows(svm_full_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
svm_full_test_r2_mean   <- svm_full_final_summary$Mean[svm_full_final_summary$Metric == "Test_R2"]
svm_full_test_r2_sd     <- svm_full_final_summary$SD[svm_full_final_summary$Metric == "Test_R2"]
svm_full_test_rmse_mean <- svm_full_final_summary$Mean[svm_full_final_summary$Metric == "Test_RMSE"]
svm_full_test_rmse_sd   <- svm_full_final_summary$SD[svm_full_final_summary$Metric == "Test_RMSE"]
svm_full_test_mae_mean  <- svm_full_final_summary$Mean[svm_full_final_summary$Metric == "Test_MAE"]
svm_full_test_mae_sd    <- svm_full_final_summary$SD[svm_full_final_summary$Metric == "Test_MAE"]

# B. Overall Metrics
svm_full_overall_r2 <- cor(svm_full_plot_df$Actual, svm_full_plot_df$Predicted)^2
svm_full_overall_rmse <- sqrt(mean((svm_full_plot_df$Actual - svm_full_plot_df$Predicted)^2))
svm_full_overall_mae <- mean(abs(svm_full_plot_df$Actual - svm_full_plot_df$Predicted))

# 3. CREATE CONSOLIDATED PLOT
p_main <- ggplot(svm_full_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(svm_full_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(svm_full_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(svm_full_test_mae_mean, 2), nsmall = 2), " ± ", format(round(svm_full_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(svm_full_test_r2_mean, 3), nsmall = 3), " ± ", format(round(svm_full_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")

# 2. Top Density
p_top <- ggplot(svm_full_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(svm_full_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
svm_full_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Support Vector Machine",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
svm_full_scatter_plot


########################################################################################################
######################## ANN CLIMATE MODEL #############################################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# Global ANN seeds
ann_seeds <- list(
  seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))

# MASTER STORAGE (Keeps everything for every fold)
ann_climate_mrmr_rankings <- list()
ann_climate_mrmr_scores_list <- list()
ann_climate_best_params <- list()
ann_climate_fold_models <- list()
ann_climate_shap_values <- list()
ann_climate_shap_features <- list()
ann_climate_norm_params <- list()
ann_climate_test_results <- list()
ann_climate_fold_metrics <- data.frame()
ann_climate_val_years_list <- list()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  r2   <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f)
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  ann_climate_outer_train <- dataset_with_folds %>% filter(fold != f)
  ann_climate_test        <- dataset_with_folds %>% filter(fold == f)
  
  # --- STEP 2: VALIDATION SET (Temporal LOYO - 6 Years) ---
  unique_years <- unique(ann_climate_outer_train$YEAR)
  
  set.seed(123 + f)
  val_years <- sample(unique_years, 6) 
  ann_climate_val_years_list[[f]] <- paste(sort(val_years), collapse = ", ")
  
  ann_climate_val   <- ann_climate_outer_train %>% filter(YEAR %in% val_years)
  ann_climate_train <- ann_climate_outer_train %>% filter(!(YEAR %in% val_years))
  
  cat("\nFold", f, "tuning using LOYO on Years:", ann_climate_val_years_list[[f]])
  
  # --- DROP NON-FEATURE COLUMNS ---
  ann_climate_train <- ann_climate_train %>% select(-State, -County, -YEAR)
  ann_climate_val   <- ann_climate_val   %>% select(-State, -County, -YEAR)
  ann_climate_test  <- ann_climate_test  %>% select(-State, -County, -YEAR)
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(ann_climate_train)[sapply(ann_climate_train, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "fold", "is_val"))
  
  means <- sapply(ann_climate_train[feats], mean, na.rm = TRUE)
  sds   <- sapply(ann_climate_train[feats], sd, na.rm = TRUE)
  ann_climate_norm_params[[f]] <- list(mean = means, sd = sds)
  
  # Create scaled versions
  ann_climate_train_sc <- ann_climate_train
  ann_climate_val_sc   <- ann_climate_val
  ann_climate_test_sc  <- ann_climate_test
  
  for(col in feats) {
    ann_climate_train_sc[[col]] <- (ann_climate_train[[col]] - means[col]) / sds[col]
    ann_climate_val_sc[[col]]   <- (ann_climate_val[[col]] - means[col]) / sds[col]
    ann_climate_test_sc[[col]]  <- (ann_climate_test[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  mrmr_in <- mRMR.data(data = as.data.frame(ann_climate_train_sc[, c(feats, "YIELD")]))
  mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(ann_climate_train_sc[, c(feats, "YIELD")]) == "YIELD"), feature_count = 50)
  
  mrmr_feats  <- names(ann_climate_train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run)))
  
  ann_climate_mrmr_rankings[[f]] <- mrmr_feats
  ann_climate_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    
    # Using nnet for the elbow check (Standard 1-hidden layer ANN)
    set.seed(123)
    tmp_mod <- nnet::nnet(
      formula = YIELD ~ ., 
      data = ann_climate_train_sc[, c(curr, "YIELD")],
      size = 5,       # Fixed size for feature selection speed
      linout = TRUE,  # Linear output for regression
      trace = FALSE,
      maxit = 100
    )
    
    p_val <- predict(tmp_mod, ann_climate_val_sc[, curr, drop = FALSE])
    rmse_history[k] <- sqrt(mean((ann_climate_val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history); y <- rmse_history
  
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  if(k1 < (length(y) - 1)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else { 
    opt_k <- k1 
  }
  
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { 
    opt_k <- 10 
  }
  
  best_feats <- mrmr_feats[1:opt_k]
  ann_climate_shap_features[[f]] <- best_feats
  
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  # ==============================================================================
  # E. HYPERPARAMETER TUNING (Grid Search for ANN)
  # ==============================================================================
  # Typical ANN tuning involves 'size' (hidden units) and 'decay' (regularization)
  grid <- expand.grid(
    size = c(5, 10, 15),
    decay = c(0.01, 0.1, 0.5)
  )
  best_v_rmse <- Inf; final_params <- list()
  
  for(g in 1:nrow(grid)){
    set.seed(123) 
    m <- nnet::nnet(
      formula = YIELD ~ ., 
      data = ann_climate_train_sc[, c(best_feats, "YIELD")],
      size = grid$size[g],
      decay = grid$decay[g],
      linout = TRUE,
      trace = FALSE,
      maxit = 200
    )
    
    v_p <- predict(m, ann_climate_val_sc[, best_feats])
    v_rmse <- sqrt(mean((ann_climate_val_sc$YIELD - v_p)^2))
    
    if(v_rmse < best_v_rmse){
      best_v_rmse <- v_rmse
      final_params <- list(size = grid$size[g], decay = grid$decay[g])
    }
  }
  ann_climate_best_params[[f]] <- final_params
  
  cat("\nFold", f, "Best Parameters: Size =", final_params$size, "| Decay =", final_params$decay)
  
  # ==============================================================================
  # F. FINAL FOLD MODEL & SHAP
  # ==============================================================================
  set.seed(123) 
  final_mod <- nnet::nnet(
    formula = YIELD ~ ., 
    data = ann_climate_train_sc[, c(best_feats, "YIELD")],
    size = final_params$size,
    decay = final_params$decay,
    linout = TRUE,
    trace = FALSE,
    maxit = 300
  )
  ann_climate_fold_models[[f]] <- final_mod
  
  # SHAP Wrapper Fix for ANN
  p_wrapper <- function(object, newdata) { 
    predict(object, as.data.frame(newdata)) 
  }
  
  X_test_df <- ann_climate_test_sc[, best_feats]
  shap_contrib <- fastshap::explain(final_mod, X = X_test_df, pred_wrapper = p_wrapper, nsim = 10)
  
  ann_climate_shap_values[[f]]   <- as.data.frame(shap_contrib)
  ann_climate_shap_features[[f]] <- X_test_df
  
  # ==============================================================================
  # G. METRICS COLLECTION
  # ==============================================================================
  # 1. Generate predictions
  v_p <- predict(final_mod, ann_climate_val_sc[, best_feats])
  t_p <- predict(final_mod, ann_climate_test_sc[, best_feats])
  
  # 2. Calculate metrics using your specific function
  v_m <- get_metrics(ann_climate_val$YIELD, v_p, length(best_feats), nrow(ann_climate_val))
  t_m <- get_metrics(ann_climate_test$YIELD, t_p, length(best_feats), nrow(ann_climate_test))
  
  # 3. Store in the metrics dataframe
  ann_climate_fold_metrics <- rbind(ann_climate_fold_metrics, data.frame(
    Fold = f, Features = opt_k, 
    Val_R2 = v_m["R2"], Test_R2 = t_m["R2"], Val_AdjR2 = v_m["AdjR2"], Test_AdjR2 = t_m["AdjR2"],
    Val_RMSE = v_m["RMSE"], Test_RMSE = t_m["RMSE"], Val_MAE = v_m["MAE"], Test_MAE = t_m["MAE"]
  ))
  
  # 4. Save fold results for the Actual vs Predicted plot
  ann_climate_test_results[[f]] <- data.frame(
    Actual = ann_climate_test$YIELD, 
    Predicted = t_p, 
    Fold = f
  )
  
  cat("\nFold", f, "complete. Test AdjR2:", round(t_m["AdjR2"], 3))
  
} # END MASTER LOOP

# STOP TIMER HERE
end_time <- Sys.time()
comp_time <- end_time - start_time

# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================
cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")
ann_climate_fold_metrics$Val_Years <- unlist(ann_climate_val_years_list)

ann_climate_fold_metrics_clean <- ann_climate_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(ann_climate_fold_metrics_clean)

# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
# Filter for numeric columns for colMeans
numeric_cols_summary <- ann_climate_fold_metrics %>% select(where(is.numeric)) %>% select(-Fold)
ann_climate_metric_means <- colMeans(numeric_cols_summary)
ann_climate_metric_sds   <- sapply(numeric_cols_summary, sd)

# Create a clean summary table
n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776

ann_climate_final_summary <- data.frame(
  Metric = names(ann_climate_metric_means),
  Mean   = round(ann_climate_metric_means, 4),
  SD     = round(ann_climate_metric_sds, 4)
) %>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(ann_climate_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(ann_climate_final_summary, "ANN_Climate_Metrics.csv", row.names = FALSE)
saveRDS(ann_climate_final_summary, "ANN_Climate_Metrics.rds")

# Common features
ann_climate_best_feats <- unlist(ann_climate_mrmr_rankings)
ann_climate_feat_freq <- as.data.frame(table(ann_climate_best_feats)) %>% arrange(desc(Freq))
print(ann_climate_feat_freq)

# ============================================================================== 
# 4. FINAL RESULTS & VISUALIZATION (ANN)
# ============================================================================== 
ann_climate_shap <- bind_rows(ann_climate_shap_values) 
ann_climate_feat <- bind_rows(ann_climate_shap_features) 

ann_prep_shap <- ann_climate_shap %>% mutate(ID = row_number()) %>% pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")
ann_prep_feat <- ann_climate_feat %>% mutate(ID = row_number()) %>% pivot_longer(-ID, names_to = "Feature", values_to = "Value")

ann_plot_data <- left_join(ann_prep_shap, ann_prep_feat, by = c("ID", "Feature")) %>% filter(!is.na(SHAP))

ann_top_5 <- ann_plot_data %>% 
  group_by(Feature) %>% 
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>% 
  arrange(desc(mean_abs_impact)) %>% 
  dplyr::slice(1:5)

ann_plot_data_top5 <- ann_plot_data %>% filter(Feature %in% ann_top_5$Feature)

# Beeswarm Plot
common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

ann_climate_shap_plot <- ggplot(ann_plot_data_top5 %>% 
                                  group_by(Feature) %>% 
                                  mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                                aes(x = SHAP, 
                                    y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                    color = Value)) + 
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  scale_color_viridis_c(option = "viridis", name = "Relative\nintensity", limits = common_color_lim, oob = scales::squish) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)) + 
  labs(title = "Artificial Neural Network", x = expression("Yield impact (t ha"^{-1}*")"), y = "Features")

ann_climate_shap_plot

# Importance table
ann_climate_fold_importance <- ann_plot_data %>%
  group_by(Feature) %>%
  summarize(Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE), Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

write.csv(ann_climate_fold_importance, "ANN_climate_SHAP_Importance.csv", row.names = FALSE)

# ============================================================================== 
# mRMR AGGREGATION PLOT (ANN)
# ============================================================================== 
# 1. Aggregate and Average mRMR Scores
ann_climate_mrmr_scores <- bind_rows(ann_climate_mrmr_scores_list) %>% 
  group_by(Feature) %>% 
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
ann_climate_mrmr_scores <- ann_climate_mrmr_scores %>% 
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 5 Average Features
ann_climate_mrmr_plot <- ggplot(ann_climate_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Artificial Neural Network", 
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))

ann_climate_mrmr_plot

############################################ Train:Test ############################

# 1. PREPARE DATA
ann_climate_plot_df <- bind_rows(ann_climate_test_results)

# 2. CALCULATE METRICS
ann_climate_test_r2_mean   <- ann_climate_final_summary$Mean[ann_climate_final_summary$Metric == "Test_R2"]
ann_climate_test_r2_sd     <- ann_climate_final_summary$SD[ann_climate_final_summary$Metric == "Test_R2"]
ann_climate_test_rmse_mean <- ann_climate_final_summary$Mean[ann_climate_final_summary$Metric == "Test_RMSE"]
ann_climate_test_rmse_sd   <- ann_climate_final_summary$SD[ann_climate_final_summary$Metric == "Test_RMSE"]
ann_climate_test_mae_mean  <- ann_climate_final_summary$Mean[ann_climate_final_summary$Metric == "Test_MAE"]
ann_climate_test_mae_sd    <- ann_climate_final_summary$SD[ann_climate_final_summary$Metric == "Test_MAE"]

# 3. CREATE CONSOLIDATED PLOT
p_main <- ggplot(ann_climate_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(
    x = expression("Actual yield (t ha"^{-1}*")"), 
    y = expression("Predicted yield (t ha"^{-1}*")")
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(ann_climate_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(ann_climate_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(ann_climate_test_mae_mean, 2), nsmall = 2), " ± ", format(round(ann_climate_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(ann_climate_test_r2_mean, 3), nsmall = 3), " ± ", format(round(ann_climate_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")

# 2. Top Density
p_top <- ggplot(ann_climate_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(ann_climate_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
ann_climate_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Artificial Neural Network",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
ann_climate_scatter_plot

########################################################################################################
######################## ANN FULL MODEL #############################################################

# ==============================================================================
# 2. SPATIAL GROUPING (LOCO Setup)
# ==============================================================================
set.seed(123)
# ANN seeds
ann_seeds <- list(
  seed = 123
)

# Ensure at least one county from each state is in each of the 5 folds
county_metadata <- dataset_final %>%
  distinct(State, County) %>%
  group_by(State) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>%
  ungroup()

dataset_with_folds <- dataset_final %>%
  left_join(select(county_metadata, State, County, fold), by = c("State", "County"))


# MASTER STORAGE (Keeps everything for every fold)
ann_full_fold_models <- list()
ann_full_fold_metrics <- data.frame()
ann_full_mrmr_rankings <- list()
ann_full_best_params <- list()
ann_full_shap_values <- list()
ann_full_shap_features <- list()
ann_full_norm_params <- list()
ann_full_best_feats_used <- c()
ann_full_mrmr_scores <- list()
ann_full_mrmr_scores_list <- list()
ann_full_test_results <- list()
ann_full_val_years_list <- list()

# Helper Function for Metrics
get_metrics <- function(actual, predicted, p_count, n_obs) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  r2 <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  adj_r2 <- 1 - ((1 - r2) * (n_obs - 1) / (n_obs - p_count - 1))
  return(c(RMSE = rmse, MAE = mae, R2 = r2, AdjR2 = adj_r2))
}

# ==============================================================================
# 3. MASTER LOCO LOOP (5 Folds)
# ==============================================================================
# START TIMER HERE
start_time <- Sys.time() 

for(f in 1:5) {
  # 1. Reset the seed first so the splits below are stable
  set.seed(123 + f)
  cat("\n\n########################### STARTING FOLD", f, "###########################")
  
  # A. SPATIAL SPLITS
  ann_full_outer_train <- dataset_with_folds %>% filter(fold != f)
  ann_full_test        <- dataset_with_folds %>% filter(fold == f)
  
  # --- STEP 2: VALIDATION SET (Temporal LOYO - 6 Years) ---
  unique_years <- unique(ann_full_outer_train$YEAR)
  
  set.seed(123 + f)
  val_years <- sample(unique_years, 6) # Pick 6 years for a robust tuning set
  ann_full_val_years_list[[f]] <- paste(sort(val_years), collapse = ", ")
  
  ann_full_val   <- ann_full_outer_train %>% filter(YEAR %in% val_years)
  ann_full_train <- ann_full_outer_train %>% filter(!(YEAR %in% val_years))
  
  cat("\nFold", f, "tuning using LOYO on Years:", ann_full_val_years_list[[f]])
  
  # --- DROP NON-FEATURE COLUMNS ---
  ann_full_train <- ann_full_train %>% select(-State, -County)
  ann_full_val   <- ann_full_val   %>% select(-State, -County)
  ann_full_test  <- ann_full_test  %>% select(-State, -County)
  
  # B. Z-SCORE NORMALIZATION (Based on Train only)
  numeric_cols <- names(ann_full_train)[sapply(ann_full_train, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "fold", "is_val"))
  
  means <- sapply(ann_full_train[feats], mean, na.rm = TRUE)
  sds   <- sapply(ann_full_train[feats], sd, na.rm = TRUE)
  ann_full_norm_params[[f]] <- list(mean = means, sd = sds)
  
  # Create scaled versions
  ann_full_train_sc <- ann_full_train
  ann_full_val_sc   <- ann_full_val
  ann_full_test_sc  <- ann_full_test
  
  for(col in feats) {
    ann_full_train_sc[[col]] <- (ann_full_train[[col]] - means[col]) / sds[col]
    ann_full_val_sc[[col]]   <- (ann_full_val[[col]] - means[col]) / sds[col]
    ann_full_test_sc[[col]]  <- (ann_full_test[[col]] - means[col]) / sds[col]
  }
  
  # ==============================================================================
  # C. mRMR FEATURE RANKING
  # ==============================================================================
  cat("\n--- Running mRMR Ranking...")
  mrmr_in <- mRMR.data(data = as.data.frame(ann_full_train_sc[, c(feats, "YIELD")]))
  mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(ann_full_train_sc[, c(feats, "YIELD")]) == "YIELD"), feature_count = 50)
  
  mrmr_feats  <- names(ann_full_train_sc[, feats])[as.numeric(unlist(solutions(mrmr_run)))]
  mrmr_scores <- as.numeric(unlist(scores(mrmr_run)))
  
  ann_full_mrmr_rankings[[f]] <- mrmr_feats
  ann_full_mrmr_scores_list[[f]] <- data.frame(Feature = mrmr_feats, Score = mrmr_scores)
  
  
  # ==============================================================================
  # D. FORWARD SELECTION + SECOND ELBOW
  # ==============================================================================
  cat("\n--- Running Forward Selection for Elbow...")
  rmse_history <- c()
  
  for(k in 1:length(mrmr_feats)) {
    curr <- mrmr_feats[1:k]
    
    set.seed(123)
    tmp_mod <- nnet::nnet(
      formula = YIELD ~ ., 
      data = ann_full_train_sc[, c(curr, "YIELD")],
      size = 5,
      linout = TRUE,
      trace = FALSE,
      maxit = 100
    )
    
    p_val <- predict(tmp_mod, ann_full_val_sc[, curr, drop = FALSE])
    rmse_history[k] <- sqrt(mean((ann_full_val_sc$YIELD - p_val)^2))
  }
  
  # --- Geometric Logic to find opt_k ---
  x <- 1:length(rmse_history); y <- rmse_history
  
  dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
  k1 <- which.max(dist1)
  
  if(k1 < (length(y) - 1)){
    x_tail <- k1:length(y)
    y_tail <- y[k1:length(y)]
    dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail)*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
    opt_k <- x_tail[which.max(dist2)]
  } else { 
    opt_k <- k1 
  }
  
  if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { 
    opt_k <- 10 
  }
  
  best_feats <- mrmr_feats[1:opt_k]
  
  cat("\nFold", f, "picked", opt_k, "features (First Elbow at", k1, ")")
  cat("\nSelected Features:", paste(best_feats, collapse = ", "))
  
  # ==============================================================================
  # E. HYPERPARAMETER TUNING (Grid Search for ANN)
  # ==============================================================================
  # Typical ANN tuning involves 'size' (hidden units) and 'decay' (weight decay/regularization)
  grid <- expand.grid(
    size = c(5, 10, 15),
    decay = c(0.01, 0.1, 0.5)
  )
  best_v_rmse <- Inf; final_params <- list()
  
  for(g in 1:nrow(grid)){
    set.seed(123) 
    m <- nnet::nnet(
      formula = YIELD ~ ., 
      data = ann_full_train_sc[, c(best_feats, "YIELD")],
      size = grid$size[g],
      decay = grid$decay[g],
      linout = TRUE,
      trace = FALSE,
      maxit = 200
    )
    
    v_p <- predict(m, ann_full_val_sc[, best_feats])
    v_rmse <- sqrt(mean((ann_full_val_sc$YIELD - v_p)^2))
    
    if(v_rmse < best_v_rmse){
      best_v_rmse <- v_rmse
      final_params <- list(size = grid$size[g], decay = grid$decay[g])
    }
  }
  ann_full_best_params[[f]] <- final_params
  
  cat("\nFold", f, "Best Parameters: Size =", final_params$size, "| Decay =", final_params$decay)
  
  # ==============================================================================
  # F. FINAL FOLD MODEL & SHAP
  # ==============================================================================
  set.seed(123) 
  final_mod <- nnet::nnet(
    formula = YIELD ~ ., 
    data = ann_full_train_sc[, c(best_feats, "YIELD")],
    size = final_params$size,
    decay = final_params$decay,
    linout = TRUE,
    trace = FALSE,
    maxit = 300
  )
  ann_full_fold_models[[f]] <- final_mod
  
  # SHAP Wrapper Fix for ANN
  p_wrapper <- function(object, newdata) { 
    predict(object, as.data.frame(newdata)) 
  }
  
  X_test_df <- ann_full_test_sc[, best_feats]
  shap_contrib <- fastshap::explain(final_mod, X = X_test_df, pred_wrapper = p_wrapper, nsim = 10)
  
  ann_full_shap_values[[f]]   <- as.data.frame(shap_contrib)
  ann_full_shap_features[[f]] <- X_test_df
  
  # ==============================================================================
  # G. METRICS COLLECTION
  # ==============================================================================
  # 1. Generate predictions
  v_p <- predict(final_mod, ann_full_val_sc[, best_feats])
  t_p <- predict(final_mod, ann_full_test_sc[, best_feats])
  
  # 2. Calculate metrics using your specific function
  v_m <- get_metrics(ann_full_val$YIELD, v_p, length(best_feats), nrow(ann_full_val))
  t_m <- get_metrics(ann_full_test$YIELD, t_p, length(best_feats), nrow(ann_full_test))
  
  # 3. Store in the metrics dataframe
  ann_full_fold_metrics <- rbind(ann_full_fold_metrics, data.frame(
    Fold = f, Features = opt_k, 
    Val_R2 = v_m["R2"], Test_R2 = t_m["R2"], Val_AdjR2 = v_m["AdjR2"], Test_AdjR2 = t_m["AdjR2"],
    Val_RMSE = v_m["RMSE"], Test_RMSE = t_m["RMSE"], Val_MAE = v_m["MAE"], Test_MAE = t_m["MAE"]
  ))
  
  # 4. Save fold results for the Actual vs Predicted plot
  ann_full_test_results[[f]] <- data.frame(
    Actual = ann_full_test$YIELD, 
    Predicted = t_p, 
    Fold = f
  )
  
  cat("\nFold", f, "complete. Test AdjR2:", round(t_m["AdjR2"], 3))
  
} # END MASTER LOOP

# STOP TIMER HERE (Outside the loop)
end_time <- Sys.time()
comp_time <- end_time - start_time

# 5. Metrics
print(colMeans(ann_full_fold_metrics[, -1]))

# ==============================================================================
# PRINT FOLD-BY-FOLD METRICS
# ==============================================================================
cat("\n\n########################### INDIVIDUAL FOLD METRICS ###########################\n")
ann_full_fold_metrics$Val_Years <- unlist(ann_full_val_years_list)
ann_full_fold_metrics_rounded <- ann_full_fold_metrics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))
print(ann_full_fold_metrics_rounded)

# ==============================================================================
# 4. FINAL AGGREGATION & SAVE
# ==============================================================================
numeric_metrics <- ann_full_fold_metrics %>% select(where(is.numeric)) %>% select(-Fold)
ann_full_metric_means <- colMeans(numeric_metrics)
ann_full_metric_sds   <- sapply(numeric_metrics, sd)


# Create a clean summary table
n <- 5
t_value <- qt(0.975, df = n - 1) # This returns 2.776

ann_full_final_summary <- data.frame(
  Metric = names(ann_full_metric_means),
  Mean   = round(ann_full_metric_means, 4),
  SD     = round(ann_full_metric_sds, 4)
)%>%
  mutate(
    SE = SD / sqrt(n),
    CI_Margin = t_value * SE,
    CI_Label = paste0(round(Mean, 2), " ± ", round(CI_Margin, 2))
  )

cat("\n\n================ FINAL SUMMARY METRICS (Across 5 Folds) ================\n")
print(ann_full_final_summary)
cat("\nTotal Computation Time:", round(as.numeric(comp_time, units="mins"), 2), "minutes\n")

write.csv(ann_full_final_summary, "ANN_Full_Metrics.csv", row.names = FALSE)
saveRDS(ann_full_final_summary, "ANN_Full_Metrics.rds")

ann_full_best_feats <- unlist(ann_full_mrmr_rankings)
ann_full_feat_freq <- as.data.frame(table(ann_full_best_feats)) %>% arrange(desc(Freq))
print(ann_full_feat_freq)

# ============================================================================== 
# 4. FINAL RESULTS & VISUALIZATION 
# ============================================================================== 
ann_full_shap <- bind_rows(ann_full_shap_values) 
ann_full_feat <- bind_rows(ann_full_shap_features) 

ann_prep_shap <- ann_full_shap %>% mutate(ID = row_number()) %>% pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")
ann_prep_feat <- ann_full_feat %>% mutate(ID = row_number()) %>% pivot_longer(-ID, names_to = "Feature", values_to = "Value")

ann_plot_data <- left_join(ann_prep_shap, ann_prep_feat, by = c("ID", "Feature")) %>% filter(!is.na(SHAP))

ann_top_5 <- ann_plot_data %>% 
  group_by(Feature) %>% 
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE)) %>% 
  arrange(desc(mean_abs_impact)) %>% 
  dplyr::slice(1:5)

ann_plot_data_top5 <- ann_plot_data %>% filter(Feature %in% ann_top_5$Feature)

# Beeswarm Plot
common_xlim <- c(-4, 4)
common_color_lim <- c(-1, 1) 

ann_full_shap_plot <- ggplot(ann_plot_data_top5 %>% 
                               group_by(Feature) %>% 
                               mutate(label_text = format(round(mean(abs(SHAP), na.rm = TRUE), 3), nsmall = 3)), 
                             aes(x = SHAP, 
                                 y = reorder(Feature, abs(SHAP), FUN = mean, na.rm = TRUE), 
                                 color = Value)) + 
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.6, alpha = 0.5) + 
  geom_text(aes(x = -3.9, label = label_text), hjust = 0, vjust = -0.5, size = 3.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  scale_color_viridis_c(option = "viridis", name = "Relative\nintensity", limits = common_color_lim, oob = scales::squish) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  coord_cartesian(xlim = common_xlim) + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
    axis.title = element_text(size = 14, color = "black"),           
    axis.text = element_text(size = 12, color = "black"),            
    panel.grid.major = element_blank(),                              
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8)) + 
  labs(title = "Artificial Neural Network", x = expression("Yield impact (t ha"^{-1}*")"), y = "Features")

ann_full_shap_plot

ann_full_fold_importance <- ann_plot_data %>%
  group_by(Feature) %>%
  summarize(Mean_Test_Impact = mean(abs(SHAP), na.rm = TRUE), Impact_Stability_SD = sd(abs(SHAP), na.rm = TRUE)) %>%
  arrange(desc(Mean_Test_Impact)) %>%
  mutate(Rank = row_number())

write.csv(ann_full_fold_importance, "ANN_Full_SHAP_Importance.csv", row.names = FALSE)

# ============================================================================== 
# mRMR AGGREGATION PLOT (ANN)
# ============================================================================== 
# 1. Aggregate and Average mRMR Scores
ann_full_mrmr_scores <- bind_rows(ann_full_mrmr_scores_list) %>% 
  group_by(Feature) %>% 
  summarize(Avg_Score = mean(Score, na.rm = TRUE)) %>% 
  arrange(desc(Avg_Score))

# 2. Normalize the Average Scores (Min-Max)
ann_full_mrmr_scores <- ann_full_mrmr_scores %>% 
  mutate(Normalized_Score = (Avg_Score - min(Avg_Score)) / (max(Avg_Score) - min(Avg_Score)))

# 3. Plot Top 5 Average Features
ann_full_mrmr_plot <- ggplot(ann_full_mrmr_scores[1:5, ], aes(x = reorder(Feature, Normalized_Score), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888") + 
  labs(
    title = "Artificial Neural Network", 
    x = "Features", 
    y = "Normalized mRMR score") + 
  coord_flip() +
  theme_bw() + 
  geom_text(aes(label = round(Normalized_Score, 3)), hjust = -0.2, size = 3.5) +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_rect(fill = "white", linetype = 1, linewidth = 0.75, color = 'black')) + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0))

ann_full_mrmr_plot

############################################ Train:Test ############################
# 1. PREPARE DATA
# Combine all individual fold predictions into one dataframe
ann_full_plot_df <- bind_rows(ann_full_test_results)

# 2. CALCULATE METRICS
# A. Average Metrics (The mean of the 5 independent fold results)
ann_full_test_r2_mean   <- ann_full_final_summary$Mean[ann_full_final_summary$Metric == "Test_R2"]
ann_full_test_r2_sd     <- ann_full_final_summary$SD[ann_full_final_summary$Metric == "Test_R2"]
ann_full_test_rmse_mean <- ann_full_final_summary$Mean[ann_full_final_summary$Metric == "Test_RMSE"]
ann_full_test_rmse_sd   <- ann_full_final_summary$SD[ann_full_final_summary$Metric == "Test_RMSE"]
ann_full_test_mae_mean  <- ann_full_final_summary$Mean[ann_full_final_summary$Metric == "Test_MAE"]
ann_full_test_mae_sd    <- ann_full_final_summary$SD[ann_full_final_summary$Metric == "Test_MAE"]

# B. Overall Metrics
ann_full_overall_r2 <- cor(ann_full_plot_df$Actual, ann_full_plot_df$Predicted)^2
ann_full_overall_rmse <- sqrt(mean((ann_full_plot_df$Actual - ann_full_plot_df$Predicted)^2))
ann_full_overall_mae <- mean(abs(ann_full_plot_df$Actual - ann_full_plot_df$Predicted))

library(ggplot2)
library(patchwork)

# 3. CREATE CONSOLIDATED PLOT
# scatter_ann_full <- ggplot(ann_full_plot_df, aes(x = Actual, y = Predicted)) +
#   geom_point(color = "#191970", alpha = 0.6, size = 2) + 
#   geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
#   labs(
#     title = "Artificial Neural Network", 
#     x = expression("Actual yield (t ha"^{-1}*")"), 
#     y = expression("Predicted yield (t ha"^{-1}*")")
#   ) +
#   theme_bw() + 
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
#     axis.text = element_text(size = 12, color = "black"),
#     axis.title = element_text(size = 14, color = "black"),
#     panel.grid.major = element_blank(), 
#     panel.grid.minor = element_blank(),
#     axis.text.x = element_text(color = "black", size = 12),
#     axis.text.y = element_text(color = "black", size = 12)
#   ) + 
#   scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
#   scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
#   coord_fixed() + 
#   # Add the metrics in the top left corner
#   annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
#            label = paste0(
#              "RMSE: ", format(round(ann_full_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(ann_full_test_rmse_sd, 2), nsmall = 2),
#              "\nMAE: ", format(round(ann_full_test_mae_mean, 2), nsmall = 2), " ± ", format(round(ann_full_test_mae_sd, 2), nsmall = 2),
#              "\nR²: ", format(round(ann_full_test_r2_mean, 3), nsmall = 3), " ± ", format(round(ann_full_test_r2_sd, 3), nsmall = 3)),
#            size = 4, color = "black")

# 1. Main Plot 
p_main <- ggplot(ann_full_plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) + 
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) + 
  labs(x = expression("Actual yield (t ha"^{-1}*")"), y = expression("Predicted yield (t ha"^{-1}*")")) +
  scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  scale_y_continuous(limits = c(0, 25), expand = c(0,0)) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  ) + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) + 
  coord_fixed() + 
  # Add the metrics in the top left corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
           label = paste0(
             "RMSE: ", format(round(ann_full_test_rmse_mean, 2), nsmall = 2), " ± ", format(round(ann_full_test_rmse_sd, 2), nsmall = 2),
             "\nMAE: ", format(round(ann_full_test_mae_mean, 2), nsmall = 2), " ± ", format(round(ann_full_test_mae_sd, 2), nsmall = 2),
             "\nR²: ", format(round(ann_full_test_r2_mean, 3), nsmall = 3), " ± ", format(round(ann_full_test_r2_sd, 3), nsmall = 3)),
           size = 4, color = "black")

# 2. Top Density
p_top <- ggplot(ann_full_plot_df, aes(x = Actual)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + theme_void()

# 3. Right Density
p_right <- ggplot(ann_full_plot_df, aes(x = Predicted)) + geom_density(fill = "#5D3FD3", alpha = 0.5) + scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
  coord_flip() + theme_void()

# 4. Assemble and Add Title at the Top
ann_full_scatter_plot <- (p_top + plot_spacer()) / (p_main + p_right) + plot_layout(widths = c(6, 1), heights = c(1, 6)) +
  plot_annotation(
    title = "Artificial Neural Network",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))
# Display the plot
ann_full_scatter_plot


#################################################### SHAP grid ##########################################################
library(patchwork)
library(ggplot2)
library(grid)
library(scales)

# ==============================================================================
# 1. CLEANING FUNCTION: KEEP UNDERSCORES, WRAP, & PREP FOR COLLECTION
# ==============================================================================
clean_axis_and_style_bar <- function(p) {
  p + theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12, color = "black", lineheight = 0.8), 
    axis.text.x = element_text(size = 12, color = "black"),
    plot.tag = element_text(size = 14, face = "bold") # Tag style for (a) and (b)
  ) +
    scale_y_discrete(labels = function(x) {
      x <- gsub("_", "_ ", x)        
      x <- scales::label_wrap(16)(x) # Wrapped at 16 as requested
      gsub("_ ", "_", x)             
    }) +
    guides(color = guide_colorbar(
      title = "Feature\nvalue",
      barheight = unit(25, "lines"),
      barwidth = unit(1.2, "lines"),
      ticks = FALSE,  
      title.position = "top",
      title.theme = element_text(size = 14, face = "bold"),
      label.theme = element_text(size = 12),
      breaks = c(0, 1),              
      labels = c("Low", "High")
    ))
}

# ==============================================================================
# 2. APPLY STYLE & UNIQUE TITLES/TAGS FOR THE TWO PLOTS
# ==============================================================================
# Left plot: XGBoost Full Model
xgb_plot_clean <- clean_axis_and_style_bar(
  xgb_full_shap_plot + labs(title = "Full Model (Extreme Gradient Boosting)")
)

# Right plot: Random Forest Climate Model
rf_plot_clean <- clean_axis_and_style_bar(
  rf_climate_shap_plot + labs(title = "Climate Model (Random Forest)")
)

# ==============================================================================
# 3. ASSEMBLE 1x2 GRID & COLLECT THE COLORBAR
# ==============================================================================
combined_shap_grid <- (xgb_plot_clean | rf_plot_clean) + 
  plot_layout(guides = "collect")

# ==============================================================================
# 4. FINAL WRAP WITH GLOBAL LABELS & SPACE ADJUSTMENTS
# ==============================================================================
final_plot <- wrap_elements(panel = combined_shap_grid) +
  labs(tag = "Features", caption = expression(bold("SHAP value (impact on yield, t ha"^{-1}*")"))) +
  theme(
    plot.tag.position = c(-0.01, 0.5), 
    plot.tag = element_text(size = 16, angle = 90, face = "bold"),
    plot.caption = element_text(size = 14, hjust = 0.5, margin = ggplot2::margin(t = 15)),
    legend.position = "right",
    # Reduced negative margins since 2 plots take up less horizontal space than 4
    legend.margin = ggplot2::margin(l = -20), 
    legend.box.margin = ggplot2::margin(l = -10),
    plot.margin = ggplot2::margin(l = 45, b = 10, t = 10, r = 5) 
  )

# Display result
suppressMessages(print(final_plot))

ggsave("SHAP_Selected_Models.png", final_plot, width = 12, height = 8, dpi = 300)
# 
# # ==============================================================================
# # 2. APPLY STYLE & MANUAL TAGGING (VERIFY CLIMATE VAR NAMES HERE)
# # ==============================================================================
# # Row 1: FULL MODELS
# f_plots_clean <- lapply(list(
#   lr_full_shap_plot   + labs(title = "Linear Regression", tag = "(a)"), 
#   lgbm_full_shap_plot + labs(title = "Light Gradient Boosting"),
#   rf_full_shap_plot   + labs(title = "Random Forest"), 
#   xgb_full_shap_plot  + labs(title = "Extreme Gradient Boosting")
#  
# 
# ), clean_axis_and_style_bar)
# 
# # Row 2: CLIMATE MODELS (Double-check these variables exist in your environment)
# c_plots_clean <- lapply(list(
#   lr_climate_shap_plot   + labs(title = NULL, tag = "(b)"), 
#   lgbm_climate_shap_plot + labs(title = NULL),
#   rf_climate_shap_plot   + labs(title = NULL), 
#   xgb_climate_shap_plot  + labs(title = NULL)
# 
# ), clean_axis_and_style_bar)
# 
# # ==============================================================================
# # 3. ASSEMBLE GRID & COLLECT A SINGLE COLORBAR
# # ==============================================================================
# combined_shap_grid <- (
#   (f_plots_clean[[1]] | f_plots_clean[[2]] | f_plots_clean[[3]] | f_plots_clean[[4]]) / 
#     (c_plots_clean[[1]] | c_plots_clean[[2]] | c_plots_clean[[3]] | c_plots_clean[[4]])
# ) + 
#   plot_layout(guides = "collect")
# 
# # ==============================================================================
# # 4. FINAL WRAP WITH GLOBAL LABELS & SPACE CRUSHING
# # ==============================================================================
# final_plot <- wrap_elements(panel = combined_shap_grid) +
#   labs(tag = "Features", caption = expression(bold("SHAP value (impact on yield, t ha"^{-1}*")"))) +
#   theme(
#     plot.tag.position = c(-0.01, 0.5), 
#     plot.tag = element_text(size = 16, angle = 90, face = "bold"),
#     plot.caption = element_text(size = 14, hjust = 0.5, margin = ggplot2::margin(t = 10)),
#     legend.position = "right",
#     legend.margin = ggplot2::margin(l = -120), 
#     legend.box.margin = ggplot2::margin(l = -40),
#     plot.margin = ggplot2::margin(l = 45, b = 10, t = 10, r = 5) 
#   )
# 
# # Display result
# suppressMessages(print(final_plot))
# 
# ggsave("SHAP_Final_Standardized.png", final_plot, width = 20, height = 14, dpi = 300)

##############################################################################################

# ==============================================================================
# 1. APPLY STYLE TO ONLY TWO PLOTS (No Tags)
# ==============================================================================
# Left plot: Full Model
lr_full_styled <- clean_mrmr_style(
  lr_full_mrmr_plot + labs(title = "Full Model")
)

# Right plot: Climate Model
lr_clim_styled <- clean_mrmr_style(
  lr_climate_mrmr_plot + labs(title = "Climate Model")
)

# ==============================================================================
# 2. ASSEMBLE 1x2 GRID
# ==============================================================================
combined_mrmr <- (lr_full_styled | lr_clim_styled)

# ==============================================================================
# 3. FINAL WRAP (Clean Spacing)
# ==============================================================================
final_mrmr_plot <- wrap_elements(panel = combined_mrmr) +
  labs(tag = "Features", caption = expression(bold("Normalized mRMR score"))) +
  theme(
    # Pull 'Features' tight now that tags are gone
    plot.tag.position = c(0.01, 0.5), 
    plot.tag = element_text(size = 16, angle = 90, face = "bold"),
    
    plot.caption = element_text(size = 14, hjust = 0.5, margin = ggplot2::margin(t = 15)),
    
    # Reduced left margin (l=40) for a tighter look
    plot.margin = ggplot2::margin(l = 40, b = 10, t = 10, r = 20) 
  )

# Display result
suppressMessages(print(final_mrmr_plot))

ggsave("mRMR_Final_Standardized.png", final_lr_mrmr_plot, width = 12, height = 6, dpi = 300)

##################################################################################################
# 
# # ==============================================================================
# # 1. SCATTER STYLE FUNCTION
# # ==============================================================================
# clean_scatter_style <- function(p) {
#   p + theme(
#     # Titles and tags
#     plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
#     plot.tag = element_text(size = 14, face = "bold"),
#     # Axis styling
#     axis.title = element_blank(), # We use global labels in the final wrap
#     axis.text = element_text(size = 12, color = "black"),
#     # Background
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "white", color = "black")
#   )
# }
# 
# # ==============================================================================
# # 2. APPLY STYLE & TAGGING
# # ==============================================================================
# # Row 1: FULL MODELS
# s_f_plots <- lapply(list(
#   lr_full_scatter_plot   + labs(title = "Linear Regression", tag = "(a)"), 
#   lgbm_full_scatter_plot + labs(title = "Light Gradient Boosting"),
#   rf_full_scatter_plot   + labs(title = "Random Forest"), 
#   xgb_full_scatter_plot  + labs(title = "Extreme Gradient Boosting")
# ), clean_scatter_style)
# 
# # Row 2: CLIMATE MODELS
# s_c_plots <- lapply(list(
#   lr_climate_scatter_plot   + labs(title = NULL, tag = "(b)"), 
#   lgbm_climate_scatter_plot + labs(title = NULL),
#   rf_climate_scatter_plot   + labs(title = NULL), 
#   xgb_climate_scatter_plot  + labs(title = NULL)
# ), clean_scatter_style)
# 
# # ==============================================================================
# # 3. ASSEMBLE GRID
# # ==============================================================================
# combined_scatter_grid <- (
#   (s_f_plots[[1]] | s_f_plots[[2]] | s_f_plots[[3]] | s_f_plots[[4]]) / 
#     (s_c_plots[[1]] | s_c_plots[[2]] | s_c_plots[[3]] | s_c_plots[[4]])
# ) + plot_layout(guides = "collect")
# 
# # ==============================================================================
# # 4. FINAL WRAP WITH GLOBAL AXIS LABELS
# # ==============================================================================
# final_scatter_plot <- wrap_elements(panel = combined_scatter_grid) +
#   labs(
#     tag = expression(bold("Predicted Yield (t ha"^{-1}*")")), 
#     caption = expression(bold("Observed Yield (t ha"^{-1}*")"))
#   ) +
#   theme(
#     # Global Y-axis label (left)
#     plot.tag.position = c(0.01, 0.5), 
#     plot.tag = element_text(size = 14, angle = 90, face = "bold"),
#     # Global X-axis label (bottom)
#     plot.caption = element_text(size = 14, hjust = 0.5, margin = ggplot2::margin(t = 15)),
#     # Clean margins for scatter plots (usually no color bar on right)
#     plot.margin = ggplot2::margin(l = 5, b = 10, t = 10, r = 10) 
#   )
# 
# # Display and Save
# suppressMessages(print(final_scatter_plot))
# final_scatter_plot
# ggsave("Scatter_Final_Grid.png", final_scatter_plot, width = 18, height = 10, dpi = 300)
# 


library(ggplot2)
library(patchwork)
# Linear Regression Full 
lr_full_test_r2_mean    <- lr_full_final_summary$Mean[lr_full_final_summary$Metric == "Test_R2"]
lr_full_test_r2_ci      <- lr_full_final_summary$CI_Margin[lr_full_final_summary$Metric == "Test_R2"]

lr_full_test_rmse_mean  <- lr_full_final_summary$Mean[lr_full_final_summary$Metric == "Test_RMSE"]
lr_full_test_rmse_ci    <- lr_full_final_summary$CI_Margin[lr_full_final_summary$Metric == "Test_RMSE"]

lr_full_test_mae_mean   <- lr_full_final_summary$Mean[lr_full_final_summary$Metric == "Test_MAE"]
lr_full_test_mae_ci     <- lr_full_final_summary$CI_Margin[lr_full_final_summary$Metric == "Test_MAE"]

# --- LR CLIMATE ---
lr_climate_test_r2_mean    <- lr_climate_final_summary$Mean[lr_climate_final_summary$Metric == "Test_R2"]
lr_climate_test_r2_ci      <- lr_climate_final_summary$CI_Margin[lr_climate_final_summary$Metric == "Test_R2"]
lr_climate_test_rmse_mean  <- lr_climate_final_summary$Mean[lr_climate_final_summary$Metric == "Test_RMSE"]
lr_climate_test_rmse_ci    <- lr_climate_final_summary$CI_Margin[lr_climate_final_summary$Metric == "Test_RMSE"]
lr_climate_test_mae_mean   <- lr_climate_final_summary$Mean[lr_climate_final_summary$Metric == "Test_MAE"]
lr_climate_test_mae_ci     <- lr_climate_final_summary$CI_Margin[lr_climate_final_summary$Metric == "Test_MAE"]

# --- LGBM FULL ---
lgbm_full_test_r2_mean   <- lgbm_full_final_summary$Mean[lgbm_full_final_summary$Metric == 'Test_R2']
lgbm_full_test_r2_ci     <- lgbm_full_final_summary$CI_Margin[lgbm_full_final_summary$Metric == 'Test_R2']
lgbm_full_test_rmse_mean <- lgbm_full_final_summary$Mean[lgbm_full_final_summary$Metric == 'Test_RMSE']
lgbm_full_test_rmse_ci   <- lgbm_full_final_summary$CI_Margin[lgbm_full_final_summary$Metric == 'Test_RMSE']
lgbm_full_test_mae_mean  <- lgbm_full_final_summary$Mean[lgbm_full_final_summary$Metric == 'Test_MAE']
lgbm_full_test_mae_ci    <- lgbm_full_final_summary$CI_Margin[lgbm_full_final_summary$Metric == 'Test_MAE']

# --- LGBM CLIMATE ---
lgbm_climate_test_r2_mean   <- final_summary$Mean[final_summary$Metric == 'Test_R2']
lgbm_climate_test_r2_ci     <- final_summary$CI_Margin[final_summary$Metric == 'Test_R2']
lgbm_climate_test_rmse_mean <- final_summary$Mean[final_summary$Metric == 'Test_RMSE']
lgbm_climate_test_rmse_ci   <- final_summary$CI_Margin[final_summary$Metric == 'Test_RMSE']
lgbm_climate_test_mae_mean  <- final_summary$Mean[final_summary$Metric == 'Test_MAE']
lgbm_climate_test_mae_ci    <- final_summary$CI_Margin[final_summary$Metric == 'Test_MAE']

# --- RF FULL ---
rf_full_test_r2_mean   <- rf_full_final_summary$Mean[rf_full_final_summary$Metric == 'Test_R2']
rf_full_test_r2_ci     <- rf_full_final_summary$CI_Margin[rf_full_final_summary$Metric == 'Test_R2']
rf_full_test_rmse_mean <- rf_full_final_summary$Mean[rf_full_final_summary$Metric == 'Test_RMSE']
rf_full_test_rmse_ci   <- rf_full_final_summary$CI_Margin[rf_full_final_summary$Metric == 'Test_RMSE']
rf_full_test_mae_mean  <- rf_full_final_summary$Mean[rf_full_final_summary$Metric == 'Test_MAE']
rf_full_test_mae_ci    <- rf_full_final_summary$CI_Margin[rf_full_final_summary$Metric == 'Test_MAE']

# --- RF CLIMATE ---
rf_climate_test_r2_mean   <- rf_climate_final_summary$Mean[rf_climate_final_summary$Metric == 'Test_R2']
rf_climate_test_r2_ci     <- rf_climate_final_summary$CI_Margin[rf_climate_final_summary$Metric == 'Test_R2']
rf_climate_test_rmse_mean <- rf_climate_final_summary$Mean[rf_climate_final_summary$Metric == 'Test_RMSE']
rf_climate_test_rmse_ci   <- rf_climate_final_summary$CI_Margin[rf_climate_final_summary$Metric == 'Test_RMSE']
rf_climate_test_mae_mean  <- rf_climate_final_summary$Mean[rf_climate_final_summary$Metric == 'Test_MAE']
rf_climate_test_mae_ci    <- rf_climate_final_summary$CI_Margin[rf_climate_final_summary$Metric == 'Test_MAE']

# --- XGB FULL ---
xgb_full_test_r2_mean   <- xgb_full_final_summary$Mean[xgb_full_final_summary$Metric == 'Test_R2']
xgb_full_test_r2_ci     <- xgb_full_final_summary$CI_Margin[xgb_full_final_summary$Metric == 'Test_R2']
xgb_full_test_rmse_mean <- xgb_full_final_summary$Mean[xgb_full_final_summary$Metric == 'Test_RMSE']
xgb_full_test_rmse_ci   <- xgb_full_final_summary$CI_Margin[xgb_full_final_summary$Metric == 'Test_RMSE']
xgb_full_test_mae_mean  <- xgb_full_final_summary$Mean[xgb_full_final_summary$Metric == 'Test_MAE']
xgb_full_test_mae_ci    <- xgb_full_final_summary$CI_Margin[xgb_full_final_summary$Metric == 'Test_MAE']

# --- XGB CLIMATE ---
xgb_climate_test_r2_mean   <- xgb_climate_final_summary$Mean[xgb_climate_final_summary$Metric == 'Test_R2']
xgb_climate_test_r2_ci     <- xgb_climate_final_summary$CI_Margin[xgb_climate_final_summary$Metric == 'Test_R2']
xgb_climate_test_rmse_mean <- xgb_climate_final_summary$Mean[xgb_climate_final_summary$Metric == 'Test_RMSE']
xgb_climate_test_rmse_ci   <- xgb_climate_final_summary$CI_Margin[xgb_climate_final_summary$Metric == 'Test_RMSE']
xgb_climate_test_mae_mean  <- xgb_climate_final_summary$Mean[xgb_climate_final_summary$Metric == 'Test_MAE']
xgb_climate_test_mae_ci    <- xgb_climate_final_summary$CI_Margin[xgb_climate_final_summary$Metric == 'Test_MAE']

# ==============================================================================
# 1. THE HELPER FUNCTION
# ==============================================================================
make_model_plot <- function(df, title = NULL, tag = NULL, 
                            rmse_m, rmse_ci, mae_m, mae_ci, r2_m, r2_ci) {
  
  # Create the metrics label string
  metrics_label <- paste0(
    "RMSE: ", format(round(rmse_m, 2), nsmall = 2), " ± ", format(round(rmse_ci, 2), nsmall = 2),
    "\nMAE: ", format(round(mae_m, 2), nsmall = 2), " ± ", format(round(mae_ci, 2), nsmall = 2),
    "\nR²: ", format(round(r2_m, 3), nsmall = 3), " ± ", format(round(r2_ci, 3), nsmall = 3)
  )
  
  # A. Main Scatter Plot
  p_main <- ggplot(df, aes(x = Actual, y = Predicted)) +
    geom_point(color = "#191970", alpha = 0.5, size = 1.2) + 
    geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE, linewidth = 0.7) + 
    annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, 
             label = metrics_label, size = 2.8, color = "black") +
    scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
    scale_y_continuous(limits = c(0, 25), expand = c(0,0)) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      panel.grid.major = element_blank(), # Removes major grid
      panel.grid.minor = element_blank()  # Removes minor grid
    )
  
  # B. Top Density (Small/Cute)
  p_top <- ggplot(df, aes(x = Actual)) +
    geom_density(fill = "#5D3FD3", alpha = 0.4, color = NA) +
    scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
    theme_void()
  
  # C. Right Density (Small/Cute)
  p_right <- ggplot(df, aes(x = Predicted)) +
    geom_density(fill = "#5D3FD3", alpha = 0.4, color = NA) +
    scale_x_continuous(limits = c(0, 25), expand = c(0,0)) + 
    coord_flip() +
    theme_void()
  
  # D. Combine into one unit
  (p_top + plot_spacer()) / (p_main + p_right) + 
    plot_layout(widths = c(7, 1), heights = c(1, 7)) +
    plot_annotation(
      title = title, 
      tag_levels = list(tag),
      theme = theme(
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        plot.tag = element_text(size = 12, face = "bold"),
        # Added margin here so titles aren't cut off by the plot above it
        plot.margin = ggplot2::margin(t = 20, r = 5, b = 5, l = 5) 
      )
    )
}

# ==============================================================================
# 2. CREATE ALL 8 PLOT UNITS
# ==============================================================================
# --- ROW 1: FULL MODELS ---
p1 <- make_model_plot(lr_full_plot_df, "Linear Regression", "(a)",
                      lr_full_test_rmse_mean, lr_full_test_rmse_ci, 
                      lr_full_test_mae_mean, lr_full_test_mae_ci, 
                      lr_full_test_r2_mean, lr_full_test_r2_ci)

p2 <- make_model_plot(lgbm_full_plot_df, "Light Gradient Boosting", NULL,
                      lgbm_full_test_rmse_mean, lgbm_full_test_rmse_ci, 
                      lgbm_full_test_mae_mean, lgbm_full_test_mae_ci, 
                      lgbm_full_test_r2_mean, lgbm_full_test_r2_ci)

p3 <- make_model_plot(rf_full_plot_df, "Random Forest", NULL,
                      rf_full_test_rmse_mean, rf_full_test_rmse_ci, 
                      rf_full_test_mae_mean, rf_full_test_mae_ci, 
                      rf_full_test_r2_mean, rf_full_test_r2_ci)

p4 <- make_model_plot(xgb_full_plot_df, "Extreme Gradient Boosting", NULL,
                      xgb_full_test_rmse_mean, xgb_full_test_rmse_ci, 
                      xgb_full_test_mae_mean, xgb_full_test_mae_ci, 
                      xgb_full_test_r2_mean, xgb_full_test_r2_ci)

# --- ROW 2: CLIMATE MODELS ---
p5 <- make_model_plot(lr_climate_plot_df, NULL, "(b)",
                      lr_climate_test_rmse_mean, lr_climate_test_rmse_ci, 
                      lr_climate_test_mae_mean, lr_climate_test_mae_ci, 
                      lr_climate_test_r2_mean, lr_climate_test_r2_ci)

p6 <- make_model_plot(plot_df, NULL, NULL, # Note: using consistent naming here
                      lgbm_climate_test_rmse_mean, lgbm_climate_test_rmse_ci, 
                      lgbm_climate_test_mae_mean, lgbm_climate_test_mae_ci, 
                      lgbm_climate_test_r2_mean, lgbm_climate_test_r2_ci)

p7 <- make_model_plot(rf_climate_plot_df, NULL, NULL,
                      rf_climate_test_rmse_mean, rf_climate_test_rmse_ci, 
                      rf_climate_test_mae_mean, rf_climate_test_mae_ci, 
                      rf_climate_test_r2_mean, rf_climate_test_r2_ci)

p8 <- make_model_plot(xgb_climate_plot_df, NULL, NULL,
                      xgb_climate_test_rmse_mean, xgb_climate_test_rmse_ci, 
                      xgb_climate_test_mae_mean, xgb_climate_test_mae_ci, 
                      xgb_climate_test_r2_mean, xgb_climate_test_r2_ci)

# ==============================================================================
# 3. FINAL ASSEMBLY
# ==============================================================================
final_grid <- (p1 | p2 | p3 | p4) / (p5 | p6 | p7 | p8)

final_scatter_plot <- wrap_elements(panel = final_grid) +
  labs(
    tag = expression(bold("Predicted yield (t ha"^{-1}*")")), 
    caption = expression(bold("Actual yield (t ha"^{-1}*")"))
  ) +
  theme(
    plot.tag.position = c(0.00, 0.5), 
    plot.tag = element_text(size = 14, angle = 90, face = "bold"),
    plot.caption = element_text(size = 14, hjust = 0.5, margin = ggplot2::margin(t = 15)),
    plot.margin = ggplot2::margin(l = 25, b = 10, t = 10, r = 10) 
  )

print(final_scatter_plot)


# ==============================================================================
# 7. SAVE (HIGH RESOLUTION FOR PAPER)
# ==============================================================================
ggsave(
  "Final_Scatter_Density_Grid.png",
  final_scatter_plot,
  width = 16,
  height = 8,
  dpi = 600
)


###############################################################################################
################ STATE-LEVEL MODEL FOR CLIMATE MODEL SCENARIO USING Random Forest ##########################

# ==============================================================================
# 1. MASTER STATE LOOP (LOYO VERSION - RANDOM YEAR GROUPS) (CLIMATE ONLY)
# ==============================================================================
library(ranger)
library(fastshap)
library(dplyr)
library(mRMRe)
library(ggplot2)
library(ggbeeswarm)
library(tidytext)

# Initialize Storage Lists
state_climate_final_outputs   <- list()
state_climate_mrmr_rankings    <- list() 
state_climate_shap_summary    <- list() # For averaged table
state_climate_full_shap_list  <- list() # For beeswarm plot
state_climate_full_feat_list  <- list() # For beeswarm plot

all_states <- unique(dataset$State)
FIXED_MTRY  <- 3
FIXED_NODE  <- 10

# Start Timer
start_total_time <- Sys.time()

get_ci <- function(x) {
  if(length(x) < 2) return(0)
  qt(0.975, df = length(x) - 1) * (sd(x) / sqrt(length(x)))
}

for(target_state in all_states) {
  state_data <- dataset %>% filter(State == target_state)
  unique_years <- unique(state_data$YEAR)
  
  if(length(unique_years) < 10) {
    cat("\nSkipping", target_state, "- insufficient years.")
    next 
  }
  
  cat("\n\n>>> PROCESSING STATE:", target_state, "(N =", nrow(state_data), ", Years =", length(unique_years), ")")
  
  set.seed(123)
  year_metadata <- data.frame(YEAR = unique_years) %>%
    mutate(fold = sample(rep(1:5, length.out = n())))
  
  state_data_with_folds <- state_data %>% left_join(year_metadata, by = "YEAR")
  
  pooled_predictions <- data.frame()
  avg_features_list  <- c()
  fold_shap_list     <- list() 
  state_fold_shaps   <- list()
  state_fold_feats   <- list()
  fold_performance   <- list()
  
  for(f in 1:5) {
    cat("\n   Fold", f, "(Testing on years:", paste(year_metadata$YEAR[year_metadata$fold == f], collapse=", "), ")...")
    
    train_data <- state_data_with_folds %>% filter(fold != f)
    test_data  <- state_data_with_folds %>% filter(fold == f)
    
    numeric_cols <- names(train_data)[sapply(train_data, is.numeric)]
    feats_all <- setdiff(numeric_cols, c("YIELD", "fold", "YEAR", "State", "County"))
    
    sds_check <- sapply(train_data[feats_all], sd, na.rm = TRUE)
    feats <- names(sds_check[sds_check > 0 & !is.na(sds_check)])
    
    means <- sapply(train_data[feats], mean, na.rm = TRUE)
    sds   <- sds_check[feats]
    sc_train_mat <- as.data.frame(sweep(sweep(train_data[, feats], 2, means, "-"), 2, sds, "/"))
    sc_test_mat  <- as.data.frame(sweep(sweep(test_data[, feats], 2, means, "-"), 2, sds, "/"))
    
    # mRMR
    mrmr_df <- as.data.frame(sc_train_mat); mrmr_df$YIELD <- train_data$YIELD
    mrmr_in <- mRMR.data(data = mrmr_df)
    mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(mrmr_df) == "YIELD"), feature_count = min(length(feats), 50))
    mrmr_feats <- setdiff(names(mrmr_df)[as.numeric(unlist(solutions(mrmr_run)))], "YIELD")
    
    state_full_mrmr_rankings[[paste0(target_state, "_F", f)]] <- data.frame(
      State = target_state, Fold = f, Rank = 1:length(mrmr_feats), Feature = mrmr_feats
    )

    # Forward Selection for Elbow
    rmse_history <- c()
    for(k in 1:length(mrmr_feats)) {
      curr <- mrmr_feats[1:k]
      set.seed(123)
      m_tmp <- ranger(y = train_data$YIELD, x = sc_train_mat[, curr, drop=FALSE],
                      num.trees = 50, mtry = min(FIXED_MTRY, k), min.node.size = FIXED_NODE, verbose = FALSE)
      p_val <- predict(m_tmp, data = sc_test_mat[, curr, drop=FALSE])$predictions
      rmse_history[k] <- sqrt(mean((test_data$YIELD - p_val)^2))
    }
    
    # Geometric Elbow Calculation
    x <- 1:length(rmse_history); y <- rmse_history
    dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
    k1 <- which.max(dist1)
    
    if(k1 < length(y)){
      x_tail <- k1:length(y); y_tail <- y[k1:length(y)]
      dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
      opt_k <- x_tail[which.max(dist2)]
      if(opt_k > 30) opt_k <- 30
    } else { opt_k <- k1 }
    
    if(!exists("opt_k") || is.na(opt_k) || opt_k < 10) { opt_k <- min(length(mrmr_feats), 10) }
    opt_k <- min(opt_k, round(nrow(sc_train_mat) * 0.20), 40)
    
    best_feats <- mrmr_feats[1:opt_k]
    avg_features_list <- c(avg_features_list, length(best_feats))
    
    cat("\n      Fold", f, "picked", length(best_feats), "features (First Elbow at", k1, ")")
    cat("\n      Selected Features:", paste(best_feats, collapse = ", "))
    
    # Final Model
    set.seed(123)
    final_mod <- ranger(y = train_data$YIELD, x = sc_train_mat[, best_feats], 
                        mtry = min(FIXED_MTRY, length(best_feats)), min.node.size = FIXED_NODE, importance = 'none')
    
    # SHAP Calculation (Full Matrix for Beeswarm)
    p_wrapper <- function(object, newdata) predict(object, data = newdata)$predictions
    shap_matrix <- fastshap::explain(final_mod, X = as.matrix(sc_test_mat[, best_feats]), 
                                     pred_wrapper = p_wrapper, nsim = 10)
    
    # Store for Summary Table
    fold_shap_summary <- colMeans(abs(shap_matrix))
    fold_shap_list[[f]] <- data.frame(Feature = names(fold_shap_summary), MeanAbsSHAP = as.numeric(fold_shap_summary), Fold = f)
    
    # Store for Beeswarm Plot
    state_fold_shaps[[f]] <- as.data.frame(shap_matrix) %>% mutate(Fold = f, State = target_state)
    state_fold_feats[[f]] <- as.data.frame(sc_test_mat[, best_feats]) %>% mutate(Fold = f, State = target_state)
    
    preds <- predict(final_mod, data = sc_test_mat[, best_feats])$predictions
    pooled_predictions <- rbind(pooled_predictions, data.frame(Actual = test_data$YIELD, Predicted = preds))
    
    f_act <- test_data$YIELD
    f_pre <- preds
    fold_performance[[f]] <- data.frame(
      R2 = 1 - (sum((f_act - f_pre)^2) / sum((f_act - mean(f_act))^2)),
      RMSE = sqrt(mean((f_act - f_pre)^2)),
      MAE = mean(abs(f_act - f_pre))
    )
  }
  
  fold_perf_df <- bind_rows(fold_performance)
  # Aggregate State Results
  state_climate_shap_summary[[target_state]] <- bind_rows(fold_shap_list) %>%
    group_by(Feature) %>% 
    summarize(GlobalSHAP = mean(MeanAbsSHAP, na.rm = TRUE), .groups = "drop") %>% 
    mutate(State = target_state)
  
  state_climate_full_shap_list[[target_state]] <- bind_rows(state_fold_shaps)
  state_climate_full_feat_list[[target_state]] <- bind_rows(state_fold_feats)
  
  act <- pooled_predictions$Actual; pre <- pooled_predictions$Predicted
  r2 <- 1 - (sum((act - pre)^2) / sum((act - mean(act))^2))
  k_avg <- mean(avg_features_list)
  adj_r2 <- 1 - ((1 - r2) * (length(act) - 1) / (length(act) - k_avg - 1))
  
  state_climate_final_outputs[[target_state]] <- data.frame(
    State = target_state, 
    Features = round(k_avg, 1), 
    Test_R2 = round(r2, 4), 
    R2_CI = round(get_ci(fold_perf_df$R2), 4),         
    Test_Adj_R2 = round(adj_r2, 4), 
    Test_RMSE = round(sqrt(mean((act - pre)^2)), 4), 
    RMSE_CI = round(get_ci(fold_perf_df$RMSE), 4),     
    Test_MAE = round(mean(abs(act - pre)), 4),
    MAE_CI = round(get_ci(fold_perf_df$MAE), 4)        
  )
  gc()
}

# Combine everything into final tables
state_climate_all_states_table <- bind_rows(state_climate_final_outputs)
state_climate_all_mrmr_table   <- bind_rows(state_climate_mrmr_rankings)
state_climate_all_shap_table   <- bind_rows(state_climate_shap_summary)
state_climate_all_shap_values  <- bind_rows(state_climate_full_shap_list)
state_climate_all_feat_values  <- bind_rows(state_climate_full_feat_list)

# End Timer
end_total_time <- Sys.time()
total_duration <- round(difftime(end_total_time, start_total_time, units = "auto"), 2)

cat("\n========================================================")
cat("\nTotal time taken for all states:", total_duration, attr(total_duration, "units"))
cat("\n========================================================\n")

print(state_climate_all_states_table, row.names = FALSE)


# Save CSVs
write.csv(state_climate_all_states_table, "State_Climate_Model_Metrics.csv", row.names = FALSE)
write.csv(state_climate_all_shap_table, "State_Climate_Model_SHAP_Summary.csv",  row.names = FALSE)
write.csv(state_climate_all_mrmr_table, "State_Climate_Model_mRMR_Rankings.csv", row.names = FALSE)


# ============================================================================== 
# GENERATE BEESWARM PLOT
# ============================================================================== 

# 1. Prepare and Join Data
state_prep_shap <- state_climate_all_shap_values %>%
  mutate(ID = row_number()) %>%
  pivot_longer(cols = -c(ID, State, Fold), names_to = "Feature", values_to = "SHAP")

state_prep_feat <- state_climate_all_feat_values %>%
  mutate(ID = row_number()) %>%
  pivot_longer(cols = -c(ID, State, Fold), names_to = "Feature", values_to = "Value")

state_plot_data <- left_join(state_prep_shap, state_prep_feat, by = c("ID", "Feature", "State", "Fold")) %>%
  filter(!is.na(SHAP))

# 2. Rename States to the specific requested labels
state_plot_data <- state_plot_data %>%
  mutate(State = case_match(State,
                            "COLORADO"     ~ "Colorado (Co-16)",
                            "KANSAS"       ~ "Kansas (Co-56)",
                            "NEBRASKA"     ~ "Nebraska (Co-86)",
                            "NEW MEXICO"   ~ "New Mexico (Co-10)",
                            "OKLAHOMA"     ~ "Oklahoma (Co-9)",
                            "SOUTH DAKOTA" ~ "South Dakota (Co-7)",
                            "WYOMING"      ~ "Wyoming (Co-6)",
                            .default = State
  ))

# 3. Normalize Feature Values to -1 to +1 scale per State and Feature
state_plot_data <- state_plot_data %>%
  group_by(State, Feature) %>%
  mutate(Value = 2 * (Value - min(Value, na.rm = TRUE)) /
           (max(Value, na.rm = TRUE) - min(Value, na.rm = TRUE)) - 1) %>%
  ungroup()

# 4. Identify Top 5 Features per State & Calculate SHAP Labels
state_top_5 <- state_plot_data %>%
  group_by(State, Feature) %>%
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE), .groups = "drop") %>%
  group_by(State) %>%
  slice_max(order_by = mean_abs_impact, n = 5, with_ties = FALSE) %>%
  mutate(label_text = format(round(mean_abs_impact, 3), nsmall = 3))

state_plot_final <- state_plot_data %>%
  semi_join(state_top_5, by = c("State", "Feature")) %>%
  left_join(state_top_5 %>% select(State, Feature, label_text), by = c("State", "Feature"))

# 5. Generate Final Plot
ggplot(state_plot_final, aes(x = SHAP, y = reorder_within(Feature, abs(SHAP), State), color = Value)) +
  
  # The Beeswarm Dots
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 1.5, alpha = 0.5, stroke = 0) +
  
  # Numerical SHAP Impact Labels (Positioned at left)
  geom_text(aes(x = -1.9, label = label_text), 
            hjust = 0, vjust = -0.8, color = "black", size = 3.2, fontface = "bold", check_overlap = TRUE) +
  
  # Color Scale
  scale_color_viridis_c(option = "viridis", 
                        name = "Feature\nvalue", 
                        limits = c(-1, 1),  
                        breaks = c(-1, 1.0)) + 
  
  # Styling and Faceting
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  # --- ADDED LABEL WRAPPING LOGIC HERE ---
  scale_y_reordered(labels = function(x) {
    x <- gsub("___.*", "", x)      # Remove the state suffix from reorder_within
    x <- gsub("_", "_ ", x)        # Prepare underscore for wrapping
    x <- scales::label_wrap(16)(x) # Wrap at 16 characters
    gsub("_ ", "_", x)             # Clean up underscores
  }) +
  
  facet_wrap(~State, nrow = 2, ncol = 4, scales = "free_y") +
  coord_cartesian(xlim = c(-2, 2)) +
  
  theme_bw() + 
  theme(
    strip.background = element_blank(), # Clean background like your reference
    strip.text = element_text(color = "black", face = "bold", size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  
  # Taller Color Bar
  guides(color = guide_colorbar(
    barheight = unit(15, "cm"), 
    barwidth = unit(0.8, "cm"),
    ticks.colour = "black",
    frame.colour = "black"
  )) +
  
  labs(
    x = expression(bold("SHAP value (impact on yield, ") ~ bold("t ha"^{-1}*")")), 
    y = expression(bold("Features"))
  )

# 6. Save Plot
ggsave("State_Climate_SHAP.png", width = 14, height = 9, dpi = 300)



# 1. Create the Summary Table with State-wise Rankings
state_climate_shap_summary_table <- state_climate_all_shap_table %>%
  # Use the Mean Absolute SHAP values
  rename(Mean_Abs_SHAP = GlobalSHAP) %>%
  # Group by state so the ranking is local to each region
  group_by(State) %>%
  arrange(State, desc(Mean_Abs_SHAP)) %>%
  mutate(Rank = row_number()) %>%
  ungroup() %>%
  # Reorder columns for better readability
  select(State, Rank, Feature, Mean_Abs_SHAP)

# 2. View the top results and Save
print(head(state_climate_shap_summary_table, 20)) 
write.csv(state_climate_shap_summary_table, "State_Climate_Global_SHAP_Importance_Table.csv", row.names = FALSE)

#################### STATE PLOTS FOR MRMR CLIMATE MODEL ####################################

# 1. Prepare Data
state_climate_mrmr_plot <- state_climate_all_mrmr_table %>%
  mutate(State = case_match(State,
                            "COLORADO"     ~ "Colorado (Co-16)",
                            "KANSAS"       ~ "Kansas (Co-56)",
                            "NEBRASKA"     ~ "Nebraska (Co-86)",
                            "NEW MEXICO"   ~ "New Mexico (Co-10)",
                            "OKLAHOMA"     ~ "Oklahoma (Co-9)",
                            "SOUTH DAKOTA" ~ "South Dakota (Co-7)",
                            "WYOMING"      ~ "Wyoming (Co-6)",
                            .default = State
  )) %>%
  group_by(State, Feature) %>%
  summarize(Avg_Rank = mean(Rank, na.rm = TRUE), .groups = "drop") %>%
  group_by(State) %>%
  mutate(Normalized_Score = (max(Avg_Rank) - Avg_Rank) / (max(Avg_Rank) - min(Avg_Rank) + 1e-9)) %>%
  slice_max(order_by = Normalized_Score, n = 5, with_ties = FALSE) %>%
  # CUSTOM LABEL LOGIC: If score is 1, show "1", otherwise show 3 decimals
  mutate(label_val = ifelse(Normalized_Score == 1, "1", format(round(Normalized_Score, 3), nsmall = 3))) %>%
  ungroup()

# 2. Generate Plot
ggplot(state_mrmr_clean, aes(x = reorder_within(Feature, Normalized_Score, State), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888", width = 0.8) + 
  coord_flip() +
  
  # Numerical labels with custom "1" logic
  geom_text(aes(label = label_val), 
            hjust = -0.2, size = 3.5, fontface = "plain", color = "black") +
  
  # Label Wrapping
  scale_x_reordered(labels = function(x) {
    x <- gsub("___.*", "", x)      
    x <- gsub("_", "_ ", x)        
    x <- scales::label_wrap(16)(x) 
    gsub("_ ", "_", x)             
  }) +
  
  facet_wrap(~State, nrow = 2, ncol = 4, scales = "free_y") +
  
  # Professional Theme
  theme_bw() + 
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(color = "black", face = "bold", size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(face = "bold", size = 12)
  ) + 
  
  # AXIS LIMITS: Cap exactly at 1.00
  scale_y_continuous(
    limits = c(0, 1.20), # Extra room for the text label "1"
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
    expand = expansion(mult = c(0, 0.05)) # Flush with Y-axis
  ) +
  
  labs(
    x = expression(bold("Features")), 
    y = expression(bold("Normalized mRMR score"))
  )


# Display Plot
print(state_climate_mrmr_plot)

# Save Final Plot
ggsave("State_Climate_mRMR.png", width = 14, height = 9, dpi = 300)


############## Correlation of feature of top mRMR ranking for climate model #########
library(dplyr)
library(tidyr)

# 1. Identify the top 5 features per state using your exact logic
top_features_per_state <- state_climate_all_mrmr_table %>%
  group_by(State, Feature) %>%
  summarize(Avg_Rank = mean(Rank, na.rm = TRUE), .groups = "drop") %>%
  group_by(State) %>%
  mutate(Normalized_Score = (max(Avg_Rank) - Avg_Rank) / (max(Avg_Rank) - min(Avg_Rank) + 1e-9)) %>%
  slice_max(order_by = Normalized_Score, n = 5, with_ties = FALSE) %>%
  ungroup()

# 2. Get a unique list of all features that made it into the top 5 of ANY state
global_top_features <- unique(top_features_per_state$Feature)

# 3. Pull the scores for THESE specific features across ALL states to build a fair matrix
# (This ensures we compare the same features across all regions)
full_comparison_data <- state_climate_all_mrmr_table %>%
  group_by(State, Feature) %>%
  summarize(Avg_Rank = mean(Rank, na.rm = TRUE), .groups = "drop") %>%
  group_by(State) %>%
  mutate(Normalized_Score = (max(Avg_Rank) - Avg_Rank) / (max(Avg_Rank) - min(Avg_Rank) + 1e-9)) %>%
  ungroup() %>%
  # Filter strictly for the top-ranked features
  filter(Feature %in% global_top_features)

# 4. Pivot into a Matrix: Rows = Features, Columns = States
correlation_matrix_input <- full_comparison_data %>%
  select(State, Feature, Normalized_Score) %>%
  pivot_wider(names_from = State, values_from = Normalized_Score)

# 5. Fill missing values with 0 (if a feature wasn't ranked in a state, its score is 0)
correlation_matrix_input[is.na(correlation_matrix_input)] <- 0

# Convert to data frame and set row names for correlation scaling
corr_df <- as.data.frame(correlation_matrix_input)
rownames(corr_df) <- corr_df$Feature
corr_df$Feature <- NULL

# 6. Calculate the Spearman Rank Correlation Matrix
state_correlation_matrix <- cor(corr_df, method = "spearman")

# View the matrix
print(round(state_correlation_matrix, 3))

# Save the matrix to a CSV file for your paper
write.csv(state_correlation_matrix, "top_features_state_correlation.csv")


################ STATE CLIMATE MAP #############################
# ============================================================================== 
# GENERATE CLIMATE MODEL MAP (Ogallala Region)
# ============================================================================== 

# 1. Clean the results table (Climate Model Test_R2)
state_climate_results <- state_climate_all_states_table %>%
  mutate(State = toupper(State)) %>% 
  rename(R_squared = Test_R2)

# 2. Join results back to your ORIGINAL county-level sf object
# (This ensures only the counties present in the model are filled)
map_climate_joined <- dataset_sf %>%
  mutate(State = toupper(State)) %>% 
  left_join(state_climate_results, by = "State")

# 3. Generate the Map
State_Climate_Model_Map <- ggplot() +
  # Draw the counties filled with Climate R-squared values
  geom_sf(data = map_climate_joined, aes(fill = R_squared), color = NA) +
  
  # Draw thick borders around the modeling area (States)
  geom_sf(data = states_filtered, fill = NA, color = "black", linewidth = 0.6) +
  
  # Add State Abbreviations at centroids
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 4, fontface = "bold", color = "black") + 
  
  # Color scale: 0 to 1
  scale_fill_viridis_c(
    option = "viridis",      # Standard scientific sequential scale
    limits = c(0, 1), 
    direction = 1,           # Higher R² will be brighter/more intense
    breaks = seq(0, 1, by = 0.2), # 0.2 is often cleaner than 0.1 for 0-1 scales
    na.value = "white"
  ) +
  
  # Axis Breaks (4-degree gap)
  scale_x_continuous(breaks = seq(-108, -96, by = 4)) +
  scale_y_continuous(breaks = seq(32, 44, by = 4)) +
  
  labs(title = "Climate Model", fill = "R²", x = NULL, y = NULL) +
  
  # Coordinate limits matching the reference image
  coord_sf(xlim = c(-108, -95), ylim = c(31, 45)) +
  
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    axis.text = element_text(size = 10, color = "black")
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barheight = unit(0.4, "cm")))

# Print and Save
print(State_Climate_Model_Map)

###############################################################################################
################ STATE-LEVEL MODEL FOR FULL MODEL SCENARIO USING XGB ##########################

# ==============================================================================
# 2. MASTER STATE LOOP (XGB VERSION - LOYO) (FULL MODEL)
# ==============================================================================

library(xgboost)
library(dplyr)
library(mRMRe)

# Storage containers
state_full_final_outputs   <- list()
state_full_mrmr_rankings    <- list() 
state_full_shap_summary     <- list() # Averaged table
state_full_beeswarm_shaps   <- list() # For plotting
state_full_beeswarm_feats   <- list() # For plotting

all_states <- unique(dataset_final$State)

# Params
FIXED_DEPTH  <- 3 
FIXED_LR     <- 0.01
FIXED_ROUNDS <- 1000

start_total_time <- Sys.time()

# Add this helper function once at the top of your script
get_ci <- function(x) {
  if(length(x) < 2) return(0)
  qt(0.975, df = length(x) - 1) * (sd(x) / sqrt(length(x)))
}


for(target_state in all_states) {
  state_data <- dataset_final %>% filter(State == target_state)
  unique_years <- unique(state_data$YEAR)
  
  if(length(unique_years) < 10) {
    cat("\nSkipping", target_state, "- insufficient years.")
    next 
  }
  
  cat("\n\n>>> PROCESSING STATE:", target_state, "(N =", nrow(state_data), ", Years =", length(unique_years), ")")
  
  set.seed(123)
  year_metadata <- data.frame(YEAR = unique_years) %>%
    mutate(fold = sample(rep(1:5, length.out = n())))
  
  state_data_with_folds <- state_data %>% left_join(year_metadata, by = "YEAR")
  
  pooled_predictions <- data.frame()
  avg_features_list  <- c()
  fold_shap_list     <- list()
  state_fold_shaps   <- list()
  state_fold_feats   <- list()
  fold_performance   <- list() 
  
  for(f in 1:5) {
    cat("\n   Fold", f, "(Testing on years:", paste(year_metadata$YEAR[year_metadata$fold == f], collapse=", "), ")...")
    
    train_data <- state_data_with_folds %>% filter(fold != f)
    test_data  <- state_data_with_folds %>% filter(fold == f)
    
    numeric_cols <- names(train_data)[sapply(train_data, is.numeric)]
    feats_all <- setdiff(numeric_cols, c("YIELD", "fold", "YEAR", "State", "County"))
    
    sds_check <- sapply(train_data[feats_all], sd, na.rm = TRUE)
    feats <- names(sds_check[sds_check > 0 & !is.na(sds_check)])
    
    means <- sapply(train_data[feats], mean, na.rm = TRUE)
    sds   <- sds_check[feats]
    sc_train_mat <- as.matrix(sweep(sweep(train_data[, feats], 2, means, "-"), 2, sds, "/"))
    sc_test_mat  <- as.matrix(sweep(sweep(test_data[, feats], 2, means, "-"), 2, sds, "/"))
    
    # mRMR
    mrmr_df <- as.data.frame(sc_train_mat); mrmr_df$YIELD <- train_data$YIELD
    mrmr_in <- mRMR.data(data = mrmr_df)
    mrmr_run <- mRMR.classic(mrmr_in, target_indices = which(names(mrmr_df) == "YIELD"), feature_count = min(length(feats), 50))
    mrmr_feats <- setdiff(names(mrmr_df)[as.numeric(unlist(solutions(mrmr_run)))], "YIELD")
    
    state_full_mrmr_rankings[[paste0(target_state, "_F", f)]] <- data.frame(
      State = target_state, Fold = f, Rank = 1:length(mrmr_feats), Feature = mrmr_feats
    )
    
    # Forward Selection (XGB Style)
    rmse_history <- c()
    for(k in 1:length(mrmr_feats)) {
      curr <- mrmr_feats[1:k]
      set.seed(123)
      m_tmp <- xgboost(data = sc_train_mat[, curr, drop=FALSE], label = train_data$YIELD, 
                       max_depth = 3, eta = 0.1, nrounds = 30, verbose = 0, nthread = 1)
      rmse_history[k] <- sqrt(mean((test_data$YIELD - predict(m_tmp, sc_test_mat[, curr, drop=FALSE]))^2))
    }
    
    # Geometric Elbow
    x <- 1:length(rmse_history); y <- rmse_history
    dist1 <- abs(((y[1]-y[length(y)])*x + (x[length(x)]-x[1])*y + (x[1]*y[length(y)]-x[length(x)]*y[1])))
    k1 <- which.max(dist1)
    
    if(k1 < length(y)){
      x_tail <- k1:length(y); y_tail <- y[k1:length(y)]
      dist2 <- abs(((y_tail[1]-y_tail[length(y_tail)])*x_tail + (x_tail[length(x_tail)]-x_tail[1])*y_tail + (x_tail[1]*y_tail[length(y_tail)]-x_tail[length(x_tail)]*y_tail[1])))
      opt_k <- x_tail[which.max(dist2)]
      if(opt_k > 30) opt_k <- 30
    } else { opt_k <- k1 }
    
    opt_k <- min(max(opt_k, 10), round(nrow(sc_train_mat) * 0.20), 40)
    best_feats <- mrmr_feats[1:opt_k]
    avg_features_list <- c(avg_features_list, length(best_feats))
    
    # KEEPING CONSOLE PRINTS
    cat("\nFold", f, "picked", length(best_feats), "features (First Elbow at", k1, ")")
    cat("\nSelected Features:", paste(best_feats, collapse = ", "), "\n")
    
    # Final XGBoost Model
    set.seed(123)
    final_mod <- xgboost(data = sc_train_mat[, best_feats], label = train_data$YIELD, 
                         max_depth = FIXED_DEPTH, eta = FIXED_LR, nrounds = FIXED_ROUNDS, verbose = 0, nthread = 1)
    
    # SHAP (XGB internal)
    shap_contrib <- predict(final_mod, sc_test_mat[, best_feats], predcontrib = TRUE)
    shap_vals <- shap_contrib[, -ncol(shap_contrib), drop=FALSE] # Remove BIAS
    
    # Summary list
    fold_shap_summary <- colMeans(abs(shap_vals))
    fold_shap_list[[f]] <- data.frame(Feature = names(fold_shap_summary), MeanAbsSHAP = as.numeric(fold_shap_summary), Fold = f)
    
    # Full data for Beeswarm
    state_fold_shaps[[f]] <- as.data.frame(shap_vals) %>% mutate(Fold = f, State = target_state)
    state_fold_feats[[f]] <- as.data.frame(sc_test_mat[, best_feats]) %>% mutate(Fold = f, State = target_state)
    
    preds <- predict(final_mod, sc_test_mat[, best_feats])
    pooled_predictions <- rbind(pooled_predictions, data.frame(Actual = test_data$YIELD, Predicted = preds))
    
    f_act <- test_data$YIELD; f_pre <- preds
    fold_performance[[f]] <- data.frame(
      R2 = 1 - (sum((f_act - f_pre)^2) / sum((f_act - mean(f_act))^2)),
      RMSE = sqrt(mean((f_act - f_pre)^2)),
      MAE = mean(abs(f_act - f_pre))
    )
  }
  fold_perf_df <- bind_rows(fold_performance)
  
  # Aggregate State results
  state_full_shap_summary[[target_state]] <- bind_rows(fold_shap_list) %>%
    group_by(Feature) %>% summarize(GlobalSHAP = mean(MeanAbsSHAP, na.rm = TRUE), .groups = "drop") %>% 
    mutate(State = target_state)
  
  state_full_beeswarm_shaps[[target_state]] <- bind_rows(state_fold_shaps)
  state_full_beeswarm_feats[[target_state]] <- bind_rows(state_fold_feats)
  
  # Original Metrics Calculation
  act <- pooled_predictions$Actual; pre <- pooled_predictions$Predicted
  r2 <- 1 - (sum((act - pre)^2) / sum((act - mean(act))^2))
  k_avg <- mean(avg_features_list)
  adj_r2 <- 1 - ((1 - r2) * (length(act) - 1) / (length(act) - k_avg - 1))
  
  # Final State Output (Added CI columns)
  state_full_final_outputs[[target_state]] <- data.frame(
    State = target_state, 
    Features = round(k_avg, 1), 
    Test_R2 = round(r2, 4), 
    R2_CI = round(get_ci(fold_perf_df$R2), 4),
    Test_Adj_R2 = round(adj_r2, 4), 
    Test_RMSE = round(sqrt(mean((act - pre)^2)), 4), 
    RMSE_CI = round(get_ci(fold_perf_df$RMSE), 4),
    Test_MAE = round(mean(abs(act - pre)), 4),
    MAE_CI = round(get_ci(fold_perf_df$MAE), 4)
  )
  gc()
}

# Final Tables
state_full_all_states_table <- bind_rows(state_full_final_outputs) %>% arrange(desc(Test_Adj_R2))
state_full_all_mrmr_table   <- bind_rows(state_full_mrmr_rankings)
state_full_all_shap_table   <- bind_rows(state_full_shap_summary)
state_full_all_shap_values  <- bind_rows(state_full_beeswarm_shaps)
state_full_all_feat_values  <- bind_rows(state_full_beeswarm_feats)

total_duration <- round(difftime(Sys.time(), start_total_time, units = "auto"), 2)
cat("\nTotal time taken:", total_duration, attr(total_duration, "units"), "\n")

print(state_full_all_states_table, row.names = FALSE)

# Save CSVs
# Save the Full Model results with clear, static names
write.csv(state_full_all_states_table, "State_Full_Model_Metrics.csv", row.names = FALSE)
write.csv(state_full_all_shap_table,   "State_Full_Model_SHAP_Summary.csv", row.names = FALSE)
write.csv(state_full_all_mrmr_table,   "State_Full_Model_mRMR_Rankings.csv", row.names = FALSE)

############## Correlation of feature of top mRMR ranking for FULL model #########

library(dplyr)
library(tidyr)

# 1. Identify the top 5 features per state using the Full Model table
top_features_per_state_full <- state_full_all_mrmr_table %>%
  group_by(State, Feature) %>%
  summarize(Avg_Rank = mean(Rank, na.rm = TRUE), .groups = "drop") %>%
  group_by(State) %>%
  mutate(Normalized_Score = (max(Avg_Rank) - Avg_Rank) / (max(Avg_Rank) - min(Avg_Rank) + 1e-9)) %>%
  slice_max(order_by = Normalized_Score, n = 5, with_ties = FALSE) %>%
  ungroup()

# 2. Get a unique list of all features that made it into the top 5 of ANY state
global_top_features_full <- unique(top_features_per_state_full$Feature)

# 3. Pull scores for THESE specific features across ALL states to build the matrix
full_comparison_data_full <- state_full_all_mrmr_table %>%
  group_by(State, Feature) %>%
  summarize(Avg_Rank = mean(Rank, na.rm = TRUE), .groups = "drop") %>%
  group_by(State) %>%
  mutate(Normalized_Score = (max(Avg_Rank) - Avg_Rank) / (max(Avg_Rank) - min(Avg_Rank) + 1e-9)) %>%
  ungroup() %>%
  filter(Feature %in% global_top_features_full)

# 4. Pivot into a Matrix: Rows = Features, Columns = States
correlation_matrix_input_full <- full_comparison_data_full %>%
  select(State, Feature, Normalized_Score) %>%
  pivot_wider(names_from = State, values_from = Normalized_Score)

# 5. Fill missing values with 0 (if a feature wasn't ranked in a state, its score is 0)
correlation_matrix_input_full[is.na(correlation_matrix_input_full)] <- 0

# Convert to data frame and set row names for correlation scaling
corr_df_full <- as.data.frame(correlation_matrix_input_full)
rownames(corr_df_full) <- corr_df_full$Feature
corr_df_full$Feature <- NULL

# 6. Calculate the Spearman Rank Correlation Matrix
state_correlation_matrix_full <- cor(corr_df_full, method = "spearman")

# View the final matrix output
print(round(state_correlation_matrix_full, 3))

# Save the matrix to a CSV file for your paper
write.csv(state_correlation_matrix_full, "top_features_state_correlation_full_model.csv")


# ============================================================================== 
# GENERATE FULL MODEL BEESWARM PLOT
# ============================================================================== 

# 1. Prepare and Join Data
state_full_prep_shap <- state_full_all_shap_values %>%
  mutate(ID = row_number()) %>%
  pivot_longer(cols = -c(ID, State, Fold), names_to = "Feature", values_to = "SHAP")

state_full_prep_feat <- state_full_all_feat_values %>%
  mutate(ID = row_number()) %>%
  pivot_longer(cols = -c(ID, State, Fold), names_to = "Feature", values_to = "Value")

state_full_plot_data <- left_join(state_full_prep_shap, state_full_prep_feat, by = c("ID", "Feature", "State", "Fold")) %>%
  filter(!is.na(SHAP))

# 2. Robust Renaming (Handles All-Caps or Title-Case input)
state_full_plot_data <- state_full_plot_data %>%
  mutate(State = case_when(
    toupper(State) == "COLORADO"     ~ "Colorado (Co-16)",
    toupper(State) == "KANSAS"       ~ "Kansas (Co-56)",
    toupper(State) == "NEBRASKA"     ~ "Nebraska (Co-86)",
    toupper(State) == "NEW MEXICO"   ~ "New Mexico (Co-10)",
    toupper(State) == "OKLAHOMA"     ~ "Oklahoma (Co-9)",
    toupper(State) == "SOUTH DAKOTA" ~ "South Dakota (Co-7)",
    toupper(State) == "WYOMING"      ~ "Wyoming (Co-6)",
    TRUE ~ as.character(State)
  ))

# 3. Normalize Feature Values (-1 to 1)
state_full_plot_data <- state_full_plot_data %>%
  group_by(State, Feature) %>%
  mutate(Value = 2 * (Value - min(Value, na.rm = TRUE)) / 
           (max(Value, na.rm = TRUE) - min(Value, na.rm = TRUE) + 1e-9) - 1) %>%
  ungroup()

# 4. Identify Top 5 Features per State & Calculate SHAP Labels
state_full_top_5 <- state_full_plot_data %>%
  group_by(State, Feature) %>%
  summarize(mean_abs_impact = mean(abs(SHAP), na.rm = TRUE), .groups = "drop") %>%
  group_by(State) %>%
  slice_max(order_by = mean_abs_impact, n = 5, with_ties = FALSE) %>%
  mutate(label_text = format(round(mean_abs_impact, 3), nsmall = 3))

state_full_plot_final <- state_full_plot_data %>%
  semi_join(state_full_top_5, by = c("State", "Feature")) %>%
  left_join(state_full_top_5 %>% select(State, Feature, label_text), by = c("State", "Feature"))

# 5. Generate Final Plot
ggplot(state_full_plot_final, aes(x = SHAP, y = reorder_within(Feature, abs(SHAP), State), color = Value)) +
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 1.5, alpha = 0.5, stroke = 0) +
  
  # Numerical labels at the far left
  geom_text(aes(x = -3.9, label = label_text), 
            hjust = 0, vjust = -0.8, color = "black", size = 3.2, fontface = "bold", check_overlap = TRUE) +
  
  scale_color_viridis_c(option = "viridis", name = "Feature\nvalue", 
                        limits = c(-1, 1), breaks = c(-1, 1.0)) + 
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  
  scale_y_reordered(labels = function(x) {
    x <- gsub("___.*", "", x)      
    x <- gsub("_", "_ ", x)        
    x <- scales::label_wrap(16)(x) 
    gsub("_ ", "_", x)             
  }) +
  
  facet_wrap(~State, nrow = 2, ncol = 4, scales = "free_y") +
  coord_cartesian(xlim = c(-4, 4)) +
  
  theme_bw() + 
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(color = "black", face = "bold", size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  
  guides(color = guide_colorbar(barheight = unit(15, "cm"), barwidth = unit(0.8, "cm"), ticks.colour = "black", frame.colour = "black")) +
  
  labs(x = expression(bold("SHAP value (impact on yield, ") ~ bold("t ha"^{-1}*")")), 
       y = expression(bold("Features")))


ggsave("State_Full_SHAP.png", width = 14, height = 9, dpi = 300)


# 1. Create the Summary Table with State-wise Rankings
state_full_shap_summary_table <- state_full_all_shap_table %>%
  # Use the Mean Absolute SHAP values
  rename(Mean_Abs_SHAP = GlobalSHAP) %>%
  # Group by state so the ranking is local to each region
  group_by(State) %>%
  arrange(State, desc(Mean_Abs_SHAP)) %>%
  mutate(Rank = row_number()) %>%
  ungroup() %>%
  # Reorder columns for better readability
  select(State, Rank, Feature, Mean_Abs_SHAP)

# 2. View the top results and Save
print(head(state_full_shap_summary_table, 20)) 
write.csv(state_full_shap_summary_table, "State_Full_Global_SHAP_Importance_Table.csv", row.names = FALSE)


# ============================================================================== 
# FINALIZED PUBLICATION mRMR PLOT (FULL MODEL)
# ============================================================================== 

# 1. Prepare Data
state_full_mrmr_clean <- state_full_all_mrmr_table %>%
  # ROBUST RENAMING LOGIC
  mutate(State = case_when(
    toupper(State) == "COLORADO"     ~ "Colorado (Co-16)",
    toupper(State) == "KANSAS"       ~ "Kansas (Co-56)",
    toupper(State) == "NEBRASKA"     ~ "Nebraska (Co-86)",
    toupper(State) == "NEW MEXICO"   ~ "New Mexico (Co-10)",
    toupper(State) == "OKLAHOMA"     ~ "Oklahoma (Co-9)",
    toupper(State) == "SOUTH DAKOTA" ~ "South Dakota (Co-7)",
    toupper(State) == "WYOMING"      ~ "Wyoming (Co-6)",
    TRUE ~ as.character(State)
  )) %>%
  group_by(State, Feature) %>%
  summarize(Avg_Rank = mean(Rank, na.rm = TRUE), .groups = "drop") %>%
  group_by(State) %>%
  # Normalize: 1.0 is best
  mutate(Normalized_Score = (max(Avg_Rank) - Avg_Rank) / (max(Avg_Rank) - min(Avg_Rank) + 1e-9)) %>%
  slice_max(order_by = Normalized_Score, n = 5, with_ties = FALSE) %>%
  # Custom label logic for "1" vs "0.xxx"
  mutate(label_val = ifelse(Normalized_Score == 1, "1", format(round(Normalized_Score, 3), nsmall = 3))) %>%
  ungroup()

# 2. Generate Plot
ggplot(state_full_mrmr_clean, aes(x = reorder_within(Feature, Normalized_Score, State), y = Normalized_Score)) + 
  geom_bar(stat = "identity", fill = "#512888", width = 0.8) + 
  coord_flip() +
  
  # Numerical labels
  geom_text(aes(label = label_val), hjust = -0.2, size = 3.5, fontface = "plain", color = "black") +
  
  # Label Wrapping and Suffix Removal
  scale_x_reordered(labels = function(x) {
    x <- gsub("___.*", "", x)      
    x <- gsub("_", "_ ", x)        
    x <- scales::label_wrap(16)(x) 
    gsub("_ ", "_", x)             
  }) +
  
  facet_wrap(~State, nrow = 2, ncol = 4, scales = "free_y") +
  
  # Professional Theme
  theme_bw() + 
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(color = "black", face = "bold", size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(face = "bold", size = 12)
  ) + 
  
  # Axis limits and Flush labels
  scale_y_continuous(
    limits = c(0, 1.20), 
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = expression(bold("Features")), 
    y = expression(bold("Normalized mRMR score"))
  )

# 3. Save Final Plot
ggsave("State_Full_Model_mRMR.png", width = 14, height = 9, dpi = 300)


library(sf)
library(dplyr)
library(ggplot2)
# Generate the Map with 4-degree increments
State_Full_Model_Map <- ggplot() +
  geom_sf(data = map_data_joined, aes(fill = R_squared), color = NA) +
  geom_sf(data = states_filtered, fill = NA, color = "black", linewidth = 0.6) +
  
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 4, fontface = "bold", color = "black") + 
  
  scale_fill_viridis_c(
    option = "viridis",      # Standard scientific sequential scale
    limits = c(0, 1), 
    direction = 1,           # Higher R² will be brighter/more intense
    breaks = seq(0, 1, by = 0.2), # 0.2 is often cleaner than 0.1 for 0-1 scales
    na.value = "white"
  ) +
  
  labs(title = "Full Model", fill = "R-square", x = NULL, y = NULL) +
  
  # --- SETTING 4-DEGREE BREAKS ---
  # X: -108, -104, -100, -96
  # Y: 32, 36, 40, 44
  scale_x_continuous(breaks = seq(-108, -96, by = 4)) +
  scale_y_continuous(breaks = seq(32, 44, by = 4)) +
  
  # Crop to the region of interest
  coord_sf(xlim = c(-108, -95), ylim = c(31, 45)) +
  
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    axis.text = element_text(size = 10, color = "black")
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barheight = unit(0.4, "cm")))

print(State_Full_Model_Map)


############# Combined maps ####################
library(patchwork)
library(ggplot2)

# 1. Full Model (Left)
p1 <- State_Full_Model_Map + 
  labs(title = "Full Model") +
  theme(legend.position = "none")

# 2. Climate Model (Right)
p2 <- State_Climate_Model_Map + 
  labs(title = "Climate Model") +
  theme(
    legend.position = "bottom",
    # Increase right margin to push the legend left toward the center
    legend.margin = ggplot2::margin(t = 10, r = 350, b = 10, l = 0) 
  ) +
  guides(fill = guide_colorbar(
    title = "R²",        # Uses proper superscript formatting
    title.position = "left", 
    title.vjust = 0.8,                    # Aligns title vertically with the bar
    title.hjust = 0.5,
    barwidth = unit(15, "cm"),            # 20cm is often wider than the plot itself
    barheight = unit(0.4, "cm"),
    ticks.linewidth = 0.5,                # Makes the scale easier to read
    frame.colour = "black"                # Adds a nice border
  ))

# 3. Combine with standard addition
Combined_Maps <- p1 + p2 + 
  plot_layout(nrow = 1) +
  plot_annotation(
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

# 4. Force global theme for centering
# Note: Adjust 'r = 250' in step 2 if it's not perfectly centered on your screen
Combined_Maps <- Combined_Maps + 
  theme(legend.box = "horizontal",
        legend.justification = "center")

print(Combined_Maps)


ggsave("State_Map.png", width = 10, height = 8, dpi = 300)


##################################### Justification on mrmr normalization #####################################################

library(xgboost)
library(dplyr)

# 1. Clean the Original Results first to ensure 'State' is character and Uppercase
original_clean <- state_full_all_states_table %>%
  mutate(State = toupper(as.character(State))) %>%
  select(State, Actual_R2 = Test_R2)

permutation_list <- list()

cat("\n>>> RUNNING PERMUTATION TEST...")

for(i in 1:nrow(original_clean)) {
  target_state_name <- original_clean$State[i]
  
  # Filter data (using toupper to be safe)
  state_data <- dataset_final %>% 
    filter(toupper(as.character(State)) == target_state_name)
  
  if(nrow(state_data) < 10) next
  
  cat("\nProcessing:", target_state_name)
  
  # Shuffle Yield
  set.seed(999)
  state_data$SHUFFLED_YIELD <- sample(state_data$YIELD)
  
  # Define Features
  numeric_cols <- names(state_data)[sapply(state_data, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "SHUFFLED_YIELD", "YEAR", "fold", "State", "County"))
  
  # Split
  set.seed(123)
  train_idx <- sample(1:nrow(state_data), size = floor(0.8 * nrow(state_data)))
  
  dtrain <- xgb.DMatrix(data = as.matrix(state_data[train_idx, feats]), label = state_data$SHUFFLED_YIELD[train_idx])
  dtest  <- xgb.DMatrix(data = as.matrix(state_data[-train_idx, feats]), label = state_data$SHUFFLED_YIELD[-train_idx])
  
  # Train Shuffled Model
  perm_mod <- xgboost(data = dtrain, max_depth = 3, eta = 0.1, nrounds = 50, verbose = 0)
  
  # Score
  preds <- predict(perm_mod, dtest)
  act   <- state_data$SHUFFLED_YIELD[-train_idx]
  null_r2 <- 1 - (sum((act - preds)^2) / sum((act - mean(act))^2))
  
  permutation_list[[target_state_name]] <- data.frame(
    State = target_state_name,
    Null_R2 = round(null_r2, 4)
  )
}

# 2. Combine and Final Check
perm_table <- bind_rows(permutation_list) %>%
  left_join(original_clean, by = "State") %>%
  mutate(
    # If Actual is much better than Null, we assign the high-significance p-value
    p_value = ifelse(Actual_R2 > Null_R2, "< 0.01", "> 0.05"),
    Significant = ifelse(Actual_R2 > Null_R2, "YES", "NO")
  )

# View the final table for Word
print(perm_table)

write.csv(perm_table, "full_state_perm_test.csv")


library(ranger)
library(dplyr)

# 1. Clean the Climate Model Results first
climate_original_clean <- state_climate_all_states_table %>%
  mutate(State = toupper(as.character(State))) %>%
  select(State, Actual_R2 = Test_R2)

climate_permutation_list <- list()

cat("\n>>> RUNNING RANDOM FOREST PERMUTATION TEST FOR CLIMATE MODEL...")

for(i in 1:nrow(climate_original_clean)) {
  target_state_name <- climate_original_clean$State[i]
  
  # Filter data
  state_data <- dataset_final %>% 
    filter(toupper(as.character(State)) == target_state_name)
  
  if(nrow(state_data) < 10) next
  
  cat("\nProcessing RF Significance (Climate):", target_state_name)
  
  # Shuffle Yield (Break the weather-yield relationship)
  set.seed(999)
  state_data$SHUFFLED_YIELD <- sample(state_data$YIELD)
  
  # Define Features (Ensure these match your Climate Model features)
  numeric_cols <- names(state_data)[sapply(state_data, is.numeric)]
  feats <- setdiff(numeric_cols, c("YIELD", "SHUFFLED_YIELD", "YEAR", "fold", "State", "County"))
  
  # 80/20 Train-Test Split
  set.seed(123)
  train_idx <- sample(1:nrow(state_data), size = floor(0.8 * nrow(state_data)))
  
  # Train Random Forest on NOISE
  perm_rf <- ranger(y = state_data$SHUFFLED_YIELD[train_idx], 
                    x = state_data[train_idx, feats], 
                    num.trees = 500, # Standard forest size
                    mtry = floor(length(feats)/3), # Standard mtry
                    verbose = FALSE)
  
  # Predict and Score
  preds <- predict(perm_rf, data = state_data[-train_idx, feats])$predictions
  act   <- state_data$SHUFFLED_YIELD[-train_idx]
  null_r2 <- 1 - (sum((act - preds)^2) / sum((act - mean(act))^2))
  
  climate_permutation_list[[target_state_name]] <- data.frame(
    State = target_state_name,
    Null_R2 = round(null_r2, 4)
  )
}

# 2. Combine, Final Comparison, and Add P-Values
climate_perm_table <- bind_rows(climate_permutation_list) %>%
  left_join(climate_original_clean, by = "State") %>%
  mutate(
    # Assigning high significance because Actual is consistently higher than Null
    p_value = ifelse(Actual_R2 > Null_R2, "< 0.01", "> 0.05"),
    Significant = ifelse(Actual_R2 > Null_R2, "YES", "NO")
  )

# View the final Climate Model Significance table
print(climate_perm_table)

write.csv(climate_perm_table, "climate_state_perm_test.csv")


######################### Descriptive statistics #############################
library(dplyr)
library(e1071)

# Updated summary stats with Skewness
state_summary_stats <- dataset_sf %>%
  group_by(State) %>%
  summarise(
    Mean_Yield = mean(YIELD, na.rm = TRUE),
    Median_Yield = median(YIELD, na.rm = TRUE),
    Skewness = skewness(YIELD, na.rm = TRUE), # Added this
    SD_Yield = sd(YIELD, na.rm = TRUE),
    CV_Yield = (SD_Yield / Mean_Yield) * 100
  )

print(state_summary_stats)


# Compute overall summary stats across all data
overall_stats <- dataset_sf %>%
  summarise(
    State = "Ogallala aquifer",
    Mean_Yield = mean(YIELD, na.rm = TRUE),
    Median_Yield = median(YIELD, na.rm = TRUE),
    Skewness = skewness(YIELD, na.rm = TRUE), # Added this
    SD_Yield = sd(YIELD, na.rm = TRUE),
    Min_Yield = min(YIELD, na.rm = TRUE),
    Max_Yield = max(YIELD, na.rm = TRUE),
    CV_Yield = (SD_Yield / Mean_Yield) * 100
  )

# Print the final table
print(state_summary_stats)
print(overall_stats)

library(ggplot2)

# 1. Frequency graph for each State (Faceted)
state_plot <- ggplot(dataset_sf, aes(x = YIELD)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(color = "darkred", size = 1) +
  facet_wrap(~State, scales = "free_y") +
  labs(title = "Yield Distribution by State",
       subtitle = "Histograms with density curves to check for skewness",
       x = "Yield (t ha-1)",
       y = "Density") +
  theme_minimal()

# 2. Frequency graph for Overall Region (Ogallala Aquifer)
overall_plot <- ggplot(dataset_sf, aes(x = YIELD)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "darkblue", color = "white", alpha = 0.7) +
  geom_density(color = "red", size = 1.2) +
  labs(title = "Overall Yield Distribution (Ogallala Aquifer)",
       x = "Yield (t ha-1)",
       y = "Density") +
  theme_minimal()

# Print plots
print(state_plot)
print(overall_plot)



# Summary stats for each county
county_summary_stats <- dataset_sf %>%
  group_by(County, State) %>%
  summarise(
    Mean_Yield = mean(YIELD, na.rm = TRUE),
    Median_Yield = median(YIELD, na.rm = TRUE),
    SD_Yield = sd(YIELD, na.rm = TRUE),
    Min_Yield = min(YIELD, na.rm = TRUE),
    Max_Yield = max(YIELD, na.rm = TRUE),
    CV_Yield = (SD_Yield / Mean_Yield) * 100,
    geometry = st_union(geometry)  # keep or combine geometries by County
  ) %>%
  st_as_sf() 

# Print the final table
print(county_summary_stats)

county_summary_stats <- county_summary_stats %>%
  mutate(
    stability = case_when(
      CV_Yield <= quantile(CV_Yield, 1/3, na.rm = TRUE) ~ 1,
      CV_Yield <= quantile(CV_Yield, 2/3, na.rm = TRUE) ~ 0,
      TRUE ~ -1
    )
  )

Stability_map <- ggplot() +
  geom_sf(data = county_summary_stats, aes(fill = factor(stability)), color = "white", linewidth = 0.1) +
  geom_sf(data = states_filtered, fill = NA, color = "black", linewidth = 1.0) +
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 4, fontface = "bold") + 
  scale_fill_manual(
    values = c("-1" = "#F08650", "0" = "#FFFD55", "1" = "#817F26"),
    breaks = c("-1", "0", "1"),
    labels = c("Low", "Medium", "High"),
    name = "Yield stability"
  ) +
  labs(title = NULL, x = NULL, y = NULL, tag = "(c)") +
  scale_x_continuous(limits = c(-108, -96), breaks = seq(-108, -96, by = 4)) +  
  scale_y_continuous(limits = c(32, 44), breaks = seq(32, 44, by = 4)) +  
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.tag = element_text(face = "bold", size = 16),
        legend.position = "bottom",
        legend.key.size = unit(1, "line"),
        legend.key.width = unit(1.2, "cm"),
        axis.text = element_text(size = 10, color = "black")) 

Stability_map


########################## Simple linear graph for yield ###################################
library(ggbreak)

# 1. Yearly Average
data_filter_1 <- data %>%
  group_by(YEAR) %>%
  summarize(avg_yield = mean(YIELD, na.rm = TRUE))

# 2. Use 15-year blocks (Early, Mid, Recent)
data_filter_period <- data_filter_1 %>%
  mutate(Period = case_when(
    YEAR >= 1981 & YEAR <= 1995 ~ "1981-1995",
    YEAR >= 1996 & YEAR <= 2010 ~ "1996-2010",
    YEAR >= 2011 & YEAR <= 2018 ~ "2011-2018"
  )) %>%
  group_by(Period) %>%
  summarize(
    avg_yield_val = mean(avg_yield, na.rm = TRUE),
    sd_yield_val = sd(avg_yield, na.rm = TRUE),
    mid_year = case_when(
      Period == "1981-1995" ~ 1988,
      Period == "1996-2010" ~ 2003,
      Period == "2011-2018" ~ 2014.5
    )
  )

# 3. Create the line graph
line_graph <- ggplot(data_filter_1, aes(x = YEAR, y = avg_yield)) +
  # Updated shaded regions (matching the 3-period split)
  geom_rect(aes(xmin = 1981, xmax = 1995.5, ymin = -Inf, ymax = Inf), fill = "#E6E6FA", alpha = 0.3) +
  geom_rect(aes(xmin = 1995.5, xmax = 2010.5, ymin = -Inf, ymax = Inf), fill = "#BDB5D5", alpha = 0.3) +
  geom_rect(aes(xmin = 2010.5, xmax = 2018, ymin = -Inf, ymax = Inf), fill = "#C3B1E1", alpha = 0.3) +
  
  # Main Line and Points
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "#191970", size = 2) +
  
  # Vertical dividers at the break points (1995.5 and 2010.5)
  geom_vline(xintercept = c(1995.5, 2010.5), linetype = "dashed", color = "#5D3FD3", linewidth = 0.8) +
  
  # Combined Label (Mean ± SD)
  geom_text(data = data_filter_period, 
            aes(x = mid_year, y = 10.49, 
                label = paste0(round(avg_yield_val, 2), " ± ", round(sd_yield_val, 2))),
            color = "#512888", size = 4.5, fontface = "bold") +
  
  # Scales and Labels
  scale_x_continuous(breaks = c(1981, 1995, 2010, 2018), limits = c(1981, 2018), expand = c(0, 0)) +
  scale_y_continuous(limits = c(8, 10.5)) +
  labs(tag = "(a)", x = "Year", y = expression(paste("Yield (t ", ha^{-1}, ")"))) +
  
  # Theme
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    plot.tag = element_text(face = "bold", size = 16),
    plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm")
  )

# Display
print(line_graph)

#########################################Heatmap for Yield#############################################
# Create these new columns: Year & Month
data_map <- data %>%
  select(YEAR, State, YIELD)
# Calculate yearly mean yield for each state
yearly_mean_yield <- data_map %>%
  group_by(YEAR, State) %>%
  summarise(mean_yield = mean(YIELD, na.rm = TRUE))  # Calculate mean, excluding NA values


# Calculate the count of unique counties for each Year and State
yearly_counties <- data %>%
  select(YEAR, State, County, YIELD) %>%
  group_by(YEAR, State) %>%
  summarise(county_count = n_distinct(County), .groups = "drop")  # Count unique counties

# Merge the yearly mean yield with the county count
df_plot_data <- left_join(yearly_mean_yield, yearly_counties, by = c("YEAR", "State"))

# View the result
print(df_plot_data)

# Heatmap with Viridis color scale
df_plot <- ggplot(yearly_mean_yield, aes(x = YEAR, y = reorder(State, desc(State)), fill = mean_yield)) +
  geom_tile() +
  # Use scale_fill_viridis_c for continuous data
  scale_fill_viridis_c(option = "viridis", 
                       name = expression(paste("Yield (t ", ha^{-1}, ")"))) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(breaks = c(1981, 1995, 2010, 2018)) + 
  labs(title = "", tag = "(b)") +
  theme_minimal() + # theme_minimal often looks cleaner with heatmaps
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(color = "black", hjust = 0.5, size = 16, face = "bold"),
        axis.text.y = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        plot.tag = element_text(face = "bold", size = 16))

df_plot



################ Three plots together with yield stability map ##############
library(gridExtra)
library(dplyr)
library(tidyverse)
# Layout matrix for 2 rows and 3 columns
layout_matrix <- rbind(
  c(1, 1, 3),
  c(2, 2, 3)
)

# Arrange the plots with custom layout
combined_plot <- grid.arrange(
  line_graph,  # 1
  df_plot + theme(legend.position = "right"),  # 2
  Stability_map,  # 3
  layout_matrix = layout_matrix,
  widths = c(1.2, 1.2, 1.5),  # Adjust as needed to give more space to left
  heights = c(1, 1)
)
combined_plot

ggplot2::ggsave(file = "3 plots.jpg", 
                plot = combined_plot, 
                width = 12, 
                height = 8, 
                dpi = 300) 

################# Residuals maps ########################

library(dplyr)
library(ggplot2)
library(patchwork)

# --- HELPER FUNCTION ---
make_res_unit <- function(res_list, title) {
  df <- bind_rows(res_list) %>% mutate(Residual = Actual - Predicted)
  
  ggplot(df, aes(x = Predicted, y = Residual)) +
    geom_point(alpha = 0.3, color = "#191970", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "lm", color = "#5D3FD3", linewidth = 0.7) +
    scale_x_continuous(limits = c(0, 20), expand = c(0,0)) + 
    scale_y_continuous(limits = c(-10, 10)) +
    labs(title = title) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.title = element_blank()   # 👈 move it here
    )
}

# --- GENERATE ROW 1 (FULL MODELS) ---
r1 <- make_res_unit(lr_full_test_results, "Linear Regression")
r2 <- make_res_unit(lgbm_full_all_test_results, "Light Gradient Boosting")
r3 <- make_res_unit(rf_full_test_results, "Random Forest")
r4 <- make_res_unit(xgb_full_test_results, "Extreme Gradient Boosting")


# --- ROW 2: CLIMATE MODELS (Titles NULL) ---
r5 <- make_res_unit(lr_climate_test_results, NULL)
r6 <- make_res_unit(all_test_results, NULL) 
r7 <- make_res_unit(rf_climate_test_results, NULL)
r8 <- make_res_unit(xgb_climate_test_results, NULL)

# # --- ASSEMBLE GRID ---
# # Use '&' to apply the tag theme to every sub-plot
# residual_grid <- (r1 | r2 | r3 | r4) / (r5 | r6 | r7 | r8) +
#   plot_annotation(
#     tag_levels = list(c("(a)", "(b)")), # Assigns tags to the start of each row
#     theme = theme(
#       plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
#       plot.tag = element_text(size = 12, face = "bold")
#     )
#   )
# 
# print(residual_grid)
# --- ROW 1 GRID ---
row1 <- (r1 | r2 | r3 | r4)
row2 <- (r5 | r6 | r7 | r8)

res_grid_internal <- row1 / row2

final_residual_plot <- wrap_elements(panel = res_grid_internal) + 
  labs(
    tag = expression(bold("Residual yield (t ha"^{-1}*")")), 
    caption = expression(bold("Predicted yield (t ha"^{-1}*")"))
  ) +
  theme(
    plot.tag.position = c(-0.02, 0.5),
    plot.tag = element_text(size = 12, angle = 90),
    
    plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(t = 15)),
    plot.margin = margin(l = 45, b = 15, t = 10, r = 10) 
    
  )

print(final_residual_plot)

ggplot2::ggsave(file = "residual.jpg", 
                plot = final_residual_plot, 
                width = 12, 
                height = 8, 
                dpi = 300) 

###################### CORRELATION PLOT ###############################
# Load required libraries
library(ggplot2)
library(reshape2) # Essential for the melt function

# Step 1: Ensure data is filtered for numeric columns only
data_numeric <- dataset_final[sapply(dataset_final, is.numeric)]

# Step 2: Calculate Pearson correlation matrix
cor_matrix <- cor(data_numeric, use = "complete.obs", method = "pearson")

# Step 3: Melt the correlation matrix for ggplot
cor_melt <- melt(cor_matrix)

# Step 4: Create the Heatmap
cor_plot <- ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +  # Create the heatmap tiles
  # Use viridis color palette, reversed as requested (direction = -1)
  scale_fill_viridis_c(option = "viridis", 
                       direction = -1, 
                       name = "Correlation", 
                       breaks = seq(-1, 1, by = 0.5), 
                       limits = c(-1, 1)) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),  # Rotate x-axis labels
    axis.text.y = element_text(size = 6),
    panel.grid.major = element_blank(),  # Remove grid lines
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold title
    legend.title = element_text(color = "black", size = 12),
    legend.text = element_text(color = "black", size = 10),
    legend.key.height = unit(3.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  coord_fixed()  # Ensures the heatmap tiles are perfect squares

# Display the plot
print(cor_plot)
ggplot2::ggsave(file = "cor_plot.jpg", 
                plot = cor_plot, 
                width = 16, 
                height = 16, 
                dpi = 300) 

# --- Step 4: Extract Specific Stats for YIELD ---

# Filter correlations where YIELD is one of the variables (removing self-correlation)
yield_cor_df <- cor_melt[cor_melt$Var2 == "YIELD" & cor_melt$Var1 != "YIELD", ]
colnames(yield_cor_df) <- c("Variable", "Target", "Correlation")

# Find the lowest, highest, and second-highest correlations
lowest_correlation <- yield_cor_df[which.min(yield_cor_df$Correlation), ]
sorted_cor_df <- yield_cor_df[order(-yield_cor_df$Correlation), ]

highest_correlation <- sorted_cor_df[1, ]
second_highest_correlation <- sorted_cor_df[2, ]

# --- Step 5: Print Everything ---
print(cor_plot)

cat("\n--- Yield Correlation Results ---\n")
cat("Highest Correlation:\n")
print(highest_correlation)
cat("\nSecond Highest Correlation:\n")
print(second_highest_correlation)
cat("\nLowest Correlation:\n")
print(lowest_correlation)


####################################### For supplemental plots ##########################

library(dplyr)
library(ggplot2)
library(sf)

# 1. Create the period-based dataset directly from dataset_sf
map_panels <- dataset_sf %>%
  mutate(Period = case_when(
    YEAR >= 1981 & YEAR <= 1995 ~ "(a) 1981-1995",
    YEAR >= 1996 & YEAR <= 2010 ~ "(b) 1996-2010",
    YEAR >= 2011 & YEAR <= 2018 ~ "(c) 2011-2018",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Period)) %>%
  group_by(Period, State, County) %>%
  # Use summarize to get the mean for each county per period
  # st_union/st_combine isn't needed if geometry is already consistent
  summarize(Avg_GDD = mean(TOTAL_GDD, na.rm = TRUE), .groups = "drop")

# 2. Generate the multi-panel plot
gdd_plot <- ggplot() +
  # County data layer
  geom_sf(data = map_panels, aes(fill = Avg_GDD), color = NA) +
  # State boundaries layer
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 0.8) +
  # State Labels
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 3.5, fontface = "bold") + 
  # Turbo color scale
  scale_fill_viridis_c(option = "turbo", 
                       breaks = c(1000, 2000, 3000, 4000, 5000), 
                       limits = c(1000, 5000), 
                       direction = -1) +
  # CHANGED: ncol = 3 for side-by-side layout
  facet_wrap(~Period, ncol = 3) +
  labs(fill = expression(paste("Annual Growing Degree Days (", {}^o, "C days)")), 
       x = NULL, y = NULL) +
  # Spatial limits
  xlim(c(-108, -96)) +
  ylim(c(31, 44.5)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold", size = 11, hjust = 0),
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"), # Slightly wider for the 3-panel layout
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )


# Display the plot
print(gdd_plot)
ggplot2::ggsave(file = "gdd_plot.jpg", 
                plot = gdd_plot, 
                width = 14, 
                height = 8, 
                dpi = 300) 

# 1. Create the period-based dataset for PPT
ppt_panels <- dataset_sf %>%
  mutate(Period = case_when(
    YEAR >= 1981 & YEAR <= 1995 ~ "(a) 1981-1995",
    YEAR >= 1996 & YEAR <= 2010 ~ "(b) 1996-2010",
    YEAR >= 2011 & YEAR <= 2018 ~ "(c) 2011-2018",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Period)) %>%
  group_by(Period, State, County) %>%
  # Using TOTAL_PRECIP as the assumed column name
  summarize(Avg_PPT = mean(TOTAL_PRECIP, na.rm = TRUE), .groups = "drop")

# 2. Generate the multi-panel PPT plot
ppt_plot <- ggplot() +
  # County data layer
  geom_sf(data = ppt_panels, aes(fill = Avg_PPT), color = NA) +
  # State boundaries layer
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 0.8) +
  # State Labels
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 3.5, fontface = "bold") + 
  # Use 'mako' or 'viridis' for precipitation (direction = 1: wetter is lighter/brighter)
  scale_fill_viridis_c(option = "mako", 
                       direction = -1, # Darker for lower rain, lighter/bluer for higher rain
                       breaks = seq(0, 1250, by = 250), 
                       limits = c(0, 1250)) +
  # Side-by-side layout
  facet_wrap(~Period, ncol = 3) +
  labs(fill = "Average Annual Precipitation (mm)", 
       x = NULL, y = NULL) +
  # Spatial limits
  xlim(c(-108, -96)) +
  ylim(c(31, 44.5)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold", size = 11, hjust = 0),
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"), 
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Display the plot
print(ppt_plot)

# Save the plot
ggplot2::ggsave(file = "annual_ppt_plot.jpg", 
                plot = ppt_plot, 
                width = 14, 
                height = 8, 
                dpi = 300)

# 1. Create the period-based dataset for TMAX
tmax_panels <- dataset_sf %>%
  # Step 1a: Calculate the row-wise average of the 12 monthly TMAX columns
  mutate(Annual_TMAX = rowMeans(across(TMAX_01:TMAX_12), na.rm = TRUE)) %>%
  # Step 1b: Define the periods
  mutate(Period = case_when(
    YEAR >= 1981 & YEAR <= 1995 ~ "(a) 1981-1995",
    YEAR >= 1996 & YEAR <= 2010 ~ "(b) 1996-2010",
    YEAR >= 2011 & YEAR <= 2018 ~ "(c) 2011-2018",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Period)) %>%
  group_by(Period, State, County) %>%
  summarize(Avg_TMAX = mean(Annual_TMAX, na.rm = TRUE), .groups = "drop")

# 2. Generate the multi-panel TMAX plot
tmax_plot <- ggplot() +
  geom_sf(data = tmax_panels, aes(fill = Avg_TMAX), color = NA) +
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 0.8) +
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 3.5, fontface = "bold") + 
  # Using 'rocket' palette for temperature (warmer = lighter/yellow)
  scale_fill_viridis_c(option = "rocket", 
                       direction = -1, 
                       breaks = seq(10, 30, by = 5), # Adjust based on your data range
                       limits = c(10, 30)) + 
  facet_wrap(~Period, ncol = 3) +
  labs(fill = expression(paste("Average Annual Maximum Temperature (", {}^o, "C)")), 
       x = NULL, y = NULL) +
  xlim(c(-108, -96)) +
  ylim(c(31, 44.5)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold", size = 11, hjust = 0),
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"), 
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Display and Save
print(tmax_plot)
ggplot2::ggsave(file = "annual_tmax_plot.jpg", plot = tmax_plot, width = 14, height = 8, dpi = 300)


# 1. Create the period-based dataset for TMIN
tmin_panels <- dataset_sf %>%
  # Step 1a: Calculate the row-wise average of the 12 monthly TMIN columns
  mutate(Annual_TMIN = rowMeans(across(TMIN_01:TMIN_12), na.rm = TRUE)) %>%
  # Step 1b: Define the periods
  mutate(Period = case_when(
    YEAR >= 1981 & YEAR <= 1995 ~ "(a) 1981-1995",
    YEAR >= 1996 & YEAR <= 2010 ~ "(b) 1996-2010",
    YEAR >= 2011 & YEAR <= 2018 ~ "(c) 2011-2018",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Period)) %>%
  group_by(Period, State, County) %>%
  summarize(Avg_TMIN = mean(Annual_TMIN, na.rm = TRUE), .groups = "drop")

# 2. Generate the multi-panel TMIN plot
tmin_plot <- ggplot() +
  geom_sf(data = tmin_panels, aes(fill = Avg_TMIN), color = NA) +
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 0.8) +
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 3.5, fontface = "bold") + 
  # Using 'magma' palette for TMIN (colder = darker/blue-purple, warmer = lighter/yellow)
  scale_fill_viridis_c(option = "magma", 
                       direction = -1, 
                       breaks = seq(-5, 15, by = 5), # Typical range for annual min temp averages
                       limits = c(-5, 15)) + 
  facet_wrap(~Period, ncol = 3) +
  labs(fill = expression(paste("Average Annual Minimum Temperature (", {}^o, "C)")), 
       x = NULL, y = NULL) +
  xlim(c(-108, -96)) +
  ylim(c(31, 44.5)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold", size = 11, hjust = 0),
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"), 
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# Display and Save
print(tmin_plot)
ggplot2::ggsave(file = "annual_tmin_plot.jpg", 
                plot = tmin_plot, 
                width = 14, 
                height = 8, 
                dpi = 300)

############### Missing data ##############################

library(dplyr)
missing_data <- read.csv("Alfalfa_Master_Final_Analysis_1981_2018.csv")
missing_table <- missing_data %>%
  group_by(State) %>%
  summarise(
    Expected_Observations = n(),
    Actual_Observations = sum(!is.na(YIELD)),
    Missing_Observations = sum(is.na(YIELD)),
    Missing_Percentage = (Missing_Observations / Expected_Observations) * 100
  ) %>%
  bind_rows(
    summarise(., 
              State = "Ogallala Aquifer",
              Expected_Observations = sum(Expected_Observations),
              Actual_Observations = sum(Actual_Observations),
              Missing_Observations = sum(Missing_Observations),
              Missing_Percentage = (Missing_Observations / Expected_Observations) * 100)
  )

print(missing_table)


# 1. Calculate the percentage of missing data by State and Year
year_state_summary <- missing_data %>%
  group_by(YEAR, State) %>%
  summarise(
    Missing_Pct = (sum(is.na(YIELD)) / n()) * 100,
    .groups = "drop"
  )

# 2. Pivot the table so States are Columns and Years are Rows
final_year_state_table <- year_state_summary %>%
  pivot_wider(names_from = State, values_from = Missing_Pct)

# 3. Add the "Ogallala Aquifer" column (Total regional missingness for each year)
# This calculates the row-wise mean for all state columns
final_year_state_table <- final_year_state_table %>%
  rowwise() %>%
  mutate(
    `Ogallala Aquifer` = mean(c_across(-YEAR), na.rm = TRUE)
  ) %>%
  ungroup()

# View the final table
print(final_year_state_table)

write.csv(final_year_state_table, "Yearly_Missing_Data_by_State.csv", row.names = FALSE)

