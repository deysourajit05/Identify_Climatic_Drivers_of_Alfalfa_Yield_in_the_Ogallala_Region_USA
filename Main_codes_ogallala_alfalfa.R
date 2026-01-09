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


############################################# With centeroid for the counties with lat and long ###################################

data <- read.csv("Ogallala_alfalfa.csv")


data$County <- tolower(data$County)
data$County <- stringr::str_to_title(data$County)

data$State <- tolower(data$State)
data$State <- stringr::str_to_title(data$State)

# Replace "Mcpherson" with "McPherson" in the 'County' column of 'data'
data$County <- str_replace(data$County, "Mcpherson", "McPherson")

datas <- as.data.frame(data)

# Load county boundaries as an sf object (cb = TRUE simplifies shapes)
counties_sf <- counties(cb = TRUE)

# Convert it explicitly to an sf object (if needed)
counties_sf <- st_as_sf(counties_sf)

# Step 4: Perform a spatial join to add county boundaries to your data
map_data <- counties_sf %>%
  left_join(data, by = c("STATE_NAME" = "State", "NAME" = "County"))

# Step 5: Filter based on the presence of Yield and remove unnecessary columns
map_data <- map_data %>%
  filter(!is.na(Yield)) %>%
  select(-COUNTYNS, -STATEFP, -COUNTYFP, -AFFGEOID, -GEOID, -NAMELSAD, -STUSPS, -LSAD, -ALAND, -AWATER, 
         -State_ANSI, -AG_District, -Ag_District_Code, -County_ANSI)

# Calculate centroids for each county
map_data_centroids <- map_data %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(longitude = st_coordinates(centroid)[, 1],
         latitude = st_coordinates(centroid)[, 2]) %>%
  select(-geometry, -centroid)  # Remove full geometry and intermediate centroid columns

# Relocate Yield to the last column
map_data_centroids <- map_data_centroids %>%
  relocate(Yield, .after = last_col())


# Convert to a regular data frame to fully remove geometry metadata
# dataset <- as.data.frame(st_drop_geometry(map_data_centroids))
dataset <- map_data_centroids

dataset <- dataset %>% rename(County = NAME, State = STATE_NAME)

dataset <- dataset %>%
  mutate(Part = case_when(
    State %in% c("Colorado", "Wyoming") ~ "West",
    State %in% c("New Mexico", "Oklahoma") ~ "Southwest",
    State %in% c("South Dakota", "Nebraska", "Kansas") ~ "Midwest"
  )) %>%
  select(Part, everything())  # Move 'Part' to the first column

# View dataset with the new column
head(dataset)

dataset$County <- tolower(dataset$County)
dataset$County <- stringr::str_to_title(dataset$County)

dataset$State <- tolower(dataset$State)
dataset$State <- stringr::str_to_title(dataset$State)

dataset_with_centeroids <- as.data.frame(dataset)

# Move the 'Yield' column to the last position in the dataset
dataset_with_centeroids <- dataset_with_centeroids %>%
  select(-Yield, everything(), Yield)

# Save as CSV
# write.csv(dataset, "dataset_with_centroids.csv", row.names = FALSE)
#############################################################################################

######################### Descriptive statistics #############################
data_yield <- dataset_with_centeroids %>%
  select (State, County, Yield, geometry)

library(dplyr)

# Summary stats for each state
summary_stats <- data_yield %>%
  group_by(State) %>%
  summarise(
    Mean_Yield = mean(Yield, na.rm = TRUE),
    Median_Yield = median(Yield, na.rm = TRUE),
    SD_Yield = sd(Yield, na.rm = TRUE),
    Min_Yield = min(Yield, na.rm = TRUE),
    Max_Yield = max(Yield, na.rm = TRUE),
    CV_Yield = (SD_Yield / Mean_Yield) * 100
  )

# Compute overall summary stats across all data
overall_stats <- data_yield %>%
  summarise(
    State = "Ogallala aquifer",
    Mean_Yield = mean(Yield, na.rm = TRUE),
    Median_Yield = median(Yield, na.rm = TRUE),
    SD_Yield = sd(Yield, na.rm = TRUE),
    Min_Yield = min(Yield, na.rm = TRUE),
    Max_Yield = max(Yield, na.rm = TRUE),
    CV_Yield = (SD_Yield / Mean_Yield) * 100
  )

# Bind the overall row to the state-wise summary
summary_stats_final <- bind_rows(summary_stats, overall_stats)

# Print the final table
print(summary_stats_final)


# Summary stats for each county
summary_stats_county <- data_yield %>%
  group_by(County, State) %>%
  summarise(
    Mean_Yield = mean(Yield, na.rm = TRUE),
    Median_Yield = median(Yield, na.rm = TRUE),
    SD_Yield = sd(Yield, na.rm = TRUE),
    Min_Yield = min(Yield, na.rm = TRUE),
    Max_Yield = max(Yield, na.rm = TRUE),
    CV_Yield = (SD_Yield / Mean_Yield) * 100,
    geometry = st_union(geometry)  # keep or combine geometries by County
  ) %>%
  st_as_sf() 

# Print the final table
print(summary_stats_county)

summary_stats_county <- summary_stats_county %>%
  mutate(
    stability = case_when(
      CV_Yield <= quantile(CV_Yield, 1/3, na.rm = TRUE) ~ 1,
      CV_Yield <= quantile(CV_Yield, 2/3, na.rm = TRUE) ~ 0,
      TRUE ~ -1
    )
  )

Stability_map <- ggplot() +
  geom_sf(data = summary_stats_county, aes(fill = factor(stability)), color = "NA") +
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 1.2) +
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
  scale_y_continuous(limits = c(31, 44.5), breaks = seq(32, 44, by = 4)) +  
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1, "line"),
        legend.key.width = unit(1.2, "cm"),
        axis.text = element_text(size = 10, color = "black")) 

Stability_map
# 
# ggplot2::ggsave(file = "stability_map.jpg",
#                 plot = Stability_map,
#                 width = 8,
#                 height = 12,
#                 dpi = 300)

###########################################################################################
data_filter <- dataset_with_centeroids %>%
  select (-Part, -County, -State, -geometry)
  # filter(Year %in% c(1988:2018)) %>%
  # filter(State %in% c("NEW MEXICO")) %>%
  # select(-Year, -State,-State_ANSI, -AG_District, -Ag_District_Code, -County, -County_ANSI)
# %>%
#   filter(Yield >= 2.47)
dataset <- as.data.frame(data_filter)

##############################################################################################
library(corrplot)
library(reshape2)
library(viridis)

# Step 1: Remove zero-variance columns (columns with constant values)
data_filter_clean <- data_filter[, sapply(data_filter, function(x) sd(x, na.rm = TRUE) != 0)]

# Step 2: Calculate Pearson correlation matrix for the cleaned data
cor_matrix <- cor(data_filter_clean, use = "complete.obs", method = "pearson")

# Step 3: Reshape the correlation matrix to long format for ggplot2
cor_melt <- melt(cor_matrix)

# write.csv(cor_melt, "correlation_matrix_long.csv", row.names = FALSE)


# Step 4: Plot the correlation heatmap
cor_plot <- ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +  # Create the heatmap with white tile borders
  scale_fill_viridis_c(option = "viridis", direction =-1, name = "Correlation", breaks = seq(-1, 1, by = 0.5), limits = c(-1, 1)) +  # Use a color palette
  labs(title = "") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),  # Rotate x-axis labels
    axis.text.y = element_text(size = 6),
    panel.grid.major = element_blank(),  # Remove major grid lines
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
    legend.title = element_text(color = "black", size = 14),
    legend.text = element_text(color = "black", size = 12),
    legend.key.height = unit(3, "cm"),
    legend.key.width = unit(0.5, "cm")) +
      coord_fixed()  # Ensures square tiles 

cor_plot

# Step 4: Filter correlations where Yield is one of the variables
yield_cor_df <- cor_melt[cor_melt$Var2 == "Yield" & cor_melt$Var1 != "Yield", ]

# Rename columns for clarity
colnames(yield_cor_df) <- c("Variable", "Yield", "Correlation")

# Step 5: Find the lowest correlation value
lowest_correlation <- yield_cor_df[which.min(yield_cor_df$Correlation), ]

# Sort the dataframe by correlation in descending order
sorted_cor_df <- yield_cor_df[order(-yield_cor_df$Correlation), ]

# Extract the highest and second-highest correlation
highest_correlation <- sorted_cor_df[1, ]
second_highest_correlation <- sorted_cor_df[2, ]

# Print results
print(highest_correlation)
print(second_highest_correlation)
print(lowest_correlation)

ggplot2::ggsave(file = "cor_plot.jpg",
                plot = cor_plot,
                width = 12,
                height = 12,
                dpi = 300)
######################################################################################################

##################################### Pearson's Correlation Coefficient##################################

library(corrplot)

# Step 2: Calculate Pearson correlation matrix for all variables including "Yield"
cor_matrix <- cor(data_filter, use = "complete.obs", method = "pearson")

# Step 3: Extract correlations between predictors and "Yield"
yield_cor <- cor_matrix[, "Yield"]
yield_cor_df <- data.frame(Variable = names(yield_cor), Correlation = yield_cor)

# Step 4: Visualize correlations with "Yield"
ggplot(yield_cor_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "#191970", width = 0.7) +
  coord_flip() +
  labs(title = "Pearson Correlation with Yield",
       x = "Predictor Variables",
       y = "Correlation") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    axis.text.y = element_text(size = 6, color = "black"),  # Reduce y-axis text size (adjust as needed)
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
    axis.line.x = element_line(color = "black"),  # Add black x-axis line
    axis.line.y = element_line(color = "black"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


######################################Regression equation for map###############################################

# Compute regression coefficients, RMSE, and number of data points for each state
regression_data <- data %>%
  group_by(Part) %>%
  summarize(
    intercept = coef(lm(Yield ~ total_ppt_07))[1],
    slope = coef(lm(Yield ~ total_ppt_07))[2],
    n = n(),
    rmse = sqrt(mean(residuals(lm(Yield ~ total_ppt_07))^2))  # Calculate RMSE
  )

# Modify the regression label function to include RMSE
regression_label <- function(df) {
  intercept <- ifelse(df$intercept < 0, paste0("- ", abs(round(df$intercept, 2))), 
                      paste0("+ ", round(df$intercept, 2)))
  
  rmse_text <- paste0("RMSE = ", round(df$rmse, 2))
  
  paste0("y = ", round(df$slope, 4), "x ", intercept, 
         "\n", "n = ", df$n, 
         "\n", rmse_text)
}

# Compute overall regression statistics and RMSE for "OGALLALA AQUIFER"
overall_regression <- data %>%
  summarize(
    intercept = coef(lm(Yield ~ total_ppt_07))[1],
    slope = coef(lm(Yield ~ total_ppt_07))[2],
    n = n(),
    rmse = sqrt(mean(residuals(lm(Yield ~ total_ppt_07))^2))  # Calculate RMSE for overall regression
  )

overall_regression <- overall_regression %>%
  mutate(Part = "OGALLALA AQUIFER")  # Add "High Plains" as State

# Combine state and overall regression data
regression_data <- bind_rows(regression_data, overall_regression)

# Add a new row to the dataset that includes all data as "High Plains"
data_with_overall <- data %>%
  bind_rows(data %>% mutate(Part = "OGALLALA AQUIFER"))  # Add an "Overall" category

# # Define a named vector with the desired facet titles
# facet_titles <- c(
#   'COLORADO' = 'COLORADO (Co-16)', 
#   'KANSAS' = 'KANSAS (Co-56)', 
#   'NEBRASKA' = 'NEBRASKA (Co-86)', 
#   'NEW MEXICO' = 'NEW MEXICO (Co-10)', 
#   'OKLAHOMA' = 'OKLAHOMA (Co-9)', 
#   'SOUTH DAKOTA' = 'SOUTH DAKOTA (Co-7)', 
#   'WYOMING' = 'WYOMING (Co-6)', 
#   'OGALLALA AQUIFER' = 'OGALLALA AQUIFER (Co-190)'
# )

facet_titles <- c(
  'Midwest' = 'Midwest (Co-149)',
  'Southwest' = 'Southwest (Co-19)',
  'West' = 'West (Co-22)',
  'OGALLALA AQUIFER' = 'Ogallala aquifer (Co-190)'
)



# Plot with regression equation, sample size, and RMSE included
ppt_pic <- ggplot(data_with_overall, aes(x = total_ppt_07, y = Yield)) +
  geom_point(aes(color = Part), alpha = 0.6) +                    # Scatter plot of the points
  geom_smooth(aes(color = Part), method = "lm", se = FALSE, linetype = "dashed", 
              color = "black", size = 1) +  # Dotted regression line per state
  facet_wrap(~factor(Part, levels = c('Midwest', 'Southwest', 'West', 'OGALLALA AQUIFER')),
             labeller = as_labeller(facet_titles), scales = "free") +  # Facet by State, with "Overall" included
  labs(title = "",
       x = "Total precipitation of July month (mm)",
       y = (expression(paste("Yield (t ", ha^{-1},")")))) +
  theme_minimal() +
  theme(legend.position = "none",              # Remove the legend
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white", linetype = 1, size = 0.75, color = 'black'),
        axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(color = "black", size = 16),
        strip.text = element_text(size = 16, face = "bold"),
        plot.margin = unit(c(0, 0.5, 0.5, 0), "cm")) +
  
  # Add regression equation, RMSE, and number of data points for each State
  geom_text(
    data = regression_data, 
    aes(x = Inf, y = Inf, label = regression_label(regression_data)),
    hjust = 1.1, vjust = 1.1, size = 4, inherit.aes = FALSE,
    fontface = "bold"
  )

# Display the final plot
ppt_pic



#To save high resolution picure
ggplot2::ggsave(file = "ppt.jpg", 
                plot = ppt_pic, 
                width = 15, 
                height = 11, 
                dpi = 300) 

#########################################Heatmap for Yield#############################################
# Create these new columns: Year & Month
data_map <- data %>%
  select(Year, State, Yield)
# Calculate yearly mean yield for each state
yearly_mean_yield <- data_map %>%
  group_by(Year, State) %>%
  summarise(mean_yield = mean(Yield, na.rm = TRUE))  # Calculate mean, excluding NA values


# Calculate the count of unique counties for each Year and State
yearly_counties <- data %>%
  select(Year, State, County, Yield) %>%
  group_by(Year, State) %>%
  summarise(county_count = n_distinct(County), .groups = "drop")  # Count unique counties

# Merge the yearly mean yield with the county count
df_plot_data <- left_join(yearly_mean_yield, yearly_counties, by = c("Year", "State"))

# View the result
print(df_plot_data)

#Heatmap
df_plot <- ggplot(yearly_mean_yield, aes(x = Year, y = reorder(State, desc(State)), fill = mean_yield)) +
  geom_tile() +
  scale_fill_gradient(low = "#fafafa", high = "#191970", name = expression(paste("Yield (t ", ha^{-1}, ")"))) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(breaks = c(1981, 1991, 2001, 2011, 2018)) + 
  labs(title = "", tag = "(b)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(color = "black", hjust = 0.5, size = 16, face = "bold"),  # Title bold
        axis.text.y = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        plot.tag = element_text())

df_plot


#######################################Heat map for annual ppt######################################
# Create these new columns: Year & Month
data_map <- data %>%
  select(Year, State, annual_ppt)

#Heatmap
df_plot2 <- ggplot(data_map, aes(x = Year, y = reorder(State, desc(State)), fill = annual_ppt)) +
  geom_tile() +
  scale_fill_gradient(low = "#fafafa", high = "#191970", name = "Precipitation (mm)") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(breaks = c(1981, 1990, 2000, 2010, 2018)) + 
  labs(title = "Annual precipitation") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(color = "black", hjust = 0.5,  size = 16, face = "bold"),  
        axis.text.y = element_blank(),      
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(color = "black", size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

df_plot2

library(gridExtra)

# Adjusting plot margins to increase the distance between the plots
df_plot_adjusted <- df_plot + 
  theme(legend.position = "right", plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase bottom margin

df_plot2_adjusted <- df_plot2 + 
  theme(legend.position = "right", plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase top margin

# Arranging the plots with increased spacing
Pic2 <- grid.arrange(
  arrangeGrob(df_plot_adjusted  + theme(legend.position="bottom"),
              df_plot2_adjusted  + theme(legend.position="bottom"),
              nrow = 1), 
  nrow = 2,
  heights = c(10, 1)
)


#To save high resolution picure
ggplot2::ggsave(file = "Heatmap.jpg", 
                plot = Pic2, 
                width = 12, 
                height = 6.5, 
                dpi = 300) 


########################## Simple linear graph for yield ###################################
library(ggbreak)

data_filter <- data %>%
  select(Year, State,Yield)

data_filter_1 <- data_filter %>%
  group_by(Year) %>%
  summarize(avg_yield = mean(Yield, na.rm = TRUE),
            sd_yield = sd(Yield, na.rm = TRUE))


# Calculate the mid-points of each period for labeling purposes
data_filter_period <- data_filter %>%
  mutate(Period = case_when(
    Year >= 1981 & Year <= 1990 ~ "1981-1990",
    Year >= 1991 & Year <= 2000 ~ "1991-2000",
    Year >= 2001 & Year <= 2010 ~ "2001-2010",
    Year >= 2011 & Year <= 2018 ~ "2011-2018"
  )) %>%
  group_by(Period) %>%
  summarize(
    avg_yield = mean(Yield, na.rm = TRUE),
    sd_yield = sd(Yield, na.rm = TRUE),
    mid_year = case_when(
      Period == "1981-1990" ~ 1985,
      Period == "1991-2000" ~ 1995,
      Period == "2001-2010" ~ 2005,
      Period == "2011-2018" ~ 2015
    )
  )

# Create the line graph with shaded regions between the vlines
line_graph <- ggplot(data_filter_1, aes(x = Year, y = avg_yield)) +
  # Add shaded regions between the years
  geom_rect(aes(xmin = 1981, xmax = 1991, ymin = -Inf, ymax = Inf), fill = "#E6E6FA", alpha = 0.3) +
  geom_rect(aes(xmin = 1991, xmax = 2001, ymin = -Inf, ymax = Inf), fill = "#BDB5D5", alpha = 0.3) +
  geom_rect(aes(xmin = 2001, xmax = 2011, ymin = -Inf, ymax = Inf), fill = "#C3B1E1", alpha = 0.3) +
  geom_rect(aes(xmin = 2011, xmax = 2018, ymin = -Inf, ymax = Inf), fill = "#CCCCFF", alpha = 0.3) +
  
  # Line plot and points for average yield
  geom_line(color = "black", size = 1) +
  geom_point(color = "#191970", size = 2) +
  
  # Labels and themes
  labs(title = "",
       tag = "(a)",
       x = "Year", 
       y = expression(paste("Yield (t  ", ha^{-1}, ")"))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        plot.tag = element_text()) + 
  
  # Customize x-axis breaks and add vertical dashed lines
  scale_x_continuous(breaks = c(1981, 1991, 2001, 2011, 2018), limits = c(1981, 2018), expand = c(0, 0)) +
  geom_vline(xintercept = c(1991, 2001, 2011), linetype = "dashed", color = "#5D3FD3", size = 0.8) +
  
  # Add text annotations for each period's average yield
  geom_text(data = data_filter_period, 
            aes(x = mid_year, y = avg_yield, label = round(avg_yield, 2)),
            vjust = -5.6,hjust = +1.5, color = "#512888", size = 5) +
  
  
  # Show ±SD
  geom_text(data = data_filter_period, 
            aes(x = mid_year, y = avg_yield, label = paste0(" ± ", round(sd_yield, 2))), 
            vjust = -5.6, hjust = +0.3, color = "#512888", size = 5)+
  scale_y_continuous(limits = c(8, 10.5))

# Display the line graph
line_graph

ggplot2::ggsave(file = "line.jpg", 
                plot = line_graph, 
                width = 14, 
                height = 4, 
                dpi = 300) 

############## Heatmap of yield and line graph #####################################

library(gridExtra)
library(grid)


# # Adjusting plot margins to increase the distance between the plots
# Heat_line <- df_plot + 
#   theme(legend.position = "right", plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Increase bottom margin


# Arranging the plots with increased spacing
Heat_line <- grid.arrange(
  arrangeGrob(
    line_graph,
    df_plot + theme(legend.position = "right"),
    ncol = 1
  ),
  bottom = textGrob("Year", gp = gpar(fontsize = 14), vjust = -3.5),
  nrow = 2,
  heights = c(10, 1)
)


#To save high resolution picure
ggplot2::ggsave(file = "Heatmap_and_line_graph.jpg", 
                plot = Heat_line, 
                width = 12, 
                height = 8, 
                dpi = 300) 

################ Three plots together with yield stability map ##############

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


ggplot2::ggsave(file = "3 plots.jpg", 
                plot = combined_plot, 
                width = 12, 
                height = 8, 
                dpi = 300) 


##########################################   MAPS  for decadal annual ppt  ########################################################
library(dplyr)
library(ggplot2)
library(tigris)
library(sf)
library(gridExtra)
library(gridtext)

###########################################################################################
data_fil <- data %>%
  filter(Year >= 2011 & Year <= 2018) %>%  # Filter for years between 1981 and 1990
  select(Year, State, County, annual_GDD) %>%
  group_by(State, County) %>%
  summarize(Avg_GDD = mean(annual_GDD, na.rm = TRUE))  # Calculate the average yield


# data_fil <- data %>%
#   filter(Year >= 1981 & Year <= 1990) %>%
#   select(Year, State, County, avg_tmin_01:avg_tmin_12) %>%
#   group_by(State, County) %>%
#   summarize(Avg_temp = mean(rowMeans(across(avg_tmin_01:avg_tmin_12), na.rm = TRUE)), .groups = "drop")


# data$County <- tolower(data$County)
# data$County <- stringr::str_to_title(data$County)
# 
data_fil$State <- tolower(data_fil$State)
data_fil$State <- stringr::str_to_title(data_fil$State)
# 

# data <- read.csv("dataset.csv")
counties <- counties(cb = TRUE)  # Load the county boundaries
states <- states(cb = TRUE)

# Define the states of interest
states_of_interest <- c("Wyoming", "Texas", "New Mexico", "Oklahoma", "Kansas", "Nebraska", "South Dakota", "Colorado")

# Filter the states data for the selected states
states_filtered <- states %>%
  filter(NAME %in% states_of_interest)

state_centroids <- states_filtered %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(State_Name = states_filtered$STUSPS)

data_filter <- data_fil %>%
  mutate(County = toupper(County))  # Convert to upper case for matching

counties <- counties %>%
  mutate(NAME = toupper(NAME))  # County names in shapefiles to uppercase

counties1 <- counties %>%
  select(STATE_NAME, NAME, geometry)

map_data <- counties1 %>%
  left_join(data_filter, by = c("STATE_NAME" = "State", "NAME" = "County"))

###################################### Use for temp only################################
# Remove rows with missing Avg_Yield after merging
map_data <- map_data %>%
  filter(!is.na(Avg_temp))  # This will keep only rows where Avg_Yield is not NA
dataset <- as.data.frame(map_data)

####################################Used for GDD map ###########################

map_data <- map_data %>%
  filter(!is.na(Avg_GDD))  # This will keep only rows where Avg_Yield is not NA
dataset <- as.data.frame(map_data)

ppt_Map4 <- ggplot() +
  geom_sf(data = map_data, aes(fill = Avg_GDD), color = "NA") +
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 1.2) +
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 4, fontface = "bold") + 
  scale_fill_viridis_c(option = "turbo", breaks = c(1000, 3000, 5000, 7000, 9000), limits = c(1000, 9000), direction = -1) +
  labs(title = "", tag = "(d)", fill = expression(paste("Annual Growing Degree Days (", {}^o, "C days)")), x = NULL, y = NULL) +
  xlim(c(-108, -96)) +
  ylim(c(31, 44.5)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom", 
        legend.key.size = unit(1, "line")) 
ppt_Map4

##############################################################

#########################################################################

# ppt_Map2 <- ggplot() +
#   geom_sf(data = map_data, aes(fill = Avg_ppt)) +
#   geom_sf(data = states_filtered, fill = NA, color = "black", size = 1.2) +
#   geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
#             size = 4, fontface = "bold") + 
#   scale_fill_viridis_c(breaks = c(5, 10, 15), limits = c(0, 20), direction = -1) +
#   labs(title = "1991-2000", fill = "Annual precipitation (mm)", x = NULL, y = NULL) +
#   xlim(c(-108, -96)) +
#   ylim(c(31, 44.5)) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         plot.title = element_text(face = "bold", hjust = 0.5),
#         legend.key.size = unit(1, "line")) 
# 
# ppt_Map3 <- ggplot() +
#   geom_sf(data = map_data, aes(fill = Avg_Yield)) +
#   geom_sf(data = states_filtered, fill = NA, color = "black", size = 1.2) +
#   geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
#             size = 4, fontface = "bold") + 
#   scale_fill_viridis_c(breaks = c(5, 10, 15), limits = c(0, 20), direction = -1) +
#   labs(title = "2001-2010", fill = "Annual precipitation (mm)", x = NULL, y = NULL) +
#   xlim(c(-108, -96)) +
#   ylim(c(31, 44.5)) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         plot.title = element_text(face = "bold", hjust = 0.5),
#         legend.key.size = unit(1, "line")) 
# 
# ppt_Map4 <- ggplot() +
#   geom_sf(data = map_data, aes(fill = Avg_Yield)) +
#   geom_sf(data = states_filtered, fill = NA, color = "black", size = 1.2) +
#   geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
#             size = 4, fontface = "bold") + 
#   scale_fill_viridis_c(breaks = c(5, 10, 15), limits = c(0, 20), direction = -1) +
#   labs(title = "2011-2018", fill = "Annual precipitation (mm)", x = NULL, y = NULL) +
#   xlim(c(-108, -96)) +
#   ylim(c(31, 44.5)) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         plot.title = element_text(face = "bold", hjust = 0.5),
#         legend.key.size = unit(1, "line")) 

#Function to extract the legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(
  ppt_Map4 + 
    theme(legend.position = "bottom",
          legend.key.width = unit(2, "cm")) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))
)


#Arranging the plots and place the legend at the bottom
ppt_map <- grid.arrange(
  arrangeGrob(
    ppt_Map1 + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm")),
    ppt_Map2 + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm")),
    ppt_Map3 + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm")),
    ppt_Map4 + theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm")),
    nrow = 2
  ),
  mylegend,
  nrow = 2,
  heights = c(10, 1)
)




ggplot2::ggsave(file = "annual_gdd_map.jpg", 
                plot = ppt_map, 
                width = 6, 
                height = 9, 
                dpi = 300) 




############################### Used for Avg annual minimum temperature map ######################

ppt_Map4 <- ggplot() +
  geom_sf(data = map_data, aes(fill = Avg_temp), color = "NA") +
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 1.2) +
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 4, fontface = "bold") + 
  scale_fill_viridis_c(option = "turbo", breaks = c(-5, 0, 5, 10), limits = c(-5, 10), direction = 1) +
  labs(title = "", tag = "(d)", fill = expression(paste("Average annual minimum temperature (", {}^o, "C)")), x = NULL, y = NULL) +
  xlim(c(-108, -96)) +
  ylim(c(31, 44.5)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom", 
        legend.key.size = unit(1, "line")) 
ppt_Map4
#######################################################################################################


################################# Used for Avg annual maximum temperature map #############
ppt_Map1 <- ggplot() +
  geom_sf(data = map_data, aes(fill = Avg_temp), color = "NA") +
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 1.2) +
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 4, fontface = "bold") + 
  scale_fill_viridis_c(option = "turbo", breaks = c(10, 15, 20, 25, 30), limits = c(10, 30), direction = 1) +
  labs(title = "", tag = "(a)", fill = expression(paste("Average annual minimum temperature (", {}^o, "C)")), x = NULL, y = NULL) +
  xlim(c(-108, -96)) +
  ylim(c(31, 44.5)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom", 
        legend.key.size = unit(1, "line")) 
ppt_Map1

#################################### used for annual avergae PPT map ####################

map_data <- map_data %>%
  filter(!is.na(Avg_ppt))  # This will keep only rows where Avg_Yield is not NA
dataset <- as.data.frame(map_data)

ppt_Map4 <- ggplot() +
  geom_sf(data = map_data, aes(fill = Avg_ppt), color = "NA") +
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 1.2) +
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 4, fontface = "bold") + 
  scale_fill_viridis_c(option = "turbo", breaks = c(0, 250, 500, 750, 1000), limits = c(0, 1000), direction = -1) +
  labs(title = "", tag = "(d)", fill = "Average annual precipitation (mm)", x = NULL, y = NULL) +
  xlim(c(-108, -96)) +
  ylim(c(31, 44.5)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom", 
        legend.key.size = unit(1, "line")) 
ppt_Map4



####################################### mRMR ranking data preparation #################################################################

# Use this chunk to get mRMR ranking for each state. In case for overall Ogallala do not use this.
# dataset <- dataset_with_centeroids%>%
#   filter(State %in% c("Nebraska")) %>% #change for different state seperately
#   select(-Part,-State, -County)


# Specify the columns to exclude from normalization
cols_to_exclude <- c("Year", "Yield", "latitude", "longitude", "geometry")

# Separate excluded columns
excluded_data <- dataset[, cols_to_exclude]
included_data <- dataset[, !colnames(dataset) %in% cols_to_exclude]

# Apply Min-Max normalization to the included data only
normalize_min_max <- preProcess(included_data, method = c("range"))
included_data_normalized <- predict(normalize_min_max, included_data)

# Combine the normalized and excluded columns
dataset_normalized <- cbind(included_data_normalized, excluded_data)

# Convert the dataset to numeric
dataset_normalized[] <- lapply(dataset_normalized, function(x) as.numeric(as.character(x)))



#################################### mRMR ###################################

# Prepare mRMRe data object
mrmr_data <- mRMR.data(data = dataset_normalized)

# Run mRMR for feature selection
target_column <- which(colnames(dataset_normalized) == "Yield")  # 'Yield' is the target variable
feature_count <- 17 # Choose the number of features to select
mrmr_result <- mRMR.classic(data = mrmr_data, target_indices = target_column, feature_count = feature_count)

# Get the selected feature indices and scores
selected_features <- solutions(mrmr_result)[[1]]
selected_feature_scores <- scores(mrmr_result)[[1]]

# Create a data frame for feature ranking
feature_ranking <- data.frame(
  Feature = colnames(dataset_normalized)[selected_features],
  mRMR_Score = selected_feature_scores
)

# write.csv(feature_ranking, "feature_ranking.csv", row.names = FALSE)


# Normalize the mRMR scores between 0 and 1
feature_ranking$Normalized_Score <- (feature_ranking$mRMR_Score - min(feature_ranking$mRMR_Score)) / 
  (max(feature_ranking$mRMR_Score) - min(feature_ranking$mRMR_Score))

# Calculate differences between consecutive mRMR scores and find local minimum
score_diffs <- diff(feature_ranking$mRMR_Score)
local_min_index <- which(score_diffs > 0)[1] + 1

# Extract top features up to the local minimum
top_features <- feature_ranking$Feature[1:local_min_index]

# Display results
cat("First local minimum at index:", local_min_index, "\n")
cat("Top features selected:\n")
print(top_features)

# Create a data frame for plotting the top features and their normalized mRMR scores
top_feature_ranking <- data.frame(
  Feature = top_features,
  Normalized_Score = feature_ranking$Normalized_Score[1:local_min_index]
)



##################################################################################################################################

# Plot the feature ranking as a bar plot based on normalized mRMR scores
Ogallala <- ggplot(top_feature_ranking, aes(x = reorder(Feature, -Normalized_Score), y = Normalized_Score)) +
  geom_bar(stat = "identity", fill = "#512888") +
  # coord_flip() +
  labs(title = "Ogallala Aquifer (Co-190)",
       x = "Predictor rank",
       y = "Normalized predictor importance score") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman", size = 14)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black", angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white", linetype = 1, size = 0.75, color = 'black')) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))  # Adjust y-axis limits for normalized scores

Ogallala

# ggplot2::ggsave(file = "mRMR_ogallala.jpg",
#                 plot = mRMR_ogallala,
#                 width = 6,
#                 height = 9,
#                 dpi = 300)

############################## Grid for mRMR #############################################################
library(gridExtra)
library(grid)
library(gridtext)
library(ggplot2)
library(cowplot)

# Reduce axis text size and remove titles
adjust_theme <- theme(
  axis.title = element_blank(),  # Remove axis titles
  axis.text = element_text(size = 10, family = ""),
  plot.title = element_text(family = "")
)

# Apply the theme adjustment to each plot
Ogallala1 <- Ogallala + adjust_theme
Colorado1 <- Colorado + adjust_theme
Kansas1 <- Kansas + adjust_theme
Nebraska1 <- Nebraska + adjust_theme
New_Mexico1 <- New_Mexico + adjust_theme
Oklahoma1 <- Oklahoma + adjust_theme
South_Dakota1 <- South_Dakota + adjust_theme
Wyoming1 <- Wyoming + adjust_theme


# Create an empty grob for left padding
left_padding <- grid::rectGrob(gp = grid::gpar(fill = NA, col = NA))

# # Arrange the plots with padding
# final_plot <- grid.arrange(
#   arrangeGrob(left_padding,  # Add blank space to the left
#               arrangeGrob(Ogallala1, Colorado1, Kansas1, Nebraska1, 
#                           New_Mexico1, Oklahoma1, South_Dakota1, 
#                           Wyoming1, 
#                           nrow = 2),
#               ncol = 2,  # Two columns: padding + main plots
#               widths = c(0.5, 10)),  # Adjust padding width
#   heights = c(10, 1)  # Adjust row heights
# )
# 
# # Start a new plotting page
# grid.newpage()
# 
# # Draw the main grid plot
# grid.draw(final_plot)
# 
# # Create a combined plot with annotations
# final_plot_with_labels <- ggdraw(final_plot) +
#   draw_label("Predictor rank", x = 0.5, y = 0.08, size = 14) +
#   draw_label("Normalized predictor importance score", x = 0.03, y = 0.5, size = 14, angle = 90)
# 
# final_plot_with_labels




# Define table header and descriptions
table_header <- c(
  "Abbreviation", "total_ppt_*", "no_ppt_*", "avg_vpdmax_*", "avg_vpdmin_*",
  "avg_tdmean_*", "avg_tmean_*", "avg_tmax_*", "annual_ppt", "annual_GDD"
)

table_desc <- c(
  "Full name",
  "Monthly total\nprecipitation",
  "Number of monthly\nprecipitation days",
  "Average monthly maximum\nvapor pressure deficit",
  "Average monthly minimum\nvapor pressure deficit",
  "Monthly mean\ndewpoint temperature",
  "Monthly mean\ntemperature",
  "Average monthly\nmaximum temperature",
  "Annual precipitation",
  "Annual growing\ndegree days"
)

# Create the table grob
info_table <- tableGrob(
  rbind(table_header, table_desc),
  rows = NULL,
  theme = ttheme_default(
    core = list(fg_params = list(fontsize = 10)),
    colhead = list(fg_params = list(fontsize = 12, fontface = "bold"))
  )
)

# Combine the plots and the table in one layout
final_combined <- grid.arrange(
  arrangeGrob(
    left_padding,
    arrangeGrob(
      Ogallala1, Colorado1, Kansas1, Nebraska1,
      New_Mexico1, Oklahoma1, South_Dakota1, Wyoming1,
      nrow = 2
    ),
    ncol = 2,
    widths = c(0.5, 10)
  ),
  info_table,
  heights = c(10, 1.5),
  nrow = 2
)

# Draw the final layout with axis labels
grid.newpage()
final_combined_with_labels <- ggdraw(final_combined) +
  draw_label("Predictor rank", x = 0.5, y = 0.13, size = 14) +
  draw_label("Normalized predictor importance score", x = 0.03, y = 0.5, size = 14, angle = 90)

# Display everything
final_combined_with_labels

# Save the plot with labels
ggsave(filename = "final_plot_mrmr_all.jpg", 
       plot = final_combined_with_labels, 
       width = 14, 
       height = 10, 
       dpi = 300)


# 
# #################################################################################################

library(ggplot2)
library(caret)
library(randomForest)
library(gbm)
library(lightgbm)
library(xgboost)


# Prepare the dataset for RMSE calculations using mRMR-ranked features
all_features <- feature_ranking$Feature  # Ranked features from mRMR
dataset_selected <- dataset_normalized[, c(all_features, "Yield")]

# Initialize an empty data frame to store results
rmse_results <- data.frame(
  Features = integer(),
  Model = character(),
  RMSE = numeric()
)

# Iterate over the number of features using mRMR ranking
for (num_features in 1:length(all_features)) {
  # Select top N features based on mRMR ranking
  features_to_use <- all_features[1:num_features]
  dataset_sub <- dataset_selected[, c(features_to_use, "Yield")]

  # Split the dataset into training (80%) and testing (20%)
  set.seed(42)  # For reproducibility
  train_index <- createDataPartition(dataset_sub$Yield, p = 0.8, list = FALSE)
  train_data <- dataset_sub[train_index, ]
  test_data <- dataset_sub[-train_index, ]

  # Train and evaluate models
  for (model_name in c("Random Forest", "GBM", "LightGBM", "XGBoost")) {
    set.seed(42)  # For reproducibility

    # Train model
    if (model_name == "Random Forest") {
      model <- randomForest(as.formula("Yield ~ ."), data = train_data)
    } else if (model_name == "GBM") {
      model <- gbm::gbm(as.formula("Yield ~ ."), data = train_data, n.trees = 100)
    } else if (model_name == "LightGBM") {
      model <- lightgbm::lgb.train(params = list(objective = "regression"),
                                   data = lightgbm::lgb.Dataset(data.matrix(train_data[, features_to_use]), label = train_data[["Yield"]]))
    } else if (model_name == "XGBoost") {
      model <- xgboost::xgb.train(
        params = list(objective = "reg:squarederror"),
        data = xgboost::xgb.DMatrix(data = data.matrix(train_data[, features_to_use]), label = train_data[["Yield"]]),
        nrounds = 100
      )
    }

    # Predict and calculate RMSE
    if (model_name %in% c("LightGBM", "XGBoost")) {
      predictions <- predict(model, data.matrix(test_data[, features_to_use]))
    } else {
      predictions <- predict(model, test_data)
    }
    rmse <- sqrt(mean((test_data[["Yield"]] - predictions)^2))

    # Store results
    rmse_results <- rbind(rmse_results, data.frame(
      Features = num_features,
      Model = model_name,
      RMSE = rmse
    ))
  }
}

# Calculate average RMSE for each number of features
average_rmse <- aggregate(RMSE ~ Features, data = rmse_results, FUN = mean)
average_rmse$Model <- "Average"

# Combine average RMSE with the original results
rmse_results_combined <- rbind(rmse_results, average_rmse)

# Plot the RMSE values, including the average line
featureno1 <- ggplot(rmse_results_combined, aes(x = Features, y = RMSE, color = Model, group = Model)) +
  geom_line(size = 1) +
  geom_point(size = 1, data = rmse_results) +  # Keep points only for individual models
  geom_line(data = average_rmse, aes(x = Features, y = RMSE), linetype = "dashed", size = 1.2, color = "black") +
  labs(
    title = "",
    x = "Number of Features",
    y = expression(paste("RMSE (t  ", ha^{-1}, ")")),
    color = "Model"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  scale_color_manual(values = c("black", scales::hue_pal()(4)),
                      labels = c("Average", "Gradient Boosting", "Light Gradient Boosting",
                                 "Random Forest", "Extreme Gradient Boosting")) +
  scale_x_continuous(breaks = seq(0, max(rmse_results_combined$Features), by = 10), expand = c(0, 0)) +
  geom_vline(xintercept = 17, linetype = "solid", color = "#191970", size = 0.8)  # Add solid line at x = 17

featureno1

ggplot2::ggsave(file = "featureno2_1.jpg",
                plot = featureno1,
                width = 9,
                height = 6,
                dpi = 300)



########################## Gradient Boosting Machine ########################################################

# Load necessary libraries
library(caret)  # For data partitioning
library(gbm)    # For Gradient Boosting Machines

# Step 1: Define the target variable
target_var <- "Yield"

# Step 2: Split the data into training and testing sets (80:10)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(dataset_normalized[[target_var]], p = 0.8, list = FALSE)
train_data <- dataset_normalized[train_index, ]
test_data <- dataset_normalized[-train_index, ]

# Check the distribution
cat("Training Data Years: ", range(train_data$Year), "\n")
cat("Testing Data Years: ", range(test_data$Year), "\n")

# Confirm the split
cat("Number of training samples:", nrow(train_data), "\n")
cat("Number of testing samples:", nrow(test_data), "\n")


# Step 3: Extract the selected features based on mRMR result
top_features <- colnames(dataset_normalized)[selected_features]
print(top_features)

# Prepare training and testing datasets using the selected features
X_train <- train_data[, top_features]
y_train <- train_data[[target_var]]

X_test <- test_data[, top_features]
y_test <- test_data[[target_var]]

# Step 4: Train the Gradient Boosting Model with 10-Fold Cross-Validation
gbm_model <- gbm(
  formula = as.formula(paste(target_var, "~ .")),
  data = train_data[, c(top_features, target_var)],
  distribution = "gaussian",       # For regression tasks
  n.trees = 1000,                  # Number of trees
  interaction.depth = 20,          # Maximum depth of each tree
  shrinkage = 0.05,                # Learning rate
  n.minobsinnode = 20,             # Minimum observations in terminal nodes
  cv.folds = 10,                   # Perform 10-fold cross-validation
  n.cores = NULL,                  # Use all available cores
  verbose = TRUE                   # Print training progress
)

# Step 5: Calculate the average RMSE from cross-validation
cv_results <- gbm.perf(gbm_model, method = "cv", plot.it = FALSE)  # Optimal iteration based on CV
average_rmse <- sqrt(min(gbm_model$cv.error))  # Average RMSE from cross-validation

# Step 6: Train the final model using the best number of trees from CV
final_gbm_model <- gbm(
  formula = as.formula(paste(target_var, "~ .")),
  data = train_data[, c(top_features, target_var)],
  distribution = "gaussian",
  n.trees = cv_results,            # Optimal number of trees
  interaction.depth = 20,
  shrinkage = 0.05,
  n.minobsinnode = 20,
  n.cores = NULL,
  verbose = FALSE
)

# Step 7: Make predictions on the test set
y_pred <- predict(final_gbm_model, newdata = test_data[, c(top_features)], n.trees = cv_results)

# Step 8: Evaluate the model on the test set
rmse <- sqrt(mean((y_test - y_pred)^2))  # RMSE
mae <- mean(abs(y_test - y_pred))        # MAE
r_squared <- 1 - (sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2))  # R-squared

# Number of observations (n) and number of predictors (p)
n <- length(y_test)          # Total number of observations in the test set
p <- length(top_features)    # Number of predictors in the GBM model

# Adjusted R-squared formula
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1)) / (n - p - 1)

# Step 9: Print the model performance metrics
cat("Average RMSE across 10 folds (Cross-Validation): ", average_rmse, "\n")
cat("RMSE on test set: ", rmse, "\n")
cat("Mean Absolute Error on test set: ", mae, "\n")
cat("R-squared on test set: ", r_squared, "\n")
cat("Adjusted R-squared on test set: ", adjusted_r_squared, "\n")

############ Putting in the table for mapping #################
library(dplyr)
# 
# # Add empty columns for RMSE, MAE, R-squared, and Adjusted R-squared
# dataset_with_errors <- dataset_with_centeroids %>%
#   mutate(
#     RMSE = NA,  # Initialize RMSE column with NA
#     MAE = NA,   # Initialize MAE column with NA
#     R_squared = NA,  # Initialize R-squared column with NA
#     Adjusted_R_squared = NA  # Initialize Adjusted R-squared column with NA
#   )

target_state <- "Nebraska"

# Add error metric values only for rows where State == "Wyoming"
dataset_with_errors <- dataset_with_errors %>%
  mutate(
    RMSE = ifelse(State == target_state, rmse, RMSE),  
    MAE = ifelse(State == target_state, mae, MAE),     
    R_squared = ifelse(State == target_state, r_squared, R_squared),  
    Adjusted_R_squared = ifelse(State == target_state, adjusted_r_squared, Adjusted_R_squared)  
  )


# Select only the required columns
dataset_selected <- dataset_with_errors %>%
  select(State, County, Yield, longitude, latitude, RMSE, MAE, R_squared, Adjusted_R_squared)



############################################################################



# Select only the required columns
dataset_selected <- dataset_with_errors %>%
  select(State, County, Yield, longitude, latitude, RMSE, MAE, R_squared, Adjusted_R_squared)

# dataset_selected <- read.csv("dataset_with_errors_GBM.csv")

data_fil <- dataset_selected %>%
  select(State, County, R_squared, longitude, latitude) %>%
  group_by(State, County)

data_fil$State <- tolower(data_fil$State)
data_fil$State <- stringr::str_to_title(data_fil$State)

counties <- counties(cb = TRUE)  # Load the county boundaries
states <- states(cb = TRUE)

# Define the states of interest
states_of_interest <- c("Wyoming", "Texas", "New Mexico", "Oklahoma", "Kansas", "Nebraska", "South Dakota", "Colorado")

# Filter the states data for the selected states
states_filtered <- states %>%
  filter(NAME %in% states_of_interest)

state_centroids <- states_filtered %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(State_Name = states_filtered$STUSPS)

data_filter <- data_fil %>%
  mutate(County = toupper(County))  # Convert to upper case for matching

counties <- counties %>%
  mutate(NAME = toupper(NAME))  # County names in shapefiles to uppercase

counties1 <- counties %>%
  select(STATE_NAME, NAME, geometry)

map_data <- counties1 %>%
  left_join(data_filter, by = c("STATE_NAME" = "State", "NAME" = "County"))

map_data <- map_data %>%
  filter(!is.na(R_squared))  # This will keep only rows where Avg_Yield is not NA
dataset <- as.data.frame(map_data)

######################################## For the mapping #####################################################

library(ggplot2)

Map1 <- ggplot() +
  geom_sf(data = map_data, aes(fill = R_squared), color = "NA") +  # Ensure R_squared is numeric
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 1.2) +
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 4, fontface = "bold") + 
  scale_fill_viridis_c(option = "turbo", limits = c(0, 1), direction = -1, breaks = seq(0, 1, by = 0.1)) +
  labs(title = "Gradient Boosting", fill = "R-squared", x = NULL, y = NULL) +
  scale_x_continuous(limits = c(-108, -96), breaks = seq(-108, -96, by = 4)) +  
  scale_y_continuous(limits = c(31, 44.5), breaks = seq(32, 44, by = 4)) +  
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1, "line"),
        legend.key.width = unit(2, "cm"),
        axis.text = element_text(size = 10, color = "black")) 

  # annotation_scale(location = "bl", width_hint = 0.3) +  # Adds a scale bar
  # annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering())  # Adds a north arrow

print(Map1)  # Ensure the plot is displayed


######################################### testing ###################################################
# 
# library(dplyr)
# library(caret)
# library(gbm)
# 
# # Step 1: Define the target variable
# target_var <- "Yield"
# 
# # Step 3: Extract the selected features based on mRMR result
# top_features <- colnames(dataset_normalized)[selected_features]
# print(top_features)
# 
# # Prepare training and testing datasets using the selected features
# X_train <- train_data[, top_features]
# y_train <- train_data[[target_var]]
# 
# X_test <- test_data[, c(top_features, "Location_ID")]  # Keep Location_ID for later grouping
# y_test <- test_data[[target_var]]
# 
# # Step 4: Train the Gradient Boosting Model with 10-Fold Cross-Validation
# gbm_model <- gbm(
#   formula = as.formula(paste(target_var, "~ .")),
#   data = train_data[, c(top_features, target_var)],
#   distribution = "gaussian",       # For regression tasks
#   n.trees = 10000,                  # Number of trees
#   interaction.depth = 20,          # Maximum depth of each tree
#   shrinkage = 0.05,                # Learning rate
#   n.minobsinnode = 20,             # Minimum observations in terminal nodes
#   cv.folds = 10,                   # Perform 10-fold cross-validation
#   n.cores = NULL,                   # Use all available cores
#   verbose = TRUE                   # Print training progress
# )
# 
# # Step 5: Calculate the average RMSE from cross-validation
# cv_results <- gbm.perf(gbm_model, method = "cv", plot.it = FALSE)  # Optimal iteration based on CV
# average_rmse <- sqrt(min(gbm_model$cv.error))  # Average RMSE from cross-validation
# 
# # Step 6: Train the final model using the best number of trees from CV
# final_gbm_model <- gbm(
#   formula = as.formula(paste(target_var, "~ .")),
#   data = train_data[, c(top_features, target_var)],
#   distribution = "gaussian",
#   n.trees = cv_results,            # Optimal number of trees
#   interaction.depth = 20,
#   shrinkage = 0.05,
#   n.minobsinnode = 20,
#   n.cores = NULL,
#   verbose = FALSE
# )
# 
# # Step 7: Make predictions on the test set
# y_pred <- predict(final_gbm_model, newdata = X_test[, top_features], n.trees = cv_results)
# 
# # Combine predictions with test set data
# results <- data.frame(
#   Location_ID = X_test$Location_ID,
#   Actual = y_test,
#   Predicted = y_pred
# )
# 
# # Step 8: Calculate error metrics for each unique Location_ID
# location_metrics <- results %>%
#   group_by(Location_ID) %>%
#   summarise(
#     RMSE = sqrt(mean((Actual - Predicted)^2)),
#     MAE = mean(abs(Actual - Predicted)),
#     R_squared = 1 - (sum((Actual - Predicted)^2) / sum((Actual - mean(Actual))^2))
#   ) %>%
#   mutate(Adjusted_R_squared = 1 - ((1 - R_squared) * (n() - 1)) / (n() - length(top_features) - 1))
# 
# # Step 9: Calculate overall error metrics
# rmse <- sqrt(mean((y_test - y_pred)^2))  # RMSE
# mae <- mean(abs(y_test - y_pred))        # MAE
# r_squared <- 1 - (sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2))  # R-squared
# 
# # Number of observations (n) and number of predictors (p)
# n <- length(y_test)          # Total number of observations in the test set
# p <- length(top_features)    # Number of predictors in the GBM model
# 
# # Adjusted R-squared formula
# adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1)) / (n - p - 1)
# 
# # Step 10: Print the model performance metrics
# cat("\n--- Overall Model Performance ---\n")
# cat("Average RMSE across 10 folds (Cross-Validation): ", average_rmse, "\n")
# cat("Overall RMSE on test set: ", rmse, "\n")
# cat("Overall Mean Absolute Error on test set: ", mae, "\n")
# cat("Overall R-squared on test set: ", r_squared, "\n")
# cat("Overall Adjusted R-squared on test set: ", adjusted_r_squared, "\n")
# 
# # Step 11: Print location-wise error metrics
# cat("\n--- Location-Specific Error Metrics ---\n")
# print(location_metrics)
# 
# 


#################################

# # Step 5b: Make predictions on the training set
# y_train_pred <- predict(gbm_model, newdata = train_data[, c(top_features)], n.trees = gbm_model$n.trees)
# 
# # Step 6b: Evaluate the model on the training set
# rmse_train <- sqrt(mean((y_train - y_train_pred)^2))  # RMSE for training set
# mae_train <- mean(abs(y_train - y_train_pred))        # MAE for training set
# r_squared_train <- 1 - (sum((y_train - y_train_pred)^2) / sum((y_train - mean(y_train))^2))  # R-squared for training set
# 
# # Step 7b: Print the model performance metrics for the training set
# cat("Training RMSE: ", rmse_train, "\n")
# cat("Training Mean Absolute Error: ", mae_train, "\n")
# cat("Training R-squared: ", r_squared_train, "\n")


# Load ggplot2 for plotting
library(ggplot2)

# Create a data frame for actual and predicted values
plot_data_gbm <- data.frame(Actual = y_test, Predicted = y_pred)

# Create scatter plot with regression line
gbm_total <- ggplot(plot_data_gbm, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) +  # Regression line
  labs(title = "Gradient Boosting",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12))  +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +  
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  coord_fixed() +
  # Add the metrics in the bottom right corner
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
           label = paste("RMSE: ", format(round(rmse, 2), nsmall = 2),
                         "\nMAE: ", format(round(mae, 2), nsmall = 2),
                         "\nR-square: ", format(round(r_squared, 2), nsmall = 2)),
           size = 4, color = "black",
           box.color = "lightgray", background.color = "white") +
  # Second annotation for units (t ha^-1)
  annotate("text", x = -Inf, y = Inf, hjust = -2.7, vjust = 1.4,
           label = expression( "t ha"^{-1}), 
           size = 4, color = "black", fontface = "bold",
           box.color = "lightgray", background.color = "white") +
  annotate("text", x = -Inf, y = Inf, hjust = -2.4, vjust = 2.8,
           label = expression( "t ha"^{-1}), 
           size = 4, color = "black", fontface = "bold",
           box.color = "lightgray", background.color = "white")

gbm_total



ggplot2::ggsave(file = "gbm_total.jpg",
                plot = gbm_total,
                width = 9,
                height = 6,
                dpi = 300)



#################################### XGBoost #######################################################

library(xgboost)
library(caret)  # For data splitting and pre-processing

# Step 1: Define the target variable
target_var <- "Yield"

# Step 2: Split the data into training and testing sets (80:10)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(dataset_normalized[[target_var]], p = 0.8, list = FALSE)
train_data <- dataset_normalized[train_index, ]
test_data <- dataset_normalized[-train_index, ]

# Step 3: Extract the selected features based on mRMR result
# Assuming `selected_features` contains the indices of important features based on mRMR output
top_features <- colnames(dataset_normalized)[selected_features]
print(top_features)

# Prepare training and testing datasets using the selected features
X_train <- as.matrix(train_data[, top_features])
y_train <- train_data[[target_var]]

X_test <- as.matrix(test_data[, top_features])
y_test <- test_data[[target_var]]

# Step 4: Perform 10-Fold Cross-Validation
cv_results <- xgb.cv(
  data = X_train,
  label = y_train,
  nrounds = 1000,                  # Number of boosting rounds
  objective = "reg:squarederror",  # For regression tasks
  max_depth = 8,                   # Maximum tree depth
  eta = 0.05,                      # Learning rate
  subsample = 1,
  colsample_bytree = 0.6,
  nthread = 2,                     # Number of threads to use
  nfold = 10,                      # Number of folds for CV
  verbose = 1,                     # Print progress during training
  early_stopping_rounds = 20       # Stop if no improvement for 20 rounds
)

# Calculate the average RMSE across all folds
average_rmse <- mean(cv_results$evaluation_log$test_rmse_mean)

# Print the average RMSE across folds and the best number of rounds
cat("Best number of rounds: ", cv_results$best_iteration, "\n")
cat("Average RMSE across folds: ", average_rmse, "\n")

# Step 5: Train the final XGBoost Model using the optimal number of rounds
final_model <- xgboost(
  data = X_train,
  label = y_train,
  nrounds = cv_results$best_iteration,  # Use the best number of rounds
  objective = "reg:squarederror",
  max_depth = 8,
  eta = 0.05,
  subsample = 1,
  colsample_bytree = 0.6,
  nthread = 2,
  verbose = 1
)

# Step 6: Make predictions on the test set
y_pred <- predict(final_model, newdata = X_test)

# Step 7: Evaluate the model on the test set
rmse <- sqrt(mean((y_test - y_pred)^2))  # RMSE
mae <- mean(abs(y_test - y_pred))        # MAE
r_squared <- 1 - (sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2))  # R-squared

# Number of observations (n) and number of predictors (p)
n <- length(y_test)          # Total number of observations in the test set
p <- length(top_features)    # Number of predictors in the GBM model

# Adjusted R-squared formula
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1)) / (n - p - 1)

# Step 8: Print the model performance metrics
cat("Average RMSE across 10 folds (Cross-Validation): ", average_rmse, "\n")
cat("RMSE on test set: ", rmse, "\n")
cat("Mean Absolute Error on test set: ", mae, "\n")
cat("R-squared on test set: ", r_squared, "\n")
cat("Adjusted R-squared on test set: ", adjusted_r_squared, "\n")


############ Putting in the table for mapping #################
library(dplyr)

# # Add empty columns for RMSE, MAE, R-squared, and Adjusted R-squared
# dataset_with_errors_xboost <- dataset_with_centeroids %>%
#   mutate(
#     RMSE = NA,  # Initialize RMSE column with NA
#     MAE = NA,   # Initialize MAE column with NA
#     R_squared = NA,  # Initialize R-squared column with NA
#     Adjusted_R_squared = NA  # Initialize Adjusted R-squared column with NA
#   )

target_state <- "Oklahoma"

# Add error metric values only for rows where State == "Wyoming"
dataset_with_errors_xboost <- dataset_with_errors_xboost %>%
  mutate(
    RMSE = ifelse(State == target_state, rmse, RMSE),  
    MAE = ifelse(State == target_state, mae, MAE),     
    R_squared = ifelse(State == target_state, r_squared, R_squared),  
    Adjusted_R_squared = ifelse(State == target_state, adjusted_r_squared, Adjusted_R_squared)  
  )


############################################################################
# Select only the required columns
dataset_selected <- dataset_with_errors_xboost %>%
  select(State, County, Yield, longitude, latitude, RMSE, MAE, R_squared, Adjusted_R_squared)

# dataset_selected <- read.csv("dataset_with_errors_xgboost.csv")

data_fil <- dataset_selected %>%
  select(State, County, R_squared, longitude, latitude) %>%
  group_by(State, County)

data_fil$State <- tolower(data_fil$State)
data_fil$State <- stringr::str_to_title(data_fil$State)

counties <- counties(cb = TRUE)  # Load the county boundaries
states <- states(cb = TRUE)

# Define the states of interest
states_of_interest <- c("Wyoming", "Texas", "New Mexico", "Oklahoma", "Kansas", "Nebraska", "South Dakota", "Colorado")

# Filter the states data for the selected states
states_filtered <- states %>%
  filter(NAME %in% states_of_interest)

state_centroids <- states_filtered %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(State_Name = states_filtered$STUSPS)

data_filter <- data_fil %>%
  mutate(County = toupper(County))  # Convert to upper case for matching

counties <- counties %>%
  mutate(NAME = toupper(NAME))  # County names in shapefiles to uppercase

counties1 <- counties %>%
  select(STATE_NAME, NAME, geometry)

map_data <- counties1 %>%
  left_join(data_filter, by = c("STATE_NAME" = "State", "NAME" = "County"))

map_data <- map_data %>%
  filter(!is.na(R_squared))  # This will keep only rows where Avg_Yield is not NA
dataset <- as.data.frame(map_data)


######################################## For the mapping #####################################################
library(ggplot2)

Map2 <- ggplot() +
  geom_sf(data = map_data, aes(fill = R_squared), color = "NA") +  # Ensure R_squared is numeric
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 1.2) +
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 4, fontface = "bold") + 
  scale_fill_viridis_c(option = "turbo", limits = c(0, 1), direction = -1, breaks = seq(0, 1, by = 0.1)) +
  labs(title = "Extreme Gradient Boosting", fill = "R-squared", x = NULL, y = NULL) +
  scale_x_continuous(limits = c(-108, -96), breaks = seq(-108, -96, by = 4)) +  
  scale_y_continuous(limits = c(31, 44.5), breaks = seq(32, 44, by = 4)) +  
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1, "line"),
        legend.key.width = unit(2, "cm"),
        axis.text = element_text(size = 10, color = "black")) 

# annotation_scale(location = "bl", width_hint = 0.3) +  # Adds a scale bar
# annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering())  # Adds a north arrow

print(Map2)  # Ensure the plot is displayed
##################################################################################################################################

# Load ggplot2 for plotting
library(ggplot2)

# Create a data frame for actual and predicted values
plot_data_xgb <- data.frame(Actual = y_test, Predicted = y_pred)


# Create scatter plot with regression line
xgb_total <- ggplot(plot_data_xgb, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) +  # Regression line
  labs(title = "Extreme Gradient Boosting",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face ="bold"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12))  +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +  
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  coord_fixed()+
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
           label = paste("RMSE: ", format(round(rmse, 2), nsmall = 2),
                         "\nMAE: ", format(round(mae, 2), nsmall = 2),
                         "\nR-square: ", format(round(r_squared, 2), nsmall = 2)),
           size = 4, color = "black",
           box.color = "lightgray", background.color = "white") +
  # Second annotation for units (t ha^-1)
  annotate("text", x = -Inf, y = Inf, hjust = -2.7, vjust = 1.4,
           label = expression( "t ha"^{-1}), 
           size = 4, color = "black", fontface = "bold",
           box.color = "lightgray", background.color = "white") +
  annotate("text", x = -Inf, y = Inf, hjust = -2.4, vjust = 2.8,
           label = expression( "t ha"^{-1}), 
           size = 4, color = "black", fontface = "bold",
           box.color = "lightgray", background.color = "white")
xgb_total
# 
# ggplot2::ggsave(file = "xgb_total.jpg",
#                 plot = xgb_total,
#                 width = 9,
#                 height = 6,
#                 dpi = 300)

# # Step 5b: Make predictions on the training set
# y_train_pred <- predict(xgb_model, newdata = X_train)
# 
# # Step 6b: Evaluate the model on the training set
# rmse_train <- sqrt(mean((y_train - y_train_pred)^2))  # RMSE for training set
# mae_train <- mean(abs(y_train - y_train_pred))        # MAE for training set
# r_squared_train <- 1 - (sum((y_train - y_train_pred)^2) / sum((y_train - mean(y_train))^2))  # R-squared for training set
# 
# # Step 7b: Print the model performance metrics for the training set
# cat("RMSE on training set: ", rmse_train, "\n")
# cat("Mean Absolute Error on training set: ", mae_train, "\n")
# cat("R-squared on training set: ", r_squared_train, "\n")


########################### SHAP for XGBoost ##########################################################
library(SHAPforxgboost)
library(ggplot2)

# Generate SHAP summary plot
shap_plot_ogallala <- shap.plot.summary.wrap1(final_model, X_train, top_n = 5) +
  ggplot2::ggtitle("Ogallala Aquifer (Co-190)") +
  ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
                 axis.line.y = element_blank(),
                 axis.ticks.y = element_blank(), legend.position = "bottom",
                 panel.grid = element_blank(), 
                 legend.title = element_text(size = 10, color = "black"),
                 legend.text = element_text(size = 10, color = "black"), 
                 axis.title.x = element_text(size = 10, color = "black"),
                 axis.text.y = element_text(size = 10, color = "black"),
                 axis.text.x.bottom = element_text(size = 10, color = "black"))
shap_plot_ogallala

########################################## Combined plot for SHAP ###############################################################

library(ggplot2)
library(gridExtra)
library(grid)

# # Function to extract the legend from a ggplot
# g_legend <- function(a.gplot) {
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   if (length(leg) > 0) {
#     return(tmp$grobs[[leg]])
#   } else {
#     return(NULL)  # Handle cases where no legend exists
#   }
# }


# Extract the legend
SHAPlegend <- g_legend(shap_plot_colorado + theme(legend.position = "bottom"))

# Define a function to remove individual x-labels and adjust plot margins
adjust_plot <- function(plot) {
  plot + theme(
    legend.position = "none", 
    axis.title.x = element_blank()
  )
}

# Apply function to all plots
shap_plots_list <- lapply(
  list(
    shap_plot_ogallala, shap_plot_colorado, shap_plot_kansas,
    shap_plot_nebraska, shap_plot_newmexico, shap_plot_oklahoma,
    shap_plot_southdakota, shap_plot_wyoming
  ), 
  adjust_plot
)

# Create a wider layout by increasing the number of columns
layout_matrix <- rbind(
  c(1, 2, 3, 4),
  c(5, 6, 7, 8)  # Increased to 4 columns instead of 3
)

SHAP <- arrangeGrob(
  grobs = shap_plots_list,
  layout_matrix = layout_matrix,  # Force wider layout
  respect = TRUE
)

# Arrange the full layout with common x-label and y-label
SHAP_plots <- grid.arrange(
  arrangeGrob(
    SHAP,
    bottom = textGrob("SHAP Value", gp = gpar(fontsize = 14)),  # Common x-axis label
    left = textGrob("Features", gp = gpar(fontsize = 14), rot = 90, x = unit(0.5, "npc"))  # Common y-axis label
  ),
  SHAPlegend,
  nrow = 2,                 
  heights = c(10, 1)  # Adjust height proportions
) 

SHAP_plots


# Save the plot with labels
ggsave(filename = "SHAP_plot_all.jpg", 
       plot = SHAP_plots, 
       width = 14, 
       height = 8, 
       dpi = 300)

#################################### Used this to get the best hyperparamter numbers for XGBoost ####################################
# 
# library(xgboost)
# library(caret)
# 
# # Define the target variable and training/testing sets
# target_var <- "Yield"
# set.seed(123)
# train_index <- createDataPartition(dataset_normalized[[target_var]], p = 0.8, list = FALSE)
# train_data <- dataset_normalized[train_index, ]
# test_data <- dataset_normalized[-train_index, ]
# 
# # Extract selected features based on mRMR results
# top_features <- colnames(dataset_normalized)[selected_features]
# X_train <- as.matrix(train_data[, top_features])
# y_train <- train_data[[target_var]]
# 
# X_test <- as.matrix(test_data[, top_features])
# y_test <- test_data[[target_var]]
# 
# # Define parameter grid for tuning
# param_grid <- expand.grid(
#   max_depth = c(3, 4, 5),          # Tree depth
#   eta = c(0.01, 0.05, 0.1),        # Learning rate
#   subsample = c(0.6, 0.8, 1),      # Sample ratio per tree
#   colsample_bytree = c(0.6, 0.8, 1) # Feature subsample ratio per tree
# )
# 
# # Initialize variable to store best performance
# best_rmse <- Inf
# best_params <- list()
# 
# # Iterate over each combination of hyperparameters
# for(i in 1:nrow(param_grid)) {
#   params <- list(
#     objective = "reg:squarederror",
#     max_depth = param_grid$max_depth[i],
#     eta = param_grid$eta[i],
#     subsample = param_grid$subsample[i],
#     colsample_bytree = param_grid$colsample_bytree[i]
#   )
#   
#   # Perform 5-fold cross-validation
#   cv_result <- xgb.cv(
#     params = params,
#     data = X_train,
#     label = y_train,
#     nrounds = 1000,              # Set a high number of rounds
#     nfold = 5,                   # 5-fold CV
#     early_stopping_rounds = 10,  # Stop if no improvement
#     verbose = 0,
#     metrics = "rmse"
#   )
#   
#   # Check if the current parameters are the best
#   mean_rmse <- min(cv_result$evaluation_log$test_rmse_mean)
#   if(mean_rmse < best_rmse) {
#     best_rmse <- mean_rmse
#     best_params <- params
#     best_nrounds <- cv_result$best_iteration  # Save best nrounds
#   }
# }
# 
# # Print best parameters and number of rounds
# cat("Best Parameters:\n")
# print(best_params)
# cat("Best RMSE from CV: ", best_rmse, "\n")
# cat("Best nrounds from CV: ", best_nrounds, "\n")
# 
# # Train final XGBoost model with best parameters and rounds
# final_model <- xgboost(
#   data = X_train,
#   label = y_train,
#   nrounds = best_nrounds,
#   params = best_params,
#   verbose = 1
# )
# 
# # Make predictions on the test set
# y_pred <- predict(final_model, newdata = X_test)
# 
# # Model evaluation
# rmse <- sqrt(mean((y_test - y_pred)^2))
# mae <- mean(abs(y_test - y_pred))
# r_squared <- 1 - (sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2))
# 
# # Number of observations (n) and number of predictors (p)
# n <- length(y_test)          # Total number of observations in the test set
# p <- length(top_features)    # Number of predictors in the GBM model
# 
# # Adjusted R-squared formula
# adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1)) / (n - p - 1)
# 
# #  Print the model performance metrics
# cat("Average RMSE across 10 folds (Cross-Validation): ", average_rmse, "\n")
# cat("RMSE on test set: ", rmse, "\n")
# cat("Mean Absolute Error on test set: ", mae, "\n")
# cat("R-squared on test set: ", r_squared, "\n")
# cat("Adjusted R-squared on test set: ", adjusted_r_squared, "\n")
# 
# 


############################# Light GBM #########################################################

# Load necessary libraries
library(caret)      # For cross-validation
library(lightgbm)   # For LightGBM

# Step 1: Define the target variable
target_var <- "Yield"

# Step 2: Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(dataset_normalized[[target_var]], p = 0.8, list = FALSE)
train_data <- dataset_normalized[train_index, ]
test_data <- dataset_normalized[-train_index, ]

# Step 3: Extract the selected features based on mRMR result
top_features <- colnames(dataset_normalized)[selected_features]
cat("Selected Features: \n", paste(top_features, collapse = ", "), "\n")

# Prepare training and testing datasets using the selected features
X_train <- as.matrix(train_data[, top_features])
y_train <- train_data[[target_var]]

X_test <- as.matrix(test_data[, top_features])
y_test <- test_data[[target_var]]

# Step 4: Set up 10-fold cross-validation
set.seed(123)
cv_folds <- createFolds(y_train, k = 10, list = TRUE)

# Store RMSE for each fold
cv_rmse <- numeric(length(cv_folds))

# Step 5: Set parameters for LightGBM
params <- list(
  objective = "regression",       # For regression tasks
  metric = "rmse",                # Evaluation metric
  learning_rate = 0.10,           # Learning rate
  max_depth = 15,                 # Maximum tree depth
  num_leaves = 50,                # Number of leaves in one tree
  bagging_fraction = 0.8,         # Randomly select part of data without resampling
  feature_fraction = 0.8          # Randomly select part of features
)

# Step 6: Perform 10-fold cross-validation
for (i in seq_along(cv_folds)) {
  # Training and validation indices
  train_indices <- cv_folds[[i]]
  
  # Split data into training and validation sets
  X_cv_train <- X_train[train_indices, ]
  y_cv_train <- y_train[train_indices]
  X_cv_val <- X_train[-train_indices, ]
  y_cv_val <- y_train[-train_indices]
  
  # Create LightGBM datasets
  dtrain <- lgb.Dataset(data = X_cv_train, label = y_cv_train)
  dval <- lgb.Dataset(data = X_cv_val, label = y_cv_val, reference = dtrain)
  
  # Train LightGBM model
  cv_model <- lgb.train(
    params = params,
    data = dtrain,
    nrounds = 1000,                # Number of boosting rounds
    valids = list(validation = dval),  # Validation dataset
    early_stopping_rounds = 50,    # Stop training if no improvement after 50 rounds
    verbose = -1                   # Suppress training output
  )
  
  # Predict on the validation set
  y_pred <- predict(cv_model, X_cv_val)
  
  # Calculate RMSE for this fold
  cv_rmse[i] <- sqrt(mean((y_cv_val - y_pred)^2))
}

# Step 7: Calculate average RMSE across folds
average_rmse <- mean(cv_rmse)

# Step 8: Train the final LightGBM model on the full training dataset
dtrain <- lgb.Dataset(data = X_train, label = y_train)

final_model <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  verbose = -1
)

# Step 9: Make predictions on the test set
y_pred_test <- predict(final_model, newdata = X_test)

# Step 10: Evaluate the model on the test set
rmse <- sqrt(mean((y_test - y_pred_test)^2))  # RMSE
mae <- mean(abs(y_test - y_pred_test))        # MAE
r_squared <- 1 - (sum((y_test - y_pred_test)^2) / sum((y_test - mean(y_test))^2))  # R-squared


# Number of observations (n) and number of predictors (p)
n <- length(y_test)          # Total number of observations in the test set
p <- length(top_features)    # Number of predictors in the GBM model

# Adjusted R-squared formula
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1)) / (n - p - 1)

# Step 11: Print the model performance metrics
cat("Average RMSE across 10 folds (Cross-Validation): ", average_rmse, "\n")
cat("RMSE on test set: ", rmse, "\n")
cat("Mean Absolute Error on test set: ", mae, "\n")
cat("R-squared on test set: ", r_squared, "\n")
cat("Adjusted R-squared on test set: ", adjusted_r_squared, "\n")


# # Step 7b: Make predictions on the training set
# y_train_pred <- predict(lgb_model, newdata = X_train)
# 
# # Step 8b: Evaluate the model on the training set
# rmse_train <- sqrt(mean((y_train - y_train_pred)^2))  # RMSE for training set
# mae_train <- mean(abs(y_train - y_train_pred))        # MAE for training set
# r_squared_train <- 1 - (sum((y_train - y_train_pred)^2) / sum((y_train - mean(y_train))^2))  # R-squared for training set
# 
# # Step 9b: Print the model performance metrics for the training set
# cat("RMSE on training set: ", rmse_train, "\n")
# cat("Mean Absolute Error on training set: ", mae_train, "\n")
# cat("R-squared on training set: ", r_squared_train, "\n")

############ Putting in the table for mapping #################
library(dplyr)

# # Add empty columns for RMSE, MAE, R-squared, and Adjusted R-squared
# dataset_with_errors_light <- dataset_with_centeroids %>%
#   mutate(
#     RMSE = NA,  # Initialize RMSE column with NA
#     MAE = NA,   # Initialize MAE column with NA
#     R_squared = NA,  # Initialize R-squared column with NA
#     Adjusted_R_squared = NA  # Initialize Adjusted R-squared column with NA
#   )

target_state <- "South Dakota"

# Add error metric values only for rows where State == "Wyoming"
dataset_with_errors_light <- dataset_with_errors_light %>%
  mutate(
    RMSE = ifelse(State == target_state, rmse, RMSE),  
    MAE = ifelse(State == target_state, mae, MAE),     
    R_squared = ifelse(State == target_state, r_squared, R_squared),  
    Adjusted_R_squared = ifelse(State == target_state, adjusted_r_squared, Adjusted_R_squared)  
  )


############################################################################
# Select only the required columns
dataset_selected <- dataset_with_errors_light %>%
  select(State, County, Yield, longitude, latitude, RMSE, MAE, R_squared, Adjusted_R_squared)

# dataset_selected <- read.csv("dataset_with_errors_lightgbm.csv")

data_fil <- dataset_selected %>%
  select(State, County, R_squared, longitude, latitude) %>%
  group_by(State, County)

data_fil$State <- tolower(data_fil$State)
data_fil$State <- stringr::str_to_title(data_fil$State)

counties <- counties(cb = TRUE)  # Load the county boundaries
states <- states(cb = TRUE)

# Define the states of interest
states_of_interest <- c("Wyoming", "Texas", "New Mexico", "Oklahoma", "Kansas", "Nebraska", "South Dakota", "Colorado")

# Filter the states data for the selected states
states_filtered <- states %>%
  filter(NAME %in% states_of_interest)

state_centroids <- states_filtered %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(State_Name = states_filtered$STUSPS)

data_filter <- data_fil %>%
  mutate(County = toupper(County))  # Convert to upper case for matching

counties <- counties %>%
  mutate(NAME = toupper(NAME))  # County names in shapefiles to uppercase

counties1 <- counties %>%
  select(STATE_NAME, NAME, geometry)

map_data <- counties1 %>%
  left_join(data_filter, by = c("STATE_NAME" = "State", "NAME" = "County"))

map_data <- map_data %>%
  filter(!is.na(R_squared))  # This will keep only rows where Avg_Yield is not NA
dataset <- as.data.frame(map_data)


######################################## For the mapping #####################################################
library(ggplot2)

Map3 <- ggplot() +
  geom_sf(data = map_data, aes(fill = R_squared), color = "NA") +  # Ensure R_squared is numeric
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 1.2) +
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 4, fontface = "bold") + 
  scale_fill_viridis_c(option = "turbo", limits = c(0, 1), direction = -1, breaks = seq(0, 1, by = 0.1)) +
  labs(title = "Light Gradient Boosting", fill = "R-squared", x = NULL, y = NULL) +
  scale_x_continuous(limits = c(-108, -96), breaks = seq(-108, -96, by = 4)) +  
  scale_y_continuous(limits = c(31, 44.5), breaks = seq(32, 44, by = 4)) +  
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1, "line"),
        legend.key.width = unit(2, "cm"),
        axis.text = element_text(size = 10, color = "black")) 

# annotation_scale(location = "bl", width_hint = 0.3) +  # Adds a scale bar
# annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering())  # Adds a north arrow

print(Map3)  # Ensure the plot is displayed


#################################################################################################################################

# Load ggplot2 for plotting
library(ggplot2)

# Create a data frame for actual and predicted values
plot_data_lgbm <- data.frame(Actual = y_test, Predicted = y_pred_test)


# Create scatter plot with regression line
lgbm_total <- ggplot(plot_data_lgbm, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) +  # Regression line
  labs(title = "Light Gradient Boosting",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face ="bold"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12))  +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +  
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  coord_fixed() +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
           label = paste("RMSE: ", format(round(rmse, 2), nsmall = 2),
                         "\nMAE: ", format(round(mae, 2), nsmall = 2),
                         "\nR-square: ", format(round(r_squared, 2), nsmall = 2)),
           size = 4, color = "black",
           box.color = "lightgray", background.color = "white") +
  # Second annotation for units (t ha^-1)
  annotate("text", x = -Inf, y = Inf, hjust = -2.7, vjust = 1.4,
           label = expression( "t ha"^{-1}), 
           size = 4, color = "black", fontface = "bold",
           box.color = "lightgray", background.color = "white") +
  annotate("text", x = -Inf, y = Inf, hjust = -2.4, vjust = 2.8,
           label = expression( "t ha"^{-1}), 
           size = 4, color = "black", fontface = "bold",
           box.color = "lightgray", background.color = "white")
lgbm_total

ggplot2::ggsave(file = "lgbm_total.jpg",
                plot = lgbm_total,
                width = 9,
                height = 6,
                dpi = 300)


##########################################Random Forest Machine Learning########################################
library(caret)
library(randomForest)


# Define the target variable
target_var <- "Yield"

# Step 1: Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(dataset_normalized[[target_var]], p = 0.8, list = FALSE)
train_data <- dataset_normalized[train_index, ]
test_data <- dataset_normalized[-train_index, ]

# Step 2: Extract the selected features based on mRMR result
top_features <- colnames(dataset_normalized)[selected_features]  # From mRMR output

# Step 3: Prepare training and testing datasets using the selected features
X_train <- train_data[, top_features]
y_train <- train_data[[target_var]]  # Using 'Yield' as the response variable

X_test <- test_data[, top_features]
y_test <- test_data[[target_var]]

# Step 4: Train the Random Forest model with 10-fold cross-validation
rf_model <- train(X_train, y_train,
                  method = "rf",
                  tuneLength = 10,  # This will tune the model for the number of trees
                  trControl = trainControl(method = "cv", number = 10, summaryFunction = defaultSummary))  # 10-fold cross-validation

# Step 5: Print the best model parameters
cat("Best Random Forest parameters:\n")
print(rf_model$bestTune)

# Step 6: Extract the RMSE from the cross-validation results
cv_results <- rf_model$resample  # Extracting the cross-validation results
average_rmse <- mean(cv_results$RMSE)  # Calculate the average RMSE across all folds

# Step 7: Print the average RMSE from cross-validation
cat("Average RMSE from cross-validation: ", average_rmse, "\n")

# Step 8: Make predictions on the test set
y_pred <- predict(rf_model, newdata = X_test)

# Step 9: Evaluate the Random Forest model on the test set
rmse_test <- sqrt(mean((y_test - y_pred)^2))  # RMSE for the test set
mae <- mean(abs(y_test - y_pred))  # MAE
r_squared <- 1 - (sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2))  # R-squared

# Number of observations (n) and number of predictors (p)
n <- length(y_test)          # Total number of observations in the test set
p <- length(top_features)    # Number of predictors in the GBM model

# Adjusted R-squared formula
adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1)) / (n - p - 1)

# Step 10: Print the model performance metrics
cat("Average RMSE across 10 folds (Cross-Validation): ", average_rmse, "\n")
cat("RMSE on test set: ", rmse, "\n")
cat("Mean Absolute Error on test set: ", mae, "\n")
cat("R-squared on test set: ", r_squared, "\n")
cat("Adjusted R-squared on test set: ", adjusted_r_squared, "\n")

############ Putting in the table for mapping #################
library(dplyr)

# # Add empty columns for RMSE, MAE, R-squared, and Adjusted R-squared
# dataset_with_errors_rf <- dataset_with_centeroids %>%
#   mutate(
#     RMSE = NA,  # Initialize RMSE column with NA
#     MAE = NA,   # Initialize MAE column with NA
#     R_squared = NA,  # Initialize R-squared column with NA
#     Adjusted_R_squared = NA  # Initialize Adjusted R-squared column with NA
#   )

target_state <- "Oklahoma"

# Add error metric values only for rows where State == "Wyoming"
dataset_with_errors_rf <- dataset_with_errors_rf %>%
  mutate(
    RMSE = ifelse(State == target_state, rmse, RMSE),  
    MAE = ifelse(State == target_state, mae, MAE),     
    R_squared = ifelse(State == target_state, r_squared, R_squared),  
    Adjusted_R_squared = ifelse(State == target_state, adjusted_r_squared, Adjusted_R_squared)  
  )


############################################################################
# Select only the required columns
dataset_selected <- dataset_with_errors_rf %>%
  select(State, County, Yield, longitude, latitude, RMSE, MAE, R_squared, Adjusted_R_squared)

# dataset_selected <- read.csv("dataset_with_errors_rf.csv")

data_fil <- dataset_selected %>%
  select(State, County, R_squared, longitude, latitude) %>%
  group_by(State, County)

data_fil$State <- tolower(data_fil$State)
data_fil$State <- stringr::str_to_title(data_fil$State)

counties <- counties(cb = TRUE)  # Load the county boundaries
states <- states(cb = TRUE)

# Define the states of interest
states_of_interest <- c("Wyoming", "Texas", "New Mexico", "Oklahoma", "Kansas", "Nebraska", "South Dakota", "Colorado")

# Filter the states data for the selected states
states_filtered <- states %>%
  filter(NAME %in% states_of_interest)

state_centroids <- states_filtered %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(State_Name = states_filtered$STUSPS)

data_filter <- data_fil %>%
  mutate(County = toupper(County))  # Convert to upper case for matching

counties <- counties %>%
  mutate(NAME = toupper(NAME))  # County names in shapefiles to uppercase

counties1 <- counties %>%
  select(STATE_NAME, NAME, geometry)

map_data <- counties1 %>%
  left_join(data_filter, by = c("STATE_NAME" = "State", "NAME" = "County"))

map_data <- map_data %>%
  filter(!is.na(R_squared))  # This will keep only rows where Avg_Yield is not NA
dataset <- as.data.frame(map_data)


######################################## For the mapping #####################################################
library(ggplot2)

Map4 <- ggplot() +
  geom_sf(data = map_data, aes(fill = R_squared), color = "NA") +  # Ensure R_squared is numeric
  geom_sf(data = states_filtered, fill = NA, color = "black", size = 1.2)+
  geom_text(data = state_centroids, aes(x = X, y = Y, label = State_Name), 
            size = 4, fontface = "bold") + 
  scale_fill_viridis_c(option = "turbo", limits = c(0, 1), direction = -1, breaks = seq(0, 1, by = 0.1)) +
  labs(title = "Random Forest", fill = "R-square", x = NULL, y = NULL) +
  scale_x_continuous(limits = c(-108, -96), breaks = seq(-108, -96, by = 4)) +  
  scale_y_continuous(limits = c(31, 44.5), breaks = seq(32, 44, by = 4)) +  
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(1, "line"),
        legend.key.width = unit(2, "cm"),
        axis.text = element_text(size = 10, color = "black")) 

# annotation_scale(location = "bl", width_hint = 0.3) +  # Adds a scale bar
# annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering())  # Adds a north arrow

print(Map4)  # Ensure the plot is displayed
###############################################################################################################################

# Load ggplot2 for plotting
library(ggplot2)

# Create a data frame for actual and predicted values
plot_data_rf <- data.frame(Actual = y_test, Predicted = y_pred)

# Create scatter plot with regression line
# First annotation for the numeric values (RMSE, MAE, and R-squared)
rf_total <- ggplot(plot_data_rf, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#191970", alpha = 0.6, size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", color = "#5D3FD3", se = FALSE) +  # Regression line
  labs(title = "Random Forest",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12))  +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +  
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  coord_fixed() +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
           label = paste("RMSE: ", format(round(rmse, 2), nsmall = 2),
                         "\nMAE: ", format(round(mae, 2), nsmall = 2),
                         "\nR-square: ", format(round(r_squared, 2), nsmall = 2)),
           size = 4, color = "black",
           box.color = "lightgray", background.color = "white") +
  # Second annotation for units (t ha^-1)
  annotate("text", x = -Inf, y = Inf, hjust = -2.7, vjust = 1.4,
           label = expression( "t ha"^{-1}), 
           size = 4, color = "black", fontface = "bold",
           box.color = "lightgray", background.color = "white") +
  annotate("text", x = -Inf, y = Inf, hjust = -2.4, vjust = 2.8,
           label = expression( "t ha"^{-1}), 
           size = 4, color = "black", fontface = "bold",
           box.color = "lightgray", background.color = "white")

rf_total



name = expression(paste("Yield (tons ", ha^{-1}, ")"))
# 
# ggplot2::ggsave(file = "rf_total.jpg",
#                 plot = rf_total,
#                 width = 9,
#                 height = 6,
#                 dpi = 300)

# # Step 6b: Make predictions on the training set
# y_train_pred <- predict(rf_model, newdata = X_train)
# 
# # Step 7b: Evaluate the Random Forest model on the training set
# rmse_train <- sqrt(mean((y_train - y_train_pred)^2))  # RMSE for training set
# mae_train <- mean(abs(y_train - y_train_pred))        # MAE for training set
# r_squared_train <- 1 - (sum((y_train - y_train_pred)^2) / sum((y_train - mean(y_train))^2))  # R-squared for training set
# 
# # Step 8b: Print the model performance on the training set
# cat("RMSE on training set: ", rmse_train, "\n")
# cat("Mean Absolute Error on training set: ", mae_train, "\n")
# cat("R-squared on training set: ", r_squared_train, "\n")

##############################################################################################################################


library(gridExtra)
library(grid)
library(gridtext)

# Remove x-axis and y-axis text for each plot
rf_total6 <- rf_total + theme(axis.title = element_blank(), axis.text = element_text(size = 12)) + coord_fixed()
gbm_total6 <- gbm_total + theme(axis.title = element_blank(), axis.text = element_text(size = 12)) + coord_fixed()
xgb_total6 <- xgb_total + theme(axis.title = element_blank(), axis.text = element_text(size = 12)) + coord_fixed()
lgbm_total6 <- lgbm_total + theme(axis.title = element_blank(), axis.text = element_text(size = 12)) + coord_fixed()

# Create an empty grob for padding
left_padding <- grid::rectGrob(gp = grid::gpar(fill = NA, col = NA))

# Create a grob for the whole layout
scatter <- grid.arrange(
  arrangeGrob(left_padding,  # Add blank space to the left
              arrangeGrob(rf_total6, gbm_total6, xgb_total6, lgbm_total6, 
                          nrow = 2),
              ncol = 2,  # Two columns: padding + main plots
              widths = c(1, 10)),  # Adjust padding width
  heights = c(10, 1)  # Adjust row heights
)

# Combine the grid plot with the text annotations
grid.newpage()  # Start a new page
grid.draw(scatter)  # Draw the main grid plot

# Add common x-axis and y-axis labels
grid.text(expression(paste("Actual yield (t ", ha^{-1},")")), 
          x = 0.55,  # Slightly adjust position to fit the new layout
          y = 0.07,  # Position near bottom
          gp = gpar(fontsize = 14))

grid.text(expression(paste("Predicted yield (t ", ha^{-1},")")), 
          x = 0.07,  # Adjust horizontal position for the new left padding
          y = 0.5,  # Position vertically in the middle
          rot = 90,  # Rotate the text for vertical label
          gp = gpar(fontsize = 14))

scatter

#To save high resolution picture
ggplot2::ggsave(file = "Scatter_ogallala.jpg", 
                plot = grid.grab(), 
                width = 8, 
                height = 8, 
                dpi = 300) 
###################################################################################################

library(ggplot2)
library(gridExtra)
library(grid)

# Function to extract the legend from a ggplot
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) {
    return(tmp$grobs[[leg]])
  } else {
    return(NULL)  # Handle cases where no legend exists
  }
}

# Extract the legend from Map4
mylegend <- g_legend(Map4 + theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 1)))

# Arrange the plots without legends
plots_grid <- arrangeGrob(
  Map4 + theme(legend.position = "none"),
  Map1 + theme(legend.position = "none"),
  Map2 + theme(legend.position = "none"),
  Map3 + theme(legend.position = "none"),
  nrow = 2,  # Keep 2x2 layout
  ncol = 2,  # Ensure it stays in 2 columns
  respect = TRUE  # Ensures plots stay aligned without extra space
)

# Arrange the full layout with legend at the bottom
R_squared_map <- grid.arrange(
  plots_grid,
  mylegend,
  nrow = 2,                 
  heights = c(10, 1)  # Adjust height proportions
) # Ensure the final plot is displayed


#To save high resolution picure
ggplot2::ggsave(file = "R_squared_map_test.jpg", 
                plot = R_squared_map, 
                width = 8, 
                height = 8, 
                dpi = 300) 
#########################################################################################################

write.csv(dataset_selected, "dataset_with_errors_GBM.csv", row.names = FALSE)
