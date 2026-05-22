
# 1. Install 'remotes' from CRAN
install.packages("remotes")

# 2. Install 'AOI' and 'climateR' from 

remotes::install_github("mikejohnson51/AOI")
remotes::install_github("mikejohnson51/climateR")


# 1. LOAD LIBRARIES
library(climateR)
library(sf)
library(terra)
library(dplyr)
library(tigris)
library(tidyr)

# 2. GET THE GEOGRAPHY (Simplified)
# List of states you need based on your FIPS: KS(20), WY(56), CO(08), SD(46), OK(40), NM(35), NE(31)
state_list <- c("KS", "WY", "CO", "SD", "OK", "NM", "NE")

# Download all counties for these states at once
all_counties <- tigris::counties(state = state_list, year = 2018, class = "sf")

# Define your target list (Paste your NAME/STATEFP data frame here)
desired_counties <- data.frame(
  NAME = c("Barber", "Barton", "Cheyenne", "Clark", "Comanche", "Decatur", "Edwards", "Ellis", "Ellsworth", "Finney", "Ford", "Gove", "Graham", "Grant", "Gray",
           "Greeley", "Hamilton", "Harper", "Harvey", "Haskell", "Hodgeman", "Jewell", "Kearny", "Kingman", "Kiowa", "Lane", "Logan", "Marion", "McPherson",
           "Meade", "Morton", "Ness", "Norton", "Pawnee", "Phillips", "Pratt", "Rawlins", "Reno", "Republic", "Rice", "Rooks", "Rush", "Scott", "Sedgwick", "Seward",
           "Sheridan", "Sherman", "Smith", "Stafford", "Stanton", "Stevens", "Sumner", "Thomas", "Trego", "Wallace", "Wichita", "Albany", "Converse", "Goshen", "Laramie",
           "Niobrara", "Platte", "Baca", "Bent", "Cheyenne", "Elbert", "Kiowa", "Kit Carson", "Larimer", "Las Animas", "Lincoln", "Logan", "Phillips", "Prowers", "Sedgwick",
           "Washington", "Weld", "Yuma", "Bennett", "Gregory", "Jackson", "Mellette", "Oglala Lakota", "Todd", "Tripp", "Beaver", "Beckham", "Cimarron", "Dewey",
           "Ellis", "Harper", "Roger Mills", "Texas", "Woodward", "Chaves", "Curry", "De Baca", "Eddy", "Guadalupe", "Harding", "Lea", "Quay", "Roosevelt", "Union",
           "Adams", "Antelope", "Arthur", "Banner", "Blaine", "Boone", "Box Butte", "Boyd", "Brown", "Buffalo", "Burt", "Butler", "Cedar", "Chase", "Cherry", "Cheyenne",
           "Clay", "Colfax", "Cuming", "Custer", "Dakota", "Dawes", "Dawson", "Deuel", "Dixon", "Dodge", "Douglas", "Dundy", "Fillmore", "Franklin", "Frontier", "Furnas",
           "Gage", "Garden", "Garfield", "Gosper", "Grant", "Greeley", "Hall", "Hamilton", "Harlan", "Hayes", "Hitchcock", "Holt", "Hooker", "Howard", "Jefferson", "Kearney",
           "Keith", "Keya Paha", "Kimball", "Knox", "Lincoln", "Logan", "Loup", "Madison", "McPherson", "Merrick", "Morrill", "Nance", "Nuckolls", "Perkins", "Phelps", 
           "Pierce", "Platte", "Polk", "Red Willow", "Rock", "Saline", "Sarpy", "Saunders", "Scotts Bluff", "Seward", "Sheridan", "Sherman", "Sioux", "Stanton", "Thayer",
           "Thomas", "Thurston", "Valley", "Washington", "Wayne", "Webster", "Wheeler", "York"), # ... (keep your full list here)
  STATEFP = c("20", "20", "20", "20", "20", "20", "20", "20", "20", "20",
              "20", "20", "20", "20", "20", "20", "20", "20", "20", "20",
              "20", "20", "20", "20", "20", "20", "20", "20", "20", "20",
              "20", "20", "20", "20", "20", "20", "20", "20", "20", "20",
              "20", "20", "20", "20", "20", "20", "20", "20", "20", "20",
              "20", "20", "20", "20", "20", "20",
              "56", "56", "56", "56", "56", "56", "08", "08", "08", "08", "08", "08", "08", "08", "08", "08", "08", "08", "08", "08", "08", "08",
              "46", "46", "46", "46", "46", "46", "46", "40", "40", "40", "40", "40", "40", "40", "40", "40", "35", "35", "35", "35", "35", "35",
              "35", "35", "35", "35",
              "31", "31", "31", "31", "31", "31", "31", "31", "31", "31",
              "31", "31", "31", "31", "31", "31", "31", "31", "31", "31",
              "31", "31", "31", "31", "31", "31", "31", "31", "31", "31",
              "31", "31", "31", "31", "31", "31", "31", "31", "31", "31",
              "31", "31", "31", "31", "31", "31", "31", "31", "31", "31",
              "31", "31", "31", "31", "31", "31", "31", "31", "31", "31",
              "31", "31", "31", "31", "31", "31", "31", "31", "31", "31",
              "31", "31", "31", "31", "31", "31", "31", "31", "31", "31",
              "31", "31", "31", "31", "31", "31"
              
  ))

# Filter the spatial object to match only your specific counties
counties_sf <- all_counties %>%
  filter(paste0(NAME, "_", STATEFP) %in% paste0(desired_counties$NAME, "_", desired_counties$STATEFP)) %>%
  st_transform(4326) # Required for climateR

########################## SOLAR RAD #################################
# 1. SETUP CHUNKS
years <- 1981:2018
year_chunks <- split(years, ceiling(seq_along(years)/5))
final_results_list <- list()

# 2. DOWNLOAD & EXTRACT LOOP
for(i in seq_along(year_chunks)){
  current_yrs <- year_chunks[[i]]
  s_date <- paste0(min(current_yrs), "-01-01")
  e_date <- paste0(max(current_yrs), "-12-31")
  
  cat("--- Processing Solar Block:", s_date, "to", e_date, "---\n")
  
  temp_list <- getGridMET(AOI = counties_sf, varname = "srad", 
                          startDate = s_date, endDate = e_date)
  
  temp_raster <- terra::rast(temp_list[]) 
  extracted <- terra::extract(temp_raster, terra::vect(counties_sf), fun = mean, na.rm = TRUE)
  
  # --- THE FAIL-SAFE DATE FIX ---
  # 1. Generate the exact dates for this chunk
  chunk_dates <- seq(as.Date(s_date), as.Date(e_date), by = "day")
  num_days <- length(chunk_dates)
  
  # 2. Pivot and assign dates by POSITION, not by parsing the column name
  chunk_tidy <- extracted %>%
    pivot_longer(cols = -ID, names_to = "VAR_NAME", values_to = "SOLAR_WM2") %>%
    group_by(ID) %>% 
    mutate(
      # This creates a perfect 1, 2, 3... sequence for EACH county (ID)
      # so it matches chunk_dates perfectly
      row_idx = row_number(), 
      DATE = chunk_dates[row_idx],
      NAME = counties_sf$NAME[ID],
      STATEFP = counties_sf$STATEFP[ID]
    ) %>%
    ungroup() %>%
    select(NAME, STATEFP, DATE, SOLAR_WM2)
  
  final_results_list[[i]] <- chunk_tidy
  
  rm(temp_list, temp_raster, chunk_tidy)
  gc()
}

# 2. AGGREGATE TO MONTHLY
library(dplyr)
library(lubridate)

# 3. MERGE AND AGGREGATE
solar_df <- bind_rows(final_results_list)

# 1. Map State FIPS
state_map <- data.frame(
  STATEFP = c("20", "56", "08", "46", "40", "35", "31"),
  STATE_NAME = c("Kansas", "Wyoming", "Colorado", "South Dakota", "Oklahoma", "New Mexico", "Nebraska")
)

solar_data_monthly <- solar_df %>%
  mutate(
    MONTH_YEAR = floor_date(as.Date(DATE), "month"),
    DAILY_MJ_m2 = (SOLAR_WM2 * 86400) / 1e6
  ) %>%
  group_by(NAME, STATEFP, MONTH_YEAR) %>%
  summarize(
    AVG_SOLAR_WM2 = mean(SOLAR_WM2, na.rm = TRUE),
    ACCUM_SOLAR_MJ_m2 = sum(DAILY_MJ_m2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(state_map, by = "STATEFP")

write.csv(solar_data_monthly, "Solar_Monthly_Final_Fixed.csv", row.names = FALSE)



################## Downloadinf ETr ############
# 3. DOWNLOAD & EXTRACT ETR (5-year chunks)
years <- 1981:2018
year_chunks <- split(years, ceiling(seq_along(years)/5))
final_results_list <- list()

for(i in seq_along(year_chunks)){
  current_yrs <- year_chunks[[i]]
  cat("--- Processing ETR Block:", min(current_yrs), "to", max(current_yrs), "---\n")
  
  # Fetch data link from gridMET
  temp_list <- getGridMET(
    AOI = counties_sf, 
    varname = "etr", 
    startDate = paste0(min(current_yrs), "-01-01"), 
    endDate = paste0(max(current_yrs), "-12-31")
  )
  
  # FIX: Convert the "glue" path/link into an actual SpatRaster object
  temp_raster <- terra::rast(temp_list[]) 
  
  # Extract Mean daily ET for each county polygon
  extracted <- terra::extract(
    temp_raster, 
    terra::vect(counties_sf), 
    fun = mean, 
    na.rm = TRUE
  )
  
  # Add identifiers
  extracted$NAME <- counties_sf$NAME
  extracted$STATEFP <- counties_sf$STATEFP
  
  final_results_list[[i]] <- extracted
  
  # Clean up memory
  rm(temp_list, temp_raster)
  gc()
}

final_daily_etr <- bind_rows(final_results_list) %>%
  pivot_longer(cols = starts_with("etr"), names_to = "DATE", values_to = "ETR_MM") %>%
  mutate(DATE = gsub("etr_", "", DATE))
saveRDS(final_daily_etr, "County_ETR_Daily_RAW_1981_2018.rds")

# 4. AGGREGATE TO MONTHLY (Total mm per month)
# Note: For ET and Precip, we SUM the daily values for the month
final_monthly_etr <- final_daily_etr %>%
  mutate(
    DATE = as.Date(DATE),
    MONTH_YEAR = floor_date(DATE, "month")
  ) %>%
  group_by(NAME, STATEFP, MONTH_YEAR) %>%
  summarize(TOTAL_ETR_MM = sum(ETR_MM, na.rm = TRUE), .groups = "drop")

write.csv(final_monthly_etr, "County_ETR_Monthly_1981_2018.csv", row.names = FALSE)

####################### Tmin, Tmax, and GDD ##########################################

# 3. CORRECTED DOWNLOAD & EXTRACTION LOOP
years <- 1981:2018
year_chunks <- split(years, ceiling(seq_along(years)/5))
final_results_list <- list()

for(i in seq_along(year_chunks)){
  current_yrs <- year_chunks[[i]]
  s_date <- paste0(min(current_yrs), "-01-01")
  e_date <- paste0(max(current_yrs), "-12-31")
  
  cat("--- Processing Block:", s_date, "to", e_date, "---\n")
  
  # Fetch both Min and Max Temp
  temp_list <- getGridMET(
    AOI = counties_sf, 
    varname = c("tmmn", "tmmx"), 
    startDate = s_date, 
    endDate = e_date
  )
  
  # Convert to SpatRaster
  temp_raster <- terra::rast(temp_list[]) 
  
  # Extract Mean
  extracted <- terra::extract(temp_raster, terra::vect(counties_sf), fun = mean, na.rm = TRUE)
  
  # --- THE CRITICAL DATE FIX ---
  # Create a date sequence for THIS specific 5-year block
  chunk_dates <- seq(as.Date(s_date), as.Date(e_date), by = "day")
  
  # Pivot and Label this chunk immediately
  chunk_tidy <- extracted %>%
    pivot_longer(
      cols = contains("temperature"), 
      names_to = "VAR_NAME", 
      values_to = "TEMP_K"
    ) %>%
    mutate(
      VARIABLE = ifelse(grepl("minimum", VAR_NAME), "TMIN", "TMAX"),
      day_index = as.numeric(str_extract(VAR_NAME, "\\d+$")),
      DATE = chunk_dates[day_index], # Maps 1 to s_date, 2 to s_date+1, etc.
      TEMP_C = TEMP_K - 273.15,
      # Attach county names using the ID row from extraction
      NAME = counties_sf$NAME[ID],
      STATEFP = counties_sf$STATEFP[ID]
    ) %>%
    select(NAME, STATEFP, DATE, VARIABLE, TEMP_C) %>%
    pivot_wider(names_from = VARIABLE, values_from = TEMP_C)
  
  final_results_list[[i]] <- chunk_tidy
  
  # Clear memory
  rm(temp_list, temp_raster, chunk_tidy)
  gc()
}

# 4. FINAL MERGE AND SAVE
# Combining the tidy chunks (which now all have valid dates)
final_temp_data <- bind_rows(final_results_list) %>%
  mutate(DAILY_GDD = pmax(((TMAX + TMIN) / 2) - 5, 0))

# Save RAW Daily RDS
saveRDS(final_temp_data, "Daily_Temps_GDD_RAW_1981_2018.rds")

# 5. AGGREGATE TO MONTHLY
state_map <- data.frame(
  STATEFP = c("20", "56", "08", "46", "40", "35", "31"),
  STATE_NAME = c("Kansas", "Wyoming", "Colorado", "South Dakota", "Oklahoma", "New Mexico", "Nebraska")
)

final_monthly_temp <- final_temp_data %>%
  mutate(MONTH_YEAR = floor_date(DATE, "month")) %>%
  group_by(NAME, STATEFP, MONTH_YEAR) %>%
  summarize(
    AVG_TMAX_C = mean(TMAX, na.rm = TRUE),
    AVG_TMIN_C = mean(TMIN, na.rm = TRUE),
    TOTAL_GDD = sum(DAILY_GDD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(state_map, by = "STATEFP")

# Save Monthly CSV
write.csv(final_monthly_temp, "Monthly_Temps_GDD_1981_2018.csv", row.names = FALSE)

##################### HUMIDTY #############################

# 1. SETUP VARIABLES
# rmin = Min Relative Humidity (%), rmax = Max Relative Humidity (%)
# sph = Specific Humidity (kg/kg)
humidity_vars <- c("rmin", "rmax", "sph")

# 2. DOWNLOAD & EXTRACT (5-year chunks)
years <- 1981:2018
year_chunks <- split(years, ceiling(seq_along(years)/5))
final_humidity_list <- list()

for(i in seq_along(year_chunks)){
  current_yrs <- year_chunks[[i]]
  s_date <- paste0(min(current_yrs), "-01-01")
  e_date <- paste0(max(current_yrs), "-12-31")
  
  cat("--- Processing Humidity Block:", s_date, "to", e_date, "---\n")
  
  # Fetch data for all three humidity variables
  temp_list <- getGridMET(
    AOI = counties_sf, 
    varname = humidity_vars, 
    startDate = s_date, 
    endDate = e_date
  )
  
  # Convert to SpatRaster and Extract
  temp_raster <- terra::rast(temp_list[]) 
  extracted <- terra::extract(temp_raster, terra::vect(counties_sf), fun = mean, na.rm = TRUE)
  
  # --- DATE FIX FOR HUMIDITY ---
  chunk_dates <- seq(as.Date(s_date), as.Date(e_date), by = "day")
  
  chunk_tidy <- extracted %>%
    pivot_longer(
      cols = matches("relative|specific"), 
      names_to = "VAR_NAME", 
      values_to = "VALUE"
    ) %>%
    mutate(
      VARIABLE = case_when(
        grepl("minimum", VAR_NAME) ~ "RH_MIN",
        grepl("maximum", VAR_NAME) ~ "RH_MAX",
        grepl("specific", VAR_NAME) ~ "SPEC_HUM"
      ),
      day_index = as.numeric(str_extract(VAR_NAME, "\\d+$")),
      DATE = chunk_dates[day_index],
      NAME = counties_sf$NAME[ID],
      STATEFP = counties_sf$STATEFP[ID]
    ) %>%
    select(NAME, STATEFP, DATE, VARIABLE, VALUE) %>%
    pivot_wider(names_from = VARIABLE, values_from = VALUE)
  
  final_humidity_list[[i]] <- chunk_tidy
  
  rm(temp_list, temp_raster, chunk_tidy)
  gc()
}

# 3. MERGE AND SAVE
final_humidity_data <- bind_rows(final_humidity_list)
saveRDS(final_humidity_data, "Alfalfa_Daily_Humidity_RAW_1981_2018.rds")

# 4. AGGREGATE TO MONTHLY AVERAGES
final_monthly_humidity <- final_humidity_data %>%
  mutate(MONTH_YEAR = floor_date(DATE, "month")) %>%
  group_by(NAME, STATEFP, MONTH_YEAR) %>%
  summarize(
    AVG_RH_MIN = mean(RH_MIN, na.rm = TRUE),
    AVG_RH_MAX = mean(RH_MAX, na.rm = TRUE),
    AVG_SPEC_HUM = mean(SPEC_HUM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(state_map, by = "STATEFP")

write.csv(final_monthly_humidity, "Monthly_Humidity_1981_2018.csv", row.names = FALSE)

################ PRECIP, VPD and WIND SPEED ####################################

# 1. SETUP VARIABLES
# vs = Wind Speed (m/s), vpd = Vapor Pressure Deficit (kPa), pr = Precipitation (mm)
target_vars <- c("vs", "vpd", "pr")

# 2. DOWNLOAD & EXTRACT (5-year chunks)
years <- 1981:2018
year_chunks <- split(years, ceiling(seq_along(years)/5))
final_results_list <- list()

for(i in seq_along(year_chunks)){
  current_yrs <- year_chunks[[i]]
  s_date <- paste0(min(current_yrs), "-01-01")
  e_date <- paste0(max(current_yrs), "-12-31")
  
  cat("--- Processing Block:", s_date, "to", e_date, "---\n")
  
  # Fetch data
  temp_list <- getGridMET(
    AOI = counties_sf, 
    varname = target_vars, 
    startDate = s_date, 
    endDate = e_date
  )
  
  # Convert to SpatRaster and Extract
  temp_raster <- terra::rast(temp_list[]) 
  extracted <- terra::extract(temp_raster, terra::vect(counties_sf), fun = mean, na.rm = TRUE)
  
  # --- DATE FIX FOR THIS BLOCK ---
  chunk_dates <- seq(as.Date(s_date), as.Date(e_date), by = "day")
  
  chunk_tidy <- extracted %>%
    pivot_longer(
      cols = matches("wind|vapor|precipitation"), 
      names_to = "VAR_NAME", 
      values_to = "VALUE"
    ) %>%
    mutate(
      VARIABLE = case_when(
        grepl("wind", VAR_NAME) ~ "WIND_SPEED",
        grepl("vapor", VAR_NAME) ~ "VPD",
        grepl("precipitation", VAR_NAME) ~ "PRECIP"
      ),
      day_index = as.numeric(str_extract(VAR_NAME, "\\d+$")),
      DATE = chunk_dates[day_index],
      NAME = counties_sf$NAME[ID],
      STATEFP = counties_sf$STATEFP[ID]
    ) %>%
    select(NAME, STATEFP, DATE, VARIABLE, VALUE) %>%
    pivot_wider(names_from = VARIABLE, values_from = VALUE)
  
  final_results_list[[i]] <- chunk_tidy
  
  # Clean memory
  rm(temp_list, temp_raster, chunk_tidy)
  gc()
}

# 3. MERGE AND SAVE RAW DAILY
final_daily_precip <- bind_rows(final_results_list)
saveRDS(final_daily_precip, "Alfalfa_Daily_Wind_VPD_Pr_RAW_1981_2018.rds")

# 4. AGGREGATE TO MONTHLY
# Note: PRECIP is summed, WIND and VPD are averaged
# 4. AGGREGATE TO MONTHLY
# Note: PRECIP is summed, WIND and VPD are averaged, DRY_DAYS are summed
final_monthly_precip <- final_daily_precip %>%
  mutate(
    MONTH_YEAR = floor_date(DATE, "month"),
    # Create a flag: 1 if precip is 0, otherwise 0
    is_dry = ifelse(PRECIP == 0, 1, 0)
  ) %>%
  group_by(NAME, STATEFP, MONTH_YEAR) %>%
  summarize(
    TOTAL_PRECIP_MM = sum(PRECIP, na.rm = TRUE),
    AVG_WIND_SPEED_MS = mean(WIND_SPEED, na.rm = TRUE),
    AVG_VPD_KPA = mean(VPD, na.rm = TRUE),
    # Sum the flags to get total dry days in the month
    DRY_DAYS_COUNT = sum(is_dry, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(state_map, by = "STATEFP")

# 5. SAVE CSV
write.csv(final_monthly_precip, "Monthly_Wind_VPD_Pr_DryDays_1981_2018.csv", row.names = FALSE)


################ MASTER FILE #################################################

library(dplyr)
library(lubridate)

# 1. FORCE STATEFP TO CHARACTER IN ALL TABLES
# This prevents the join from failing
final_monthly_temp     <- final_monthly_temp     %>% mutate(STATEFP = as.character(STATEFP), MONTH_YEAR = as.Date(MONTH_YEAR))
final_monthly_humidity <- final_monthly_humidity %>% mutate(STATEFP = as.character(STATEFP), MONTH_YEAR = as.Date(MONTH_YEAR))
final_monthly_etr      <- final_monthly_etr      %>% mutate(STATEFP = as.character(STATEFP), MONTH_YEAR = as.Date(MONTH_YEAR))
final_monthly_precip   <- final_monthly_precip   %>% mutate(STATEFP = as.character(STATEFP), MONTH_YEAR = as.Date(MONTH_YEAR))
final_monthly_solar    <- solar_data_monthly    %>% mutate(STATEFP = as.character(STATEFP), MONTH_YEAR = as.Date(MONTH_YEAR))

# 2. SEQUENTIAL JOIN (Keys only)
master_climate <- final_monthly_temp %>%
  select(-any_of(c("STATE_NAME", "STATE_NAME_FULL"))) %>% 
  full_join(select(final_monthly_humidity, -any_of(c("STATE_NAME", "STATE_NAME_FULL"))), by = c("STATEFP", "NAME", "MONTH_YEAR")) %>%
  full_join(select(final_monthly_etr,      -any_of(c("STATE_NAME", "STATE_NAME_FULL"))), by = c("STATEFP", "NAME", "MONTH_YEAR")) %>%
  full_join(select(final_monthly_precip,   -any_of(c("STATE_NAME", "STATE_NAME_FULL"))), by = c("STATEFP", "NAME", "MONTH_YEAR")) %>%
  full_join(select(final_monthly_solar,    -any_of(c("STATE_NAME", "STATE_NAME_FULL"))), by = c("STATEFP", "NAME", "MONTH_YEAR"))

# 3. DEFINE MAP AND JOIN (Forcing Character here too)
state_map <- data.frame(
  STATEFP = c("20", "56", "08", "46", "40", "35", "31"),
  STATE_NAME = c("Kansas", "Wyoming", "Colorado", "South Dakota", "Oklahoma", "New Mexico", "Nebraska"),
  stringsAsFactors = FALSE
)

# Join and Organize
master_climate <- master_climate %>%
  left_join(state_map, by = "STATEFP") %>%
  select(STATE_NAME, STATEFP, NAME, MONTH_YEAR, everything())

# 4. SAVE
write.csv(master_climate, "Alfalfa_Master_Climate_Long.csv", row.names = FALSE)

# VERIFY: Check if STATE_NAME has any NAs
cat("NAs in STATE_NAME:", sum(is.na(master_climate$STATE_NAME)), "\n")


##############################################################################################

library(dplyr)
library(tidyr)
library(lubridate)

# 1. Prepare for widening and RENAME variables to your preferred names
master_wide_prep <- master_climate %>%
  mutate(
    YEAR = year(MONTH_YEAR),
    MONTH = sprintf("%02d", month(MONTH_YEAR)),
    STATE_NAME = toupper(STATE_NAME),
    NAME = toupper(NAME)
  ) %>%
  # RENAME HERE: New_Name = Old_Name
  rename(
    GDD = TOTAL_GDD,
    ACCUM_SOLAR_RAD = ACCUM_SOLAR_MJ_m2,
    ETR = TOTAL_ETR_MM,
    PRECIP = TOTAL_PRECIP_MM,
    DRY_DAYS = DRY_DAYS_COUNT,
    TMAX = AVG_TMAX_C,
    TMIN = AVG_TMIN_C,
    RH_MIN = AVG_RH_MIN,
    RH_MAX = AVG_RH_MAX,
    SPEC_HUM = AVG_SPEC_HUM,
    VPD_MEAN = AVG_VPD_KPA,
    WIND_SPEED = AVG_WIND_SPEED_MS,
    SOLAR_RAD = AVG_SOLAR_WM2
  ) %>%
  select(-MONTH_YEAR)

# 2. Pivot to Wide Format
# Now the 'values_from' uses your NEW short names
master_wide_final <- master_wide_prep %>%
  pivot_wider(
    id_cols = c(STATE_NAME, STATEFP, NAME, YEAR),
    names_from = MONTH,
    values_from = c(
      "GDD", "ACCUM_SOLAR_RAD", "ETR", "PRECIP", "DRY_DAYS", 
      "TMAX", "TMIN", "RH_MIN", "RH_MAX", "SPEC_HUM", 
      "VPD_MEAN", "WIND_SPEED", "SOLAR_RAD"
    ),
    names_glue = "{.value}_{MONTH}"
  )

# 3. Final Column Renaming and Sorting
master_wide_final <- master_wide_final %>%
  rename(
    Year = YEAR,
    State = STATE_NAME,
    State.ANSI = STATEFP,
    County = NAME
  ) %>%
  arrange(State, County, Year)