#Code used to clear the console
rm(list = ls())
cat("\014")
if (!is.null(dev.list())) dev.off()

################################################################################
# Load packages
library(prism)
library(sf)
library(tigris)
library(terra)
library(lubridate)
library(dplyr) # We will use the filter() function from this package



# Set the download directory for PRISM data 
prism_set_dl_dir("C:/Users/sdey/Downloads/Work dataset/PRISM_data_testing_download") #(change the directory as needed)

# Get county boundaries for a specific states
ks_counties <- tigris::counties(state = "KS", year = 1981, class = "sf")
wy_counties <- tigris::counties(state = "WY", year = 1981, class = "sf")
co_counties <- tigris::counties(state = "CO", year = 1981, class = "sf")
sd_counties <- tigris::counties(state = "SD", year = 1981, class = "sf")
ok_counties <- tigris::counties(state = "OK", year = 1981, class = "sf")
nm_counties <- tigris::counties(state = "NM", year = 1981, class = "sf")
ne_counties <- tigris::counties(state = "NE", year = 1981, class = "sf")

# Combine the county data frames into a single sf object
# Make sure columns that are not identical but represent the same thing (like 'NAME')
# are treated consistently. st_as_sf handles this relatively well for tigris data.
states <- rbind(ks_counties, wy_counties, co_counties, sd_counties, ok_counties, nm_counties, ne_counties)

# Define a vector of the specific county names you want
desired_counties <- data.frame(NAME = c("Barber", "Barton", "Cheyenne", "Clark", "Comanche", "Decatur", "Edwards", "Ellis", "Ellsworth", "Finney", "Ford", "Gove", "Graham", "Grant", "Gray",
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
                      "Thomas", "Thurston", "Valley", "Washington", "Wayne", "Webster", "Wheeler", "York"),
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

# Create the unique identifier to use for filtering
# The separator "_" is arbitrary, any separator will work.
desired_unique_ids <- paste0(desired_counties$NAME, "_", desired_counties$STATEFP)

# Check the resulting vector
print(desired_unique_ids)


# Filter the combined sf object by both NAME and STATEFP
# Note: The `tigris` object has a `STATEFP` column.
counties <- states %>%
  dplyr::filter(paste0(NAME, "_", STATEFP) %in% desired_unique_ids)

# (Optional) Re-project the county shapefile to match PRISM's projection (NAD83)
counties <- st_transform(counties, crs = "NAD83")

# Display the filtered counties object to verify it worked
print(counties)

##################################################

# Define the date range for your analysis 
start_date <- ymd("1981-01-01")
end_date <- ymd("1981-01-10")
all_dates <- seq(start_date, end_date, by = "day")

# Create an empty data frame to store all results
daily_county_data <- data.frame(
  COUNTY = rep(counties$NAME, length(all_dates)),
  STATEFP = rep(counties$STATEFP, length(all_dates)),
  DATE = rep(all_dates, each = nrow(counties)),
  TMIN = NA # Adjust column name for your variable
)

# Loop through each date
for (i in seq_along(all_dates)) {
  current_date <- all_dates[i]
  cat("Processing date:", as.character(current_date), "\n")
  
  # Download daily data
  get_prism_dailys(
    type = "tmin", # Use "tmin" for min temperature, "tmax" for max, "ppt" for precipitation
    minDate = as.character(current_date),
    maxDate = as.character(current_date),
    keepZip = FALSE # Delete zip files to save space
  )
  
  # Get the file path for the downloaded raster
  prism_file <- prism_archive_ls()[[i]]
  
  # Check if the file was downloaded successfully
  if (is.null(prism_file)) {
    warning(paste("Could not download data for", as.character(current_date)))
    next
  }
  
  # Read the raster using terra
  daily_raster <- pd_stack(prism_file)
  
  # Perform zonal statistics
  daily_tmin <- terra::extract(
    daily_raster,
    counties,
    fun = mean,
    na.rm = TRUE,
    ID = FALSE
  )
  
  # Add results to the master data frame
  # This section assumes the order of counties remains consistent, which is typically true for tigris data
  # You may want to add logic to match by county name or FIPS code to be more robust
  daily_county_data[daily_county_data$DATE == current_date, "TMIN"] <- daily_tmin[,1]
  
  # (Optional) Clean up downloaded files
  # This is a good practice for large daily datasets
  prism_archive_clean(current_date)
}


# View the first few rows
head(daily_county_data)

# Check the structure and data types
str(daily_county_data)

