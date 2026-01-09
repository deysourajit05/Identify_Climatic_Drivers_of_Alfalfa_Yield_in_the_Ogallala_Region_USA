# Identify_Climatic_Drivers_of_Alfalfa_Yield_in_the_Ogallala_Region_USA
This is a GitHub Repository that contains data on climatic features collected from PRISM from 1981 to 2018, as well as county-level alfalfa yield data collected from NASS. 

### PRISM_data_download.R
This script is used to download PRISM climate datasets. To ensure computational stability and prevent R session crashes, data downloads should be performed in short time chunks, with a maximum of one year per download for each climate variable (e.g., tmin, tmax, precip, and so on). Processing the data year-by-year for each parameter is strongly recommended, especially when working with large spatial extents or multiple variables.

### Ogallala_alfalfa.csv
This is the raw data file used in the study, which was created by merging all climate data from PRISM with a spatial resolution of 4 km and yield data for each county under the Ogallala aquifer from USDA-NASS. In the CSV, column header with numbers 01 - 12 indicates January to December months for each variable, namely growing degree days (GDD), annual precipitation, no precipitation days,  total precipitation, average minimum temperature, average maximum temperature, dew point temperature, and vapor pressure deficit (both minimum and maximum).

### Main_codes_ogallala_aquifer.R
This script initially adds the centroids for each county specified by latitude and longitude, which helps to make spatial maps with this data. Descriptive statistics were eventually done with the dataset. Eventually, some self-explanatory graphs are created for the paper, which are mentioned in the script. Then, for the mRMR ranking data, it is prepared starting from line 892. Codes for plots related to mRMR are also provided. The code chunk from 1113 to 1227 shows how the number of features subset were selected based on the RMSE value. This chunk also includes the plotting of the same.  Then, the effectiveness of the subset selected by mRMR for alfalfa yield estimation was checked by some tree-based models, namely, Gradient boosting, extreme gradient boosting, Light gradient boosting, and Random Forest machine learning models. 
Within each model, with its heading in chunks (e.g., line 1229), after the model is run, all the error metrics are compiled into a table to create a map based on the error matrix. Finally, scatter plots were created comparing actual yield values to estimated yield values for alfalfa.
SHAP was executed for the XGBoost model, which is in the chunk from 1839 to 1928, including the plots created from SHAP analysis.
