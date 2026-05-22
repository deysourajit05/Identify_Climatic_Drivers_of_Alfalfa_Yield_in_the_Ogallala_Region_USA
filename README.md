
# Feature Selection and Explainable Machine Learning to Identify Climatic Drivers of Alfalfa Yield in the Ogallala Region (USA)

## Authors
**Sourajit Dey¹**, **Farshina Nazrul Shimim²**, **Jiyung Kim¹’³**, **Prasad Deshpande¹**, **Xuan Xu⁴**, **Bradley Whitaker²**, **Mahendra Bhandari⁵**, **Jamie L. Foster⁶**, **Yuri Clements Daglia Calil⁷**, **P. V. Vara Prasad¹**, **Jonathan Aguilar⁸’⁹**, **Doohong Min¹**, **Gaurav Jha¹***

---

### Affiliations
1. Department of Agronomy, Kansas State University, Manhattan, KS 66506, USA
2. Department of Electrical and Computer Engineering, Montana State University, Bozeman, MT 59717, USA
3. Forages Production Systems Division, National Institute of Animal Science, Rural Development Administration, Cheonan, Republic of Korea
4. Department of Statistics, Kansas State University, Manhattan, KS 66506, USA
5. Department of Soil and Crop Sciences, Texas A&M AgriLife Center, Corpus Christi, TX 78406, USA
6. Department of Soil and Crop Sciences, Texas A&M AgriLife Center, Beeville, TX 78102, USA
7. Department of Agricultural Economics, Texas A&M AgriLife Center, Corpus Christi, TX 78406, USA
8. Southwest Research-Extension Center, Kansas State University, Garden City, KS 67846, USA
9. Department of Biological and Agricultural Engineering, Kansas State University, Manhattan, KS 66506, USA

**\* Correspondence:** Gaurav Jha (gjha@ksu.edu)

---
## Identify_Climatic_Drivers_of_Alfalfa_Yield_in_the_Ogallala_Region_USA
## Overview
This repository contains the R code and data processing workflows for identifying key climatic drivers of alfalfa yield within the Ogallala Aquifer region using explainable machine learning and feature selection  techniques. Climatic features were collected from PRISM and GridMET from 1981 to 2018, along with county-level alfalfa yield data from USDA-NASS. 

### PRISM_data_download.R
This script is used to download PRISM climate datasets. To ensure computational stability and prevent R session crashes, data downloads should be performed in short time chunks, with a maximum of one year per download for each climate variable (e.g., tmin, tmax, precip, and so on). Processing the data year-by-year for each parameter is strongly recommended, especially when working with large spatial extents or multiple variables.

### GridMET_data_download.R
This script is used to download GridMET climate datasets. At first, the target county list is provided in the code for the manuscript. Then, solar radiation, crop evapotranspiration, relative humidity, wind speed, and so on were downloaded from 1981 to 2018 at a 5-year chunk basis. The data resolution was 4km, so zonal statistics were used to get values for each county.

### Alfalfa_Master_Final_Analysis_1981_2018.csv
This is the raw data file used in the study, which was created by merging all climate data from PRISM and GridMET with a spatial resolution of 4 km and yield data for each county under the Ogallala aquifer from USDA-NASS. In the CSV, column header with numbers 01 - 12 indicates January to December months for each variable, namely growing degree days (GDD), accumulated solar radiation, reference evapotranspiration, annual precipitation, dry days, average temperature (minimum, maximum and mean), average minimum relative humidity, average maximum relative humidity, specific humidity, mean dew point temperature, wind speed, avergae solar radiation intensity, and vapor pressure deficit (minimum, maximum and mean).

### ML_climate_alfalfa.R
This script initially adds the centroids for each county specified by latitude and longitude, which helps to make spatial maps with this data. To ensure spatial and temporal robustness, a nested five-fold stratified spatial block cross-validation with temporal holdout subsampling was used. Two modeling scenarios were considered, namely Climate-only and Full. Models used- LightGBM, GBM, XGBoost, Random Forest, Linear Regression, SVM, and ANN.

### License
- Code: MIT License
- Data: Creative Commons Attribution 4.0 (CC BY 4.0)

