# Shark Depredation Research Project
For Computational Skills for 21st Century Biologists (BIOL 5560) Final Project
Department of Life Sciences, Texas A&M University- Corpus Christi

##Project Overview
This research project will be to clean, organize and statistically analyze shark depredation data within the Gulf of Mexico Recreational Fisheries. 

This analysis is split into two r codes Water Quality and Shark Depredation. Water Quality was automated by making a for loop to create water quality profiles based on the 4 water parameters that were measured using an Exo data sonde that was deployed before fishing commences at each fishing locations. Water samples were recorded every 1 second through out the water profile and was collected only on the deployment since once the data sonde reaches the sea floor it would cause a plume of sediment potentially changing our results. 

Shark deprdation data is intended to answer two questions does the shark deterrent effect the target catch rates and how efficent is the Sharkbanz Zeppelin at decreasing depredation rates. 
My data will include water profiles, depredation behavior observations in the prescense and absence of shark deterrents , and species compositional data.

## Shark Depredation Analysis
### CPUE Control vs. Zeppelin
## Water Quality Analysis
### Water Quality Profiles
Install Packages
'code'
##install packages##
install.packages("readxl")
install.packages("tidyverse")
install.packages("readr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("dplyr")
