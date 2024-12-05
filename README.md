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
###Prepare/ Clean Data
Install Packages
```
install.packages("readxl")
install.packages("tidyverse")
install.packages("readr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("dplyr")
```
Load libraries
```
library(readxl)
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
library(dplyr)
```
Set Working Directory
```
setwd("~/Final_Project_Compbio_2024_kking17208/Shark_Depredation_Data/")
```
Read each Excel Sheet into R
```
sheet_names <- excel_sheets("Water_Quality_Data.xlsx")
```
Create a list of sheet names 
```
sheet_data <- list()
```
Use a for loop to clean the column names for each sheet
```
for (sheet in sheet_names) {
  data <- read_excel("Water_Quality_Data.xlsx", sheet = sheet) %>%
    clean_names(replace = c("µ" = "u"))
  sheet_data[[sheet]] <- data
  cat("Loaded sheet:", sheet, "\n")
}
```
### Extact Data and Create Water Profiles
####Create Dissolved Oxygen Profiles for each day/ location
Use a for loop to extract data from each sheet and make DO water profiles
```
for (sheet in names(sheet_data)) {
  data <- sheet_data[[sheet]]  # Get the data for this sheet
if ("depth_m" %in% names(data) && "odo_mg_l" %in% names(data)) {
    # Create a plot
    plot <- ggplot(data, aes(x = odo_mg_l, y = depth_m)) +
      geom_line() +
      scale_y_reverse() +  # Reverse y-axis for depth
      labs(
        title = paste("Dissolved Oxygen Profile -", sheet),
        x = "Dissolved Oxygen (mg/L)",
        y = "Depth (m)"
      ) +
      theme_minimal()
##Save Dissolved Oxygen Profiles##
    ggsave(filename = paste0("Water_Quality_Profiles/", sheet, "_Dissolved_Oxygen.png"),
           plot = plot, width = 8, height = 6)
    
    cat("Saved plot for sheet:", sheet, "\n")
  }
}
```
#### Create Temperature profiles for each location
Use a for loop to extract data from each sheet and make Temperature water profiles
```
for (sheet in names(sheet_data)) {
  data <- sheet_data[[sheet]]  # Get the data for this sheet
  
if ("depth_m" %in% names(data) && "temp_c" %in% names(data)) {
  # Create a plot
  plot <- ggplot(data, aes(x = temp_c, y = depth_m)) +
    geom_line() +
    scale_y_reverse() +  # Reverse y-axis for depth
    labs(
      title = paste("Temperature (C) -", sheet),
      x = "Temperature (C)",
      y = "Depth (m)"
    ) +
    theme_minimal()
##Save each temperature water profile##
ggsave(filename = paste0("Water_Quality_Profiles/", sheet, "_Temperature.png"),
         plot = plot, width = 8, height = 6)
  
  cat("Saved plot for sheet:", sheet, "\n")
}
}
```
Continue doing the same process of making for loops for all water quality profiles
#### Create Turbidity Profiles for each location
Use a for loop to turbidity data from each sheet to make turbidity plots
```
for (sheet in names(sheet_data)) {
  data <- sheet_data[[sheet]]  # Get the data for this sheet
  
  if ("depth_m" %in% names(data) && "turbidity_fnu" %in% names(data)) {
    # Create a plot
    plot <- ggplot(data, aes(x = turbidity_fnu, y = depth_m)) +
      geom_line() +
      scale_y_reverse() +  # Reverse y-axis for depth
      labs(
        title = paste("Turbidity (FNU)", sheet),
        x = "Turbidity (FNU)",
        y = "Depth (m)"
      ) +
      theme_minimal()
## Save the plots
    ggsave(filename = paste0("Water_Quality_Profiles/", sheet, "_Turbidity.png"),
           plot = plot, width = 8, height = 6)
    
    cat("Saved plot for sheet:", sheet, "\n")
  }
}
```
#### Create Salinity Water Quality Profiles
Use a for loop to extract salinity data from each sheet and make salinity plots
```
for (sheet in names(sheet_data)) {
  data <- sheet_data[[sheet]]  # Get the data for this sheet
  
  if ("depth_m" %in% names(data) && "sal_psu" %in% names(data)) {
    # Create a plot
    plot <- ggplot(data, aes(x = sal_psu, y = depth_m)) +
      geom_line() +
      scale_y_reverse() +  # Reverse y-axis for depth
      labs(
        title = paste("Salinity (PSU)", sheet),
        x = "Salinity (PSU)",
        y = "Depth (m)"
      ) +
      theme_minimal()
    ## Save the plot
    ggsave(filename = paste0("Water_Quality_Profiles/", sheet, "_Salinity.png"),
           plot = plot, width = 8, height = 6)
    
    cat("Saved plot for sheet:", sheet, "\n")
  }
}
```
