##Water Quality Data#
##install packages##
install.packages("readxl")
install.packages("tidyverse")
install.packages("readr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("rstudioapi")
##load libraries##
library(readxl)
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
library(dplyr)
library(rstudioapi)
##set working directory##
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../Final_Project_Compbio_2024_kking17208/Shark_Depredation_Data/")

##read excel sheets##
sheet_names <- excel_sheets("Water_Quality_Data.xlsx")

##Create a list of sheets##
sheet_data <- list()

##use a for loop to clean all the column names for each sheet##
for (sheet in sheet_names) {
  data <- read_excel("Water_Quality_Data.xlsx", sheet = sheet) %>%
    clean_names(replace = c("µ" = "u"))
  sheet_data[[sheet]] <- data
  cat("Loaded sheet:", sheet, "\n")
}

##Extract data from each sheet then make a water profile##
for (sheet in names(sheet_data)) {
  data <- sheet_data[[sheet]]  # Get the data for this sheet
  
if ("depth_m" %in% names(data) && "odo_mg_l" %in% names(data)) {
    # Create a plot
    plot <- ggplot(data, aes(x = odo_mg_l, y = depth_m)) +
      geom_line(color = "red") +
      scale_y_reverse() +  # Reverse y-axis for depth
      labs(
        title = paste("Dissolved Oxygen Profile -", sheet),
        x = "Dissolved Oxygen (mg/L)",
        y = "Depth (m)"
      ) +
      theme_classic()
    # Save the plot
    ggsave(filename = paste0("Water_Quality_Profiles/", sheet, "_Dissolved_Oxygen.png"),
           plot = plot, width = 8, height = 6)
    
    cat("Saved plot for sheet:", sheet, "\n")
  }
}

##Temperature Water Profiles##
for (sheet in names(sheet_data)) {
  data <- sheet_data[[sheet]]  # Get the data for this sheet
  
if ("depth_m" %in% names(data) && "temp_c" %in% names(data)) {
  # Create a plot
  plot <- ggplot(data, aes(x = temp_c, y = depth_m)) +
    geom_line(color = "red") +
    scale_y_reverse() +  # Reverse y-axis for depth
    labs(
      title = paste("Temperature (C) -", sheet),
      x = "Temperature (C)",
      y = "Depth (m)"
    ) +
    theme_classic()
  # Save the plot
  ggsave(filename = paste0("Water_Quality_Profiles/", sheet, "_Temperature.png"),
         plot = plot, width = 8, height = 6)
  
  cat("Saved plot for sheet:", sheet, "\n")
}
}

##Turbidity water profiles##
for (sheet in names(sheet_data)) {
  data <- sheet_data[[sheet]]  # Get the data for this sheet
  
  if ("depth_m" %in% names(data) && "turbidity_fnu" %in% names(data)) {
    # Create a plot
    plot <- ggplot(data, aes(x = turbidity_fnu, y = depth_m)) +
      geom_line(color = "red") +
      scale_y_reverse() +  # Reverse y-axis for depth
      labs(
        title = paste("Turbidity (FNU)", sheet),
        x = "Turbidity (FNU)",
        y = "Depth (m)"
      ) +
      theme_classic()
    # Save the plot
    ggsave(filename = paste0("Water_Quality_Profiles/", sheet, "_Turbidity.png"),
           plot = plot, width = 8, height = 6)
    
    cat("Saved plot for sheet:", sheet, "\n")
  }
}

##Salinity water profiles##
for (sheet in names(sheet_data)) {
  data <- sheet_data[[sheet]]  # Get the data for this sheet
  
  if ("depth_m" %in% names(data) && "sal_psu" %in% names(data)) {
    # Create a plot
    plot <- ggplot(data, aes(x = sal_psu, y = depth_m)) +
      geom_line(color = "red") +
      scale_y_reverse() +  # Reverse y-axis for depth
      labs(
        title = paste("Salinity (PSU)", sheet),
        x = "Salinity (PSU)",
        y = "Depth (m)"
      ) +
      theme_classic()
    # Save the plot
    ggsave(filename = paste0("Water_Quality_Profiles/", sheet, "_Salinity.png"),
           plot = plot, width = 8, height = 6)
    
    cat("Saved plot for sheet:", sheet, "\n")
  }
}

###
for (sheet in names(sheet_data)) {
  # Dynamically fetch data for the current sheet
  data <- sheet_data[[sheet]]
  
  # Check if required columns exist for all parameters
  if ("depth_m" %in% names(data)) {
    # 1. Dissolved Oxygen Plot
    if ("odo_mg_l" %in% names(data)) {
      plot_oxygen <- ggplot(data, aes(x = odo_mg_l, y = depth_m)) +
        geom_line(color = "red") +
        scale_y_reverse() +
        labs(
          title = paste("Dissolved Oxygen Profile -", sheet),
          x = "Dissolved Oxygen (mg/L)",
          y = "Depth (m)"
        ) +
        theme_classic()
      ggsave(filename = paste0("Water_Quality_Profiles/", sheet, "_Dissolved_Oxygen.png"),
             plot = plot_oxygen, width = 8, height = 6)
    }
    
    # 2. Salinity Plot
    if ("sal_psu" %in% names(data)) {
      plot_salinity <- ggplot(data, aes(x = sal_psu, y = depth_m)) +
        geom_line(color = "red") +
        scale_y_reverse() +
        labs(
          title = paste("Salinity Profile -", sheet),
          x = "Salinity (PSU)",
          y = "Depth (m)"
        ) +
        theme_classic()
      ggsave(filename = paste0("Water_Quality_Profiles/", sheet, "_Salinity.png"),
             plot = plot_salinity, width = 8, height = 6)
    }
    
    # 3. Turbidity Plot
    if ("turbidity_fnu" %in% names(data)) {
      plot_turbidity <- ggplot(data, aes(x = turbidity_fnu, y = depth_m)) +
        geom_line(color = "red") +
        scale_y_reverse() +
        labs(
          title = paste("Turbidity Profile -", sheet),
          x = "Turbidity (FNU)",
          y = "Depth (m)"
        ) +
        theme_classic()
      ggsave(filename = paste0("Water_Quality_Profiles/", sheet, "_Turbidity.png"),
             plot = plot_turbidity, width = 8, height = 6)
    }
    
    # 4. Temperature Plot
    if ("temp_c" %in% names(data)) {
      plot_temperature <- ggplot(data, aes(x = temp_c, y = depth_m)) +
        geom_line(color = "red") +
        scale_y_reverse() +
        labs(
          title = paste("Temperature Profile -", sheet),
          x = "Temperature (°C)",
          y = "Depth (m)"
        ) +
        theme_classic()
      ggsave(filename = paste0("Water_Quality_Profiles/", sheet, "_Temperature.png"),
             plot = plot_temperature, width = 8, height = 6)
    }
  }
  
  # Print progress
  cat("Processed and saved plots for sheet:", sheet, "\n")
}

