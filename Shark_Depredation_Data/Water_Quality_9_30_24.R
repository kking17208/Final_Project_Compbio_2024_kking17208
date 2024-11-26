##Water Quality Data 9/30##
##load tidyverse##
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
library(dplyr)
##set working directory##
setwd("~/OneDrive/Documents/")

##reade excel file into R##
water_quality <- read_csv("Kor Measurement File Export - 112524 085335.csv", locale = locale(encoding = "ISO-8859-1"))

##Clean the names of each column##
water_quality <- water_quality %>%
  clean_names(replace = c("\u00b5" = "u"))


##prepare data##
depth_column <- "depth_m"  # Replace with the actual column name
temperature_column <- "temp_c"  # Replace with the actual column name

water_quality_clean <- water_quality %>%
  select(depth = !!sym(depth_column), temperature = !!sym(temperature_column)) %>%
  filter(!is.na(depth) & !is.na(temperature))

water_quality_clean <- water_quality_clean %>%
  group_by(depth) %>%
  summarize(temperature = mean(temperature, na.rm = TRUE))

##make Depth vs Temperature Water Profile##
ggplot(water_quality_clean, aes(x = temperature, y = depth)) +
  geom_line(color = "red", size = 1) +
  labs(
    title = "Water Profile",
    x = "Temperature (°C)",
    y = "Depth (m)"
  ) +
  scale_y_reverse() +
  scale_x_reverse() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

##make depth vs dissolved oxygen##
water_quality_depth_do <- water_quality %>%
  select(depth = depth_m, dissolved_oxygen = odo_mg_l)

ggplot(water_quality_depth_do, aes(x = dissolved_oxygen, y = depth)) +
  geom_line(color = "red", size = 1) +
  labs(
    title = "Water Profile",
    x = "Dissolved Oxygen (mg/l)",
    y = "Depth (m)"
  ) +
  scale_y_reverse() +
  scale_x_reverse() +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
