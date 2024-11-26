##install all necessary packages##
install.packages("tidyverse")
install.packages("readxl")
install.packages("janitor")
install.packages("lubridate")

##load libraries##
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

##set working directory##
setwd("~/OneDrive/Desktop/Shark_Depredation_Data/")

##read excel file##
yf_data <- read_excel("Shark_Depredation_Data.xlsx", sheet = "Yellowfin_Fishing") %>%
  clean_names()

##change data information from mm_dd_yy to yyyy-mm-dd##
yf_data <- yf_data %>%
  filter(!is.na(total_length) & total_length != "unknown")

yf_data <- yf_data %>%
  mutate(date = str_replace_all(date, "_", "-"),
         date = as_date(date, format = "%m-%d-%Y"),
         lat = as.numeric(str_replace(lat, "\\.", "")),  # Remove unexpected extra periods
         long = as.numeric(str_replace(long, "\\.", "")),
         total_length = as.numeric(total_length),
         depredation = as.factor(depredation),
         across(c(common_name, scientific_name, depredated_species, predatory_species, notes), as.character),
         across(c(start_time, end_time), as.character)
  )

##histogram of red snapper sizes
yf_data %>%
  filter(common_name == "red_snapper") %>%  # Filter for Red Snapper
  ggplot(aes(x = total_length)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Red Snapper Total Length",
    x = "Fish Length (mm)", # Adjust unit if necessary
    y = "Frequency"
  ) +
  theme_minimal()

##histogram with normal distribution 
yf_data %>%
  filter(common_name == "red_snapper") %>%  # Filter for Red Snapper
  ggplot(aes(x = total_length)) +
  geom_histogram(
    aes(y = ..density..),  # Use density for the y-axis to match the normal distribution scale
    binwidth = 5, 
    fill = "blue", 
    color = "black", 
    alpha = 0.7
  ) +
  stat_function(
    fun = dnorm,  # Add the normal distribution line
    args = list(
      mean = mean(filter(yf_data, common_name == "red_snapper")$total_length, na.rm = TRUE),  # Mean of Red Snapper lengths
      sd = sd(filter(yf_data, common_name == "red_snapper")$total_length, na.rm = TRUE)      # Standard deviation
    ),
    color = "red",
    size = 1
  ) +
  labs(
    title = "Histogram of Red Snapper Fish Lengths with Normal Distribution",
    x = "Fish Length (mm)",  # Adjust unit if necessary
    y = "Density"
  ) +
  theme_minimal()

##pie chart of species composition
## count all species##
species_counts <- yf_data %>%
  count(common_name, name = "count") %>%
  mutate(percent = round(100 * count / sum(count), 1)) %>%
  arrange(desc(count))

species_counts %>%
  ggplot(aes(x = "", y = count, fill = common_name)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +  # Convert to a pie chart
  geom_text(
    aes(
      label = ifelse(percent > 10, paste0(percent, "%"), ""),  # Show labels only if > 5%
    ),
    size = 4
  ) +
  labs(
    title = "Species Composition",
    fill = "Species"
  ) +
  theme_void() +  # Remove unnecessary gridlines and axes
  theme(legend.position = "right") 
