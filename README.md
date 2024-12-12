# Shark Depredation Research Project
For Computational Skills for 21st Century Biologists (BIOL 5560) Final Project
Department of Life Sciences, Texas A&M University- Corpus Christi

## Project Overview
This research project will be to clean, organize and statistically analyze shark depredation data within the Gulf of Mexico Recreational Fisheries. 

This analysis is split into two r codes Water Quality and Shark Depredation. Water Quality was automated by making a for loop to create water quality profiles based on the 4 water parameters that were measured using an Exo data sonde that was deployed before fishing commences at each fishing locations. Water samples were recorded every 1 second through out the water profile and was collected only on the deployment since once the data sonde reaches the sea floor it would cause a plume of sediment potentially changing our results. 

Shark deprdation data is intended to answer two questions does the shark deterrent effect the target catch rates and how efficent is the Sharkbanz Zeppelin at decreasing depredation rates. 
My data will include water profiles, depredation behavior observations in the prescense and absence of shark deterrents , and species compositional data.

## Shark Depredation Analysis
Install packages 
```
install.packages("tidyverse")
install.packages("readxl")
install.packages("janitor")
install.packages("lubridate")
install.packages("ggpubr")
```
Load Libraries
```
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(ggpubr)
```
Set Working Directory
```
setwd("~/Final_Project_Compbio_2024_kking17208/Shark_Depredation_Data/")
```
Read Excel File 
```
yf_data <- read_excel("Shark_Depredation_Data.xlsx", sheet = "Yellowfin_Fishing") %>%
  clean_names()
```
### CPUE Control vs. Zeppelin
Clean and Prepair Data
```
yf_data <- yf_data %>%
  mutate(
    total_length = as.numeric(str_replace_all(total_length, "[^0-9.]", "")),
    scientific_name = if_else(is.na(scientific_name), "no_catch", scientific_name),
    date = str_replace_all(date, "_", "-"),
    date = as_date(date, format = "%m-%d-%Y"),
    lat = as.numeric(str_replace(lat, "\\.", "")),
    long = as.numeric(str_replace(long, "\\.", "")),
    start_time = as.numeric(start_time),
    end_time = as.numeric(end_time),
    effort_hours = (end_time - start_time) / 100,
    depredation_binary = if_else(depredation == "yes", 1, 0)
  )
```
Aggregate data by angler
```
angler_data <- yf_data %>%
  group_by(date, treatment, angler) %>%
  summarise(
    total_catches = sum(scientific_name != "no_catch"),  # Count only non-"no_catch" entries
    effort_hours = mean(effort_hours),
    .groups = "drop"
  )
```
Then aggregate data by treatment of CPUE control vs. zeppelin
```
cpue_by_treatment <- angler_data %>%
  group_by(date, treatment) %>%
  summarise(
    total_catches = sum(total_catches),
    total_effort = sum(effort_hours),
    CPUE = total_catches / total_effort,
    .groups = "drop"
  )
```
Make GLM for CPUE
```
glm_CPUE <- glm(CPUE ~ treatment, data = cpue_by_treatment, family = gaussian())
summary(glm_CPUE)
```
output
```
Call:
glm(formula = CPUE ~ treatment, family = gaussian(), data = cpue_by_treatment)
Coefficients:
             Estimate Std. Error t value Pr(>|t|)
(Intercept)    4.8141     0.4168  11.550 0.000321
treatmentzep  -0.4200     0.5894  -0.713 0.515448
                
(Intercept)  ***
treatmentzep    
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Dispersion parameter for gaussian family taken to be 0.5211284)
    Null deviance: 2.3492  on 5  degrees of freedom
Residual deviance: 2.0845  on 4  degrees of freedom
AIC: 16.684
Number of Fisher Scoring iterations: 2
```
```
#Make a box and whisker plot to visualize the output
cpue_by_treatment %>%
  ggplot(aes(x = treatment, y = CPUE, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  labs(
    title = "CPUE by Treatment",
    x = "Treatment",
    y = "CPUE (Catch Per Unit Effort)"
  ) +
  theme_minimal()
```
Box and whisker plot comparing treatments

![Box and Whiskerplot cont vs zep comparing CPUE](Shark_Depredation_Data/Box%20and%20Whisker%20plot%20cont%20vs%20zep%20CPUE.png)

Check the assumptions of a GLM (guassian) normality, homoscedasticity, and continuous Variance
```
par(mfrow = c(2, 2))
plot(glm_CPUE)
```
Assumptions Test Image

![](Check%20for%20assumptions%20CPUE%20treatments.png)

Recreate Box and Whisker Plot with the p-value
```
glm_summary <- summary(glm_CPUE)
p_value <- glm_summary$coefficients[2, 4]  # Extract the p-value for the treatment effect
ggboxplot(data = cpue_by_treatment, x = "treatment", y = "CPUE", add = "jitter") +
  annotate("text", x = 1.5, y = max(cpue_by_treatment$CPUE), label = paste0("p = ", round(p_value, 3)), size = 5) +
  labs(
    title = "CPUE Comparison by Treatment",
    x = "Treatment",
    y = "Catch Per Unit Effort"
  )
```
Output

![Add P-value to box and whisker plot](CPUE%20Box%20and20%Whisker%20Plots20%with20%P-value.png)

### Shark Deterrent Results
Using the previous steps from running the GLM for CPUE continue on to run binomial GLM to determine the difference between shark depredation rates of our two treatments. 

Start by running the GLM with our already prepared data
```
glm_dep <- glm(depredation_binary ~ treatment, data = yf_data, family = binomial(link = "logit"))
summary(glm_dep)
#output
Call:
glm(formula = depredation_binary ~ treatment, family = binomial(link = "logit"), 
    data = yf_data)
Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)    -2.6210     0.5179  -5.061 4.16e-07 ***
treatmentzep  -17.9450  2458.7600  -0.007    0.994    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Dispersion parameter for binomial family taken to be 1)
    Null deviance: 34.440  on 110  degrees of freedom
Residual deviance: 29.252  on 109  degrees of freedom
AIC: 33.252
Number of Fisher Scoring iterations: 19
```
Create predicted probabilies of having shark depredation on either treatment
```
predicted_probs <- yf_data %>%
  mutate(predicted_prob = predict(glm_dep, type = "response"))
```
Create boxplots of the predicted probabilites of the treatments control vs zeppelin
```
ggplot(predicted_probs, aes(x = treatment, y = predicted_prob, fill = treatment)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.2) +
  labs(
    title = "Predicted Probability of Depredation by Treatment",
    x = "Treatment",
    y = "Predicted Probability of Depredation"
  ) +
  theme_minimal()
```
![Predicted probability of depredation using each treatment](Predicted%20Probability%20of%20Depredation%20Occurring%20between%20treatments.png)

## Water Quality Analysis
### Prepare/ Clean Data
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
#### Create Dissolved Oxygen Profiles for each day/ location
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
#### Final water profiles can be found
[Water Quality Profiles](Shark_Depredation_Data/Water_Quality_Profiles)
