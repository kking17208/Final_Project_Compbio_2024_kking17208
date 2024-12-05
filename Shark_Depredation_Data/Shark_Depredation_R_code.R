##Catch Data and Depredation events##
##Install Packages##
install.packages("tidyverse")
install.packages("readxl")
install.packages("janitor")
install.packages("lubridate")
install.packages("ggpubr")
##load libraries##
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(ggpubr)
##set working directory##
setwd("~/Final_Project_Compbio_2024_kking17208/Shark_Depredation_Data/")

##read excel file##
yf_data <- read_excel("Shark_Depredation_Data.xlsx", sheet = "Yellowfin_Fishing") %>%
  clean_names()

##change data information from mm_dd_yy to yyyy-mm-dd##
##clean and transform data##
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

##aggregate data by angler##
angler_data <- yf_data %>%
  group_by(date, treatment, angler) %>%
  summarise(
    total_catches = sum(scientific_name != "no_catch"),  # Count only non-"no_catch" entries
    effort_hours = mean(effort_hours),
    .groups = "drop"
  )

##aggregate data by treatment##
cpue_by_treatment <- angler_data %>%
  group_by(date, treatment) %>%
  summarise(
    total_catches = sum(total_catches),
    total_effort = sum(effort_hours),
    CPUE = total_catches / total_effort,
    .groups = "drop"
  )
##Make GLMs ##
##CPUE Analysis##
glm_CPUE <- glm(CPUE ~ treatment, data = cpue_by_treatment, family = gaussian())
summary(glm_CPUE)

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

## check for the assumptions of normality and homoscedasticity##
par(mfrow = c(2, 2))
plot(glm_CPUE)

glm_summary <- summary(glm_CPUE)
p_value <- glm_summary$coefficients[2, 4]  # Extract the p-value for the treatment effect
ggboxplot(data = cpue_by_treatment, x = "treatment", y = "CPUE", add = "jitter") +
  annotate("text", x = 1.5, y = max(cpue_by_treatment$CPUE), label = paste0("p = ", round(p_value, 3)), size = 5) +
  labs(
    title = "CPUE Comparison by Treatment",
    x = "Treatment",
    y = "Catch Per Unit Effort"
  )

##Depredation Analysis##
glm_dep <- glm(depredation_binary ~ treatment, data = yf_data, family = binomial(link = "logit"))
summary(glm_dep)

predicted_probs <- yf_data %>%
  mutate(predicted_prob = predict(glm_dep, type = "response"))

ggplot(predicted_probs, aes(x = treatment, y = predicted_prob, fill = treatment)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.2) +
  labs(
    title = "Predicted Probability of Depredation by Treatment",
    x = "Treatment",
    y = "Predicted Probability of Depredation"
  ) +
  theme_minimal()
