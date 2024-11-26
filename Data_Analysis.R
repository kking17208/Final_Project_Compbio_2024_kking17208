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
setwd("/home/kking17")

yf_data <- read_excel(file_path, sheet = "YF recreational trials") %>%
  clean_names()
