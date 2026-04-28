install.packages("here")
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(dplyr)
library(tidyverse)
library(haven)
library(survey)
library(ggplot2)
library(epiR)
library(table1)
library(here) 

<<<<<<< HEAD
#Open working directory 
data_dir <- "/Users/anshupoudel/EPI-514-Project"

#Download dataset
#brfss_21_clean <- read.csv(file.path(data_dir, "brfss_21_clean.csv"))
=======
# Open dataset ------------------------------------------------------------
brfss_24 <- read_xpt(here("LLCP2024.XPT "))

# Filtering for states ------------------------------------------------------------
states_to_exclude <- c(8, 9, 12, 17, 21, 23, 26, 29, 31, 32, 36, 39, 40, 41, 42, 48, 51, 53, 56, 66) 

study_data <- brfss_24 %>% 
  filter(!(`_STATE` %in% states_to_exclude))

table(study_data$`_STATE`)

# Tooth loss variable count for power calculation ------------------------------------------------------------
table(study_data$`RMVTETH4`, useNA = "ifany")
                          
>>>>>>> a66cd0ddbb54185190b7178bf9329358fad57889
