# install.packages("here")
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

#Open working directory 
#data_dir <- "~/EPI-514-Project" #updated to home directory of every user with ~

#Download dataset
#brfss_21_clean <- read.csv(file.path(data_dir, "brfss_21_clean.csv"))

# Open dataset ------------------------------------------------------------
<<<<<<< HEAD
brfss_24 <- read_xpt(here("LLCP2024.XPT "))
=======
brfss_24 <- read_xpt(here('LLCP2024.XPT '))
>>>>>>> a3269a55385a6e885fc2952cb7821a9fd55abb2c

#to remove the annoying space do this in the terminal:
# mv "LLCP2024.XPT " "LLCP2024.XPT"

# Filtering for states ------------------------------------------------------------
states_to_exclude <- c(8, 9, 12, 17, 21, 23, 26, 29, 31, 32, 36, 39, 40, 41, 42, 48, 51, 53, 56, 66) 

study_data <- brfss_24 %>% 
  filter(!(`_STATE` %in% states_to_exclude))

table(study_data$`_STATE`)

# Tooth loss variable count for power calculation ------------------------------------------------------------
table(study_data$`RMVTETH4`, useNA = "ifany")

<<<<<<< HEAD
#heyyy 
                          
=======



>>>>>>> a3269a55385a6e885fc2952cb7821a9fd55abb2c

