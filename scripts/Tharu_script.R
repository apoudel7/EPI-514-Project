#Tharu analysis 

#Data cleaning ---------
#clear the global environment 
rm(list = ls())


# Load libraries -------------
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
brfss_24 <- read_xpt(here("LLCP2024.XPT"))

#to remove the annoying space do this in the terminal:
# mv "LLCP2024.XPT " "LLCP2024.XPT"

# Filtering for states ------------------------------------------------------------
states_to_exclude <- c(8, 9, 12, 17, 21, 23, 26, 29, 31, 32, 36, 39, 40, 41, 42, 48, 51, 53, 56, 66) 

study_data <- brfss_24 %>% 
  filter(!(`_STATE` %in% states_to_exclude))

table(study_data$`_STATE`)

# Tooth loss variable count for power calculation ------------------------------
table(study_data$`RMVTETH4`, useNA = "ifany")

#Power analysis ---------------


#filter data to select states

# Fixed inputs
n_total <- 201540 # total sample size need to change for our study
n_unexposed <-  159988
n_exposed <- 41552
ratio_m <- n_unexposed / n_exposed # ratio of unexposed to exposed need to change for our study 

# P0 values range from calculation of prevalence of outcome from dataset 
p0_range <- c(0.40, 0.43, 0.46, 0.49, 0.52)

# Function to calculate MDPR for each P0
calculate_mdor <- function(p0_val) {
  result <- epi.sscc(
    OR = NA,
    p0 = p0_val,
    n = n_total,
    power = 0.80,
    r = ratio_m,
    method = "unmatched"
  )
  mdor <- result$OR
  # Convert OR -> PR
  mdpr <- mdor / ((1 - p0_val) + (p0_val * mdor))
  return(mdpr)
}

# Generate the results
cat("--- Power Analysis Results ---\n")
for(p in p0_range) {
  mdor_result <- calculate_mdor(p)
  cat(paste0("If P0 is ", p*100, "%, MDOR is: ", round(mdor_result, 3), "\n"))
}



