
#load libraries
library(epiR)
library(dplyr)
library(tidyverse)

#load data

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

