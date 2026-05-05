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
library(knitr)
library(kableExtra)

# Open dataset ------------------------------------------------------------
brfss_24 <- read_xpt(here('LLCP2024.XPT '))

#to remove the annoying space do this in the terminal:
# mv "LLCP2024.XPT " "LLCP2024.XPT"

# Filtering for states ------------------------------------------------------------
states_to_exclude <- c(8, 9, 12, 17, 21, 23, 26, 29, 31, 32, 36, 39, 40, 41, 42, 48, 51, 53, 56, 66) 

brfss24_clean <- brfss_24 %>% 
  filter(!(`_STATE` %in% states_to_exclude))

table(brfss24_clean$`_STATE`)

# Tooth loss variable count for power calculation ------------------------------------------------------------
table(brfss24_clean$`RMVTETH4`, useNA = "ifany")

# Clean food insecurity ------------------------------------------------------------
  # Check counts and percentages for table 1
table(brfss24_clean$SDHFOOD1, useNA = "ifany")

x <- brfss24_clean$SDHFOOD1
food_secure <- sum(x == 5, na.rm = TRUE)
food_insecure <- sum(x %in% c(1,2,3,4), na.rm = TRUE)
missing <- sum(x %in% c(7,9) | is.na(x))
total <- length(x)
pct_secure <- food_secure / total * 100
pct_insecure <- food_insecure / total * 100
pct_missing <- missing / total * 100

  # Set new variable to NA 
brfss24_clean$food_insecurity <- NA

  # Make binary 
brfss24_clean$food_insecurity[brfss24_clean$SDHFOOD1 %in% c(1,2,3,4)] <- 1
brfss24_clean$food_insecurity[brfss24_clean$SDHFOOD1 == 5] <- 0

  # Remove Missing 
brfss24_clean <- brfss24_clean[!is.na(brfss24_clean$food_insecurity), ]

  # Make a labeled factor
brfss24_clean <- brfss24_clean %>%
  mutate(food_insec_lab = factor(food_insecurity,
                            levels = c(0,1),
                            labels = c("Food secure",
                                       "Food insecure")))

# Clean Race ------------------------------------------------------------
brfss24_clean <- brfss24_clean %>%
  mutate(imprace = factor(`_IMPRACE`,
                     levels = c(1,2,3,4,5,6),
                     labels = c("White, Non-Hispanic",
                                "Black, Non-Hispanic",
                                "Asian, Non-Hispanic",
                                "American Indian/Alaskan Native, Non-Hispanic",
                                "Hispanic",
                                "Other race, Non-Hispanic")))

# Race category for Table 1 ------------------------------------------------------------
table1(~ imprace | food_insec_lab, data = brfss24_clean)

# Clean smoking ------------------------------------------------------------
  # Make binary
brfss24_clean <- brfss24_clean %>%
  mutate(
    smoker = case_when(
      `_SMOKER3` %in% c(1,2,3) ~ 1,
      `_SMOKER3` == 4 ~ 0,
      `_SMOKER3` == 9 ~ NA))

  # Make labeled factor
brfss24_clean <- brfss24_clean %>%
  mutate(
    smoker_lab = case_when(
      smoker == 1 ~ "Smoker (current or former)",
      smoker == 0 ~ "Never smoker",
      is.na(smoker) ~ "Missing"))

# Smoking category for Table 1 ------------------------------------------------------------
table1(~ smoker_lab | food_insec_lab, data = brfss24_clean)
