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
  mutate(food_insec_lab = factor(
    food_insecurity,
    levels = c(1, 0),
    labels = c("Food Insecure N(%)",
               "Food Secure N(%)")))

# Clean Race ------------------------------------------------------------
brfss24_clean <- brfss24_clean %>%
  mutate(imprace = factor(`_IMPRACE`,
                     levels = c(1,2,3,4,5,6),
                     labels = c("White*",
                                "Black*",
                                "Asian*",
                                "American Indian/Alaskan Native*",
                                "Hispanic",
                                "Another race*")))

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
      is.na(smoker) ~ "Missing"),
    smoker_lab = factor(smoker_lab, 
                        levels = c("Smoker (current or former)", "Never smoker")))

# Clean age ------------------------------------------------------------
brfss24_clean$age_cat <- case_when(
  brfss24_clean$`_AGEG5YR` %in% c(1,2,3) ~ "18-34", 
  brfss24_clean$`_AGEG5YR` %in% c(4,5) ~ "35-44", 
  brfss24_clean$`_AGEG5YR` %in% c(6,7) ~ "45-54",
  brfss24_clean$`_AGEG5YR` %in% c(8,9) ~ "55-64",
  brfss24_clean$`_AGEG5YR` %in% c(10,11) ~ "65-74", 
  brfss24_clean$`_AGEG5YR` %in% c(12,13) ~ "75+",
  brfss24_clean$`_AGEG5YR` == 14 ~ "Missing", 
  TRUE ~ "Missing"  
) %>% 
  factor(levels = c("18-34", 
                    "35-44", 
                    "45-54", 
                    "55-64", 
                    "65-74", 
                    "75+", 
                    "Missing")) #label 

# Cleaning variable names ------------------------------------------------------------
names(brfss24_clean)
names(brfss24_clean) <- sub("^_", "", names(brfss24_clean))
names(brfss24_clean)

# Clean income --------------------------------------------------------------------
brfss24_clean <- brfss24_clean %>% 
  mutate(
    income_lab = case_when(
      INCOMG1 == 1 ~ 1, 
      INCOMG1 %in% c(2, 3, 4) ~ 2, 
      INCOMG1 == 5 ~ 3, 
      INCOMG1 == 6 ~ 4, 
      INCOMG1 == 7 ~ 5,
      INCOMG1 == 9 ~ NA,
    ),
    income_lab = factor(
      income_lab,
      levels = 1:5,
      labels = c(
        "Below $15,000",
        "$15,000 - $50,000",
        "$50,000 - $100,000",
        "$100,000 - $200,000",
        "$200,000+")))

# Clean dental visit ----------------------------------------------------------------
brfss24_clean <- brfss24_clean %>%
  mutate(
    dent_visit_lab = case_when(
      DENVST3 == 1 ~ 1, 
      DENVST3 == 2 ~ 2, 
      DENVST3 == 9 ~ NA), 
    dent_visit_lab = factor(
      dent_visit_lab,
      levels = 1:2,
      labels = c("Yes","No")
    ))

# Clean sex ----------------------------------------------------------------
brfss24_clean$sex_lab <- factor(brfss24_clean$SEXVAR, levels = c(1,2),
                                labels = c("Male","Female"))

# Create labels for table 1 ----------------------------------------------------------------
label(brfss24_clean$imprace)   <- "Race/Ethnicity"
label(brfss24_clean$age_cat)      <- "Age (years)"
label(brfss24_clean$sex_lab)       <- "Sex"
label(brfss24_clean$smoker_lab)    <- "Smoking"
label(brfss24_clean$income_lab) <- "Income (USD)¹"
label(brfss24_clean$dent_visit_lab)      <- "Dental Visit (visit in the past year)"

# Create Table 1 ----------------------------------------------------------------
  # To remove % sign from missing 
brfss24_clean <- brfss24_clean %>%
  mutate(
    age_cat = forcats::fct_explicit_na(age_cat, na_level = "Missing"),
    smoker_lab = forcats::fct_explicit_na(smoker_lab, na_level = "Missing"),
    income_lab = forcats::fct_explicit_na(income_lab, na_level = "Missing"),
    dent_visit_lab = forcats::fct_explicit_na(dent_visit_lab, na_level = "Missing"))

  # To remove % sign from continuous variables
my.render.cont <- function(x) {
  s <- stats.default(x)
  s <- stats.apply.rounding(s)
  c("", "Mean (SD)" = paste0(s$MEAN, " (", s$SD, ")"))
}

  # To remove % sign from categorical variables
my.render.cat <- function(x) {
  y <- stats.default(x)
  c(
    "",
    sapply(y, function(z)
      paste0(z$FREQ, " (", round(z$PCT, 2), ")")))}

  # Table 1 final
table1(
  ~ imprace + age_cat + sex_lab + smoker_lab + income_lab + dent_visit_lab | food_insec_lab,
  data = brfss24_clean,
  overall = "Total N(%)",
  render.continuous = my.render.cont,
  render.categorical = my.render.cat,
  caption = "Table 1: Characteristics of BRFSS respondents who completed the Social Determinants module in 2024, total and disaggregated by food security status (N=238783)",
  footnote = c(
  "* Non-Hispanic",
  "¹ ",
  "Footnote: Missing data for the exposure variable (food insecurity) was N = 37243 (15.6%); these respondents were included in the missing category. All data is unweighted."))
  
  