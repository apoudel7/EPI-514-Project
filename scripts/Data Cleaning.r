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

# Cleaning variable names ------------------------------------------------------------
names(brfss24_clean)
names(brfss24_clean) <- sub("^_", "", names(brfss24_clean))
names(brfss24_clean)
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

# Clean tooth loss ----------------------------------------------------------------
# Make binary
brfss24_clean <- brfss24_clean %>%
  mutate(
    tooth_loss = case_when(
      RMVTETH4 %in% c(1,2,3) ~ 1,
      RMVTETH4 == 8 ~ 0,
      RMVTETH4 %in% c(7,9) ~ NA_real_))

# Make labeled factor
brfss24_clean <- brfss24_clean %>%
  mutate(
    tooth_loss_lab = factor(
      tooth_loss,
      levels = c(0, 1),
      labels = c("No Tooth Loss", "Tooth Loss")))
# Clean race ------------------------------------------------------------
  # Old Race variable
# brfss24_clean <- brfss24_clean %>%
#   mutate(imprace = factor(`_IMPRACE`,
#                      levels = c(1,2,3,4,5,6),
#                      labels = c("White*",
#                                 "Black*",
#                                 "Asian*",
#                                 "American Indian/Alaskan Native*",
#                                 "Hispanic",
#                                 "Another race*")))

  # New Race variable 
brfss24_clean <- brfss24_clean %>%
                  mutate(race = factor(RACE,
                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                  labels = c(
                    "White only*",
                    "Black only*",
                    "American Indian/Alaska Native only*",
                    "Asian only*",
                    "Native Hawaiian/Other Pacific Islander only*",
                    "Another race only*",
                    "Multiracial*",
                    "Hispanic",
                    "Missing")))


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
# Clean age ------------------------------------------------------------
# Make binary
brfss24_clean <- brfss24_clean %>%
  mutate(
    age_binary = case_when(
      AGE65YR == 1 ~ "18–64",
      AGE65YR == 2 ~ "65+",
      AGE65YR == 3 ~ NA_character_)) %>%
  filter(!is.na(age_binary)) %>%
  mutate(
    age_binary = factor(age_binary, levels = c("18–64", "65+")))

# Make labeled factor
brfss24_clean$age_cat <- case_when(
  brfss24_clean$AGEG5YR %in% c(1,2,3) ~ "18-34", 
  brfss24_clean$AGEG5YR %in% c(4,5) ~ "35-44", 
  brfss24_clean$AGEG5YR %in% c(6,7) ~ "45-54",
  brfss24_clean$AGEG5YR %in% c(8,9) ~ "55-64",
  brfss24_clean$AGEG5YR %in% c(10,11) ~ "65-74", 
  brfss24_clean$AGEG5YR %in% c(12,13) ~ "75+",
  brfss24_clean$AGEG5YR == 14 ~ "Missing", 
  TRUE ~ "Missing"  
) %>% 
  factor(levels = c("18-34", 
                    "35-44", 
                    "45-54", 
                    "55-64", 
                    "65-74", 
                    "75+", 
                    "Missing")) 

# Clean sex ----------------------------------------------------------------
brfss24_clean$sex_lab <- factor(brfss24_clean$SEXVAR, levels = c(1,2),
                                labels = c("Male","Female"))

# Clean smoking ------------------------------------------------------------
  # Make binary
brfss24_clean <- brfss24_clean %>%
  mutate(
    smoker = case_when(
      SMOKER3 %in% c(1,2,3) ~ 1,
      SMOKER3 == 4 ~ 0,
      SMOKER3 == 9 ~ NA))

  # Make labeled factor
brfss24_clean <- brfss24_clean %>%
  mutate(
    smoker_lab = case_when(
      smoker == 1 ~ "Smoker (current or former)",
      smoker == 0 ~ "Never smoker",
      is.na(smoker) ~ "Missing"),
    smoker_lab = factor(smoker_lab, 
                        levels = c("Smoker (current or former)", "Never smoker")))

# Clean income --------------------------------------------------------------------
brfss24_clean <- brfss24_clean %>% 
  mutate(
    income_lab = case_when(
      INCOMG1 == 1 ~ 1, 
      INCOMG1 %in% c(2, 3, 4) ~ 2, 
      INCOMG1 == 5 ~ 3, 
      INCOMG1 == 6 ~ 4, 
      INCOMG1 == 7 ~ 5,
      INCOMG1 == 9 ~ NA_real_
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

# Create Table 1 ----------------------------------------------------------------
  # Create labels for table 1
label(brfss24_clean$race)   <- "Race/Ethnicity"
label(brfss24_clean$age_cat)      <- "Age (years)"
label(brfss24_clean$sex_lab)       <- "Sex"
label(brfss24_clean$smoker_lab)    <- "Smoking"
label(brfss24_clean$income_lab) <- "Income (USD)¹"
label(brfss24_clean$dent_visit_lab)      <- "Dental Visit (visit in the past year)"

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
  ~ race + age_cat + sex_lab + smoker_lab + income_lab + dent_visit_lab | food_insec_lab,
  data = brfss24_clean,
  overall = "Total N(%)",
  render.continuous = my.render.cont,
  render.categorical = my.render.cat,
  caption = "Table 1: Characteristics of BRFSS respondents who completed the Social Determinants module in 2024, total and disaggregated by food security status (N=238783)",
  footnote = c(
  "* Non-Hispanic",
  "¹ Annual Household Income from all sources. Category groupings informed by Pew Research Center and U.S. Census Bureau data classifications",
  "Footnote: Missing data for the exposure variable (food insecurity) was N = 37243 (15.6%); these respondents were included in the missing category. All data is unweighted."))

# Create Table 2A ----------------------------------------------------------------
# Tooth loos prevalence % for exposed vs. unexposed 
table2a <- brfss24_clean %>%
  group_by(food_insec_lab) %>%
  summarise(
    n = n(),
    tooth_loss_prev = paste0(round(mean(tooth_loss_lab == "Tooth Loss", na.rm = TRUE) * 100, 1), "%"),
    .groups = "drop")
table2a

# Tooth loss prevalence % for tooth loss by race/ethnicity and exposure status 
table2a_1 <- brfss24_clean %>%
  filter(
    !is.na(race) & race != "Missing",
    !is.na(food_insec_lab),
    !is.na(tooth_loss_lab)) %>%
  group_by(race, food_insec_lab) %>%
  summarise(
    n = n(),
    tooth_loss_prev = mean(tooth_loss_lab == "Tooth Loss") * 100,
    .groups = "drop")
table2a_1

# Tooth loss prevalence % by dental visit and exposure status
table2a_2 <- brfss24_clean %>%
  filter(
    !is.na(dent_visit_lab),
    !is.na(food_insec_lab),
    !is.na(tooth_loss_lab)) %>%
  group_by(dent_visit_lab, food_insec_lab) %>%
  summarise(
    n = n(),
    tooth_loss_prev = mean(tooth_loss_lab == "Tooth Loss") * 100,
    .groups = "drop")
table2a_2 

# Check effect modification ----------------------------------------------------------------
  # Race/Ethnicity
tab_race <- xtabs(~ food_insecurity + tooth_loss + race, data = brfss24_clean)   # Create 3-way table
race_res <- epi.2by2(tab_race, method = "cross.sectional")   # Run MH
race_res$massoc.detail$PR.strata.wald   # View stratum specific PR

# Dental Visit (Excluding "Missing" level)
tab_dent <- xtabs(~ food_insecurity + tooth_loss + dent_visit_lab, 
                  data = subset(brfss24_clean, dent_visit_lab != "Missing"))
dent_res <- epi.2by2(tab_dent, method = "cross.sectional")
dent_res$massoc.detail$PR.strata.wald

  # Dental Visit
tab_dent <- xtabs(~ food_insecurity + tooth_loss + dent_visit_lab, data = brfss24_clean)   # Create the 3-way table
dent_res <- epi.2by2(tab_dent, method = "cross.sectional")   # Run MH
dent_res$massoc.detail$PR.strata.wald   # View stratum specific PR

# Check confounding ----------------------------------------------------------------
# Age 
tab_age <- xtabs(~ food_insecurity + tooth_loss + age_binary, 
                 data = subset(brfss24_clean, !is.na(age_binary)))
epi.2by2(tab_age, method = "cross.sectional")

# Sex
tab_sex <- xtabs(~ food_insecurity + tooth_loss + sex_lab, 
                 data = subset(brfss24_clean, !is.na(sex_lab)))
epi.2by2(tab_sex, method = "cross.sectional")

# Smoking
tab_smoke <- xtabs(~ food_insecurity + tooth_loss + smoker_lab, 
                   data = subset(brfss24_clean, smoker_lab != "Missing") %>% droplevels())
epi.2by2(tab_smoke, method = "cross.sectional")


# Income
tab_inc <- xtabs(~ food_insecurity + tooth_loss + income_lab, 
                 data = subset(brfss24_clean, income_lab != "Missing") %>% droplevels())
epi.2by2(tab_inc, method = "cross.sectional")
