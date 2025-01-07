library(MatchIt)
library(tidyverse)
library(sf)
library(lmtest)
library(fixest)
library(here)
library(RColorBrewer)

# libraries for covariate testing
library(cobalt)
library(tableone)
library(kableExtra)
#library for Sant'Anna and Zhou (2020)
library(DRDID)
# library for Callaway and Sant'Anna (2021)
library(did)

library(splines)


rm(list=ls())

# 0. Set seed and parameters -----

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small", or "combined"
data_type <- "combined"

set.seed(123456L)
# set number of nearest neighbors for KNN matching
knn_neighbors <- 1

# Notes: With CDD large, KNN = 2 seems to work well
# with the smaller dataset, KNN = 3 work

# define directories
merged_data_dir <- here("data", "derived", "merged", data_type)
match_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset")

map_output_dir <- here("output", "figures", "matched_did", data_type)
balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)
holc_data_dir <- here("data", "derived", "holc")

map_dir <- here("output", "figures", "matched_did", data_type, "with_rings")
results_dir <- here("output", "regression_results", "matched_did", data_type)
balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)


# define file paths
tract_with_treatment_status_filepath <-
  here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")
treated_tracts_panel_filepath <-
  here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")

event_study_rings_filepath <-
  here(merged_data_dir, "unique_tracts_and_rings.csv")

# read census data with treatment status
census_tract_sample_raw <-
  st_read(tract_with_treatment_status_filepath)

# read tract tracts panel
treated_tracts_panel_raw <-
  st_read(treated_tracts_panel_filepath) %>% 
  st_drop_geometry()

# read in unique rings and tracts
# These are the tracts that are treated, the ring of tracts touching those, and the ring of tracts
# touching the inner ring tracts
tracts_and_rings <-
  read_csv(event_study_rings_filepath) %>% 
   #try excluding outer
  filter(location_type != "outer")

# get datasets for regressions
tract_data_matched_1_year <- read_csv(here(match_data_dir, "tract_data_matched_1_year.csv"))
tract_data_matched_2_year <- read_csv(here(match_data_dir, "tract_data_matched_2_year.csv"))

# tract_data_matched_1_year <- read_csv(here(match_data_dir, "tract_data_matched_1_year_genetic.csv"))
# tract_data_matched_2_year <- read_csv(here(match_data_dir, "tract_data_matched_2_year_genetic.csv"))

# Run regressions ----
# outcome variables
outcome_variables <- c("asinh_private_population_estimate", "private_population_estimate",
                       "black_share", "white_share",
                       "total_pop", "black_pop", "white_pop",
                       "asinh_pop_total", "asinh_pop_black",
                       "asinh_pop_white",
                        "median_income", "asinh_median_income",
                       "median_home_value_calculated", "asinh_median_home_value_calculated", "median_home_value_reported",
                      "median_rent_calculated",  "asinh_median_rent_calculated",
                       "population_density", 
                      "housing_density", "total_units", "vacancy_rate", "high_skill_share",
                      "lfp_rate", "unemp_rate", 
                       "pct_hs_grad", "pct_some_college", "median_educ_years_25plus",
                      "high_skill_share", "low_skill_share")


## 4. Estimate DiD, Two Period, TWFE -----
# For some reason, did variable is getting dropped due to collinearity now...
# Dont need to do this for now
# did_estimation <- function(data, outcome_var) {
#   
#   data <-
#     data %>%
#     # keep only treated units and matched controls
#     filter(!is.na(match_group)) %>%
#     group_by(match_group) %>%
#     # create a post variable and a Post X treated variable
#     mutate(
#       group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
#       post = ifelse(year >= group_treatment_year, 1, 0),
#       did = treatment_group * post
#     ) %>%
#     ungroup()
#   
#   formula <- as.formula(paste(outcome_var, "~ treated + post + did | year^city + tract_id + match_group"))
#   
#   model <- feols(formula, data = data, weights = data$weights,
#                  cluster = "tract_id")
#   
#   return(model)
# }
# 
# 
# # initialize an empty list to store results
# did_results_2_period <- list()
# 
# 
# for (outcome in outcome_variables) {
#   did_results_2_period[[outcome]] <- did_estimation(tract_data_matched, outcome)
# }
# 
# did_results_2_period



## 5. Estimate Event studies: TWFE and Sun and Abraham (2020)-----

did_event_study <- function(input_data, outcome_var, treatment_group,
                            # heterogeneity filters
                            size = NULL, city_filter = NULL, initial_share = NULL) {
  
  
  # For testing 
  input_data <- tract_data_matched_2_year
  outcome_var <- "black_share"
  treatment_group <- "treated"
  size = NULL
  city_filter = NULL
  initial_share = NULL
   
  data <- input_data
  
  # Filter for size category if specified
  if (!is.null(size)) {
    data <- data %>%
      filter(size_group == size)
  }
  
  # Filter for city if specified
  if (!is.null(city_filter)) {
    data <- data %>%
      filter(city == city_filter)
  }
  
  # filter for initial share if specified
  if (!is.null(initial_share)) {
    data <- data %>%
      filter(race_group == initial_share)
  }
  
  
  data <- data %>%
    # keep only treated units and matched controls
    filter(!is.na(match_group)) %>%
    # keep only the chosen group and controls affiliated with it
    filter(group_type == treatment_group) %>% 
    group_by(match_group) %>%
    mutate(
      group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
      event_time = year - group_treatment_year) %>% 
    ungroup() %>% 
    mutate(treated = ifelse(location_type == treatment_group, 1, 0)) %>% 
    # remove -50 and +50 event times, sample size gets too small
    filter(event_time > -40, event_time < 50)
  
  
  # calculate pre-period mean of variable
  pre_period_mean <-
    data %>%
    filter(event_time == -10) %>%
    summarize(mean_outcome = mean(!!sym(outcome_var), na.rm = TRUE)) %>%
    pull(mean_outcome)
  
  
  # Create the formula for the event study
  # baseline model
  # Note: city-year does nothing in this model with matched FE
  formula <- as.formula(paste(outcome_var, "~ i(event_time, treated, ref = -10) | cbsa_title^year + tract_id +  match_group^event_time"))
  model <- feols(formula, data = data, weights = ~weights, cluster = ~tract_id)
  model
  
  # Model without matching
  formula_no_match <- as.formula(paste(outcome_var, "~ i(event_time, treated, ref = -10) | year^cbsa_title + tract_id"))
  model_no_match <- feols(formula_no_match, data = data, weights = ~weights, cluster = ~tract_id)
  model_no_match
  
  # Sun and Abraham model, with matching
  sunab_data <- 
    data %>% 
    # set treatment_year = 100 if missing
    mutate(treatment_year = ifelse(is.na(treatment_year), 100, treatment_year))
  
  formula_sunab <- as.formula(paste(outcome_var, "~ sunab(treatment_year, year, ref.p = -10) | cbsa_title^year + tract_id + match_group^year"))
  model_sunab <- feols(formula_sunab, data = sunab_data, weights = ~weights, cluster = ~tract_id)
  model_sunab
  
  # sun and abraham model without matching
  formula_sunab_no_match <- as.formula(paste(outcome_var, "~ sunab(treatment_year, year, ref.p = -10) | cbsa_title^year + tract_id"))
  model_sunab_no_match <- feols(formula_sunab_no_match, data = sunab_data, weights = ~weights, cluster = ~tract_id)
  model_sunab_no_match
  
  return(list(twfe = model, sunab = model_sunab, sunab_no_match = model_sunab_no_match))
}

# Function to standardize the "term" format between TWFE and Sun and Abraham
standardize_terms <- function(term, model_type) {
  if (model_type == "TWFE") {
    # For TWFE, remove "treated" and just keep event_time value
    return(gsub("event_time::|:treated", "", term))
  } else  {
    # For Sun and Abraham, rename "year" to "event_time" to match TWFE
    return(gsub("year::", "", term))  # This step is just to ensure consistency
  }
  return(term)
}

# function to save model results in df
save_estimates <- function(model_list, estimator_name) {
  map_dfr(names(model_list), function(outcome_var_name) {
    tidy(model_list[[outcome_var_name]]) %>%
      mutate(outcome = outcome_var_name,
             estimator = estimator_name,
             term = standardize_terms(term, estimator_name)) %>% 
      mutate_if(is.numeric, round, 4) 
  })
}

group_types <- c("treated", "inner")

## Full sample -----
# initialize lists
did_results_event_study_twfe <- list() 
did_results_event_study_sunab <- list() 
did_results_event_study_sunab_no_match <- list()


# save regression output to lists
for (outcome in outcome_variables) {
  for (group in  group_types) {
  
    # display which outcome and group is being worked on
    print(paste("Outcome:", outcome, "Group:", group))
    
    results <- did_event_study(tract_data_matched_2_year,
                               outcome, group)
    did_results_event_study_twfe[[paste(outcome, group, sep = "_")]] <- results$twfe
    did_results_event_study_sunab[[paste(outcome, group, sep = "_")]] <- results$sunab
    did_results_event_study_sunab_no_match[[paste(outcome, group, sep = "_")]] <- results$sunab_no_match
  }
}

# save results in df
event_study_twfe_coefs <- save_estimates(did_results_event_study_twfe, "TWFE")
event_study_sunab_coefs <- save_estimates(did_results_event_study_sunab, "Sun-Abraham")
event_study_sunab_no_match_coefs <- save_estimates(did_results_event_study_sunab_no_match, "Sun-Abraham without matching")

combined_results <- 
  bind_rows(event_study_twfe_coefs, event_study_sunab_coefs,
            event_study_sunab_no_match_coefs)

# look at significant pretrends
combined_results %>% filter(str_detect(term, "-20"), p.value < .05) %>% View()

# look at significant post trends
combined_results %>% filter(p.value < .05) %>% View()



### ATT summary ----

# black share
summary(did_results_event_study_sunab$black_share_treated, agg = "ATT")
summary(did_results_event_study_sunab$black_share_inner, agg = "ATT")

# asinh total population
summary(did_results_event_study_sunab$asinh_pop_total_treated, agg = "ATT")
summary(did_results_event_study_sunab$asinh_pop_total_inner, agg = "ATT")

# asinh private pop
summary(did_results_event_study_sunab$asinh_private_population_estimate_treated, agg = "ATT")

# black population
summary(did_results_event_study_sunab$asinh_pop_black_treated, agg = "ATT")
summary(did_results_event_study_sunab$asinh_pop_black_inner, agg = "ATT")
summary(did_results_event_study_sunab$black_pop_treated, agg = "ATT")
summary(did_results_event_study_sunab$black_pop_inner, agg = "ATT")

# white population
summary(did_results_event_study_sunab$asinh_pop_white_treated, agg = "ATT")
summary(did_results_event_study_sunab$asinh_pop_white_inner, agg = "ATT")
summary(did_results_event_study_sunab$white_pop_treated, agg = "ATT")
summary(did_results_event_study_sunab$white_pop_inner, agg = "ATT")

# median income 
summary(did_results_event_study_sunab$median_income_treated, agg = "ATT")
summary(did_results_event_study_sunab$median_income_inner, agg = "ATT")

# asinh median income
summary(did_results_event_study_sunab$asinh_median_income_treated, agg = "ATT")
summary(did_results_event_study_sunab$asinh_median_income_inner, agg = "ATT")

# median home value
summary(did_results_event_study_sunab$median_home_value_calculated_treated, agg = "ATT")
summary(did_results_event_study_sunab$median_home_value_calculated_inner, agg = "ATT")

# asinh home value
summary(did_results_event_study_sunab$asinh_median_home_value_calculated_treated, agg = "ATT")
summary(did_results_event_study_sunab$asinh_median_home_value_calculated_inner, agg = "ATT")

# median rent
summary(did_results_event_study_sunab$median_rent_calculated_treated, agg = "ATT")
summary(did_results_event_study_sunab$median_rent_calculated_inner, agg = "ATT")

# asinh_median rent
summary(did_results_event_study_sunab$asinh_median_rent_calculated_treated, agg = "ATT")
summary(did_results_event_study_sunab$asinh_median_rent_calculated_inner, agg = "ATT")

# population density
summary(did_results_event_study_sunab$population_density_treated, agg = "ATT")
summary(did_results_event_study_sunab$population_density_inner, agg = "ATT")

# LFP
summary(did_results_event_study_sunab$lfp_rate_treated, agg = "ATT")
summary(did_results_event_study_sunab$lfp_rate_inner, agg = "ATT")

# Unemployment rate
summary(did_results_event_study_sunab$unemp_rate_treated, agg = "ATT")
summary(did_results_event_study_sunab$unemp_rate_inner, agg = "ATT")

# PCT high school grad
summary(did_results_event_study_sunab$pct_hs_grad_treated, agg = "ATT")
summary(did_results_event_study_sunab$pct_hs_grad_inner, agg = "ATT")

# high skill share
summary(did_results_event_study_sunab$high_skill_share_treated, agg = "ATT")
summary(did_results_event_study_sunab$high_skill_share_inner, agg = "ATT")


# Heterogeneity analysis ----


## By Project size -----

### Treated  -----

# Get only the treated + their donor pool
tract_data_matched_2_year_treated <- 
  tract_data_matched_2_year %>%
  filter(group_type == "treated")

# Get the size group for the treated units
size_groups <- tract_data_matched_2_year_treated %>% 
  select(match_group, total_public_housing_units) %>% 
  filter(!is.na(total_public_housing_units)) %>% 
  distinct() %>% 
  # assign quartiles
  mutate(size_group = ntile(total_public_housing_units, 3)) %>% 
  select(-total_public_housing_units)

# Merge onto the treated units
tract_data_matched_with_size_treated <- 
  tract_data_matched_2_year_treated %>% 
  left_join(size_groups, by = "match_group")

# Initialize lists
did_results_event_study_twfe_size_treated <- list()
did_results_event_study_sunab_size_treated <- list()
did_results_event_study_sunab_no_match_size_treated <- list()


# Loop over outcome variables and size categories
for (outcome in outcome_variables) {
  for (s in 1:max(tract_data_matched_with_size_treated$size_group, na.rm = TRUE)) {


      results <- did_event_study(input_data = tract_data_matched_with_size_treated,
                                 outcome_var = outcome,
                                treatment_group = "treated",
                                size = s)
      did_results_event_study_twfe_size_treated[[paste(outcome, s, sep = "_")]] <- results$twfe
      did_results_event_study_sunab_size_treated[[paste(outcome, s,  sep = "_")]] <- results$sunab
      did_results_event_study_sunab_no_match_size_treated[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
  }
}

# save estimates in df
event_study_twfe_size_coefs_treated <- save_estimates(did_results_event_study_twfe_size_treated, "TWFE")
event_study_sunab_size_coefs_treated <- save_estimates(did_results_event_study_sunab_size_treated, "Sun-Abraham")
event_study_sunab_no_match_size_coefs_treated <- save_estimates(did_results_event_study_sunab_no_match_size_treated,
                                                                "Sun-Abraham without matching")

# combine all results
combined_size_results_treated <- 
  bind_rows(event_study_twfe_size_coefs_treated, event_study_sunab_size_coefs_treated,
            event_study_sunab_no_match_size_coefs_treated)

# check pretrends
combined_size_results %>% filter(str_detect(term, "-20"),  p.value < .05) %>% View()


### Inner ring: -----
# read in inner ring -> treatment size correspondence
inner_ring_tracts_first_treated <- 
  read_csv(here(merged_data_dir, "inner_ring_tracts_first_treated.csv")) %>% 
  select(-treatment_year)

tract_data_matched_2_year_inner <- 
  tract_data_matched_2_year %>%
  filter(group_type == "inner") 

# 
tract_data_matched_with_size_inner <- 
  tract_data_matched_2_year_inner %>% 
  left_join(inner_ring_tracts_first_treated)

# Initialize lists
size_groups_inner <- tract_data_matched_with_size_inner %>% 
  select(match_group, total_public_housing_units_built_nearby) %>% 
  filter(!is.na(total_public_housing_units_built_nearby)) %>% 
  distinct() %>% 
  # assign quartiles
  mutate(size_group = ntile(total_public_housing_units_built_nearby, 3)) %>% 
  select(-total_public_housing_units_built_nearby)

# Merge onto size groups
tract_data_matched_with_size_inner <- 
  tract_data_matched_with_size_inner %>% 
  left_join(size_groups_inner, by = "match_group")

# Initialize lists
did_results_event_study_twfe_size_inner <- list()
did_results_event_study_sunab_size_inner <- list()
did_results_event_study_sunab_no_match_size_inner <- list()

# Loop over outcome variables and size categories
for (outcome in outcome_variables) {
  for (s in 1:max(tract_data_matched_with_size_inner$size_group, na.rm = TRUE)) {
    
    results <- did_event_study(input_data = tract_data_matched_with_size_inner,
                               outcome_var = outcome,
                               treatment_group = "inner",
                               size = s)
    did_results_event_study_twfe_size_inner[[paste(outcome, s, sep = "_")]] <- results$twfe
    did_results_event_study_sunab_size_inner[[paste(outcome, s,  sep = "_")]] <- results$sunab
    did_results_event_study_sunab_no_match_size_inner[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
  }
}

# save estimates in df
event_study_twfe_size_coefs_inner <- save_estimates(did_results_event_study_twfe_size_inner, "TWFE")
event_study_sunab_size_coefs_inner <- save_estimates(did_results_event_study_sunab_size_inner, "Sun-Abraham")
event_study_sunab_no_match_size_coefs_inner <- save_estimates(did_results_event_study_sunab_no_match_size_inner,
                                                                "Sun-Abraham without matching")

# combine all results
combined_size_results_inner <- 
  bind_rows(event_study_twfe_size_coefs_inner, event_study_sunab_size_coefs_inner,
            event_study_sunab_no_match_size_coefs_inner)

# check pretrends
combined_size_results_inner %>% filter(str_detect(term, "-20"),  p.value < .05) %>% View()

 

## By minority share ----

# get pre-treatment black share for treated units, create bins
initial_nonwhite_share  <- 
  tract_data_matched_2_year %>%
  filter(location_type == "treated" | location_type == "inner") %>%
  mutate(event_time = year - treatment_year) %>% 
  filter(event_time == -10) %>%
  mutate(nonwhite_share = 1-white_share) %>% 
  dplyr::rename(pre_treatment_nonwhite_share = nonwhite_share) %>% 
  select(match_group, group_type, pre_treatment_nonwhite_share) %>% 
  distinct() %>% 
  # create bins
  mutate(nonwhite_bin = case_when(
    pre_treatment_nonwhite_share <= 0.05 ~ 1,
    pre_treatment_nonwhite_share > 0.05 & pre_treatment_nonwhite_share <= .2  ~ 2,
    pre_treatment_nonwhite_share > .2 & pre_treatment_nonwhite_share <= 0.5 ~ 3,
    pre_treatment_nonwhite_share > 0.5 ~ 4))

# merge with matched data
tract_data_matched_with_nonwhite_share <- 
  tract_data_matched_2_year %>%
  left_join(initial_nonwhite_share)

#### treated ----
# Initialize lists
did_results_event_study_twfe_nonwhite_treated <- list()
did_results_event_study_sunab_nonwhite_treated <- list()
did_results_event_study_sunab_no_match_nonwhite_treated <- list()

# Loop over outcome variables and race categories
for (outcome in outcome_variables) {
  for (s in 1:max(tract_data_matched_with_nonwhite_share$nonwhite_bin, na.rm = TRUE)) {
    
    
    results <- did_event_study(input_data = tract_data_matched_with_nonwhite_share %>%
                                 filter(nonwhite_bin == s),
                               outcome_var = outcome,
                               treatment_group = "treated")
    did_results_event_study_twfe_nonwhite_treated[[paste(outcome, s, sep = "_")]] <- results$twfe
    did_results_event_study_sunab_nonwhite_treated[[paste(outcome, s,  sep = "_")]] <- results$sunab
    did_results_event_study_sunab_no_match_nonwhite_treated[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
  }
}

# save estimates in df
event_study_twfe_nonwhite_coefs_treated <- save_estimates(did_results_event_study_twfe_nonwhite_treated, "TWFE")
event_study_sunab_nonwhite_coefs_treated <- save_estimates(did_results_event_study_sunab_nonwhite_treated, "Sun-Abraham")
event_study_sunab_no_match_nonwhite_coefs_treated <- save_estimates(did_results_event_study_sunab_no_match_nonwhite_treated,
                                                                "Sun-Abraham without matching")

# combine all results
combined_nonwhite_results_treated <- 
  bind_rows(event_study_twfe_nonwhite_coefs_treated, event_study_sunab_nonwhite_coefs_treated,
            event_study_sunab_no_match_nonwhite_coefs_treated)

# check pretrends
combined_nonwhite_results_treated %>% filter(str_detect(term, "-20"),  p.value < .05) %>% View()

#### inner ring -----

# Initialize lists
did_results_event_study_twfe_nonwhite_inner <- list()
did_results_event_study_sunab_nonwhite_inner <- list()
did_results_event_study_sunab_no_match_nonwhite_inner <- list()

# Loop over
for (outcome in outcome_variables) {
  for (s in 1:max(tract_data_matched_with_nonwhite_share$nonwhite_bin, na.rm = TRUE)) {
    
    
    results <- did_event_study(input_data = tract_data_matched_with_nonwhite_share %>%
                                 filter(nonwhite_bin == s),
                               outcome_var = outcome,
                               treatment_group = "inner")
    did_results_event_study_twfe_nonwhite_inner[[paste(outcome, s, sep = "_")]] <- results$twfe
    did_results_event_study_sunab_nonwhite_inner[[paste(outcome, s,  sep = "_")]] <- results$sunab
    did_results_event_study_sunab_no_match_nonwhite_inner[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
  }
}

# save estimates in df
event_study_twfe_nonwhite_coefs_inner <- save_estimates(did_results_event_study_twfe_nonwhite_inner, "TWFE")
event_study_sunab_nonwhite_coefs_inner <- save_estimates(did_results_event_study_sunab_nonwhite_inner, "Sun-Abraham")
event_study_sunab_no_match_nonwhite_coefs_inner <- save_estimates(did_results_event_study_sunab_no_match_nonwhite_inner,
                                                                "Sun-Abraham without matching")

# combine all results
combined_nonwhite_results_inner <- 
  bind_rows(event_study_twfe_nonwhite_coefs_inner, event_study_sunab_nonwhite_coefs_inner,
            event_study_sunab_no_match_nonwhite_coefs_inner)

# check pretrends
combined_nonwhite_results_inner %>% filter(str_detect(term, "-20"),  p.value < .05) %>% View()


## By initial income -----
# get pre-treatment income for treated units, create bins
initial_income <- 
  tract_data_matched_2_year %>%
  filter(location_type == "treated" | location_type == "inner") %>%
  mutate(event_time = year - treatment_year) %>% 
  filter(event_time == -10) %>%
  dplyr::rename(pre_treatment_income = median_income) %>% 
  select(match_group, group_type, pre_treatment_income) %>% 
  distinct() %>% 
  # create terciles
  mutate(income_tercile = ntile(pre_treatment_income, 3)) %>% 
  select(match_group, group_type, income_tercile)

# merge with data
tract_data_matched_with_income_tercile <- 
  tract_data_matched_2_year %>%
  left_join(initial_income, by = c("match_group", "group_type"))


#### Treated ----
# Initialize lists
did_results_event_study_twfe_income_treated <- list()
did_results_event_study_sunab_income_treated <- list()
did_results_event_study_sunab_no_match_income_treated <- list()

# Loop over
for (outcome in outcome_variables) {
  for (s in 1:max(tract_data_matched_with_income_tercile$income_tercile, na.rm = TRUE)) {
    
    
    results <- did_event_study(input_data = tract_data_matched_with_income_tercile %>%
                                 filter(income_tercile == s),
                               outcome_var = outcome,
                               treatment_group = "treated")
    did_results_event_study_twfe_income_treated[[paste(outcome, s, sep = "_")]] <- results$twfe
    did_results_event_study_sunab_income_treated[[paste(outcome, s,  sep = "_")]] <- results$sunab
    did_results_event_study_sunab_no_match_income_treated[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
  }
}

# combine all results
event_study_twfe_income_coefs_treated <- save_estimates(did_results_event_study_twfe_income_treated, "TWFE")
event_study_sunab_income_coefs_treated <- save_estimates(did_results_event_study_sunab_income_treated, "Sun-Abraham")
event_study_sunab_no_match_income_coefs_treated <- save_estimates(did_results_event_study_sunab_no_match_income_treated,
                                                                "Sun-Abraham without matching")

# combine all results
combined_income_results_treated <- 
  bind_rows(event_study_twfe_income_coefs_treated, event_study_sunab_income_coefs_treated,
            event_study_sunab_no_match_income_coefs_treated)

#### Inner -----
# Initialize lists
did_results_event_study_twfe_income_inner <- list()
did_results_event_study_sunab_income_inner <- list()
did_results_event_study_sunab_no_match_income_inner <- list()

# Loop over
for (outcome in outcome_variables) {
  for (s in 1:max(tract_data_matched_with_income_tercile$income_tercile, na.rm = TRUE)) {
    
    
    results <- did_event_study(input_data = tract_data_matched_with_income_tercile %>%
                                 filter(income_tercile == s),
                               outcome_var = outcome,
                               treatment_group = "inner")
    did_results_event_study_twfe_income_inner[[paste(outcome, s, sep = "_")]] <- results$twfe
    did_results_event_study_sunab_income_inner[[paste(outcome, s,  sep = "_")]] <- results$sunab
    did_results_event_study_sunab_no_match_income_inner[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
  }
}

# combine all results
event_study_twfe_income_coefs_inner <- save_estimates(did_results_event_study_twfe_income_inner, "TWFE")
event_study_sunab_income_coefs_inner <- save_estimates(did_results_event_study_sunab_income_inner, "Sun-Abraham")
event_study_sunab_no_match_income_coefs_inner <- save_estimates(did_results_event_study_sunab_no_match_income_inner,
                                                                "Sun-Abraham without matching")

# combine all results
combined_income_results_inner <- 
  bind_rows(event_study_twfe_income_coefs_inner, event_study_sunab_income_coefs_inner,
            event_study_sunab_no_match_income_coefs_inner)


## By initial population density -----
# get pre-treatment income for treated units, create bins
initial_pop_density <- 
  tract_data_matched_2_year %>%
  filter(location_type == "treated" | location_type == "inner") %>%
  mutate(event_time = year - treatment_year) %>% 
  filter(event_time == -10) %>%
  dplyr::rename(pre_treatment_pop_density = population_density) %>% 
  select(match_group, group_type, pre_treatment_pop_density) %>% 
  distinct() %>% 
  # create terciles
  mutate(pop_density_tercile = ntile(pre_treatment_pop_density, 3)) %>% 
  select(match_group, group_type, pop_density_tercile)


# merge with data
tract_data_matched_with_pop_density_tercile <- 
  tract_data_matched_2_year %>%
  left_join(initial_pop_density, by = c("match_group", "group_type"))

#### Treated ----
# Initialize lists
did_results_event_study_twfe_pop_density_treated <- list()
did_results_event_study_sunab_pop_density_treated <- list()
did_results_event_study_sunab_no_match_pop_density_treated <- list()

# Loop over
for (outcome in outcome_variables) {
  for (s in 1:max(tract_data_matched_with_pop_density_tercile$pop_density_tercile, na.rm = TRUE)) {
    
    
    results <- did_event_study(input_data = tract_data_matched_with_pop_density_tercile %>%
                                 filter(pop_density_tercile == s),
                               outcome_var = outcome,
                               treatment_group = "treated")
    did_results_event_study_twfe_pop_density_treated[[paste(outcome, s, sep = "_")]] <- results$twfe
    did_results_event_study_sunab_pop_density_treated[[paste(outcome, s,  sep = "_")]] <- results$sunab
    did_results_event_study_sunab_no_match_pop_density_treated[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
  }
}

event_study_twfe_pop_density_coefs_treated <- save_estimates(did_results_event_study_twfe_pop_density_treated, "TWFE")
event_study_sunab_pop_density_coefs_treated <- save_estimates(did_results_event_study_sunab_pop_density_treated, "Sun-Abraham")
event_study_sunab_no_match_pop_density_coefs_treated <- save_estimates(did_results_event_study_sunab_no_match_pop_density_treated,
                                                                "Sun-Abraham without matching")

# combine all results
combined_pop_density_results_treated <- 
  bind_rows(event_study_twfe_pop_density_coefs_treated, event_study_sunab_pop_density_coefs_treated,
            event_study_sunab_no_match_pop_density_coefs_treated)

#### Inner ----
# Initialize lists
did_results_event_study_twfe_pop_density_inner <- list()
did_results_event_study_sunab_pop_density_inner <- list()
did_results_event_study_sunab_no_match_pop_density_inner <- list()

# Loop over
for (outcome in outcome_variables) {
  for (s in 1:max(tract_data_matched_with_pop_density_tercile$pop_density_tercile, na.rm = TRUE)) {
    
    
    results <- did_event_study(input_data = tract_data_matched_with_pop_density_tercile %>%
                                 filter(pop_density_tercile == s),
                               outcome_var = outcome,
                               treatment_group = "inner")
    did_results_event_study_twfe_pop_density_inner[[paste(outcome, s, sep = "_")]] <- results$twfe
    did_results_event_study_sunab_pop_density_inner[[paste(outcome, s,  sep = "_")]] <- results$sunab
    did_results_event_study_sunab_no_match_pop_density_inner[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
  }
}

event_study_twfe_pop_density_coefs_inner <- save_estimates(did_results_event_study_twfe_pop_density_inner, "TWFE")
event_study_sunab_pop_density_coefs_inner <- save_estimates(did_results_event_study_sunab_pop_density_inner, "Sun-Abraham")
event_study_sunab_no_match_pop_density_coefs_inner <- save_estimates(did_results_event_study_sunab_no_match_pop_density_inner,
                                                                "Sun-Abraham without matching")

# combine all results
combined_pop_density_results_inner <- 
  bind_rows(event_study_twfe_pop_density_coefs_inner, event_study_sunab_pop_density_coefs_inner,
            event_study_sunab_no_match_pop_density_coefs_inner)

## TODO Separately for New York City and Non-NYC-----
tract_data_matched_2_year_nyc <- 
  tract_data_matched_2_year %>%
  filter(city == "New York City")

tract_data_matched_non_nyc <- 
  tract_data_matched_2_year %>%
  filter(city != "New York City")  






# 6. Plot results -----

# function for plotting the event study graphs:
# reg_results_df is a dataframe with 
# data is the data used for the regression
# dep_var is the dependent variable
create_event_study_plot <- function(reg_results_df, dep_var, category_filter = NULL, title) {
  
  # reg_results_df =  event_study_sunab_coefs
  # data = tract_data_matched
  # dep_var = outcome
  # title = paste0("Matched DiD: Effect of public housing on ", outcome)
  # category_filter = NULL
  
  coefficients <- reg_results_df %>% 
    filter(str_starts(outcome, dep_var)) %>% 
    # create a column with everyhting after dep_var_
    mutate(category = str_remove(outcome, paste0(dep_var, "_"))) %>% 
    mutate(outcome = dep_var) %>% 
    # remove year:: from term
    mutate(event_time = as.numeric(str_remove(term, "year::"))) %>% 
    # remove term
    dplyr::select(-term)
  
  # if category filter is not NULL, apply category filter
  if (!is.null(category_filter)) {
    coefficients <- coefficients %>% 
      filter(category == category_filter)
  }
  
  coefficients <- coefficients %>% 
    # add row for 0s for treated and inner
    bind_rows(tibble(event_time = -10, estimate = 0, std.error = 0, category = "treated"),
              tibble(event_time = -10, estimate = 0, std.error = 0, category = "inner"))
  
  
  
  # create the plot
  plot <- 
    coefficients %>% 
    ggplot(aes(x = event_time, y = estimate, color = category))  +
    geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                  width = 0.3,
                  size = 1.1,
                  alpha = 0.7,
                  position = position_dodge(width = 1)) +
    geom_point(size = 2,
               position = position_dodge(width = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title,
         subtitle = "",
         x = "Years Relative to Treatment",
         y = "Difference in Difference Estimate") +
    scale_x_continuous(breaks = seq(-40, 40, 10)) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    theme_minimal() +
    theme(legend.position = "bottom", plot.background = element_rect(fill = "white"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) 
  
  plot
  #guides(color = guide_legend(title = "Location Type")) +
  #scale_color_brewer(palette = "Set1", labels = c("Inner Ring", "Treated"))
  # # add text at bottom of plot that says "t = 10 coefficient is t10_coefficient of the mean at t = 0"
  # labs(caption = paste("Treated coefficient at t = 10 is", round(t10_coefficient, 2), "of the treated tract mean at t = -10")) +
  # # add text at top of plot that says "n = n_obs"
  #annotate("text", x = 50, y = max(reg_results_df$coefficients$estimate), label = paste("n =", n_obs), hjust = 1)
  
}


# For Heterogeneity analysis, I will display all 
create_event_study_plot_by_group <- function(reg_results_df, dep_var, title) {
  
  # for testing
  # reg_results_df =  event_study_sunab_size_coefs
  # data = tract_data_matched_with_size_treated
  # dep_var = "asinh_pop_black"
  # title = paste0("Matched DiD: Effect of public housing on ", outcome)

  coefficients <- reg_results_df %>% 
    filter(str_starts(outcome, dep_var)) %>% 
    # create a column with everything after dep_var_
    mutate(category = str_remove(outcome, paste0(dep_var, "_"))) %>% 
    mutate(outcome = dep_var) %>% 
    # remove year:: from term
    mutate(event_time = as.numeric(str_remove(term, "year::"))) %>% 
    # remove term
    dplyr::select(-term)
  
  # dynamically add row for all categories 
  categories <- coefficients %>% distinct(category) %>% pull(category)
  
  # Create a tibble with a row for each category
  additional_rows <- tibble(
    event_time = -10,
    estimate = 0,
    std.error = 0,
    category = categories
  )

  coefficients <- coefficients %>% 
    # add row for 0s for all categories
    bind_rows(additional_rows)
  
  
  # create the plot
  dodge_width <- 1.5
  
  plot <- 
    coefficients %>% 
    ggplot(aes(x = event_time, y = estimate, color = category))  +
    geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                  width = 0.3,
                  size = 1.1,
                  alpha = 0.7, 
                  position = position_dodge(width = dodge_width)) +
    geom_point(size = 2,
               position = position_dodge(width = dodge_width)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title,
         subtitle = "",
         x = "Years Relative to Treatment",
         y = "Difference in Difference Estimate") +
    scale_x_continuous(breaks = seq(-40, 40, 10)) +
    scale_color_brewer(type = "seq", palette = "Reds") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "white"),
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) 
  
  plot
  #guides(color = guide_legend(title = "Location Type")) +
  #scale_color_brewer(palette = "Set1", labels = c("Inner Ring", "Treated"))
  # # add text at bottom of plot that says "t = 10 coefficient is t10_coefficient of the mean at t = 0"
  # labs(caption = paste("Treated coefficient at t = 10 is", round(t10_coefficient, 2), "of the treated tract mean at t = -10")) +
  # # add text at top of plot that says "n = n_obs"
  #annotate("text", x = 50, y = max(reg_results_df$coefficients$estimate), label = paste("n =", n_obs), hjust = 1)
  
}


## Plot baseline results  -----

### Sun-Abraham Results -----
pdf(here(results_dir, "event_study_plots_baseline_sunab_matching.pdf"))

for(i in seq_along(outcome_variables)) {
  # Create and print the plot
  plot <- create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                                  dep_var = outcome_variables[i],
                                  title = paste0("Matched DiD: Effect of public housing on ",
                                                 outcome_variables[i], "Sun-Abraham estimator"))
  
  print(plot)
}

dev.off()



### TWFE results ----
pdf(here(results_dir, "event_study_plots_baseline_twfe_matching.pdf")) 

# loop and print plots
for(i in seq_along(outcome_variables)) {
  # Create and print the plot
  plot <- create_event_study_plot(reg_results_df =  event_study_twfe_coefs,
                                           dep_var = outcome_variables[i],
                                           title = paste0("Matched DiD: Effect of public housing on ",
                                                          outcome_variables[i] ))

  print(plot)
}

dev.off()


### Sun-Abraham results without matching -----
pdf(here(results_dir, "event_study_plots_baseline_sunab_no_match.pdf"))

for(i in seq_along(outcome_variables)) {
  # Create and print the plot
  plot <- create_event_study_plot(reg_results_df =  event_study_sunab_no_match_coefs,
                                  dep_var = outcome_variables[i],
                                  title = paste0("Effect of public housing on ",
                                                 outcome_variables[i], ", no match FE"))
  
  print(plot)
}

dev.off()




## Plot heterogeneity results ----

### Project size ----

#### treated  -----
pdf_file_name <- here(results_dir, paste0("event_study_plots_by_size_treated_twfe_matching.pdf"))
  
# Open PDF device for the current size
pdf(pdf_file_name, width = 10, height = 5)

for(i in seq_along(outcome_variables)) {
  # Create and print the plot
  plot <- create_event_study_plot_by_group(reg_results_df =  event_study_twfe_size_coefs_treated,
                                  dep_var = outcome_variables[i],
                                  title = paste0("Effect of public housing on ",
                                                 outcome_variables[i], " by project size tercile, treated tracts" ))
  
  
  print(plot)
}

# Close the PDF device for the current size
dev.off()

#### inner ring -----
pdf_file_name <- here(results_dir, paste0("event_study_plots_by_size_inner_ring_twfe_matching.pdf"))

# Open PDF device for the current size
pdf(pdf_file_name, width = 10, height = 5)

for(i in seq_along(outcome_variables)) {
  # Create and print the plot
  plot <- create_event_study_plot_by_group(reg_results_df =  event_study_twfe_size_coefs_inner,
                                  dep_var = outcome_variables[i],
                                  title = paste0("Effect of public housing on ",
                                                 outcome_variables[i], " by project size tercile, inner ring tracts" ))
  
  print(plot)
}


dev.off()

### Initial nonwhite share -----

#### treated ----
pdf_file_name <- here(results_dir, paste0("event_study_plots_by_minority_share_treated_twfe_matching.pdf"))

# Open PDF device for the current size
pdf(pdf_file_name, width = 10, height = 5)

for(i in seq_along(outcome_variables)) {
  # Create and print the plot
  plot <- create_event_study_plot_by_group(reg_results_df =  event_study_twfe_nonwhite_coefs_treated,
                                  dep_var = outcome_variables[i],
                                  title = paste0("Effect of public housing on ",
                                                 outcome_variables[i], " by initial minority share tercile, treated tracts" ))
  
  print(plot)
}

dev.off()

#### inner ----
pdf_file_name <- here(results_dir, paste0("event_study_plots_by_minority_share_inner_ring_twfe_matching.pdf"))

# Open PDF device for the current size
pdf(pdf_file_name, width = 10, height = 5)

for(i in seq_along(outcome_variables)) {
  # Create and print the plot
  plot <- create_event_study_plot_by_group(reg_results_df =  event_study_twfe_nonwhite_coefs_inner,
                                  dep_var = outcome_variables[i],
                                  title = paste0("Effect of public housing on ",
                                                 outcome_variables[i], " by initial minority share tercile, inner ring tracts" ))
  
  print(plot)
}

dev.off()

### By initial income -----

#### treated ----
pdf_file_name <- here(results_dir, paste0("event_study_plots_by_income_treated_twfe_matching.pdf"))

# Open PDF device for the current size
pdf(pdf_file_name, width = 10, height = 5)

for(i in seq_along(outcome_variables)) {
  # Create and print the plot
  plot <- create_event_study_plot_by_group(reg_results_df =  event_study_twfe_income_coefs_treated,
                                  dep_var = outcome_variables[i],
                                  title = paste0("Effect of public housing on ",
                                                 outcome_variables[i], " by initial income tercile, treated tracts" ))
  
  print(plot)
}

dev.off()

#### inner ----
pdf_file_name <- here(results_dir, paste0("event_study_plots_by_income_inner_ring_twfe_matching.pdf"))

# Open PDF device for the current size
pdf(pdf_file_name, width = 10, height = 5)

for(i in seq_along(outcome_variables)) {
  # Create and print the plot
  plot <- create_event_study_plot_by_group(reg_results_df =  event_study_twfe_income_coefs_inner,
                                  dep_var = outcome_variables[i],
                                  title = paste0("Effect of public housing on ",
                                                 outcome_variables[i], " by initial income tercile, inner ring tracts" ))
  
  print(plot)
}

dev.off()


### By population density ----

#### treated ----
pdf_file_name <- here(results_dir, paste0("event_study_plots_by_density_treated_twfe_matching.pdf"))

# Open PDF device for the current size
pdf(pdf_file_name, width = 10, height = 5)

for(i in seq_along(outcome_variables)) {
  # Create and print the plot
  plot <- create_event_study_plot_by_group(reg_results_df =  event_study_twfe_pop_density_coefs_treated,
                                  dep_var = outcome_variables[i],
                                  title = paste0("Effect of public housing on ",
                                                 outcome_variables[i], " by initial population density tercile, treated tracts" ))
  
  print(plot)
}

dev.off()

#### inner ----
pdf_file_name <- here(results_dir, paste0("event_study_plots_by_density_inner_ring_twfe_matching.pdf"))

# Open PDF device for the current size
pdf(pdf_file_name, width = 10, height = 5)

for(i in seq_along(outcome_variables)) {
  # Create and print the plot
  plot <- create_event_study_plot_by_group(reg_results_df =  event_study_twfe_pop_density_coefs_inner,
                                  dep_var = outcome_variables[i],
                                  title = paste0("Effect of public housing on ",
                                                 outcome_variables[i], " by initial population density tercile, inner ring tracts" ))
  
  print(plot)
}

dev.off()
