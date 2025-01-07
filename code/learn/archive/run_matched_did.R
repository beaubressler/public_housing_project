library(MatchIt)
library(tidyverse)
library(sf)
library(lmtest)
library(fixest)

# libraries for covariate testing
library(cobalt)
library(tableone)
#library for Sant'Anna and Zhou (2020)
library(DRDID)
# library for Callaway and Sant'Anna (2021)
library(did)


rm(list=ls())


# 0. Set seed and parameters -----

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small", or "combined"
data_type <- "combined"


set.seed(123456L)
# set number of nearest neighbors for KNN matching
knn_neighbors <- 2

# Notes: With CDD large, KNN = 2 seems to work well
# with the smaller dataset, KNN = 3 work

# define file paths
tract_with_treatment_status_filepath <- paste0("data/derived/merged/census_tract_sample_with_treatment_status_", data_type, ".gpkg")
treated_tracts_panel_filepath <- paste0("data/derived/merged/treated_tracts_panel_", data_type, ".gpkg")

map_dir <- paste0("output/figures/matched_did/", data_type, "/")

results_dir <- paste0("output/regression_results/matched_did/", data_type, "/")

# read census data with treatment status
census_tract_sample_raw <-
  st_read(tract_with_treatment_status_filepath)

# read tract tracts pane
treated_tracts_panel_raw <-
  st_read(treated_tracts_panel_filepath) %>% 
  st_drop_geometry()

# read in unique rings and tracts

# 1. Data Preparation ------

# read in HOLC data
holc_data <-
  read_csv("data/derived/holc/tract_holc_classifications.csv") %>% 
  select(-contains("city"), -state)


# get treated tracts, unique, and merge onto census tract sample
treated_tracts_panel <-
  treated_tracts_panel_raw %>% 
  distinct(STATE, COUNTY, TRACTA, treatment_year, total_public_housing_units) %>% 
  mutate(treatment_group = 1)

census_tract_data <-
  left_join(census_tract_sample_raw, treated_tracts_panel) %>% 
  # merge on HOLC data
  left_join(holc_data) %>% 
  # set treatment_group = 0 if NA
  mutate(treatment_group = ifelse(is.na(treatment_group), 0, treatment_group)) %>% 
  # create unique id for each tract
  mutate(tract_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
  mutate(asinh_pop_total = asinh(total_pop),
         asinh_pop_white = asinh(white_pop),
         asinh_pop_black = asinh(black_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent = asinh(median_rent),
         asinh_median_home_value = asinh(median_home_value)) %>% 
  dplyr::rename(year = YEAR) %>% 
  st_drop_geometry() %>% 
  # Ensure data is sorted by tract_id and year
  arrange(tract_id, year)  %>% 
  # for HOLC variables (grade and category) if category is missisng ,set to "missing"
  mutate(category = ifelse(is.na(category), "missing", category),
         grade = ifelse(is.na(grade), "missing", grade))
        

# 2. Run KNN Matching ------
# Because of my data structure, it makes sense to do the propensity score matching 
# separately for each treatment year, then combine

# function for performing matching in each year
perform_matching_for_year <- function(data, treatment_year, match_vars, nearest_neighbors) {
  # Filter data to include:
  # 1. Tracts treated in the specified year
  # 2. Never-treated tracts
  # 3. Use data from 10 years prior to treatment
  matching_data <- data %>%
    filter((treatment_group == 1 & treatment_year == !!treatment_year) | 
             (treatment_group == 0 & year == !!treatment_year - 10)) %>%
    filter(year == !!treatment_year - 10) %>% 
    # delete rows with messed up data
    filter(total_pop != 0, white_pop < total_pop, black_pop < total_pop)
  
  # Construct the formula dynamically
  formula_str <- paste("treatment_group ~", paste(match_vars, collapse = " + "))
  match_formula <- as.formula(formula_str)
  
  
  
  # Create black_share categories
  matching_data <- matching_data %>%
    mutate(black_share_cat = cut(black_share, 
                                 breaks = seq(0, 1, by = 0.1),
                                 labels = FALSE,
                                 include.lowest = TRUE))
  
  # Set calipers for all variables
  # calipers <- c(
  #   black_share = 0.02, # black share within 5%
  #   asinh_pop_black = 0.05, # asinh of black population within 5%
  #   population_density = 0.2, 
  #   distance_from_cbd = 0.2,
  #   asinh_pop_total = 0.2,
  #   category = 0.2  # Note: for factor variables, this is applied to the linear predictor
  # )
  # 
  # # std_caliper = False for black share and asinh_pop_black
  # set_std_caliper <-c(
  #   FALSE,
  #   FALSE,
  #   TRUE,
  #   TRUE,
  #   TRUE,
  #   TRUE
  # )
  # 
  
  # Step 2: KNN matching
  m.out <- matchit(match_formula,
                   data = matching_data,
                   exact = c("city"),
                   method = "nearest",
                   distance = "glm",
                   ratio = nearest_neighbors,
                   caliper = 0.1,
                   std.caliper = TRUE)
                   #add caliper
                   # calipers = calipers,
                   # std.caliper = set_std_caliper)
  
  # # Perform genetic matching
  # m.out <- matchit(match_formula,
  #                  data = matching_data,
  #                  method = "genetic",
  #                  distance = "glm",
  #                  ratio = nearest_neighbors,
  #                  caliper = 0.1,
  #                  std.caliper = TRUE,
  #                  exact = "city",
  #                  pop.size = 100,  # Increase for better results, but slower runtime
  #                  wait.generations = 5,
  #                  max.generations = 10,
  #                  importance = importance)
  # 
  
  # Get matched data
  matched_data <- match.data(m.out)
  
  # Add treatment year information
  matched_data$matched_treatment_year <- treatment_year
  
  return(list(matched_data = matched_data, m.out = m.out))
}


# Matching variables: For now, I will keep everything static, but going forward, I can incorporate more matching tracts
# matching_vars <- c("black_share", "total_pop", "asinh_pop_total", "asinh_pop_black", "black_pop",
#                    "white_pop", 
#                    "distance_from_cbd", "population_density", "distance_from_cbd:population_density",
#                    "black_share:distance_from_cbd")

# exploring matching variables
# I think this is a good sensible set of matching criteria that have good fit
# Black share, Total population and black population, population density, distance from CBD, 
# and HOLC classification (set to "missing" if it's missing)
matching_vars <- c("black_share", "population_density", "distance_from_cbd", 
                   "asinh_pop_black", "asinh_pop_total", "category")



# matching_vars <- list(
#   "1940" = c("black_share", "white_share", "total_pop", "black_pop", "median_home_value", "distance_from_cbd"),
#   "1950" = c("black_share", "white_share", "total_pop", "black_pop", "median_home_value", "distance_from_cbd"),
#   "1960" = c("black_share", "white_share", "total_pop", "black_pop", "median_home_value", "distance_from_cbd"),
#   # ... add more years as needed
# )


# get list of treatment years
treatment_years <-
  unique(census_tract_data %>%
           filter(!is.na(treatment_year)) %>%
           pull(treatment_year))


# Initialize an empty list to store matched datasets
matched_datasets <- list()
m_out_objects <- list()


# Perform matching for each treatment year
for (year in treatment_years) {
  df_name <- paste0("matched_data_", year)
  m_out_name <- paste0("m_out_", year)
  
  result <- perform_matching_for_year(census_tract_data, year, matching_vars, knn_neighbors)
  
  matched_datasets[[df_name]] <- result[["matched_data"]]
  m_out_objects[[m_out_name]] <- result[["m.out"]]
}

# Combine all matched datasets
final_matched_data <- 
  bind_rows(matched_datasets) %>%
  # delete duplicates
  distinct(tract_id, .keep_all = TRUE)

# Merge matching information back to the original dataset
tract_data_matched <- census_tract_data %>%
  left_join(final_matched_data %>% 
              select(tract_id, weights, matched_treatment_year, subclass),
            by = "tract_id") %>% 
  # Replace NA weights with 0 (for unmatched observations)
  mutate(weights = ifelse(is.na(weights), 0, weights)) %>% 
  #  create a match_group, which is a unique identifer for each match
  mutate(
    match_group = ifelse(!is.na(matched_treatment_year),
                         paste(city, matched_treatment_year, subclass, sep = "_"),
                         NA)
  )
  

### 2.25 Try with random forest propensity score matching -----
# Note: As of now, I am not using this method, but I am keeping it here for future reference
# Balance was much worse with this method, so I am sticking with nearest neighbor matching
# perform_rf_matching_for_year <- function(data, treatment_year, match_vars, nearest_neighbors) {
#   # Filter data
#   matching_data <- data %>%
#     filter((treatment_group == 1 & treatment_year == !!treatment_year) | 
#              (treatment_group == 0 & year == !!treatment_year - 10)) %>%
#     filter(year == !!treatment_year - 10) %>% 
#     filter(total_pop != 0)
#   
#   # Prepare formula
#   formula_str <- paste("as.factor(treatment_group) ~", paste(match_vars, collapse = " + "))
#   rf_formula <- as.formula(formula_str)
#   
#   # Fit random forest
#   rf_model <- randomForest(rf_formula, data = matching_data, ntree = 1000)
#   
#   # Calculate propensity scores
#   matching_data$propensity_score <- predict(rf_model, type = "prob")[,2]
#   
#   # Perform matching on propensity scores
#   m.out <- matchit(treatment_group ~ propensity_score,
#                    data = matching_data, 
#                    method = "nearest", 
#                    ratio = nearest_neighbors,
#                    caliper = 0.2,
#                    std.caliper = TRUE)
#   
#   # Get matched data
#   matched_data <- match.data(m.out)
#   
#   # Add treatment year information
#   matched_data$matched_treatment_year <- treatment_year
#   
#   return(matched_data)
# }
# 
# # Matching variables
# matching_vars_rf <- c("black_share", "population_density", "distance_from_cbd", 
#                    "asinh_pop_black", "asinh_pop_total")
# 
# # Get list of treatment years
# treatment_years <- unique(census_tract_data %>% 
#                             filter(!is.na(treatment_year)) %>% 
#                             pull(treatment_year))
# 
# # Perform matching for each treatment year
# matched_datasets_rf <- list()
# for (year in treatment_years) {
#   matched_datasets_rf[[paste0("matched_data_", year)]] <- perform_rf_matching_for_year(
#     census_tract_data, year, matching_vars_rf, 3
#   )
# }
# 
# # Combine all matched datasets
# final_matched_data_rf <- bind_rows(matched_datasets_rf) %>%
#   distinct(tract_id, .keep_all = TRUE)
# 
# # Merge matching information back to the original dataset
# tract_data_matched_rf <- census_tract_data %>%
#   left_join(final_matched_data_rf %>% 
#               select(tract_id, weights, matched_treatment_year, subclass),
#             by = "tract_id") %>% 
#   mutate(weights = ifelse(is.na(weights), 0, weights),
#          match_group = ifelse(!is.na(matched_treatment_year),
#                               paste(city, matched_treatment_year, subclass, sep = "_"),
#                               NA))
# 


# 3. check balance of covariates ----
# Assuming 'matched_data' is your dataset with both treated and matched control units

# plotting love plots

# love.plot(m_out_objects$m_out_1940, var.order = "unadjusted", 
#           threshold = 0.1)
# love.plot(m_out_objects$m_out_1950, var.order = "unadjusted", 
#           threshold = 0.1)
# love.plot(m_out_objects$m_out_1960, var.order = "unadjusted",
#           threshold = 0.1)
# love.plot(m_out_objects$m_out_1970, var.order = "unadjusted",
#           threshold = 0.1)
# love.plot(m_out_objects$m_out_1980, var.order = "unadjusted",
#           threshold = 0.1)
  
## KNN matching  ----
# dataset of just matched units at pre-treatment period
balanced_data <- tract_data_matched %>%
  filter(!is.na(match_group)) %>%
  group_by(match_group) %>%
  mutate(group_treatment_year = min(matched_treatment_year, na.rm = TRUE)) %>%
  filter(year == group_treatment_year - 10) %>%
  ungroup()

# List of covariates to check balance
covariates <- c("black_share", "white_share",  "asinh_median_income", "median_income",
                "median_home_value", "asinh_median_home_value", "median_rent","asinh_median_rent", 
                "distance_from_cbd", "population_density", "housing_density", "total_pop", "black_pop",
                "asinh_pop_black", "asinh_pop_white", "asinh_pop_total", "category")  # Add or modify as needed

# Create table
balance_table <- CreateTableOne(vars = covariates,
                                strata = "treatment_group",
                                data = balanced_data,
                                test = TRUE)

# Print the table
print(balance_table, smd = TRUE)

# Definition from Claude.AI
# The SMD is the difference in means between two groups (in your case, treated and control),
# divided by the pooled standard deviation of the two groups.
# Rule of thumb for balance:
# SMD < 0.1: Covariate is well-balanced
# SMD between 0.1 and 0.2: Covariate good balance

plot_balance <- function(data, var) {
  ggplot(data, aes(x = .data[[var]], fill = factor(treatment_group))) +
    geom_density(alpha = 0.5) +
    ggtitle(paste("Distribution of", var, "in Matched Data")) +
    scale_fill_discrete(name = "Treatment Group", labels = c("Control", "Treated"))
}

# Create plots for each covariate
balance_plots <- lapply(covariates, function(var) plot_balance(balanced_data, var))

# Display plots
library(gridExtra)
do.call(grid.arrange, c(balance_plots, ncol = 2))


## Random Forest matched data  ----
# dataset of just matched units at pre-treatment period
# balanced_data_rf <- tract_data_matched_rf %>%
#   filter(!is.na(match_group)) %>%
#   group_by(match_group) %>%
#   mutate(group_treatment_year = min(matched_treatment_year, na.rm = TRUE)) %>%
#   filter(year < group_treatment_year) %>%
#   ungroup()
# 
# # Create table
# balance_table_rf <- CreateTableOne(vars = covariates,
#                                    strata = "treatment_group",
#                                    data = balanced_data_rf,
#                                    test = FALSE)
# 
# # Print the table
# print(balance_table_rf, smd = TRUE)

# Run regressions ----
# outcome variables
outcome_variables <- c("black_share", "white_share", "asinh_pop_total", "asinh_pop_black",
                       "asinh_pop_white",
                       "asinh_median_income", "asinh_median_home_value", "asinh_median_rent",
                       "population_density", "lfp_rate", "unemp_rate", "housing_density")


# Create maps of treated and control tracts for each city-----
create_city_map <- function(data, city_name) {
  # Filter data for the specific city
  city_data <- data %>%
    filter(city == city_name)
  
  # Create the map
  ggplot() +
    geom_sf(data = city_data, aes(fill = treatment_status), color = "black", size = 0.1) +
    scale_fill_manual(values = c("treated" = "#FF4136", "control" = "#0074D9", "excluded" = "#AAAAAA"),
                      name = "Tract Type",
                      labels = c("Treated" = "Treated", "Control" = "Control", "Excluded" = "Excluded")) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "bottom",
      legend.background = element_rect(fill = "white", color = "black"),
      legend.key = element_rect(color = "black", size = 0.2),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    ggtitle(paste("Treated and matched control tracts in", city_name))
}


# Merge matching info onto full dataset with spatial info 
all_tracts_with_treated_and_control_status <- 
  census_tract_sample_raw %>%
  # keep just one year
  filter(YEAR== 1990) %>% 
  # merge on matching information
  left_join(tract_data_matched %>% 
              filter(year == 1990) %>% 
              select(STATE, COUNTY, TRACTA, treatment_group, weights, matched_treatment_year),
            by = c("STATE", "COUNTY", "TRACTA")) %>% 
  # create variable equal to "treatment" if treatment_group == 1, "control" if  
  # treatment_group == 0 and !is.na(matched_treatment_year), and "excluded" otherwise
  mutate(treatment_status = case_when(
    treatment_group == 1 ~ "treated",
    treatment_group == 0 & !is.na(matched_treatment_year) ~ "control",
    TRUE ~ "excluded"
  ))

cities <- unique(all_tracts_with_treated_and_control_status$city)

for (city in cities) {
  map <- create_city_map(all_tracts_with_treated_and_control_status, city)
  
  # Save the map
  ggsave(filename = paste0(map_dir, gsub(" ", "_", city), "_treated_control_map.png"),
         plot = map, width = 12, height = 10, dpi = 300, bg = "white")
  
  print(paste("Map created for", city))
}


# 4. Estimate DiD, Two Period, TWFE -----
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

# 5. Estimate Event studies: TWFE and Sun and Abraham (2020)-----

did_event_study <- function(input_data, outcome_var, size = NULL, city_filter = NULL, initial_share = NULL) {
  
  
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
    group_by(match_group) %>%
    mutate(
      group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
      event_time = year - group_treatment_year) %>%
    ungroup() %>% 
    # remove -50 and +50 event times, sample size gets too small
    filter(event_time > -50, event_time < 50)
  
  # Create the formula for the event study
  # baseline model
  formula <- as.formula(paste(outcome_var, "~ i(event_time, treatment_group, ref = -10) | city^year + tract_id + match_group"))
  model <- feols(formula, data = data, weights = ~weights, cluster = ~tract_id)
  
  
  # Sun and Abraham model
  sunab_data <- 
    data %>% 
    # set treatment_year = 100 if missing
    mutate(treatment_year = ifelse(is.na(treatment_year), 100, treatment_year))
  
  formula_sunab <- as.formula(paste(outcome_var, "~ sunab(treatment_year, year, ref.p = -10) | year^city + tract_id + match_group"))
  model_sunab <- feols(formula_sunab, data = sunab_data, weights = ~weights, cluster = ~tract_id)
  
  return(list(twfe = model, sunab = model_sunab))
}
# function to save model results in df
save_estimates <- function(model_list) {
  map_dfr(names(model_list), function(model_name) {
    tidy(model_list[[model_name]]) %>%
      mutate(outcome = model_name) %>% 
      mutate_if(is.numeric, round, 4)
  })
}


## Full sample -----
# initialize lists
did_results_event_study_twfe <- list() 
did_results_event_study_sunab <- list() 

# save regression output to lists
for (outcome in outcome_variables) {
  results <- did_event_study(tract_data_matched, outcome)
  did_results_event_study_twfe[[outcome]] <- results$twfe
  did_results_event_study_sunab[[outcome]] <- results$sunab
}

# save results in df
event_study_twfe_coefs <- save_estimates(did_results_event_study_twfe)
event_study_sunab_coefs <- save_estimates(did_results_event_study_sunab)

summary(did_results_event_study_sunab$black_share, agg = "ATT")
summary(did_results_event_study_sunab$asinh_pop_black, agg = "ATT")
summary(did_results_event_study_sunab$asinh_pop_white, agg = "ATT")
summary(did_results_event_study_sunab$asinh_pop_total, agg = "ATT")
summary(did_results_event_study_sunab$asinh_median_income, agg = "ATT")
summary(did_results_event_study_sunab$asinh_median_home_value, agg = "ATT")
summary(did_results_event_study_sunab$asinh_median_rent, agg = "ATT")
summary(did_results_event_study_sunab$population_density, agg = "ATT")
summary(did_results_event_study_sunab$lfp_rate, agg = "ATT")
summary(did_results_event_study_sunab$unemp_rate, agg = "ATT")
summary(did_results_event_study_sunab$housing_density, agg = "ATT")


# Heterogeneity  ----

## Heterogeneity by project size -----
# question: is total_public_housing_units the size of the first project or all projects

# Step 1: Categorize treated units
tract_data_matched_with_size <-
  tract_data_matched %>%
  # for treated units, assign to size category based on total public housing units
  mutate(
    size_group = case_when(
      treatment_group == 1 ~ ntile(total_public_housing_units, 2),
      TRUE ~ NA_integer_
    )
  ) %>% 
  # For all untreated units, assign to size category based on matched treated unit
  group_by(match_group) %>%
  fill(size_group, .direction = "downup") %>%
  ungroup()

# Initialize lists
did_results_event_study_twfe_size <- list()
did_results_event_study_sunab_size <- list()

# Loop over outcome variables and size categories
for (outcome in outcome_variables) {
  for (s in 1:2) {
    results <- did_event_study(tract_data_matched_with_size, outcome, size = s)
    did_results_event_study_twfe_size[[paste(outcome, s, sep = "_")]] <- results$twfe
    did_results_event_study_sunab_size[[paste(outcome, s, sep = "_")]] <- results$sunab
  }
}


# save estimates in df
event_study_twfe_size_coefs <- save_estimates(did_results_event_study_twfe_size)
event_study_sunab_size_coefs <- save_estimates(did_results_event_study_sunab_size)

## Heterogeneity by initial black share ----
# get pre-treatment black share for treated units
treated_units_black_share <- 
  tract_data_matched %>%
  filter(treatment_group == 1) %>%
  mutate(event_time = year - treatment_year) %>% 
  filter(event_time == -10) %>%
  select(tract_id, black_share) %>% 
  dplyr::rename(pre_treatment_black_share = black_share)

blk_sh_25p <- quantile(treated_units_black_share$pre_treatment_black_share, probs = 0.25, na.rm = TRUE)
blk_sh_50p <- quantile(treated_units_black_share$pre_treatment_black_share, probs = 0.50, na.rm = TRUE)
blk_sh_75p <- quantile(treated_units_black_share$pre_treatment_black_share, probs = 0.75, na.rm = TRUE)


# merge with matched data
tract_data_matched_with_black_share <- 
  tract_data_matched %>%
  left_join(treated_units_black_share, by = "tract_id", suffix = c("", "_treated")) %>% 
  # for treated units, assign to 3 bins based on pre-treatment_black share
  mutate(
    race_group = case_when(
      treatment_group == 1 ~ ntile(total_public_housing_units, 4),
      TRUE ~ NA_integer_
    )
  ) %>% 
  # For all untreated units, assign to race_group based on matched treated unit
  group_by(match_group) %>%
  fill(race_group, .direction = "downup") %>%
  ungroup()

# Initialize lists
did_results_event_study_twfe_initial_share<- list()
did_results_event_study_sunab_initial_share <- list()

# Loop over outcome variables
# Loop over outcome variables and race groups
for (outcome in outcome_variables) {
  for (r in unique(tract_data_matched_with_black_share$race_group)) {
    results <- did_event_study(tract_data_matched_with_black_share, outcome, initial_share = r)
    did_results_event_study_twfe_initial_share[[paste(outcome, r, sep = "_")]] <- results$twfe
    did_results_event_study_sunab_initial_share[[paste(outcome, r, sep = "_")]] <- results$sunab
  }
}


# save estimates in df
event_study_twfe_initial_share_coefs <- save_estimates(did_results_event_study_twfe_initial_share)
event_study_sunab_initial_share_coefs <- save_estimates(did_results_event_study_sunab_initial_share)



## Heterogeneity by city -----

# run separately for each city
did_results_event_study_twfe_city <- list()
did_results_event_study_sunab_city <- list()

for (outcome in outcome_variables) {
  for (c in unique(tract_data_matched$city)) {
    results <- did_event_study(tract_data_matched, outcome, city_filter = c)
    did_results_event_study_twfe_city[[paste(outcome, c, sep = "_")]] <- results$twfe
    did_results_event_study_sunab_city[[paste(outcome, c, sep = "_")]] <- results$sunab
  }
}

# save estimates
event_study_twfe_city_coefs <- save_estimates(did_results_event_study_twfe_city)
event_study_sunab_city_coefs <- save_estimates(did_results_event_study_sunab_city)



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
    filter(str_detect(outcome, dep_var)) %>% 
    # create a column with everyhting after dep_var_
    mutate(category = str_remove(outcome, paste0(dep_var, "_"))) %>% 
    mutate(outcome = dep_var) %>% 
    # remove year:: from term
    mutate(event_time = as.numeric(str_remove(term, "year::"))) %>% 
    # remove term
    select(-term)
  
  # if category filter is not NULL, apply category filter
  if (!is.null(category_filter)) {
    coefficients <- coefficients %>% 
      filter(category == category_filter)
  }
  
  coefficients <- coefficients %>% 
    # add row for 0s
    bind_rows(tibble(event_time = -10, estimate = 0, std.error = 0))
  
    

  # create the plot
  plot <- 
    coefficients %>% 
  ggplot(aes(x = event_time, y = estimate))  +
    geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                  width = 0.2, alpha = 0.4, position = position_dodge(width = 1)) +
    geom_point(position = position_dodge(width = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title,
         subtitle = "",
         x = "Years Relative to Treatment",
         y = "Difference in Difference Estimate") +
    scale_x_continuous(breaks = seq(-40, 40, 10)) +
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

## Plot baseline results  -----

# have to run each manually because of how R's pdf function works I guess...
pdf(paste0(results_dir, "event_study_plots_baseline", ".pdf"))

# black_share
create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                        dep_var = "black_share",
                        title = paste0("Matched DiD: Effect of public housing on " , "black_share"))


# white_share. 
create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                        dep_var = "white_share",
                        title = paste0("Matched DiD: Effect of public housing on " , "white_share"))

# asinh_pop_total
create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                        dep_var = "asinh_pop_total",
                        title = paste0("Matched DiD: Effect of public housing on " , "asinh_pop_total"))

# asinh_pop_black
create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                        dep_var = "asinh_pop_black",
                        title = paste0("Matched DiD: Effect of public housing on " , "asinh_pop_black"))

# asinh_pop_white
create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                        dep_var = "asinh_pop_white",
                        title = paste0("Matched DiD: Effect of public housing on " , "asinh_pop_white"))

# asinh_median_income
create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                        dep_var = "asinh_median_income",
                        title = paste0("Matched DiD: Effect of public housing on " , "asinh_median_income"))

# asinh_median_home_value
create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                        dep_var = "asinh_median_home_value",
                        title = paste0("Matched DiD: Effect of public housing on " , "asinh_median_home_value"))

# asinh_median_rent
create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                        dep_var = "asinh_median_rent",
                        title = paste0("Matched DiD: Effect of public housing on " , "asinh_median_rent"))

# population_density
create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                        dep_var = "population_density",
                        title = paste0("Matched DiD: Effect of public housing on " , "population_density"))

# lfp_rate
create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                        dep_var = "lfp_rate",
                        title = paste0("Matched DiD: Effect of public housing on " , "lfp_rate"))

# unemp_rate
create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                        dep_var = "unemp_rate",
                        title = paste0("Matched DiD: Effect of public housing on " , "unemp_rate"))

# housing_density
create_event_study_plot(reg_results_df =  event_study_sunab_coefs,
                        dep_var = "housing_density",
                        title = paste0("Matched DiD: Effect of public housing on " , "housing_density"))

dev.off()


## Plot heterogeneity results ----

### Project size ----

for(size_group in unique(tract_data_matched_with_size$size_group)) {
  
  pdf_file_name <- paste0(results_dir, "event_study_plots_size_group_", size_group, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  for(i in seq_along(outcome_variables)) {
      # Create and print the plot
    plot <- create_event_study_plot(reg_results_df =  event_study_sunab_size_coefs,
                                    dep_var = outcome_variables[i],
                                    category_filter = size_group,
                                    title = paste0("Matched DiD: Effect of public housing on ", outcome_variables[i], ", size group ", size_group ))
    
    
    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}

### Initial black share -----

for(black_share_group in unique(tract_data_matched_with_black_share$race_group)) {
  
  pdf_file_name <- paste0(results_dir, "event_study_plots_black_share_group_", black_share_group, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  for(i in seq_along(outcome_variables)) {
      # Create and print the plot
    plot <- create_event_study_plot(reg_results_df =  event_study_sunab_initial_share_coefs,
                                    dep_var = outcome_variables[i],
                                    category_filter = black_share_group,
                                    title = paste0("Matched DiD: Effect of public housing on ", outcome_variables[i], ", initial black share group ", black_share_group ))
    
    
    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}


### City by city ----

for(city in unique(tract_data_matched$city)) {
  
  pdf_file_name <- paste0(results_dir, "by_city/event_study_plots_city_", city, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  for(i in seq_along(outcome_variables)) {
      # Create and print the plot
    plot <- create_event_study_plot(reg_results_df =  event_study_sunab_city_coefs,
                                    dep_var = outcome_variables[i],
                                    category_filter = city,
                                    title = paste0("Matched DiD: Effect of public housing on ", outcome_variables[i], ", city ", city ))
    
    
    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}



# TODO: 7. Estimate Sant'Anna and Callaway (2021) DiD -----
data <- 
  tract_data_matched %>%
  # keep only treated units and matched controls
  filter(!is.na(match_group)) %>%
  group_by(match_group) %>%
  mutate(
    group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
    event_time = year - group_treatment_year) %>%
  ungroup() %>%
  # create varaible equal to treatment_year if treated, 0 otherwise
  mutate(treated_year = ifelse(treatment_group == 1, treatment_year, 0)) %>% 
  # convert tract_id to unique number
  mutate(tract_id = as.numeric(as.factor(tract_id)))
  


# I'm not sure I'm doing this correctly... Right now it's a matched DiD
# with Callaway and Sant'Anna (2021) DiD, but with no match fixed effects
# Not sure if this is what I would want to run... The size of the groups is gonna be pretty small
# I could run this with the entire sample with those controls
cs21 = att_gt(
  yname         = "asinh_pop_black",
  tname         = "year",
  idname        = "tract_id",
  gname         = "treated_year",
  xformla       = ~ distance_from_cbd + population_density + total_pop + city,       # controls
  control_group = "notyettreated", # Too few groups for "nevertreated" default
  clustervars   = "tract_id", 
  data          = data
)

cs21






# ### Defunct 2.5: trying out LASSO ----
# 
# lasso_variable_selection <- function(data, treatment, potential_vars, alpha = 1, nfolds = 10, max_iter = 1e6) {
#   
#   # testing 
#   # data <- matching_data
#   # treatment <- "treatment_group"
#   # potential_vars <- match_vars
#   
#   # Create a formula with all potential variables and interactions
#   full_formula <- as.formula(paste(treatment, "~", paste(potential_vars, collapse = " + ")))
#   
#   # Create model matrix and handle missing data
#   model_frame <- model.frame(full_formula, data = data, na.action = na.pass)
#   x <- model.matrix(full_formula, model_frame)[,-1]  # Remove intercept
#   y <- model_frame[[treatment]]
#   
#   # Remove rows with NA values
#   complete_cases <- complete.cases(x, y)
#   x <- x[complete_cases,]
#   y <- y[complete_cases]
#   
#   # Perform cross-validated LASSO
#   cv_model <- cv.glmnet(x, y, alpha = alpha, family = "binomial", nfolds = nfolds)
#   
#   # Get the best lambda
#   best_lambda <- cv_model$lambda.min
#   
#   # Fit the model with the best lambda
#   best_model <- glmnet(x, y, alpha = alpha, family = "binomial", lambda = best_lambda,
#                        maxit = max_iter)
#   
#   # Get the non-zero coefficients
#   selected_vars <- coef(best_model) %>%
#     as.matrix() %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column("Variable") %>%
#     filter(s0 != 0, Variable != "(Intercept)") %>%
#     pull(Variable)
#   
#   return(selected_vars)
# }
# 
# # version for lasso
# perform_matching_for_year <- function(data, treatment_year, match_vars) {
#   
#   # testing
#   data <- census_tract_data
#   treatment_year <- 1950
#   match_vars <- potential_vars
#   
#   # Filter data as before
#   matching_data <- data %>%
#     filter((treatment_group == 1 & treatment_year == !!treatment_year) | 
#              (treatment_group == 0 & year == !!treatment_year - 10)) %>%
#     filter(year == !!treatment_year - 10) %>% 
#     filter(total_pop != 0)
#   
#   # Perform LASSO variable selection
#   selected_vars <- lasso_variable_selection(matching_data, "treatment_group", match_vars)
#   
#   # Construct the formula using selected variables
#   formula_str <- paste("treatment_group ~", paste(selected_vars, collapse = " + "))
#   match_formula <- as.formula(formula_str)
#   
#   # Perform matching
#   m.out <- matchit(match_formula,
#                    data = matching_data, 
#                    exact = "city", 
#                    method = "nearest", ratio = 1,
#                    caliper = 0.2,
#                    std.caliper = TRUE)
#   
#   # Get matched data
#   matched_data <- match.data(m.out)
#   
#   # Add treatment year information
#   matched_data$matched_treatment_year <- treatment_year
#   
#   return(list(matched_data = matched_data, selected_vars = selected_vars))
# }
# 
# 
# 
# potential_vars <- c("black_share", "asinh_pop_total", "total_pop",
#                     "asinh_pop_black", "black_pop", "asinh_pop_white", "white_pop",
#                     "distance_from_cbd", "population_density",
#                     "distance_from_cbd:population_density",
#                     "distance_from_cbd:black_share")
# 
# # Get list of treatment years
# treatment_years <- unique(census_tract_data %>% filter(!is.na(treatment_year)) %>% pull(treatment_year))
# 
# # Perform matching for each treatment year
# matched_results <- list()
# for (year in treatment_years) {
#   cat("Processing year:", year, "\n")
#   matched_results[[ paste0("matched_data_", year)]] <-
#     perform_matching_for_year(census_tract_data, year, potential_vars)
# }
# 
# # Extract matched datasets and selected variables
# matched_datasets <- map(matched_results, "matched_data")
# selected_vars_by_year <- map(matched_results, "selected_vars")
# 
# # Combine all matched datasets
# final_matched_data <- bind_rows(matched_datasets) %>%
#   distinct(tract_id, .keep_all = TRUE)
# 
# # Merge matching information back to the original dataset
# tract_data_matched <- census_tract_data %>%
#   left_join(final_matched_data %>% 
#               select(tract_id, weights, matched_treatment_year, subclass),
#             by = "tract_id") %>% 
#   mutate(weights = ifelse(is.na(weights), 0, weights),
#          match_group = ifelse(!is.na(matched_treatment_year),
#                               paste(city, matched_treatment_year, subclass, sep = "_"),
#                               NA))
# 
# # Print selected variables for each year
# walk2(treatment_years, selected_vars_by_year, ~cat("Selected variables for", .x, ":", paste(.y, collapse = ", "), "\n\n"))
# 
# # Check balance for each year
# balance_checks <- map2(matched_datasets, treatment_years, ~{
#   m.out <- matchit(treatment_group ~ 1, data = .x, exact = "city")
#   cat("Balance check for year", .y, ":\n")
#   print(summary(m.out))
#   cat("\n")
# })
# 


