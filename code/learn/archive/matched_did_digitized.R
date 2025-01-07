library(MatchIt)
library(tidyverse)
library(lmtest)
library(sandwich)
# libraries for covariate testing
library(cobalt)
library(tableone)
#library for Sant'Anna and Zhou (2020)
library(DRDID)
# library for Callaway and Sant'Anna (2021)
library(did)

rm(list=ls())


# 0. Set seed and parameters -----

set.seed(123456L)
knn_neighbors <- 3

# choose which data to use
data_type <- "digitized"



# 1. Data Preparation ------

# if data_type == "digitized"
if (data_type == "digitized") {
  # read in Census tract data with treatment status
  census_tract_sample_raw <-
    st_read("data/derived/merged/census_tract_sample_with_treatment_status_digitized.gpkg")
  
  treated_tracts_panel_raw <-
    st_read("data/derived/merged/treated_tracts_panel_digitized.gpkg") %>% 
    st_drop_geometry()
} else if (data_type == "cdd") {
  # read in Census tract data with treatment status
  census_tract_sample_raw <-
    st_read("data/derived/merged/census_tract_sample_with_treatment_status_full_count.gpkg")
  
  treated_tracts_panel_raw <-
    st_read("data/derived/merged/treated_tracts_panel_full_count.gpkg") %>% 
    st_drop_geometry()
} else {
  stop("data_type must be either 'digitized' or 'cdd'")
}

# read in HOLC data
holc_data <-
  read_csv("data/derived/holc/tract_holc_classifications.csv") %>% 
  select(-contains("city"), -state)


# get treated tracts, unique, and merge onto census tract sample
treated_tracts_panel <-
  treated_tracts_panel_raw %>% 
  distinct(STATE, COUNTY, TRACTA, treatment_year) %>% 
  mutate(treatment_group = 1)

census_tract_data <-
  left_join(census_tract_sample_raw, treated_tracts_panel) %>% 
  # merge on HOLC data
  left_join(holc_data) %>% 
  # set treatment_group = 0 if NA
  mutate(treatment_group = ifelse(is.na(treatment_group), 0, treatment_group)) %>% 
  # create unique id for each tract
  mutate(tract_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
  # # select relevant columns
  # select(tract_id, YEAR, black_share, white_share, city,
  #        distance_from_cbd, population_density, median_income, median_home_value, 
  #        median_rent, treatment_year, treated, treatment_group, total_pop, white_pop, black_pop,
  #        population_density, share_needing_repair, 
  #        employment_pop_ratio, unemp_rate, lfp_rate) %>% 
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
        


# 2. Create datasets for propensity score matching ------
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
    filter(total_pop != 0 )
  
  # Construct the formula dynamically
  formula_str <- paste("treatment_group ~", paste(match_vars, collapse = " + "))
  match_formula <- as.formula(formula_str)
  
  # Step 2: KNN matching
  m.out <- matchit(match_formula,
                   data = matching_data, 
                   exact = "city", 
                   method = "nearest",
                   distance = "glm",
                   ratio = nearest_neighbors,
                   #add caliper 
                   caliper = 0.2,
                   std.caliper = TRUE)
  
  # Get matched data
  matched_data <- match.data(m.out)
  
  # Add treatment year information
  matched_data$matched_treatment_year <- treatment_year
  
  return(matched_data)
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
  unique(census_tract_data %>% filter(!is.na(treatment_year)) %>% pull(treatment_year))


# Initialize an empty list to store matched datasets
matched_datasets <- list()

# Perform matching for each treatment year
for (year in treatment_years) {
  df_name <- paste0("matched_data_", year)
  matched_datasets[[df_name]] <- 
    perform_matching_for_year(census_tract_data, year, matching_vars, knn_neighbors)
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

## KNN matching  ----
# dataset of just matched units at pre-treatment period
balanced_data <- tract_data_matched %>%
  filter(!is.na(match_group)) %>%
  group_by(match_group) %>%
  mutate(group_treatment_year = min(matched_treatment_year, na.rm = TRUE)) %>%
  filter(year < group_treatment_year) %>%
  ungroup()

# List of covariates you used for matching
covariates <- c("black_share", "white_share",  "asinh_median_income", "median_income",
                "median_home_value", "asinh_median_home_value", "median_rent","asinh_median_rent", 
                "distance_from_cbd", "population_density", "total_pop", "black_pop",
                "asinh_pop_black", "asinh_pop_white", "asinh_pop_total")  # Add or modify as needed

# Create table
balance_table <- CreateTableOne(vars = covariates,
                                strata = "treatment_group",
                                data = balanced_data,
                                test = FALSE)

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
outcome_variables <- c("black_share", "white_share", "asinh_pop_total", "asinh_pop_black",
                       "asinh_pop_white",
                       "asinh_median_income", "asinh_median_home_value", "asinh_median_rent",
                       "population_density")

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

did_event_study <- function(data, outcome_var) {
  
  data <- tract_data_matched
  #outcome_var <- "black_share"
  
  data <- data %>%
    # keep only treated units and matched controls
    filter(!is.na(match_group)) %>%
    group_by(match_group) %>%
    mutate(
      group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
      event_time = year - group_treatment_year) %>%
    ungroup()
  
  # Create the formula for the event study
  # baseline model
  formula <- as.formula(paste(outcome_var, "~ i(event_time, treatment_group, ref = -10) | city^year + tract_id + match_group"))
  model <- feols(formula, data = data, weights = data$weights, cluster = "tract_id")
  
  
  # Sun and Abraham model
  sunab_data <- 
    data %>% 
    # set treatment_year = 100 if missing
    mutate(treatment_year = ifelse(is.na(treatment_year), 100, treatment_year))
  
  formula_sunab <- as.formula(paste(outcome_var, "~ sunab(treatment_year, year, ref.p = -10) | year^city + tract_id + match_group"))

  model_sunab <- feols(formula_sunab, data = sunab_data, cluster = "tract_id")
  
  return(list(model, model_sunab))
}

# initialize lists
did_results_event_study_twfe <- list()
did_results_event_study_sunab <- list()

# save regression output to lists
for (outcome in outcome_variables) {
  did_results_event_study_twfe[[outcome]] <- did_event_study(tract_data_matched, outcome)[[1]]
  did_results_event_study_sunab[[outcome]] <- did_event_study(tract_data_matched, outcome)[[2]]
}

did_results_event_study_twfe
did_results_event_study_sunab

summary(did_results_event_study_sunab$black_share, agg = "ATT")
summary(did_results_event_study_sunab$asinh_pop_black, agg = "ATT")
summary(did_results_event_study_sunab$asinh_pop_white, agg = "ATT")
summary(did_results_event_study_sunab$asinh_pop_total, agg = "ATT")
summary(did_results_event_study_sunab$asinh_median_income, agg = "ATT")
summary(did_results_event_study_sunab$asinh_median_home_value, agg = "ATT")
summary(did_results_event_study_sunab$asinh_median_rent, agg = "ATT")
summary(did_results_event_study_sunab$population_density, agg = "ATT")

# No match group FE: Results for black_share, white_share, pop, black pop, white pop, rent maybe, population density
# With match group FE: Black share, white share, Tot pop, black pop, white pop, rent maybe, population density
# Including city X time FE:
# looks good either way!

# ATTs

# 6. Plot results -----

did_results_event_study_twfe$asinh_pop_black |>
  iplot(
    main     = "(Asinh) Black Population",
    xlab     = "Time to treatment")

did_results_event_study_sunab$asinh_pop_white |>
  iplot(
    main     = "(Asinh) White Population",
    xlab     = "Time to treatment")

did_results_event_study_sunab$asinh_pop_total |>
  iplot(
    main     = "(Asinh) Total Population",
    xlab     = "Time to treatment")

did_results_event_study_sunab$black_share |>
  iplot(
    main     = "Black Share",
    xlab     = "Time to treatment")

did_results_event_study_sunab$white_share |>
  iplot(
    main     = "White Share",
    xlab     = "Time to treatment")



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


