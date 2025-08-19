####
# Matching Algorithms with Replacement
# This script runs propensity score matching WITH replacement to maximize sample size
# while maintaining strong identification through better individual matches
####

library(MatchIt)
library(sf)
library(lmtest)
library(fixest)
library(here)
library(RColorBrewer)
library(tidyverse)

# libraries for covariate testing
library(cobalt)
library(tableone)
library(kableExtra)

rm(list=ls())

# 0. Set seed and parameters -----

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small", or "combined"
data_type <- "combined"

set.seed(123456L)
# set number of nearest neighbors for KNN matching
knn_neighbors <- 1

# define directories
merged_data_dir <- here("data", "derived", "merged", data_type)
map_output_dir <- here("output", "figures", "matched_did", data_type)
balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)

output_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset")

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
tracts_and_rings <-
  read_csv(event_study_rings_filepath) %>% 
  # exclude "outer ring"
  filter(location_type != "outer")

# 1. Data Preparation ------

# get treated tracts, unique, and merge onto census tract sample
treated_tracts_panel <-
  treated_tracts_panel_raw %>% 
  distinct(GISJOIN_1950, total_public_housing_units)

census_tract_data <-
  census_tract_sample_raw %>% 
  # calculate lat and long of centroid of each tract
  mutate(centroid = st_centroid(geom)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  select(-centroid) %>% 
  # merge on number of housing units in treated tracts
  left_join(treated_tracts_panel) %>%
  # merge on rings
  left_join(tracts_and_rings) %>% 
  # set location_type = "donor_pool" if it's NA
  mutate(location_type = if_else(is.na(location_type), "donor_pool", location_type)) %>%
  # create transformed variables
  mutate(asinh_pop_total = asinh(total_pop),
         asinh_pop_white = asinh(white_pop),
         asinh_pop_black = asinh(black_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent_calculated = asinh(median_rent_calculated),
         asinh_median_home_value_calculated = asinh(median_home_value_calculated),
         asinh_distance_from_cbd = asinh(distance_from_cbd),
         ln_population_density = log(population_density),
         asinh_private_population_estimate = asinh(private_population_estimate),
         asinh_private_black_population_estimate = asinh(private_black_population_estimate),
         asinh_private_white_population_estimate = asinh(private_white_population_estimate),
         log_private_units_estimate = asinh(total_private_units_estimate)
         ) %>% 
  # create county identifier
  mutate(county_id = paste0(STATEA, COUNTYA)) %>% 
  dplyr::rename(year = YEAR) %>% 
  st_drop_geometry() %>% 
  # Ensure data is sorted by tract_id and year
  arrange(GISJOIN_1950, year)  %>% 
  # for HOLC variables (grade and category) if category is missing, set to "missing"
  mutate(category_most_overlap = ifelse(is.na(category_most_overlap), "missing", category_most_overlap),
         grade_most_overlap = ifelse(is.na(grade_most_overlap), "missing", grade_most_overlap))

# Define group types and matching variables -----
group_types <- c("treated", "inner")

# Matching variables based on site selection analysis and theoretical importance
matching_vars <- c(
  "asinh_distance_from_cbd",     # Geographic/transportation access
  "asinh_pop_total",             # Neighborhood size
  "black_share",                 # Racial composition (key outcome driver)
  "unemp_rate",                  # Economic conditions
  "asinh_median_income",         # Socioeconomic status
  "median_rent_calculated"       # Housing market conditions
  )

# Function to perform matching WITH REPLACEMENT
perform_matching_with_replacement <- function(data, treatment_year, match_vars, nearest_neighbors, group_type,
                                             match_type = "nearest", distance_option = "glm",
                                             match_link = "logit", 
                                             pre_treatment_decades = 2,
                                             exact_match_vars = c("cbsa_title", "redlined_binary_80pp", "ur_binary_5pp", "has_highway_1km")) {
  
  # for testing
  # data <- census_tract_data
  # treatment_year <- 1970  # or 1960, 1980 - pick one to test
  # match_vars <- matching_vars
  # nearest_neighbors <- knn_neighbors
  # distance_option <- "glm"
  # match_link <- "logit"
  # group_type <- "treated"  # test treated group first
  # match_type <- "nearest"
  # pre_treatment_decades <- 2
  # exact_match_vars <- c("cbsa_title", "redlined_binary_80pp")
  
  
  
  # Determine pre-treatment years
  if (pre_treatment_decades == 1) {
    pre_treatment_years <- c(treatment_year - 10)
  } else if (pre_treatment_decades == 2) {
    pre_treatment_years <- c(treatment_year - 10, treatment_year - 20)
  }
  
  # Separate time-invariant and time-varying variables
  time_invariant_vars <- c("asinh_distance_from_cbd")
  time_varying_vars <- setdiff(match_vars, time_invariant_vars)
  
  # Filter data for matching
  matching_data <- data %>%
    filter(
      (location_type == group_type & treatment_year == !!treatment_year) | # Treatment group
      (location_type == "donor_pool" & year %in% pre_treatment_years))     # Control pool
  
  # Convert category to character
  matching_data <- matching_data %>%
    mutate(category_most_overlap = as.character(category_most_overlap))
  
  # Reshape to wide format
  matching_data_wide <- matching_data %>%
    dplyr::select(GISJOIN_1950, city, cbd, cbsa_title,
                  redlined_binary_80pp, category_most_overlap, location_type, 
                  year, all_of(c(time_varying_vars, time_invariant_vars))) %>%
    pivot_wider(
      names_from = year,
      values_from = all_of(time_varying_vars),
      names_glue = "year_{year}_{.value}"
    )
  
  # Get relevant variables for matching
  relevant_vars <- c(
    paste0("year_", rep(pre_treatment_years, each = length(time_varying_vars)),
           "_", rep(time_varying_vars, times = length(pre_treatment_years))),
    time_invariant_vars
  )
  
  # Filter complete cases
  complete_cases <- matching_data_wide %>%
    dplyr::select(all_of(relevant_vars)) %>%
    complete.cases()
  
  matching_data_wide <- matching_data_wide %>%
    filter(complete_cases)
  
  # Create treatment indicator
  matching_data_wide <- matching_data_wide %>%
    mutate(treatment_group = as.factor(ifelse(location_type == group_type, 1, 0)))
  
  # Create matching formula
  formula_str <- paste("treatment_group ~", paste(relevant_vars, collapse = " + "))
  match_formula <- as.formula(formula_str)
  
  cat("Matching with replacement for", group_type, "tracts treated in", treatment_year, "\n")
  cat("Treatment units:", sum(matching_data_wide$treatment_group == 1), "\n")
  cat("Control units:", sum(matching_data_wide$treatment_group == 0), "\n")
  
  # Perform matching WITH REPLACEMENT
  m.out <- matchit(match_formula,
                   data = matching_data_wide,
                   exact = exact_match_vars,
                   method = match_type,
                   distance = distance_option,
                   ratio = nearest_neighbors,
                   link = match_link,
                   replace = TRUE)  # KEY CHANGE: WITH REPLACEMENT
  
  # Print matching summary
  cat("Matched units:", sum(m.out$weights[m.out$weights > 0]), "\n")
  cat("Control units used:", sum(m.out$weights[matching_data_wide$treatment_group == 0] > 0), "\n")
  cat("Controls used multiple times:", 
      sum(m.out$weights[matching_data_wide$treatment_group == 0] > 1), "\n\n")
  
  # Get matched data using get_matches() for replacement matching
  matched_data <- get_matches(m.out, data = matching_data_wide)
  
  # Add treatment year and group type information
  matched_data$matched_treatment_year <- treatment_year
  matched_data$group_type <- group_type
  matched_data$match_type <- match_type
  
  # Get variable names for reshaping
  year_vars <- grep("^year_", names(matched_data), value = TRUE)
  
  # Reshape back to long format
  matched_data_long <- matched_data %>%
    mutate(across(contains("category"), as.character)) %>%
    pivot_longer(
      cols = all_of(year_vars),
      names_to = c("year", ".value"),
      names_pattern = "year_([0-9]+)_(.+)"
    ) %>%
    mutate(year = as.integer(year))
  
  return(list(matched_data = matched_data_long, m.out = m.out))
}

# Run matching WITH REPLACEMENT -----
treatment_years <- unique(census_tract_data %>%
                            filter(!is.na(treatment_year)) %>%
                            pull(treatment_year))

# Initialize lists to store results
matched_datasets_replacement <- list()
m_out_objects_replacement <- list()

# Perform matching for each group and year
for (group in group_types) {
  for (year in treatment_years) {
    for (nyear in c(1, 2)) {
      df_name <- paste0("matched_data_", group, "_", nyear, "pretreatment_", year, "_replacement")
      m_out_name <- paste0("m_out_", group, "_", nyear, "pretreatment_", year, "_replacement")
      
      cat("=== Matching with replacement ===\n")
      cat("Group:", group, "| Year:", year, "| Pre-treatment decades:", nyear, "\n")
      
      result <- perform_matching_with_replacement(
        data = census_tract_data, 
        treatment_year = year, 
        match_vars = matching_vars,
        nearest_neighbors = knn_neighbors, 
        group_type = group,
        pre_treatment_decades = nyear,
        distance_option = "glm",
        match_type = "nearest",
        match_link = "logit",
        exact_match_vars = c("cbsa_title", "redlined_binary_80pp")
      )
      
      matched_datasets_replacement[[df_name]] <- result[["matched_data"]]
      m_out_objects_replacement[[m_out_name]] <- result[["m.out"]]
    }
  }
}

# Create combined matched datasets -----

# Combine 2-year pretreatment matching (primary specification)
all_matched_data_2_year_replacement <- 
  bind_rows(matched_datasets_replacement$matched_data_treated_2pretreatment_1950_replacement,
            matched_datasets_replacement$matched_data_treated_2pretreatment_1960_replacement,
            matched_datasets_replacement$matched_data_treated_2pretreatment_1970_replacement,
            matched_datasets_replacement$matched_data_treated_2pretreatment_1980_replacement,
            matched_datasets_replacement$matched_data_inner_1pretreatment_1950_replacement,
            matched_datasets_replacement$matched_data_inner_1pretreatment_1960_replacement,
            matched_datasets_replacement$matched_data_inner_1pretreatment_1970_replacement,
            matched_datasets_replacement$matched_data_inner_1pretreatment_1980_replacement
            ) %>% 
  # calculate weights based on number of unique matches
  select(GISJOIN_1950, matched_treatment_year, subclass, group_type) %>%
  distinct() %>% 
  mutate(weights = 1)
# %>%
#   # 8/11/2025: Do NOT use these weights. Just set weights equal to 1
#   group_by(GISJOIN_1950) %>%
#   mutate(
#     times_matched = n(),                    # How many unique matches this tract has
#     weights = 1/times_matched,    # for controls that matched k times, weight = 1/k
#   ) %>%
#   ungroup()


# Merge matching information back to original dataset
tract_data_matched_2_year_replacement <- census_tract_data %>%
  left_join(all_matched_data_2_year_replacement,
            by = "GISJOIN_1950",
            relationship = "many-to-many") %>% 
  # Create match group identifier
  mutate(
    match_group = ifelse(!is.na(matched_treatment_year),
                         paste(cbsa_title, matched_treatment_year, subclass, sep = "_"),
                         NA)
  ) %>% 
  # Keep only matched observations
  filter(!is.na(match_group))

# Summary statistics -----
cat("\n=== MATCHING WITH REPLACEMENT SUMMARY ===\n")
cat("Original treated tracts (all years):", 
    nrow(census_tract_data %>% filter(location_type == "treated") %>% distinct(GISJOIN_1950)), "\n")

matched_summary <- tract_data_matched_2_year_replacement %>%
  group_by(group_type, location_type) %>%
  summarise(
    n_obs = n(),
    n_tracts = n_distinct(GISJOIN_1950),
    n_match_groups = n_distinct(match_group),
    .groups = 'drop'
  )

print(matched_summary)

# Check balance -----
cat("\n=== BALANCE CHECK ===\n")

# Covariates for balance checking
covariates <- c("black_share", "white_share", "asinh_median_income", 
                "median_rent_calculated", "distance_from_cbd", 
                "total_pop", "asinh_pop_total", "unemp_rate")

# Balance for treated group
balance_data_treated <- tract_data_matched_2_year_replacement %>%
  filter(group_type == "treated", year == 1940) # Pre-treatment year

if (nrow(balance_data_treated) > 0) {
  balance_table_treated <- CreateTableOne(
    vars = covariates,
    strata = "location_type",
    data = balance_data_treated,
    test = TRUE
  )
  
  cat("Balance for TREATED group matching:\n")
  print(balance_table_treated, smd = TRUE)
}

# Balance for inner gorup
balance_data_inner <- tract_data_matched_2_year_replacement %>%
  filter(group_type == "inner", year == 1940) # Pre-treatment year

if (nrow(balance_data_inner) > 0) {
  balance_table_inner <- CreateTableOne(
    vars = covariates,
    strata = "location_type",
    data = balance_data_inner,
    test = TRUE
  )
  
  cat("Balance for INNER group matching:\n")
  print(balance_table_inner, smd = TRUE)
}

# Save datasets -----
cat("\n=== SAVING DATASETS ===\n")

write_csv(tract_data_matched_2_year_replacement, 
          here(output_data_dir, "tract_data_matched_2_year_replacement.csv"))

cat("Dataset saved to:", here(output_data_dir, "tract_data_matched_2_year_replacement.csv"), "\n")

# Save matching objects for diagnostics
# saveRDS(m_out_objects_replacement, 
#         here(output_data_dir, "m_out_objects_replacement.rds"))

cat("\n=== MATCHING WITH REPLACEMENT COMPLETE ===\n")

