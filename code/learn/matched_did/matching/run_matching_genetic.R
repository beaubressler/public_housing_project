####
# Genetic Matching with Replacement
# Uses genetic algorithm to optimize covariate weights for maximum balance
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
library(tinytable) # for formatting balance tables

# rm(list=ls())  # Commented out to preserve loop variables in complete analysis script

# 0. Load shared configuration -----
source(here("code", "learn", "matched_did", "matching", "matching_config.R"))

# output directory
variant <- "genetic"

# Load helper functions
source(here("code", "learn", "matched_did", "helpers", "balance_table_creation.R"))
source(here("code", "learn", "matched_did", "helpers", "time_series_plotting.R"))


# define directories
merged_data_dir <- here("data", "derived", "merged", data_type)
map_output_dir <- here("output", "figures", "matched_did", data_type)

output_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset", variant)
dir.create(output_data_dir, recursive = TRUE, showWarnings = FALSE)


figures_dir <- here("output", "figures", "matched_did", data_type, variant)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

balance_table_dir <- here("output", "balance_tables", "matched_did", data_type, variant)
dir.create(balance_table_dir, recursive = TRUE, showWarnings = FALSE)


set.seed(123)

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

# data on total public housing units inner ring is exposed to in t=0, calculated in spatial_did build
inner_ring_units <- 
  read_csv(here(merged_data_dir, "inner_ring_tracts_first_treated.csv")) %>% 
  select(-treatment_year)


# Load matching function
source(here("code", "helpers", "matching_functions.R"))


# 1. Data Preparation ------

# get treated tracts, unique, and merge onto census tract sample
treated_tracts_panel <-
  treated_tracts_panel_raw %>%
  distinct(GISJOIN_1950, total_public_housing_units,
           total_original_project_size, largest_original_project_size,
           has_highrise_project)

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
         grade_most_overlap = ifelse(is.na(grade_most_overlap), "missing", grade_most_overlap)) %>% 
  # merge on inner ring units
  left_join(inner_ring_units, by = c("GISJOIN_1950" = "GISJOIN_1950"))

# Use exact matching variables to match baseline variant
exact_matching_vars <- c("county_id", "ur_binary_5pp")

genetic_matching_vars <- c("asinh_pop_total", "asinh_pop_black", "black_share",
                           "asinh_median_income", "asinh_median_rent_calculated",
                           "lfp_rate", "unemp_rate")


# Function to perform matching with replacement
perform_matching_with_replacement <- function(data, treatment_year, match_vars, nearest_neighbors, group_type,
                                             match_type = "nearest", distance_option = "glm",
                                             match_link = "logit", 
                                             pre_treatment_decades = 2,
                                             exact_match_vars = c("county_id", "redlined_binary_80pp", "ur_binary_5pp"),
                                             caliper = NULL) {
  
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
  # exact_match_vars <- exact_matching_vars
  
  
  
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
    dplyr::select(GISJOIN_1950, city, cbd, cbsa_title, county_id,
                  redlined_binary_80pp, ur_binary_5pp, category_most_overlap, location_type, 
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
  
  # Perform genetic matching WITH REPLACEMENT
  m.out <- matchit(match_formula,
                   data = matching_data_wide,
                   exact = exact_match_vars,
                   method = "genetic",
                   distance = distance_option,
                   ratio = nearest_neighbors,
                   replace = TRUE,
                   pop.size = 100,  # genetic algorithm population size
                   caliper = caliper,
                   std.caliper = TRUE)
  
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
  matched_data$match_type <- "genetic"
  
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

# Run matching with replacement -----
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
        match_vars = genetic_matching_vars,
        nearest_neighbors = knn_neighbors, 
        group_type = group,
        pre_treatment_decades = nyear,
        distance_option = "glm",
        match_type = "nearest",
        match_link = "logit",
        #caliper = 0.4,
        exact_match_vars =exact_matching_vars
      )
      
      matched_datasets_replacement[[df_name]] <- result[["matched_data"]]
      m_out_objects_replacement[[m_out_name]] <- result[["m.out"]]
    }
  }
}

# Create combined matched datasets -----

# Combine 1-year pretreatment matching (primary specification)
all_matched_data_1_year_replacement <- 
  bind_rows(matched_datasets_replacement$matched_data_treated_1pretreatment_1950_replacement,
            matched_datasets_replacement$matched_data_treated_1pretreatment_1960_replacement,
            matched_datasets_replacement$matched_data_treated_1pretreatment_1970_replacement,
            matched_datasets_replacement$matched_data_treated_1pretreatment_1980_replacement,
            matched_datasets_replacement$matched_data_inner_1pretreatment_1950_replacement,
            matched_datasets_replacement$matched_data_inner_1pretreatment_1960_replacement,
            matched_datasets_replacement$matched_data_inner_1pretreatment_1970_replacement,
            matched_datasets_replacement$matched_data_inner_1pretreatment_1980_replacement
            ) %>% 
  # calculate weights based on number of unique matches
  select(GISJOIN_1950, matched_treatment_year, subclass, group_type) %>%
  distinct() %>% 
  mutate(weights = 1)

# Combine 2-year pretreatment matching
all_matched_data_2_year <- bind_rows(
  matched_datasets_replacement$matched_data_treated_2pretreatment_1950_replacement,
  matched_datasets_replacement$matched_data_treated_2pretreatment_1960_replacement,
  matched_datasets_replacement$matched_data_treated_2pretreatment_1970_replacement,
  matched_datasets_replacement$matched_data_treated_2pretreatment_1980_replacement,
  matched_datasets_replacement$matched_data_inner_2pretreatment_1950_replacement,
  matched_datasets_replacement$matched_data_inner_2pretreatment_1960_replacement,
  matched_datasets_replacement$matched_data_inner_2pretreatment_1970_replacement,
  matched_datasets_replacement$matched_data_inner_2pretreatment_1980_replacement
) %>%
  select(GISJOIN_1950, matched_treatment_year, subclass, group_type) %>%
  distinct() %>%
  mutate(weights = 1)


# Merge matching information back to original dataset
tract_data_matched_1_year <- census_tract_data %>%
  left_join(all_matched_data_1_year_replacement,
            by = "GISJOIN_1950",
            relationship = "many-to-many") %>% 
  # Create match group identifier
  mutate(
    match_group = ifelse(!is.na(matched_treatment_year),
                    paste(matched_treatment_year, subclass, sep = "_"),
                         NA)
  ) %>% 
  # Keep only matched observations
  filter(!is.na(match_group))

# Merge for 2-year
tract_data_matched_2_year <- census_tract_data %>%
  left_join(all_matched_data_2_year, by = "GISJOIN_1950", relationship = "many-to-many") %>%
  mutate(match_group = ifelse(!is.na(matched_treatment_year),
                              paste(matched_treatment_year, subclass, sep = "_"), NA)) %>%
  filter(!is.na(match_group))


# Summary statistics -----
cat("\n=== MATCHING WITH REPLACEMENT SUMMARY ===\n")
cat("Original treated tracts (all years):", 
    nrow(census_tract_data %>% filter(location_type == "treated") %>% distinct(GISJOIN_1950)), "\n")

matched_summary <- tract_data_matched_1_year %>%
  group_by(group_type, location_type) %>%
  summarise(
    n_obs = n(),
    n_tracts = n_distinct(GISJOIN_1950),
    n_match_groups = n_distinct(match_group),
    .groups = 'drop'
  )

print(matched_summary)


# Additional cleaning -----
## Clean private population ----
# For each match group, if ANY observation has missing private_population_estimate,
# set ALL observations in that match group to NA for private population variables

tract_data_matched_1_year <- tract_data_matched_1_year %>%
  group_by(match_group, group_type) %>%
  mutate(
    has_missing_private_pop = any(is.na(private_population_estimate)),
    private_population_estimate_y = ifelse(has_missing_private_pop, NA, private_population_estimate),
    private_black_population_estimate = ifelse(has_missing_private_pop, NA, private_black_population_estimate),
    private_white_population_estimate = ifelse(has_missing_private_pop, NA, private_white_population_estimate),
    asinh_private_population_estimate = ifelse(has_missing_private_pop, NA, asinh_private_population_estimate),
    asinh_private_black_population_estimate = ifelse(has_missing_private_pop, NA, asinh_private_black_population_estimate),
    asinh_private_white_population_estimate = ifelse(has_missing_private_pop, NA, asinh_private_white_population_estimate)
  ) %>%
  ungroup()

## Merge spillover project size data -----
# For inner ring matching, we need to ensure spillover characteristics flow through to controls

# Extract spillover characteristics for inner ring match groups
# Get one spillover value per inner ring match group (from the inner ring tract)
inner_spillover_characteristics <- tract_data_matched_1_year %>%
  filter(group_type == "inner", location_type == "inner", 
         !is.na(total_public_housing_units_built_nearby)) %>%
  select(match_group, total_public_housing_units_built_nearby) %>%
  distinct() %>%
  mutate(
    spillover_size_bin = ntile(total_public_housing_units_built_nearby, 3),
    spillover_size_group = factor(case_when(
      spillover_size_bin == 1 ~ "Small Nearby",
      spillover_size_bin == 2 ~ "Medium Nearby", 
      spillover_size_bin == 3 ~ "Large Nearby"
    ), levels = c("Small Nearby", "Medium Nearby", "Large Nearby")),
    log_spillover_units = log(total_public_housing_units_built_nearby)
  )

# Merge spillover characteristics to all observations in inner ring match groups
tract_data_matched_1_year <- 
  tract_data_matched_1_year %>%
  select(-total_public_housing_units_built_nearby) %>% 
  left_join(inner_spillover_characteristics, 
            by = "match_group") %>% 
  # if treated, set to NA
  mutate(
    total_public_housing_units_built_nearby = ifelse(location_type == "treated", NA, total_public_housing_units_built_nearby),
    spillover_size_group = ifelse(location_type == "treated", NA, spillover_size_group),
    log_spillover_units = ifelse(location_type == "treated", NA, log_spillover_units)
  )

cat("\n=== SPILLOVER SIZE DATA MERGED ===\n")
spillover_check <- tract_data_matched_1_year %>%
  filter(group_type == "inner") %>%
  group_by(location_type) %>%
  summarise(
    n_obs = n(),
    n_with_spillover_data = sum(!is.na(total_public_housing_units_built_nearby)),
    n_with_size_groups = sum(!is.na(spillover_size_group)),
    .groups = 'drop'
  )
print(spillover_check)

cat("\nShould see spillover data for BOTH inner ring tracts and their matched donor pool controls\n")

# Create balance tables using modularized function ----
create_balance_tables(tract_data_matched_1_year, tract_data_matched_2_year, balance_table_dir)


# Save datasets -----
cat("\n=== SAVING DATASETS ===\n")

write_csv(tract_data_matched_1_year, 
          here(output_data_dir, "tract_data_matched_1_year.csv"))

write_csv(tract_data_matched_2_year,
          here(output_data_dir, "tract_data_matched_2_year.csv"))


cat("Datasets saved to:\n",
    here(output_data_dir, "tract_data_matched_1_year.csv"), "\n",
    here(output_data_dir, "tract_data_matched_2_year.csv"), "\n")


# Save matching objects for diagnostics
# saveRDS(m_out_objects_replacement, 
#         here(output_data_dir, "m_out_objects_replacement.rds"))

# Analyze matching dropouts -----
cat("\n=== MATCHING DROPOUT ANALYSIS ===\n")

# Get all treated tracts that went into matching
all_treated_tracts <- census_tract_data %>%
  filter(location_type == "treated") %>%
  distinct(GISJOIN_1950, treatment_year, county_id, redlined_binary_80pp, ur_binary_5pp, cbsa_title)

# Get treated tracts that successfully matched
matched_treated_tracts <- tract_data_matched_1_year %>%
  filter(location_type == "treated") %>%
  distinct(GISJOIN_1950, matched_treatment_year, county_id, redlined_binary_80pp, ur_binary_5pp, cbsa_title) %>%
  rename(treatment_year = matched_treatment_year)

# Find unmatched tracts
unmatched_tracts <- all_treated_tracts %>%
  anti_join(matched_treated_tracts, by = c("GISJOIN_1950", "treatment_year"))

cat("Total treated tracts:", nrow(all_treated_tracts), "\n")
cat("Successfully matched treated tracts:", nrow(matched_treated_tracts), "\n") 
cat("Unmatched treated tracts:", nrow(unmatched_tracts), "\n\n")

if (nrow(unmatched_tracts) > 0) {
  cat("REASONS FOR MATCHING FAILURE:\n")
  
  # Check exact matching constraint failures
  cat("\n1. County distribution of unmatched tracts:\n")
  county_summary <- unmatched_tracts %>%
    count(county_id, cbsa_title, sort = TRUE)
  print(county_summary)
  
  cat("\n2. Redlining status of unmatched tracts:\n")
  redlining_summary <- unmatched_tracts %>%
    count(redlined_binary_80pp, sort = TRUE)
  print(redlining_summary)
  
  cat("\n3. Urban renewal status of unmatched tracts:\n") 
  ur_summary <- unmatched_tracts %>%
    count(ur_binary_5pp, sort = TRUE)
  print(ur_summary)
  
  cat("\n4. Combined exact matching combinations for unmatched tracts:\n")
  exact_combo_summary <- unmatched_tracts %>%
    count(county_id, redlined_binary_80pp, ur_binary_5pp, cbsa_title, sort = TRUE)
  print(head(exact_combo_summary, 10))
  
  # Save unmatched tracts for further analysis
  write_csv(unmatched_tracts, here(output_data_dir, "unmatched_treated_tracts.csv"))
  cat("\nUnmatched tracts saved to:", here(output_data_dir, "unmatched_treated_tracts.csv"), "\n")
}

cat("\n=== DROPOUT ANALYSIS COMPLETE ===\n")

# Create time series plots using modularized function ----
variables_to_plot <- list(
  list(var = "black_share", label = "Black Share", percent = TRUE),
  list(var = "black_pop", label = "Black pop", percent = FALSE),
  list(var = "total_pop", label = "Total Population", percent = FALSE),
  list(var = "median_income", label = "Median Income (1950$)", percent = FALSE)
)

# Create time series for 1-year matched sample
cat("\n=== CREATING TIME SERIES FOR 1-YEAR MATCHED SAMPLE ===\n")
create_multiple_time_series(
  data = tract_data_matched_1_year,
  variables_to_plot = variables_to_plot,
  figures_dir = here("output", "time_series", data_type, variant, "1yr")
)

# Create time series for 2-year matched sample
cat("\n=== CREATING TIME SERIES FOR 2-YEAR MATCHED SAMPLE ===\n")
create_multiple_time_series(
  data = tract_data_matched_2_year,
  variables_to_plot = variables_to_plot,
  figures_dir = here("output", "time_series", data_type, variant, "2yr")
)


