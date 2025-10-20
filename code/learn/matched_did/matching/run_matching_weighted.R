####
# Weighted Matching for Public Housing Analysis
# Each treated tract gets matched to all control tracts in same county
# using distance-based weights (overlap, kernel, or IPW)
####

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
library(tinytable)

# 0. Load shared configuration -----
source(here("code", "learn", "matched_did", "matching", "matching_config.R"))

variant <- "weighted"

# Load helper functions
source(here("code", "learn", "matched_did", "helpers", "balance_table_creation.R"))
source(here("code", "learn", "matched_did", "helpers", "time_series_plotting.R"))
source(here("code", "learn", "matched_did", "matching", "helpers", "compute_weighted_matches.R"))

# define directories
merged_data_dir <- here("data", "derived", "merged", data_type)
output_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset", variant)
dir.create(output_data_dir, recursive = TRUE, showWarnings = FALSE)

figures_dir <- here("output", "figures", "matched_did", data_type, variant)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

balance_table_dir <- here("output", "balance_tables", "matched_did", data_type, variant)
dir.create(balance_table_dir, recursive = TRUE, showWarnings = FALSE)

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
  filter(location_type != "outer")

# data on total public housing units inner ring is exposed to
inner_ring_units <-
  read_csv(here(merged_data_dir, "inner_ring_tracts_first_treated.csv")) %>%
  select(-treatment_year)

## !! Set matching variables
match_vars <- c("asinh_pop_total", "asinh_pop_black",
                "asinh_median_rent_calculated", "asinh_median_income",
                "unemp_rate", "lfp_rate")

time_invariant_vars <- c("asinh_distance_from_cbd")
exact_match_vars <- c("redlined_binary_80pp", "ur_binary_5pp")

# Use overlap weights
weight_method <- "overlap"

# 1. Data Preparation ------
treated_tracts_panel <-
  treated_tracts_panel_raw %>%
  distinct(GISJOIN_1950, total_public_housing_units,
           total_original_project_size, largest_original_project_size,
           has_highrise_project)

census_tract_data <-
  census_tract_sample_raw %>%
  mutate(centroid = st_centroid(geom)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  select(-centroid) %>%
  left_join(treated_tracts_panel) %>%
  left_join(tracts_and_rings) %>%
  mutate(location_type = if_else(is.na(location_type), "donor_pool", location_type)) %>%
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
  mutate(county_id = paste0(STATEA, COUNTYA)) %>%
  dplyr::rename(year = YEAR) %>%
  st_drop_geometry() %>%
  arrange(GISJOIN_1950, year)  %>%
  mutate(category_most_overlap = ifelse(is.na(category_most_overlap), "missing", category_most_overlap),
         grade_most_overlap = ifelse(is.na(grade_most_overlap), "missing", grade_most_overlap)) %>%
  left_join(inner_ring_units, by = c("GISJOIN_1950" = "GISJOIN_1950"))

# 2. Run Weighted Matching -----

# Get all treated tracts
treated_tracts <- census_tract_data %>%
  filter(location_type == "treated", !is.na(treatment_year)) %>%
  distinct(GISJOIN_1950, treatment_year, county_id) %>%
  arrange(treatment_year, GISJOIN_1950)

cat("\n=== WEIGHTED MATCHING ===\n")
cat("Total treated tracts:", nrow(treated_tracts), "\n")
cat("Weight method:", weight_method, "\n\n")

# Initialize list to store results
all_matched_data_treated <- list()
all_matched_data_inner <- list()

# Loop over each treated tract
for (i in 1:nrow(treated_tracts)) {
  tract <- treated_tracts$GISJOIN_1950[i]
  year <- treated_tracts$treatment_year[i]

  if (i %% 10 == 0) {
    cat("Processing tract", i, "of", nrow(treated_tracts), "\n")
  }

  # Match for treated group
  matched_treated <- compute_weighted_matches(
    data = census_tract_data,
    treated_tract = tract,
    treatment_year = year,
    outcome_vars = match_vars,
    time_invariant_vars = time_invariant_vars,
    exact_match_vars = exact_match_vars,
    weight_method = weight_method,
    bandwidth = 0.06,
    use_trends = FALSE,
    group_type = "treated"
  )

  if (!is.null(matched_treated)) {
    all_matched_data_treated[[i]] <- matched_treated
  }
}

# Get inner ring tracts (spillovers)
inner_tracts <- census_tract_data %>%
  filter(location_type == "inner", !is.na(treatment_year)) %>%
  distinct(GISJOIN_1950, treatment_year, county_id) %>%
  arrange(treatment_year, GISJOIN_1950)

cat("\nMatching inner ring tracts (spillovers):", nrow(inner_tracts), "tracts\n\n")

# Loop over inner ring tracts
for (i in 1:nrow(inner_tracts)) {
  tract <- inner_tracts$GISJOIN_1950[i]
  year <- inner_tracts$treatment_year[i]

  if (i %% 10 == 0) {
    cat("Processing inner tract", i, "of", nrow(inner_tracts), "\n")
  }

  # Match for inner group
  matched_inner <- compute_weighted_matches(
    data = census_tract_data,
    treated_tract = tract,
    treatment_year = year,
    outcome_vars = match_vars,
    time_invariant_vars = time_invariant_vars,
    exact_match_vars = exact_match_vars,
    weight_method = weight_method,
    bandwidth = 0.06,
    use_trends = FALSE,
    group_type = "inner"
  )

  if (!is.null(matched_inner)) {
    all_matched_data_inner[[i]] <- matched_inner
  }
}

# Stack all match groups together
cat("\n=== COMBINING MATCHED DATA ===\n")

tract_data_matched <- bind_rows(
  c(all_matched_data_treated, all_matched_data_inner)
) %>%
  arrange(GISJOIN_1950, year, match_group)

cat("Total observations in matched dataset:", nrow(tract_data_matched), "\n")
cat("Unique tracts:", n_distinct(tract_data_matched$GISJOIN_1950), "\n")
cat("Unique match groups:", n_distinct(tract_data_matched$match_group), "\n\n")

# 3. Additional cleaning -----

## Clean private population ----
tract_data_matched <- tract_data_matched %>%
  group_by(match_group, group_type) %>%
  mutate(
    has_missing_private_pop = any(is.na(private_population_estimate)),
    private_population_estimate = ifelse(has_missing_private_pop, NA, private_population_estimate),
    private_black_population_estimate = ifelse(has_missing_private_pop, NA, private_black_population_estimate),
    private_white_population_estimate = ifelse(has_missing_private_pop, NA, private_white_population_estimate),
    asinh_private_population_estimate = ifelse(has_missing_private_pop, NA, asinh_private_population_estimate),
    asinh_private_black_population_estimate = ifelse(has_missing_private_pop, NA, asinh_private_black_population_estimate),
    asinh_private_white_population_estimate = ifelse(has_missing_private_pop, NA, asinh_private_white_population_estimate)
  ) %>%
  ungroup()

## Merge spillover project size data -----
inner_spillover_characteristics <- tract_data_matched %>%
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

tract_data_matched <-
  tract_data_matched %>%
  select(-total_public_housing_units_built_nearby) %>%
  left_join(inner_spillover_characteristics,
            by = "match_group") %>%
  mutate(
    total_public_housing_units_built_nearby = ifelse(location_type == "treated", NA, total_public_housing_units_built_nearby),
    spillover_size_group = ifelse(location_type == "treated", NA, spillover_size_group),
    log_spillover_units = ifelse(location_type == "treated", NA, log_spillover_units)
  )

# Rename weight column to weights for consistency with other variants
tract_data_matched <- tract_data_matched %>%
  rename(weights = weight)

# 4. Summary statistics -----
cat("\n=== WEIGHTED MATCHING SUMMARY ===\n")

matched_summary <- tract_data_matched %>%
  group_by(group_type, location_type) %>%
  summarise(
    n_obs = n(),
    n_tracts = n_distinct(GISJOIN_1950),
    n_match_groups = n_distinct(match_group),
    .groups = 'drop'
  )

print(matched_summary)

# Weight distribution
cat("\n=== WEIGHT DISTRIBUTION ===\n")
weight_summary <- tract_data_matched %>%
  group_by(group_type) %>%
  summarise(
    min_weight = min(weights, na.rm = TRUE),
    median_weight = median(weights, na.rm = TRUE),
    mean_weight = mean(weights, na.rm = TRUE),
    max_weight = max(weights, na.rm = TRUE),
    # Effective sample size
    ess = sum(weights)^2 / sum(weights^2),
    .groups = 'drop'
  )

print(weight_summary)

# 5. Create balance tables ----
# Note: We only have 1-year pre-treatment for weighted matching currently
# Can add 2-year version with trends later if needed

create_balance_tables_single <- function(data, balance_dir) {

  # Treated neighborhoods balance
  treated_balance <- data %>%
    filter(group_type == "treated") %>%
    mutate(baseline_year = matched_treatment_year - 10) %>%
    filter(year == baseline_year) %>%
    mutate(treatment_group = ifelse(location_type == "treated", "Treated", "Control"))

  # Create balance table using tableone
  balance_vars <- c("asinh_pop_total", "black_share", "asinh_pop_black",
                   "asinh_median_income", "asinh_median_rent_calculated",
                   "unemp_rate", "lfp_rate", "asinh_distance_from_cbd")

  tab_treated <- CreateTableOne(
    vars = balance_vars,
    strata = "treatment_group",
    data = treated_balance,
    test = FALSE
  )

  # Convert to data frame with standardized differences
  balance_df <- print(tab_treated, smd = TRUE, printToggle = FALSE) %>%
    as.data.frame() %>%
    rownames_to_column("Variable")

  # Clean up variable names
  var_labels <- c(
    "asinh_pop_total" = "Total Population (asinh)",
    "black_share" = "Black Share",
    "asinh_pop_black" = "Black Population (asinh)",
    "asinh_median_income" = "Median Income (asinh)",
    "asinh_median_rent_calculated" = "Median Rent (asinh)",
    "unemp_rate" = "Unemployment Rate",
    "lfp_rate" = "Labor Force Participation Rate",
    "asinh_distance_from_cbd" = "Distance from CBD (asinh)"
  )

  balance_df <- balance_df %>%
    mutate(Variable = ifelse(Variable %in% names(var_labels),
                            var_labels[Variable], Variable))

  # Add N row at top
  n_row <- data.frame(
    Variable = "N (Tracts)",
    Control = as.character(sum(treated_balance$treatment_group == "Control")),
    Treated = as.character(sum(treated_balance$treatment_group == "Treated")),
    SMD = ""
  )

  balance_df <- bind_rows(n_row, balance_df)

  # Create tinytable
  balance_table <- tt(balance_df,
                     caption = paste0("Pre-treatment Covariate Balance: Treated Neighborhoods",
                                     "\\label{tab:balance_treated}"),
                     notes = "\\textit{Notes:} Sample includes all treated neighborhoods and their matched controls from the weighted matching procedure.") %>%
    style_tt(font_size = 0.9)

  # Save
  save_tt(balance_table, here(balance_dir, "balance_table_treated_neighborhoods.tex"),
          overwrite = TRUE)

  cat("Balance table saved to:", here(balance_dir, "balance_table_treated_neighborhoods.tex"), "\n")
}

create_balance_tables_single(tract_data_matched, balance_table_dir)

# 6. Save datasets -----
cat("\n=== SAVING DATASETS ===\n")

write_csv(tract_data_matched,
          here(output_data_dir, "tract_data_matched_weighted.csv"))

cat("Dataset saved to:\n",
    here(output_data_dir, "tract_data_matched_weighted.csv"), "\n")

# 7. Create time series plots ----
variables_to_plot <- list(
  list(var = "black_share", label = "Black Share", percent = TRUE),
  list(var = "black_pop", label = "Black pop", percent = FALSE),
  list(var = "total_pop", label = "Total Population", percent = FALSE),
  list(var = "median_income", label = "Median Income (1950$)", percent = FALSE)
)

cat("\n=== CREATING TIME SERIES PLOTS ===\n")
create_multiple_time_series(
  data = tract_data_matched,
  variables_to_plot = variables_to_plot,
  figures_dir = here("output", "time_series", data_type, variant)
)

cat("\n=== WEIGHTED MATCHING COMPLETE ===\n")
