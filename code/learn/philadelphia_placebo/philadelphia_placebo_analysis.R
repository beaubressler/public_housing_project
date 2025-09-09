####
# Philadelphia Placebo Site Selection Analysis
# Uses proposed-but-not-built locations for counterfactual matched DiD
####

library(tidyverse)
library(here)
library(sf)
library(modelsummary)
library(tinytable)

rm(list = ls())

# Parameters -----
data_type <- "combined"
merged_data_dir <- here("data", "derived", "merged", data_type)
results_dir <- here("output", "regression_results", "philadelphia_placebo", data_type)


treated_tracts_panel_filepath <-
  here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")

# Create output directories
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# Step 1: Load proposed locations shapefile -----
cat("Loading proposed Philadelphia public housing locations...\n")

proposed_locations <- st_read(here("georeferencing", "philadelphia", "proposed_locations_bauman.shp"))

cat("Proposed locations shapefile structure:\n")
print(str(proposed_locations))
cat("\nColumn names:\n")
print(names(proposed_locations))
cat("\nFirst few rows:\n")
print(head(proposed_locations))
cat("\nCRS information:\n")
print(st_crs(proposed_locations))

# Step 2: Load census tract data -----
cat("\nLoading census tract sample...\n")

census_tract_sample <- st_read(here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg"))

treated_tracts_panel_raw <- st_read(treated_tracts_panel_filepath)

# Filter to Philadelphia only
philly_tracts <- census_tract_sample %>%
  filter(city == "Philadelphia")

cat("Philadelphia tracts found:", nrow(philly_tracts %>% distinct(GISJOIN_1950)), "unique tracts\n")

# Check CRS compatibility
cat("\nCRS comparison:\n")
cat("Proposed locations CRS:", st_crs(proposed_locations)$input, "\n")
cat("Census tracts CRS:", st_crs(philly_tracts)$input, "\n")

# Transform to common CRS if needed
if(st_crs(proposed_locations) != st_crs(philly_tracts)) {
  cat("Transforming proposed locations to match census tracts CRS...\n")
  proposed_locations <- st_transform(proposed_locations, st_crs(philly_tracts))
}

# Step 3: Spatial join to identify tracts containing proposed locations -----
cat("\nPerforming spatial join to identify census tracts with proposed locations...\n")

# Find which tracts contain proposed locations
proposed_tract_join <- st_join(proposed_locations, philly_tracts, join = st_within)

cat("Spatial join results:\n")
cat("Total proposed locations:", nrow(proposed_locations), "\n")
cat("Proposed locations matched to tracts:", sum(!is.na(proposed_tract_join$GISJOIN_1950)), "\n")
cat("Unmatched proposed locations:", sum(is.na(proposed_tract_join$GISJOIN_1950)), "\n")

# Get unique tracts with proposed locations
proposed_tracts <- proposed_tract_join %>%
  filter(!is.na(GISJOIN_1950)) %>%
  st_drop_geometry() %>%
  distinct(GISJOIN_1950) %>%
  pull(GISJOIN_1950)

cat("Unique census tracts with proposed locations:", length(proposed_tracts), "\n")

# Step 4: Check overlap with actual public housing tracts -----
actual_philly_treated <- philly_tracts %>%
  st_drop_geometry() %>%
  filter(treated == 1) %>%
  distinct(GISJOIN_1950) %>%
  pull(GISJOIN_1950)

cat("Actual Philadelphia public housing tracts:", length(actual_philly_treated), "\n")

overlap_tracts <- intersect(proposed_tracts, actual_philly_treated)
cat("Tracts with both proposed and actual public housing:", length(overlap_tracts), "\n")

proposed_only <- setdiff(proposed_tracts, actual_philly_treated)
cat("Tracts with only proposed (not actual) public housing:", length(proposed_only), "\n")

# Step 5: Create summary table of proposed vs actual locations -----
location_summary <- tibble(
  location_type = c("Proposed locations (total)", 
                    "Proposed locations matched to tracts",
                    "Unique tracts with proposed locations",
                    "Actual Philadelphia public housing tracts",
                    "Tracts with both proposed and actual",
                    "Tracts with only proposed locations"),
  count = c(nrow(proposed_locations),
            sum(!is.na(proposed_tract_join$GISJOIN_1950)),
            length(proposed_tracts),
            length(actual_philly_treated),
            length(overlap_tracts),
            length(proposed_only))
)

print(location_summary)

# Step 6: Descriptive comparison - Proposed vs Actual Philadelphia locations -----
cat("\n=== DESCRIPTIVE COMPARISON: PROPOSED VS ACTUAL LOCATIONS ===\n")

# Get 1940 characteristics for comparison
philly_1940_data <- philly_tracts %>%
  filter(YEAR == 1940) %>%
  st_drop_geometry() %>%
  mutate(
    location_type = case_when(
      GISJOIN_1950 %in% proposed_only ~ "Proposed Only",
      GISJOIN_1950 %in% actual_philly_treated ~ "Actual Public Housing",
      TRUE ~ "Neither"
    ),
    asinh_pop_total = asinh(total_pop),
    asinh_median_income = asinh(median_income),
    asinh_median_rent_calculated = asinh(median_rent_calculated),
    asinh_distance_from_cbd = asinh(distance_from_cbd)
  ) %>%
  filter(location_type != "Neither")

cat("Sample sizes for comparison:\n")
table(philly_1940_data$location_type)

# Variables for comparison
comparison_vars <- c(
  "black_share", "asinh_pop_total", "asinh_median_income",
  "pct_hs_grad", "unemp_rate", "lfp_rate",
  "asinh_median_rent_calculated", "share_needing_repair",
  "asinh_distance_from_cbd", "redlined_binary_80pp", "ur_binary_5pp"
)

# Calculate descriptive statistics by location type
descriptive_comparison <- philly_1940_data %>%
  select(location_type, all_of(comparison_vars)) %>%
  pivot_longer(cols = -location_type, names_to = "variable", values_to = "value") %>%
  group_by(location_type, variable) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    sd_val = sd(value, na.rm = TRUE),
    n_obs = sum(!is.na(value)),
    .groups = 'drop'
  ) %>%
  mutate(
    mean_sd = ifelse(variable %in% c("redlined_binary_80pp", "ur_binary_5pp"),
                     sprintf("%.3f", mean_val),
                     sprintf("%.3f (%.3f)", mean_val, sd_val))
  ) %>%
  select(location_type, variable, mean_sd) %>%
  pivot_wider(names_from = location_type, values_from = mean_sd)

# Calculate differences and t-tests
difference_tests_philly <- philly_1940_data %>%
  select(location_type, all_of(comparison_vars)) %>%
  pivot_longer(cols = -location_type, names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    proposed_mean = mean(value[location_type == "Proposed Only"], na.rm = TRUE),
    actual_mean = mean(value[location_type == "Actual Public Housing"], na.rm = TRUE),
    diff = proposed_mean - actual_mean,
    t_stat = tryCatch({
      t.test(value[location_type == "Proposed Only"],
             value[location_type == "Actual Public Housing"])$statistic
    }, error = function(e) NA),
    p_value = tryCatch({
      t.test(value[location_type == "Proposed Only"],
             value[location_type == "Actual Public Housing"])$p.value
    }, error = function(e) NA),
    .groups = 'drop'
  ) %>%
  mutate(
    diff_formatted = sprintf("%.3f", diff),
    significance = case_when(
      is.na(p_value) ~ "",
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    diff_sig = paste0(diff_formatted, significance)
  )

# Combine into final comparison table
philadelphia_comparison_table <- descriptive_comparison %>%
  left_join(difference_tests_philly, by = "variable") %>%
  mutate(
    Variable = case_when(
      variable == "black_share" ~ "Black Share",
      variable == "asinh_pop_total" ~ "Log Total Population",
      variable == "asinh_median_income" ~ "Log Median Income",
      variable == "pct_hs_grad" ~ "Pct Graduated HS",
      variable == "unemp_rate" ~ "Unemployment Rate",
      variable == "lfp_rate" ~ "LFP Rate",
      variable == "asinh_median_rent_calculated" ~ "Log Median Rent",
      variable == "share_needing_repair" ~ "Share Needing Major Repairs",
      variable == "asinh_distance_from_cbd" ~ "Log Dist. from CBD",
      variable == "redlined_binary_80pp" ~ "Redlined (HOLC)",
      variable == "ur_binary_5pp" ~ "Urban Renewal Area",
      TRUE ~ variable
    )
  ) %>%
  select(Variable, `Actual Public Housing`, `Proposed Only`, diff_sig) %>%
  rename("Difference (Proposed - Actual)" = diff_sig)

print("=== PHILADELPHIA: PROPOSED VS ACTUAL LOCATIONS (1940) ===")
print(philadelphia_comparison_table)

# Create publication-ready table
philly_comparison_table_formatted <- tt(philadelphia_comparison_table,
                                        caption = "1940 Neighborhood Characteristics: Proposed vs Actual Public Housing Locations in 
  Philadelphia\\label{tab:philly_proposed_vs_actual}") %>%
  style_tt(font_size = 0.9) %>%
  format_tt(escape = FALSE) %>%
  theme_tt("resize")

# Step 7: Set up placebo matching following existing methodology -----
cat("\n=== PHILADELPHIA PLACEBO MATCHING SETUP ===\n")

# First, get the actual Philadelphia treatment years to inform placebo timing
actual_philly_treatment_timing <- census_tract_sample %>%
  st_drop_geometry() %>%
  filter(city == "Philadelphia", treated == 1) %>%
  # You'll need to get treatment year from treated_tracts_panel
  left_join(treated_tracts_panel_raw %>% st_drop_geometry(), by = "GISJOIN_1950") %>%
  filter(!is.na(treatment_year)) %>%
  distinct(GISJOIN_1950, treatment_year)

cat("Philadelphia actual treatment years:\n")
print(table(actual_philly_treatment_timing$treatment_year))

# Use median year for placebo treatment
placebo_treatment_year <- 1960

# Create placebo dataset following your structure
cat("\n--- CREATING PLACEBO DATASET ---\n")

# Create placebo version of tracts_and_rings
placebo_tracts_and_rings <- tibble(
  GISJOIN_1950 = proposed_only,
  location_type = "treated"  # Treat proposed locations as "treated" 
)

# Create placebo version of treated_tracts_panel  
placebo_treated_tracts_panel <- tibble(
  GISJOIN_1950 = proposed_only,
  total_public_housing_units = 100,  # Assign placeholder units
  treatment_year = placebo_treatment_year
)

# Create placebo census tract data following your exact structure
placebo_census_tract_data <- census_tract_sample %>%
  # Same geographic processing as your original
  mutate(centroid = st_centroid(geom)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  select(-centroid) %>%
  # Merge placebo treatment info instead of actual
  left_join(placebo_treated_tracts_panel, by = "GISJOIN_1950") %>%
  # Merge placebo rings
  left_join(placebo_tracts_and_rings, by = "GISJOIN_1950") %>%
  # Same location_type logic as original
  mutate(location_type = if_else(is.na(location_type), "donor_pool", location_type)) %>%
  # Same variable transformations as your original
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
         log_private_units_estimate = asinh(total_private_units_estimate)) %>%
  # Same county ID and other processing
  mutate(county_id = paste0(STATEA, COUNTYA)) %>%
  dplyr::rename(year = YEAR) %>%
  st_drop_geometry() %>%
  arrange(GISJOIN_1950, year) %>%
  mutate(category_most_overlap = ifelse(is.na(category_most_overlap), "missing", category_most_overlap),
         grade_most_overlap = ifelse(is.na(grade_most_overlap), "missing", grade_most_overlap))

cat("Placebo dataset created:\n")
cat("- Placebo treated tracts:", sum(placebo_census_tract_data$location_type == "treated", na.rm = TRUE), "\n")
cat("- Donor pool tracts:", sum(placebo_census_tract_data$location_type == "donor_pool", na.rm = TRUE), "\n")

# Now you can run your existing perform_matching_with_replacement function on this placebo dataset!
cat("\n=== READY FOR PLACEBO MATCHING ===\n")
cat("Use placebo_census_tract_data with your existing perform_matching_with_replacement() function\n")
cat("Treatment year:", placebo_treatment_year, "\n")
cat("Group types: 'treated' (placebo locations)\n")


# Step 8: Run placebo matching using existing function -----
cat("\n=== RUNNING PLACEBO MATCHING ===\n")

# Use your existing matching parameters
matching_vars <- c(
  "total_pop",
  "black_share",
  "unemp_rate",
  "median_income",
  "median_rent_calculated"
)

exact_matching_vars <- c(
  "county_id",
  "redlined_binary_80pp",
  "ur_binary_5pp"
)

knn_neighbors <- 1

# Run placebo matching (only "treated" group since no inner ring for placebo)
placebo_matched_result <- perform_matching_with_replacement(
  data = placebo_census_tract_data,
  treatment_year = placebo_treatment_year,  # 1960
  match_vars = matching_vars,
  nearest_neighbors = knn_neighbors,
  group_type = "treated",  # Placebo treated locations
  match_type = "nearest",
  distance_option = "glm",
  match_link = "logit",
  pre_treatment_decades = 1,
  exact_match_vars = exact_matching_vars,
  caliper = NULL
)

cat("Placebo matching complete!\n")
cat("Matched placebo treated units:", sum(placebo_matched_result$matched_data$treatment_group == 1), "\n")
cat("Matched control units:", sum(placebo_matched_result$matched_data$treatment_group == 0), "\n")

# Create placebo matched panel dataset (following your structure)
placebo_matched_tract_ids <- placebo_matched_result$matched_data %>%
  select(GISJOIN_1950, subclass) %>%
  mutate(
    matched_treatment_year = placebo_treatment_year,
    group_type = "treated",
    match_type = "nearest"
  )

# Create full placebo matched panel
placebo_tract_data_matched <- placebo_census_tract_data %>%
  inner_join(placebo_matched_tract_ids, by = "GISJOIN_1950") %>%
  mutate(
    match_group = paste(county_id, matched_treatment_year, subclass, sep = "_"),
    weights = 1  # Equal weights for now
  )

cat("Placebo matched panel created:\n")
cat("- Total observations:", nrow(placebo_tract_data_matched), "\n")
cat("- Unique tracts:", length(unique(placebo_tract_data_matched$GISJOIN_1950)), "\n")
cat("- Treatment observations:", sum(placebo_tract_data_matched$location_type == "treated"), "\n")
cat("- Control observations:", sum(placebo_tract_data_matched$location_type == "donor_pool"), "\n")

