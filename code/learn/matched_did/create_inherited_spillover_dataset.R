# Create Inherited Spillover Dataset
# This script takes the matched treated/control pairs and creates spillover groups
# by finding inner rings of BOTH treated tracts and their matched controls

library(tidyverse)
library(sf)
library(here)
library(spdep)

rm(list = ls())

# Parameters -----
data_type <- "combined"
merged_data_dir <- here("data", "derived", "merged", data_type)
match_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset")

# Read data -----
# 1. Matched data (treated tracts and their controls only) - WITH REPLACEMENT
tract_data_matched_2_year <- read_csv(here(match_data_dir, "tract_data_matched_2_year_replacement.csv"))

# 2. Full balanced dataset (all tracts)
census_tract_sample <- st_read(here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")) %>%
  st_drop_geometry()

# 3. Rings data
tracts_and_rings <- read_csv(here(merged_data_dir, "unique_tracts_and_rings.csv")) %>%
  filter(location_type != "outer")  # Keep only treated and inner

# Step 1: Get matched pairs (treated and their controls) -----
matched_pairs <- tract_data_matched_2_year %>%
  filter(group_type == "treated") %>%  # Only treated group matching
  distinct(GISJOIN_1950, match_group, location_type) %>%
  select(GISJOIN_1950, match_group, original_location_type = location_type)

cat("Found", nrow(matched_pairs %>% filter(original_location_type == "treated")), "treated tracts\n")
cat("Found", nrow(matched_pairs %>% filter(original_location_type == "donor_pool")), "matched control tracts\n")

# Step 2: Set up spatial adjacency (following spatial DiD approach) -----
# Read full census data with geometry
census_with_geom <- st_read(here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg"))

# Get unique tracts and create neighbor list (following spatial DiD approach)
census_unique <- census_with_geom %>%
  group_by(GISJOIN_1950) %>%
  filter(row_number() == 1) %>% 
  ungroup() %>%
  mutate(tract_index = row_number())

# Create neighbor list using polygon adjacency
nb <- poly2nb(census_unique)
nblags <- nblag(nb, maxlag = 2)  # Get 1st and 2nd order neighbors (we only use 1st)

# Create crosswalk from GISJOIN to tract_index  
tract_crosswalk <- census_unique %>%
  st_drop_geometry() %>%
  select(GISJOIN_1950, tract_index)

# Get all treated tract IDs to exclude from spillover rings -----
all_treated_tract_ids <- 
  census_tract_sample %>%
  filter(treated == 1) %>%
  pull(GISJOIN_1950) %>% 
  unique()

cat("Found", length(all_treated_tract_ids), "treated tracts to exclude from spillover rings\n")

# Debug: Check dimensions
cat("Census unique tracts:", nrow(census_unique), "\n")
cat("Neighbor list length:", length(nb), "\n")
cat("Sample tract_crosswalk:\n")
print(head(tract_crosswalk))
cat("Sample treated tracts:\n")  
print(head(matched_pairs %>% filter(original_location_type == "treated")))

# Step 3: Get inner rings of treated tracts -----
treated_tracts <- matched_pairs %>%
  filter(original_location_type == "treated")

# Get inner rings for each treated tract using adjacency
treated_inner_mapping <- treated_tracts %>%
  left_join(tract_crosswalk, by = "GISJOIN_1950") %>%
  rowwise() %>%
  mutate(
    inner_ring_indices = list(if(is.na(tract_index) || tract_index > length(nblags[[1]])) integer(0) else nblags[[1]][[tract_index]])
  ) %>%
  unnest_longer(inner_ring_indices, values_to = "inner_tract_index") %>%
  left_join(tract_crosswalk, by = c("inner_tract_index" = "tract_index")) %>%
  select(treated_tract = GISJOIN_1950.x, inner_ring_tract = GISJOIN_1950.y, match_group) %>%
  filter(!is.na(inner_ring_tract))

# Create treated inner rings dataset (exclude treated tracts)
treated_inner_rings <- treated_inner_mapping %>%
  select(GISJOIN_1950 = inner_ring_tract, match_group) %>%
  filter(!GISJOIN_1950 %in% all_treated_tract_ids) %>%  # EXCLUDE TREATED TRACTS
  mutate(spillover_type = "treated_spillover")

cat("Found", nrow(treated_inner_rings), "inner ring tracts of treated units\n")

# Step 4: Get inner rings of matched control tracts -----
control_tracts <- matched_pairs %>%
  filter(original_location_type == "donor_pool")

# Get inner rings for each control tract using adjacency
control_inner_mapping <- control_tracts %>%
  left_join(tract_crosswalk, by = "GISJOIN_1950") %>%
  rowwise() %>%
  mutate(
    inner_ring_indices = list(if(is.na(tract_index) || tract_index > length(nblags[[1]])) integer(0) else nblags[[1]][[tract_index]])
  ) %>%
  unnest_longer(inner_ring_indices, values_to = "inner_tract_index") %>%
  left_join(tract_crosswalk, by = c("inner_tract_index" = "tract_index")) %>%
  select(control_tract = GISJOIN_1950.x, inner_ring_tract = GISJOIN_1950.y, match_group) %>%
  filter(!is.na(inner_ring_tract))

# Create control inner rings dataset (exclude treated tracts)
control_inner_rings <- control_inner_mapping %>%
  select(GISJOIN_1950 = inner_ring_tract, match_group) %>%
  filter(!GISJOIN_1950 %in% all_treated_tract_ids) %>%  # EXCLUDE TREATED TRACTS
  mutate(spillover_type = "control_spillover")

cat("Found", nrow(control_inner_rings), "inner ring tracts of control units\n")

# Step 5: Handle overlap issues -----
# Some tracts might be inner rings of both treated and control units

overlap_tracts <- intersect(
  treated_inner_rings$GISJOIN_1950,
  control_inner_rings$GISJOIN_1950
)

cat("Found", length(overlap_tracts), "tracts that are inner rings of both treated and controls\n")

if (length(overlap_tracts) > 0) {
  cat("Overlap tracts:\n")
  print(overlap_tracts)
  
  # Decision: Remove overlap tracts to maintain clean comparison
  # Alternative: Keep them but flag them
  treated_inner_rings <- treated_inner_rings %>%
    filter(!GISJOIN_1950 %in% overlap_tracts)
  
  control_inner_rings <- control_inner_rings %>%
    filter(!GISJOIN_1950 %in% overlap_tracts)
  
  cat("After removing overlaps:\n")
  cat("Treated spillover tracts:", nrow(treated_inner_rings), "\n")
  cat("Control spillover tracts:", nrow(control_inner_rings), "\n")
}

# Step 6: Create inherited spillover dataset -----
inherited_spillover_ids <- bind_rows(
  treated_inner_rings %>% select(GISJOIN_1950, match_group, spillover_type),
  control_inner_rings %>% select(GISJOIN_1950, match_group, spillover_type)
)

# Merge with full census data to create analysis dataset
inherited_spillover_dataset <- census_tract_sample %>%
  inner_join(inherited_spillover_ids, by = "GISJOIN_1950", relationship = "many-to-many") %>%
  mutate(
    location_type = spillover_type,  # treated_spillover or control_spillover
    weights = 1,  # Equal weights for inherited approach
    group_type = "inherited_spillover",  # Mark as inherited spillover analysis
    # Add transformed variables (following matching script)
    asinh_pop_total = asinh(total_pop),
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
    log_private_units_estimate = asinh(total_private_units_estimate),
    ln_total_units = log(total_units)
  )

# Step 6.1: Compute Wing–Freedman–Hollingsworth (2024) weights ("Wing weights")
# Citation: Wing, C., Freedman, M., & Hollingsworth, A. (2024).

wing_sizes <- inherited_spillover_dataset %>%
  group_by(match_group) %>%
  summarise(
    ND = sum(location_type == "treated_spillover"),
    NC = sum(location_type == "control_spillover"),
    .groups = "drop"
  ) %>%
  mutate(
    ND_tot = sum(ND),
    NC_tot = sum(NC),
    wing_weight_control = (ND / ND_tot) / (NC / NC_tot)
  )

inherited_spillover_dataset <- inherited_spillover_dataset %>%
  left_join(wing_sizes %>% select(match_group, wing_weight_control),
            by = "match_group") %>%
  mutate(
    wing_weight = ifelse(location_type == "treated_spillover", 1, wing_weight_control)
  )

# Step 6.2: Add treated tract baseline characteristics for heterogeneity analysis -----
cat("Adding treated tract baseline characteristics for heterogeneity analysis...\n")

# Read matched dataset to get treated tract characteristics
matched_data <- read_csv(here(match_data_dir, "tract_data_matched_2_year_replacement.csv"))

# Extract treated tract characteristics at t=-10 (baseline)
# First create event_time in matched data, then filter
treated_baseline_chars <- matched_data %>%
  # Create event time (same logic as in spillover analysis)
  mutate(
    treatment_year = as.numeric(str_extract(match_group, "\\d{4}")),
    event_time = year - treatment_year
  ) %>%
  filter(location_type == "treated", event_time == -10) %>%
  select(match_group, 
         black_share, 
         asinh_median_income,
         local_dissimilarity_index,
         population_density,
         unemp_rate,
         lfp_rate) %>%
  rename_with(~paste0(., "_tminus10"), .cols = -match_group)

# Get project characteristics at t=0 (treatment year)
project_chars <- matched_data %>%
  mutate(
    treatment_year = as.numeric(str_extract(match_group, "\\d{4}")),
    event_time = year - treatment_year
  ) %>%
  filter(location_type == "treated", event_time == 0) %>%
  select(match_group, 
         total_public_housing_units,
         total_public_housing_pop_estimate,
         black_public_housing_pop_estimate,
         white_public_housing_pop_estimate) %>%
  mutate(
    # Calculate project racial composition (only when total pop > 0)
    project_black_share_t0 = ifelse(total_public_housing_pop_estimate > 0, 
                                   black_public_housing_pop_estimate / total_public_housing_pop_estimate, 
                                   NA),
    project_white_share_t0 = ifelse(total_public_housing_pop_estimate > 0,
                                   white_public_housing_pop_estimate / total_public_housing_pop_estimate,
                                   NA)
  ) %>%
  select(match_group, 
         total_public_housing_units_t0 = total_public_housing_units,
         project_black_share_t0,
         project_white_share_t0)

# Merge project characteristics with baseline characteristics
treated_baseline_chars <- treated_baseline_chars %>%
  left_join(project_chars, by = "match_group")

# Merge baseline characteristics into spillover dataset
inherited_spillover_dataset <- inherited_spillover_dataset %>%
  left_join(treated_baseline_chars, by = "match_group")

cat("Added baseline characteristics for", n_distinct(treated_baseline_chars$match_group), "match groups\n")

# Step 7: Create summary statistics -----
cat("\n=== INHERITED SPILLOVER DATASET SUMMARY ===\n")
cat("Total observations:", nrow(inherited_spillover_dataset), "\n")
cat("Unique tracts:", n_distinct(inherited_spillover_dataset$GISJOIN_1950), "\n")
cat("Unique match groups:", n_distinct(inherited_spillover_dataset$match_group), "\n")

spillover_summary <- inherited_spillover_dataset %>%
  group_by(location_type) %>%
  summarise(
    n_obs = n(),
    n_tracts = n_distinct(GISJOIN_1950),
    n_match_groups = n_distinct(match_group),
    .groups = 'drop'
  )

print(spillover_summary)

# Step 8: Save dataset -----
write_csv(inherited_spillover_dataset, 
          here(match_data_dir, "tract_data_inherited_spillovers_2_year_replacement.csv"))

cat("\n=== DATASET SAVED ===\n")
cat("Saved to:", here(match_data_dir, "tract_data_inherited_spillovers_2_year_replacement.csv"), "\n")

# Step 9: Basic balance check -----
if (nrow(inherited_spillover_dataset) > 0) {
  library(tableone)


  balance_data <- inherited_spillover_dataset %>%
    filter(YEAR == 1940)  # Pre-treatment
  
  if (nrow(balance_data) > 0) {
    covariates <- c("black_share", "asinh_pop_total", "asinh_median_income", "asinh_median_rent_calculated", 
                    "asinh_median_home_value_calculated", "asinh_distance_from_cbd",
                    "ln_population_density", "asinh_pop_black", "asinh_pop_white")
    
    balance_table <- CreateTableOne(
      vars = covariates,
      strata = "location_type",
      data = balance_data,
      test = TRUE
    )
    
    cat("\n=== BALANCE CHECK (1940) ===\n")
    print(balance_table, smd = TRUE)
  }
}
