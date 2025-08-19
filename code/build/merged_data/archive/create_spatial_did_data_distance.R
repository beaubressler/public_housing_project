###
# Create data for spatial difference-in-differences regressions
# PURE DISTANCE-BASED RINGS APPROACH
# This script creates rings based only on distance from projects,
# ignoring tract contiguity/neighbor relationships
####

# Preliminaries -----
library(tidyverse)
library(data.table)
library(sf)
library(here)
library(tigris)
library(units)

rm(list=ls())
set.seed(123)

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small" or "combined"
data_type <- "combined"

## Define filepaths -----
# output paths
merged_data_dir <- here("data", "derived", "merged", data_type)
map_output_dir <- here("output", "figures", "stacked_did", data_type, "distance_based")
balance_table_dir <- here("output", "balance_tables", "stacked_did", data_type, "distance_based")

# Create output directories if they don't exist
dir.create(map_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(balance_table_dir, recursive = TRUE, showWarnings = FALSE)

# input paths 
treated_tracts_panel_path <- here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")
census_tract_sample_path <- here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")

# event study data paths for distance-based approach
event_study_distance_rings_output_path <- here(merged_data_dir, "event_study_data_distance_rings.csv")
unique_tracts_distance_rings_output_path <- here(merged_data_dir, "unique_tracts_distance_rings.csv")

# Read in and prep data ----

# read treated_tracts_panel.gpkg
treated_tracts_panel <- st_read(treated_tracts_panel_path) %>% 
  st_drop_geometry() 

# read in Census tract sample with treatment status
census_tract_sample <- st_read(census_tract_sample_path)

# read in public housing project locations and create unique project IDs
public_housing_data <- st_read(here("data", "derived", "public_housing", "working", "cleaned_housing_projects.gpkg")) %>%
  mutate(project_id = row_number())  # Create unique project identifier 

# Create a dataframe with the total number of public housing units per tract
total_ph_units_per_tract <- treated_tracts_panel %>%
  dplyr::select(GISJOIN_1950, total_public_housing_units) %>% 
  distinct()

# Get all unique tracts in the sample
census_tract_sample_indexed_unique <- census_tract_sample %>%
  group_by(GISJOIN_1950) %>%
  filter(row_number() == 1) %>% 
  mutate(tract_id = row_number()) %>%
  ungroup()

# Ensure geometries are valid
census_tract_sample_indexed_unique <- st_make_valid(census_tract_sample_indexed_unique)

# Get treated tracts and their treatment years from treated_tracts_panel
treated_tracts <- treated_tracts_panel %>%
  dplyr::select(GISJOIN_1950, treatment_year) %>%
  distinct()

# Create distance-based rings ----

cat("Creating distance-based rings...\n")

# Get centroids of all tracts and projects for distance calculations
tract_centroids <- st_centroid(census_tract_sample_indexed_unique)
project_centroids <- st_centroid(public_housing_data)

# Ensure same CRS
tract_centroids <- st_transform(tract_centroids, crs = 3857)  # Web Mercator for distance calculations
project_centroids <- st_transform(project_centroids, crs = 3857)

# Define distance thresholds (in meters)
distance_thresholds <- list(
  treated_max = 100,    # 0-100m = treated
  inner_1_min = 100,    # 100-450m = inner ring 1  
  inner_1_max = 450,
  inner_2_min = 450,    # 450-800m = inner ring 2
  inner_2_max = 800,
  outer_min = 800,      # 800-1000m = outer ring (control)
  outer_max = 1000
)

# Function to assign distance rings
assign_distance_ring <- function(distance_m) {
  case_when(
    distance_m <= distance_thresholds$treated_max ~ "treated",
    distance_m > distance_thresholds$inner_1_min & distance_m <= distance_thresholds$inner_1_max ~ "inner_1",
    distance_m > distance_thresholds$inner_2_min & distance_m <= distance_thresholds$inner_2_max ~ "inner_2", 
    distance_m >= distance_thresholds$outer_min & distance_m <= distance_thresholds$outer_max ~ "outer",
    TRUE ~ "excluded"
  )
}

# Initialize results data.table
distance_results <- data.table()

# Get coordinates for efficient distance calculation
tract_coords <- st_coordinates(tract_centroids)
project_coords <- st_coordinates(project_centroids)

cat("Calculating distances between", nrow(tract_centroids), "tracts and", nrow(project_centroids), "projects...\n")

# For each project, find tracts in each distance ring
for (i in seq_len(nrow(project_centroids))) {
  
  if (i %% 50 == 0) cat("Processing project", i, "of", nrow(project_centroids), "\n")
  
  # Get current project info
  current_project <- project_centroids[i, ]
  project_id <- current_project$project_id
  
  # Skip if project doesn't have valid treatment year
  project_treatment_year <- public_housing_data$treatment_year[public_housing_data$project_id == project_id]
  if (length(project_treatment_year) == 0 || is.na(project_treatment_year)) next
  
  # Calculate distances from this project to all tracts
  distances <- st_distance(tract_centroids, current_project)
  distances_m <- as.numeric(distances)
  
  # Assign rings based on distance
  tract_rings <- assign_distance_ring(distances_m)
  
  # Keep only tracts that fall into defined rings (exclude "excluded")
  valid_tracts <- which(tract_rings != "excluded")
  
  if (length(valid_tracts) > 0) {
    # Create data.table with results
    project_results <- data.table(
      project_id = project_id,
      treatment_year = project_treatment_year,
      tract_index = valid_tracts,
      GISJOIN_1950 = tract_centroids$GISJOIN_1950[valid_tracts],
      distance_ring = tract_rings[valid_tracts],
      distance_m = distances_m[valid_tracts]
    )
    
    distance_results <- rbind(distance_results, project_results)
  }
}

cat("Distance calculations complete. Found", nrow(distance_results), "tract-project combinations.\n")

# Handle overlapping assignments ----
# Some tracts might be assigned to multiple projects - keep the closest assignment

cat("Resolving overlapping tract assignments...\n")

distance_results_clean <- distance_results %>%
  # For each tract-ring combination, keep only the closest project
  group_by(GISJOIN_1950, distance_ring) %>%
  filter(distance_m == min(distance_m)) %>%
  # If still tied, keep first project_id alphabetically for consistency
  filter(project_id == min(project_id)) %>%
  ungroup() %>%
  as.data.table()

cat("After cleaning:", nrow(distance_results_clean), "unique tract-project-ring assignments.\n")

# Create long dataset for event studies ----

cat("Creating event study dataset...\n")

# Convert to long format for event studies
years <- seq(1930, 1990, by = 10)

# Expand to include all years
distance_event_study_long <- distance_results_clean %>%
  # Cross join with years using expand_grid (tidyverse approach)
  as_tibble() %>%
  expand_grid(year = years) %>%
  # Create treated indicator
  mutate(
    treated = ifelse(year >= treatment_year & distance_ring == "treated", TRUE, FALSE),
    # Variables for Sun and Abraham estimates  
    cohort = as.factor(treatment_year),
    relative_period = (year - treatment_year) / 10,
    event_time = year - treatment_year
  )

# Merge with census data ----

cat("Merging with census tract data...\n")

# Prepare census data (same as original script)
census_data_for_event_study <- census_tract_sample %>% 
  dplyr::select(GISJOIN_1950, STATE, COUNTY, TRACTA, YEAR, city, cbsa_title,
                # population by race
                black_share, white_share, white_pop, black_pop, total_pop, 
                # private population 
                private_population_estimate, private_black_population_estimate, private_white_population_estimate,
                # income and housing
                median_income, median_rent_calculated, median_home_value_calculated, 
                # SES measures
                pct_hs_grad, unemp_rate, lfp_rate,
                population_density, distance_from_cbd) %>%
  st_drop_geometry() %>% 
  # Inverse hyperbolic sine transformations
  mutate(
    asinh_pop_total = asinh(total_pop),
    asinh_pop_white = asinh(white_pop),
    asinh_pop_black = asinh(black_pop),
    asinh_private_pop = asinh(private_population_estimate),
    asinh_private_black_pop = asinh(private_black_population_estimate),
    asinh_private_white_pop = asinh(private_white_population_estimate),
    asinh_median_income = asinh(median_income),
    asinh_median_rent_calculated = asinh(median_rent_calculated),
    asinh_median_home_value_calculated = asinh(median_home_value_calculated)
  ) %>%
  dplyr::rename(YEAR = YEAR)  # Ensure consistent naming

# Prepare total public housing units mapping
treated_project_to_units <- public_housing_data %>%
  st_drop_geometry() %>%
  dplyr::select(project_id, total_public_housing_units) %>%
  distinct()

# Merge everything together
event_study_final <- distance_event_study_long %>%
  # Merge with census data
  left_join(census_data_for_event_study, by = c("GISJOIN_1950", "year" = "YEAR")) %>%
  # Merge with public housing units data
  left_join(treated_project_to_units, by = "project_id") %>%
  # Add distance from project (use the calculated distance)
  mutate(distance_from_project = distance_m) %>%
  # Keep distinct observations
  distinct()

# Create final ring variables ----
event_study_data_distance_rings <- event_study_final %>%
  mutate(
    event_time = as.factor(year - treatment_year),
    location_type = distance_ring,  # Use distance_ring as location_type for consistency
    ring = as.factor(case_when(
      distance_ring == "treated" ~ 0,
      distance_ring == "inner_1" ~ 1,
      distance_ring == "inner_2" ~ 2,
      distance_ring == "outer" ~ 3
    )),
    location_type_factor = factor(distance_ring, levels = c("outer", "inner_2", "inner_1", "treated")),
    location_type_dummy = as.numeric(location_type_factor),
    # Create unique identifiers
    tract_id = paste(GISJOIN_1950, sep = "_"),
    treated_id = project_id  # Use project_id as treated_id
  ) %>%
  # Rename for consistency with original script
  dplyr::rename(
    !!sym("year") := year
  )

# Calculate baseline black share (at event_time == -10)
baseline_black_share <- event_study_data_distance_rings %>%
  filter(event_time == "-10") %>% 
  dplyr::select(treated_id, GISJOIN_1950, black_share) %>% 
  rename(black_share_baseline = black_share) %>% 
  distinct()

# Merge baseline black share
event_study_data_distance_rings <- event_study_data_distance_rings %>%
  left_join(baseline_black_share, by = c("treated_id", "GISJOIN_1950"))

# Summary statistics ----
cat("\n=== DISTANCE-BASED RINGS SUMMARY ===\n")

ring_summary <- event_study_data_distance_rings %>%
  group_by(distance_ring) %>%
  summarise(
    n_tract_years = n(),
    n_unique_tracts = n_distinct(GISJOIN_1950),
    n_projects = n_distinct(treated_id),
    min_distance = min(distance_from_project, na.rm = TRUE),
    max_distance = max(distance_from_project, na.rm = TRUE),
    median_distance = median(distance_from_project, na.rm = TRUE),
    .groups = 'drop'
  )

print(ring_summary)

city_ring_summary <- event_study_data_distance_rings %>%
  group_by(city, distance_ring) %>%
  summarise(
    n_tract_years = n(),
    n_unique_tracts = n_distinct(GISJOIN_1950),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = distance_ring, values_from = c(n_tract_years, n_unique_tracts), values_fill = 0)

cat("\nSample sizes by city:\n")
print(city_ring_summary)

# Output datasets ----
cat("\nSaving distance-based datasets...\n")

# Output event study data with distance rings
write_csv(event_study_data_distance_rings, event_study_distance_rings_output_path)

# Output unique tracts and distance ring status
unique_tracts_distance_rings <- event_study_data_distance_rings %>%
  dplyr::select(GISJOIN_1950, distance_ring, treatment_year, distance_from_project) %>%
  distinct() %>%
  # Keep first assignment for tracts assigned to multiple projects
  group_by(GISJOIN_1950) %>%
  filter(row_number() == 1) %>%
  ungroup()

write_csv(unique_tracts_distance_rings, unique_tracts_distance_rings_output_path)

cat("Distance-based spatial DiD datasets created successfully!\n")
cat("Main dataset:", event_study_distance_rings_output_path, "\n")
cat("Unique tracts:", unique_tracts_distance_rings_output_path, "\n")
cat("Results saved to:", merged_data_dir, "\n")