# ==============================================================================
# Chicago Land Value Data Preparation
# 
# This script prepares the analysis dataset for Chicago land value spatial DiD.
# Combines Ahlfeldt-McMillen land value data with public housing projects,
# creates spatial rings, highway controls, and urban renewal indicators.
#
# Output: Clean analysis dataset saved for regression analysis
# ==============================================================================

# SETUP ========================================================================

# Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(here)
  library(haven)
  library(units)
})

set.seed(123)

# Configuration
crs_chicago <- 26971  # NAD83 Illinois East Zone for accurate distance calculations

# Ring distance definitions (in meters) - using original controls 
ring_width <- 100            # 100m ring width
max_ring_distance <- 600     # Maximum distance for treated rings (0.6 km)
control_ring_start <- 800    # Control ring starts at 800m  
control_ring_end <- 1000     # Control ring ends at 1000m (800-1000m control)

# Highway control configuration
include_highway_controls <- TRUE  # Whether to include highway proximity controls

# Output directory
data_dir <- here("output", "data", "chicago_land_values")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# DATA LOADING =================================================================

cat("=== DATA LOADING ===\n")

## 1. Load Ahlfeldt-McMillen land value data
cat("Loading Ahlfeldt-McMillen land value data...\n")
land_value_data <- read_dta(here("data", "raw", "ahlfeldt_mcmillen", "GRID_LONG.dta"))

land_value_data_sf <- land_value_data %>%
  filter(!is.na(longitude), !is.na(latitude), !is.na(llv)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = crs_chicago)

cat("Land value data loaded:", nrow(land_value_data_sf), "observations\n")
cat("Years available:", paste(sort(unique(land_value_data_sf$Year)), collapse = ", "), "\n")

## 2. Load Chicago public housing data
cat("Loading Chicago public housing data...\n")
public_housing_data <- read_csv(here("data", "derived", "public_housing", "working", "combined_cdd_and_digitized_projects.csv")) %>%
  filter(locality == "CHICAGO", !is.na(latitude), !is.na(longitude), !is.na(year_completed)) %>%
  mutate(treatment_year = case_when(
    year_completed %in% 1933:1939 ~ 1939,
    year_completed %in% 1940:1949 ~ 1949,
    year_completed %in% 1950:1961 ~ 1961,
    year_completed %in% 1962:1971 ~ 1971,
    year_completed %in% 1972:1981 ~ 1981,
    year_completed %in% 1982:1990 ~ 1990,
    year_completed %in% 1991:2000 ~ 2000,
    year_completed %in% 2001:2009 ~ 2009,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(treatment_year), total_units >= 50) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = crs_chicago)

cat("Public housing projects loaded:", nrow(public_housing_data), "projects\n") 
cat("Treatment years:", paste(sort(unique(public_housing_data$treatment_year)), collapse = ", "), "\n")

## 3. Load urban renewal data
cat("Loading Chicago urban renewal spatial boundaries...\n")
ur_spatial <- st_read(here("data", "raw", "Renewing_Inequality_Data-master", "Data", "shapefile", "ur_projects.shp"), quiet = TRUE) %>%
  filter(str_detect(tolower(city), "chicago")) %>%
  st_transform(crs = crs_chicago)

cat("Urban renewal projects loaded:", nrow(ur_spatial), "projects\n")

## 4. Load highway data (if using highway controls)
if(include_highway_controls) {
  cat("Loading highway shapefiles for direct distance calculation...\n")
  highways_raw <- st_read(here("data", "raw", "highways", "weiwu_2024", "highways_actual.shp"), quiet = TRUE)
  highways <- st_transform(highways_raw, crs = crs_chicago)
  cat("Highway data loaded:", nrow(highways), "highway segments\n")
} else {
  cat("Highway controls disabled\n")
}

# SPATIAL RING CREATION ========================================================

cat("\n=== SPATIAL RING CREATION ===\n")

# Get representative land value points
land_value_points <- land_value_data_sf %>% 
  filter(Year == 1961) %>%
  select(grid_id, geometry)

cat("Land value points:", nrow(land_value_points), "\n")
cat("Public housing projects:", nrow(public_housing_data), "\n")

# Calculate distances between all points and projects
cat("Calculating distances between points and projects...\n")
distances <- st_distance(land_value_points, public_housing_data)
distances_df <- as.data.frame(distances)
names(distances_df) <- paste0("project_", 1:nrow(public_housing_data))

# Create stacked assignments
cat("Creating stacked spatial assignments...\n")
stacked_assignments <- land_value_points %>%
  st_drop_geometry() %>%
  bind_cols(distances_df) %>%
  pivot_longer(
    cols = starts_with("project_"),
    names_to = "project_id_temp", 
    values_to = "distance"
  ) %>%
  mutate(
    project_id = as.integer(str_extract(project_id_temp, "[0-9]+")),
    distance = as.numeric(distance)
  ) %>%
  select(-project_id_temp) %>%
  # Assign to rings - including 600-800m treatment ring (no gap)
  mutate(location_type = case_when(
    distance <= 200 ~ "ring_0_200",
    distance <= 400 ~ "ring_200_400", 
    distance <= 600 ~ "ring_400_600",
    distance <= 800 ~ "ring_600_800",
    distance > control_ring_start & distance <= control_ring_end ~ "control_800_1000",
    TRUE ~ "beyond"
  )) %>%
  filter(location_type != "beyond") %>%
  # Add project information
  left_join(
    public_housing_data %>% 
      st_drop_geometry() %>% 
      mutate(project_id = row_number()) %>%
      select(project_id, treatment_year, total_units),
    by = "project_id"
  )

cat("Stacked assignment summary:\n")
cat("Total point-project pairs within max distance:", nrow(stacked_assignments), "\n")
print(table(stacked_assignments$location_type))

# CONTAMINATION CONTROL ========================================================

cat("\n=== CLEANING CONTAMINATION ===\n")

# Step 1: Get all grid_ids that are ever in treatment rings
treated_grid_ids <- stacked_assignments %>%
  filter(str_detect(location_type, "ring_")) %>%
  select(grid_id) %>%
  distinct()

# Control points should not be within treatment range of ANY other project
contaminated_controls <- stacked_assignments %>%
  filter(location_type == "control_800_1000", grid_id %in% treated_grid_ids$grid_id) %>%
  select(grid_id, location_type) %>%
  distinct()

cat("Removing", nrow(contaminated_controls), "contaminated control points\n")

# Remove contaminated controls
clean_stacked_assignments <- stacked_assignments %>%
  anti_join(contaminated_controls, by = c("grid_id", "location_type"))

# Step 2: For multiply-treated points, keep only first treatment
clean_stacked_assignments <- clean_stacked_assignments %>%
  group_by(grid_id) %>%
  filter(treatment_year == min(treatment_year)) %>%
  ungroup()

# Step 3: Ensure each grid_id appears exactly once
clean_stacked_assignments <- clean_stacked_assignments %>%
  group_by(grid_id) %>%
  filter(distance == min(distance)) %>%  # Keep closest project
  slice_min(project_id, n = 1, with_ties = FALSE) %>%  # Deterministic tie-breaking
  ungroup()

cat("Clean assignments:", nrow(clean_stacked_assignments), "for", n_distinct(clean_stacked_assignments$grid_id), "unique grid points\n")

# Create final panel dataset
stacked_panel <- clean_stacked_assignments %>%
  crossing(year = sort(unique(land_value_data_sf$Year))) %>%
  mutate(unit_id = paste(grid_id, project_id, sep = "_"))

# URBAN RENEWAL CONTROLS =======================================================

cat("\n=== CREATING URBAN RENEWAL CONTROLS ===\n")

# Create urban renewal indicators
if(nrow(ur_spatial) > 0) {
  land_value_points_ur <- land_value_data_sf %>% filter(Year == 1961)
  
  ur_indicators <- st_join(land_value_points_ur, 
                          ur_spatial %>% select(project_id),
                          join = st_within) %>%
    st_drop_geometry() %>%
    mutate(in_urban_renewal = !is.na(project_id)) %>%
    group_by(grid_id) %>%
    summarise(in_urban_renewal = any(in_urban_renewal), .groups = "drop")
} else {
  ur_indicators <- land_value_points %>%
    st_drop_geometry() %>%
    mutate(in_urban_renewal = FALSE) %>%
    select(grid_id, in_urban_renewal)
}

cat("Grid points in urban renewal areas:", sum(ur_indicators$in_urban_renewal), "\n")

# HIGHWAY CONTROLS =============================================================

cat("\n=== CREATING HIGHWAY CONTROLS ===\n")

if(include_highway_controls) {
  
  cat("Calculating highway proximity using spatial buffers...\n")
  
  # Get grid points in Chicago projection
  land_value_grid_points <- land_value_points %>%
    st_transform(crs = crs_chicago)
  
  # Filter highways to Chicago area for efficiency
  chicago_bbox <- st_bbox(land_value_grid_points)
  highways_chicago <- st_crop(highways, chicago_bbox)
  cat("Highways in Chicago area:", nrow(highways_chicago), "\n")
  
  # Create highway buffers (much faster than exact distance calculation)
  highway_buffer_1km <- st_union(st_buffer(highways_chicago, dist = 1000))  # 1km buffer
  highway_buffer_2km <- st_union(st_buffer(highways_chicago, dist = 2000))  # 2km buffer
  
  # Use spatial intersection to determine proximity
  grid_within_1km <- st_intersects(land_value_grid_points, highway_buffer_1km, sparse = FALSE)
  grid_within_2km <- st_intersects(land_value_grid_points, highway_buffer_2km, sparse = FALSE)
  
  highway_indicators <- land_value_grid_points %>%
    st_drop_geometry() %>%
    mutate(
      has_highway_1km = as.logical(grid_within_1km),
      has_highway_2km = as.logical(grid_within_2km)
    ) %>%
    select(grid_id, has_highway_1km, has_highway_2km)
  
  cat("Grid points within 1km of highway:", sum(highway_indicators$has_highway_1km, na.rm = TRUE), "\n")
  cat("Grid points within 2km of highway:", sum(highway_indicators$has_highway_2km, na.rm = TRUE), "\n")
  
} else {
  # Create dummy highway indicators
  highway_indicators <- land_value_points %>%
    st_drop_geometry() %>%
    mutate(
      has_highway_1km = FALSE,
      has_highway_2km = FALSE
    ) %>%
    select(grid_id, has_highway_1km, has_highway_2km)
  
  cat("Highway controls disabled - using dummy indicators\n")
}

# FINAL DATASET CREATION =======================================================

cat("\n=== CREATING FINAL ANALYSIS DATASET ===\n")

# Merge everything together
land_value_stacked <- stacked_panel %>%
  left_join(
    land_value_data_sf %>% 
      st_drop_geometry() %>%
      select(grid_id, Year, llv),
    by = c("grid_id", "year" = "Year")
  ) %>%
  left_join(ur_indicators, by = "grid_id") %>%
  left_join(highway_indicators, by = "grid_id") %>%
  filter(year >= 1913) %>%
  # Create event study variables
  mutate(
    event_time = year - treatment_year,
    treated = (str_detect(location_type, "ring_") & year >= treatment_year),
    ring_numeric = case_when(
      location_type == "ring_0_200" ~ 0,
      location_type == "ring_200_400" ~ 1,
      location_type == "ring_400_600" ~ 2,
      location_type == "ring_600_800" ~ 3,
      location_type == "control_800_1000" ~ 4
    )
  ) %>%
  filter(!is.na(llv))

cat("Final dataset summary:\n")
cat("Total observations:", nrow(land_value_stacked), "\n")
cat("Unique grid points:", n_distinct(land_value_stacked$grid_id), "\n")
cat("Unique projects:", n_distinct(land_value_stacked$project_id), "\n")
cat("Year range:", min(land_value_stacked$year), "-", max(land_value_stacked$year), "\n")

# EVENT STUDY DATA PREPARATION =================================================

cat("\n=== PREPARING EVENT STUDY DATA ===\n")

# Get ordered sequence of available data years
data_years <- sort(unique(land_value_stacked$year))
cat("Available data years:", paste(data_years, collapse = ", "), "\n")

# Function to convert year and treatment_year to relative period
get_relative_period <- function(year, treatment_year) {
  year_position <- match(year, data_years)
  treatment_position <- match(treatment_year, data_years)
  return(year_position - treatment_position)
}

event_study_data <- land_value_stacked %>%
  filter(str_detect(location_type, "ring_") | location_type == "control_800_1000") %>%
  mutate(
    event_time = mapply(get_relative_period, year, treatment_year),
    cohort = paste0("cohort_", treatment_year),
    # Add linear time trend variables for cohort-specific trends
    time_trend = year - min(year),  # Linear time variable starting from 0
    # Alternative: trend from treatment year
    time_trend_from_treatment = year - treatment_year
  )

cat("Event study data created:", nrow(event_study_data), "observations\n")
cat("Event time range:", min(event_study_data$event_time), "to", max(event_study_data$event_time), "\n")

# WING WEIGHTS CALCULATION =====================================================

cat("\n=== CALCULATING WING (2024) WEIGHTS ===\n")

event_study_data <- event_study_data %>%
  group_by(treatment_year, project_id, location_type) %>%
  mutate(N_re = n()) %>%
  ungroup() %>%
  group_by(treatment_year, location_type) %>%
  mutate(N_r = n()) %>%
  ungroup() %>%
  mutate(wing_weight = N_re / N_r)

cat("Wing weights calculated\n")
cat("Weight range:", round(min(event_study_data$wing_weight), 4), "to", round(max(event_study_data$wing_weight), 4), "\n")

# BASELINE CONTROLS CALCULATION ===============================================

cat("\n=== CALCULATING BASELINE CONTROLS ===\n")

# Calculate baseline land values (mean of event_time -3 to -2) for each grid-project pair
baseline_controls <- event_study_data %>%
  filter(event_time %in% c(-3, -2)) %>%
  group_by(grid_id, project_id) %>%
  summarise(
    baseline_llv = mean(llv, na.rm = TRUE),
    n_baseline_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_baseline_obs > 0, !is.na(baseline_llv))

cat("Baseline controls calculated for", nrow(baseline_controls), "grid-project pairs\n")
cat("Baseline period: event_time -3 to -2\n")
cat("Baseline land value range:", round(min(baseline_controls$baseline_llv), 3), "to", round(max(baseline_controls$baseline_llv), 3), "\n")

# Merge baseline controls back to main dataset
event_study_data <- event_study_data %>%
  left_join(
    baseline_controls %>% select(grid_id, project_id, baseline_llv),
    by = c("grid_id", "project_id")
  )

# Check merge success
cat("Baseline controls merged: ", sum(!is.na(event_study_data$baseline_llv)), "of", nrow(event_study_data), "observations have baseline values\n")

# SAVE FINAL DATASETS ==========================================================

cat("\n=== SAVING FINAL DATASETS ===\n")

# Save main analysis dataset
write_csv(event_study_data, here(data_dir, "chicago_land_value_analysis_data.csv"))

# Save public housing project data for reference
public_housing_summary <- public_housing_data %>%
  st_drop_geometry() %>%
  mutate(project_id = row_number()) %>%
  select(project_id, treatment_year, total_units, year_completed)

write_csv(public_housing_summary, here(data_dir, "chicago_public_housing_projects.csv"))

# Create summary statistics
summary_stats <- event_study_data %>%
  filter(event_time >= -3, event_time <= 5) %>%
  group_by(location_type) %>%
  summarise(
    n_obs = n(),
    n_projects = n_distinct(project_id),
    n_grid_points = n_distinct(grid_id),
    mean_llv = mean(llv, na.rm = TRUE),
    sd_llv = sd(llv, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summary_stats, here(data_dir, "chicago_summary_statistics.csv"))

cat("\n=== DATA PREPARATION COMPLETE ===\n")
cat("Analysis data saved to:", here(data_dir, "chicago_land_value_analysis_data.csv"), "\n")
cat("Summary statistics saved to:", here(data_dir, "chicago_summary_statistics.csv"), "\n")
cat("\nKey statistics:\n")
cat("Final sample size:", nrow(event_study_data), "observations\n")
cat("Number of public housing projects:", nrow(public_housing_data), "\n")
cat("Treatment periods:", paste(sort(unique(event_study_data$treatment_year)), collapse = ", "), "\n")
cat("Highway controls included:", include_highway_controls, "\n")