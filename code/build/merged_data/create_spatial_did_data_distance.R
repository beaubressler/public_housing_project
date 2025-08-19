###
# load packages
library(here)
library(sf)
library(tidyverse)
library(MatchIt)

# file paths
# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small" or "combined"
data_type <- "combined"

## define filepaths  -----
# output paths
merged_data_dir <- here("data", "derived", "merged", data_type)
ph_data_dir <- here("data", "derived", "public_housing", "working")
# Output
distance_rings_stacked_output_path <- here(merged_data_dir, "distance_rings_stacked.csv")


census_tract_sample_path <- here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")
public_housing_sample_filepath <-
  here(ph_data_dir, data_type, "public_housing_sample_balanced.gpkg")

# Read in the census tract sample
census_tract_sample <- st_read(census_tract_sample_path)

# Read in projects in the treated sample
public_housing_sample <- st_read(public_housing_sample_filepath)


# SPATIAL RING CREATION ========================================================

cat("\n=== SPATIAL RING CREATION ===\n")

# Get unique tracts for ring assignment
unique_tracts <- census_tract_sample %>%
  group_by(GISJOIN_1950) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(GISJOIN_1950, geom)  # Keep only ID and geometry

cat("Census tracts:", nrow(unique_tracts), "\n")
cat("Public housing projects:", nrow(public_housing_sample), "\n")

# Define distance thresholds (in meters)
distance_thresholds <- list(
  inner_1_max = 400,    # 50-400m = inner ring 1  
  inner_2_max = 800,    # 400-700m = inner ring 2
  control_min = 800,    # 700-1200m = control ring (expanded from 1000m)
  control_max = 1500
)

# Calculate distances between all tracts and projects using full geometries
cat("Calculating distances between tracts and projects using full geometries...\n")
distances <- st_distance(unique_tracts, public_housing_sample)
distances_df <- as.data.frame(distances)
names(distances_df) <- paste0("project_", 1:nrow(public_housing_sample))

# Create stacked assignments
cat("Creating stacked spatial assignments...\n")
stacked_assignments <- unique_tracts %>%
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
  # Assign to rings - back to 3 rings
  mutate(location_type = case_when(
    distance <= 50 ~ "treated_0_50",
    distance > 50 & distance <= distance_thresholds$inner_2_max ~ "spillover_50_800",
    distance >= distance_thresholds$control_min & distance <= distance_thresholds$control_max ~ "control_800_1500",
    TRUE ~ "beyond"
  )) %>%
  filter(location_type != "beyond") %>%
  # Add project information
  left_join(
    public_housing_sample %>% 
      st_drop_geometry() %>% 
      mutate(project_id = row_number()) %>%
      select(project_id, treatment_year, total_public_housing_units),
    by = "project_id"
  )

cat("Stacked assignment summary:\n")
cat("Total tract-project pairs within analysis range:", nrow(stacked_assignments), "\n")
print(table(stacked_assignments$location_type))

# CONTAMINATION CLEANING ========================================================

cat("\n=== CONTAMINATION CLEANING ===\n")

# Following Chicago approach: identify tracts that are treated for some project
treated_tract_ids <- stacked_assignments %>%
  filter(location_type %in% c("treated_0_50", "spillover_50_800")) %>%
  select(GISJOIN_1950) %>%
  distinct()

cat("Treated tract IDs:", nrow(treated_tract_ids), "\n")

# Control tracts should not be within treatment range of ANY other project
contaminated_controls <- stacked_assignments %>%
  filter(location_type == "control_800_1500", GISJOIN_1950 %in% treated_tract_ids$GISJOIN_1950) %>%
  select(GISJOIN_1950, location_type) %>%
  distinct()

cat("Before contamination cleaning:", nrow(stacked_assignments), "tract-project assignments\n")
cat("Removing", nrow(contaminated_controls), "contaminated control tracts\n")

# Remove contaminated controls
clean_assignments <- stacked_assignments %>%
  anti_join(contaminated_controls, by = c("GISJOIN_1950", "location_type"))

cat("After control contamination cleaning:", nrow(clean_assignments), "assignments\n")

# Step 2: For multiply-treated tracts, keep only first treatment (Chicago approach)
cat("Removing multiple treatment assignments - keeping earliest treatment...\n")
clean_assignments <- clean_assignments %>%
  group_by(GISJOIN_1950) %>%
  filter(treatment_year == min(treatment_year)) %>%
  ungroup()

cat("After first treatment filter:", nrow(clean_assignments), "assignments\n")

# # Step 3: Ensure each tract appears exactly once - keep closest project (Chicago approach)
# Commenting out: This allows spillover tracts to appear multiple times
# cat("Ensuring unique tract assignments - keeping closest project...\n")
# clean_assignments <- clean_assignments %>%
#   group_by(GISJOIN_1950) %>%
#   filter(distance == min(distance)) %>%  # Keep closest project
#   slice_min(project_id, n = 1, with_ties = FALSE) %>%  # Deterministic tie-breaking
#   ungroup()
# 
# cat("After complete deduplication:", nrow(clean_assignments), "assignments for", n_distinct(clean_assignments$GISJOIN_1950), "unique tracts\n")

# Check clean control tracts
clean_controls <- clean_assignments %>%
  filter(location_type == "control_800_1500") %>%
  distinct(GISJOIN_1950)

cat("Clean control tracts:", nrow(clean_controls), "\n")

# Summary by location type after cleaning
cat("\nClean assignments by location type:\n")
print(table(clean_assignments$location_type))

# FILTER TO PROJECTS WITH CONTROLS =============================================

cat("\n=== FILTERING TO PROJECTS WITH CONTROLS ===\n")

# Identify projects that have control observations
projects_with_controls <- clean_assignments %>%
  group_by(project_id) %>%
  summarise(has_control = any(location_type == "control_800_1500"), .groups = "drop")

cat("Projects with controls summary:\n")
print(table(projects_with_controls$has_control))

projects_to_keep <- projects_with_controls %>%
  filter(has_control == TRUE) %>%
  pull(project_id)

cat("Projects with controls:", length(projects_to_keep), "\n")
cat("Projects without controls:", sum(!projects_with_controls$has_control), "\n")

# Filter to only projects with controls
clean_assignments <- clean_assignments %>%
  filter(project_id %in% projects_to_keep)

cat("After filtering to projects with controls:", nrow(clean_assignments), "assignments\n")

# Summary by location type after filtering
cat("\nFinal assignments by location type:\n")
print(table(clean_assignments$location_type))

# Keep only nearby controls =================================

K_max <- 1  # Maximum number of controls to keep per project

# Apply matching to each project

match_project_controls <- function(project_data, K_max = 3) {
  
  # Get baseline covariates and trends using event_time approach
  baseline_data <- project_data %>%
    expand_grid(YEAR = seq(1930, 1990, by = 10)) %>%
    mutate(event_time = YEAR - treatment_year) %>%
    filter(event_time %in% c(-20, -10)) %>%  # Get both periods for trend calculation
    left_join(
      census_tract_sample %>%
        st_drop_geometry() %>%
        select(GISJOIN_1950, YEAR, white_pop, black_pop, median_income, unemp_rate, black_share),
      by = c("GISJOIN_1950", "YEAR")
    ) %>%
    # Calculate levels and trends within each project's timeframe
    group_by(GISJOIN_1950) %>%
    filter(n() == 2) %>%  # Keep only tracts with both event_time = -20 and -10
    mutate(
      # Level variables at event_time = -10
      asinh_white_pop = asinh(white_pop[event_time == -10]),
      asinh_black_pop = asinh(black_pop[event_time == -10]),
      asinh_median_income = asinh(median_income[event_time == -10]),
      unemp_rate_level = unemp_rate[event_time == -10],
      # Trend variables (change from event_time = -20 to -10)
      delta_black_share = black_share[event_time == -10] - black_share[event_time == -20],
      delta_log_white_pop = ifelse(all(white_pop > 0), 
                                   log(white_pop[event_time == -10]) - log(white_pop[event_time == -20]), NA),
      delta_log_median_income = ifelse(all(median_income > 0),
                                       log(median_income[event_time == -10]) - log(median_income[event_time == -20]), NA),
      is_control = ifelse(location_type == "control_800_1500", 1, 0)
    ) %>%
    ungroup() %>%
    filter(event_time == -10) %>%  # Keep only event_time = -10 for matching
    # Filter for complete cases including trend variables
    filter(complete.cases(asinh_white_pop, asinh_black_pop, asinh_median_income, unemp_rate_level,
                         delta_black_share, delta_log_white_pop, delta_log_median_income)) %>%
    select(GISJOIN_1950, project_id, treatment_year, distance, location_type, is_control,
           asinh_white_pop, asinh_black_pop, asinh_median_income, unemp_rate_level,
           delta_black_share, delta_log_white_pop, delta_log_median_income)
  
  # Split treated/spillover vs control
  treated_data <- baseline_data %>% filter(is_control == 0)
  control_data <- baseline_data %>% filter(is_control == 1)
  
  # If not enough controls or treated, return all treated/spillover and no controls
  if (nrow(treated_data) == 0 | nrow(control_data) == 0) {
    return(treated_data %>% select(GISJOIN_1950, project_id, treatment_year, distance, location_type))
  }
  
  # Define covariate matrix - include both levels AND trends
  covariates <- c(
    "asinh_white_pop", "asinh_black_pop", "asinh_median_income", "unemp_rate_level",
                  "delta_black_share", "delta_log_white_pop", "delta_log_median_income")
  
  # Regularized covariance matrix
  cov_matrix <- cov(rbind(treated_data[, covariates], control_data[, covariates])) +
    diag(1e-6, length(covariates))
  
  # Mean of treated units
  center <- colMeans(treated_data[, covariates])
  
  # Compute Mahalanobis distances
  control_data$mahal_dist <- mahalanobis(
    x = control_data[, covariates],
    center = center,
    cov = cov_matrix
  )
  
  # Filter controls within chi-squared threshold
  threshold_95 <- qchisq(0.95, df = length(covariates))
  threshold_75 <- qchisq(0.75, df = length(covariates))
  threshold_50 <- qchisq(0.75, df = length(covariates))
  
  # Bind distances and select closest controls
  matched_controls <- control_data %>%
    arrange(mahal_dist) %>%
    # Keep only those within the 75th percentile threshold
    filter(mahal_dist <= threshold_50) %>%
    # Keep at most K_max controls per treated unit
    slice_head(n = min(K_max, nrow(.))) %>%
    select(GISJOIN_1950, project_id, treatment_year, distance, location_type)
  
  # Return all treated + matched controls
  matched_data <- treated_data %>%
    select(GISJOIN_1950, project_id, treatment_year, distance, location_type) %>%
    bind_rows(matched_controls)
  
  return(matched_data)
}


# Apply matching to each project using split-apply-combine approach
project_list <- clean_assignments %>%
  split(.$project_id)

matched_assignments <- map_dfr(project_list, match_project_controls)

cat("After KNN matching:\n")
cat("Total assignments:", nrow(matched_assignments), "\n")
print(table(matched_assignments$location_type))
print(table(clean_assignments$location_type))


# Show controls per project after matching
controls_per_project <- matched_assignments %>%
  filter(location_type == "control_800_1500") %>%
  group_by(project_id) %>%
  summarise(n_controls = n(), .groups = "drop") %>%
  pull(n_controls) %>%
  table()

cat("Controls per project after matching:\n")
print(controls_per_project)

# Update clean_assignments to use matched version
clean_assignments <- matched_assignments

# EXPAND TO PANEL FORMAT ====================================================

cat("\n=== CREATING EVENT STUDY PANEL ===\n")

# Expand stacked data to panel format
years <- seq(1930, 1990, by = 10)

# Create event study panel
event_study_panel <- clean_assignments %>%
  # Cross join with years
  expand_grid(YEAR = years) %>%
  # Create event study variables
  mutate(
    event_time = YEAR - treatment_year,
    treated = ifelse(YEAR >= treatment_year & location_type %in% c("treated_0_50", "spillover_50_800"), TRUE, FALSE),
    cohort = as.factor(treatment_year),
    treated_id = project_id  # Use project_id as treated_id for consistency
  )

cat("Event study panel created:\n")
cat("Total observations:", nrow(event_study_panel), "\n")
cat("Years:", min(event_study_panel$YEAR), "to", max(event_study_panel$YEAR), "\n")

# MERGE WITH CENSUS DATA =====================================================

cat("\n=== MERGING WITH CENSUS DATA ===\n")

# Merge with census data
event_study_data <- event_study_panel %>%
  left_join(
    census_tract_sample %>%
      st_drop_geometry() %>%
      select(GISJOIN_1950, STATE, COUNTY, TRACTA, YEAR, city, cbsa_title,
             # Population by race
             black_share, white_share, white_pop, black_pop, total_pop,
             # Private population 
             private_population_estimate, private_black_population_estimate, private_white_population_estimate,
             # Income and housing
             median_income, median_rent_calculated, median_home_value_calculated,
             # SES measures
             pct_hs_grad, unemp_rate, lfp_rate,
             # Geographic controls
             population_density, distance_from_cbd,
             # Policy controls
             ur_binary_10pp, has_highway_1km),
    by = c("GISJOIN_1950", "YEAR")
  ) %>%
  # Add transformations
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
  # Calculate WING (2024) weights
  group_by(treatment_year, treated_id, location_type) %>%
  mutate(N_re = n()) %>%  # Observations in ring r around project e
  ungroup() %>%
  group_by(treatment_year, location_type) %>%
  mutate(N_r = n()) %>%  # Total observations in ring r across all projects
  ungroup() %>%
  mutate(wing_weight = N_re / N_r)

cat("Census data merged:\n")
cat("Final dataset size:", nrow(event_study_data), "\n")

# Check for missing data
missing_summary <- event_study_data %>%
  summarise(
    missing_black_share = sum(is.na(black_share)),
    missing_income = sum(is.na(median_income)),
    missing_housing = sum(is.na(median_home_value_calculated)),
    missing_controls = sum(is.na(ur_binary_10pp) | is.na(has_highway_1km))
  )

cat("\nMissing data summary:\n")
print(missing_summary)

# SAVE OUTPUT ========================================================

cat("\n=== SAVING OUTPUT ===\n")

# Save the clean stacked assignments
write_csv(clean_assignments, distance_rings_stacked_output_path)

# Save the complete event study dataset
distance_rings_event_study_output_path <- here(merged_data_dir, "distance_rings_event_study.csv")
write_csv(event_study_data, distance_rings_event_study_output_path)

cat("Clean stacked distance rings saved to:", distance_rings_stacked_output_path, "\n")
cat("Complete event study dataset saved to:", distance_rings_event_study_output_path, "\n")
cat("Dataset ready for spatial DiD analysis!\n")


clean_assignments %>% 
    group_by(project_id, location_type) %>%
    summarise(num_observations = n(), .groups = "drop") %>%
  arrange(project_id, location_type) %>%
  filter(location_type == "treated_0_50") %>%
  pull(num_observations) %>% 
  table()

clean_assignments %>% 
  group_by(project_id, location_type) %>%
  summarise(num_observations = n(), .groups = "drop") %>%
  arrange(project_id, location_type) %>%
  filter(location_type == "spillover_50_800") %>%
  pull(num_observations) %>% 
  table()

clean_assignments %>% 
  group_by(project_id, location_type) %>%
  summarise(num_observations = n(), .groups = "drop") %>%
  arrange(project_id, location_type) %>%
  filter(str_detect(location_type,"control_800")) %>%
  pull(num_observations) %>% 
  table()


# ============================================================================
# SPILLOVER ANALYSIS AND MAPPING
# ============================================================================

# Analyze spillover distribution
spillover_summary <- clean_assignments %>%
  filter(location_type == "spillover_50_800") %>%
  group_by(project_id) %>%
  summarise(n_spillover = n(), .groups = "drop") %>%
  arrange(desc(n_spillover))

cat("Top 10 projects with most spillover tracts:\n")
print(head(spillover_summary, 10))

# Get project details for high spillover projects
high_spillover_details <- spillover_summary %>%
  slice_head(n = 5) %>%
  left_join(
    public_housing_sample %>%
      st_drop_geometry() %>%
      mutate(project_id = row_number()) %>%
      select(project_id, locality, project_name, treatment_year, total_public_housing_units),
    by = "project_id"
  )

cat("\nDetails of projects with most spillover tracts:\n")
print(high_spillover_details)

# Create simple map for one high-spillover project
focus_project <- spillover_summary$project_id[1]
cat("\nMapping project:", focus_project, "\n")

# Get tracts for focused project
project_tracts <- clean_assignments %>%
  filter(project_id == focus_project)

# Get tract geometries
tract_data <- census_tract_sample %>%
  filter(GISJOIN_1950 %in% project_tracts$GISJOIN_1950, YEAR == 1950) %>%
  left_join(project_tracts, by = "GISJOIN_1950")

# Get project location
project_location <- public_housing_sample %>%
  mutate(project_id = row_number()) %>%
  filter(project_id == focus_project)

# Simple mapping
library(ggplot2)
map_plot <- ggplot() +
  geom_sf(data = tract_data, aes(fill = location_type), alpha = 0.7) +
  geom_sf(data = project_location, color = "red", size = 4) +
  scale_fill_manual(values = c(
    "treated_0_50" = "darkred",
    "spillover_50_800" = "orange",
    "control_800_1500" = "lightblue"
  )) +
  theme_void() +
  labs(title = paste("Project", focus_project, "Spatial Rings"))

print(map_plot)

# Summary stats
cat("\nRing counts for project", focus_project, ":\n")
print(table(project_tracts$location_type))

