# ==============================================================================
# Chicago Land Value Spatial Difference-in-Differences Analysis
# 
# This script implements a stacked spatial DiD design using Ahlfeldt-McMillen 
# land value grid data and Chicago public housing projects (≥50 units).
#
# Key Features:
# - Spatial rings: treated (0-0.4km), inner (0.4-0.8km), outer (0.8-1.6km)
# - Clean sub-experiments to avoid contamination across projects
# - Wing (2024) weights for proper stacked DiD estimation
# - Multiple robustness specifications with urban renewal controls
# ==============================================================================

# SETUP ========================================================================

# Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(here)
  library(haven)
  library(fixest)
  library(ggfixest)
  library(units)
  library(viridis)
  library(RColorBrewer)
})

set.seed(123)

# Configuration
crs_chicago <- 26971  # NAD83 Illinois East Zone for accurate distance calculations
data_type <- "combined"  # Which public housing dataset to use

# Highway control configuration
include_highway_controls <- TRUE  # Whether to include highway proximity controls
highway_control_distance <- 1     # Distance threshold in km for highway controls

# Ring distance definitions (in meters) - following Blanco & Neri (2025)
# Note: Land value data has 300x300 feet (~91m) resolution, so 100m rings are appropriate
ring_width <- 100            # 100m ring width
max_ring_distance <- 800     # Maximum distance for treated rings (0.8 km)
control_ring_start <- 800    # Control ring starts at 800m
control_ring_end <- 1000     # Control ring ends at 1000m (800-1000m control)

# Output directories
figures_dir <- here("output", "figures", "chicago_land_values")
results_dir <- here("output", "regression_results", "chicago_land_values")
data_dir <- here("output", "data", "chicago_land_values")

# Create directories
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# HELPER FUNCTIONS =============================================================

#' Run spatial DiD regression with specified data filters and controls
#' 
#' @param data Event study dataset
#' @param location_filter Which location types to include
#' @param urban_renewal_treatment How to handle urban renewal: "baseline", "exclude", "control"  
#' @param highway_controls Whether to include highway proximity controls
#' @param period_filter Optional filter for treatment years
#' @param spec_name Name for the specification (for logging)
run_spatial_did <- function(data, location_filter, urban_renewal_treatment = "baseline", 
                           highway_controls = FALSE, period_filter = NULL, spec_name = "Specification") {
  
  # Apply filters
  analysis_data <- data %>%
    filter(location_type %in% location_filter,
           event_time >= -3, event_time <= 5)
  
  # Apply period filter if specified
  if (!is.null(period_filter)) {
    if (period_filter$type == "early") {
      analysis_data <- analysis_data %>% filter(treatment_year <= period_filter$cutoff)
    } else if (period_filter$type == "late") {
      analysis_data <- analysis_data %>% filter(treatment_year > period_filter$cutoff)
    }
  }
  
  # Handle urban renewal and highway controls
  if (urban_renewal_treatment == "exclude") {
    analysis_data <- analysis_data %>% filter(!in_urban_renewal)
    if (highway_controls) {
      fixed_effects <- "project_id^year + project_id^location_type + grid_id + has_highway_1km^year"
    } else {
      fixed_effects <- "project_id^year + project_id^location_type + grid_id"
    }
  } else if (urban_renewal_treatment == "control") {
    if (highway_controls) {
      fixed_effects <- "project_id^year + project_id^location_type + grid_id + in_urban_renewal^year + has_highway_1km^year"
    } else {
      fixed_effects <- "project_id^year + project_id^location_type + grid_id + in_urban_renewal^year"
    }
  } else {
    if (highway_controls) {
      fixed_effects <- "project_id^year + project_id^location_type + grid_id + has_highway_1km^year"
    } else {
      fixed_effects <- "project_id^year + project_id^location_type + grid_id"
    }
  }
  
  # Run regression
  cat(paste("\n=== ", spec_name, " ===\n"))
  cat("Sample size:", nrow(analysis_data), "observations\n")
  
  # Check that -1 exists as reference period
  available_event_times <- sort(unique(analysis_data$event_time))
  cat("Available event times:", paste(available_event_times, collapse = ", "), "\n")
  
  if(!(-1 %in% available_event_times)) {
    stop("Reference period -1 not found in data. Available event times: ", paste(available_event_times, collapse = ", "))
  }
  
  cat("Using reference event time: -1\n")
  
  model <- feols(
    fml = as.formula(paste("llv ~ i(event_time, location_type, ref = -1, ref2 = 'control_800_1000') |", fixed_effects)),
    data = analysis_data,
    weights = ~ wing_weight,
    cluster = ~ project_id
  )
  
  cat("Regression completed\n")
  return(model)
}

#' Run spatial DiD regression with ALL rings in single regression (Blanco & Neri approach)
#' 
#' @param data Event study dataset with all location types
#' @param urban_renewal_treatment How to handle urban renewal: "baseline", "exclude", "control"
#' @param highway_controls Whether to include highway proximity controls
#' @param period_filter Optional filter for treatment years
#' @param spec_name Name for the specification (for logging)
run_spatial_did_all_rings <- function(data, urban_renewal_treatment = "baseline", 
                                     highway_controls = FALSE, period_filter = NULL, spec_name = "All Rings") {
  
  # Apply time filters but keep ALL location types
  analysis_data <- data %>%
    filter(event_time >= -3, event_time <= 5)  # NO location_type filtering!
  
  # Apply period filter if specified
  if (!is.null(period_filter)) {
    if (period_filter$type == "early") {
      analysis_data <- analysis_data %>% filter(treatment_year <= period_filter$cutoff)
    } else if (period_filter$type == "late") {
      analysis_data <- analysis_data %>% filter(treatment_year > period_filter$cutoff)
    }
  }
  
  # Handle urban renewal and highway controls
  if (urban_renewal_treatment == "exclude") {
    analysis_data <- analysis_data %>% filter(!in_urban_renewal)
    if (highway_controls) {
      fixed_effects <- "project_id^year + project_id^location_type + grid_id + has_highway_1km^year"
    } else {
      fixed_effects <- "project_id^year + project_id^location_type + grid_id"
    }
  } else if (urban_renewal_treatment == "control") {
    if (highway_controls) {
      fixed_effects <- "project_id^year + project_id^location_type + grid_id + in_urban_renewal^year + has_highway_1km^year"
    } else {
      fixed_effects <- "project_id^year + project_id^location_type + grid_id + in_urban_renewal^year"
    }
  } else {
    if (highway_controls) {
      fixed_effects <- "project_id^year + project_id^location_type + grid_id + has_highway_1km^year"
    } else {
      fixed_effects <- "project_id^year + project_id^location_type + grid_id"
    }
  }
  
  # Run regression
  cat(paste("\n=== ", spec_name, " (Single Regression) ===\n"))
  cat("Sample size:", nrow(analysis_data), "observations\n")
  
  # Check available event times and location types
  cat("Location types included:", paste(unique(analysis_data$location_type), collapse = ", "), "\n")
  available_event_times <- sort(unique(analysis_data$event_time))
  cat("Available event times:", paste(available_event_times, collapse = ", "), "\n")
  
  if(!(-1 %in% available_event_times)) {
    stop("Reference period -1 not found in data. Available event times: ", paste(available_event_times, collapse = ", "))
  }
  
  cat("Using reference: event_time = -1, location_type = control_800_1000\n")
  
  # Run single regression with all rings
  model <- feols(
    fml = as.formula(paste("llv ~ i(event_time, location_type, ref = -1, ref2 = 'control_800_1000') |", fixed_effects)),
    data = analysis_data,
    weights = ~ wing_weight,
    cluster = ~ project_id
  )
  
  cat("Regression completed\n")
  
  # Show sample sizes by ring
  ring_counts <- analysis_data %>%
    group_by(location_type) %>%
    summarise(n = n(), .groups = "drop")
  cat("\nObservations by ring:\n")
  print(ring_counts)
  
  return(model)
}

#' Create diagnostic visualization of ring assignments
create_ring_visualization <- function(land_value_data, public_housing_data, sample_year = 1961) {
  
  # Get sample for visualization
  land_value_sample <- land_value_data %>% filter(Year == sample_year)
  
  ggplot() +
    geom_sf(data = land_value_sample, aes(color = llv), size = 0.3, alpha = 0.7) +
    geom_sf(data = public_housing_data, 
            color = "black", fill = "white", size = 1.2, shape = 17, stroke = 0.8) +
    scale_color_viridis_c(name = "Log Land Value") +
    labs(
      title = "Chicago Land Values and Public Housing Projects",
      subtitle = paste("Sample year:", sample_year),
      caption = "Triangles = Public housing projects (≥50 units)"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(), 
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "right"
    )
}

#' Extract coefficients from all-rings regression for all ring types
extract_all_rings_coefficients <- function(model) {
  coeffs <- coef(model)
  ses <- se(model)
  
  # Get all ring types (excluding control)
  ring_types <- c("ring_0_100", "ring_100_200", "ring_200_300", "ring_300_400", 
                  "ring_400_500", "ring_500_600", "ring_600_700", "ring_700_800")
  
  # Extract coefficients for each ring type
  all_ring_data <- map_dfr(ring_types, function(ring) {
    ring_idx <- str_detect(names(coeffs), paste0("location_type::", ring))
    if(any(ring_idx)) {
      ring_coefs <- coeffs[ring_idx]
      ring_ses <- ses[ring_idx]
      ring_times <- as.numeric(str_extract(names(ring_coefs), "-?[0-9]+"))
      
      tibble(
        event_time = ring_times,
        estimate = ring_coefs,
        se = ring_ses,
        location_type = ring
      )
    } else {
      tibble()
    }
  })
  
  # Add reference points for all rings at event_time = -1
  plot_data <- all_ring_data %>%
    bind_rows(
      tibble(
        event_time = -1,
        estimate = 0,
        se = 0,
        location_type = ring_types
      )
    ) %>%
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    ) %>%
    arrange(location_type, event_time)
  
  return(plot_data)
}

#' Create event study plot from regression results
create_event_study_plot <- function(model, title_suffix = "", all_rings = FALSE) {
  
  if(all_rings) {
    # Use the all-rings coefficient extraction function
    plot_data <- extract_all_rings_coefficients(model)
  } else {
    # Original extraction logic for separate regressions
    coeffs <- coef(model)
    ses <- se(model)
    
    # Get all event time coefficients (excluding control_800_1000 which is reference)
    event_coeffs <- coeffs[str_detect(names(coeffs), "event_time.*:ring_")]
    event_ses <- ses[str_detect(names(ses), "event_time.*:ring_")]
    
    if(length(event_coeffs) > 0) {
      # Extract event times and location types from coefficient names
      coef_info <- str_match(names(event_coeffs), "event_time::(-?[0-9]+):location_type::(ring_[0-9]+_[0-9]+)")
      event_times <- as.numeric(coef_info[,2])
      location_types <- coef_info[,3]
      
      # Create plot data
      plot_data <- tibble(
        event_time = event_times,
        estimate = event_coeffs,
        se = event_ses,
        location_type = location_types
      ) %>%
        # Add reference points at event_time = -1 for all location types
        bind_rows(
          tibble(
            event_time = -1,
            estimate = 0,
            se = 0,
            location_type = unique(location_types)
          )
        ) %>%
        mutate(
          ci_lower = estimate - 1.96 * se,
          ci_upper = estimate + 1.96 * se
        )
    } else {
      # Fallback for simple two-group comparisons
      plot_data <- tibble(
        event_time = numeric(0),
        estimate = numeric(0),
        se = numeric(0),
        location_type = character(0),
        ci_lower = numeric(0),
        ci_upper = numeric(0)
      )
    }
  }
  
  # Create plot
  ggplot(plot_data, aes(x = event_time, y = estimate, color = location_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
    geom_vline(xintercept = -0.5, linetype = "solid", alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, alpha = 0.7) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("ring_0_100" = "red", "ring_100_200" = "orange", "ring_200_300" = "yellow", 
                 "ring_300_400" = "green", "ring_400_500" = "blue", "ring_500_600" = "purple",
                 "ring_600_700" = "brown", "ring_700_800" = "pink",
                 "treated" = "red", "inner" = "orange", "outer" = "blue"),  # fallback colors
      labels = c("ring_0_100" = "0-100m", "ring_100_200" = "100-200m", "ring_200_300" = "200-300m",
                 "ring_300_400" = "300-400m", "ring_400_500" = "400-500m", "ring_500_600" = "500-600m",
                 "ring_600_700" = "600-700m", "ring_700_800" = "700-800m",
                 "treated" = "Treated", "inner" = "Inner", "outer" = "Outer"),  # fallback labels
      name = "Distance to Project"
    ) +
    labs(
      title = paste("Effect of Public Housing on Chicago Land Values", title_suffix),
      subtitle = "Stacked Difference-in-Differences Event Study",
      x = "Years Since Public Housing Completion", 
      y = "Effect on Log Land Value",
      caption = "Reference: Control ring (800-1000m). 95% confidence intervals.\nClustered standard errors at project level."
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "bottom"
    )
}

# DATA LOADING AND PREPARATION ================================================

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

## 4. Load highway data (shapefiles for direct distance calculation)
if(include_highway_controls) {
  cat("Loading highway shapefiles for direct distance calculation...\n")
  highways_raw <- st_read(here("data", "raw", "highways", "weiwu_2024", "highways_actual.shp"), quiet = TRUE)
  highways <- st_transform(highways_raw, crs = crs_chicago)  # Transform to Chicago projection
  cat("Highway data loaded:", nrow(highways), "highway segments\n")
} else {
  cat("Highway controls disabled\n")
}

# SPATIAL ANALYSIS =============================================================

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
  # Assign to rings - using 100m-wide rings following Blanco & Neri (2025)
  mutate(location_type = case_when(
    distance <= 100 ~ "ring_0_100",
    distance <= 200 ~ "ring_100_200", 
    distance <= 300 ~ "ring_200_300",
    distance <= 400 ~ "ring_300_400",
    distance <= 500 ~ "ring_400_500",
    distance <= 600 ~ "ring_500_600",
    distance <= 700 ~ "ring_600_700",
    distance <= 800 ~ "ring_700_800",
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

cat("Identifying contaminated control points...\n")

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
# If a point is assigned to multiple projects, keep only the closest one
cat("Resolving multiple project assignments per grid point...\n")

# Check for grid points assigned to multiple projects
multiple_assignments_check <- clean_stacked_assignments %>%
  group_by(grid_id) %>%
  summarise(
    n_projects = n_distinct(project_id),
    n_assignments = n(),
    .groups = "drop"
  ) %>%
  filter(n_projects > 1)

cat("Grid points assigned to multiple projects:", nrow(multiple_assignments_check), "\n")

# Keep only the closest project for each grid point
clean_stacked_assignments <- clean_stacked_assignments %>%
  group_by(grid_id) %>%
  filter(distance == min(distance)) %>%  # Keep closest project
  slice_min(project_id, n = 1, with_ties = FALSE) %>%  # Deterministic tie-breaking if distances are equal
  ungroup()

# Verify uniqueness
uniqueness_check <- clean_stacked_assignments %>%
  group_by(grid_id) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

cat("Grid points with multiple assignments after resolution:", nrow(uniqueness_check), "\n")

cat("Sample sizes:\n")
cat("- Original assignments:", nrow(stacked_assignments), "\n")
cat("- After contamination control:", nrow(clean_stacked_assignments), "assignments for", n_distinct(clean_stacked_assignments$grid_id), "unique grid points\n")
cat("- Sample retention:", round(100 * nrow(clean_stacked_assignments) / nrow(stacked_assignments), 1), "%\n")

# Create final panel dataset
stacked_panel <- clean_stacked_assignments %>%
  # Expand to all years
  crossing(year = sort(unique(land_value_data_sf$Year))) %>%
  mutate(unit_id = paste(grid_id, project_id, sep = "_"))

cat("Clean observations by location type:\n")
print(table(stacked_panel$location_type))

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
    # Handle potential duplicates from overlapping UR areas
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

# Calculate highway proximity directly from grid points to highway segments
if(include_highway_controls) {
  
  cat("Calculating distances from grid points to highways...\n")
  
  # Get grid points for distance calculation (using same as for spatial rings)
  land_value_grid_points <- land_value_points %>%
    st_transform(crs = crs_chicago)  # Ensure same projection as highways
  
  # Calculate distance from each grid point to nearest highway
  distances_to_highways <- st_distance(land_value_grid_points, highways)
  min_distances_m <- apply(distances_to_highways, 1, min)
  min_distances_km <- as.numeric(min_distances_m) / 1000
  
  # Create highway indicators
  highway_indicators <- land_value_grid_points %>%
    st_drop_geometry() %>%
    mutate(
      distance_to_highway_km = min_distances_km,
      has_highway_1km = distance_to_highway_km <= 1,
      has_highway_2km = distance_to_highway_km <= 2
    ) %>%
    select(grid_id, has_highway_1km, has_highway_2km, distance_to_highway_km)
  
  cat("Grid points with highway data:", nrow(highway_indicators), "\n")
  cat("Grid points within 1km of highway:", sum(highway_indicators$has_highway_1km, na.rm = TRUE), "\n")
  cat("Grid points within 2km of highway:", sum(highway_indicators$has_highway_2km, na.rm = TRUE), "\n")
  
} else {
  # Create dummy highway indicators if not using highway controls
  highway_indicators <- land_value_points %>%
    st_drop_geometry() %>%
    mutate(
      has_highway_1km = FALSE,
      has_highway_2km = FALSE,
      distance_to_highway_km = 999
    ) %>%
    select(grid_id, has_highway_1km, has_highway_2km, distance_to_highway_km)
  
  cat("Highway controls disabled - using dummy indicators\n")
}

# FINAL DATASET CREATION =======================================================

cat("\n=== CREATING FINAL ANALYSIS DATASET ===\n")

# Merge everything together
land_value_stacked <- stacked_panel %>%
  left_join(
    land_value_data_sf %>% 
      st_drop_geometry() %>%
      select(grid_id, Year, llv),  # Only select needed columns to avoid conflicts
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
      location_type == "ring_0_100" ~ 0,
      location_type == "ring_100_200" ~ 1,
      location_type == "ring_200_300" ~ 2,
      location_type == "ring_300_400" ~ 3,
      location_type == "ring_400_500" ~ 4,
      location_type == "ring_500_600" ~ 5,
      location_type == "ring_600_700" ~ 6,
      location_type == "ring_700_800" ~ 7,
      location_type == "control_800_1000" ~ 8
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

# Create event study dataset with period-based event times
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
  # Create relative period (not calendar time difference)
  mutate(
    event_time = mapply(get_relative_period, year, treatment_year),
    # Create cohort identifier
    cohort = paste0("cohort_", treatment_year)
  )

cat("Event study data created:", nrow(event_study_data), "observations\n")
cat("Event time range:", min(event_study_data$event_time), "to", max(event_study_data$event_time), "\n")

# Show the new event time distribution
cat("\nEvent time (relative periods) distribution:\n")
print(table(event_study_data$event_time))

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

# Save event study data
write_csv(event_study_data, here(data_dir, "chicago_land_value_event_study_data.csv"))
cat("Event study data saved\n")

# VISUALIZATION ================================================================

cat("\n=== CREATING VISUALIZATIONS ===\n")

# Create overview map
overview_map <- create_ring_visualization(land_value_data_sf, public_housing_data, sample_year = 1961)
ggsave(here(figures_dir, "chicago_overview_map.pdf"), overview_map, width = 10, height = 8)

# Create wing weight distribution plot
wing_weight_plot <- event_study_data %>%
  distinct(project_id, location_type, wing_weight) %>%
  ggplot(aes(x = wing_weight, fill = location_type)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~ location_type, scales = "free_y", ncol = 3) +
  scale_fill_manual(
    values = c("ring_0_100" = "red", "ring_100_200" = "orange", "ring_200_300" = "yellow", 
               "ring_300_400" = "green", "ring_400_500" = "blue", "ring_500_600" = "purple",
               "ring_600_700" = "brown", "ring_700_800" = "pink", "control_800_1000" = "grey")
  ) +
  labs(
    title = "Distribution of Wing (2024) Weights by Location Type",
    x = "Wing Weight (N_re / N_r)",
    y = "Number of Project-Location Combinations"
  ) +
  theme_minimal() +
  theme(legend.position = "none", 
        strip.text = element_text(size = 8))

ggsave(here(figures_dir, "wing_weights_distribution.pdf"), wing_weight_plot, width = 10, height = 6)

cat("Visualizations saved\n")

# REGRESSION ANALYSIS ==========================================================

cat("\n=== RUNNING SPATIAL DID REGRESSIONS ===\n")

## Main Specifications (Closest Ring vs Control)
# Compare closest treated ring (0-100m) to control ring (800-1000m)
specs <- list(
  list(name = "Baseline (No Controls)", 
       location_filter = c("ring_0_100", "control_800_1000"), 
       urban_renewal_treatment = "baseline", highway_controls = FALSE),
  
  list(name = "Highway Controls Only", 
       location_filter = c("ring_0_100", "control_800_1000"), 
       urban_renewal_treatment = "baseline", highway_controls = TRUE),
  
  list(name = "UR Controls Only", 
       location_filter = c("ring_0_100", "control_800_1000"), 
       urban_renewal_treatment = "control", highway_controls = FALSE),
  
  list(name = "UR + Highway Controls", 
       location_filter = c("ring_0_100", "control_800_1000"), 
       urban_renewal_treatment = "control", highway_controls = TRUE),
  
  list(name = "Exclude UR Areas", 
       location_filter = c("ring_0_100", "control_800_1000"), 
       urban_renewal_treatment = "exclude", highway_controls = FALSE),
  
  list(name = "Exclude UR + Highway Controls", 
       location_filter = c("ring_0_100", "control_800_1000"), 
       urban_renewal_treatment = "exclude", highway_controls = TRUE)
)

# Run main specifications
main_results <- map2(specs, 1:length(specs), ~{
  spec_name <- paste("Specification", .y, ":", .x$name)
  run_spatial_did(
    data = event_study_data,
    location_filter = .x$location_filter,
    urban_renewal_treatment = .x$urban_renewal_treatment,
    highway_controls = .x$highway_controls,
    spec_name = spec_name
  )
})

names(main_results) <- c("baseline", "highway_only", "ur_only", "ur_highway", "exclude_ur", "exclude_ur_highway")

## Mid-Distance Ring Specifications (400-500m vs Control)
# Compare mid-distance ring to control ring for comparison
mid_specs <- list(
  list(name = "Mid Ring Baseline", 
       location_filter = c("ring_400_500", "control_800_1000"), 
       urban_renewal_treatment = "baseline"),
  
  list(name = "Mid Ring Exclude UR", 
       location_filter = c("ring_400_500", "control_800_1000"), 
       urban_renewal_treatment = "exclude"),
  
  list(name = "Mid Ring Control UR", 
       location_filter = c("ring_400_500", "control_800_1000"), 
       urban_renewal_treatment = "control")
)

mid_results <- map2(mid_specs, 1:length(mid_specs), ~{
  spec_name <- paste("Mid Ring Specification", .y, ":", .x$name)
  run_spatial_did(
    data = event_study_data,
    location_filter = .x$location_filter, 
    urban_renewal_treatment = .x$urban_renewal_treatment,
    spec_name = spec_name
  )
})

names(mid_results) <- c("mid_baseline", "mid_exclude_ur", "mid_control_ur")

## All Rings Specifications (Blanco & Neri Approach - Single Regression)
cat("\n=== BLANCO & NERI APPROACH: ALL RINGS IN SINGLE REGRESSION ===\n")

all_rings_specs <- list(
  list(name = "All Rings Baseline", 
       urban_renewal_treatment = "baseline", highway_controls = FALSE),
  
  list(name = "All Rings Control UR + Highway", 
       urban_renewal_treatment = "control", highway_controls = TRUE),
  
  list(name = "All Rings Exclude UR", 
       urban_renewal_treatment = "exclude", highway_controls = FALSE)
)

all_rings_results <- map2(all_rings_specs, 1:length(all_rings_specs), ~{
  spec_name <- paste("All Rings Specification", .y, ":", .x$name)
  run_spatial_did_all_rings(
    data = event_study_data,
    urban_renewal_treatment = .x$urban_renewal_treatment,
    highway_controls = .x$highway_controls,
    spec_name = spec_name
  )
})

names(all_rings_results) <- c("all_rings_baseline", "all_rings_exclude_ur", "all_rings_control_ur")

## Robustness Check: Compare 100m rings vs 400m aggregated rings
cat("\n=== ROBUSTNESS CHECK: 100m vs 400m RING COMPARISON ===\n")

# Create aggregated ring assignments for comparison
event_study_data_400m <- event_study_data %>%
  mutate(location_type_400m = case_when(
    location_type %in% c("ring_0_100", "ring_100_200", "ring_200_300", "ring_300_400") ~ "treated_0_400",
    location_type %in% c("ring_400_500", "ring_500_600", "ring_600_700", "ring_700_800") ~ "inner_400_800", 
    location_type == "control_800_1000" ~ "control_800_1000",
    TRUE ~ location_type
  ))

# Recalculate Wing weights for 400m specification
event_study_data_400m <- event_study_data_400m %>%
  group_by(treatment_year, project_id, location_type_400m) %>%
  mutate(N_re_400m = n()) %>%
  ungroup() %>%
  group_by(treatment_year, location_type_400m) %>%
  mutate(N_r_400m = n()) %>%
  ungroup() %>%
  mutate(wing_weight_400m = N_re_400m / N_r_400m)

# Run 400m specification
robustness_400m <- run_spatial_did_all_rings(
  data = event_study_data_400m %>% 
    select(-location_type, -wing_weight, -N_re, -N_r) %>%
    rename(location_type = location_type_400m, wing_weight = wing_weight_400m),
  urban_renewal_treatment = "baseline",
  spec_name = "400m Aggregated Rings (Robustness)"
)

## Heterogeneity by Treatment Period (Separate Regressions)
period_results <- list(
  early = run_spatial_did(
    data = event_study_data,
    location_filter = c("ring_0_100", "control_800_1000"),
    urban_renewal_treatment = "control", 
    period_filter = list(type = "early", cutoff = 1961),
    spec_name = "Early Treatment Periods (≤1961)"
  ),
  
  late = run_spatial_did(
    data = event_study_data,
    location_filter = c("ring_0_100", "control_800_1000"),
    urban_renewal_treatment = "control",
    period_filter = list(type = "late", cutoff = 1961), 
    spec_name = "Late Treatment Periods (>1961)"
  )
)

## All Rings Period Results 
all_rings_period_results <- list(
  early = run_spatial_did_all_rings(
    data = event_study_data,
    urban_renewal_treatment = "control",
    highway_controls = TRUE,
    period_filter = list(type = "early", cutoff = 1961),
    spec_name = "All Rings Early Periods (≤1961)"
  ),
  
  late = run_spatial_did_all_rings(
    data = event_study_data,
    urban_renewal_treatment = "control",
    highway_controls = TRUE,
    period_filter = list(type = "late", cutoff = 1961),
    spec_name = "All Rings Late Periods (>1961)"
  )
)

## HETEROGENEITY ANALYSIS ======================================================

cat("\n=== HETEROGENEITY ANALYSIS ===\n")

# Create heterogeneity indicators
event_study_data_hetero <- event_study_data %>%
  mutate(
    # Project size categories
    project_size_category = case_when(
      total_units <= 100 ~ "small",
      total_units <= 500 ~ "medium", 
      total_units > 500 ~ "large"
    ),
    # Treatment period categories
    period_category = case_when(
      treatment_year <= 1949 ~ "early",
      treatment_year <= 1971 ~ "middle",
      treatment_year > 1971 ~ "late"
    )
  )

# Project size heterogeneity
size_hetero_results <- list()
for(size_cat in c("small", "medium", "large")) {
  size_data <- event_study_data_hetero %>% filter(project_size_category == size_cat)
  
  if(nrow(size_data) > 100) {  # Only run if sufficient data
    size_hetero_results[[size_cat]] <- run_spatial_did(
      data = size_data,
      location_filter = c("ring_0_100", "control_800_1000"),
      urban_renewal_treatment = "control",
      highway_controls = TRUE,
      spec_name = paste("Project Size:", size_cat)
    )
  }
}

# Treatment period heterogeneity  
period_hetero_results <- list()
for(period_cat in c("early", "middle", "late")) {
  period_data <- event_study_data_hetero %>% filter(period_category == period_cat)
  
  if(nrow(period_data) > 100) {  # Only run if sufficient data
    period_hetero_results[[period_cat]] <- run_spatial_did(
      data = period_data,
      location_filter = c("ring_0_100", "control_800_1000"),
      urban_renewal_treatment = "control", 
      highway_controls = TRUE,
      spec_name = paste("Treatment Period:", period_cat)
    )
  }
}

# RESULTS OUTPUT ===============================================================

cat("\n=== SAVING RESULTS ===\n")

# Create event study plots
main_plot <- create_event_study_plot(main_results$closest_baseline)
ggsave(here(figures_dir, "chicago_land_value_event_study_separate.pdf"), main_plot, width = 10, height = 6)

# Create all-rings plot
all_rings_plot <- create_event_study_plot(all_rings_results$all_rings_baseline, 
                                         title_suffix = " (Blanco & Neri Single Regression)", 
                                         all_rings = TRUE)
ggsave(here(figures_dir, "chicago_land_value_event_study_all_rings.pdf"), all_rings_plot, width = 10, height = 6)

# Create robustness comparison plot
robustness_plot <- create_event_study_plot(robustness_400m, 
                                          title_suffix = " (400m Aggregated Rings)", 
                                          all_rings = TRUE)
ggsave(here(figures_dir, "chicago_land_value_event_study_400m_robustness.pdf"), robustness_plot, width = 10, height = 6)

# Save regression tables with updated specifications
etable(main_results$baseline, main_results$highway_only, main_results$ur_only, main_results$ur_highway,
       tex = TRUE,
       file = here(results_dir, "chicago_land_value_main_results.tex"),
       title = "Effect of Public Housing on Chicago Land Values - Main Specifications",
       label = "tab:chicago_main")

# Save robustness specifications (excluding UR areas)
etable(main_results$exclude_ur, main_results$exclude_ur_highway,
       tex = TRUE,
       file = here(results_dir, "chicago_land_value_robustness_exclude_ur.tex"),
       title = "Effect of Public Housing on Chicago Land Values - Excluding Urban Renewal Areas",
       label = "tab:chicago_exclude_ur")

etable(mid_results$mid_baseline, mid_results$mid_exclude_ur, mid_results$mid_control_ur,
       tex = TRUE,
       file = here(results_dir, "chicago_land_value_mid_results.tex"), 
       title = "Effect of Public Housing on Chicago Land Values - Mid-Distance Ring Specifications",
       label = "tab:chicago_mid")

etable(period_results$early, period_results$late,
       tex = TRUE,
       file = here(results_dir, "chicago_land_value_period_results.tex"),
       title = "Effect of Public Housing on Chicago Land Values - By Treatment Period", 
       label = "tab:chicago_period")

# Save all-rings regression tables
etable(all_rings_results$all_rings_baseline, all_rings_results$all_rings_exclude_ur, all_rings_results$all_rings_control_ur,
       tex = TRUE,
       file = here(results_dir, "chicago_land_value_all_rings_results.tex"),
       title = "Effect of Public Housing on Chicago Land Values - All Rings Single Regression (Blanco & Neri Approach)",
       label = "tab:chicago_all_rings")

etable(all_rings_period_results$early, all_rings_period_results$late,
       tex = TRUE,
       file = here(results_dir, "chicago_land_value_all_rings_period_results.tex"),
       title = "Effect of Public Housing on Chicago Land Values - All Rings by Treatment Period",
       label = "tab:chicago_all_rings_period")

# Save robustness comparison table
etable(all_rings_results$all_rings_baseline, robustness_400m,
       tex = TRUE,
       file = here(results_dir, "chicago_land_value_robustness_100m_vs_400m.tex"),
       title = "Robustness Check: 100m vs 400m Ring Specifications",
       label = "tab:chicago_robustness")

# Save heterogeneity results
if(length(size_hetero_results) > 0) {
  etable(size_hetero_results,
         tex = TRUE,
         file = here(results_dir, "chicago_land_value_heterogeneity_size.tex"),
         title = "Heterogeneity by Project Size",
         label = "tab:chicago_hetero_size")
}

if(length(period_hetero_results) > 0) {
  etable(period_hetero_results,
         tex = TRUE,
         file = here(results_dir, "chicago_land_value_heterogeneity_period.tex"),
         title = "Heterogeneity by Treatment Period",
         label = "tab:chicago_hetero_period")
}

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

summary_stats_preperiod <- 
event_study_data %>%
  filter(event_time == -1) %>%
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
write_csv(summary_stats_preperiod, here(data_dir, "chicago_summary_statistics_preperiod.csv"))
cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to:\n")
cat("- Figures:", figures_dir, "\n")
cat("- Regression tables:", results_dir, "\n")
cat("- Data outputs:", data_dir, "\n")

cat("\nKey findings:\n")
cat("Main sample size:", nrow(event_study_data %>% filter(event_time >= -3, event_time <= 5)), "observations\n")
cat("Number of public housing projects:", nrow(public_housing_data), "\n")
cat("Treatment periods:", paste(sort(unique(event_study_data$treatment_year)), collapse = ", "), "\n")


# Check multiply treated points
contamination_check <- event_study_data %>%
  group_by(grid_id, year) %>%
  summarise(
    n_projects = n_distinct(project_id),
    location_types = paste(unique(location_type), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_projects > 1)

cat("Grid points exposed to multiple projects:", nrow(contamination_check), "\n")


# Check control ring contamination  
control_contamination <- event_study_data %>%
  filter(location_type == "control_800_1000") %>%
  group_by(grid_id) %>%
  summarise(
    total_exposures = n_distinct(project_id),
    .groups = "drop"
  ) %>%
  filter(total_exposures > 1)

# Count number of projects each control point is exposed to
projects_per_control_point <-
  event_study_data %>% 
  group_by(grid_id, location_type) %>%
  summarise(
    n_projects = n_distinct(project_id),
    .groups = "drop"
  ) %>% 
  filter(location_type == "control_800_1000")


cat("Control points exposed to multiple projects:", nrow(control_contamination), "\n")
