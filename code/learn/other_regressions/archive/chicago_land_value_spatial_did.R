#####
# Chicago Land Value Spatial Difference-in-Differences Analysis
# Using Ahlfeldt-McMillen land value grid data and public housing projects
#####

# Preliminaries -----
library(tidyverse)
library(sf)
library(here)
library(haven)
library(fixest)
library(ggfixest)
library(units)
library(viridis)
library(RColorBrewer)

set.seed(123)

# Output directories -----
figures_dir <- here("output", "figures", "chicago_land_values")
results_dir <- here("output", "regression_results", "chicago_land_values")
data_dir <- here("output", "data", "chicago_land_values")

# Ensure directories exist
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# Define projected CRS for Chicago (NAD83 Illinois East Zone)
# Using geographic CRS (4326) can cause distance calculation errors
# Chicago-specific projected CRS (26971) ensures accurate meter-based distances
crs_chicago <- 26971

# Load and prepare data -----


## 1. Load Ahlfeldt-McMillen land value data
cat("Loading Ahlfeldt-McMillen land value data...\n")
land_value_data <- read_dta(here("data", "raw", "ahlfeldt_mcmillen", "GRID_LONG.dta"))

# Convert to sf object
# Coordinates are in decimal degrees (WGS84), transform to Chicago projected CRS
land_value_data_sf <- land_value_data %>%
  filter(!is.na(longitude), !is.na(latitude), !is.na(llv)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%  # Data is in WGS84
  st_transform(crs = crs_chicago) %>%  # Transform to Chicago projected CRS for accurate distances
  # Create unique grid point ID
  mutate(grid_point_id = row_number())



cat("Land value data loaded:", nrow(land_value_data_sf), "observations\n")
cat("Years available:", paste(sort(unique(land_value_data_sf$Year)), collapse = ", "), "\n")

## 2. Load Chicago public housing data
cat("Loading Chicago public housing data...\n")
public_housing_data <- read_csv(here("data", "derived", "public_housing", "working", "combined_cdd_and_digitized_projects.csv")) %>%
  filter(locality == "CHICAGO") %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(year_completed)) %>%
  # Create treatment year variable (BASED ON land_value_data years)
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
  filter(!is.na(treatment_year)) %>%
  # Keep only those above 50 units
  filter(total_units >= 50) %>%
  # Convert to sf object (coordinates likely in WGS84)
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = crs_chicago)  # Transform to Chicago projected CRS


cat("Public housing projects loaded:", nrow(public_housing_data), "projects\n")
cat("Treatment years:", paste(sort(unique(public_housing_data$treatment_year)), collapse = ", "), "\n")

## 3. Load Chicago urban renewal data (spatial boundaries only)
cat("Loading Chicago urban renewal spatial boundaries...\n")

# Load urban renewal spatial boundaries
ur_spatial <- st_read(here("data", "raw", "Renewing_Inequality_Data-master", "Data", "shapefile", "ur_projects.shp")) %>%
  filter(str_detect(tolower(city), "chicago")) %>%
  st_transform(crs = crs_chicago)  # Transform to Chicago projected CRS

cat("Urban renewal projects loaded:", nrow(ur_spatial), "projects\n")

## 4. Load Chicago community areas
cat("Loading Chicago community areas...\n")

# Load community areas from GeoJSON
community_areas <- st_read(here("data", "raw", "chicago_community_areas", "Boundaries - Community Areas_20250728.geojson")) %>%
  st_transform(crs = crs_chicago) %>%  # Transform to Chicago projected CRS
  # Clean community area names
  mutate(community_area = as.character(community))

cat("Community areas loaded:", nrow(community_areas), "areas\n")

# Filter UR projects to only those that actually overlap with land value points
if(nrow(ur_spatial) > 0) {
  # Get land value points for filtering (we'll create this properly later)
  land_value_points_temp <- land_value_data_sf %>% 
    filter(Year == 1961) %>%
    select(grid_id, geometry)
  
  # Test spatial join to find which UR projects actually contain land value points
  ur_join_test <- st_join(land_value_points_temp, ur_spatial, join = st_within)
  overlapping_ur_ids <- unique(ur_join_test$project_id[!is.na(ur_join_test$project_id)])
  
  if(length(overlapping_ur_ids) > 0) {
    ur_spatial <- ur_spatial %>% filter(project_id %in% overlapping_ur_ids)
    cat("Filtered to", nrow(ur_spatial), "UR projects that actually overlap with land values\n")
  } else {
    cat("No UR projects overlap with land value points\n")
  }
} else {
  cat("No UR projects found for Chicago\n")
}

# Data validation and exploration -----
cat("\n=== Data Overview ===\n")
cat("Land value years:", min(land_value_data_sf$Year), "-", max(land_value_data_sf$Year), "\n")
cat("Public housing completion years:", min(public_housing_data$year_completed), "-", max(public_housing_data$year_completed), "\n")
cat("Treatment cohorts and counts:\n")
print(table(public_housing_data$treatment_year))



## Create initial visualization -----
cat("Creating initial data visualization...\n")
sample_year <- 1961
land_value_sample <- land_value_data_sf %>% filter(Year == sample_year)

initial_map <- ggplot() +
  geom_sf(data = land_value_sample, aes(color = llv), size = 0.3, alpha = 0.7) +
  # Add urban renewal boundaries if they exist
  {if(nrow(ur_spatial) > 0) geom_sf(data = ur_spatial, fill = "yellow", alpha = 0.3, color = "orange", size = 0.8)} +
  geom_sf(data = public_housing_data, color = "red", size = 1.5, shape = 17) +
  scale_color_viridis_c(name = "Log Land Value") +
  labs(title = paste("Chicago Land Values, Public Housing, and Urban Renewal,", sample_year),
       subtitle = paste("Land value grid points:", nrow(land_value_sample), 
                       "| Public housing projects:", nrow(public_housing_data),
                       "| Urban renewal projects:", nrow(ur_spatial))) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave(here(figures_dir, "chicago_overview_map.pdf"), initial_map, width = 10, height = 8)
cat("Initial map saved to:", here(figures_dir, "chicago_overview_map.pdf"), "\n")

# Define spatial rings -----
cat("\n=== Creating Spatial Rings ===\n")

# Ring definitions (in meters, converted from kilometers)
# Updated to 200m bands for better power and cleaner identification
inner_ring_distance <- 400     # 0.4 km
outer_ring_distance <- 800     # 0.8 km  
max_ring_distance <- 1600      # 1.6 km

# Alternative 200m band definitions for robustness
alt_ring_200 <- 200    # 0.2 km
alt_ring_400 <- 400    # 0.4 km
alt_ring_600 <- 600    # 0.6 km
alt_ring_800 <- 800    # 0.8 km
alt_ring_1000 <- 1000  # 1.0 km


# Create stacked spatial DiD dataset following census tract methodology
sf::sf_use_s2(FALSE)

cat("\\n=== Creating Stacked Spatial DiD Dataset ===\\n")

# Get land value points for assignment (single year since locations are constant)
land_value_points <- land_value_data_sf %>% 
  filter(Year == 1961) %>%
  select(grid_id, geometry)

cat("Land value points:", nrow(land_value_points), "\\n")
cat("Public housing projects:", nrow(public_housing_data), "\\n")

# Calculate distance matrix between all points and all projects
cat("Calculating distances between points and projects...\\n")
distances_matrix <- st_distance(land_value_points, public_housing_data)

# Convert to numeric matrix in meters
distances_df <- as.data.frame(as.matrix(distances_matrix))
names(distances_df) <- paste0("project_", 1:nrow(public_housing_data))

# Add grid_point_id
distances_df$grid_id <- land_value_points$grid_id

# Reshape to long format for stacked structure
distances_long <- distances_df %>%
  pivot_longer(
    cols = starts_with("project_"),
    names_to = "project_id_temp",
    values_to = "distance"
  ) %>%
  mutate(
    project_id = as.integer(str_extract(project_id_temp, "[0-9]+"))
  ) %>%
  select(-project_id_temp)

# Filter to only keep points within maximum distance of each project
stacked_assignments <- distances_long %>%
  mutate(distance = as.numeric(distance)) %>%  # Convert units to numeric (meters)
  filter(distance <= max_ring_distance) %>%
  mutate(
    # Assign location type based on distance (original 100m bands)
    location_type = case_when(
      distance <= 100 ~ "ring_0_100",
      distance <= 200 ~ "ring_100_200", 
      distance <= 300 ~ "ring_200_300",
      distance <= 400 ~ "ring_300_400",
      distance <= 500 ~ "ring_400_500",
      distance <= 600 ~ "ring_500_600",
      distance <= 700 ~ "ring_600_700",
      distance <= 800 ~ "ring_700_800",
      distance <= 1000 ~ "control_800_1000",
      TRUE ~ NA_character_
    ),
    # Alternative 200m bands for robustness
    location_type_200m = case_when(
      distance <= alt_ring_200 ~ "ring_0_200",
      distance <= alt_ring_400 ~ "ring_200_400",
      distance <= alt_ring_600 ~ "ring_400_600", 
      distance <= alt_ring_800 ~ "ring_600_800",
      distance <= alt_ring_1000 ~ "control_800_1000",
      TRUE ~ NA_character_
    )
  )

# Add treatment year from public housing data
project_info <- public_housing_data %>%
  st_drop_geometry() %>%
  mutate(project_id = row_number()) %>%
  select(project_id, treatment_year, project_name, locality, total_units)

stacked_assignments <- stacked_assignments %>%
  left_join(project_info, by = "project_id")

cat("\\nStacked assignment summary:\\n")
cat("Total point-project pairs within max distance:", nrow(stacked_assignments), "\\n")
cat("\\nBy location type:\\n")
print(table(stacked_assignments$location_type))
cat("\\nBy treatment year:\\n")
print(table(stacked_assignments$treatment_year))

# Expand to all years (similar to census tract approach)
years <- sort(unique(land_value_data_sf$Year))

# Create full panel
stacked_panel <- expand_grid(
  stacked_assignments,
  Year = years
) %>%
  rename(year = Year)

# Create binary urban renewal indicator for land value points
cat("Creating binary urban renewal indicator...\n")

# Get unique land value points (single year since locations are constant)
land_value_points_ur <- land_value_data_sf %>% 
  filter(Year == 1961) %>%
  select(grid_id, geometry)

# Create simple binary indicator via spatial join
if(nrow(ur_spatial) > 0) {
  ur_indicators <- st_join(land_value_points_ur, 
                          ur_spatial %>% select(project_id),
                          join = st_within) %>%
    st_drop_geometry() %>%
    mutate(in_urban_renewal = !is.na(project_id)) %>%
    select(grid_id, in_urban_renewal)
} else {
  # If no urban renewal data, create empty indicators
  ur_indicators <- land_value_points_ur %>%
    st_drop_geometry() %>%
    mutate(in_urban_renewal = FALSE) %>%
    select(grid_id, in_urban_renewal)
}

cat("Urban renewal binary indicator created\n")
cat("Grid points in urban renewal areas:", sum(ur_indicators$in_urban_renewal), "\n")

# Create community area indicators
cat("Creating community area assignments...\n")
community_area_indicators <- st_join(land_value_data_sf %>% filter(Year == 1961),
                                    community_areas %>% select(community_area),
                                    join = st_within) %>%
  st_drop_geometry() %>%
  select(grid_id, community_area)

cat("Community area assignments created\n")
cat("Grid points assigned to community areas:", sum(!is.na(community_area_indicators$community_area)), "\n")
cat("Grid points missing community areas:", sum(is.na(community_area_indicators$community_area)), "\n")

# Create diagnostic maps to understand missing community area assignments
cat("Creating diagnostic maps for community area assignments...\n")

# Get land value points with community area assignments
land_value_with_ca <- land_value_data_sf %>% 
  filter(Year == 1961) %>%
  left_join(community_area_indicators, by = "grid_id") %>%
  mutate(has_community_area = !is.na(community_area))

# Map 1: Land value points colored by whether they have community area assignment
ca_coverage_map <- ggplot() +
  # Community area boundaries
  geom_sf(data = community_areas, fill = "lightgray", color = "white", alpha = 0.3) +
  # Land value points colored by coverage
  geom_sf(data = land_value_with_ca, 
          aes(color = has_community_area), 
          size = 0.2, alpha = 0.7) +
  # Public housing projects on top
  geom_sf(data = public_housing_data, 
          color = "black", fill = "yellow", size = 2, shape = 17, stroke = 1) +
  scale_color_manual(
    values = c("TRUE" = "blue", "FALSE" = "red"),
    labels = c("TRUE" = "Has Community Area", "FALSE" = "Missing Community Area"),
    name = "Coverage Status"
  ) +
  labs(
    title = "Land Value Grid Points and Community Area Coverage",
    subtitle = paste("Blue = assigned (", sum(!is.na(community_area_indicators$community_area)), 
                    "), Red = missing (", sum(is.na(community_area_indicators$community_area)), ")"),
    caption = "Gray boundaries = Chicago community areas. Yellow triangles = Public housing projects (≥50 units)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )

ggsave(here(figures_dir, "community_area_coverage_diagnostic.pdf"), 
       ca_coverage_map, width = 12, height = 10)

# Map 2: Just the missing points to see where they are
missing_points <- land_value_with_ca %>% filter(is.na(community_area))

missing_points_map <- ggplot() +
  # Community area boundaries
  geom_sf(data = community_areas, fill = "lightblue", color = "white", alpha = 0.5) +
  # Only missing points
  geom_sf(data = missing_points, color = "red", size = 0.5, alpha = 0.8) +
  # Public housing projects on top
  geom_sf(data = public_housing_data, 
          color = "black", fill = "yellow", size = 2, shape = 17, stroke = 1) +
  labs(
    title = "Land Value Points Missing Community Area Assignment",
    subtitle = paste("Red points (", nrow(missing_points), ") are outside community area boundaries"),
    caption = "Light blue = Chicago community areas. Yellow triangles = Public housing projects (≥50 units)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

ggsave(here(figures_dir, "missing_community_areas_diagnostic.pdf"), 
       missing_points_map, width = 12, height = 10)

# Map 3: Community areas with names
community_area_labels <- community_areas %>%
  mutate(centroid = st_centroid(geometry)) %>%
  st_drop_geometry() %>%
  bind_cols(st_coordinates(st_centroid(community_areas$geometry))) %>%
  rename(lon = X, lat = Y)

community_areas_map <- ggplot() +
  geom_sf(data = community_areas, aes(fill = community_area), color = "white", alpha = 0.7) +
  geom_text(data = community_area_labels, 
            aes(x = lon, y = lat, label = community_area), 
            size = 2, check_overlap = TRUE) +
  # Public housing projects on top
  geom_sf(data = public_housing_data, 
          color = "black", fill = "white", size = 2, shape = 17, stroke = 1) +
  scale_fill_viridis_d(guide = "none") +  # Remove legend since we have labels
  labs(
    title = "Chicago Community Areas",
    subtitle = "Community area boundaries with labels and public housing projects"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

ggsave(here(figures_dir, "chicago_community_areas_labeled.pdf"), 
       community_areas_map, width = 12, height = 10)

cat("Diagnostic maps saved:\n")
cat("- community_area_coverage_diagnostic.pdf: Shows which points have/missing community areas\n")
cat("- missing_community_areas_diagnostic.pdf: Shows only the missing points\n") 
cat("- chicago_community_areas_labeled.pdf: Shows community area boundaries with names\n")

# Merge with land value data
land_value_stacked <- stacked_panel %>%
  left_join(
    land_value_data_sf %>% st_drop_geometry(),
    by = c("grid_id", "year" = "Year")
  ) %>% 
  # Add urban renewal indicators
  left_join(ur_indicators, by = "grid_id") %>%
  # Add community area indicators
  left_join(community_area_indicators, by = "grid_id") %>%
  filter(year >= 1913)

# Create event time and other DiD variables
land_value_stacked <- land_value_stacked %>%
  mutate(
    # Event time (years since treatment)
    event_time = year - treatment_year,
    # Cohort identifier
    cohort = factor(treatment_year),
    # Treatment indicator
    treated = (location_type == "treated" & year >= treatment_year),
    # Create unique identifiers
    unit_id = paste(grid_point_id, project_id, sep = "_"),
    # Ring factor for analysis
    ring = factor(case_when(
      location_type == "treated" ~ 0,
      location_type == "inner" ~ 1,
      location_type == "outer" ~ 2
    ))
  )

# Final dataset uses stacked structure with outer rings as controls
land_value_with_rings <- land_value_stacked

cat("\\nFinal dataset summary:\\n")
cat("Total observations:", nrow(land_value_with_rings), "\\n")
cat("Unique grid points:", n_distinct(land_value_with_rings$grid_point_id), "\\n")
cat("Unique projects:", n_distinct(land_value_with_rings$project_id[!is.na(land_value_with_rings$project_id)]), "\\n")

# Create visualization of rings, projects, and land values -----
cat("\\n=== Creating Ring Visualization ===\\n")

# Get land value points with ring assignments for a sample year
sample_year <- 1961
ring_vis_data <- land_value_with_rings %>% 
  filter(year == sample_year, !is.na(llv)) %>%
  # Get unique grid points with their ring assignments (may have multiple due to stacking)
  group_by(grid_id) %>%
  # For visualization, keep the "highest priority" ring assignment (treated > inner > outer)
  arrange(match(location_type, c("treated", "inner", "outer"))) %>%
  slice(1) %>%
  ungroup()

# Convert back to sf for mapping
ring_vis_sf <- land_value_data_sf %>%
  filter(Year == sample_year) %>%
  inner_join(ring_vis_data %>% select(grid_id, location_type, distance, project_id), 
             by = "grid_id")


# Create a second map showing land values with rings
land_value_map <- ggplot() +
  # Add ring boundaries as subtle background first
  geom_sf(data = ring_vis_sf %>% filter(location_type == "outer"), 
          color = "#E8F4FD", fill = "#E8F4FD", size = 0.1, alpha = 0.3) +
  geom_sf(data = ring_vis_sf %>% filter(location_type == "inner"), 
          color = "#FFE5B4", fill = "#FFE5B4", size = 0.1, alpha = 0.4) +
  geom_sf(data = ring_vis_sf %>% filter(location_type == "treated"), 
          color = "#FFE4E6", fill = "#FFE4E6", size = 0.1, alpha = 0.5) +
  geom_sf(data = land_value_sample, aes(color = llv), size = 0.3, alpha = 0.7) +
  
  # Public housing projects on top (smaller)
  geom_sf(data = public_housing_data, 
          color = "black", fill = "white", size = 1.2, shape = 17, stroke = 0.8) +
  
  # Better color scale for land values
  scale_color_viridis_c(name = "Log Land Value") +
  labs(
    title = "Chicago Land Values and Public Housing Projects",
    subtitle = paste("Log land values with spatial ring boundaries,", sample_year),
    caption = "Triangles = Public housing projects (≥50 units). Background shading shows treatment rings."
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.5, "cm")
  )

ggsave(here(figures_dir, "chicago_land_values_with_rings_map.pdf"), 
       land_value_map, width = 12, height = 10, dpi = 300)

cat("Land value map saved to:", here(figures_dir, "chicago_land_values_with_rings_map.pdf"), "\\n")




# Create clean sub-experiments (no contamination) -----
cat("\n=== Creating Clean Sub-Experiments ===\n")

# Step 1: Track complete exposure history for each grid point
exposure_history <- land_value_with_rings %>%
  group_by(grid_id) %>%
  summarise(
    # First year in each ring type
    first_treated_year = ifelse(any(location_type == "treated"), 
                               min(treatment_year[location_type == "treated"], na.rm = TRUE), 
                               NA_real_),
    first_inner_year = ifelse(any(location_type == "inner"), 
                             min(treatment_year[location_type == "inner"], na.rm = TRUE), 
                             NA_real_),
    first_outer_year = ifelse(any(location_type == "outer"), 
                             min(treatment_year[location_type == "outer"], na.rm = TRUE), 
                             NA_real_),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    # Overall first exposure
    first_exposure_year = min(c(first_treated_year, first_inner_year, first_outer_year), na.rm = TRUE),
    first_exposure_year = ifelse(is.infinite(first_exposure_year), NA_real_, first_exposure_year),
    # Type of first exposure
    first_exposure_type = case_when(
      !is.na(first_treated_year) & first_treated_year == first_exposure_year ~ "treated",
      !is.na(first_inner_year) & first_inner_year == first_exposure_year ~ "inner", 
      !is.na(first_outer_year) & first_outer_year == first_exposure_year ~ "outer",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

cat("Exposure history summary:\n")
cat("Grid points with any exposure:", sum(!is.na(exposure_history$first_exposure_year)), "\n")
cat("First exposure types:\n")
print(table(exposure_history$first_exposure_type, useNA = "ifany"))

# Step 2: Apply clean sub-experiment rules
land_value_clean <- land_value_with_rings %>%
  left_join(exposure_history, by = "grid_id") %>%
  # Filter for clean sub-experiments
  filter(
    case_when(
      # Treated ring: only if this is first ever exposure AND first treatment
      location_type == "treated" ~ 
        (is.na(first_exposure_year) | first_exposure_year == treatment_year) &
        (!is.na(first_treated_year) & first_treated_year == treatment_year),
      
      # Inner ring: never treated before, and not in inner ring before this experiment  
      location_type == "inner" ~
        (is.na(first_treated_year) | first_treated_year > treatment_year) &
        (is.na(first_inner_year) | first_inner_year >= treatment_year),
      
      # Outer ring: never treated or inner before, and this is first/appropriate outer exposure
      location_type == "outer" ~
        (is.na(first_treated_year) | first_treated_year > treatment_year) &
        (is.na(first_inner_year) | first_inner_year > treatment_year) &
        (is.na(first_outer_year) | first_outer_year >= treatment_year),
      
      # Should not have other location types at this point
      TRUE ~ FALSE
    )
  )

# Summary of cleaning
cat("\nDataset cleaning summary:\n")
cat("Original observations:", nrow(land_value_with_rings), "\n")
cat("Clean observations:", nrow(land_value_clean), "\n") 
cat("Removed (contaminated):", nrow(land_value_with_rings) - nrow(land_value_clean), "\n")
cat("Retention rate:", round(nrow(land_value_clean) / nrow(land_value_with_rings) * 100, 1), "%\n")

cat("\nClean observations by location type:\n")
print(table(land_value_clean$location_type))

# Validation: check no double treatment
validation_treated <- land_value_clean %>%
  filter(location_type == "treated") %>%
  group_by(grid_id) %>%
  summarise(n_treatments = n_distinct(treatment_year), .groups = "drop")

cat("\nValidation - grid points treated multiple times:", sum(validation_treated$n_treatments > 1), "\n")

# Use clean dataset going forward
land_value_with_rings <- land_value_clean

# Prepare event study data -----
cat("\n=== Preparing Event Study Data ===\n")

# Create event study dataset with period-based event times
# Get ordered sequence of available data years
data_years <- sort(unique(land_value_with_rings$year))
cat("Available data years:", paste(data_years, collapse = ", "), "\\n")

# Function to convert year and treatment_year to relative period
get_relative_period <- function(year, treatment_year) {
  year_position <- match(year, data_years)
  treatment_position <- match(treatment_year, data_years)
  return(year_position - treatment_position)
}

event_study_data <- land_value_with_rings %>%
  filter(location_type %in% c("treated", "inner", "outer")) %>%
  # Create relative period (not calendar time difference)
  mutate(
    event_time = mapply(get_relative_period, year, treatment_year),
    # Create cohort identifier
    cohort = paste0("cohort_", treatment_year)
  )

cat("Event study data created:", nrow(event_study_data), "observations\n")
cat("Event time range:", min(event_study_data$event_time), "to", max(event_study_data$event_time), "\n")

# Show the new event time distribution
cat("\\nEvent time (relative periods) distribution:\\n")
print(table(event_study_data$event_time))

# Summary by location type and event time
event_summary <- event_study_data %>%
  group_by(location_type, event_time) %>%
  summarise(n_obs = n(), mean_llv = mean(llv, na.rm = TRUE), .groups = "drop")

cat("Sample sizes by location type:\n")
print(event_study_data %>% count(location_type))

# Save processed data
write_csv(event_study_data, here(data_dir, "chicago_land_value_event_study_data.csv"))
cat("Event study data saved to:", here(data_dir, "chicago_land_value_event_study_data.csv"), "\n")

# Run stacked difference-in-differences regression -----
cat("\n=== Running Stacked DiD Regressions with Urban Renewal Tests ===\n")

# Check urban renewal overlap with analysis sample
cat("Urban renewal summary in event study data:\n")
ur_summary <- event_study_data %>%
  group_by(location_type, in_urban_renewal) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  pivot_wider(names_from = in_urban_renewal, values_from = n_obs, names_prefix = "ur_")

print(ur_summary)


# Calculate Wing (2024) weights for stacked DiD
cat("Calculating Wing (2024) weights...\n")
# Wing weights calculated within each event year (treatment cohort)
event_study_data <- event_study_data %>%
  group_by(event_time, project_id, location_type) %>%
  mutate(N_re = n()) %>%  # Observations in ring r around project p to which land value cell belongs
  ungroup() %>%
  group_by(event_time, location_type) %>%
  mutate(N_r = n()) %>%  # Total observations in ring r across all projects in this cohort
  ungroup() %>%
  mutate(wing_weight = N_re / N_r)  # Corrected formula: N_re / N_r

cat("Wing weights calculated\n")

# DETAILED WING WEIGHT DIAGNOSTICS
cat("\n=== Wing Weight Diagnostics ===\n")

# 1. Wing weight summary by location type
cat("Wing weight summary by location type:\n")
wing_summary <- event_study_data %>%
  group_by(location_type) %>%
  summarise(
    mean_weight = mean(wing_weight, na.rm = TRUE),
    min_weight = min(wing_weight, na.rm = TRUE),
    max_weight = max(wing_weight, na.rm = TRUE),
    sd_weight = sd(wing_weight, na.rm = TRUE),
    .groups = "drop"
  )
print(wing_summary)

# 2. Sample sizes that create the weights
cat("\nSample size breakdown (N_re = obs per project-location combo):\n")
sample_breakdown <- event_study_data %>%
  group_by(project_id, location_type) %>%
  summarise(N_re = n(), .groups = "drop") %>%
  group_by(location_type) %>%
  summarise(
    n_project_combos = n(),
    mean_N_re = mean(N_re),
    min_N_re = min(N_re),
    max_N_re = max(N_re),
    total_N_r = sum(N_re),
    .groups = "drop"
  )
print(sample_breakdown)

# 3. Check for projects with very different sample sizes
cat("\nProjects with extreme sample sizes:\n")
project_sizes <- event_study_data %>%
  group_by(project_id, location_type) %>%
  summarise(N_re = n(), wing_weight = first(wing_weight), .groups = "drop") %>%
  arrange(desc(N_re))

cat("Largest N_re (smallest weights):\n")
print(head(project_sizes, 10))

cat("\nSmallest N_re (largest weights):\n") 
print(tail(project_sizes, 10))

# 4. Distribution of wing weights
cat("\nWing weight distribution:\n")
wing_dist <- event_study_data %>%
  select(project_id, location_type, wing_weight) %>%
  distinct() %>%
  pull(wing_weight) %>%
  quantile(probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), na.rm = TRUE)
print(wing_dist)

# 5. Check if any weights are extreme
extreme_weights <- event_study_data %>%
  select(project_id, location_type, wing_weight) %>%
  distinct() %>%
  filter(wing_weight > 10 | wing_weight < 0.1) %>%
  arrange(desc(wing_weight))

if(nrow(extreme_weights) > 0) {
  cat("\nExtreme weights (>10 or <0.1):\n")
  print(extreme_weights)
} else {
  cat("\nNo extreme weights found (all between 0.1 and 10)\n")
}

# 6. Create wing weight visualization
cat("\nCreating wing weight visualization...\n")
weight_plot_data <- event_study_data %>%
  select(project_id, location_type, wing_weight) %>%
  distinct()

wing_weight_plot <- ggplot(weight_plot_data, aes(x = wing_weight, fill = location_type)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~ location_type, scales = "free_y") +
  scale_fill_manual(values = c("treated" = "red", "inner" = "orange", "outer" = "blue")) +
  labs(
    title = "Distribution of Wing (2024) Weights by Location Type",
    x = "Wing Weight (N_r / N_re)",
    y = "Number of Project-Location Combinations",
    caption = "Higher weights = smaller project samples get more influence"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here(figures_dir, "wing_weights_distribution.pdf"), wing_weight_plot, width = 10, height = 6)
cat("Wing weight plot saved to:", here(figures_dir, "wing_weights_distribution.pdf"), "\n")

# numbeer of times each tract appears in dataset
event_study_data %>%
  filter(year == 1961) %>% 
  group_by(grid_id) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  arrange(desc(n_obs)) %>%
  head(10) %>%
  print()

# PRE-TREND ROBUSTNESS CHECKS
cat("\n=== Pre-trend Robustness Tests ===\n")

# Test 1: Alternative reference period (t=-2)
cat("Testing alternative reference period (t=-2)...\n")
alt_ref_reg <- feols(
  llv ~ i(event_time, location_type, ref = -2, ref2 = "control_800_1000") |
    project_id^year + project_id^location_type + grid_id,
  data = event_study_data %>% filter(event_time >= -3, event_time <= 5),
  weights = ~ wing_weight,
  cluster = ~ project_id
)

# Test 2: Linear pre-trend test
cat("Testing for linear pre-trends...\n")
pretrend_data <- event_study_data %>% 
  filter(event_time >= -3, event_time <= -1) %>%
  mutate(linear_trend = event_time + 3)  # Normalize to 0,1,2

pretrend_reg <- feols(
  llv ~ linear_trend:location_type |
    project_id^year + project_id^location_type + grid_id,
  data = pretrend_data,
  weights = ~ wing_weight,
  cluster = ~ project_id
)

cat("Pre-trend test results:\n")
print(pretrend_reg)

# SPECIFICATION 1: Baseline (no urban renewal controls) 
cat("\n=== Specification 1: Baseline (No UR Controls) ===\n")
baseline_reg <- feols(
  llv ~ i(event_time, location_type, ref = -1, ref2 = "control_800_1000") |
    project_id^year + project_id^location_type + grid_id,
  data = event_study_data %>% filter(event_time >= -3, event_time <= 5),
  weights = ~ wing_weight,
  cluster = ~ project_id
)

# SPECIFICATION 1B: 200m Bands Robustness
cat("\n=== Specification 1B: 200m Bands Robustness ===\n")
baseline_200m_reg <- feols(
  llv ~ i(event_time, location_type_200m, ref = -1, ref2 = "control_800_1000") |
    project_id^year + project_id^location_type_200m + grid_id,
  data = event_study_data %>% filter(event_time >= -3, event_time <= 5),
  weights = ~ wing_weight,
  cluster = ~ project_id
)

cat("Baseline regression completed\n")
print(baseline_reg)

# SPECIFICATION 2: Exclude urban renewal areas entirely
cat("\n=== Specification 2: Exclude UR Areas ===\n")
exclude_ur_reg <- feols(
  llv ~ i(event_time, location_type, ref = -1, ref2 = "control_800_1000") |
    project_id^year + project_id^location_type + grid_id,
  data = event_study_data %>% 
    filter(event_time >= -3, event_time <= 5,
           !in_urban_renewal),  # Exclude UR areas
  weights = ~ wing_weight,
  cluster = ~ project_id
)

# SPECIFICATION 2B: Exclude UR with 200m bands
cat("\n=== Specification 2B: Exclude UR Areas (200m bands) ===\n")
exclude_ur_200m_reg <- feols(
  llv ~ i(event_time, location_type_200m, ref = -1, ref2 = "control_800_1000") |
    project_id^year + project_id^location_type_200m + grid_id,
  data = event_study_data %>% 
    filter(event_time >= -3, event_time <= 5,
           !in_urban_renewal),  # Exclude UR areas
  weights = ~ wing_weight,
  cluster = ~ project_id
)

cat("Exclude UR regression completed\n")
print(exclude_ur_reg)

# SPECIFICATION 3: Control for urban renewal
cat("\n=== Specification 3: Control for UR ===\n")
control_ur_reg <- feols(
  llv ~ i(event_time, location_type, ref = -1, ref2 = "control_800_1000") |
    project_id^year + project_id^location_type + grid_id + in_urban_renewal^project_id,
  data = event_study_data %>% filter(event_time >= -3, event_time <= 5),
  weights = ~ wing_weight,
  cluster = ~ project_id
)

# SPECIFICATION 3B: Control for UR with 200m bands
cat("\n=== Specification 3B: Control for UR (200m bands) ===\n")
control_ur_200m_reg <- feols(
  llv ~ i(event_time, location_type_200m, ref = -1, ref2 = "control_800_1000") |
    project_id^year + project_id^location_type_200m + grid_id + in_urban_renewal^project_id,
  data = event_study_data %>% filter(event_time >= -3, event_time <= 5),
  weights = ~ wing_weight,
  cluster = ~ project_id
)

cat("Control UR regression completed\n")
print(control_ur_reg)

cat("\nAll three regression specifications completed\n")

# ADDITIONAL SPECIFICATIONS: Include inner ring (treated vs inner+outer)
cat("\n=== Additional Specifications: Include Inner Ring ===\n")

# SPECIFICATION 4: Baseline with inner ring (no UR controls)
cat("\n=== Specification 4: Baseline with Inner Ring (No UR Controls) ===\n")
baseline_inner_reg <- feols(
  llv ~ i(event_time, location_type, ref = -1, ref2 = "outer") |
    project_id^year + project_id^location_type + grid_id,
  data = event_study_data %>% filter(location_type != "treated", event_time >= -3, event_time <= 5),
  weights = ~ wing_weight,
  cluster = ~ project_id
)

cat("Baseline with inner ring regression completed\n")
print(baseline_inner_reg)

# SPECIFICATION 5: Exclude UR areas with inner ring
cat("\n=== Specification 5: Exclude UR Areas with Inner Ring ===\n")
exclude_ur_inner_reg <- feols(
  llv ~ i(event_time, location_type, ref = -1, ref2 = "outer") |
    project_id^year + project_id^location_type + grid_id,
  data = event_study_data %>% 
    filter(location_type != "treated", 
           event_time >= -3, event_time <= 5,
           !in_urban_renewal),  # Exclude UR areas
  weights = ~ wing_weight,
  cluster = ~ project_id
)

cat("Exclude UR with inner ring regression completed\n")
print(exclude_ur_inner_reg)

# SPECIFICATION 6: Control for UR with inner ring
cat("\n=== Specification 6: Control for UR with Inner Ring ===\n")
control_ur_inner_reg <- feols(
  llv ~ i(event_time, location_type, ref = -1, ref2 = "outer") |
    project_id^year + project_id^location_type + grid_id + in_urban_renewal^project_id,
  data = event_study_data %>% filter(location_type != "treated", event_time >= -3, event_time <= 5),
  weights = ~ wing_weight,
  cluster = ~ project_id
)

cat("Control UR with inner ring regression completed\n")
print(control_ur_inner_reg)

cat("\nAll six regression specifications completed\n")

# HETEROGENEITY BY TREATMENT YEAR
cat("\n=== Heterogeneity Analysis by Treatment Year ===\n")

# Check treatment year distribution
cat("Treatment year distribution:\n")
treatment_year_summary <- event_study_data %>%
  filter(location_type == "treated") %>%
  count(treatment_year) %>%
  arrange(treatment_year)
print(treatment_year_summary)


# SPECIFICATION 8A: Early treatment periods only (<=1961, exclude UR)
cat("\n=== Specification 8A: Early Treatment Periods (<=1961, Exclude UR) ===\n")
early_reg <- feols(
  llv ~ i(event_time, location_type, ref = -1, ref2 = "outer") |
    project_id^year + project_id^location_type + grid_id + in_urban_renewal^project_id,
  data = event_study_data %>% 
    filter(location_type != "inner", 
           event_time >= -3, event_time <= 5,
           treatment_year <= 1961),  # Early period only
  weights = ~ wing_weight,
  cluster = ~ project_id
)

cat("Early treatment regression completed\n")
print(early_reg)

# SPECIFICATION 8B: Late treatment periods only (>1961, exclude UR)  
cat("\n=== Specification 8B: Late Treatment Periods (>1961, Exclude UR) ===\n")
late_reg <- feols(
  llv ~ i(event_time, location_type, ref = -1, ref2 = "outer") |
    project_id^year + project_id^location_type + grid_id + in_urban_renewal^project_id,
  data = event_study_data %>% 
    filter(location_type != "inner", 
           event_time >= -3, event_time <= 5,
           treatment_year > 1961),   # Late period only
  weights = ~ wing_weight,
  cluster = ~ project_id
)

cat("Late treatment regression completed\n")
print(late_reg)

# Show sample sizes by period
cat("\nSample sizes by treatment period (excluding UR):\n")
early_n <- event_study_data %>%
  filter(location_type != "inner", 
         event_time >= -3, event_time <= 5,
         !in_urban_renewal,
         treatment_year <= 1961) %>%
  count(location_type) %>%
  mutate(period = "early")

late_n <- event_study_data %>%
  filter(location_type != "inner", 
         event_time >= -3, event_time <= 5,
         !in_urban_renewal,
         treatment_year > 1961) %>%
  count(location_type) %>%
  mutate(period = "late")

period_summary <- bind_rows(early_n, late_n)
print(period_summary)

cat("\nHeterogeneity analysis completed\n")


# Extract coefficients for plotting
extract_event_coeffs <- function(reg_results, location_type_name) {
  coef_names <- names(coef(reg_results))
  event_coefs <- coef_names[str_detect(coef_names, paste0("event_time.*", location_type_name))]
  
  coeffs <- coef(reg_results)[event_coefs]
  se <- sqrt(diag(vcov(reg_results)))[event_coefs]
  
  # Extract event times
  event_times <- str_extract(names(coeffs), "-?[0-9]+")
  event_times <- as.numeric(event_times)
  
  # Add reference period
  if(!(-1 %in% event_times)) {
    event_times <- c(event_times, -1)
    coeffs <- c(coeffs, 0)
    se <- c(se, 0)
  }
  
  data.frame(
    event_time = event_times,
    coeff = coeffs,
    se = se,
    location_type = location_type_name,
    ci_lower = coeffs - 1.96 * se,
    ci_upper = coeffs + 1.96 * se
  ) %>%
    arrange(event_time)
}

# Extract coefficients
treated_coeffs <- extract_event_coeffs(main_reg, "treated")
inner_coeffs <- extract_event_coeffs(main_reg, "inner")

event_study_coeffs <- bind_rows(treated_coeffs, inner_coeffs)

# Create event study plot
cat("Creating event study plots...\n")

event_study_plot <- ggplot(event_study_coeffs, aes(x = event_time, y = coeff, color = location_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_vline(xintercept = -0.5, linetype = "solid", alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.5, alpha = 0.7) +
  geom_line(alpha = 0.7) +
  scale_color_manual(values = c("treated" = "red", "inner" = "orange"),
                     labels = c("treated" = "Treatment Zone (0-0.4 km)", 
                               "inner" = "Inner Ring (0.4-0.8 km)"),
                     name = "Location") +
  labs(
    title = "Effect of Public Housing on Chicago Land Values",
    subtitle = "Stacked Difference-in-Differences Event Study",
    x = "Years Since Public Housing Completion",
    y = "Effect on Log Land Value",
    caption = "Reference: Outer ring (0.8-1.6 km). 95% confidence intervals.\nClustered standard errors at project level."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom"
  )

ggsave(here(figures_dir, "chicago_land_value_event_study.pdf"), 
       event_study_plot, width = 12, height = 8)

# Save all robustness regression results
cat("Saving robustness regression results...\n")

# Save comprehensive comparison table
etable(baseline_reg, baseline_200m_reg, exclude_ur_reg, exclude_ur_200m_reg, control_ur_reg, control_ur_200m_reg,
       tex = TRUE,
       file = here(results_dir, "chicago_land_value_robustness_comprehensive.tex"),
       title = "Chicago Land Values: Pre-trend and Ring Width Robustness",
       label = "tab:chicago_robustness_comprehensive")

# Save pre-trend test results
etable(pretrend_reg,
       tex = TRUE, 
       file = here(results_dir, "chicago_land_value_pretrend_tests.tex"),
       title = "Chicago Land Values: Linear Pre-trend Tests",
       label = "tab:chicago_pretrend_tests")

# Save alternative reference period results  
etable(alt_ref_reg,
       tex = TRUE,
       file = here(results_dir, "chicago_land_value_alt_reference.tex"), 
       title = "Chicago Land Values: Alternative Reference Period (t=-2)",
       label = "tab:chicago_alt_reference")

# Compare sample sizes by ring width
cat("\nSample size comparison by ring specification:\n")
sample_comparison <- bind_rows(
  event_study_data %>% 
    filter(event_time >= -3, event_time <= 5) %>%
    count(location_type) %>% 
    mutate(specification = "100m_bands"),
  event_study_data %>% 
    filter(event_time >= -3, event_time <= 5) %>%
    count(location_type_200m) %>% 
    rename(location_type = location_type_200m) %>%
    mutate(specification = "200m_bands")
)
print(sample_comparison)
write_csv(sample_comparison, here(data_dir, "chicago_sample_size_comparison.csv"))

# Summary statistics
summary_stats <- event_study_data %>%
  group_by(location_type) %>%
  summarise(
    n_obs = n(),
    n_unique_points = n_distinct(grid_id),
    mean_llv = mean(llv, na.rm = TRUE),
    sd_llv = sd(llv, na.rm = TRUE),
    min_year = min(year),
    max_year = max(year),
    .groups = "drop"
  )

write_csv(summary_stats, here(data_dir, "chicago_summary_statistics.csv"))

cat("\n=== Robustness Checks ===\n")

# Alternative ring definitions
cat("Running robustness check with alternative ring definitions...\n")

# Smaller rings for robustness: 0.2, 0.5, 1.0 km
alt_inner_distance <- 200        # 0.2 km
alt_outer_distance <- 500        # 0.5 km
alt_max_distance <- 1000         # 1.0 km

# Function to assign alternative rings
assign_alt_rings <- function(land_points, ph_projects) {
  land_points$alt_location_type <- "control"
  land_points$alt_treatment_year <- NA
  
  for(i in 1:nrow(ph_projects)) {
    project <- ph_projects[i,]
    
    distances <- as.numeric(st_distance(land_points, project))
    
    # Assign based on distance thresholds (closest assignment wins)
    for(j in 1:nrow(land_points)) {
      if(distances[j] <= alt_inner_distance) {
        land_points$alt_location_type[j] <- "treated"
        land_points$alt_treatment_year[j] <- project$treatment_year
      } else if(distances[j] <= alt_outer_distance && land_points$alt_location_type[j] == "control") {
        land_points$alt_location_type[j] <- "inner"
        land_points$alt_treatment_year[j] <- project$treatment_year
      } else if(distances[j] <= alt_max_distance && land_points$alt_location_type[j] == "control") {
        land_points$alt_location_type[j] <- "outer"
        land_points$alt_treatment_year[j] <- project$treatment_year
      }
    }
  }
  
  return(land_points)
}

# Create alternative event study data
alt_event_data <- land_value_data_sf %>%
  group_by(Year) %>%
  group_modify(~ assign_alt_rings(.x, public_housing_data)) %>%
  ungroup() %>%
  filter(alt_location_type %in% c("treated", "inner", "outer")) %>%
  mutate(
    event_time = Year - alt_treatment_year,
    cohort = paste0("cohort_", alt_treatment_year)
  ) %>%
  filter(event_time >= -20, event_time <= 20) %>%
  st_drop_geometry()

# Run alternative regression
alt_reg <- feols(
  llv ~ i(event_time, alt_location_type, ref = c(-1, "outer")) | 
        grid_point_id + Year + cohort + community_area,
  data = alt_event_data,
  cluster = ~ grid_point_id
)

# Save alternative results
etable(alt_reg,
       tex = TRUE, 
       file = here(results_dir, "chicago_land_value_alt_rings_regression.tex"),
       title = "Effect of Public Housing on Chicago Land Values - Alternative Ring Definitions (0.2, 0.5, 1.0 km)",
       label = "tab:chicago_land_values_alt")

cat("\n=== Analysis Complete ===\n")
cat("Results saved to:\n")
cat("- Figures:", figures_dir, "\n")
cat("- Regression tables:", results_dir, "\n") 
cat("- Data outputs:", data_dir, "\n")

# Print key results
cat("\nKey findings:\n")
cat("Main sample size:", nrow(event_study_data), "observations\n")
cat("Number of public housing projects:", nrow(public_housing_data), "\n")
cat("Treatment periods:", paste(sort(unique(event_study_data$treatment_year)), collapse = ", "), "\n")