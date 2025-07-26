###
# Create data for spatial difference-in-differences regressions

####

### Right now, this is my main analysis file

# Preliminaries -----
# Load libraries
library(tidyverse)
library(data.table)
library(sf)
library(here)
library(tigris)
library(crsuggest)
library(mapview)
library(maps)
library(ragg)
library(viridis)
library(spdep)
library(RColorBrewer)
# for covariate balance testing
library(tableone)
library(kableExtra)

library(units)


rm(list=ls())

set.seed(123)

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small" or "combined"
data_type <- "combined"


## define filepaths  -----
# output paths
merged_data_dir <- here("data", "derived", "merged", data_type)
map_output_dir <- here("output", "figures", "stacked_did", data_type)
balance_table_dir <- here("output", "balance_tables", "stacked_did", data_type)

# input paths 
treated_tracts_panel_path <- here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")
census_tract_sample_path <- here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")

# event study data paths
# event study for spatial did
event_study_rings_output_path <- here(merged_data_dir, "event_study_data_rings.csv")

# unique rings and tracts
unique_tracts_and_rings_output_path <- here(merged_data_dir, "unique_tracts_and_rings.csv")

# dataset of inner rings and total public housing built nearby 
inner_ring_tracts_first_treated_output_path <- here(merged_data_dir, "inner_ring_tracts_first_treated.csv")

# Read in and prep data ----

# read treated_tracts_panel.gpkg
treated_tracts_panel <-
  st_read(treated_tracts_panel_path) %>% 
  st_drop_geometry() 

# read in Census tract sample with treatment status
census_tract_sample <-
  st_read(census_tract_sample_path)

# read in public housing project locations
public_housing_data <-
  st_read(here("data", "derived", "public_housing", "working", "cleaned_housing_projects.gpkg")) 

# Create a dataframe with the total number of public housing units per tract
total_ph_units_per_tract <-
  treated_tracts_panel %>%
  dplyr::select(GISJOIN_1950, total_public_housing_units) %>% 
  distinct()


# Create, in each year, the treated, inner, and outer rings ----
# all neighbors among sample tracts

# 0. Get all unique tracts in the sample
census_tract_sample_indexed_unique <-
  census_tract_sample %>%
  group_by(GISJOIN_1950) %>%
  filter(row_number() == 1) %>% 
  mutate(tract_id = row_number())

# get crosswalk to all unique census tracts
census_tract_unique_crosswalk <-
  census_tract_sample_indexed_unique %>% 
  mutate(row_number = row_number()) %>%
  dplyr::select(row_number,tract_id, GISJOIN_1950) %>% 
  st_drop_geometry()

# Ensure geometries are valid
census_tract_sample_indexed_unique <-
  st_make_valid(census_tract_sample_indexed_unique)


# Now 'event_study_data' contains:
# - 'treated_id': index of the treated tract
# - 'neighbor_id': index of the neighbor tract (inner or outer ring)
# - 'ring': 1 for inner ring, 2 for outer ring
# - 'TRACTA', 'COUNTY', 'STATE': identifiers for the neighbor tract
# - 'TRACTA_treated', 'COUNTY_treated', 'STATE_treated': identifiers for the treated tract
# - 'YEAR': the year(s) associated with the treated tract


## Try creating tract clusters based on K-means clustering of census variables to use as FE-----
# Function to get tract centroids
get_centroids <- function(tracts) {
  centroids <- st_centroid(tracts)
  coords <- st_coordinates(centroids)
  return(data.frame(tract_id = tracts$tract_id, x = coords[,1], y = coords[,2]))
}

# Function to determine number of clusters based on number of tracts
determine_clusters <- function(n_tracts, factor = 0.5, min_clusters = 5, max_clusters = 40) {
  clusters <- round(sqrt(n_tracts) * factor)
  return(max(min_clusters, min(clusters, max_clusters)))
}

# Perform clustering for each city
clustered_tracts <- 
  census_tract_sample_indexed_unique %>%
  group_by(city) %>%
  group_modify(~ {
    city_centroids <- get_centroids(.x)
    n_clusters <- determine_clusters(nrow(.x))
    
    kmeans_result <- kmeans(city_centroids[, c("x", "y")], centers = n_clusters)
    
    .x$cluster <- kmeans_result$cluster
    .x$cluster_id <- paste(.x$city, .x$cluster, sep = "_")
    return(.x)
  }) %>%
  ungroup()

clustered_tracts <- st_as_sf(clustered_tracts)


n_clusters <- length(unique(clustered_tracts$cluster_id[clustered_tracts$city == "New York City"]))
colors <- colorRampPalette(brewer.pal(8, "Set2"))(n_clusters)

# Plot for a given city
ggplot(clustered_tracts %>% filter(city == "New York City")) +
  geom_sf(aes(fill = factor(cluster_id)), color = "white", size = 0.1) +
  scale_fill_manual(values = colors) +
  facet_wrap(~ city) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Census Tracts Clustered by Proximity Within Cities")


# create dataset with cluster FE
clustered_tracts_panel <- 
  clustered_tracts %>% 
  dplyr::select(GISJOIN_1950, city, cluster) %>% 
  distinct() 


# Set up stacked, spatial DiD dataset  ----

### 1. Get treated tracts, inner ring tracts, outer ring tracts in a wide dataset ---- 
# above

# identify ever treated tracts
ever_treated_tracts <- 
  census_tract_sample %>%
  filter(treated == 1 ) %>% 
  st_drop_geometry() %>% 
  dplyr::select(GISJOIN_1950) %>%
  distinct() %>% 
  mutate(ever_treated = 1)

# merge to census tract sample
census_tract_sample_indexed_unique <-
  census_tract_sample_indexed_unique %>%
  left_join(ever_treated_tracts, by = c("GISJOIN_1950")) %>%
  mutate(ever_treated = ifelse(is.na(ever_treated), 0, 1))


# create neighbor list
nb <- poly2nb(census_tract_sample_indexed_unique)

# create higher-order neighbor list
nblags <- nblag(nb, maxlag = 2)

# Convert tracts to data.table

tracts_dt <- data.table(
  tract_index = 1:nrow(census_tract_sample_indexed_unique),
  GISJOIN_1950 = census_tract_sample_indexed_unique$GISJOIN_1950
)

# Convert treated_tracts_panel to data.table if not already
setDT(treated_tracts_panel)

# Get unique treated tracts with treatment_year
treated_years <- unique(treated_tracts_panel[, .(GISJOIN_1950, treatment_year)])

# Merge to get tract indices for treated tracts
treated_indices <- merge(
  tracts_dt,
  treated_years,
  by = c("GISJOIN_1950"),
  all.x = FALSE,
  all.y = TRUE
)[, .(tract_index, GISJOIN_1950, treatment_year)]

# Initialize results data.table
results <- data.table()

# For each treated tract
for (i in treated_indices$tract_index) {
  treated_id <- i
  
  # Ring 1 (Inner Ring) Neighbors
  first_order_neighbors <- nblags[[1]][[i]]
  if (length(first_order_neighbors) > 0) {
    results <- rbind(results, data.table(
      treated_id = treated_id,
      ring = 1,
      neighbor_id = first_order_neighbors
    ))
  }
  
  # Ring 2 (Outer Ring) Neighbors
  second_order_neighbors <- nblags[[2]][[i]]
  # Exclude tracts already in treated_id and first_order_neighbors
  second_order_neighbors <- setdiff(second_order_neighbors, c(i, first_order_neighbors))
  if (length(second_order_neighbors) > 0) {
    results <- rbind(results, data.table(
      treated_id = treated_id,
      ring = 2,
      neighbor_id = second_order_neighbors
    ))
  }
}

# Remove duplicates in results
results <- unique(results)

# Merge to get neighbor tract information
neighbor_info <- tracts_dt[, .(
  neighbor_id = tract_index,
  GISJOIN_1950 = GISJOIN_1950
)]

results <- merge(
  results,
  neighbor_info,
  by = "neighbor_id",
  all.x = TRUE
)

# Get treated tract information
treated_info <- tracts_dt[, .(
  treated_id = tract_index,
  GISJOIN_1950_treated = GISJOIN_1950
)]
# Merge treated tract information
event_study_data <- merge(
  results,
  treated_info,
  by = "treated_id",
  all.x = TRUE
)

# # Merge treatment_year into event_study_data
# event_study_data <- merge(
#   event_study_data,
#   treated_indices[, .(treated_id = tract_index, treatment_year)],
#   by = "treated_id",
#   all.x = TRUE
# )
# 

## 2. Convert to long, ensure clean comparisons ----
# Convert event_study_data to a tibble
event_study_data <- as_tibble(event_study_data)

# Create treated tracts dataset
treated_tracts <- 
  event_study_data %>%
  dplyr::select(treated_id, GISJOIN_1950_treated) %>%
  distinct() %>%
  mutate(location_type = "treated")

# Create inner ring dataset
inner_ring_tracts <- event_study_data %>%
  filter(ring == 1) %>%
  dplyr::select(treated_id, neighbor_id, GISJOIN_1950) %>%
  distinct() %>%
  rename(ring_id = neighbor_id) %>%
  mutate(location_type = "inner")

# Create outer ring dataset
outer_ring_tracts <- event_study_data %>%
  filter(ring == 2) %>%
  dplyr::select(treated_id, neighbor_id, GISJOIN_1950) %>%
  distinct() %>%
  rename(ring_id = neighbor_id) %>%
  mutate(location_type = "outer")

# Combine datasets
combined_ring_data <- bind_rows(
  treated_tracts %>%
    mutate(ring_id = treated_id, GISJOIN_1950 = GISJOIN_1950_treated) %>%
    dplyr::select(-ends_with("_treated")),
  inner_ring_tracts,
  outer_ring_tracts
)

# Identify ever-treated tracts
ever_treated_tracts <- treated_tracts %>%
  dplyr::select(GISJOIN_1950_treated) %>%
  distinct() %>%
  rename(GISJOIN_1950 = GISJOIN_1950_treated) %>%
  mutate(ever_treated = 1)

# Clean the data
event_study_data_long <-
  combined_ring_data %>%
  left_join(ever_treated_tracts, by = c("GISJOIN_1950")) %>% 
  # Filter ever-treated tracts from inner and outer rings
  filter(is.na(ever_treated) | location_type == "treated")

# Identify ever inner ring tracts among these never-treated tracts
ever_inner_ring_tracts <- 
  event_study_data_long %>%
  filter(location_type == "inner") %>%
  dplyr::select(GISJOIN_1950) %>%
  distinct() %>%
  mutate(ever_inner_ring = 1)

# Filter ever inner ring tracts from outer rings
event_study_data_long <-
  event_study_data_long %>%
  left_join(ever_inner_ring_tracts, by = c("GISJOIN_1950")) %>%
  filter(is.na(ever_inner_ring) | location_type %in% c("treated", "inner")) %>% 
  # Drop ever_treated and ever_inner_ring columns
  dplyr::select(-ever_treated, -ever_inner_ring)

# Prepare tracts and rings
tracts_and_rings <-
  event_study_data_long %>%
  dplyr::select(GISJOIN_1950, treated_id, location_type) %>%
  distinct()

years <- seq(1930, 1990, by = 10)

# Expand the data to include all combinations of tracts and years
event_study_data_full <-
  expand_grid(tracts_and_rings, YEAR = years)

# merge on treatment years 
event_study_data_full<-
  merge(
    event_study_data_full,
    treated_indices[, .(treated_id = tract_index, treatment_year)],
    by = "treated_id"
  )

# Prepare census data
census_data_for_event_study <- census_tract_sample %>% 
  dplyr::select(GISJOIN_1950, STATE, COUNTY, TRACTA, YEAR, city, cbsa_title,
         black_share, white_share, white_pop, black_pop, total_pop, 
         median_income, median_rent_calculated, median_home_value_calculated, median_educ_years_25plus,
         pct_hs_grad, pct_some_college,
         population_density, distance_from_cbd, distance_from_project, 
         housing_density,
         employment_pop_ratio, unemp_rate, lfp_rate) %>%
  st_drop_geometry() %>% 
  # Inverse hyperbolic sine transformations
  mutate(
    asinh_pop_total = asinh(total_pop),
    asinh_pop_white = asinh(white_pop),
    asinh_pop_black = asinh(black_pop),
    asinh_median_income = asinh(median_income),
    asinh_median_rent_calculated = asinh(median_rent_calculated),
    asinh_median_home_value_calculated = asinh(median_home_value_calculated)
  ) %>% 
  # Join on clusters if applicable
  left_join(clustered_tracts_panel, by = c("GISJOIN_1950", "city"))

# Merge on census variables
event_study_final <-
  left_join(event_study_data_full, census_data_for_event_study, 
            by = c("GISJOIN_1950", "YEAR")) %>% 
  # Define treated variable equal to 1 if YEAR >= treatment_year and location_type == treated
  mutate(
    treated = ifelse(YEAR >= treatment_year & location_type == "treated", TRUE, FALSE),
    # Variables for Sun and Abraham estimates
    cohort = as.factor(treatment_year), 
    relative_period = (YEAR - treatment_year) / 10
  ) %>% 
  # Keep distinct observations: One observation per treated_id-tract-year
  distinct()

# Create ring variable for ring fixed effects
event_study_data_rings <- 
  event_study_final %>%
  mutate(
    event_time = as.factor(YEAR - treatment_year),
    ring = as.factor(case_when(
      location_type == "treated" ~ 0,
      location_type == "inner" ~ 1,
      location_type == "outer" ~ 2
    )),
    location_type_factor = factor(location_type, levels = c("outer", "inner", "treated")),
    location_type_dummy = as.numeric(location_type_factor),
    # Create a unique tract ID
    tract_id = paste(GISJOIN_1950, sep = "_")
  ) %>% 
  dplyr::rename(year = YEAR)

# Calculate baseline black share (at event_time == -10)
baseline_black_share <-
  event_study_data_rings %>%
  filter(event_time == "-10") %>% 
  dplyr::select(treated_id, GISJOIN_1950, black_share) %>% 
  rename(black_share_baseline = black_share) %>% 
  distinct()

# Merge on by treated project
event_study_data_rings <-
  event_study_data_rings %>%
  left_join(baseline_black_share, by = c("treated_id", "GISJOIN_1950"))

# Define distance thresholds (in meters, same units as distance_from_project)

max_dist_outer <- 1000    # and within 1000 meters (following Blanco and Neri)
                          # This is approximately 0.6 miles
max_dist_inner <- 500     # and within 500 meters
                          # This is approximately 0.3 miles

# No minimum distance thresholds - trust the contiguity-based ring definitions
# Only use maximum distance to exclude tracts that are unreasonably far due to odd boundaries



# look at distributions of distance_from_project
inner_dist_summary <- event_study_data_rings %>%
  filter(location_type == "inner") %>% 
  pull(distance_from_project) %>% 
  summary()

outer_dist_summary <- event_study_data_rings %>%
  filter(location_type == "outer") %>% 
  pull(distance_from_project) %>% 
  summary()

cat("Inner ring distances (contiguity-based):\n")
print(inner_dist_summary)
cat("\nOuter ring distances (contiguity-based):\n") 
print(outer_dist_summary)

# Based on these distributions, we can choose distance thresholds for distance-based rings
# Common choices in urban economics literature:
# - 0.25 miles (≈400m) for immediate spillovers
# - 0.5 miles (≈800m) for broader neighborhood effects
# - 1 km for extended effects (as in Blanco and Neri)

# Filter - only maximum distance thresholds to exclude unreasonably distant tracts
event_study_data_rings_filtered <- event_study_data_rings %>%
  filter(
    (location_type == "treated") |
      (location_type == "inner" & distance_from_project <= max_dist_inner) |
      (location_type == "outer" & distance_from_project <= max_dist_outer)
  )

# FOR NOW: TRY WITH EVENT STUDY DATA RINGS FILTERED
event_study_data_rings <- event_study_data_rings_filtered

# Output event study data with rings
write_csv(event_study_data_rings, event_study_rings_output_path)

## Visualize ring structure -----
# Function to visualize rings for a specific treated tract
visualize_rings_for_treated_tract <- function(treated_tract_id, year = 1970) {
  
  # Get all tracts associated with this treated tract
  rings_for_tract <- event_study_data_rings %>%
    filter(treated_id == treated_tract_id, year == year)
  
  if(nrow(rings_for_tract) == 0) {
    stop("No data found for treated_id:", treated_tract_id)
  }
  
  # Get the geographic data
  tract_geoms <- census_tract_sample %>%
    filter(YEAR == year, GISJOIN_1950 %in% rings_for_tract$GISJOIN_1950) %>%
    select(GISJOIN_1950, geom)
  
  # Merge with ring data
  rings_with_geom <- rings_for_tract %>%
    left_join(tract_geoms, by = "GISJOIN_1950") %>%
    st_as_sf()
  
  # Get the treated tract location for reference
  treated_tract_geom <- rings_with_geom %>% filter(location_type == "treated")
  
  # Color mapping
  colors <- c("treated" = "red", "inner" = "orange", "outer" = "lightblue")
  
  # Create mapview
  map <- mapview(rings_with_geom, 
                 zcol = "location_type",
                 col.regions = colors,
                 layer.name = paste("Rings for Treated Tract", treated_tract_id))
  
  return(map)
}

# Function to investigate 0-distance issues
investigate_zero_distance <- function() {
  
  # Find problematic cases
  zero_dist_outer <- event_study_data_rings %>%
    filter(location_type == "outer", distance_from_project == 0, !is.na(distance_from_project))
  
  if(nrow(zero_dist_outer) == 0) {
    cat("No outer ring tracts with 0 distance found after filtering!\n")
    return(NULL)
  }
  
  cat("Found", nrow(zero_dist_outer), "outer ring tracts with 0 distance\n")
  
  # Pick first example to visualize
  example <- zero_dist_outer[1,]
  
  # Get the tract geometry for the problem tract
  problem_tract <- census_tract_sample %>%
    filter(YEAR == 1970, GISJOIN_1950 == example$GISJOIN_1950)
  
  if(nrow(problem_tract) == 0) {
    cat("Could not find geometry for problem tract\n")
    return(example)
  }
  
  # Get all projects and tracts in the area around this tract
  area_buffer <- st_buffer(problem_tract, dist = 2000)  # 2km buffer
  
  area_tracts <- st_filter(census_tract_sample %>% filter(YEAR == 1970), area_buffer)
  area_projects <- st_filter(public_housing_data, area_buffer)
  
  # Create map showing the problem
  map <- mapview(area_tracts, alpha.regions = 0.1, color = "gray", layer.name = "Area Tracts") +
         mapview(area_projects, col.regions = "red", cex = 3, layer.name = "Projects") +
         mapview(problem_tract, col.regions = "blue", layer.name = "Problem Tract (0 distance)")
  
  return(list(data = example, map = map))
}

# Example usage (uncomment to run):
#visualize_rings_for_treated_tract(1)  # Replace 1 with actual treated_id
#investigate_zero_distance()

# Output unique tracts and ring status, and their associated treated tract ----
unique_tracts_and_rings <-
  event_study_data_rings %>%
  dplyr::select(GISJOIN_1950, location_type, treatment_year) %>%
  distinct() %>%
  # Keep first year of treatment for each tract
  group_by(GISJOIN_1950, location_type) %>%
  filter(treatment_year == min(treatment_year)) %>%
  ungroup()

write_csv(unique_tracts_and_rings, unique_tracts_and_rings_output_path)

## create a dataset with all "inner ring" tracts and their associated treated tract info variable ----
# Need: 1. Treated tract and its total public housing in event time 0 
#       2. Inner ring tracts and their associated treated tract
#       3. Combine to get each inner ring tract's associated treated tract and total public housing in event time 0
#       4. Collapse 

treatment_size_event_time_0 <-
  total_ph_units_per_tract %>% 
  mutate(treated_tract_id = paste(GISJOIN_1950, sep = "_")) %>% 
  dplyr::select(treated_tract_id, total_public_housing_units) 

# inner ring tracts (first treated) and their associated treated tract
inner_ring_tracts_first_treated <- 
  event_study_data_rings %>% 
  #filter(location_type == "inner") %>%
  dplyr::select(GISJOIN_1950, treated_id, tract_id, location_type, treatment_year) %>% 
  distinct() %>% 
  # Keep first year of treatment for each tract
  group_by(GISJOIN_1950, location_type) %>%
  filter(treatment_year == min(treatment_year)) %>%
  ungroup()

unique_treated_tracts <-
  inner_ring_tracts_first_treated %>% filter(location_type == "treated") %>% 
  dplyr::select(treated_id, tract_id) %>%
  dplyr::rename(associated_treated_tract_id = tract_id)

# merge onto the unique tracts and rings to associate each ring with its treated tract
inner_ring_tracts_first_treated <- inner_ring_tracts_first_treated %>% 
  left_join(unique_treated_tracts, by = "treated_id") %>% 
  filter(location_type == "inner") %>% 
  dplyr::select(GISJOIN_1950, treatment_year, associated_treated_tract_id) %>% 
  distinct() 

# merge on the total public housing units in event time 0 and collapse 
inner_ring_tracts_first_treated_collapsed <- inner_ring_tracts_first_treated %>% 
  left_join(treatment_size_event_time_0, by = c("associated_treated_tract_id" = "treated_tract_id"))  %>% 
  # Collapse: sum 
  group_by(GISJOIN_1950, treatment_year) %>%
  summarise(total_public_housing_units_built_nearby = sum(total_public_housing_units))

# output 
write_csv(inner_ring_tracts_first_treated_collapsed, inner_ring_tracts_first_treated_output_path)



#  TODO: Move this elsewhere
# # Create, in each year, 800, 1600, 2400, and 3200m rings based on centroid distance ----
# 
# # unique treated tracts, centroids
# treated_tracts_centroids <-
#   census_tract_sample %>% 
#   filter(treated == 1) %>% 
#   dplyr::select(STATE, COUNTY, TRACTA) %>% 
#   distinct() %>% 
#   st_centroid()
# 
# # all non-treated tracts
# never_treated_tracts <- 
#   census_tract_sample %>% 
#   st_drop_geometry() %>% 
#   dplyr::select(STATE, COUNTY, TRACTA) %>% 
#   distinct() %>% 
#   anti_join(treated_tracts_centroids, by = c("STATE", "COUNTY", "TRACTA"))
# 
# never_treated_tract_centroids <- 
#   census_tract_sample_indexed_unique %>% 
#   dplyr::select(STATE, COUNTY, TRACTA) %>% 
#   semi_join(never_treated_tracts, by = c("STATE", "COUNTY", "TRACTA")) %>%
#   st_centroid()
# 
# # Now, you can use `within_distance_1600m` and `within_distance_3200m` for your analysis.
# # For example, you could filter tracts within the 1600m or 3200m rings:
# 
# ## Create stacked dataset ----
# 
# # 1. Create an empty list to store results
# stacked_rings_data <- list()
# 
# # Step 2: Loop over each treated tract and find which tracts fall into its rings
# for (i in seq_len(nrow(treated_tracts_centroids))) {
#   
#   # Get the current treated tract centroid
#   treated_tract <- treated_tracts_centroids[i, ]
#   
#   # Calculate distance between the current treated tract and all non-treated tracts
#   distances <- st_distance(never_treated_tract_centroids, treated_tract)
#   
#   # Convert thresholds (800m, 1600m, etc.) to units of meters
#   threshold_800m <- set_units(800, "m")
#   threshold_1600m <- set_units(1600, "m")
#   threshold_2400m <- set_units(2400, "m")
#   threshold_3200m <- set_units(3200, "m")
#   
#   
#   # Identify tracts within the specified distance rings
#   tracts_in_800m <- never_treated_tract_centroids[distances <= threshold_800m, ]
#   tracts_in_1600m <- never_treated_tract_centroids[distances > threshold_800m & distances <= threshold_1600m, ]
#   tracts_in_2400m <- never_treated_tract_centroids[distances > threshold_1600m & distances <= threshold_2400m, ]
#   tracts_in_3200m <- never_treated_tract_centroids[distances > threshold_2400m & distances <= threshold_3200m, ]
#   
#   # Label the rings for each group of tracts
#   tracts_in_800m$ring <- "800m"
#   tracts_in_1600m$ring <- "1600m"
#   tracts_in_2400m$ring <- "2400m"
#   tracts_in_3200m$ring <- "3200m"
#   
#   # Combine all tracts for this treated tract
#   tracts_for_treated <- bind_rows(tracts_in_800m, tracts_in_1600m, tracts_in_2400m, tracts_in_3200m)
#   
#   # Add treated tract information (i.e., identifier for the treated tract)
#   tracts_for_treated$treated_tract_id <- treated_tract$TRACTA
#   
#   # Add to the list of results
#   stacked_rings_data[[i]] <- tracts_for_treated
# }
# 
# # Step 3: Combine the results into a single stacked dataset
# stacked_rings_dataset <- bind_rows(stacked_rings_data)
# 
# # Clean up the dataset, keeping relevant columns
# stacked_rings_dataset <-
#   stacked_rings_dataset %>%
#   dplyr::select(STATE, COUNTY, TRACTA, treated_tract_id, ring) %>%
#   st_drop_geometry() %>% 
#   distinct()  # Ensure there are no duplicates
# 
# # output the stacked dataset
# write_csv(stacked_rings_dataset, here(merged_data_dir, "stacked_distance_rings_dataset.csv"))
# 
# 
# ## Create a map for fun -----
# # merge on geographies
# x <-
#   stacked_rings_dataset %>% 
#   left_join(census_tract_sample_indexed_unique %>% dplyr::select(city, STATE, COUNTY, TRACTA, geom),  by = c("STATE", "COUNTY", "TRACTA"))
# 
# chicago_rings <-
#   x %>% 
#   filter(city == "Chicago") 
# 
# # Step 1: Filter data for Chicago
# chicago_tracts <- census_tract_sample %>%
#   filter(city == "Chicago") %>%   # Ensure there is a 'city' or similar variable indicating Chicago
#   # keep unique
#   group_by(STATE, COUNTY, TRACTA) %>%
#   distinct() %>% 
#   ungroup()
# 
# # Step 2: Define layers for treated tracts, 1600m rings, and 3200m rings
# treated_tracts_chicago <- census_tract_sample %>%
#   filter(city == "Chicago", treated == 1) %>% 
#   group_by(STATE, COUNTY, TRACTA) %>%
#   filter(row_number() == 1)
# 
# tracts_within_800m <- 
#   chicago_rings %>%
#   filter(ring == "800m")  %>% # Tracts within 800 meters of treated tracts
#   group_by(STATE, COUNTY, TRACTA) %>%
#   filter(row_number() == 1)
# 
# tracts_within_1600m_chicago <-
#   chicago_rings %>%
#   filter(ring == "1600m") %>%  # Tracts within 1600 meters of treated tracts
#   group_by(STATE, COUNTY, TRACTA) %>%
#   filter(row_number() == 1)
# 
# tracts_within_2400m_chicago <-
#   chicago_rings %>%
#   filter(ring == "2400m")  %>% # Tracts within 2400 meters of treated tracts
#   group_by(STATE, COUNTY, TRACTA) %>%
#   filter(row_number() == 1)
# 
# tracts_within_3200m_chicago <-
#   chicago_rings %>%
#   filter(ring == "3200m") %>%  # Tracts within 3200 meters of treated tracts
#   group_by(STATE, COUNTY, TRACTA) %>%
#   filter(row_number() == 1)
# 
# # Step 3: Create the map using ggplot2
# # Create the map using ggplot2
# ggplot() +
#   # Plot all Chicago tracts as the base layer (very light grey)
#   geom_sf(data = chicago_tracts, aes(geometry = geom), fill = "lightgray", color = "darkgrey", size = 0.2) +
#   # Plot 3200m rings first (lightest blue)
#   geom_sf(data = tracts_within_3200m_chicago, aes(geometry = geom),
#           fill = "#3895d3", color = NA, alpha = 0.7) +
#   # Plot 2400m rings (light-medium blue)
#   geom_sf(data = tracts_within_2400m_chicago, aes(geometry = geom),
#           fill = "#1261a0", color = NA, alpha = 0.7) +
#   # Plot 1600m rings (medium-dark blue)
#   geom_sf(data = tracts_within_1600m_chicago, aes(geometry = geom),
#           fill = "#072f5f", color = NA, alpha = 0.7) +
#   # Plot 800m rings (darkest blue)
#   geom_sf(data = tracts_within_800m, aes(geometry = geom),
#           fill = "#05204a", color = NA, alpha = 0.7) +
#   # Plot treated tracts on top of the rings (dark red)
#   geom_sf(data = treated_tracts_chicago, aes(geometry = geom),
#           fill = "#990000", color = "black", size = 0.2, alpha = 0.9) +
#   # Customize the appearance
#   theme_minimal() +
#   labs(
#     title = "Chicago: Treated Tracts and Surrounding Rings",
#     subtitle = "Dark Red = Treated Tracts, Shades of Blue = Distance Rings (Lighter = Further)",
#     caption = "Source: Census Tract Data"
#   ) +
#   theme(legend.position = "none")
# 
