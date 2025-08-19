###
# Create data for spatial difference-in-differences regressions

####


####
# Spatial "inner and outer ring" DiD/event studies

# 1. Create inner ring and outer rings
# 2. 
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



# for event study
library(fixest)
library(ggfixest)
library(did2s)

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
event_study_rings_output_path <- here(merged_data_dir, "event_study_data_rings.csv")
unique_tracts_and_rings_output_path <- here(merged_data_dir, "unique_tracts_and_rings.csv")


# Read in and prep data ----

# read treated_tracts_panel.gpkg
treated_tracts_panel <-
  st_read(treated_tracts_panel_path) %>% 
  st_drop_geometry() 

# read in Census tract sample with treatment status
census_tract_sample <-
  st_read(census_tract_sample_path) 

# Create a dataframe with the total number of public housing units per tract
total_ph_units_per_tract <-
  treated_tracts_panel %>%
  dplyr::rename_at(vars(contains("totunitsplanned")), ~"total_public_housing_units") %>%
  select(TRACTA, COUNTY, STATE, total_public_housing_units) %>% 
  distinct()


# Create, in each year, the treated, inner, and outer rings ----
# all neighbors among sample tracts

# 0. Get all unique tracts in the sample
census_tract_sample_indexed_unique <-
  census_tract_sample %>%
  group_by(TRACTA, COUNTYA, STATEA) %>%
  filter(row_number() == 1) %>% 
  mutate(tract_id = row_number())

# get crosswalk to all unique census tracts
census_tract_unique_crosswalk <-
  census_tract_sample_indexed_unique %>% 
  mutate(row_number = row_number()) %>%
  select(row_number,tract_id, TRACTA, COUNTY, STATE) %>% 
  st_drop_geometry()

# Ensure geometries are valid
census_tract_sample_indexed_unique <-
  st_make_valid(census_tract_sample_indexed_unique)

# Find neighbors
neighbors <- st_touches(census_tract_sample_indexed_unique)

# Create neighbor dataframe more efficiently
neighbor_df <- tibble(
  from = rep(seq_along(neighbors), lengths(neighbors)),
  to = unlist(neighbors),
  neighbor = 1
)

# Join with crosswalk data
tract_neighbors <- neighbor_df %>%
  left_join(
    census_tract_unique_crosswalk %>% 
      select(row_number, TRACTA, COUNTY, STATE) %>%
      rename(from = row_number),
    by = "from"
  ) %>%
  rename(
    TRACTA_from = TRACTA,
    COUNTY_from = COUNTY,
    STATE_from = STATE
  )

# drop neighbors from memory



## Alternative inner and outer rings method. ----


# identify ever treated tracts
ever_treated_tracts <- 
  census_tract_sample %>%
  filter(treated == 1 ) %>% 
  st_drop_geometry() %>% 
  select(TRACTA, COUNTY, STATE) %>%
  distinct() %>% 
  mutate(ever_treated = 1)

# merge to census tract sample
census_tract_sample_indexed_unique <-
  census_tract_sample_indexed_unique %>%
  left_join(ever_treated_tracts, by = c("TRACTA", "COUNTY", "STATE")) %>%
  mutate(ever_treated = ifelse(is.na(ever_treated), 0, 1))


# create neighbor list
nb <- poly2nb(census_tract_sample_indexed_unique)

# create higher-order neighbor list
nblags <- nblag(nb, maxlag = 2)

# Convert tracts to data.table

tracts_dt <-
   data.table(
    tract_index = 1:nrow(census_tract_sample_indexed_unique),
    TRACTA = census_tract_sample_indexed_unique$TRACTA,
    COUNTY = census_tract_sample_indexed_unique$COUNTYA,
    STATE = census_tract_sample_indexed_unique$STATEA,
    ever_treated = census_tract_sample_indexed_unique$ever_treated
  )

# Get indices of treated tracts
treated_indices <- tracts_dt[ever_treated == 1, tract_index]

# Initialize results data.table
results <- data.table()

for (i in treated_indices) {
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

# Merge to get neighbor tract information
results <- merge(results, tracts_dt, by.x = "neighbor_id", by.y = "tract_index", all.x = TRUE)

# Get treated tract information
treated_info <- tracts_dt[tract_index %in% treated_indices, .(
  treated_id = tract_index,
  TRACTA_treated = TRACTA,
  COUNTY_treated = COUNTY,
  STATE_treated = STATE
)]

# Construct the event study data
event_study_data <- merge(results, treated_info, by = "treated_id", all.x = TRUE)

# Map 'treated_id' to 'TRACTA'
treated_ids_map <- tracts_dt[, .(treated_id = tract_index, TRACTA)]

# Merge 'YEAR' information
# TODO: Error here
treated_tracts_unique <- unique(treated_tracts_panel[, .(TRACTA, YEAR)])

treated_years <- merge(
  treated_tracts_unique[, .(TRACTA, YEAR)],
  treated_ids_map,
  by = 'TRACTA',
  all.x = TRUE
)

# Merge 'YEAR' into event_study_data
event_study_data <- merge(event_study_data, treated_years[, .(treated_id, YEAR)], by = 'treated_id', allow.cartesian = TRUE)

# Now 'event_study_data' contains:
# - 'treated_id': index of the treated tract
# - 'neighbor_id': index of the neighbor tract (inner or outer ring)
# - 'ring': 1 for inner ring, 2 for outer ring
# - 'TRACTA', 'COUNTY', 'STATE': identifiers for the neighbor tract
# - 'TRACTA_treated', 'COUNTY_treated', 'STATE_treated': identifiers for the treated tract
# - 'YEAR': the year(s) associated with the treated tract


## Get neighbors of treated tracts (inner ring) and their neighbors (outer ring)-----
# There are two versions of this:
# 1. Time series datasets of all treated tracts, inner rings, outer rings
# 2. Stacked dataset with inner and outer rings linked to treated tracts directly
#    This would be for the econometrics

#  To do this:
# 1. Join tract_neighbors to treated_tracts_panel by tract information. This should
#    get, for each treated tract, the row_number in census_tract_sample_indexed_unique
#.  of all of it's neighbors
# 2. Filter to only those where neighbor == 1
# 3. Merge on the tract information of the "FROM"

inner_rings_panel <- 
  # Merge treated tracts time series onto tract_neighbors by tract information
  left_join(treated_tracts_panel,
            tract_neighbors,
            by = c("TRACTA" = "TRACTA_from", "COUNTY" = "COUNTY_from",
                   "STATE" = "STATE_from")) %>% 
  # 2. Filter to only those where neighbor == 1
  filter(neighbor == 1)  %>% 
  select(from, to, YEAR) %>%
  st_drop_geometry() %>%
  # 3. Merge on the tract information of the inner ring
  left_join(census_tract_unique_crosswalk, by = c("to" = "row_number")) %>% 
  dplyr::rename(TRACT_to = TRACTA, COUNTY_to = COUNTY,
                STATE_to = STATE) %>%
  mutate(inner_ring = 1) %>% 
  st_drop_geometry() %>% 
  distinct()

# 1. take neighbors with from in the list of "to" (in inner rings). 
# This will get the list of neighbors for all of the inner ring tracts, in all years
# 1. get distinct list
# 2. right_join onto the inner rings to get year pairs from (in outer_ring) to to (in inner_ring)
# 3. Merge on geographic information via the crosswalk
outer_rings_panel <-  
  tract_neighbors %>% 
  st_drop_geometry() %>% 
  filter(from %in% unique(inner_rings_panel$to)) %>% 
  filter(neighbor == 1) %>% 
  distinct() %>%
  right_join(inner_rings_panel, by = c("from" = "to")) %>% 
  select(YEAR, from, to) %>% 
  left_join(census_tract_unique_crosswalk, by = c("to" = "row_number")) %>% 
  mutate(outer_ring = 1) %>%
  st_drop_geometry() %>% 
  distinct()

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
  select(STATE, COUNTY, TRACTA, city, cluster) %>% 
  distinct() 


# Set up stacked, spatial DiD dataset  ----

### 1. Get treated tracts, inner ring tracts, outer ring tracts in a wide dataset ---- 

# # Convert to data.table format
# setDT(treated_tracts_panel)
# setDT(tract_neighbors)
# setDT(census_tract_unique_crosswalk)
# setDT(outer_rings_panel)
# 
# # Merge treated, inner rings, and outer rings, wide, using dt
# # Step 1: Merge treated tracts time series onto tract_neighbors by tract information
# event_study_data <- merge(treated_tracts_panel, tract_neighbors, 
#                           by.x = c("TRACTA", "COUNTY", "STATE"), 
#                           by.y = c("TRACTA_from", "COUNTY_from", "STATE_from"), 
#                           all.x = TRUE, allow.cartesian = TRUE)
# 
# # Step 2: Remove the 'neighbor' column and rename columns
# event_study_data[, neighbor := NULL]  # Remove column
# setnames(event_study_data, old = c("from", "to", "TRACTA", "COUNTY", "STATE"), 
#          new = c("treated_id", "inner_ring_id", "TRACTA_treated", "COUNTY_treated", "STATE_treated"))
# 
# # Step 3: Merge on the tract information of the inner ring
# event_study_data <- merge(event_study_data, census_tract_unique_crosswalk, 
#                           by.x = "inner_ring_id", 
#                           by.y = "row_number", 
#                           all.x = TRUE, allow.cartesian = TRUE)
# 
# # Step 4: Rename columns after the merge
# setnames(event_study_data, old = c("TRACTA", "COUNTY", "STATE"), 
#          new = c("TRACTA_inner", "COUNTY_inner", "STATE_inner"))
# 
# # Step 5: Select relevant columns by removing unnecessary ones
# event_study_data[, tract_id := NULL]  # Drop the 'tract_id' column
# 
# # Step 6: Merge with outer rings panel
# event_study_data <- merge(event_study_data, outer_rings_panel, 
#                           by.x = c("inner_ring_id", "YEAR"), 
#                           by.y = c("from", "YEAR"), 
#                           all.x = TRUE, allow.cartesian = TRUE)
# 
# # Step 7: Rename outer ring columns
# setnames(event_study_data, old = c("to", "TRACTA", "COUNTY", "STATE"), 
#          new = c("outer_ring_id", "TRACTA_outer", "COUNTY_outer", "STATE_outer"))
# 
# # Step 8: Remove unnecessary columns and filter out rows where treated_id is NA
# event_study_data <- event_study_data[!is.na(treated_id), .(treated_id, inner_ring_id, outer_ring_id, 
#                                                            TRACTA_treated, COUNTY_treated, STATE_treated,
#                                                            TRACTA_inner, COUNTY_inner, STATE_inner,
#                                                            TRACTA_outer, COUNTY_outer, STATE_outer)]


# Treated, inner rings, and outer rings, wide
event_study_data <-
  # Merge treated tracts time series onto tract_neighbors by tract information
  left_join(treated_tracts_panel,
            tract_neighbors,
            by = c("TRACTA" = "TRACTA_from", "COUNTY" = "COUNTY_from",
                   "STATE" = "STATE_from")) %>% 
  select(-neighbor) %>% 
  # rename columns
  dplyr::rename(treated_id = from, inner_ring_id = to,
                TRACTA_treated = TRACTA, COUNTY_treated = COUNTY,
                STATE_treated = STATE) %>%
  # 3. Merge on the tract information of the inner ring
  left_join(census_tract_unique_crosswalk, by = c("inner_ring_id" = "row_number")) %>% 
  dplyr::rename(TRACTA_inner = TRACTA, COUNTY_inner = COUNTY,
                STATE_inner = STATE) %>%
  select(-tract_id) %>% 
  left_join(outer_rings_panel,
            by = c("inner_ring_id" = "from", "YEAR")) %>% 
  # rename variables
  dplyr::rename(outer_ring_id = to, TRACTA_outer = TRACTA, COUNTY_outer = COUNTY,
                STATE_outer = STATE) %>% 
  select(-outer_ring, -tract_id) %>% 
  filter(!is.na(treated_id))

### 2. Convert to long and ensure clean comparisons ----
# I want the treatment-control to be clean:
# 1. Keep only never-treated observations are in the inner and outer rings
# 2. If a tract appears in the inner ring, it should not appear in the outer ring

# Create treated tracts dataset
treated_tracts <- 
  event_study_data %>%
  select(YEAR, treated_id, STATE_treated, COUNTY_treated, TRACTA_treated) %>%
  distinct() %>%
  mutate(location_type = "treated")

# Create inner ring dataset
inner_ring_tracts <- event_study_data %>%
  select(YEAR, treated_id, inner_ring_id, STATE_inner, COUNTY_inner, TRACTA_inner) %>%
  distinct() %>%
  rename(ring_id = inner_ring_id,
         STATE = STATE_inner, COUNTY = COUNTY_inner, TRACTA = TRACTA_inner) %>%
  mutate(location_type = "inner")

# Create outer ring dataset
outer_ring_tracts <- event_study_data %>%
  select(YEAR, treated_id, outer_ring_id, STATE_outer, COUNTY_outer, TRACTA_outer) %>%
  distinct() %>%
  rename(ring_id = outer_ring_id,
         STATE = STATE_outer, COUNTY = COUNTY_outer, TRACTA = TRACTA_outer) %>%
  mutate(location_type = "outer")

# Combine datasets
combined_ring_data <- bind_rows(
  treated_tracts %>%
    mutate(ring_id = treated_id, STATE = STATE_treated, 
           COUNTY = COUNTY_treated, TRACTA = TRACTA_treated) %>%
    select(-ends_with("_treated")),
  inner_ring_tracts) %>% 
  bind_rows(outer_ring_tracts)

# Identify ever-treated tracts
ever_treated_tracts <- treated_tracts %>%
  select(STATE_treated, COUNTY_treated, TRACTA_treated) %>%
  distinct() %>%
  rename(STATE = STATE_treated, COUNTY = COUNTY_treated, TRACTA = TRACTA_treated) %>%
  mutate(ever_treated = 1)

# Clean the data
event_study_data_long <- combined_ring_data %>%
  left_join(ever_treated_tracts, by = c("STATE", "COUNTY", "TRACTA")) %>% 
  # filter ever treated tracts from inner and outer rings
  filter(is.na(ever_treated) | location_type == "treated") 

# Identify ever inner ring tracts among these never treated tracts
ever_inner_ring_tracts <- 
  event_study_data_long %>%
  filter(location_type == "inner") %>%
  select(STATE, COUNTY, TRACTA) %>%
  distinct() %>%
  mutate(ever_inner_ring = 1)

# filter ever inner ring tracts from outer rings
event_study_data_long <-
  event_study_data_long %>%
  left_join(ever_inner_ring_tracts, by = c("STATE", "COUNTY", "TRACTA")) %>%
  filter(is.na(ever_inner_ring) | location_type == "inner" | location_type == "treated") %>% 
  # drop ever treated and ever inner ring columns
  select(-ever_treated, -ever_inner_ring)
  
# merge on treatment year
treatment_years <-
  event_study_data %>% 
  select(treated_id, treatment_year) %>%
  distinct()

event_study_data_long <- 
  event_study_data_long %>%
  left_join(treatment_years, by = c("treated_id" = "treated_id"))
            

### Include 1930 data for all of the tracts for the event study ----
tracts_and_rings <-
  event_study_data_long %>%
  select(TRACTA, COUNTY, STATE, TRACTA, 
         treatment_year, treated_id, location_type) %>%
  distinct()

year <- c(1930, 1940, 1950, 1960, 1970, 1980, 1990)

# all combinations of x and y 
event_study_data_full <-
  expand_grid(tracts_and_rings, year) 

## 3. Merge on census data ----

# Merge on total public housing units per tract in first treated deacde
event_study_long <- 
  event_study_data_full %>% 
  # # merge on total units in each year
  left_join(total_ph_units_per_tract) %>%
  # fill by year and treated_id
  group_by(year, treated_id) %>%
  fill(total_public_housing_units) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(total_public_housing_units = ifelse(is.na(total_public_housing_units), 0, total_public_housing_units))

# keep relevant variables, transform vars
census_data_for_event_study <-
  census_tract_sample %>% 
  select(STATE, COUNTY, TRACTA, YEAR, city,
         black_share, white_share, white_pop, black_pop, total_pop, 
         median_income, median_rent, median_home_value, median_educ_years_25plus,
         pct_hs_grad, pct_some_college,
         population_density, distance_from_cbd, housing_density,
         employment_pop_ratio, unemp_rate, lfp_rate) %>%
  st_drop_geometry() %>% 
  dplyr::rename(year = YEAR) %>% 
  # inverse hyperbolic sine transformation of populations and medians
  mutate(asinh_pop_total = asinh(total_pop),
         asinh_pop_white = asinh(white_pop),
         asinh_pop_black = asinh(black_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent = asinh(median_rent),
         asinh_median_home_value = asinh(median_home_value)) %>% 
  # join on clusters
  left_join(clustered_tracts_panel)

# merge on census variables
event_study_final <-
  left_join(event_study_long, census_data_for_event_study) %>% 
  # define treated variable equal to 1 if year >= treatment year and location_type == treated
  mutate(treated = ifelse(year >= treatment_year & location_type == "treated", TRUE, FALSE)) %>%
  # create variables for sun abraham estimates
  mutate(cohort = as.factor(year), 
         relative_period = (year - treatment_year)/10) %>% 
  # drop any tracts with 0 population
  filter(total_pop > 0) %>%
  # keep distinct observations: One observation per treated_id-tract-year
  distinct()

# create ring variable for ring fixed effects
event_study_data_rings <- 
  event_study_final %>%
  mutate(event_time = as_factor(year - treatment_year)) %>%
  mutate(ring = as.factor(case_when(location_type == "treated" ~ 0,
                                    location_type == "inner" ~ 1,
                                    location_type == "outer" ~ 2)),
         location_type_factor = factor(location_type, levels = c("outer", "treated", "inner")),
         location_type_dummy = as.numeric(location_type_factor)) %>% 
  # create a unique tract variable which is COUNTY + TRACTA
  mutate(tract_id = paste0(COUNTY, TRACTA))

#### calculate and merge on baseline black share as a control ------
baseline_black_share <-
  event_study_data_rings %>%
  filter(event_time == -10) %>% 
  select(treated_id, STATE, COUNTY, TRACTA, black_share) %>% 
  dplyr::rename(black_share_baseline = black_share) %>% 
  distinct()

# merge on by treated project
event_study_data_rings <-
  event_study_data_rings %>%
  left_join(baseline_black_share, by = c("treated_id","STATE", "COUNTY", "TRACTA"))

### 4. output event study data with rings -----
write_csv(event_study_data_rings, event_study_rings_output_path)

# output unique tracts and rings status
unique_tracts_and_rings <- 
  event_study_data_rings %>% 
  select(STATE, COUNTY, TRACTA, location_type, treatment_year) %>% 
  distinct() %>% 
  # keep first year of treatment for each tract (some inner and outer rings are affiliated with multiple treated tracts)
  group_by(STATE, COUNTY, TRACTA, location_type) %>%
  filter(treatment_year == min(treatment_year))
  
write_csv(unique_tracts_and_rings, unique_tracts_and_rings_output_path)


# Create, in each year, 800, 1600, 2400, and 3200m rings based on centroid distance ----

# unique treated tracts, centroids
treated_tracts_centroids <-
  census_tract_sample %>% 
  filter(treated == 1) %>% 
  select(STATE, COUNTY, TRACTA) %>% 
  distinct() %>% 
  st_centroid()

# all non-treated tracts
never_treated_tracts <- 
  census_tract_sample %>% 
  st_drop_geometry() %>% 
  select(STATE, COUNTY, TRACTA) %>% 
  distinct() %>% 
  anti_join(treated_tracts_centroids, by = c("STATE", "COUNTY", "TRACTA"))

never_treated_tract_centroids <- 
  census_tract_sample_indexed_unique %>% 
  select(STATE, COUNTY, TRACTA) %>% 
  semi_join(never_treated_tracts, by = c("STATE", "COUNTY", "TRACTA")) %>%
  st_centroid()

# Now, you can use `within_distance_1600m` and `within_distance_3200m` for your analysis.
# For example, you could filter tracts within the 1600m or 3200m rings:

## Create stacked dataset ----

# 1. Create an empty list to store results
stacked_rings_data <- list()

# Step 2: Loop over each treated tract and find which tracts fall into its rings
for (i in seq_len(nrow(treated_tracts_centroids))) {
  
  # Get the current treated tract centroid
  treated_tract <- treated_tracts_centroids[i, ]
  
  # Calculate distance between the current treated tract and all non-treated tracts
  distances <- st_distance(never_treated_tract_centroids, treated_tract)
  
  # Convert thresholds (800m, 1600m, etc.) to units of meters
  threshold_800m <- set_units(800, "m")
  threshold_1600m <- set_units(1600, "m")
  threshold_2400m <- set_units(2400, "m")
  threshold_3200m <- set_units(3200, "m")
  
  
  # Identify tracts within the specified distance rings
  tracts_in_800m <- never_treated_tract_centroids[distances <= threshold_800m, ]
  tracts_in_1600m <- never_treated_tract_centroids[distances > threshold_800m & distances <= threshold_1600m, ]
  tracts_in_2400m <- never_treated_tract_centroids[distances > threshold_1600m & distances <= threshold_2400m, ]
  tracts_in_3200m <- never_treated_tract_centroids[distances > threshold_2400m & distances <= threshold_3200m, ]
  
  # Label the rings for each group of tracts
  tracts_in_800m$ring <- "800m"
  tracts_in_1600m$ring <- "1600m"
  tracts_in_2400m$ring <- "2400m"
  tracts_in_3200m$ring <- "3200m"
  
  # Combine all tracts for this treated tract
  tracts_for_treated <- bind_rows(tracts_in_800m, tracts_in_1600m, tracts_in_2400m, tracts_in_3200m)
  
  # Add treated tract information (i.e., identifier for the treated tract)
  tracts_for_treated$treated_tract_id <- treated_tract$TRACTA
  
  # Add to the list of results
  stacked_rings_data[[i]] <- tracts_for_treated
}

# Step 3: Combine the results into a single stacked dataset
stacked_rings_dataset <- bind_rows(stacked_rings_data)

# Clean up the dataset, keeping relevant columns
stacked_rings_dataset <-
  stacked_rings_dataset %>%
  select(STATE, COUNTY, TRACTA, treated_tract_id, ring) %>%
  st_drop_geometry() %>% 
  distinct()  # Ensure there are no duplicates

# output the stacked dataset
write_csv(stacked_rings_dataset, here(merged_data_dir, "stacked_distance_rings_dataset.csv"))


## Create a map for fun -----
# merge on geographies
x <-
  stacked_rings_dataset %>% 
  left_join(census_tract_sample_indexed_unique %>% select(city, STATE, COUNTY, TRACTA, geom),  by = c("STATE", "COUNTY", "TRACTA"))

chicago_rings <-
  x %>% 
  filter(city == "Chicago") 

# Step 1: Filter data for Chicago
chicago_tracts <- census_tract_sample %>%
  filter(city == "Chicago") %>%   # Ensure there is a 'city' or similar variable indicating Chicago
  # keep unique
  group_by(STATE, COUNTY, TRACTA) %>%
  distinct() %>% 
  ungroup()

# Step 2: Define layers for treated tracts, 1600m rings, and 3200m rings
treated_tracts_chicago <- census_tract_sample %>%
  filter(city == "Chicago", treated == 1) %>% 
  group_by(STATE, COUNTY, TRACTA) %>%
  filter(row_number() == 1)

tracts_within_800m <- 
  chicago_rings %>%
  filter(ring == "800m")  %>% # Tracts within 800 meters of treated tracts
  group_by(STATE, COUNTY, TRACTA) %>%
  filter(row_number() == 1)

tracts_within_1600m_chicago <-
  chicago_rings %>%
  filter(ring == "1600m") %>%  # Tracts within 1600 meters of treated tracts
  group_by(STATE, COUNTY, TRACTA) %>%
  filter(row_number() == 1)

tracts_within_2400m_chicago <-
  chicago_rings %>%
  filter(ring == "2400m")  %>% # Tracts within 2400 meters of treated tracts
  group_by(STATE, COUNTY, TRACTA) %>%
  filter(row_number() == 1)

tracts_within_3200m_chicago <-
  chicago_rings %>%
  filter(ring == "3200m") %>%  # Tracts within 3200 meters of treated tracts
  group_by(STATE, COUNTY, TRACTA) %>%
  filter(row_number() == 1)

# Step 3: Create the map using ggplot2
# Create the map using ggplot2
ggplot() +
  # Plot all Chicago tracts as the base layer (very light grey)
  geom_sf(data = chicago_tracts, aes(geometry = geom), fill = "lightgray", color = "darkgrey", size = 0.2) +
  # Plot 3200m rings first (lightest blue)
  geom_sf(data = tracts_within_3200m_chicago, aes(geometry = geom),
          fill = "#3895d3", color = NA, alpha = 0.7) +
  # Plot 2400m rings (light-medium blue)
  geom_sf(data = tracts_within_2400m_chicago, aes(geometry = geom),
          fill = "#1261a0", color = NA, alpha = 0.7) +
  # Plot 1600m rings (medium-dark blue)
  geom_sf(data = tracts_within_1600m_chicago, aes(geometry = geom),
          fill = "#072f5f", color = NA, alpha = 0.7) +
  # Plot 800m rings (darkest blue)
  geom_sf(data = tracts_within_800m, aes(geometry = geom),
          fill = "#05204a", color = NA, alpha = 0.7) +
  # Plot treated tracts on top of the rings (dark red)
  geom_sf(data = treated_tracts_chicago, aes(geometry = geom),
          fill = "#990000", color = "black", size = 0.2, alpha = 0.9) +
  # Customize the appearance
  theme_minimal() +
  labs(
    title = "Chicago: Treated Tracts and Surrounding Rings",
    subtitle = "Dark Red = Treated Tracts, Shades of Blue = Distance Rings (Lighter = Further)",
    caption = "Source: Census Tract Data"
  ) +
  theme(legend.position = "none")

