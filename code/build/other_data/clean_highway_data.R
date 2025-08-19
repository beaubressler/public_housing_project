# calculate_tract_highway_proximity.R
# Simple script to calculate highway proximity for census tracts

library(sf)
library(dplyr)
library(here)

# Load census tracts (this is a panel, so take unique tracts only)
census_directory <- here("data", "derived", "census")
census_tracts_raw <- st_read(here(census_directory, "tract_population_data.gpkg"))

# Get unique tracts only (remove year dimension)
census_tracts_unique <- census_tracts_raw %>%
  group_by(GISJOIN_1950) %>%
  slice(1) %>%
  ungroup()

# Load highways  
highways_raw <- st_read(here("data", "raw", "highways", "weiwu_2024", "highways_actual.shp"))

# Transform both to same projection for distance calculations
census_tracts <- st_transform(census_tracts_unique, crs = 5070)  # US Albers Equal Area
highways <- st_transform(highways_raw, crs = 5070)

# Calculate tract centroids
tract_centroids <- st_centroid(census_tracts)

# Calculate distance from each tract to nearest highway
distances <- st_distance(tract_centroids, highways)
min_distances_m <- apply(distances, 1, min)
min_distances_km <- as.numeric(min_distances_m) / 1000

# Create highway proximity data (just GISJOIN_1950 + highway variables)
highway_proximity_data <- data.frame(
  GISJOIN_1950 = census_tracts$GISJOIN_1950,
  distance_to_highway_km = min_distances_km,
  distance_to_highway_miles = min_distances_km * 0.621371,
  has_highway_1km = min_distances_km <= 1,
  has_highway_2km = min_distances_km <= 2, 
  has_highway_1mile = min_distances_km * 0.621371 <= 1,
  has_highway_2miles = min_distances_km * 0.621371 <= 2
)

# Save just the highway proximity lookup table
output_path <- here("data", "derived", "highways", "tract_data_with_highway_proximity.csv") 
write_csv(highway_proximity_data, output_path)

cat("Highway proximity calculated for", nrow(highway_proximity_data), "tracts\n")
cat("Saved to:", output_path, "\n")
cat("Summary:\n")
cat("- Within 1km:", sum(highway_proximity_data$has_highway_1km), "tracts\n")
cat("- Within 1 mile:", sum(highway_proximity_data$has_highway_1mile), "tracts\n")