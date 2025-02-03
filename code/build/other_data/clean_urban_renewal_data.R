# 

#

## Preliminaries -----
library(sf)
library(here)
library(tidyverse)


# set directory
ur_directory_raw <- here("data", "raw", "Renewing_Inequality_Data-master", "Data", "shapefile")
census_directory <- here("data", "derived", "census")
output_dir <- here("data", "derived", "urban_renewal")

ur_data_raw <- read_sf(here(ur_directory_raw, "ur_projects.shp"))

# read 1990 Census tract data
census_tracts_raw <- st_read(here(census_directory, "tract_population_data.gpkg"))


invalid_geometries <- ur_data_raw %>% filter(!st_is_valid(.))


# convert UR data to same projection as Census data
ur_data <- 
  ur_data_raw %>% 
  # correct any invalid geometries
  st_make_valid() %>% 
  st_transform(st_crs(census_tracts_raw)) 


ur_data_union <-
  ur_data %>% 
  # union overlapping urban renwal polygons into 1
  st_union()

# Keep only one of each unique 1990 census tract ----
census_tracts <- 
  census_tracts_raw %>% 
  group_by(STATEA, COUNTYA, TRACTA) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  # correct invalid geometries
  st_make_valid() %>% 
  # keep only the columns we need
  select(STATEA, COUNTYA, TRACTA) %>% 
  mutate(original_tract_area = st_area(geom))


## Intersect UR data with Census tracts, get area share -----
# turn off spherical geometry, which seems to muck up the intersection
sf_use_s2(FALSE)

# intersect HOLC and Census tracts
tract_ur_intersections <- 
  # intersect the HOLC data with the census tracts: Results in multiple rows per tract
  st_intersection(census_tracts, ur_data_union) %>% 
  mutate(intersection_area = as.numeric(st_area(geom))) %>%  # Area of the intersection
  st_drop_geometry() %>%
  # sum up total intersection area by tract
  group_by(STATEA, COUNTYA, TRACTA) %>%
  summarize(
    total_intersection_area = sum(intersection_area, na.rm = TRUE)
  )

# Calculate urban renewal share
tract_ur_summary <-
  left_join(census_tracts, tract_ur_intersections, by = c("STATEA", "COUNTYA", "TRACTA")) %>% 
  st_drop_geometry() %>% 
  # calculate share of the tract in urban renewal areas
  mutate(
    total_intersection_area = as.numeric(ifelse(is.na(total_intersection_area), 0, total_intersection_area)),
    total_ur_share = as.numeric(total_intersection_area / original_tract_area)
  ) %>% 
  mutate(ur_binary_80pp = ifelse(as.numeric(total_ur_share) >= 0.8, 1, 0),
         ur_binary_70pp = ifelse(as.numeric(total_ur_share) >= 0.7, 1, 0),
         ur_binary_50pp = ifelse(as.numeric(total_ur_share) >= 0.5, 1, 0),
         ur_binary_25pp = ifelse(as.numeric(total_ur_share) >= 0.25, 1, 0),
         ur_binary_10pp = ifelse(as.numeric(total_ur_share) >= 0.1, 1, 0),
         ur_binary_5pp = ifelse(as.numeric(total_ur_share) >= 0.05, 1, 0),
         ur_binary_any = ifelse(as.numeric(total_ur_share) > 0, 1, 0)
         ) %>% 
  select(STATEA, COUNTYA, TRACTA, contains("ur_binary"), total_ur_share)
  
# output
tract_ur_summary %>% 
  write_csv(here(output_dir, "tract_urban_renewal_classifications.csv"))
  






