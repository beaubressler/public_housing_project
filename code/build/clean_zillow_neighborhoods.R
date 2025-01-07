library(sf)
library(tidyverse)

zillow_neighborhoods <- st_read("data/raw/Zillow_Neighborhoods/ZillowNeighborhoods.gdb.zip")
census_tracts <- st_read("data/derived/census/tract_population_data.gpkg")


if (st_crs(zillow_neighborhoods) != st_crs(census_tracts)) {
  zillow_neighborhoods <- st_transform(zillow_neighborhoods, st_crs(census_tracts))
}

# Perform spatial join
tracts_with_neighborhoods <- st_intersection(census_tracts, zillow_neighborhoods)

# If a tract intersects multiple neighborhoods, choose the one with the largest overlap
tracts_with_neighborhoods <- tracts_with_neighborhoods %>%
  # calculate the area of the intersection
  mutate(intersection_area = st_area(geom)) %>% 
  group_by(STATEA, COUNTYA, TRACTA) %>% 
  # keep only the row with the largest intersection area
  filter(intersection_area == max(intersection_area)) %>% 
  ungroup() %>%
  select(-intersection_area)


# output
tracts_with_neighborhoods_output <- 
  tracts_with_neighborhoods %>% 
  st_drop_geometry() %>% 
  select(STATEA, COUNTYA, TRACTA, Name) %>% 
  dplyr::rename(neighborhood = Name) %>%
  distinct() %>%
  write_csv("data/derived/zillow/tracts_with_zillow_neighborhoods.csv")

# neighborhoods_per_city <-
#   tracts_with_neighborhoods %>%
#   st_drop_geometry() %>% 
#   group_by(COUNTY, STATE) %>%
#   select(City, Name) %>% 
#   distinct() %>% 
#   summarise(n_neighborhoods = n()) 


