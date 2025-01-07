#####
# Test land value ring regressions
# Estimate at the lnad-value grid level... so this requires setting up this data to do distance rings and all of that

# Steps: (non-trivial)
# 1. Read in public housing data and land value data
# 2. 


#####

library(tidyverse)
library(haven)
library(sf)

# read in public housing data
public_housing_sample <- 
  read_sf("data/derived/public_housing/working/cdd_housing_project_sample.gpkg") %>% 
  filter(locality == "CHICAGO")

chicago_digitized_sample <- 
  read_csv("data/derived/public_housing/intermediate/digitized/chicago_1973_geocoded.csv")

chicago_digitized_sample <- 
  st_as_sf(chicago_digitized_sample, coords = c("long", "lat"), crs = 4326)


# read in chicago land-value data 
chicago_land_value_data <- read_dta("data/raw/ahlfeldt_mcmillen/GRID_LONG.dta")
# geocode it
chicago_land_value_data_geo <- st_as_sf(chicago_land_value_data, coords = c("longitude", "latitude"), crs = 4326)



# plot chicago land value data on map of chicago
ggplot(chicago_land_value_data_geo %>% filter(Year == 1949)) +
  geom_sf(aes(color = llv), size = 0.2) +
  geom_sf(data = chicago_digitized_sample, aes(geometry = geometry), color = "black", size = 1) +
  scale_color_gradient(low = "blue", high = "red", name = "Log Land Value") +
  labs(title = "Chicago Land Values, 1949",
       x = "Longitude",
       y = "Latitude",
       color = "Land Value")



## Merge land value data with ring event study data
