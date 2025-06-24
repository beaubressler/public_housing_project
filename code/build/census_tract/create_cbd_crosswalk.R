###
# Create crosswalk for central business districts, which come from 1980 Census
###
library(tidyverse)
library(here)
library(ipumsr)

# Preliminaries -----
geographic_crosswalk_dir <- here("data", "derived", "geographic_crosswalks")
census_data_dir <- here("data", "derived", "census")

# Read in relevant data----

# 1980 population data: This is used to get a list of relevant tracts 
tract_data_1980_1 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1980/nhgis0027_ds104_1980_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1980/US_tract_1980.shp",
                  file_select = starts_with("US_tract_1980")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

# Tract crosswalk to 1950
tract_crosswalk_1980 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1980_to_1950.csv"))


# Get central business district indicator ---- 
# This appears only in 1980
cbd_tracts <- 
  tract_data_1980_1 %>% 
  filter(!is.na(CBD))

# link to 1950 
cbd_tracts_1950 <- 
  cbd_tracts %>% 
  left_join(tract_crosswalk_1980, by = c("GISJOIN" = "GISJOIN_1980")) %>% 
  filter(!is.na(GISJOIN_1950)) %>% 
  # select GISJOIN_1950
  select(GISJOIN_1950) %>%
  mutate(cbd = 1) %>% 
  st_drop_geometry() %>% 
  distinct()


# Save ----
write_csv(cbd_tracts_1950, here(census_data_dir, "cbd_tracts_1950_concorded.csv"))
