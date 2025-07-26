###
#
# Run this script after geolocating the full count addresses in ESRI Streetmap Premium
# What does it do?
# 1. Merge onto ED maps? Make sure the location is in the right ED?
# 2. Merge onto the 1940 geographic reference file?

###

# Preliminaries -----

library(tidyverse)
library(sf)
library(here)

# Directories
#
grf_dir <- here("data", "derived", "geographic_reference_file")

# this is where I store geolocate addresses from SMP
esri_geolocated_dir <- here(grf_dir, "geolocated", "esri") 
# this is the output
working_geolocated_dir <- here(grf_dir, "geolocated", "working")

# Ed geographies
ed_dir <- here("data", "derived", "geographic_crosswalks", "ed_to_tract")

# Read in geolocated addresses
# 1940 unique addresses, post geocoding
grf_1940_geolocated_esri <- read_csv(here(esri_geolocated_dir, "grf_unique_addresses_1940_Geocoded_ExportTable.csv"))

# 1940 histid with enumeration district
grf_1940_histid_ed_city <-
  read_csv(here(grf_dir, "grf_1940_histid_ed_city.csv"))

# 1940 histid with addresses
grf_1940_histid_street <-
  read_csv(here(grf_dir, "grf_addresses_1940.csv"))

# ED maps 
ed_maps_1940 <- st_read(here(grf_dir, "ed_maps", "1940_ed_maps.shp"))

# Rules for keeping the match -----
## 1. Keep match score > 85 (as in Valenzuela-Casasempere 2025)
grf_1940_geolocated_clean <-
  grf_1940_geolocated_esri %>% 
  filter(Score >= 85)

# 2. Exclude ties (these would be things where it's not obvious whether is the North or South version of the street)
grf_1940_geolocated_clean_exact <-
  grf_1940_geolocated_clean %>% 
  filter(Status != "T") 

# 3. same city? This will drop many merges...
# Some of these seem wrong, some of them seem like it could be a suburb that became a different city
grf_1940_geolocated_clean_exact <-
  grf_1940_geolocated_clean_exact %>% 
  mutate(USER_city = str_extract(USER_full_street_address, "(?<=, )[^,]+(?=,)"))

# keep only the columns we need
grf_1940_geolocated_clean_final <-
  grf_1940_geolocated_clean_exact %>% 
  select(Score, Match_addr, X, Y, USER_city, City, USER_full_street_address)

x <- 
  grf_1940_geolocated_clean_final %>% 
  filter(USER_city == City)
  

# Merge back to GRF using full street address (?)
grf_1940_geolocated_full <- 
  grf_1940_histid_street %>% 
  left_join(grf_1940_geolocated_clean_final, by = c("full_street_address" = "USER_full_street_address"))

grf_1940_geolocated_full_alt <-
  grf_1940_histid_street %>% 
  left_join(x, by = c("full_street_address" = "USER_full_street_address"))

# What percent of people in the grf were geolocated: 91.8% (as of 2025/2/)
nrow(grf_1940_geolocated_full %>% filter(!is.na(X)))/nrow(grf_1940_geolocated_full)

# With City restriction: 65% of addresses. But City in the geolocation is sometimes funky...
# For example
nrow(grf_1940_geolocated_full_alt %>% filter(!is.na(X)))/nrow(grf_1940_geolocated_full_alt)
