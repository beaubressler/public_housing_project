######
# In this program, I will concord counties across time, so that we are comparing 
# outcomes within the same units over time
# References I am looking at: 
# Fabian Eckert's crosswalk files
# I might want to concord everything to 1870 counties, rather than 2010
######

library(tidyverse)
library(haven)
library(ipumsr)
library(sf)

# Note: Making maps or any big figures on Mac takes a long time for some reason,
# unless you use ragg and change the graphics device.
library(ragg)

raw_crosswalk_dir <-
  "data/derived/geographic_crosswalks/"

raw_shp_dir <-
  "data/raw/nhgis/gis/"

### Using my Eckert crosswalks --- 

## 0. Read in weights, constructed by construct_tract_crosswalk_eglp.py
weights_1930_to_1990 <- 
  read_csv(paste0(raw_crosswalk_dir, "tract_concordance_weights1930_to_1990.csv"))

weights_1940_to_1990 <-
  read_csv(paste0(raw_crosswalk_dir, "tract_concordance_weights1940_to_1990.csv"))

weights_1950_to_1990 <-
  read_csv(paste0(raw_crosswalk_dir, "tract_concordance_weights1950_to_1990.csv"))

weights_1960_to_1990 <-
  read_csv(paste0(raw_crosswalk_dir, "tract_concordance_weights1960_to_1990.csv"))

weights_1970_to_1990 <-
  read_csv(paste0(raw_crosswalk_dir, "tract_concordance_weights1970_to_1990.csv"))

weights_1980_to_1990 <-
  read_csv(paste0(raw_crosswalk_dir, "tract_concordance_weights1980_to_1990.csv"))

weights_1990_to_2000 <-
  read_csv(paste0(raw_crosswalk_dir, "tract_concordance_weights1990_to_2000.csv"))

## 1. Read in the NHGIS shape files
shapefile_1930 <- read_sf(paste0(raw_shp_dir, "nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp"))
shapefile_1940 <- read_sf(paste0(raw_shp_dir, "nhgis0027_shapefile_tl2000_us_tract_1940/US_tract_1940.shp"))
shapefile_1950 <- read_sf(paste0(raw_shp_dir, "nhgis0027_shapefile_tl2000_us_tract_1950/US_tract_1950.shp"))
shapefile_1960 <- read_sf(paste0(raw_shp_dir, "nhgis0027_shapefile_tl2000_us_tract_1960/US_tract_1960.shp"))
shapefile_1970 <- read_sf(paste0(raw_shp_dir, "nhgis0027_shapefile_tl2000_us_tract_1970/US_tract_1970.shp"))
shapefile_1980 <- read_sf(paste0(raw_shp_dir, "nhgis0027_shapefile_tl2000_us_tract_1980/US_tract_1980.shp"))
shapefile_1990 <- read_sf(paste0(raw_shp_dir, "nhgis0027_shapefile_tl2000_us_tract_1990/US_tract_1990.shp"))
shapefile_2000 <- read_sf(paste0(raw_shp_dir, "nhgis0027_shapefile_tl2000_us_tract_2000/US_tract_2000.shp"))

# TODO: Do I need the shape file or do I need to read in the whole files to get the tract name... shapefile doesnt have
# I think I should actually read in the table too. Keep GISJOIN, TRACTA, STATE, and COUNTY
# but this is hard because they are split among a bunch of different files for some years...oof
# Maybe I do this AFTER I create consolidated tract data...
# I can just use GISJOIN instead of tract names

## 2. 

# Goal: 

cw_1930_to_2000 <- 
  left_join(weights_1930_to_2000, shapefile_1930, by = c("GISJOIN_1930" = "GISJOIN")) %>% 
  select(GISJOIN_1930, GISJOIN_2000, weight) %>% 
  left_join(shapefile_2000, by = c("GISJOIN_1870" = "GISJOIN")) %>% 
  select(ICPSRFIP, NHGISST, weight, fips, year) %>% 
  dplyr::rename(
    fips_1870 = ICPSRFIP,
    state_1870 = NHGISST) %>% 
  mutate(state_1870 = substr(state_1870, 1, 2)) # get first 2, which is the state fips code

cw_1890_to_1870 <- 
  left_join(weights_1890_to_1870, shapefile_1890, by = c("GISJOIN_1890" = "GISJOIN")) %>% 
  select(ICPSRFIP, DECADE, weight, GISJOIN_1890, GISJOIN_1870) %>% 
  dplyr::rename(
    fips = ICPSRFIP,
    year = DECADE) %>% 
  left_join(shapefile_1870, by = c("GISJOIN_1870" = "GISJOIN")) %>% 
  select(ICPSRFIP, NHGISST, weight, fips, year) %>% 
  dplyr::rename(
    fips_1870 = ICPSRFIP,
    state_1870 = NHGISST) %>% 
  mutate(state_1870 = substr(state_1870, 1, 2)) # get first 2, which is the state fips code


cw_1900_to_1870 <- 
  left_join(weights_1900_to_1870, shapefile_1900, by = c("GISJOIN_1900" = "GISJOIN")) %>% 
  select(ICPSRFIP, DECADE, weight, GISJOIN_1900, GISJOIN_1870) %>% 
  dplyr::rename(
    fips = ICPSRFIP,
    year = DECADE) %>% 
  left_join(shapefile_1870, by = c("GISJOIN_1870" = "GISJOIN")) %>% 
  select(ICPSRFIP, NHGISST, weight, fips, year) %>% 
  dplyr::rename(
    fips_1870 = ICPSRFIP,
    state_1870 = NHGISST) %>% 
  mutate(state_1870 = substr(state_1870, 1, 2)) # get first 2, which is the state fips code

cw_1910_to_1870 <- 
  left_join(weights_1910_to_1870, shapefile_1910, by = c("GISJOIN_1910" = "GISJOIN")) %>% 
  select(ICPSRFIP, DECADE, weight, GISJOIN_1910, GISJOIN_1870) %>% 
  dplyr::rename(
    fips = ICPSRFIP,
    year = DECADE) %>% 
  left_join(shapefile_1870, by = c("GISJOIN_1870" = "GISJOIN")) %>% 
  select(ICPSRFIP, NHGISST, weight, fips, year) %>% 
  dplyr::rename(
    fips_1870 = ICPSRFIP,
    state_1870 = NHGISST) %>% 
  mutate(state_1870 = substr(state_1870, 1, 2)) # get first 2, which is the state fips code



# combine crosswalks
cw_full <- 
  bind_rows(cw_1880_to_1870, cw_1890_to_1870) %>% 
  bind_rows(cw_1900_to_1870) %>% 
  bind_rows(cw_1910_to_1870) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year), !is.na(fips_1870), fips_1870 != 0) %>% 
  # round weight to 4 digits
  mutate(weight = round(weight, 4))

### Output county concordances for use 
write_dta(cw_full, "data/derived/crosswalks/geographic_crosswalks/cleaned_geo_cw_to_1870.dta")
