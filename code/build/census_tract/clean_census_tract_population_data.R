####
# Read in raw NHGIS tract-level population data

# To start, get the following:
# 1. Tract level population
# 2. Tract level white and black population and population shares

# Also get the central business district (CBD) indicator for each tract from 1980
####

library(tidyverse)
library(tidycensus)
library(ipumsr)
library(here)


# directories 
output_dir <- here("data", "derived", "census")
original_output_dir <- here("data", "derived", "census", "non_concorded")
geographic_crosswalk_dir <- here("data", "derived", "geographic_crosswalks")


# Read in geographic tract crosswalks ----
# crosswalks, 1930 to 1990
tract_crosswalk_1930 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1930_to_1950.csv"))
tract_crosswalk_1940 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1940_to_1950.csv"))
tract_crosswalk_1960 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1960_to_1950.csv"))
tract_crosswalk_1970 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1970_to_1950.csv"))
tract_crosswalk_1980 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1980_to_1950.csv"))
tract_crosswalk_1990 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1990_to_1950.csv"))
tract_crosswalk_2000 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights2000_to_1950.csv"))

# Read CBD crosswalk ----
cbd_tracts_1950 <- read_csv(here(output_dir, "cbd_tracts_1950_concorded.csv"))

# Compile tract-level Census population data ----

# shared variables we want to keep
tract_background_variables <-
  c("NHGISST","NHGISCTY", "GISJOIN", "GISJOIN2", "SHAPE_AREA", "SHAPE_LEN",
    "geometry", "YEAR", "STATE", "STATEA", "COUNTY", "COUNTYA", 
    # "PRETRACTA", "POSTTRCTA", # excluding these for now
    "TRACTA",  "AREANAME")


### 1920 ----
chicago_1920_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1920/nhgis0027_ds45_1920_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1920/US_tract_1920.shp",
                  file_select = starts_with("US_tract_1920")
                  ),
    by = "GISJOIN") %>% 
  filter(!is.na(YEAR))

cleveland_1920_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1920/nhgis0027_ds46_1920_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1920/US_tract_1920.shp",
    file_select = starts_with("US_tract_1920")), by = "GISJOIN") %>% 
  filter(!is.na(YEAR))

milwaukee_1920_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1920/nhgis0027_ds47_1920_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1920/US_tract_1920.shp",
    file_select = starts_with("US_tract_1920")), by = "GISJOIN") %>% 
  filter(!is.na(YEAR))

ny_1920_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1920/nhgis0027_ds48_1920_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1920/US_tract_1920.shp",
    file_select = starts_with("US_tract_1920")), by = "GISJOIN") %>% 
  filter(!is.na(YEAR))

full_tract_data_1920 <-
  bind_rows(chicago_1920_tract_raw, cleveland_1920_tract_raw,
            milwaukee_1920_tract_raw, ny_1920_tract_raw)

####  harmonize variables and create tract-level population dataset----

tract_population_data_1920 <- 
  full_tract_data_1920 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), A94001,
         contains("BAT"), BBJ001, contains("BBL"), BBP001,
         contains("BBQ"), contains("BCG")) %>% 
  # Total population: BBP001, BBQ002, A94001, BBJ001
  mutate(total_pop = case_when(
    !is.na(BBP001) ~ BBP001,
    !is.na(BBQ002) ~ BBQ002,
    !is.na(A94001) ~ A94001,
    !is.na(BBJ001) ~ BBJ001,
    TRUE ~ NA_real_
  )) %>%
  select(-BBP001, -BBQ002,-A94001, -BBJ001) %>% 
  # White population: 
    # NY: (BCG001 + BCG002 + BCG003 + BCG004)
    # Chicago: (BAT001 + BAT002 + BAT003 + BAT004)
    # Cleveland: (BBL001 + BBL002 + BBL003)
    # Milwaukee: unavailable
  mutate(white_pop = case_when(
    !is.na(BCG001) ~ (BCG001 + BCG002 + BCG003 + BCG004),
    !is.na(BAT001) ~ (BAT001 + BAT002 + BAT003 + BAT004),
    !is.na(BBL001) ~ (BBL001 + BBL002 + BBL003),
    TRUE ~ NA_real_
  )) %>%
  # Foreign-born white population: 
    # NY: (BCG003)
    # Chicago: (BAT004)
    # Cleveland: (BBL003)
    # Milwaukee: unavailable
  mutate(foreign_born_white_pop = case_when(
    !is.na(BCG003) ~ BCG003,
    !is.na(BAT004) ~ BAT004,
    !is.na(BBL003) ~ BBL003,
    TRUE ~ NA_real_
  )) %>%
  # Black population: 
    # NY:  BCG005
    # Chicago: BAT005
    # Cleveland: BBL004
    # Milwaukee: unavailable
  mutate(black_pop = case_when(
    !is.na(BCG005) ~ BCG005,
    !is.na(BAT005) ~ BAT005,
    !is.na(BBL004) ~ BBL004,
    TRUE ~ NA_real_
  )) %>%
  # "Other" population:
    # NY: BCG006
    # Chicago: BAT006
    # Cleveland: BBL005
    # Milwaukee: unavailable
  mutate(other_pop = case_when(
    !is.na(BCG006) ~ BCG006,
    !is.na(BAT006) ~ BAT006,
    !is.na(BBL005) ~ BBL005,
    TRUE ~ NA_real_
  )) %>% 
  select(any_of(tract_background_variables), contains("pop"))
  
  
### 1930 ----
chicago_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1930/nhgis0027_ds58_1930_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
    file_select = starts_with("US_tract_1930")), by = "GISJOIN") %>% 
  filter(!is.na(YEAR))

cincinnati_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1930/nhgis0027_ds59_1930_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
    file_select = starts_with("US_tract_1930")), by = "GISJOIN") %>% 
  filter(!is.na(YEAR))

cleveland_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1930/nhgis0027_ds60_1930_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
    file_select = starts_with("US_tract_1930")), by = "GISJOIN") %>% 
  filter(!is.na(YEAR))

detroit_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1930/nhgis0027_ds62_1930_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
    file_select = starts_with("US_tract_1930")), by = "GISJOIN") %>% 
  filter(!is.na(YEAR))

misc_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1930/nhgis0027_ds63_1930_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
    file_select = starts_with("US_tract_1930")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

misc_1930_tract_raw_2 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1930/nhgis0027_ds67_1930_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
                  file_select = starts_with("US_tract_1930")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR)) %>% 
  # misc_1930_tract_raw contains some of the same tracts, and they end up being duplicates or off by like 1 person,
  # so we'll just filter them out here
  filter(!GISJOIN %in% misc_1930_tract_raw$GISJOIN)


nashville_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1930/nhgis0027_ds65_1930_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
    file_select = starts_with("US_tract_1930")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

ny_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1930/nhgis0027_ds66_1930_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
    file_select = starts_with("US_tract_1930")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))


stl_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1930/nhgis0027_ds68_1930_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
    file_select = starts_with("US_tract_1930")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

syracuse_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1930/nhgis0027_ds70_1930_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
    file_select = starts_with("US_tract_1930")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

dc_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1930/nhgis0027_ds71_1930_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
    file_select = starts_with("US_tract_1930")), by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))


# intersect all 1930 tract dfs
shared_columns <-
  intersect(names(chicago_1930_tract_raw), names(cincinnati_1930_tract_raw)) %>% 
  intersect(names(cleveland_1930_tract_raw)) %>% 
  intersect(names(detroit_1930_tract_raw)) %>% 
  intersect(names(misc_1930_tract_raw)) %>% 
  intersect(names(nashville_1930_tract_raw)) %>% 
  intersect(names(ny_1930_tract_raw)) %>% 
  intersect(names(misc_1930_tract_raw_2)) %>% 
  intersect(names(stl_1930_tract_raw)) %>% 
  intersect(names(syracuse_1930_tract_raw)) %>% 
  intersect(names(dc_1930_tract_raw)) 
  # keep only "STATEA", "COUNTYA", "PRETRACTA", "TRACTA, "POSTTRCTA"


columns_to_convert <-
  c("STATEA", "COUNTYA", "PRETRACTA", "TRACTA", "POSTTRCTA")

  
# convert these columns to character in all these tract datasets using lapply
list_of_1930_dfs <-
  list(chicago_1930_tract_raw, cincinnati_1930_tract_raw,
       cleveland_1930_tract_raw, detroit_1930_tract_raw,
       misc_1930_tract_raw, nashville_1930_tract_raw,
       ny_1930_tract_raw, misc_1930_tract_raw_2,
       stl_1930_tract_raw, syracuse_1930_tract_raw,
       dc_1930_tract_raw)

list_of_1930_dfs_converted <- lapply(list_of_1930_dfs, function(df) {
  df %>% mutate(across(all_of(columns_to_convert), as.character))
})

# save names of those dfs as a string vector
names_1930_dfs = c("chicago_1930_tract_raw", "cincinnati_1930_tract_raw",
          "cleveland_1930_tract_raw", "detroit_1930_tract_raw",
          "misc_1930_tract_raw", "nashville_1930_tract_raw",
          "ny_1930_tract_raw", "misc_1930_tract_raw_2",
          "stl_1930_tract_raw", "syracuse_1930_tract_raw",
          "dc_1930_tract_raw")

# reassign to the global environment
names(list_of_1930_dfs_converted) <- names_1930_dfs
list2env(list_of_1930_dfs_converted, envir = .GlobalEnv)

# create full tract dataset
full_tract_data_1930 <-
  bind_rows(chicago_1930_tract_raw, cincinnati_1930_tract_raw,
            cleveland_1930_tract_raw, detroit_1930_tract_raw,
            misc_1930_tract_raw, nashville_1930_tract_raw,
            ny_1930_tract_raw, misc_1930_tract_raw_2,
            stl_1930_tract_raw, syracuse_1930_tract_raw,
            dc_1930_tract_raw) 

####  harmonize variables and create tract-level population dataset----

# Definitions are as follows (manually got these from the codebooks):
# washington DC: BRQ001 (total pop), BRT001 + BRT002 + BRT003  (white pop)
#              BRT003 (foreign white) BRT004 (black pop) BRT005 (other) 
# Syracuse: Skipping because lazy (ds70)
# St Louis: BOO001 (total pop), BPW001 + BPW002 + BPW003 (white pop)
#           BPW003 (foreign white) BPW004 (black pop) BPW005 (other)
# Misc1: BOJ001 (total pop), BOK001 + BOK002 + BOK003 (white pop), BOK003 (foreign white)
#      BOK004 (black pop) BOK005 (other)
# NYC: BOI001 (total pop), BOC001+BOC002+BOC003+BOC006+BOC007+BOC008 (white pop)
#      BOC003+BOC008 (foreign white), BOC004 + BOC009 (Black), BOC005+BOC010 (other)
# note: NYC here is I pulled by race and sex
# Nashville:  BNE001 (total pop), BNP001+BNP002+BNP003 (white pop), BNP003 (foreign white)
#             BNP004 (black pop), BNP005 (other)
# Misc2: BLW001 (total pop), BM4001+BM4002+BM4003 (white pop), BM4003 (foreign white)
#        BM4004 (black pop), BM4005 (other)
# Detroit: BLO001 (total pop), BLR001+BLR002+BLR003 (white pop), BLR003 (foreign white)
#          BLR004 (black pop), BLR005 (other) 
# Cleveland: BJW001 (total pop), BK1001+BK1002+BK1003 (white pop), BK1003 (foreign white)
#            BK1004 (black pop), BK1005 (other)
# Cincinnati: BI1001 (total pop), BJR001 + BJR002 (white pop), BJR002 (foreign white),
#             BJR003 (black pop), BI1001 - BJR001 + BJR002 - BJR003 (other)
# Chicago: BHI001 (total pop), BIQ001 + BIQ002 + BIQ003 (white pop), BIQ003 (foreign white)
#          BIQ004 (black pop), BIQ005 (other)


tract_population_data_1930 <- 
  full_tract_data_1930 %>% 
  # Total population: 
  mutate(total_pop = case_when(
    !is.na(BRQ001) ~ BRQ001,
    !is.na(BOO001) ~ BOO001,
    !is.na(BOJ001) ~ BOJ001,
    !is.na(BOI001) ~ BOI001,
    !is.na(BOJ001) ~ BOJ001,
    !is.na(BNE001) ~ BNE001,
    !is.na(BLW001) ~ BLW001,
    !is.na(BLO001) ~ BLO001,
    !is.na(BJW001) ~ BJW001,
    !is.na(BI1001) ~ BI1001,
    !is.na(BHI001) ~ BHI001,
    TRUE ~ NA_real_
  )) %>%
  # White population:
  mutate(white_pop = case_when(
    !is.na(BRT001 + BRT002 + BRT003) ~ BRT001 + BRT002 + BRT003,
    !is.na(BPW001 + BPW002 + BPW003) ~ BPW001 + BPW002 + BPW003,
    !is.na(BOK001 + BOK002 + BOK003) ~ BOK001 + BOK002 + BOK003,
    !is.na(BOC001 + BOC002 + BOC003 + BOC006 + BOC007 + BOC008) ~ BOC001 + BOC002 + BOC003 + BOC006 + BOC007 + BOC008,
    !is.na(BNP001 + BNP002 + BNP003) ~ BNP001 + BNP002 + BNP003,
    !is.na(BM4001 + BM4002 + BM4003) ~ BM4001 + BM4002 + BM4003,
    !is.na(BLR001 + BLR002 + BLR003) ~ BLR001 + BLR002 + BLR003,
    !is.na(BK1001 + BK1002 + BK1003) ~ BK1001 + BK1002 + BK1003,
    !is.na(BJR001 + BJR002) ~ BJR001 + BJR002,
    !is.na(BIQ001 + BIQ002 + BIQ003) ~ BIQ001 + BIQ002 + BIQ003,
    TRUE ~ NA_real_
  )) %>%
  # Foreign white population:
  mutate(foreign_white_pop = case_when(
    !is.na(BRT003) ~ BRT003,
    !is.na(BPW003) ~ BPW003,
    !is.na(BOK003) ~ BOK003,
    !is.na(BOC003 + BOC008) ~ BOC003 + BOC008,
    !is.na(BNP003) ~ BNP003,
    !is.na(BM4003) ~ BM4003,
    !is.na(BLR003) ~ BLR003,
    !is.na(BK1003) ~ BK1003,
    !is.na(BJR002) ~ BJR002,
    !is.na(BIQ003) ~ BIQ003,
    TRUE ~ NA_real_
  )) %>%
  # Black population:
  mutate(black_pop = case_when(
    !is.na(BRT004) ~ BRT004,
    !is.na(BPW004) ~ BPW004,
    !is.na(BOK004) ~ BOK004,
    !is.na(BOC004 + BOC009) ~ BOC004 + BOC009,
    !is.na(BNP004) ~ BNP004,
    !is.na(BM4004) ~ BM4004,
    !is.na(BLR004) ~ BLR004,
    !is.na(BK1004) ~ BK1004,
    !is.na(BJR003) ~ BJR003,
    !is.na(BIQ004) ~ BIQ004,
    TRUE ~ NA_real_
  )) %>%
  # "Other" population:
  mutate(other_pop = case_when(
    !is.na(BRT005) ~ BRT005,
    !is.na(BPW005) ~ BPW005,
    !is.na(BOK005) ~ BOK005,
    !is.na(BOC005 + BOC010) ~ BOC005 + BOC010,
    !is.na(BNP005) ~ BNP005,
    !is.na(BM4005) ~ BM4005,
    !is.na(BLR005) ~ BLR005,
    !is.na(BK1005) ~ BK1005,
    !is.na(BI1001 - BJR001 - BJR002 - BJR003) ~ BI1001 - BJR001 - BJR002 - BJR003,
    !is.na(BIQ005) ~ BIQ005,
    TRUE ~ NA_real_
  )) %>%
  select(any_of(tract_background_variables), contains("pop")) %>% 
  # keep only one of each background variables... deletes a few duplicate tracts that differ by 1 in population
  group_by(YEAR, STATE, COUNTY, TRACTA) %>%
  slice(1) %>% 
  distinct()
  
  

### 1940 ----

# read in 1940 tract data
full_tract_data_1940 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1940/nhgis0027_ds76_1940_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1940/US_tract_1940.shp",
    file_select = starts_with("US_tract_1940")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))


#### harmonization ----
tract_population_data_1940 <-
  full_tract_data_1940 %>% 
  mutate(total_pop = BUB001,
         white_pop = BUQ001,
         foreign_white_pop = BUB001 - BU5001,
         black_pop = BVG001, 
         other_pop = total_pop - white_pop - black_pop) %>%
  select(any_of(tract_background_variables), contains("pop"))

### 1950 ----

# read in 1950 tract data
full_tract_data_1950 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1950/nhgis0027_ds82_1950_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1950/US_tract_1950.shp",
    file_select = starts_with("US_tract_1950")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

#### Harmonization ----

tract_population_data_1950 <-
  full_tract_data_1950 %>% 
  mutate(total_pop = BZ8001,
         white_pop = B0J001,
         foreign_white_pop = B1K001,
         black_pop = B0J002, 
         other_pop = B0J003) %>%
  select(any_of(tract_background_variables), contains("pop"))

#### save 1950 Census tract information ----

# GISJOIN + tract IDs
tract_info_1950_gisjoin <-
  tract_population_data_1950 %>% 
  select(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, TRACTA) %>% 
  # calculate area in square meters
  mutate(area_m2 = st_area(geometry))
  

### 1960 ----
full_tract_data_1960 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1960/nhgis0027_ds92_1960_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1960/US_tract_1960.shp",
    file_select = starts_with("US_tract_1960")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))


#### Harmonization ----

tract_population_data_1960 <-
  full_tract_data_1960 %>% 
  mutate(total_pop = B60001, # there are a few different possible total pop columns, but this seems to be most consistent with race pops
         white_pop = B7B001,
         foreign_white_pop = NA,
         black_pop = B7B002, 
         other_pop = B7B003) %>%
  # create population shares
  select(any_of(tract_background_variables), contains("pop"))

### 1970 ----
tract_data_1970_1 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1970/nhgis0027_ds97_1970_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1970/US_tract_1970.shp",
    file_select = starts_with("US_tract_1970")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))


tract_data_1970_2 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1970/nhgis0027_ds98_1970_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1970/US_tract_1970.shp",
    file_select = starts_with("US_tract_1970")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

full_tract_data_1970 <-
  tract_data_1970_1 %>% 
  select(NHGISST, NHGISCTY, GISJOIN2, COUNTYA, TRACTA, contains("CY")) %>% 
  st_drop_geometry() %>% 
  right_join(tract_data_1970_2)

#### harmonization ----

# Using variables from 1970 Census: Count 4Pa - Sample-Based Population Data (ds98)
# I am just going to calculate total population as the some of white, black and other
# This is not from the 100% sample, but it's close and will be consistent

tract_population_data_1970 <-
  full_tract_data_1970 %>% 
  mutate(white_pop = C0X001,
         foreign_white_pop = NA,
         black_pop = C0X002, 
         other_pop = C0X003,
         total_pop = white_pop + black_pop + other_pop) %>%
  select(any_of(tract_background_variables), contains("pop"))


### 1980 -------

# 100% data
tract_data_1980_1 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1980/nhgis0027_ds104_1980_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1980/US_tract_1980.shp",
    file_select = starts_with("US_tract_1980")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

# sample based
tract_data_1980_2 <-
  ipums_shape_full_join(
      read_nhgis("data/raw/nhgis/tables/1980/nhgis0027_ds107_1980_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1980/US_tract_1980.shp",
    file_select = starts_with("US_tract_1980")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

# PL 94-171 Population Counts
tract_data_1980_3 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1980/nhgis0027_ds116_1980_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1980/US_tract_1980.shp",
    file_select = starts_with("US_tract_1980")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

#### harmonization ----

# Note: Using only tract_data_1980_1: This is based on the 100% Census data (see codebook)
# For tracts with non missing geometries, these are almost always the same:
# View(x %>% mutate(diff = C7L001 - C6W001) %>% 
# arrange(diff) %>% filter(TRACTA != 999999, !is.na(NHGISST)) %>% filter(diff < 0))

# View(tract_data_1980_3 %>% select(C6W001, TRACTA))

x <- tract_data_1980_1 %>% select(C7L001, TRACTA, COUNTYA, NHGISST, GISJOIN) %>%
  st_drop_geometry() %>%
  right_join(tract_data_1980_3 %>% select(C6W001, GISJOIN, TRACTA, COUNTYA, NHGISST))


tract_population_data_1980 <-
  tract_data_1980_1 %>% 
  mutate(total_pop = C7L001, 
         white_pop = C9D001,
         foreign_white_pop = NA,
         black_pop = C9D002, 
         other_pop = total_pop - white_pop - black_pop) %>%
  select(any_of(tract_background_variables), contains("pop"))

### 1990 ----

# 100% data
tract_data_1990_1 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1990/nhgis0027_ds120_1990_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1990/US_tract_1990.shp",
    file_select = starts_with("US_tract_1990")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

# sample-based
tract_data_1990_2 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1990/nhgis0027_ds123_1990_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1990/US_tract_1990.shp",
    file_select = starts_with("US_tract_1990")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

#### harmonization ----

# Note: Using only tract_data_1990_1: This is based on the 100% Census data (see codebook)

tract_population_data_1990 <-
  tract_data_1990_1 %>% 
  mutate(total_pop = ET1001, 
         white_pop = EUY001,
         foreign_white_pop = NA,
         black_pop = EUY002, 
         other_pop = total_pop - white_pop - black_pop) %>%
  select(any_of(tract_background_variables), contains("pop"))

### 2000 ----

# read in 2000 tract data
full_tract_data_2000 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/2000/nhgis0027_ds146_2000_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_2000/US_tract_2000.shp",
    file_select = starts_with("US_tract_2000")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR))

#### harmonization ----

tract_population_data_2000 <-
  full_tract_data_2000 %>% 
  mutate(total_pop = FL5001, 
         white_pop = FMR001,
         foreign_white_pop = NA,
         black_pop = FMR002, 
         other_pop = FMR003 + FMR004 + FMR005 + FMR006 + FMR007) %>%
  select(any_of(tract_background_variables), contains("pop"))



# Concord datasets to 1950 Census tracts ----
# 1. Join on crosswalk to get GISJOIN_1950 and weights
# 2. Weight populations by "weight" and collapse to GISJOIN_1950
# 3. Merge geography information from 1990 NHGIS file

## Loop for 1930-2000 ----- 
years <- c(1930, 1940, 1960, 1970, 1980, 1990, 2000)

for (year in years) {
  print(year)
  # Construct variable names and file names dynamically based on the year
  tract_population_data_var <- paste0("tract_population_data_", year)
  tract_crosswalk_var <- paste0("tract_crosswalk_", year)
  tract_population_data_concorded_var <- paste0("tract_population_data_", year, "_concorded")
  
  # Execute the operations inside the loop
  assign(tract_population_data_concorded_var, 
         get(tract_population_data_var) %>% 
           # join on crosswalk
           left_join(get(tract_crosswalk_var), by = c("GISJOIN" = paste0("GISJOIN_", year))) %>%
           # weight populations
           mutate_at(vars(contains("pop")), ~ . * weight) %>%
           st_drop_geometry() %>% 
           # collapse to GISJOIN_1950
           group_by(GISJOIN_1950, YEAR) %>% 
           summarise_at(vars(contains("pop")), sum, na.rm = TRUE) %>%
           # merge on 1950 tract IDs for each GISJOIN 
           left_join(tract_info_1950_gisjoin, by = c("GISJOIN_1950" = "GISJOIN")) 
         
  )
}


## Clean 1950 data to same format as concorded data  ----
tract_population_data_1950_concorded <-
  tract_population_data_1950 %>% 
  st_drop_geometry() %>% 
  select(GISJOIN, YEAR, contains("pop")) %>% 
  left_join(tract_info_1950_gisjoin) %>% 
  dplyr::rename(GISJOIN_1950 = GISJOIN)
  
  
# Combine tract-level datasets ----

# population data with the original tracts
# might use this to compare at some point
tract_population_data_original_tracts <-
  bind_rows(tract_population_data_1930, tract_population_data_1940,
            tract_population_data_1950, tract_population_data_1960, tract_population_data_1970, 
            tract_population_data_1980, tract_population_data_1990, tract_population_data_2000) %>%
  # calculate area in square meters
  mutate(area_m2 = st_area(geometry)) %>% 
  # fix total population data if sum of components are bigger than total population (adjust total pop)
  mutate(total_pop = pmax(total_pop, white_pop + black_pop + other_pop)) %>% 
  # calculate white, black and other share, as well as foreign white share
  mutate(white_share = white_pop / total_pop,
         foreign_white_share = foreign_white_pop / total_pop,
         black_share = black_pop / total_pop,
         other_share = other_pop / total_pop,
         population_density = total_pop/area_m2) %>%
  select(any_of(tract_background_variables), contains("pop"), contains("share"), population_density) %>% 
  # drop rows with missing geometry
  filter(!st_is_empty(geometry)) 

# tract population data concorded to 1990 tracts
tract_population_data_concorded <-
  bind_rows(tract_population_data_1930_concorded, tract_population_data_1940_concorded,
            tract_population_data_1950_concorded, tract_population_data_1960_concorded,
            tract_population_data_1970_concorded, tract_population_data_1980_concorded,
            tract_population_data_1990_concorded, tract_population_data_2000_concorded) %>%
  # merge on CBD indicator
  left_join(cbd_tracts_1950)  %>% 
  mutate(cbd = ifelse(is.na(cbd), 0, cbd)) %>% 
  # fix total population data if sum of components are bigger than total population (adjust total pop)
  mutate(total_pop = pmax(total_pop, white_pop + black_pop + other_pop)) %>% 
  # calculate white, black and other share, as well as foreign white share
  mutate(white_share = white_pop / total_pop,
         foreign_white_share = foreign_white_pop / total_pop,
         black_share = black_pop / total_pop,
         other_share = other_pop / total_pop,
         population_density = total_pop/area_m2) %>%
  select(any_of(tract_background_variables), contains("pop"), contains("share"), population_density, cbd) %>% 
  # drop rows with missing geometry
  filter(!st_is_empty(geometry)) 


# Save ----

# save concorded tract_population_data as shapefile
st_write(tract_population_data_concorded, here(output_dir, "tract_population_data.gpkg"),
         append = FALSE, layer = "points", driver = "GPKG")

# save non-concorded tract_population_data as shapefile
st_write(tract_population_data_original_tracts, here(original_output_dir, "tract_population_data_original_tracts.gpkg"),
         append = FALSE, layer = "points", driver = "GPKG")


