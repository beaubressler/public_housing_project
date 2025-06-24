####
# Read in raw NHGIS tract-level data from

# To start, get tract level:
# 1. Median rents
# 2. Median home value
####

library(tidyverse)
library(tidycensus)
library(sf)
library(ipumsr)


geographic_crosswalk_dir <- "data/derived/geographic_crosswalks/"


# Write function for caluculating median when we are given counts of a bunch of groups
# This is what we get in some Census years for, say, home values and rented homes
# We will get the number of owned homes with value less than $1000, $1000-1500, etc, and I want the median

calculate_median_from_census <- function(data, var_code, var_name) {
  data %>% 
  # convert all the B8W variables to numeric
  mutate_at(vars(contains(var_code)), as.numeric) %>%
  pivot_longer(cols = starts_with(var_code), names_to = "range", values_to = "num_in_sample") %>%
  mutate(
    range = gsub(var_code, "", range),
    range = as.numeric(range)) %>%
  group_by(YEAR, GISJOIN_1950) %>%
  mutate(
    cumulative_freq = cumsum(num_in_sample),
    total = sum(num_in_sample),
    median_position = total / 2) %>% 
  filter(cumulative_freq >= median_position) %>%
  slice(1) %>% 
  mutate(median = paste0(var_code, str_pad(range, 3, pad = "0"))) %>% 
  select(YEAR, GISJOIN_1950, median) %>% 
  # drop geometry
  st_drop_geometry() %>% 
  dplyr::rename(!!var_name := median)
}


# Read in geographic tract crosswalks ----
# crosswalks, 1930 to 1950
tract_crosswalk_1930 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1930_to_1950.csv"))
tract_crosswalk_1940 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1940_to_1950.csv"))
tract_crosswalk_1960 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1960_to_1950.csv"))
tract_crosswalk_1970 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1970_to_1950.csv"))
tract_crosswalk_1980 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1980_to_1950.csv"))
tract_crosswalk_1990 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1990_to_1950.csv"))
# Compile tract-level Census housing data ----

# shared variables we want to keep
tract_background_variables <-
  c("GISJOIN_1950", "NHGISST","NHGISCTY", "GISJOIN", "GISJOIN2", "SHAPE_AREA", "SHAPE_LEN",
    "geometry", "YEAR", "STATE", "STATEA", "COUNTY", "COUNTYA",
    "TRACTA", "POSTTRCTA", "AREANAME")

### 1920 ---- 
# 4/2024: For now, not going to use 1920 NYC data
# ny_1920_tract_raw <-
#   ipums_shape_full_join(
#     read_nhgis(
#       "data/raw/nhgis/tables/housing/1920/nhgis0034_ds48_1920_tract.csv"
#     ),
#     read_ipums_sf(
#       "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1920/US_tract_1920.shp",
#       file_select = starts_with("US_tract_1920")
#     ),
#     by = "GISJOIN"
#   ) %>%
#   filter(!is.na(YEAR)) 
# 
# 
# full_tract_data_1920 <-
#   bind_rows(ny_1920_tract_raw)

####  harmonize variables and create tract-level housing dataset----

### 1930 -----
chicago_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1930/nhgis0034_ds58_1930_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
      file_select = starts_with("US_tract_1930")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

cleveland_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1930/nhgis0034_ds60_1930_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
      file_select = starts_with("US_tract_1930")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))


stl_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1930/nhgis0034_ds68_1930_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
      file_select = starts_with("US_tract_1930")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

misc_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1930/nhgis0034_ds63_1930_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1930/US_tract_1930.shp",
      file_select = starts_with("US_tract_1930")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

# compile 1930 data
columns_to_convert <-
  c("STATEA", "COUNTYA", "PRETRACTA", "TRACTA", "POSTTRCTA")


#### 


#### Calculate median rents and housing values for 1930 (for which I can get it) ----

##### Chicago 1930 

chicago_1930_tract <-
  chicago_1930_tract_raw %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), starts_with("BIO"), starts_with("BIN"))

var_names <- names(chicago_1930_tract)
var_names[startsWith(var_names, "BIO")] <- sub("^BIO", "rent_group", var_names[startsWith(var_names, "BIO")])
var_names[startsWith(var_names, "BIN")] <- sub("^BIN", "home_value_group", var_names[startsWith(var_names, "BIN")])
names(chicago_1930_tract) <- var_names


##### Cleveland 1930

cleveland_1930_tract <-
  cleveland_1930_tract_raw %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), starts_with("BK0"), starts_with("BKZ"))

var_names <- names(cleveland_1930_tract)
var_names[startsWith(var_names, "BK0")] <- sub("^BK0", "rent_group", var_names[startsWith(var_names, "BK0")])
var_names[startsWith(var_names, "BKZ")] <- sub("^BKZ", "home_value_group", var_names[startsWith(var_names, "BKZ")])
names(cleveland_1930_tract) <- var_names

##### St Louis 1930
# median_rent_stl_1930 <- 
#   calculate_median_from_census(stl_1930_tract_raw, var_code = "BPZ", var_name = "median_rent_group")
# 
# median_home_value_stl_1930 <-
#   calculate_median_from_census(stl_1930_tract_raw, var_code = "BPY", var_name = "median_home_value_group")

stl_1930_tract <-
  stl_1930_tract_raw %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("BPY"), contains("BPZ"))  

var_names <- names(stl_1930_tract)
var_names[startsWith(var_names, "BPZ")] <- sub("^BPZ", "rent_group", var_names[startsWith(var_names, "BPZ")])
var_names[startsWith(var_names, "BPY")] <- sub("^BPY", "home_value_group", var_names[startsWith(var_names, "BPY")])
names(stl_1930_tract) <- var_names
         

##### Misc 1930
# median_home_value_misc_1930 <-
#   calculate_median_from_census(misc_1930_tract_raw, var_code = "BMZ", var_name = "median_home_value_group")

misc_1930_tract <-
  misc_1930_tract_raw %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("BMZ"))

var_names <- names(misc_1930_tract)
var_names[startsWith(var_names, "BMZ")] <- sub("^BMZ", "home_value_group", var_names[startsWith(var_names, "BMZ")])
names(misc_1930_tract) <- var_names
  

# convert these columns to character in all these tract datasets using lapply
list_of_1930_dfs <-
  list(chicago_1930_tract, cleveland_1930_tract, stl_1930_tract, misc_1930_tract)

list_of_1930_dfs_converted <- lapply(list_of_1930_dfs, function(df) {
  df %>% mutate(across(any_of(columns_to_convert), as.character))
})

# save names of those dfs as a string vector
names_1930_dfs = c("chicago_1930_tract", "cleveland_1930_tract",
                   "stl_1930_tract", "misc_1930_tract")

# reassign to the global environment
names(list_of_1930_dfs_converted) <- names_1930_dfs
list2env(list_of_1930_dfs_converted, envir = .GlobalEnv)

# create full tract dataset
tract_housing_data_1930 <-
  bind_rows(chicago_1930_tract, cleveland_1930_tract,
            stl_1930_tract, misc_1930_tract)


### 1940  -------
# median home value and median rent data
full_tract_data_1940 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1940/nhgis0045_ds76_1940_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1940/US_tract_1940.shp",
      file_select = starts_with("US_tract_1940")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

# 
full_tract_data_1940_add <- ipums_shape_full_join(
  read_nhgis(
    "data/raw/nhgis/tables/housing/1940/nhgis0038_ds76_1940_tract.csv"
  ),
  read_ipums_sf(
    "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1940/US_tract_1940.shp",
    file_select = starts_with("US_tract_1940")
  ),
  by = "GISJOIN"
) %>%
  filter(!is.na(YEAR))



#### harmonization ----
# Median value and median contract rent
tract_housing_data_1940 <- 
  full_tract_data_1940 %>% 
  dplyr::rename(median_home_value_reported = BVC001, median_rent_reported = BVF001) %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), median_home_value_reported,
         median_rent_reported, contains("BVB"), contains("BVE"))

# Share needing major repairs, and vacancy rates
tract_housing_data_1940_add <-
  full_tract_data_1940_add %>% 
  mutate(vacant_units = BU4001 + BU4002,
         total_units = BU1001,
         units_needing_repairs = BVK002,
         units_reporting_condition = BVK001 + BVK002,
         units_no_water = BVL008) %>% 
  # keep only variables of interest
  select(YEAR, STATEA, TRACTA, COUNTYA, GISJOIN2, vacant_units, 
         total_units, units_needing_repairs, units_no_water, units_reporting_condition) %>% 
  st_drop_geometry()

# merge vacancy rate and share_needing_repairs on
tract_housing_data_1940 <-
  left_join(tract_housing_data_1940, tract_housing_data_1940_add)

# rent and home value group variable names
var_names <- names(tract_housing_data_1940)
var_names[startsWith(var_names, "BVE")] <- sub("^BVE", "rent_group", var_names[startsWith(var_names, "BVE")])
var_names[startsWith(var_names, "BVB")] <- sub("^BVB", "home_value_group", var_names[startsWith(var_names, "BVB")])
names(tract_housing_data_1940) <- var_names


### 1950 -----
full_tract_data_1950 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1950/nhgis0046_ds82_1950_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1950/US_tract_1950.shp",
      file_select = starts_with("US_tract_1950")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

full_tract_data_1950_add <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1950/nhgis0038_ds82_1950_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1950/US_tract_1950.shp",
      file_select = starts_with("US_tract_1950")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----
# TODO: 1950 rents and home values seem weird
# Use medians calculated by group, compare, fill in missings

# median home value and rent
tract_housing_data_1950 <- 
  full_tract_data_1950 %>% 
  dplyr::rename(median_home_value_reported = B09001, median_rent_reported = B05001) %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), median_home_value_reported,
         median_rent_reported, contains("B08"), contains("B04")) 

# Share lacking running water, vacant units, and age
tract_housing_data_1950_add <-
  full_tract_data_1950_add %>% 
  mutate(vacant_units = B0T003 + B0T004,
         total_units = B0S001,
         units_no_bath = B1D001,
         units_no_running_water = B1D002,
         units_reporting_condition = B1C001,
         age_group001 = B0Y001, #units_0_to_10y_old
         age_group002 = B0Y002, #units_10_to_20y_old
         age_group003 = B0Y003, #units_20_to_30y_old
         age_group004 = B0Y004) %>%  #units_30yplus_old
  # keep only variables of interest
  select(YEAR, STATEA, TRACTA, COUNTYA, GISJOIN2, vacant_units, 
         total_units, units_no_bath, units_no_running_water, units_reporting_condition,
         contains("age_group")) %>% 
  st_drop_geometry()

# merge vacancy rate and share_needing_repairs on
tract_housing_data_1950 <-
  left_join(tract_housing_data_1950, tract_housing_data_1950_add)

# 
var_names <- names(tract_housing_data_1950)
var_names[startsWith(var_names, "B04")] <- sub("^B04", "rent_group", var_names[startsWith(var_names, "B04")])
var_names[startsWith(var_names, "B08")] <- sub("^B08", "home_value_group", var_names[startsWith(var_names, "B08")])
names(tract_housing_data_1950) <- var_names

#### save 1950 Census tract information ----


# GISJOIN + tract IDs
tract_info_1950_gisjoin <-
  tract_housing_data_1950 %>% 
  select(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, TRACTA) %>% 
  mutate(area_m2 = st_area(geometry))




### 1960 -----

full_tract_data_1960 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1960/nhgis0037_ds92_1960_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1960/US_tract_1960.shp",
      file_select = starts_with("US_tract_1960")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

full_tract_data_1960_add <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1960/nhgis0038_ds92_1960_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1960/US_tract_1960.shp",
      file_select = starts_with("US_tract_1960")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----
# 1960 and 1970: I have to calculate medians from the grouped data after concordances

tract_housing_data_1960 <-
  full_tract_data_1960 %>% 
  select(any_of(tract_background_variables), contains("CAR"), contains("CAO"))

var_names <- names(tract_housing_data_1960)
var_names[startsWith(var_names, "CAR")] <- sub("^CAR", "rent_group", var_names[startsWith(var_names, "CAR")])
var_names[startsWith(var_names, "CAO")] <- sub("^CAO", "home_value_group", var_names[startsWith(var_names, "CAO")])
names(tract_housing_data_1960) <- var_names


# vacant units, units by age, and units by condition

tract_housing_data_1960_add <-
  full_tract_data_1960_add %>% 
  mutate(vacant_units = B9R001 + B9R002 + B9R003,
         total_units = CA5001,
         units_dilapidated = B9E007,
         units_deteriorating = B9E005, 
         age_group001 = B96001, # units_0_to_10y_old
         age_group002 = B96002, #units_10_to_20y_old
         age_group003 = B96003) %>%  #units_20yplus_old
  # keep only variables of interest
  select(YEAR, STATEA, TRACTA, COUNTYA, GISJOIN2, vacant_units, 
         total_units, units_dilapidated, units_deteriorating,
         contains("age_group")) %>% 
  st_drop_geometry()

# merge on
tract_housing_data_1960 <-
  left_join(tract_housing_data_1960, tract_housing_data_1960_add)

### 1970 -----

full_tract_data_1970 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1970/nhgis0034_ds95_1970_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1970/US_tract_1970.shp",
      file_select = starts_with("US_tract_1970")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

# data on total units, vacant units, units by age
full_tract_data_1970_add1 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1970/nhgis0038_ds95_1970_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1970/US_tract_1970.shp",
      file_select = starts_with("US_tract_1970")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

full_tract_data_1970_add2 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1970/nhgis0038_ds96_1970_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1970/US_tract_1970.shp",
      file_select = starts_with("US_tract_1970")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

full_tract_data_1970_add3 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1970/nhgis0038_ds97_1970_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1970/US_tract_1970.shp",
      file_select = starts_with("US_tract_1970")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----
tract_housing_data_1970 <-
  full_tract_data_1970 %>% 
  select(any_of(tract_background_variables), contains("CG7"), contains("CHA")) 

# rename variables home_value_group and rent_group
var_names <- names(tract_housing_data_1970)
var_names[startsWith(var_names, "CHA")] <- sub("^CHA", "rent_group", var_names[startsWith(var_names, "CHA")])
var_names[startsWith(var_names, "CG7")] <- sub("^CG7", "home_value_group", var_names[startsWith(var_names, "CG7")])
names(tract_housing_data_1970) <- var_names

# vacant units
tract_housing_data_1970_add1 <-
  full_tract_data_1970_add1 %>% 
  mutate(vacant_year_round_units = CE9002 + CE9003 + CE9004 + CE9005 + CE9006,
         total_year_round_units = CE9001 + vacant_year_round_units) %>% 
  select(YEAR, STATEA, TRACTA, COUNTYA, GISJOIN2, vacant_year_round_units, total_year_round_units) %>% 
  st_drop_geometry()

# total units
tract_housing_data_1970_add2 <-
  full_tract_data_1970_add2 %>% 
  select(YEAR, STATEA, TRACTA, COUNTYA, GISJOIN2, total_units = CK0001) %>% 
  st_drop_geometry()

# units by age
tract_housing_data_1970_add3 <-
  full_tract_data_1970_add3 %>% 
  mutate(age_group001 = CZ2001, # year_round_units_0_to_1y_old
         age_group002 = CZ2002, # year_round_units_2_to_5y_old
         age_group003 = CZ2003, # year_round_units_6_to_10y_old
         age_group004 = CZ2004, # year_round_units_11_to_20y_old
         age_group005 = CZ2005, # year_round_units_21_to_30y_old
         age_group006 = CZ2006) %>% # year_round_units_31yplus_old
  select(YEAR, STATEA, TRACTA, COUNTYA, GISJOIN2, contains("age_group")) %>% 
  st_drop_geometry()

# merge together
tract_housing_data_1970 <-
  tract_housing_data_1970 %>% 
  left_join(tract_housing_data_1970_add1) %>% 
  left_join(tract_housing_data_1970_add2) %>% 
  left_join(tract_housing_data_1970_add3)

### 1980 -----

full_tract_data_1980 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1980/nhgis0044_ds104_1980_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1980/US_tract_1980.shp",
      file_select = starts_with("US_tract_1980")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

full_tract_data_1980_add1 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1980/nhgis0038_ds104_1980_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1980/US_tract_1980.shp",
      file_select = starts_with("US_tract_1980")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

full_tract_data_1980_add2 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1980/nhgis0038_ds107_1980_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1980/US_tract_1980.shp",
      file_select = starts_with("US_tract_1980")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))


#### harmonization -----

tract_housing_data_1980 <- 
  full_tract_data_1980 %>% 
  dplyr::rename(median_home_value_reported = C8J001, median_rent_reported = C8O001) %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), median_home_value_reported, median_rent_reported, contains("C8N"), contains("C8I")) 

# rename variables home_value_group and rent_group
var_names <- names(tract_housing_data_1980)
var_names[startsWith(var_names, "C8N")] <- sub("^C8N", "rent_group", var_names[startsWith(var_names, "C8N")])
var_names[startsWith(var_names, "C8I")] <- sub("^C8I", "home_value_group", var_names[startsWith(var_names, "C8I")])
names(tract_housing_data_1980) <- var_names


# vacant units and total units
tract_housing_data_1980_add1 <-
  full_tract_data_1980_add1 %>% 
  mutate(vacant_year_round_units = C9B002,
         total_year_round_units = C9A001,
         total_units = C8Y001) %>% 
  select(YEAR, STATEA, TRACTA, COUNTYA, GISJOIN2, vacant_year_round_units, total_year_round_units, total_units) %>% 
  st_drop_geometry()

# units by age
tract_housing_data_1980_add2 <-
  full_tract_data_1980_add2 %>% 
  mutate(age_group001 = DEQ001, # year_round_units_0_to_1y_old
         age_group002 = DEQ002, # year_round_units_2_to_5y_old
         age_group003 = DEQ003, # year_round_units_6_to_10y_old
         age_group004 = DEQ004, # year_round_units_11_to_20y_old
         age_group005 = DEQ005, # year_round_units_21_to_30y_old
         age_group006 = DEQ006, # year_round_units_31_to_40y_old
         age_group007 = DEQ007) %>% # year_round_units_41yplus_old
  select(YEAR, STATEA, TRACTA, COUNTYA, GISJOIN2, contains("age_group")) %>% 
  st_drop_geometry()

# merge
tract_housing_data_1980 <-
  tract_housing_data_1980 %>% 
  left_join(tract_housing_data_1980_add1) %>% 
  left_join(tract_housing_data_1980_add2)

### 1990 -----

full_tract_data_1990 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1990/nhgis0044_ds120_1990_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1990/US_tract_1990.shp",
      file_select = starts_with("US_tract_1990")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

full_tract_data_1990_add1 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/housing/1990/nhgis0038_ds120_1990_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1990/US_tract_1990.shp",
      file_select = starts_with("US_tract_1990")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----

tract_housing_data_1990 <- 
  full_tract_data_1990 %>% 
  dplyr::rename(median_home_value_reported = EST001, median_rent_reported = ES6001) %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), median_home_value_reported,
         median_rent_reported, contains("ES4"), contains("ESR")) 

# rename variables home_value_group and rent_group
var_names <- names(tract_housing_data_1990)
var_names[startsWith(var_names, "ES4")] <- sub("^ES4", "rent_group", var_names[startsWith(var_names, "ES4")])
var_names[startsWith(var_names, "ESR")] <- sub("^ESR", "home_value_group", var_names[startsWith(var_names, "ESR")])
names(tract_housing_data_1990) <- var_names


# vacant units and total units
tract_housing_data_1990_add1 <-
  full_tract_data_1990_add1 %>% 
  mutate(vacant_units = ESN002,
         total_units = ESA001) %>% 
  select(YEAR, STATEA, TRACTA, COUNTYA, GISJOIN2, vacant_units, total_units) %>% 
  st_drop_geometry()

# merge
tract_housing_data_1990 <-
  tract_housing_data_1990 %>% 
  left_join(tract_housing_data_1990_add1)


# Concord datasets to 1950 Census tracts ----
# Have to do this separately for years for which we are given medians for rents and housing values (1940, 1950, 1980) 
# and for which we have to calculate medians (1930, 1960, 1970)
# for the latter, we should reweight populations in each group

# still, can loop through them all the same

# 04/30/2024: Actually... maybe I can just do this for all years at once... trying now

# 1. Join on crosswalk to get GISJOIN_1950 and weights
# 2. Weight medians (or group values) by "weight" and collapse to GISJOIN_2000
# 3. Merge geography information from 1950 NHGIS file

years <- c(1930, 1940, 1960, 1970, 1980, 1990)

for (year in years) {
  # Construct variable names and file names dynamically based on the year
  tract_housing_data_var <- paste0("tract_housing_data_", year)
  tract_crosswalk_var <- paste0("tract_crosswalk_", year)
  tract_housing_data_concorded_var <- paste0("tract_housing_data_", year, "_concorded")
  
  # Execute the operations inside the loop
  assign(tract_housing_data_concorded_var, 
         get(tract_housing_data_var) %>% 
           # join on crosswalk
           left_join(get(tract_crosswalk_var), by = c("GISJOIN" = paste0("GISJOIN_", year))) %>%
           # weight populations
           mutate_at(vars(contains(c("rent", "home_value", "units", "age_group"))), ~ . * weight) %>%
           st_drop_geometry() %>% 
           # collapse to GISJOIN_1950
           group_by(GISJOIN_1950, YEAR) %>% 
           summarise_at(vars(contains(c("rent", "home_value", "units", "age_group"))), sum, na.rm = TRUE) %>%
           ungroup()  %>%
           # merge on 1950 tract IDs for each GISJOIN 
           left_join(tract_info_1950_gisjoin, by = c("GISJOIN_1950" = "GISJOIN")) 
  )
}

## Clean 1950 data to same format as concorded data  ----
tract_housing_data_1950_concorded <-
  tract_housing_data_1950 %>% 
  ungroup() %>% # not sure why I needed this
  dplyr::rename(GISJOIN_1950 = GISJOIN) %>% 
  select(GISJOIN_1950, YEAR, contains(c("rent", "home_value", "units", "age_group")))


# Calculate median house value and rents in each year, for years for which I just have populations ----
## for 1930, 1960, 1970, I have populations by group of house value and rent, so I need to calculate the median 
# after the concordance

## 1930 ----
median_rent_1930 <- 
  calculate_median_from_census(tract_housing_data_1930_concorded, var_code = "rent_group", var_name = "median_rent_group")

median_home_value_1930 <-
  calculate_median_from_census(tract_housing_data_1930_concorded, var_code = "home_value_group", var_name = "median_home_value_group")

tract_housing_data_1930_concorded <-
  tract_housing_data_1930_concorded %>% 
  # keep only variables of interest
  select(-contains("rent_group"), -contains("home_value_group")) %>%
  left_join(median_rent_1930, by = c("YEAR", "GISJOIN_1950")) %>% 
  left_join(median_home_value_1930, by = c("YEAR", "GISJOIN_1950")) %>%
  # replace the medians with the label of the variable it represents (midpoint values except last value)
  mutate(median_home_value_calculated =
           case_when(median_home_value_group == "home_value_group001" ~ 500, # 0 - 1000
                     median_home_value_group == "home_value_group002" ~ 1500, # 1000 - 2000
                     median_home_value_group == "home_value_group003" ~ 2500, # 2000 - 3000
                     median_home_value_group == "home_value_group004" ~ 3500, # 3000 - 4000
                     median_home_value_group == "home_value_group005" ~ 4500, # 4000 - 5000
                     median_home_value_group == "home_value_group006" ~ 6250, # 5000 - 7500
                     median_home_value_group == "home_value_group007" ~ 8750, # 7500 - 10000
                     median_home_value_group == "home_value_group008" ~ 12500, # 10000 - 15000
                     median_home_value_group == "home_value_group009" ~ 17500, # 15000 - 20000
                     median_home_value_group == "home_value_group010" ~ 20000, # 20000+
                     median_home_value_group == "home_value_group011" ~ NA),
         median_rent_calculated = 
           case_when(median_rent_group == "rent_group001" ~ 5, # 0-10
                     median_rent_group == "rent_group002" ~ 12.5, # 10-15
                     median_rent_group == "rent_group003" ~ 17.5, # 15-20
                     median_rent_group == "rent_group004" ~ 25, # 20-30
                     median_rent_group == "rent_group005" ~ 40, # 30-50
                     median_rent_group == "rent_group006" ~ 62.5, # 50-75
                     median_rent_group == "rent_group007" ~ 87.5, # 75-100
                     median_rent_group == "rent_group008" ~ 125, # 100-150
                     median_rent_group == "rent_group009" ~ 175, # 150-200
                     median_rent_group == "rent_group010" ~ 200, # 200+
                     median_rent_group == "rent_group011" ~ NA))


## 1940 ------
median_rent_1940 <- 
  calculate_median_from_census(tract_housing_data_1940_concorded, var_code = "rent_group", var_name = "median_rent_group")

median_home_value_1940 <-
  calculate_median_from_census(tract_housing_data_1940_concorded, var_code = "home_value_group", var_name = "median_home_value_group")

temp <- function(first, last) {
  return((last - first)/2 + first)
}
temp(1500,1999)


tract_housing_data_1940_concorded <-
  tract_housing_data_1940_concorded %>% 
  # keep only variables of interest
  select(-contains("rent_group"), -contains("home_value_group")) %>%
  left_join(median_rent_1940, by = c("YEAR", "GISJOIN_1950")) %>% 
  left_join(median_home_value_1940, by = c("YEAR", "GISJOIN_1950")) %>%
  # replace the medians with the label of the variable it represents (midpoint values except last value)
  mutate(median_home_value_calculated =
           case_when(median_home_value_group == "home_value_group001" ~ 250, # 0 - 500
                     median_home_value_group == "home_value_group002" ~ 599.5, # 500-699
                     median_home_value_group == "home_value_group003" ~ 799.5, # 700-999
                     median_home_value_group == "home_value_group004" ~ 1249.5, # 1000-1499
                     median_home_value_group == "home_value_group005" ~ 1749.5, # 1500-1999
                     median_home_value_group == "home_value_group006" ~ 2249.5, # 2000-2499
                     median_home_value_group == "home_value_group007" ~ 2749.5, # 2500-2999
                     median_home_value_group == "home_value_group008" ~ 3499.5, # 3000-3999
                     median_home_value_group == "home_value_group009" ~ 4499.5, # 4000-4999
                     median_home_value_group == "home_value_group010" ~ 5499.5, # 5000-5999
                     median_home_value_group == "home_value_group011" ~ 6749.5, # 6000-7499
                     median_home_value_group == "home_value_group012" ~ 8749.5, # 7500-9999
                     median_home_value_group == "home_value_group013" ~ 12499.5, # 10000-14999
                     median_home_value_group == "home_value_group014" ~ 17499.5, # 15000-19999
                     median_home_value_group == "home_value_group015" ~ 20000, # 20000+
                     median_home_value_group == "home_value_group011" ~ NA),
         median_rent_calculated = 
           case_when(median_rent_group == "rent_group001" ~ 2.5, # 0-5
                     median_rent_group == "rent_group002" ~ 5.5, # 5-6
                     median_rent_group == "rent_group003" ~ 8, # 7-9
                     median_rent_group == "rent_group004" ~ 12, # 10-14
                     median_rent_group == "rent_group005" ~ 17, # 15-19
                     median_rent_group == "rent_group006" ~ 22, # 20-24
                     median_rent_group == "rent_group007" ~ 27, # 25-29
                     median_rent_group == "rent_group008" ~ 34.5, # 30-39
                     median_rent_group == "rent_group009" ~ 44.5, # 40-49
                     median_rent_group == "rent_group010" ~ 54.5, # 50-59
                     median_rent_group == "rent_group011" ~ 67, #60-74
                     median_rent_group == "rent_group012" ~ 87, # 75-99
                     median_rent_group == "rent_group013" ~ 124.5, # 100-149
                     median_rent_group == "rent_group014" ~ 174.5, # 150-199
                     median_rent_group == "rent_group015" ~ 200)) # 200+

## 1950 -----
median_rent_1950 <- 
  calculate_median_from_census(tract_housing_data_1950_concorded,
                               var_code = "rent_group",
                               var_name = "median_rent_group")

median_home_value_1950 <-
  calculate_median_from_census(tract_housing_data_1950_concorded,
                               var_code = "home_value_group",
                               var_name = "median_home_value_group")

tract_housing_data_1950_concorded <-
  tract_housing_data_1950_concorded %>% 
  # keep only variables of interest
  select(-contains("rent_group"), -contains("home_value_group")) %>%
  left_join(median_rent_1950, by = c("YEAR", "GISJOIN_1950")) %>% 
  left_join(median_home_value_1950, by = c("YEAR", "GISJOIN_1950")) %>%
  # replace the medians with the label of the variable it represents (midpoint values except last value)
  mutate(median_home_value_calculated =
           case_when(median_home_value_group == "home_value_group001" ~ 1500, # 0 - 3000
                     median_home_value_group == "home_value_group002" ~ 3499.5, # 3000-3999
                     median_home_value_group == "home_value_group003" ~ 4499.5, # 4000-4999
                     median_home_value_group == "home_value_group004" ~ 6249.5, # 5000-7499
                     median_home_value_group == "home_value_group005" ~ 8749.5, # 7500-9999
                     median_home_value_group == "home_value_group006" ~ 12499.5, # 10000-14999
                     median_home_value_group == "home_value_group007" ~ 15000), # 15000+
         median_rent_calculated = 
           case_when(median_rent_group == "rent_group001" ~ 5, # 0-10
                     median_rent_group == "rent_group002" ~ 14.5, # 10-19
                     median_rent_group == "rent_group003" ~ 24.5, # 20-29
                     median_rent_group == "rent_group004" ~ 34.5, # 30-39
                     median_rent_group == "rent_group005" ~ 44.5, # 40-49
                     median_rent_group == "rent_group006" ~ 54.5, # 50-59
                     median_rent_group == "rent_group007" ~ 67, # 60-74
                     median_rent_group == "rent_group008" ~ 87, # 75-99
                     median_rent_group == "rent_group009" ~ 100)) # 100+

## 1960 -----
median_rent_1960 <- 
  calculate_median_from_census(tract_housing_data_1960_concorded, var_code = "rent_group", var_name = "median_rent_group")

median_home_value_1960 <-
  calculate_median_from_census(tract_housing_data_1960_concorded, var_code = "home_value_group", var_name = "median_home_value_group")

tract_housing_data_1960_concorded <-
  tract_housing_data_1960_concorded %>% 
  # keep only variables of interest
  select(-contains("rent_group"), -contains("home_value_group")) %>%
  left_join(median_rent_1960, by = c("YEAR", "GISJOIN_1950")) %>% 
  left_join(median_home_value_1960, by = c("YEAR", "GISJOIN_1950")) %>%
  # replace the medians with the label of the variable it represents (midpoint values, except the last point)
  mutate(median_home_value_calculated =
           case_when(median_home_value_group == "home_value_group001" ~ 2500, # 0 - 5000
                     median_home_value_group == "home_value_group002" ~ 6250, # 5000 - 7500
                     median_home_value_group == "home_value_group003" ~ 8750, # 7500 - 10000
                     median_home_value_group == "home_value_group004" ~ 11250, # 10000 - 12500
                     median_home_value_group == "home_value_group005" ~ 13750, # 12500 - 15000
                     median_home_value_group == "home_value_group006" ~ 16250, # 15000 - 17500
                     median_home_value_group == "home_value_group007" ~ 18750, # 17500 - 20000
                     median_home_value_group == "home_value_group008" ~ 22500, # 20000 - 25000
                     median_home_value_group == "home_value_group009" ~ 30000, # 25000 - 35000
                     median_home_value_group == "home_value_group010" ~ 35000), # 35000+
         median_rent_calculated = 
           case_when(median_rent_group == "rent_group001" ~ 10, # 0-20
                     median_rent_group == "rent_group002" ~ 25, # 20-30
                     median_rent_group == "rent_group003" ~ 35, # 30-40
                     median_rent_group == "rent_group004" ~ 45, # 40-50
                     median_rent_group == "rent_group005" ~ 55, # 50-60
                     median_rent_group == "rent_group006" ~ 65, # 60-70
                     median_rent_group == "rent_group007" ~ 75, # 70-80
                     median_rent_group == "rent_group008" ~ 85, # 80-90
                     median_rent_group == "rent_group009" ~ 95, # 90-100
                     median_rent_group == "rent_group010" ~ 110, # 100-120
                     median_rent_group == "rent_group011" ~ 135, # 120-150
                     median_rent_group == "rent_group012" ~ 175, # 150-200
                     median_rent_group == "rent_group013" ~ 200)) # 200+

## 1970 -----
median_rent_1970 <- 
  calculate_median_from_census(tract_housing_data_1970_concorded, var_code = "rent_group", var_name = "median_rent_group")

median_home_value_1970 <-
  calculate_median_from_census(tract_housing_data_1970_concorded, var_code = "home_value_group", var_name = "median_home_value_group")

tract_housing_data_1970_concorded <-
  tract_housing_data_1970_concorded %>% 
  # keep only variables of interest
  select(-contains("rent_group"), -contains("home_value_group")) %>%
  left_join(median_rent_1970, by = c("YEAR", "GISJOIN_1950")) %>% 
  left_join(median_home_value_1970, by = c("YEAR", "GISJOIN_1950")) %>%
  # replace the medians with the label of the variable it represents (mid point values, except the last)
  mutate(median_home_value_calculated =
           case_when(median_home_value_group == "home_value_group001" ~ 2500, # 0-5000
                     median_home_value_group == "home_value_group002" ~ 6250, # 5000-7500
                     median_home_value_group == "home_value_group003" ~ 8750, # 7500-10000
                     median_home_value_group == "home_value_group004" ~ 11250, # 10000-12500
                     median_home_value_group == "home_value_group005" ~ 12500, # 12500-15000
                     median_home_value_group == "home_value_group006" ~ 16250, # 15000-17500
                     median_home_value_group == "home_value_group007" ~ 18750, # 17500-20000
                     median_home_value_group == "home_value_group008" ~ 22500, # 20000-25000
                     median_home_value_group == "home_value_group009" ~ 30000, # 25000-35000
                     median_home_value_group == "home_value_group010" ~ 42500, # 35000-50000 
                     median_home_value_group == "home_value_group011" ~ 50000), # 50000+
         median_rent_calculated = 
           case_when(median_rent_group == "rent_group001" ~ 15, # 0-30
                     median_rent_group == "rent_group002" ~ 35, # 30-40
                     median_rent_group == "rent_group003" ~ 45, # 40-50
                     median_rent_group == "rent_group004" ~ 55, # 50-60
                     median_rent_group == "rent_group005" ~ 65, # 60-70
                     median_rent_group == "rent_group006" ~ 75, # 70-80
                     median_rent_group == "rent_group007" ~ 85, # 80-90
                     median_rent_group == "rent_group008" ~ 95, # 90-100
                     median_rent_group == "rent_group009" ~ 110, # 100-120
                     median_rent_group == "rent_group010" ~ 135, # 120-150
                     median_rent_group == "rent_group011" ~ 175, # 150-200
                     median_rent_group == "rent_group012" ~ 225, # 200-250
                     median_rent_group == "rent_group013" ~ 275, # 250-300
                     median_rent_group == "rent_group014" ~ 300, # 300+
                     median_rent_group == "rent_group015" ~ NA))

## 1980 -----
median_rent_1980 <- 
  calculate_median_from_census(tract_housing_data_1980_concorded, var_code = "rent_group", var_name = "median_rent_group")

median_home_value_1980 <-
  calculate_median_from_census(tract_housing_data_1980_concorded, var_code = "home_value_group", var_name = "median_home_value_group")

tract_housing_data_1980_concorded <-
  tract_housing_data_1980_concorded %>% 
  # keep only variables of interest
  select(-contains("rent_group"), -contains("home_value_group")) %>%
  left_join(median_rent_1980, by = c("YEAR", "GISJOIN_1950")) %>% 
  left_join(median_home_value_1980, by = c("YEAR", "GISJOIN_1950")) %>%
  # replace the medians with the label of the variable it represents (mid point values, except the last)
  mutate(median_home_value_calculated =
           case_when(median_home_value_group == "home_value_group001" ~ 10000, # 0-10000
                     median_home_value_group == "home_value_group002" ~ 12499.5, # 10000-14999
                     median_home_value_group == "home_value_group003" ~ 17499.5, # 15000-19999
                     median_home_value_group == "home_value_group004" ~ 22499.5, # 20000-24999
                     median_home_value_group == "home_value_group005" ~ 27499.5, # 25000-29999
                     median_home_value_group == "home_value_group006" ~ 32499.5, # 30000-34999
                     median_home_value_group == "home_value_group007" ~ 37499.5, # 35000-39999
                     median_home_value_group == "home_value_group008" ~ 44999.5, # 40000-49999
                     median_home_value_group == "home_value_group009" ~ 64999.5, # 50000-79999
                     median_home_value_group == "home_value_group010" ~ 89999.5, # 80000-99999
                     median_home_value_group == "home_value_group011" ~ 124999.5, # 100000-149999
                     median_home_value_group == "home_value_group012" ~ 174999.5, # 150000-199999
                     median_home_value_group == "home_value_group013" ~ 200000, # 200000+
                     ),
         median_rent_calculated = 
           case_when(median_rent_group == "rent_group001" ~ 25, # 0-49
                     median_rent_group == "rent_group002" ~ 74.5, # 50-99
                     median_rent_group == "rent_group003" ~ 109.5, # 100-119
                     median_rent_group == "rent_group004" ~ 129.5, # 120-139
                     median_rent_group == "rent_group005" ~ 144.5, # 140-149
                     median_rent_group == "rent_group006" ~ 154.5, # 150-159
                     median_rent_group == "rent_group007" ~ 164.5, # 160-169
                     median_rent_group == "rent_group008" ~ 184.5, # 170-199
                     median_rent_group == "rent_group009" ~ 224.5, # 200-249
                     median_rent_group == "rent_group010" ~ 274.5, # 250-299
                     median_rent_group == "rent_group011" ~ 349.5, # 300-399
                     median_rent_group == "rent_group012" ~ 449.5, # 400-499
                     median_rent_group == "rent_group013" ~ 500, # 500+
                     median_rent_group == "rent_group014" ~ NA))
  

## 1990 -----
median_rent_1990 <- 
  calculate_median_from_census(tract_housing_data_1990_concorded, var_code = "rent_group", var_name = "median_rent_group")

median_home_value_1990 <-
  calculate_median_from_census(tract_housing_data_1990_concorded, var_code = "home_value_group", var_name = "median_home_value_group")

tract_housing_data_1990_concorded <-
  tract_housing_data_1990_concorded %>% 
  # keep only variables of interest
  select(-contains("rent_group"), -contains("home_value_group")) %>%
  left_join(median_rent_1990, by = c("YEAR", "GISJOIN_1950")) %>% 
  left_join(median_home_value_1990, by = c("YEAR", "GISJOIN_1950")) %>%
  # replace the medians with the label of the variable it represents (mid point values, except the last)
  mutate(median_home_value_calculated =
           case_when(median_home_value_group == "home_value_group001" ~ 7499.5, # 0-14999
                     median_home_value_group == "home_value_group002" ~ 17499.5, # 15000-19999
                     median_home_value_group == "home_value_group003" ~ 22499.5, # 20000-24999
                     median_home_value_group == "home_value_group004" ~ 27499.5, # 25000-29999
                     median_home_value_group == "home_value_group005" ~ 32499.5, # 30000-34999
                     median_home_value_group == "home_value_group006" ~ 37499.5, # 35000-39999
                     median_home_value_group == "home_value_group007" ~ 42499.5, # 40000-44999
                     median_home_value_group == "home_value_group008" ~ 47499.5, # 45000-49999
                     median_home_value_group == "home_value_group009" ~ 54999.5, # 50000-59999
                     median_home_value_group == "home_value_group010" ~ 67499.5, # 60000-74999
                     median_home_value_group == "home_value_group011" ~ 87499.5, # 75000-99999
                     median_home_value_group == "home_value_group012" ~ 112499.5, # 100000-124999
                     median_home_value_group == "home_value_group013" ~ 137499.5, # 125000-149999
                     median_home_value_group == "home_value_group014" ~ 162499.5, # 150000-174999
                     median_home_value_group == "home_value_group015" ~ 187499.5, # 175000-199999
                     median_home_value_group == "home_value_group016" ~ 224999.5, # 200000-249999
                     median_home_value_group == "home_value_group017" ~ 274999.5, # 250000-299999
                     median_home_value_group == "home_value_group018" ~ 349999.5, # 300000-399999
                     median_home_value_group == "home_value_group019" ~ 449999.5, # 400000-499999
                     median_home_value_group == "home_value_group020" ~ 500000, # 500000+
                     ),
         median_rent_calculated = 
           case_when(median_rent_group == "rent_group001" ~ 100, #100
                     median_rent_group == "rent_group002" ~ 124.5,  #100 to 149
                     median_rent_group == "rent_group003" ~ 174.5,  #150 to 199
                     median_rent_group == "rent_group004" ~ 224.5,  #200 to 249
                     median_rent_group == "rent_group005" ~ 274.5,  #250 to 299
                     median_rent_group == "rent_group006" ~ 324.5,  #300 to 349
                     median_rent_group == "rent_group007" ~ 374.5,  #350 to 399
                     median_rent_group == "rent_group008" ~ 424.5,  #400 to 449
                     median_rent_group == "rent_group009" ~ 499.5,  #450 to 499
                     median_rent_group == "rent_group010" ~ 524.5,  #500 to 549
                     median_rent_group == "rent_group011" ~ 574.5,  #550 to 599
                     median_rent_group == "rent_group012" ~ 624.5,  #600 to 649
                     median_rent_group == "rent_group013" ~ 674.5,  #650 to 699
                     median_rent_group == "rent_group014" ~ 724.5,  #700 to 749
                     median_rent_group == "rent_group015" ~ 874.5,  #750 to 999
                     median_rent_group == "rent_group016" ~ 1000, #1000+
                     median_rent_group == "rent_group017" ~ NA)
         )

# Calculate median house age ----
# not available in 1940

## 1950 -----
median_house_age_1950 <-
  calculate_median_from_census(tract_housing_data_1950_concorded, var_code = "age_group", var_name = "median_housing_age_group")

tract_housing_data_1950 <-
  tract_housing_data_1950_concorded %>% 
  # keep only variables of interest
  select(-contains("age_group")) %>%
  left_join(median_house_age_1950, by = c("YEAR", "GISJOIN_1950")) %>%
  # replace the medians with the label of the variable it represents (mid point values, except the last)
  mutate(median_housing_age =
           case_when(median_housing_age_group == "age_group001" ~ 5.5, # 0-10
                     median_housing_age_group == "age_group002" ~ 15.5, # 11 to 20
                     median_housing_age_group == "age_group003" ~ 25.5, # 21 to 30
                     median_housing_age_group == "age_group004" ~ 31)) # 31+

## 1960 ----
median_house_age_1960 <-
  calculate_median_from_census(tract_housing_data_1960_concorded, var_code = "age_group", var_name = "median_housing_age_group")

tract_housing_data_1960_concorded <-
  tract_housing_data_1960_concorded %>% 
  # keep only variables of interest
  select(-contains("age_group")) %>%
  left_join(median_house_age_1960, by = c("YEAR", "GISJOIN_1950")) %>%
  # replace the medians with the label of the variable it represents (mid point values, except the last)
  mutate(median_housing_age =
           case_when(median_housing_age_group == "age_group001" ~ 5.5,  # 0 to 10
                     median_housing_age_group == "age_group002" ~ 15.5, # 11 to 20 
                     median_housing_age_group == "age_group003" ~ 21))  # 21+ 

## 1970 ----
median_house_age_1970 <-
  calculate_median_from_census(tract_housing_data_1970_concorded, var_code = "age_group", var_name = "median_housing_age_group")

tract_housing_data_1970_concorded <-
  tract_housing_data_1970_concorded %>% 
  # keep only variables of interest
  select(-contains("age_group")) %>%
  left_join(median_house_age_1970, by = c("YEAR", "GISJOIN_1950")) %>%
  # replace the medians with the label of the variable it represents (mid point values, except the last)
  mutate(median_housing_age =
           case_when(median_housing_age_group == "age_group001" ~ 0.5,  # 0 to 1
                     median_housing_age_group == "age_group002" ~ 3.5, # 2 to 5
                     median_housing_age_group == "age_group003" ~ 8, # 6 to 10 
                     median_housing_age_group == "age_group004" ~ 15.5, # 11 to 20
                     median_housing_age_group == "age_group005" ~ 25.5, # 21 to 30
                     median_housing_age_group == "age_group006" ~ 31)) # 31 plus
                     
## 1980 -----
median_house_age_1980 <-
  calculate_median_from_census(tract_housing_data_1980_concorded, var_code = "age_group", var_name = "median_housing_age_group")

tract_housing_data_1980_concorded <-
  tract_housing_data_1980_concorded %>% 
  # keep only variables of interest
  select(-contains("age_group")) %>%
  left_join(median_house_age_1980, by = c("YEAR", "GISJOIN_1950")) %>%
  # replace the medians with the label of the variable it represents (mid point values, except the last)
  mutate(median_housing_age =
           case_when(median_housing_age_group == "age_group001" ~ 0.5,  # 0 to 1
                     median_housing_age_group == "age_group002" ~ 3.5, # 2 to 5
                     median_housing_age_group == "age_group003" ~ 8, # 6 to 10 
                     median_housing_age_group == "age_group004" ~ 15.5, # 11 to 20
                     median_housing_age_group == "age_group005" ~ 25.5, # 21 to 30
                     median_housing_age_group == "age_group006" ~ 35.5, # 31 to 40
                     median_housing_age_group == "age_group006" ~ 41)) # 41 plus


# Calculate vacancy rates, housing density, and share needing repairs -----
## 1940 -----
tract_housing_data_1940_concorded <-
  tract_housing_data_1940_concorded %>% 
  mutate(vacancy_rate = vacant_units/total_units,
         # share needing repair: units needing repair/units reporting condition
         share_needing_repair = units_needing_repairs/units_reporting_condition,
         # share no water
         share_no_water = units_no_water/units_reporting_condition,
         # housing density: Units per square meter
         housing_density = total_units/area_m2)

## 1950 ----
tract_housing_data_1950_concorded <-
  tract_housing_data_1950_concorded %>% 
  mutate(
    # calculate area
    area_m2 = st_area(geometry),
    vacancy_rate = vacant_units/total_units,
         # share no running water
         share_no_water = units_no_running_water/units_reporting_condition,
         # housing density: Units per square meter
         housing_density = total_units/area_m2)

## 1960 ----
tract_housing_data_1960_concorded <-
  tract_housing_data_1960_concorded %>% 
  mutate(vacancy_rate = vacant_units/total_units,
         # share needing repairs: share dilapidated or deteriorating
         share_needing_repair = (units_dilapidated + units_deteriorating)/total_units,
         # housing density
         housing_density = total_units/area_m2)


## 1970 ----
tract_housing_data_1970_concorded <-
  tract_housing_data_1970_concorded %>% 
  mutate(
    # vacancy rate is vacancy rate of year-round units
    vacancy_rate = vacant_year_round_units/total_year_round_units,
    # housing density
    housing_density = total_units/area_m2)

## 1980 ----

tract_housing_data_1980_concorded <-
  tract_housing_data_1980_concorded %>% 
  mutate(
    # vacancy rate is vacancy rate of year-round units
    vacancy_rate = vacant_year_round_units/total_year_round_units,
   # housing density
   housing_density = total_units/area_m2)


## 1990 ----

tract_housing_data_1990_concorded <-
  tract_housing_data_1990_concorded %>% 
  mutate(
    # vacancy rate is vacancy rate of year-round units
    vacancy_rate = vacant_units/total_units,
    # housing density
    housing_density = total_units/area_m2)

# Combine tract-level datasets ----

# data with the original tracts
# might use this to compare at some point
tract_housing_data_original_tracts <-
  bind_rows(tract_housing_data_1930, tract_housing_data_1940,
            tract_housing_data_1950, tract_housing_data_1960, tract_housing_data_1970, 
            tract_housing_data_1980, tract_housing_data_1990) %>%
  select(any_of(tract_background_variables), contains("median")) %>% 
  # drop rows with missing geometry
  filter(!st_is_empty(geometry))

# tract population data concorded to 1990 tracts
tract_housing_data_concorded <-
  bind_rows(tract_housing_data_1930_concorded, tract_housing_data_1940_concorded,
            tract_housing_data_1950_concorded, tract_housing_data_1960_concorded,
            tract_housing_data_1970_concorded, tract_housing_data_1980_concorded,
            tract_housing_data_1990_concorded) %>%
  select(GISJOIN_1950, YEAR, contains("median"), total_units, total_units, vacancy_rate,
         share_needing_repair, share_no_water, housing_density, geometry) %>% 
  # create median_rent and median_home_value composite variables
  mutate(median_rent_calculated = ifelse(is.na(median_rent_calculated),
                                         median_rent_reported,
                                         median_rent_calculated),
         median_home_value_calculated = ifelse(is.na(median_home_value_calculated),
                                               median_home_value_reported,
                                               median_home_value_calculated)) %>% 
  mutate(median_rent_reported = ifelse(is.na(median_rent_reported),
                                       median_rent_calculated,
                                       median_rent_reported),
         median_home_value_reported = ifelse(is.na(median_home_value_reported),
                                             median_home_value_calculated,
                                             median_home_value_reported)) %>% 
  # calculate an alternative reported median rent, where values less than 5 are replaced by the calculated value
  mutate(median_rent_reported_alt = ifelse(median_rent_reported < 5,
                                       median_rent_calculated,
                                       median_rent_reported)) %>%
  mutate(median_home_value_reported_alt = ifelse(median_home_value_reported == 0,
                                                 median_home_value_calculated,
                                                 median_home_value_reported)) %>% 
  # drop rows with missing geometry
  filter(!st_is_empty(geometry))


# Save ----
# save concorded tract_housing_data as shapefile
st_write(tract_housing_data_concorded, "data/derived/census/tract_housing_data.gpkg",
                   append = FALSE, layer = "points", driver = "GPKG")


# Analysis-----
# create time series of average median rent per year
median_rent_time_series <-
  tract_housing_data_concorded %>% 
  select(YEAR, median_rent_reported_alt) %>% 
  group_by(YEAR) %>% 
  summarise(median_rent = mean(median_rent_reported_alt, na.rm = TRUE))

ggplot(median_rent_time_series, aes(x = YEAR, y = median_rent)) +
  geom_line() +
  labs(title = "Average Median Rent by Year",
       x = "Year",
       y = "Median Rent") +
  theme_minimal()


## compare median rents calculated vs reported in years for which we have both ----
## 1990
tract_housing_data_concorded %>%
  filter(YEAR == 1990) %>%
  select(median_rent_reported_alt, median_rent_calculated) %>%
  pivot_longer(cols = c(median_rent_reported_alt, median_rent_calculated), names_to = "type", values_to = "rent") %>%
  ggplot(aes(x = rent, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Comparison of Calculated vs Reported Median Rents (1990)",
       x = "Median Rent",
       fill = "Rent Type") +
  theme_minimal()


## 1980 
tract_housing_data_concorded %>%
  filter(YEAR == 1980) %>%
  select(median_rent_reported_alt, median_rent_calculated) %>%
  pivot_longer(cols = c(median_rent_reported_alt, median_rent_calculated), names_to = "type", values_to = "rent") %>%
  ggplot(aes(x = rent, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Comparison of Calculated vs Reported Median Rents (1980)",
       x = "Median Rent",
       fill = "Rent Type") +
  theme_minimal()

## 1950
tract_housing_data_concorded %>%
  filter(YEAR == 1950) %>%
  select(median_rent_reported_alt, median_rent_calculated) %>%
  filter(median_rent_reported_alt <= 100) %>% 
  #mutate(median_rent = round(median_rent, 3)) %>% 
  pivot_longer(cols = c(median_rent_reported_alt, median_rent_calculated), names_to = "type", values_to = "rent") %>%
  ggplot(aes(x = rent, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Comparison of Calculated vs Reported Median Rents (1950)",
       x = "Median Rent",
       fill = "Rent Type") +
  theme_minimal()

# 1940 
tract_housing_data_concorded %>%
  filter(YEAR == 1940) %>%
  select(median_rent_reported_alt, median_rent_calculated) %>%
  filter(median_rent_reported_alt <= 100, median_rent_calculated <= 100) %>% 
  pivot_longer(cols = c(median_rent_reported_alt, median_rent_calculated), names_to = "type", values_to = "rent") %>%
  ggplot(aes(x = rent, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Comparison of Calculated vs Reported Median Rents (1940)",
       x = "Median Rent",
       fill = "Rent Type") +
  theme_minimal()

## Median home values ----
#1990
tract_housing_data_concorded %>%
  filter(YEAR == 1990) %>%
  select(median_home_value_reported, median_home_value_calculated) %>%
  pivot_longer(cols = c(median_home_value_reported, median_home_value_calculated), names_to = "type", values_to = "home_v") %>%
  ggplot(aes(x = home_v, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Comparison of Calculated vs Reported Median Home Values (1990)",
       x = "Median Home Value",
       fill = "Home Value Type") +
  theme_minimal()

# 1980
tract_housing_data_concorded %>%
  filter(YEAR == 1980) %>%
  select(median_home_value_reported, median_home_value_calculated) %>%
  pivot_longer(cols = c(median_home_value_reported, median_home_value_calculated), names_to = "type", values_to = "home_v") %>%
  ggplot(aes(x = home_v, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Comparison of Calculated vs Reported Median Home Values (1980)",
       x = "Median Home Value",
       fill = "Home Value Type") +
  theme_minimal()

# 1950
tract_housing_data_concorded %>%
  filter(YEAR == 1950) %>%
  select(median_home_value_reported, median_home_value_calculated) %>%
  pivot_longer(cols = c(median_home_value_reported, median_home_value_calculated), names_to = "type", values_to = "home_v") %>%
  ggplot(aes(x = home_v, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Comparison of Calculated vs Reported Median Home Values (1970)",
       x = "Median Home Value",
       fill = "Home Value Type") +
  theme_minimal()

# 1940
tract_housing_data_concorded %>%
  filter(YEAR == 1940) %>%
  select(median_home_value_reported, median_home_value_calculated) %>%
  pivot_longer(cols = c(median_home_value_reported, median_home_value_calculated), names_to = "type", values_to = "home_v") %>%
  ggplot(aes(x = home_v, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Comparison of Calculated vs Reported Median Home Values (1940)",
       x = "Median Home Value",
       fill = "Home Value Type") +
  theme_minimal()

