####
# Read in raw NHGIS tract-level data from

# To start, get the following:
# 1. Tract level population
# 2. Tract level white and black population and population shares
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
tract_crosswalk_2000 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights2000_to_1950.csv"))

# Compile tract-level Census income data ----

# shared variables we want to keep
tract_background_variables <-
  c("NHGISST","NHGISCTY", "GISJOIN", "GISJOIN2", "SHAPE_AREA", "SHAPE_LEN",
    "geometry", "YEAR", "STATE", "STATEA", "COUNTY", "COUNTYA",
    "TRACTA", "POSTTRCTA", "AREANAME")

### 1950 -----

full_tract_data_1950 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/income/1950/nhgis0047_ds82_1950_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1950/US_tract_1950.shp",
      file_select = starts_with("US_tract_1950")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----

tract_income_data_1950 <- 
  full_tract_data_1950 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("B0F001"), contains("B0E")) %>% 
  mutate(median_income_reported = B0F001) %>% 
  select(any_of(tract_background_variables), median_income_reported, contains("B0E"))


var_names <- names(tract_income_data_1950)
var_names[startsWith(var_names, "B0E")] <- sub("^B0E", "income_group", var_names[startsWith(var_names, "B0E")])
names(tract_income_data_1950) <- var_names

#### save 1950 Census tract information ----
# GISJOIN + tract IDs
tract_info_1950_gisjoin <-
  tract_income_data_1950 %>% 
  select(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, TRACTA) 

### 1960 -----

full_tract_data_1960 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/income/1960/nhgis0036_ds92_1960_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1960/US_tract_1960.shp",
      file_select = starts_with("US_tract_1960")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----
# for 1960 and 1970, we have the number of families in each income range
# to get the median income, I will find the median income range, and then just take the low value
# This is inexact, but since I am not comparing across years, it should be internally consistent

tract_income_data_1960<- 
  full_tract_data_1960 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("B8W")) 

var_names <- names(tract_income_data_1960)
var_names[startsWith(var_names, "B8W")] <- sub("^B8W", "income_group", var_names[startsWith(var_names, "B8W")])
names(tract_income_data_1960) <- var_names


### 1970 -----

full_tract_data_1970 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/income/1970/nhgis0036_ds99_1970_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1970/US_tract_1970.shp",
      file_select = starts_with("US_tract_1970")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----
# for 1960 and 1970, we have the number of families in each income range
# to get the median income, I will find the median income range, and then just take the low value
# This is inexact, but since I am not comparing across years, it should be internally consistent


tract_income_data_1970<- 
  full_tract_data_1970 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("C3T")) 

var_names <- names(tract_income_data_1970)
var_names[startsWith(var_names, "C3T")] <- sub("^C3T", "income_group", var_names[startsWith(var_names, "C3T")])
names(tract_income_data_1970) <- var_names


### 1980 -----

full_tract_data_1980 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/income/1980/nhgis0036_ds107_1980_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1980/US_tract_1980.shp",
      file_select = starts_with("US_tract_1980")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----

tract_income_data_1980 <- 
  full_tract_data_1980 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), DIE001) %>% 
  mutate(median_income = DIE001) %>% 
  select(any_of(tract_background_variables), median_income)

### 1990 -----

full_tract_data_1990 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/income/1990/nhgis0036_ds123_1990_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1990/US_tract_1990.shp",
      file_select = starts_with("US_tract_1990")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))


#### harmonization -----

tract_income_data_1990 <- 
  full_tract_data_1990 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), E4U001) %>% 
  mutate(median_income = E4U001) %>% 
  select(any_of(tract_background_variables), median_income)

### 2000 -----

full_tract_data_2000 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/income/2000/nhgis0051_ds151_2000_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_2000/US_tract_2000.shp",
      file_select = starts_with("US_tract_2000")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----

tract_income_data_2000 <- 
  full_tract_data_2000 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), GMY001) %>% 
  mutate(median_income = GMY001) %>% 
  select(any_of(tract_background_variables), median_income)



# Concord datasets to 1950 Census tracts ----
# Have to do this separately for years for which we are given medians (1950, 1980) 
# and for which we have to calculate medians (1960, 1970)
# for the latter, we should reweight populations in each group

# 11/2024 Actually, I can do this all at the same time

# 1. Join on crosswalk to get GISJOIN_1950 and weights
# 2. Weight medians (or group values) by "weight" and collapse to GISJOIN_1950
# 3. Merge geography information from 1950 NHGIS file

years <- c(1960, 1970, 1980, 1990, 2000)

for (year in years) {
  # Construct variable names and file names dynamically based on the year
  tract_income_data_var <- paste0("tract_income_data_", year)
  tract_crosswalk_var <- paste0("tract_crosswalk_", year)
  tract_income_data_concorded_var <- paste0("tract_income_data_", year, "_concorded")
  
  # Execute the operations inside the loop
  assign(tract_income_data_concorded_var, 
         get(tract_income_data_var) %>% 
           # join on crosswalk
           left_join(get(tract_crosswalk_var), by = c("GISJOIN" = paste0("GISJOIN_", year))) %>%
           # weight median incomes
           mutate_at(vars(contains(c("median_income","income_group"))), ~ . * weight) %>%
           st_drop_geometry() %>% 
           # collapse to GISJOIN_1950
           group_by(GISJOIN_1950, YEAR) %>% 
           summarise_at(vars(contains(c("median_income", "income_group"))), sum, na.rm = TRUE)  %>%
           # merge on 1950 tract IDs for each GISJOIN 
           left_join(tract_info_1950_gisjoin, by = c("GISJOIN_1950" = "GISJOIN")) 
  )
}

## Clean 1950 data to same format as concorded data  ----
tract_income_data_1950_concorded <-
  tract_income_data_1950 %>% 
  ungroup() %>% 
  dplyr::rename(GISJOIN_1950 = GISJOIN) %>% 
  select(GISJOIN_1950, YEAR, contains(c("median_income", "income_group")))


# years <- c(1960, 1970)
# 
# for (year in years) {
#   # Construct variable names and file names dynamically based on the year
#   tract_income_data_var <- paste0("tract_income_data_", year)
#   tract_crosswalk_var <- paste0("tract_crosswalk_", year)
#   tract_income_data_concorded_var <- paste0("tract_income_data_", year, "_concorded")
#   
#   # Execute the operations inside the loop
#   assign(tract_income_data_concorded_var, 
#          get(tract_income_data_var) %>% 
#            # join on crosswalk
#            left_join(get(tract_crosswalk_var), by = c("GISJOIN" = paste0("GISJOIN_", year))) %>%
#            # weight populations
#            mutate_at(vars(contains("income_group")), ~ . * weight) %>%
#            st_drop_geometry() %>% 
#            # collapse to GISJOIN_1990
#            group_by(GISJOIN_1990, YEAR) %>% 
#            summarise_at(vars(contains("income_group")), sum, na.rm = TRUE) %>%
#            ungroup()  %>%
#            # merge geography information from 2000 NHGIS file
#            left_join(tract_info_1990, by = c("GISJOIN_1990" = "GISJOIN"))
#   )
# }


# Calculate median incomes for years for which I just have populations ----

## for 1950, 1960 and 1970, I have populations by income groups, so I need to calculate the median 
# after the concordance

## 1950 ----
median_income_1950 <-
  calculate_median_from_census(tract_income_data_1950_concorded, var_code = "income_group", var_name = "median_income_group")

tract_income_data_1950_concorded <-
  tract_income_data_1950_concorded %>% 
  left_join(median_income_1950, by = c("YEAR", "GISJOIN_1950")) %>% 
  # replace the medians with the label of the variable it represents (minimum values)
  mutate(median_income = 
           case_when(median_income_group == "income_group001" ~ 249.5, # 0-499
                     median_income_group == "income_group002" ~ 749.5, # 500-999
                     median_income_group == "income_group003" ~ 1249.5, # 1000-1499
                     median_income_group == "income_group004" ~ 1749.5, # 1500-1999
                     median_income_group == "income_group005" ~ 2249.5, # 2000-2499
                     median_income_group == "income_group006" ~ 2749.5, # 2500-2999
                     median_income_group == "income_group007" ~ 3249.5, # 3000-3499
                     median_income_group == "income_group008" ~ 3749.5, # 3500-3999
                     median_income_group == "income_group009" ~ 4249.5, # 4000-4499
                     median_income_group == "income_group010" ~ 4749.5, # 4500-4999
                     median_income_group == "income_group011" ~ 5499.5, # 5000-5999
                     median_income_group == "income_group012" ~ 6499.5, # 6000-6999
                     median_income_group == "income_group013" ~ 8499.5, # 7000-9999
                     median_income_group == "income_group014" ~ 10000 # 10000+
           ))

## 1960 ----
median_income_1960 <-
  calculate_median_from_census(tract_income_data_1960_concorded, var_code = "income_group", var_name = "median_income_group")

tract_income_data_1960_concorded <-
  tract_income_data_1960_concorded %>% 
  left_join(median_income_1960, by = c("YEAR", "GISJOIN_1950")) %>% 
  # replace the medians with the label of the variable it represents (minimum values)
  mutate(median_income = 
           case_when(median_income_group == "income_group001" ~ 500, # 0-1000
                     median_income_group == "income_group002" ~ 1500, # 1000-2000
                     median_income_group == "income_group003" ~ 2500, # 2000-3000
                     median_income_group == "income_group004" ~ 3500, # 3000-4000
                     median_income_group == "income_group005" ~ 4500, # 4000-5000
                     median_income_group == "income_group006" ~ 5500, # 5000-6000
                     median_income_group == "income_group007" ~ 6500, # 6000-7000
                     median_income_group == "income_group008" ~ 7500, # 7000-8000
                     median_income_group == "income_group009" ~ 8500, # 8000-9000
                     median_income_group == "income_group010" ~ 9500, # 9000-10000
                     median_income_group == "income_group011" ~ 12500, # 10000-15000
                     median_income_group == "income_group012" ~ 20000, # 15000-25000
                     median_income_group == "income_group013" ~ 25000)) # 25000+
  
## 1970 -----
median_income_1970 <-
  calculate_median_from_census(tract_income_data_1970_concorded, var_code = "income_group", var_name = "median_income_group")

tract_income_data_1970_concorded <-
  tract_income_data_1970_concorded %>% 
  left_join(median_income_1970, by = c("YEAR", "GISJOIN_1950")) %>% 
  # replace the medians with the label of the variable it represents (midpoint values, except for last value)
  mutate(median_income = 
           case_when(median_income_group == "income_group001" ~ 500, # 0-1000
                     median_income_group == "income_group002" ~ 1500, # 1000-2000
                     median_income_group == "income_group003" ~ 2500, # 2000-3000 
                     median_income_group == "income_group004" ~ 3500, # 3000-4000
                     median_income_group == "income_group005" ~ 4500, # 4000-5000
                     median_income_group == "income_group006" ~ 5500, # 5000-6000
                     median_income_group == "income_group007" ~ 6500, # 6000-7000
                     median_income_group == "income_group008" ~ 7500, # 7000-8000
                     median_income_group == "income_group009" ~ 8500, # 8000-9000
                     median_income_group == "income_group010" ~ 9500, # 9000-10000
                     median_income_group == "income_group011" ~ 11000, # 10000-12000
                     median_income_group == "income_group012" ~ 13500, # 12000-15000
                     median_income_group == "income_group013" ~ 20000, # 15000-25000
                     median_income_group == "income_group014" ~ 37500, # 25000-50000
                     median_income_group == "income_group015" ~ 50000)) # 50000+


# Combine tract-level datasets ----

# population data with the original tracts
# might use this to compare at some point
tract_income_data_original_tracts <-
  bind_rows(tract_income_data_1950, tract_income_data_1960, tract_income_data_1970, 
            tract_income_data_1980, tract_income_data_1990, tract_income_data_2000) %>%
  select(any_of(tract_background_variables), contains("median_income")) %>% 
  # drop rows with missing geometry
  filter(!st_is_empty(geometry))

# tract population data concorded to 1990 tracts
tract_income_data_concorded <-
  bind_rows(tract_income_data_1950_concorded, tract_income_data_1960_concorded,
            tract_income_data_1970_concorded, tract_income_data_1980_concorded,
            tract_income_data_1990_concorded, tract_income_data_2000_concorded) %>%
  # drop rows with missing geometry
  filter(!st_is_empty(geometry))

# Save ----

# save concorded tract_income_data as shapefile
st_write(tract_income_data_concorded, "data/derived/census/tract_income_data.gpkg",
         append = FALSE, layer = "points", driver = "GPKG")


# visualize
tract_income_data_concorded %>% group_by(YEAR) %>% summarise(mean_med_income = mean(median_income, na.rm = TRUE)) %>% 
  ggplot(aes(x= YEAR, y = mean_med_income)) + geom_line() + geom_point() 






