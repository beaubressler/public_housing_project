####
# Read in raw NHGIS tract-level employment data

####

library(tidyverse)
library(tidycensus)
library(sf)
library(ipumsr)


geographic_crosswalk_dir <- "data/derived/geographic_crosswalks/"



# Write function for caluculating median years of employment in a census tract

calculate_median_ed_from_census <- function(data, rank_df, var_code, var_name) {
  data %>% 
    # convert all the pop variables to numeric
    mutate_at(vars(contains(var_code)), as.numeric) %>%
    pivot_longer(cols = starts_with(var_code), names_to = "range", values_to = "num_in_sample") %>%
    left_join(rank_df, by = c("range" = "name")) %>%
    group_by(YEAR, GISJOIN_1950) %>%
    arrange(YEAR, GISJOIN_1950, rank) %>% 
    mutate(
      cumulative_freq = cumsum(num_in_sample),
      total = sum(num_in_sample),
      median_position = total / 2) %>% 
    filter(cumulative_freq >= median_position) %>%
    slice(1) %>% 
    select(YEAR, GISJOIN_1950, range) %>% 
    dplyr::rename(!!var_name := range) %>% 
    # drop geometry
    st_drop_geometry()
}


# Read in geographic tract crosswalks ----
# crosswalks, 1930-1990 to 1950
tract_crosswalk_1930 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1930_to_1950.csv"))
tract_crosswalk_1940 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1940_to_1950.csv"))
tract_crosswalk_1960 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1960_to_1950.csv"))
tract_crosswalk_1970 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1970_to_1950.csv"))
tract_crosswalk_1980 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1980_to_1950.csv"))
tract_crosswalk_1990 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1990_to_1950.csv"))
tract_crosswalk_2000 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights2000_to_1950.csv"))
tract_crosswalk_2010 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights2010_to_1950.csv"))


# Compile tract-level Census employment data ----

# shared variables we want to keep
tract_background_variables <-
  c("NHGISST","NHGISCTY", "GISJOIN", "GISJOIN2", "SHAPE_AREA", "SHAPE_LEN",
    "geometry", "YEAR", "STATE", "STATEA", "COUNTY", "COUNTYA",
    "TRACTA", "POSTTRCTA", "AREANAME")


### 1930 -----
chicago_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/employment/1930/nhgis0040_ds58_1930_tract.csv"
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
      "data/raw/nhgis/tables/employment/1930/nhgis0040_ds60_1930_tract.csv"
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
      "data/raw/nhgis/tables/employment/1930/nhgis0040_ds68_1930_tract.csv"
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
      "data/raw/nhgis/tables/employment/1930/nhgis0040_ds63_1930_tract.csv"
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


# convert these columns to character in all these tract datasets using lapply
list_of_1930_dfs <-
  list(chicago_1930_tract_raw, cleveland_1930_tract_raw, stl_1930_tract_raw, misc_1930_tract_raw)

list_of_1930_dfs_converted <- lapply(list_of_1930_dfs, function(df) {
  df %>% mutate(across(all_of(columns_to_convert), as.character))
})

# save names of those dfs as a string vector
names_1930_dfs = c("chicago_1930_tract_raw", "cleveland_1930_tract_raw",
                   "stl_1930_tract_raw", "misc_1930_tract_raw")

# reassign to the global environment
names(list_of_1930_dfs_converted) <- names_1930_dfs
list2env(list_of_1930_dfs_converted, envir = .GlobalEnv)

# create full tract dataset
full_tract_data_1930 <-
  bind_rows(chicago_1930_tract_raw, cleveland_1930_tract_raw,
            stl_1930_tract_raw, misc_1930_tract_raw)

####  harmonize variables and create tract-level employment dataset----

tract_employment_data_1930 <- 
  full_tract_data_1930 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("BPN"), contains("BIC"),
         contains("BKO"), contains("BMO")) %>% 
  # create employed pop variable
  mutate(employed_pop = case_when(!is.na(BIC001) ~ BIC001 + BIC002,
                                      !is.na(BPN001) ~ BPN001 + BPN002,
                                      !is.na(BKO001) ~ BKO001 + BKO002,
                                      !is.na(BMO001) ~ BMO001 + BMO002)) %>%
  select(any_of(tract_background_variables), contains("pop"))

### 1940  -------
full_tract_data_1940 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/employment/1940/nhgis0039_ds76_1940_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1940/US_tract_1940.shp",
      file_select = starts_with("US_tract_1940")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))


#### harmonization ----

# calculate employed pop, LFP rate, and unemployed pop and rate

tract_employment_data_1940 <- 
  full_tract_data_1940 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("BUW"), contains("BUZ")) %>% 
  # create employed pop variable: men + women in labor force - unemployed
  # create LFP rate (labor force/not in labor force)
  # create unemployment rate
  mutate(employed_pop = BUW001 + BUW003 - BUZ001 - BUZ002-BUZ003-BUZ004,
         unemployed_pop = BUZ001 + BUZ002 + BUZ003 + BUZ004,
         not_in_lf_pop = BUW002 + BUW004,
         lfp_rate = (employed_pop + unemployed_pop)/(employed_pop + unemployed_pop + not_in_lf_pop),
         unemp_rate = (unemployed_pop)/(employed_pop + unemployed_pop)) %>%
  select(any_of(tract_background_variables), contains("rate"), contains("pop"))

### 1950 -----


full_tract_data_1950 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/employment/1950/nhgis0041_ds82_1950_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1950/US_tract_1950.shp",
      file_select = starts_with("US_tract_1950")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----
tract_employment_data_1950 <- 
  full_tract_data_1950 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("B0P"), contains("B0N")) %>% 
  mutate(employed_pop = B0P001 + B0P003,
         unemployed_pop = B0P002 + B0P004,
         unemp_rate = unemployed_pop/(employed_pop + unemployed_pop),
         not_in_lf_pop = B0N002 + B0N004, 
         lfp_rate = (employed_pop + unemployed_pop)/(employed_pop + unemployed_pop + not_in_lf_pop)) %>%
  select(any_of(tract_background_variables), contains("rate"), contains("pop"))

#### save 1950 Census tract information ----

# GISJOIN + tract IDs
tract_info_1950_gisjoin <-
  tract_employment_data_1950 %>% 
  select(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, TRACTA) 



### 1960 -----
full_tract_data_1960 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/employment/1960/nhgis0039_ds92_1960_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1960/US_tract_1960.shp",
      file_select = starts_with("US_tract_1960")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----
tract_employment_data_1960 <- 
  full_tract_data_1960 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("B82")) %>% 
  mutate(employed_pop = B82001 + B82005,
         unemployed_pop = B82002 + B82006,
         armed_forces_pop = B82003 + B82007,
         unemp_rate = unemployed_pop/(employed_pop + unemployed_pop),
         not_in_lf_pop = B82004 + B82008,
         # dont consider armed forces in labor force
         lfp_rate = (employed_pop + unemployed_pop)/(employed_pop + unemployed_pop + not_in_lf_pop)) %>%
  select(any_of(tract_background_variables), contains("rate"), contains("pop"))

### 1970 -----
full_tract_data_1970 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/employment/1970/nhgis0039_ds98_1970_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1970/US_tract_1970.shp",
      file_select = starts_with("US_tract_1970")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----

tract_employment_data_1970 <- 
  full_tract_data_1970 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("C07")) %>% 
  mutate(employed_pop = C07002,
         unemployed_pop = C07003,
         armed_forces_pop = C07001,
         unemp_rate = unemployed_pop/(employed_pop + unemployed_pop),
         not_in_lf_pop = C07004,
         # consider armed forces as part of the labor force (or not, just exclude)
         lfp_rate = (employed_pop + unemployed_pop)/(employed_pop + unemployed_pop))


### 1980 -----

full_tract_data_1980 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/employment/1980/nhgis0039_ds107_1980_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1980/US_tract_1980.shp",
      file_select = starts_with("US_tract_1980")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----

tract_employment_data_1980 <- 
  full_tract_data_1980 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("DHX")) %>% 
  mutate(employed_pop = DHX002 + DHX006,
         unemployed_pop = DHX003 + DHX007,
         armed_forces_pop = DHX001 + DHX005,
         unemp_rate = unemployed_pop/(employed_pop + unemployed_pop),
         not_in_lf_pop = DHX004 + DHX008,
         # consider armed forces as part of the labor force (or not, just exclude)
         lfp_rate = (employed_pop + unemployed_pop)/(employed_pop + unemployed_pop + not_in_lf_pop)) %>%
  select(any_of(tract_background_variables), contains("rate"), contains("pop"))



### 1990 -----

full_tract_data_1990 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/employment/1990/nhgis0039_ds123_1990_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1990/US_tract_1990.shp",
      file_select = starts_with("US_tract_1990")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))


#### harmonization -----

tract_employment_data_1990 <- 
  full_tract_data_1990 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("E4I")) %>% 
  mutate(employed_pop = E4I002 + E4I006,
         unemployed_pop = E4I003 + E4I007,
         armed_forces_pop = E4I001 + E4I005,
         unemp_rate = unemployed_pop/(employed_pop + unemployed_pop),
         not_in_lf_pop = E4I004 + E4I008, 
         # consider armed forces as part of the labor force (or not, just exclude)
         lfp_rate = (employed_pop + unemployed_pop)/(employed_pop + unemployed_pop + not_in_lf_pop)) %>%
  select(any_of(tract_background_variables), contains("rate"), contains("pop"))

### 2000 -----

full_tract_data_2000 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/employment/2000/nhgis0051_ds151_2000_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_2000/US_tract_2000.shp",
      file_select = starts_with("US_tract_2000")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----

tract_employment_data_2000 <-
  full_tract_data_2000 %>%
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("GLP"), contains("GLR")) %>%
  mutate(employed_pop = GLR001 + GLR003,  # Male employed + Female employed
         unemployed_pop = GLR002 + GLR004,  # Male unemployed + Female unemployed
         not_in_lf_pop = GLP002 + GLP004,  # Male not in LF + Female not in LF
         armed_forces_pop = NA_real_,  # Not available in 2000 data
         unemp_rate = unemployed_pop/(employed_pop + unemployed_pop),
         lfp_rate = (employed_pop + unemployed_pop)/(employed_pop + unemployed_pop + not_in_lf_pop)) %>%
  select(any_of(tract_background_variables), contains("rate"), contains("pop"))

### 2010 -----

full_tract_data_2010 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/employment/2010/nhgis0053_ds177_20105_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2010_us_tract_2010/US_tract_2010.shp",
      file_select = starts_with("US_tract_2010")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR)) %>%
  mutate(YEAR = 2010)

#### harmonization -----

tract_employment_data_2010 <-
  full_tract_data_2010 %>%
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("J6QE")) %>%
  mutate(
    # Sum employed across all ages for males and females
    employed_pop = J6QE007 + J6QE014 + J6QE021 + J6QE028 + J6QE035 + J6QE042 +
                   J6QE049 + J6QE056 + J6QE063 + J6QE070 + J6QE075 + J6QE080 + J6QE085 +
                   J6QE093 + J6QE100 + J6QE107 + J6QE114 + J6QE121 + J6QE128 +
                   J6QE135 + J6QE142 + J6QE149 + J6QE156 + J6QE161 + J6QE166 + J6QE171,
    # Sum unemployed across all ages for males and females
    unemployed_pop = J6QE008 + J6QE015 + J6QE022 + J6QE029 + J6QE036 + J6QE043 +
                     J6QE050 + J6QE057 + J6QE064 + J6QE071 + J6QE076 + J6QE081 + J6QE086 +
                     J6QE094 + J6QE101 + J6QE108 + J6QE115 + J6QE122 + J6QE129 +
                     J6QE136 + J6QE143 + J6QE150 + J6QE157 + J6QE162 + J6QE167 + J6QE172,
    # Sum not in labor force across all ages for males and females
    not_in_lf_pop = J6QE009 + J6QE016 + J6QE023 + J6QE030 + J6QE037 + J6QE044 +
                    J6QE051 + J6QE058 + J6QE065 + J6QE072 + J6QE077 + J6QE082 + J6QE087 +
                    J6QE095 + J6QE102 + J6QE109 + J6QE116 + J6QE123 + J6QE130 +
                    J6QE137 + J6QE144 + J6QE151 + J6QE158 + J6QE163 + J6QE168 + J6QE173,
    armed_forces_pop = NA_real_,
    unemp_rate = unemployed_pop/(employed_pop + unemployed_pop),
    lfp_rate = (employed_pop + unemployed_pop)/(employed_pop + unemployed_pop + not_in_lf_pop)
  ) %>%
  select(any_of(tract_background_variables), contains("rate"), contains("pop"))

# Concord datasets to 1950 Census tracts ----
# 1. Join on crosswalk to get GISJOIN_1950 and weights
# 2. Weight populations by "weight" and collapse to GISJOIN_1950
# 3. Merge geography information from 1950 NHGIS file

## Loop for 1930-2010-----
years <- c(1930, 1940, 1960, 1970, 1980, 1990, 2000, 2010)

for (year in years) {
  # Construct variable names and file names dynamically based on the year
  tract_employment_data_var <- paste0("tract_employment_data_", year)
  tract_crosswalk_var <- paste0("tract_crosswalk_", year)
  tract_employment_data_concorded_var <- paste0("tract_employment_data_", year, "_concorded")
  
  # Execute the operations inside the loop
  assign(tract_employment_data_concorded_var, 
         get(tract_employment_data_var) %>% 
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
tract_employment_data_1950_concorded <-
  tract_employment_data_1950 %>% 
  ungroup() %>% 
  dplyr::rename(GISJOIN_1950 = GISJOIN) %>% 
  select(GISJOIN_1950, YEAR, contains("pop"))


# Combine tract-level datasets ----

# tract population data concorded to 1950 tracts
tract_employment_data_concorded <-
  bind_rows(tract_employment_data_1930_concorded, tract_employment_data_1940_concorded,
            tract_employment_data_1950_concorded, tract_employment_data_1960_concorded,
            tract_employment_data_1970_concorded, tract_employment_data_1980_concorded,
            tract_employment_data_1990_concorded, tract_employment_data_2000_concorded,
            tract_employment_data_2010_concorded) %>%
  # calculate unemployment rate, lfp rate
  mutate(unemp_rate = unemployed_pop/(employed_pop + unemployed_pop),
         lfp_rate = (employed_pop + unemployed_pop)/(employed_pop + unemployed_pop + not_in_lf_pop)) %>%
  select(any_of(tract_background_variables), contains("pop"), contains("rate")) %>%
  # drop rows with missing geometry
  filter(!st_is_empty(geometry))

# Save ----
# save concorded tract_employment_data as shapefile
st_write(tract_employment_data_concorded, "data/derived/census/tract_employment_data.gpkg",
         append = FALSE, layer = "points", driver = "GPKG")






