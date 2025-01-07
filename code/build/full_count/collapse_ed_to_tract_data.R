####
# Collapse ED-level to tract level data
#####

library(tidyverse)
library(sf)
library(here)
library(ipumsr)

city_ed_dir <- here("data", "derived", "census", "full_count", "ed_by_city")
ed_tract_crosswalk_dir <- here("data", "derived", "geographic_crosswalks", "ed_to_tract")

tract_background_variables <-
  c("NHGISST","NHGISCTY", "GISJOIN", "GISJOIN2", "SHAPE_AREA", "SHAPE_LEN",
    "geometry", "YEAR", "STATE", "STATEA", "COUNTY", "COUNTYA", "PRETRACTA",
    "TRACTA", "POSTTRCTA", "AREANAME")

tract_output_dir <- here("data", "derived", "census", "full_count", "tract")
  
# Read and clean data before merge -----

# City-ED data
city_ed_data_1930_raw <- read_csv(here(city_ed_dir, "city_ed_data_1930.csv"))
city_ed_data_1940_raw <- read_csv(here(city_ed_dir, "city_ed_data_1940.csv"))

# ED-Tract crosswalks
ed_tract_crosswalk_1930 <- 
  read_csv(here(ed_tract_crosswalk_dir, "ed_1930_to_1990_tracts.csv")) %>% 
  dplyr::rename(b_ed = ed_1930) %>% 
  mutate(b_ed = tolower(b_ed))

ed_tract_crosswalk_1940 <-
  read_csv(here(ed_tract_crosswalk_dir, "ed_1940_to_1990_tracts.csv")) %>% 
  dplyr::rename(b_ed = ed_1940) %>% 
  mutate(b_ed = tolower(b_ed))

# 1990 Tract information
tract_background_variables <-
  c("NHGISST","NHGISCTY", "GISJOIN", "GISJOIN2", "SHAPE_AREA", "SHAPE_LEN",
    "geometry", "YEAR", "STATE", "STATEA", "COUNTY", "COUNTYA", "PRETRACTA",
    "TRACTA", "POSTTRCTA", "AREANAME")

tract_info_1990 <-
  ipums_shape_full_join(
    read_nhgis("data/raw/nhgis/tables/1990/nhgis0027_ds120_1990_tract.csv"),
    read_ipums_sf("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1990/US_tract_1990.shp",
                  file_select = starts_with("US_tract_1990")), by = "GISJOIN"
  ) %>% 
  filter(!is.na(YEAR)) %>% 
  select(any_of(tract_background_variables), -YEAR)

# Central business district indicator
cbd_tracts_1990 <- read_csv(here(output_dir, "cbd_tracts_1990_concorded.csv"))

# Clean data ----
# split CITY_NAME into city and state, splitting on commas
city_ed_data_1930 <- city_ed_data_1930_raw %>%
  separate(CITY_NAME, c("city", "state"), sep = ",") %>%
  mutate(city = str_trim(city), state = str_trim(state)) %>% 
  # convert b_ed to lower case character
  mutate(b_ed = tolower(b_ed)) %>% 
  mutate(YEAR = 1930)

city_ed_data_1940 <- city_ed_data_1940_raw %>% 
  separate(CITY_NAME, c("city", "state"), sep = ",") %>%
  mutate(city = str_trim(city), state = str_trim(state)) %>% 
  # convert b_ed to lower case character
  mutate(b_ed = tolower(b_ed)) %>%
  mutate(YEAR = 1940)

# Collapse ED-level to tract level data ----

years <- c(1930, 1940)

for (year in years) {
  
  print(year)
  # Construct variable names and file names dynamically based on the year
  ed_data_var <- paste0("city_ed_data_", year)
  crosswalk_var <- paste0("ed_tract_crosswalk_", year)
  data_concorded_var <- paste0("tract_data_", year, "_concorded")
  
  # Execute the operations inside the loop
  assign(data_concorded_var, 
         get(ed_data_var) %>% 
           # join on crosswalk
           left_join(get(crosswalk_var),
                     by = c("state", "city", "b_ed")) %>%
           # weight populations
           mutate_at(vars(contains("pop"), contains("rent_group"), contains("home_value_"), 
                          contains("home_value"), contains("valueh_group"), contains("employed"),
                          contains("not_in_lf")), ~ . * weight) %>%
           st_drop_geometry()  %>% 
           # collapse to GISJOIN_1990
           group_by(GISJOIN_1990, YEAR) %>% 
           summarise_at(vars(contains("pop"), contains("rent_group"), contains("home_value_"), 
                             contains("home_value"), contains("valueh_group"), contains("employed"),
                             contains("not_in_lf")), sum, na.rm = TRUE) %>%
           ungroup()  %>%
           # merge geography information from 1990 NHGIS file
           left_join(tract_info_1990, by = c("GISJOIN_1990" = "GISJOIN"))
  )
}

# Calculate medians for home values and rents ----

calculate_median_from_census <- function(data, var_code, var_name) {
  data %>% 
    pivot_longer(cols = starts_with(var_code), names_to = "range", values_to = "num_in_sample") %>% 
    mutate(
      range = gsub(var_code, "", range),
      range = as.numeric(range)) %>%
    group_by(YEAR, STATEA, COUNTYA, TRACTA) %>%
    mutate(
      cumulative_freq = cumsum(num_in_sample),
      total = sum(num_in_sample),
      median_position = total / 2) %>% 
    filter(cumulative_freq >= median_position) %>%
    slice(1) %>%
    mutate(median = paste0(var_code, range)) %>% 
    select(YEAR, STATEA, COUNTYA, TRACTA, median) %>% 
    # drop geometry
    st_drop_geometry() %>% 
    dplyr::rename(!!var_name := median)
}

## 1930 ----
median_rent_1930 <- 
  calculate_median_from_census(tract_data_1930_concorded, var_code = "rent_group_", var_name = "median_rent_group")

median_home_value_1930 <-
  calculate_median_from_census(tract_data_1930_concorded, var_code = "home_value_", var_name = "median_home_value_group")

tract_data_1930_concorded <-
  tract_data_1930_concorded %>% 
  # keep only variables of interest
  select(-contains("rent_group_"), -contains("home_value_")) %>%
  left_join(median_rent_1930, by = c("YEAR", "STATEA", "COUNTYA", "TRACTA")) %>% 
  left_join(median_home_value_1930, by = c("YEAR", "STATEA", "COUNTYA", "TRACTA")) %>%
  # replace the medians with the label of the variable it represents (midpoint values except last value)
  mutate(median_home_value_calculated = case_when(
          median_home_value_group == "home_value_1" ~ 250,      # 0 - 500
          median_home_value_group == "home_value_2" ~ 600,      # 501 - 699
          median_home_value_group == "home_value_3" ~ 850,      # 700 - 999
          median_home_value_group == "home_value_4" ~ 1250,     # 1000 - 1499
          median_home_value_group == "home_value_5" ~ 1750,     # 1500 - 1999
          median_home_value_group == "home_value_6" ~ 2250,     # 2000 - 2499
          median_home_value_group == "home_value_7" ~ 2750,     # 2500 - 2999
          median_home_value_group == "home_value_8" ~ 3500,     # 3000 - 3999
          median_home_value_group == "home_value_9" ~ 4500,     # 4000 - 4999
          median_home_value_group == "home_value_10" ~ 5500,    # 5000 - 5999
          median_home_value_group == "home_value_11" ~ 6750,    # 6000 - 7499
          median_home_value_group == "home_value_12" ~ 8750,    # 7500 - 9999
          median_home_value_group == "home_value_13" ~ 12500,   # 10000 - 14999
          median_home_value_group == "home_value_14" ~ 17500,   # 15000 - 19999
          median_home_value_group == "home_value_15" ~ 20000    # 20000+
        ),
        median_rent_calculated = case_when(
          median_rent_group == "rent_group_1" ~ 2.5,    # 0-5
          median_rent_group == "rent_group_2" ~ 5.5,    # 5-6
          median_rent_group == "rent_group_3" ~ 7.5,    # 6-9
          median_rent_group == "rent_group_4" ~ 11.5,   # 9-14
          median_rent_group == "rent_group_5" ~ 16.5,   # 14-19
          median_rent_group == "rent_group_6" ~ 21.5,   # 19-24
          median_rent_group == "rent_group_7" ~ 26.5,   # 24-29
          median_rent_group == "rent_group_8" ~ 34,     # 29-39
          median_rent_group == "rent_group_9" ~ 44,     # 39-49
          median_rent_group == "rent_group_10" ~ 54,     # 49-59
          median_rent_group == "rent_group_11" ~ 66.5,   # 59-74
          median_rent_group == "rent_group_12" ~ 87.5,   # 74-99
          median_rent_group == "rent_group_13" ~ 124,    # 99-149
          median_rent_group == "rent_group_14" ~ 174,    # 149-199
          median_rent_group == "rent_group_15" ~ 200     # 200+
        ))
        
        
## 1940 ----
median_rent_1940 <-
  calculate_median_from_census(tract_data_1940_concorded, var_code = "rent_group_", var_name = "median_rent_group")

median_home_value_1940 <-
  calculate_median_from_census(tract_data_1940_concorded, var_code = "valueh_group_", var_name = "median_home_value_group")

tract_data_1940_concorded <-
  tract_data_1940_concorded %>% 
  select(-contains("rent_group_"), -contains("valueh_group_")) %>%
  left_join(median_rent_1940, by = c("YEAR", "STATEA", "COUNTYA", "TRACTA")) %>% 
  left_join(median_home_value_1940, by = c("YEAR", "STATEA", "COUNTYA", "TRACTA")) %>%
  # replace the medians with the label of the variable it represents (midpoint values except last value)
  mutate(median_home_value_calculated = case_when(
    median_home_value_group == "valueh_group_1" ~ 250,      # 0 - 500
    median_home_value_group == "valueh_group_2" ~ 600,      # 501 - 699
    median_home_value_group == "valueh_group_3" ~ 850,      # 700 - 999
    median_home_value_group == "valueh_group_4" ~ 1250,     # 1000 - 1499
    median_home_value_group == "valueh_group_5" ~ 1750,     # 1500 - 1999
    median_home_value_group == "valueh_group_6" ~ 2250,     # 2000 - 2499
    median_home_value_group == "valueh_group_7" ~ 2750,     # 2500 - 2999
    median_home_value_group == "valueh_group_8" ~ 3500,     # 3000 - 3999
    median_home_value_group == "valueh_group_9" ~ 4500,     # 4000 - 4999
    median_home_value_group == "valueh_group_10" ~ 5500,    # 5000 - 5999
    median_home_value_group == "valueh_group_11" ~ 6750,    # 6000 - 7499
    median_home_value_group == "valueh_group_12" ~ 8750,    # 7500 - 9999
    median_home_value_group == "valueh_group_13" ~ 12500,   # 10000 - 14999
    median_home_value_group == "valueh_group_14" ~ 17500,   # 15000 - 19999
    median_home_value_group == "valueh_group_15" ~ 20000    # 20000+
  ),
  median_rent_calculated = case_when(
    median_rent_group == "rent_group_1" ~ 2.5,    # 0-5
    median_rent_group == "rent_group_2" ~ 5.5,    # 5-6
    median_rent_group == "rent_group_3" ~ 7.5,    # 6-9
    median_rent_group == "rent_group_4" ~ 11.5,   # 9-14
    median_rent_group == "rent_group_5" ~ 16.5,   # 14-19
    median_rent_group == "rent_group_6" ~ 21.5,   # 19-24
    median_rent_group == "rent_group_7" ~ 26.5,   # 24-29
    median_rent_group == "rent_group_8" ~ 34,     # 29-39
    median_rent_group == "rent_group_9" ~ 44,     # 39-49
    median_rent_group == "rent_group_10" ~ 54,     # 49-59
    median_rent_group == "rent_group_11" ~ 66.5,   # 59-74
    median_rent_group == "rent_group_12" ~ 87.5,   # 74-99
    median_rent_group == "rent_group_13" ~ 124,    # 99-149
    median_rent_group == "rent_group_14" ~ 174,    # 149-199
    median_rent_group == "rent_group_15" ~ 200     # 200+
  ))


# Combine years of data -----
tract_data_concorded_1930_1940 <- 
  bind_rows(tract_data_1930_concorded, tract_data_1940_concorded) %>% 
  # merge on CBD indicator
  left_join(cbd_tracts_1990)  %>% 
  mutate(cbd = ifelse(is.na(cbd), 0, cbd)) %>% 
  # calculate area in square meters
  mutate(area_m2 = st_area(geometry)) %>% 
  # calculate total pop
  mutate(total_pop = white_pop + black_pop + other_pop) %>% 
  # calculate white, black and other share, as well as foreign white share
  mutate(white_share = white_pop / total_pop,
         black_share = black_pop / total_pop,
         other_share = other_pop / total_pop,
         population_density = total_pop/area_m2) %>%
    # calculate unemployment and LFP rates
  mutate(unemp_rate = unemployed/(employed + unemployed),
         lfp_rate = (employed + unemployed)/(employed + unemployed + not_in_lf)) 

# Save data -----
st_write(tract_data_concorded_1930_1940, here(tract_output_dir, "tract_data_concorded_from_ed_1930_1940.gpkg"),
         append = FALSE, layer = "points", driver = "GPKG")


