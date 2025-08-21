####
# Read in raw NHGIS tract-level education data


# Calculate various forms of educational attainment, depending on what's available in each year:
# 1920: Illiterate population over 10
   # Need to merge to population data and divide by total population (unless I can get population over 10)

# 1930: Some places have illiterate pop over 10, some places give pct illiterate pop over 10
# for places for which we don't have pct, calculate it as above 

# 1940-1990: Calculate median years of education (or groups of years) 

# Steps:
# 1. Read in raw data, select variables
# 2. Concord populations in each year to 1990 tracts
# 3. Calculate median educational attainment

####

library(tidyverse)
library(tidycensus)
library(sf)
library(ipumsr)


geographic_crosswalk_dir <- "data/derived/geographic_crosswalks/"



# Write function for caluculating median years of education in a census tract

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
# crosswalks, 1930 to 1990
tract_crosswalk_1930 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1930_to_1950.csv"))
tract_crosswalk_1940 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1940_to_1950.csv"))
tract_crosswalk_1960 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1960_to_1950.csv"))
tract_crosswalk_1970 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1970_to_1950.csv"))
tract_crosswalk_1980 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1980_to_1950.csv"))
tract_crosswalk_1990 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights1990_to_1950.csv"))
tract_crosswalk_2000 <- read_csv(paste0(geographic_crosswalk_dir, "tract_concordance_weights2000_to_1950.csv"))

# Compile tract-level Census education data ----

# shared variables we want to keep
tract_background_variables <-
  c("GISJOIN_1950", "NHGISST","NHGISCTY", "GISJOIN", "GISJOIN2", "SHAPE_AREA", "SHAPE_LEN",
    "geometry", "YEAR", "STATE", "STATEA", "COUNTY", "COUNTYA",
    "TRACTA", "POSTTRCTA", "AREANAME")

### 1920 ---- 
ny_1920_tract_raw <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/education/1920/nhgis0031_ds48_1920_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1920/US_tract_1920.shp",
      file_select = starts_with("US_tract_1920")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR)) 

chicago_1920_tract_raw <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/education/1920/nhgis0031_ds45_1920_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1920/US_tract_1920.shp",
      file_select = starts_with("US_tract_1920")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))
  

full_tract_data_1920 <-
  bind_rows(chicago_1920_tract_raw, ny_1920_tract_raw)

####  harmonize variables and create tract-level population dataset----
# for now, will do illiterate population share

tract_education_data_1920 <- 
  full_tract_data_1920 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("BB3"), contains("BA8")) %>% 
  # Total population: BBP001, BBQ002, A94001, BBJ001
  mutate(illiterate_pop_over_10 = case_when(
    !is.na(BA8001) ~ BA8001 + BA8002 + BA8003,
    !is.na(BB3001) ~ BB3001 + BB3002 + BB3001,
    TRUE ~ NA_real_
  )) %>%
  select(any_of(tract_background_variables), contains("pop"))


### 1930 -----
chicago_1930_tract_raw <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/education/1930/nhgis0031_ds58_1930_tract.csv"
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
      "data/raw/nhgis/tables/education/1930/nhgis0033_ds60_1930_tract.csv"
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
      "data/raw/nhgis/tables/education/1930/nhgis0031_ds68_1930_tract.csv"
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
      "data/raw/nhgis/tables/education/1930/nhgis0033_ds63_1930_tract.csv"
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

####  harmonize variables and create tract-level education dataset----

tract_education_data_1930 <- 
  full_tract_data_1930 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), BHZ001, contains("BB3"), contains("BA8"),
         contains("BO7"), contains("BO8"), contains("BKE"), contains("BKF"), contains("BMJ")) %>% 
  mutate(illiterate_pop_over_10 = case_when(
    !is.na(BO7001) ~ BO7001,
    !is.na(BKE001) ~ BKE001,
    !is.na(BHZ001) ~ BHZ001,
    TRUE ~ NA_real_
  )) %>%
  mutate(pct_illiterate_pop_over_10 = case_when(
    !is.na(BKF001) ~ BKF001,
    !is.na(BO8001) ~ BO8001,
    TRUE ~ NA_real_
  )) %>% 
  select(any_of(tract_background_variables), contains("pop"))


### 1940  -------
full_tract_data_1940 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/education/1940/nhgis0031_ds76_1940_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1940/US_tract_1940.shp",
      file_select = starts_with("US_tract_1940")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))


#### harmonization ----
# for 1940 onward: calculate median educational attainment for those 25 and older

tract_education_data_1940 <- 
  full_tract_data_1940 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("BUH")) %>% 
  mutate(pop_no_school = BUH001 + BUH010,
         pop_elementary_1_to_4 = BUH002 + BUH011,
         pop_elementary_5_to_6 = BUH003 + BUH012,
         pop_elementary_7_to_8 = BUH004 + BUH013,
         pop_hs_1_to_3 = BUH005 + BUH014,
         pop_hs_4 = BUH006 + BUH015,
         pop_college_1_to_3 = BUH007 + BUH016,
         pop_college_4plus = BUH008 + BUH017) %>% 
  select(any_of(tract_background_variables), contains("pop")) %>% 
  # keep if not all the population variables are 0
  filter(pop_no_school > 0 | pop_elementary_1_to_4 > 0 | pop_elementary_5_to_6 > 0 |
           pop_elementary_7_to_8 > 0 | pop_hs_1_to_3 > 0 | pop_hs_4 > 0 | pop_college_1_to_3 > 0 |
           pop_college_4plus > 0) %>% 
  # calculate total pop
  mutate(total_pop = pop_no_school + pop_elementary_1_to_4 + pop_elementary_5_to_6 +
           pop_elementary_7_to_8 + pop_hs_1_to_3 + pop_hs_4 + pop_college_1_to_3 + pop_college_4plus) %>% 
  # calculate share that finished HS and share with at least some college
  mutate(pct_hs_grad = (pop_hs_4 + pop_college_1_to_3 + pop_college_4plus) / total_pop,
         pct_some_college = (pop_college_1_to_3 + pop_college_4plus) / total_pop)
  




### 1950 -----


full_tract_data_1950 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/education/1950/nhgis0031_ds82_1950_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1950/US_tract_1950.shp",
      file_select = starts_with("US_tract_1950")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----
#calculate median educational attainment for those 25 and older

tract_education_data_1950 <- 
  full_tract_data_1950 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("B0B")) %>% 
  mutate(pop_no_school = B0B001,
         pop_elementary_1_to_4 = B0B002,
         pop_elementary_5_to_6 = B0B003,
         pop_elementary_7 = B0B004,
         pop_elementary_8 = B0B005,
         pop_hs_1_to_3 = B0B006,
         pop_hs_4 = B0B007,
         pop_college_1_to_3 = B0B008,
         pop_college_4plus = B0B009
         ) %>% 
  select(any_of(tract_background_variables), contains("pop")) %>% 
  # keep if not all the population variables are 0
  filter(pop_no_school > 0 | pop_elementary_1_to_4 > 0 | pop_elementary_5_to_6 > 0 |
           pop_elementary_7 > 0 | pop_elementary_8 > 0 | pop_hs_1_to_3 > 0 | pop_hs_4 > 0 |
           pop_college_1_to_3 > 0 | pop_college_4plus > 0) %>% 
  # if missing, set to 0. Some tracts in Los Angeles have missings for some of these variables
  mutate_at(vars(contains("pop")), ~replace_na(., 0)) %>%
  # calculate total pop
  mutate(total_pop = pop_no_school + pop_elementary_1_to_4 + pop_elementary_5_to_6 +
           pop_elementary_7 + pop_elementary_8 + pop_hs_1_to_3 + pop_hs_4 + pop_college_1_to_3 +
           pop_college_4plus) %>% 
  # calculate share that finished HS and share with at least some college
  mutate(pct_hs_grad = (pop_hs_4 + pop_college_1_to_3 + pop_college_4plus) / total_pop,
         pct_some_college = (pop_college_1_to_3 + pop_college_4plus) / total_pop)

#### save 1950 Census tract information ----

# GISJOIN + tract IDs
tract_info_1950_gisjoin <-
  tract_education_data_1950 %>% 
  select(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, TRACTA) 


### 1960 -----

full_tract_data_1960 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/education/1960/nhgis0031_ds92_1960_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1960/US_tract_1960.shp",
      file_select = starts_with("US_tract_1960")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----
# calculate median educational attainment for those 25 and older

tract_education_data_1960 <- 
  full_tract_data_1960 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("B8R")) %>% 
  mutate(pop_no_school = B8R001,
         pop_elementary_1_to_4 = B8R002,
         pop_elementary_5_to_7 = B8R003,
         pop_elementary_8 = B8R004,
         pop_hs_1_to_3 = B8R005,
         pop_hs_4 = B8R006,
         pop_college_1_to_3 = B8R007,
         pop_college_4plus = B8R008
         ) %>%
  select(any_of(tract_background_variables), contains("pop")) %>% 
  # drop if all are 0
  filter(pop_no_school > 0 | pop_elementary_1_to_4 > 0 | pop_elementary_5_to_7 > 0 | pop_elementary_8 > 0 | 
           pop_hs_1_to_3 > 0 | pop_hs_4 > 0 | pop_college_1_to_3 > 0 | pop_college_4plus > 0) %>% 
  # calculate total pop
  mutate(total_pop = pop_no_school + pop_elementary_1_to_4 + pop_elementary_5_to_7 +
           pop_elementary_8 + pop_hs_1_to_3 + pop_hs_4 + pop_college_1_to_3 + pop_college_4plus) %>% 
  # calculate share that finished HS and share with at least some college
  mutate(pct_hs_grad = (pop_hs_4 + pop_college_1_to_3 + pop_college_4plus) / total_pop,
         pct_some_college = (pop_college_1_to_3 + pop_college_4plus) / total_pop)



### 1970 -----

full_tract_data_1970 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/education/1970/nhgis0031_ds98_1970_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1970/US_tract_1970.shp",
      file_select = starts_with("US_tract_1970")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----

tract_education_data_1970 <- 
  full_tract_data_1970 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("C06")) %>% 
  mutate(pop_no_school = C06001,
         pop_elementary_1_to_4 = C06002,
         pop_elementary_5_to_6 = C06003,
         pop_elementary_7 = C06004,
         pop_elementary_8 = C06005,
         pop_hs_1_to_3 = C06006,
         pop_hs_4 = C06007,
         pop_college_1_to_3 = C06008,
         pop_college_4 = C06009,
         pop_college_5_plus = C06010
         ) %>%
  select(any_of(tract_background_variables), contains("pop")) %>% 
  # drop if all 0
  filter(pop_no_school > 0 | pop_elementary_1_to_4 > 0 | pop_elementary_5_to_6 > 0 | 
           pop_elementary_7 > 0 | pop_elementary_8 > 0 | pop_hs_1_to_3 > 0 |
           pop_hs_4 > 0 | pop_college_1_to_3 > 0 | pop_college_4 > 0 | pop_college_5_plus > 0) %>% 
  # calculate total pop
  mutate(total_pop = pop_no_school + pop_elementary_1_to_4 + pop_elementary_5_to_6 +
           pop_elementary_7 + pop_elementary_8 + pop_hs_1_to_3 + pop_hs_4 + pop_college_1_to_3 +
           pop_college_4 + pop_college_5_plus) %>% 
  # calculate share that finished HS and share with at least some college
  mutate(pct_hs_grad = (pop_hs_4 + pop_college_1_to_3 + pop_college_4 + pop_college_5_plus) / total_pop,
         pct_some_college = (pop_college_1_to_3 + pop_college_4 + pop_college_5_plus) / total_pop)

  
### 1980 -----

full_tract_data_1980 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/education/1980/nhgis0031_ds107_1980_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1980/US_tract_1980.shp",
      file_select = starts_with("US_tract_1980")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----

tract_education_data_1980 <- 
  full_tract_data_1980 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("DHM")) %>% 
  mutate(across(starts_with("DHM"), as.numeric)) %>% 
  mutate(pop_elementary_0_to_8 = DHM001,
         pop_hs_1_to_3 = DHM002,
         pop_hs_4 = DHM003,
         pop_college_1_to_3 = DHM004,
         pop_college_4plus = DHM005) %>%
  select(any_of(tract_background_variables), contains("pop")) %>% 
  # drop if all 0
  filter(pop_elementary_0_to_8 > 0 | pop_hs_1_to_3 > 0 | pop_hs_4 > 0 |
           pop_college_1_to_3 > 0 | pop_college_4plus > 0) %>% 
  # calculate total pop
  mutate(total_pop = pop_elementary_0_to_8 + pop_hs_1_to_3 + pop_hs_4 + pop_college_1_to_3 + pop_college_4plus) %>% 
  # calculate share that finished HS and share with at least some college
  mutate(pct_hs_grad = (pop_hs_4 + pop_college_1_to_3 + pop_college_4plus) / total_pop,
         pct_some_college = (pop_college_1_to_3 + pop_college_4plus) / total_pop)


### 1990 -----

full_tract_data_1990 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/education/1990/nhgis0031_ds123_1990_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1990/US_tract_1990.shp",
      file_select = starts_with("US_tract_1990")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))


#### harmonization -----

tract_education_data_1990 <- 
  full_tract_data_1990 %>% 
  # keep only variables of interest
  select(any_of(tract_background_variables), contains("E33")) %>% 
  mutate(pop_elementary_0_to_8 = E33001,
         pop_hs_1_to_3 = E33002, 
         pop_hs_4 = E33003,
         pop_college_no_degree = E33004,
         pop_college_associates = E33005,
         pop_college_bachelors = E33006,
         pop_college_post_bachelors = E33007) %>%
  select(any_of(tract_background_variables), contains("pop")) %>%
  # drop if all 0
  filter(pop_elementary_0_to_8 > 0 | pop_hs_1_to_3 > 0 | pop_hs_4 > 0 |
           pop_college_no_degree > 0 |  pop_college_associates > 0 |
           pop_college_bachelors > 0 | pop_college_post_bachelors > 0) %>% 
  # calculate total pop
  mutate(total_pop = pop_elementary_0_to_8 + pop_hs_1_to_3 + pop_hs_4 +
           pop_college_no_degree + pop_college_associates +
           pop_college_bachelors + pop_college_post_bachelors) %>% 
  # calculate share that finished HS and share with at least some college
  mutate(pct_hs_grad = (pop_hs_4 + pop_college_no_degree + pop_college_associates +
                        pop_college_bachelors + pop_college_post_bachelors) / total_pop,
         pct_some_college = (pop_college_no_degree + pop_college_associates +
                             pop_college_bachelors + pop_college_post_bachelors) / total_pop)

### 2000 ----

full_tract_data_2000 <-
  ipums_shape_full_join(
    read_nhgis(
      "data/raw/nhgis/tables/education/2000/nhgis0031_ds151_2000_tract.csv"
    ),
    read_ipums_sf(
      "data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_2000/US_tract_2000.shp",
      file_select = starts_with("US_tract_2000")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR))

#### harmonization -----

tract_education_data_2000 <- 
  full_tract_data_2000 %>% 
  # keep only variables of interest - sum male and female
  select(any_of(tract_background_variables), contains("GKT")) %>% 
  mutate(
    # Elementary 0-8: No schooling + nursery-4th + 5th-6th + 7th-8th
    pop_elementary_0_to_8 = (GKT001 + GKT002 + GKT003 + GKT004) + 
                           (GKT017 + GKT018 + GKT019 + GKT020),
    # High school 1-3: 9th + 10th + 11th + 12th no diploma
    pop_hs_1_to_3 = (GKT005 + GKT006 + GKT007 + GKT008) + 
                   (GKT021 + GKT022 + GKT023 + GKT024),
    # High school graduate
    pop_hs_4 = GKT009 + GKT025,
    # Some college, no degree
    pop_college_no_degree = (GKT010 + GKT011) + (GKT026 + GKT027),
    # Associate degree
    pop_college_associates = GKT012 + GKT028,
    # Bachelor's degree
    pop_college_bachelors = GKT013 + GKT029,
    # Post-bachelor's (Master's, Professional, Doctorate)
    pop_college_post_bachelors = (GKT014 + GKT015 + GKT016) + 
                                (GKT030 + GKT031 + GKT032)
  ) %>%
  select(any_of(tract_background_variables), contains("pop")) %>%
  # drop if all 0
  filter(pop_elementary_0_to_8 > 0 | pop_hs_1_to_3 > 0 | pop_hs_4 > 0 |
           pop_college_no_degree > 0 |  pop_college_associates > 0 |
           pop_college_bachelors > 0 | pop_college_post_bachelors > 0) %>% 
  # calculate total pop
  mutate(total_pop = pop_elementary_0_to_8 + pop_hs_1_to_3 + pop_hs_4 +
           pop_college_no_degree + pop_college_associates +
           pop_college_bachelors + pop_college_post_bachelors) %>% 
  # calculate share that finished HS and share with at least some college
  mutate(pct_hs_grad = (pop_hs_4 + pop_college_no_degree + pop_college_associates +
                        pop_college_bachelors + pop_college_post_bachelors) / total_pop,
         pct_some_college = (pop_college_no_degree + pop_college_associates +
                             pop_college_bachelors + pop_college_post_bachelors) / total_pop)



# Concord datasets to 1950 Census tracts ----
# 1. Join on crosswalk to get GISJOIN_1990 and weights
# 2. Weight populations by "weight" and collapse to GISJOIN_1990
# 3. Merge geography information from 1990 NHGIS file

## Loop for 1930-2000 -----
years <- c(1930, 1940, 1960, 1970, 1980, 1990, 2000)

for (year in years) {
  # Construct variable names and file names dynamically based on the year
  tract_education_data_var <- paste0("tract_education_data_", year)
  tract_crosswalk_var <- paste0("tract_crosswalk_", year)
  tract_education_data_concorded_var <- paste0("tract_education_data_", year, "_concorded")
  
  # Execute the operations inside the loop
  assign(tract_education_data_concorded_var, 
         get(tract_education_data_var) %>% 
           # join on crosswalk
           left_join(get(tract_crosswalk_var), by = c("GISJOIN" = paste0("GISJOIN_", year))) %>%
           # weight populations
           mutate_at(vars(contains("pop")), ~ . * weight) %>%
           st_drop_geometry() %>% 
           # collapse to GISJOIN_1950
           group_by(GISJOIN_1950, YEAR) %>% 
           summarise_at(vars(contains("pop")), sum, na.rm = TRUE) %>%
           ungroup() %>% 
           # merge on 1950 tract IDs for each GISJOIN 
           left_join(tract_info_1950_gisjoin, by = c("GISJOIN_1950" = "GISJOIN"))
  )
}

## Clean 1950 data to same format as concorded data  ----
tract_education_data_1950_concorded <-
  tract_education_data_1950 %>% 
  dplyr::rename(GISJOIN_1950 = GISJOIN) %>% 
  select(GISJOIN_1950, YEAR, contains("pop"))



# Calculate median education in each year ----

## 1940 ----
rank_1940 <-
  tibble(
    name = c("pop_no_school", "pop_elementary_1_to_4", "pop_elementary_5_to_6", "pop_elementary_7_to_8",
             "pop_hs_1_to_3", "pop_hs_4", "pop_college_1_to_3", "pop_college_4plus"),
    rank = c(1, 2, 3, 4, 5, 6, 7, 8)
  )

# calculate median years of education and merge on 
tract_education_data_1940_concorded <- 
  tract_education_data_1940_concorded %>% 
  # calculate median years of education
  left_join(calculate_median_ed_from_census(tract_education_data_1940_concorded, rank_1940,
                                            var_code = "pop_", var_name = "median_educ_attainment_25plus"))


## 1950 ----
rank_1950 <-
  tibble(
    name = c("pop_no_school", "pop_elementary_1_to_4", "pop_elementary_5_to_6", "pop_elementary_7",
             "pop_elementary_8", "pop_hs_1_to_3", "pop_hs_4", "pop_college_1_to_3", "pop_college_4plus"),
    rank = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  )

tract_education_data_1950_concorded <- 
  tract_education_data_1950_concorded %>% 
  left_join(
    calculate_median_ed_from_census(tract_education_data_1950_concorded, rank_1950,
                                    var_code = "pop_", var_name = "median_educ_attainment_25plus")
  )

## 1960 ----
rank_1960 <-
  tibble(
    name = c("pop_no_school", "pop_elementary_1_to_4", "pop_elementary_5_to_7", "pop_elementary_8",
             "pop_hs_1_to_3", "pop_hs_4", "pop_college_1_to_3", "pop_college_4plus"),
    rank = c(1, 2, 3, 4, 5, 6, 7, 8)
  )

tract_education_data_1960_concorded <- 
  tract_education_data_1960_concorded %>% 
  left_join(
    calculate_median_ed_from_census(tract_education_data_1960_concorded, rank_1960,
                                    var_code = "pop_", var_name = "median_educ_attainment_25plus")
  )


## 1970 ---- 

rank_1970 <-
  tibble(
    name = c("pop_no_school", "pop_elementary_1_to_4", "pop_elementary_5_to_6", "pop_elementary_7",
             "pop_elementary_8", "pop_hs_1_to_3", "pop_hs_4", "pop_college_1_to_3", "pop_college_4", "pop_college_5_plus"),
    rank = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )

tract_education_data_1970_concorded <-
  tract_education_data_1970_concorded %>% 
  left_join(
    calculate_median_ed_from_census(tract_education_data_1970_concorded, rank_1970,
                                    var_code = "pop_", var_name = "median_educ_attainment_25plus")
  )


## 1980 ----

rank_1980 <-
  tibble(
    name = c("pop_elementary_0_to_8", "pop_hs_1_to_3", "pop_hs_4", "pop_college_0_to_3", "pop_college_4plus"),
    rank = c(1, 2, 3, 4, 5)
  )

tract_education_data_1980_concorded <- 
  tract_education_data_1980_concorded %>% 
  left_join(
    calculate_median_ed_from_census(tract_education_data_1980_concorded, rank_1980,
                                    var_code = "pop_", var_name = "median_educ_attainment_25plus")
  )

## 1990 ----

rank_1990 <-
  tibble(
    name = c("pop_elementary_0_to_8", "pop_hs_1_to_3", "pop_hs_4", "pop_college_no_degree", "pop_college_associates",
             "pop_college_bachelors", "pop_college_post_bachelors"),
    rank = c(1, 2, 3, 4, 5, 6, 7)
  )


tract_education_data_1990_concorded <-
  tract_education_data_1990_concorded %>% 
  left_join(
    calculate_median_ed_from_census(tract_education_data_1990_concorded, rank_1990,
                                    var_code = "pop_", var_name = "median_educ_attainment_25plus")
  )


# Combine tract-level datasets ----

# population data with the original tracts
# might use this to compare at some point
# tract_education_data_original_tracts <-
#   bind_rows(tract_education_data_1930, tract_education_data_1940,
#             tract_education_data_1950, tract_education_data_1960, tract_education_data_1970, 
#             tract_education_data_1980, tract_education_data_1990) %>%
#   select(any_of(tract_background_variables), starts_with("pop"), median_educ_attainment_25plus, ) %>% 
#   # drop rows with missing geometry
#   filter(!st_is_empty(geometry))

# tract population data concorded to 1990 tracts
tract_education_data_concorded <-
  bind_rows(tract_education_data_1930_concorded, tract_education_data_1940_concorded,
            tract_education_data_1950_concorded, tract_education_data_1960_concorded,
            tract_education_data_1970_concorded, tract_education_data_1980_concorded,
            tract_education_data_1990_concorded, tract_education_data_2000_concorded) %>% 
  # calculate share that finished HS and share with at least some college  (based on earlier calculations)
  mutate(
    pct_hs_grad = case_when(
      YEAR == 1940 ~ (pop_hs_4 + pop_college_1_to_3 + pop_college_4plus) / total_pop,
      YEAR == 1950 ~ (pop_hs_4 + pop_college_1_to_3 + pop_college_4plus) / total_pop,
      YEAR == 1960 ~ (pop_hs_4 + pop_college_1_to_3 + pop_college_4plus) / total_pop,
      YEAR == 1970 ~ (pop_hs_4 + pop_college_1_to_3 + pop_college_4 + pop_college_5_plus) / total_pop,
      YEAR == 1980 ~ (pop_hs_4 + pop_college_1_to_3 + pop_college_4plus) / total_pop,
      YEAR == 1990 ~ (pop_hs_4 + pop_college_no_degree + pop_college_associates +
                        pop_college_bachelors + pop_college_post_bachelors) / total_pop,
      YEAR == 2000 ~ (pop_hs_4 + pop_college_no_degree + pop_college_associates +
                        pop_college_bachelors + pop_college_post_bachelors) / total_pop,
      TRUE ~ NA_real_),
    pct_some_college = case_when(
      YEAR == 1940 ~ (pop_college_1_to_3 + pop_college_4plus) / total_pop,
      YEAR == 1950 ~ (pop_college_1_to_3 + pop_college_4plus) / total_pop,
      YEAR == 1960 ~ (pop_college_1_to_3 + pop_college_4plus) / total_pop,
      YEAR == 1970 ~ (pop_college_1_to_3 + pop_college_4 + pop_college_5_plus) / total_pop,
      YEAR == 1980 ~ (pop_college_1_to_3 + pop_college_4plus) / total_pop,
      YEAR == 1990 ~ (pop_college_no_degree + pop_college_associates +
                        pop_college_bachelors + pop_college_post_bachelors) / total_pop,
      YEAR == 2000 ~ (pop_college_no_degree + pop_college_associates +
                        pop_college_bachelors + pop_college_post_bachelors) / total_pop,
      TRUE ~ NA_real_)) %>%
  select(GISJOIN_1950, YEAR, geometry, median_educ_attainment_25plus, pct_hs_grad, pct_some_college) %>% 
  # drop rows with missing geometry
  filter(!st_is_empty(geometry))

# Assign years of education to each of the groups 
# April 2024: I will just assign the "midpoint" of the group for now
# Starting from 1 year of elementary

tract_education_data_concorded <-
  tract_education_data_concorded %>% 
  # median educational attainment
  mutate(median_educ_years_25plus = 
           case_when(
             median_educ_attainment_25plus == "pop_no_school" ~ 0,
             median_educ_attainment_25plus == "pop_elementary_0_to_8" ~ 4,
             median_educ_attainment_25plus == "pop_elementary_1_to_4" ~ 2.5,
             median_educ_attainment_25plus == "pop_elementary_5_to_6" ~ 5.5,
             median_educ_attainment_25plus == "pop_elementary_5_to_7" ~ 6,
             median_educ_attainment_25plus == "pop_elementary_7" ~ 7,
             median_educ_attainment_25plus == "pop_elementary_7_to_8" ~ 7.5,
             median_educ_attainment_25plus == "pop_elementary_8" ~ 8,
             median_educ_attainment_25plus == "pop_hs_1_to_3" ~ 10,
             median_educ_attainment_25plus == "pop_hs_4" ~ 12,
             median_educ_attainment_25plus == "pop_college_1_to_3" ~ 14,
             median_educ_attainment_25plus == "pop_college_4plus" ~ 16,
             median_educ_attainment_25plus == "pop_college_associates" ~ 14,
             median_educ_attainment_25plus == "pop_college_bachelors" ~ 16,
             median_educ_attainment_25plus == "pop_college_no_degree" ~ 14,
             median_educ_attainment_25plus == "pop_college_post_bachelors" ~ 18,
             TRUE ~ NA_real_))

 
# Save ----

# save concorded tract_education_data as shapefile
st_write(tract_education_data_concorded, "data/derived/census/tract_education_data.gpkg",
         append = FALSE, layer = "points", driver = "GPKG")






