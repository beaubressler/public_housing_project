# Harmonized Occupational Categories for SES Measurement
# This code classifies occupations into high-skill, mid-skill, and low-skill categories 
# across different Census years (1940, 1950, 1960, 1970, 1980, 1990) to create a consistent 
# measure of socioeconomic status (SES) for neighborhoods over time.
# 
# - High-Skill Occupations: These represent managerial, professional, and technical occupations 
#   that typically require higher education and specialized training, indicating higher SES.
# 
# - Mid-Skill Occupations: These include clerical, sales, and craft-related occupations that 
#   often require moderate education or training, reflecting middle SES.
# 
# - Low-Skill Occupations: These encompass service work, labor, operatives, and farm labor, 
#   which generally involve manual labor or lower educational requirements, indicating lower SES.
# 
# The classification uses Census occupation codes for each decade, and the same occupation categories 
# are used consistently across decades to analyze changes in SES over time.
# 
# High-Skill Codes (1940-1990): BU0001+BU0002+BU0011+BU0012 (1940), B0R001+B0R002+B0R011+B0R012 (1950), 
# B9B001+B9B003+B9B013+B9B015 (1960), C08001+C08002 (1970), DIB001+DIB002+DIB003 (1980), E4Q001+E4Q002 (1990)
# 
# Mid-Skill Codes (1940-1990): BU0004+BU0005+BU0014+BU0015 (1940), B0R003+B0R004+B0R005+B0R013+B0R014+B0R015 (1950), 
# B9B004+B9B005+B9B006+B9B016+B9B017+B9B018 (1960), C08003+C08004+C08005 (1970), DIB004+DIB005+DIB010 (1980), 
# E4Q003+E4Q004+E4Q005+E4Q010 (1990)
# 
# Low-Skill Codes (1940-1990): BU0006+BU0007+BU0008+BU0009+BU0016+BU0017+BU0018+BU0019 (1940), 
# B0R006+B0R007+B0R008+B0R009+B0R016+B0R017+B0R018+B0R019 (1950), B9B007+B9B008+B9B009+B9B010+B9B011+B9B019+B9B020+B9B021+B9B022+B9B023 (1960), 
# C08006+C08007+C08008+C08009+C08010+C08011+C08012 (1970), DIB006+DIB007+DIB008+DIB009+DIB011+DIB012+DIB013 (1980), 
# E4Q006+E4Q007+E4Q008+E4Q009+E4Q011+E4Q012+E4Q013 (1990)


library(tidyverse)
library(sf)
library(ipumsr)
library(here)


geographic_crosswalk_dir <- here("data", "derived", "geographic_crosswalks")
occupation_data_dir <- here("data", "raw", "nhgis", "tables", "occupation")
gis_data_dir <- here("data", "raw", "nhgis", "gis")
output_dir <- here("data", "derived", "census")

# Read in geographic tract crosswalks ----
# crosswalks, 1930 to 1950
tract_crosswalk_1930 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1930_to_1950.csv"))
tract_crosswalk_1940 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1940_to_1950.csv"))
tract_crosswalk_1960 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1960_to_1950.csv"))
tract_crosswalk_1970 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1970_to_1950.csv"))
tract_crosswalk_1980 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1980_to_1950.csv"))
tract_crosswalk_1990 <- read_csv(here(geographic_crosswalk_dir, "tract_concordance_weights1990_to_1950.csv"))


# shared variables we want to keep
tract_background_variables <-
  c("NHGISST","NHGISCTY", "GISJOIN", "GISJOIN2", "SHAPE_AREA", "SHAPE_LEN",
    "geometry", "YEAR", "STATE", "STATEA", "COUNTY", "COUNTYA",
    "TRACTA", "POSTTRCTA", "AREANAME")


# Compile tract-level Census occupation data ----
## 1940  -------
full_tract_data_1940 <-
  ipums_shape_full_join(
    read_nhgis(
      here(occupation_data_dir, "nhgis0042_ds76_1940_tract.csv")),
    read_ipums_sf(
      here(gis_data_dir, "nhgis0027_shapefile_tl2000_us_tract_1940/US_tract_1940.shp"),
      file_select = starts_with("US_tract_1940")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR)) %>% 
  # classify occupations into skill groups
  mutate(
    high_skill_occupations = BU0001+BU0002+BU0011+BU0012,
    mid_skill_occupations = BU0004+BU0005+BU0014+BU0015,
    low_skill_occupations = BU0006+BU0007+BU0008+BU0009+BU0016+BU0017+BU0018+BU0019
  ) %>% 
  # keep relevant columns
  select(any_of(tract_background_variables), contains("occupations"))



## 1950  -------
full_tract_data_1950 <-
  ipums_shape_full_join(
    read_nhgis(
      here(occupation_data_dir, "nhgis0042_ds82_1950_tract.csv")),
    read_ipums_sf(
      here(gis_data_dir, "nhgis0027_shapefile_tl2000_us_tract_1950/US_tract_1950.shp"),
      file_select = starts_with("US_tract_1950")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR)) %>% 
  # classify occupations into skill groups
  mutate(
    high_skill_occupations = B0R001+B0R002+B0R011+B0R012,
    mid_skill_occupations = B0R003+B0R004+B0R005+B0R013+B0R014+B0R015,
    low_skill_occupations = B0R006+B0R007+B0R008+B0R009+B0R016+B0R017+B0R018+B0R019
  ) %>% 
  # keep relevant columns
  select(any_of(tract_background_variables), contains("occupations"))


### save 1950 Census tract information ----

# GISJOIN + tract IDs
tract_info_1950_gisjoin <-
  full_tract_data_1950 %>% 
  select(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, TRACTA) 


## 1960  -------
full_tract_data_1960 <-
  ipums_shape_full_join(
    read_nhgis(
      here(occupation_data_dir, "nhgis0042_ds92_1960_tract.csv")),
    read_ipums_sf(
      here(gis_data_dir, "nhgis0027_shapefile_tl2000_us_tract_1960/US_tract_1960.shp"),
      file_select = starts_with("US_tract_1960")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR)) %>% 
  # classify occupations into skill groups
  mutate(
    high_skill_occupations = B9B001+B9B003+B9B013+B9B015,
    mid_skill_occupations = B9B004+B9B005+B9B006+B9B016+B9B017+B9B018,
    low_skill_occupations = B9B007+B9B008+B9B009+B9B010+B9B011+B9B019+B9B020+B9B021+B9B022+B9B023
  ) %>% 
  # keep relevant columns
  select(any_of(tract_background_variables), contains("occupations"))



## 1970  -------
full_tract_data_1970 <-
  ipums_shape_full_join(
    read_nhgis(
      here(occupation_data_dir, "nhgis0042_ds98_1970_tract.csv")),
    read_ipums_sf(
      here(gis_data_dir, "nhgis0027_shapefile_tl2000_us_tract_1970/US_tract_1970.shp"),
      file_select = starts_with("US_tract_1970")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR)) %>% 
  # classify occupations into skill groups
  mutate(
    high_skill_occupations = C08001+C08002,
    mid_skill_occupations = C08003+C08004+C08005,
    low_skill_occupations = C08006+C08007+C08008+C08009+C08010+C08011+C08012
  ) %>% 
  # keep relevant columns
  select(any_of(tract_background_variables), contains("occupations"))



## 1980  -------
full_tract_data_1980 <-
  ipums_shape_full_join(
    read_nhgis(
      here(occupation_data_dir, "nhgis0042_ds107_1980_tract.csv")),
    read_ipums_sf(
      here(gis_data_dir, "nhgis0027_shapefile_tl2000_us_tract_1980/US_tract_1980.shp"),
      file_select = starts_with("US_tract_1980")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR)) %>% 
  # classify occupations into skill groups
  mutate(
    high_skill_occupations = DIB001+DIB002+DIB003,
    mid_skill_occupations = DIB004+DIB005+DIB010,
    low_skill_occupations = DIB006+DIB007+DIB008+DIB009+DIB011+DIB012+DIB013
  ) %>% 
  # keep relevant columns
  select(any_of(tract_background_variables), contains("occupations"))


## 1990 -----
full_tract_data_1990 <-
  ipums_shape_full_join(
    read_nhgis(
      here(occupation_data_dir, "nhgis0042_ds123_1990_tract.csv")),
    read_ipums_sf(
      here(gis_data_dir, "nhgis0027_shapefile_tl2000_us_tract_1990/US_tract_1990.shp"),
      file_select = starts_with("US_tract_1990")
    ),
    by = "GISJOIN"
  ) %>%
  filter(!is.na(YEAR)) %>% 
  # classify occupations into skill groups
  mutate(
    high_skill_occupations = E4Q001 + E4Q002,
    mid_skill_occupations = E4Q003 + E4Q004 + E4Q005 + E4Q010,
    low_skill_occupations = E4Q006+E4Q007+E4Q008+E4Q009+E4Q011+E4Q012+E4Q013
  ) %>%
  # keep relevant columns
  select(any_of(tract_background_variables), contains("occupations"))


# Concord datasets to 1950 Census tracts ----
# 1. Join on crosswalk to get GISJOIN_1950 and weights
# 2. Weight occupations by "weight" and collapse to GISJOIN_1950
# 3. Merge geography information from 1950 NHGIS file

## Loop for 1940-1980 -----
years <- c(1940, 1960, 1970, 1980, 1990)

for (year in years) {
  print(year)
  # Construct variable names and file names dynamically based on the year
  tract_occupation_data_var <- paste0("full_tract_data_", year)
  tract_crosswalk_var <- paste0("tract_crosswalk_", year)
  tract_occupation_data_concorded_var <- paste0("tract_occupation_data_", year, "_concorded")
  
  # Execute the operations inside the loop
  assign(tract_occupation_data_concorded_var, 
         get(tract_occupation_data_var) %>% 
           # join on crosswalk
           left_join(get(tract_crosswalk_var), by = c("GISJOIN" = paste0("GISJOIN_", year))) %>%
           # weight occupations
           mutate_at(vars(contains("occupations")), ~ . * weight) %>%
           st_drop_geometry() %>% 
           # collapse to GISJOIN_1950
           group_by(GISJOIN_1950, YEAR) %>% 
           summarise_at(vars(contains("occupations")), sum, na.rm = TRUE) %>%
           # merge on 1950 tract IDs for each GISJOIN 
           left_join(tract_info_1950_gisjoin, by = c("GISJOIN_1950" = "GISJOIN")) 
  )
}

## Clean 1950 data to same format as concorded data  ----
tract_occupation_data_1950_concorded <-
  full_tract_data_1950 %>% 
  ungroup() %>% 
  dplyr::rename(GISJOIN_1950 = GISJOIN) %>% 
  select(GISJOIN_1950, YEAR, contains("occupations"))

# population data with the original tracts
# might use this to compare at some point
tract_occupation_data_original_tracts <-
  bind_rows(full_tract_data_1940, full_tract_data_1950,
            full_tract_data_1960, full_tract_data_1970,
            full_tract_data_1980, full_tract_data_1990) %>% 
  # calculate shares 
  mutate(high_skill_share = high_skill_occupations/(low_skill_occupations + mid_skill_occupations + high_skill_occupations),
         mid_skill_share = mid_skill_occupations/(low_skill_occupations + mid_skill_occupations + high_skill_occupations),
         low_skill_share = low_skill_occupations/(low_skill_occupations + mid_skill_occupations + high_skill_occupations)) %>%
  # drop rows with missing geometry
  filter(!st_is_empty(geometry)) 

# tract population data concorded to 1990 tracts
tract_occupation_data_concorded <-
  bind_rows(tract_occupation_data_1940_concorded, tract_occupation_data_1950_concorded,
            tract_occupation_data_1960_concorded, tract_occupation_data_1970_concorded,
            tract_occupation_data_1980_concorded, tract_occupation_data_1990_concorded) %>%
  # calculate shares
  mutate(high_skill_share = high_skill_occupations/(low_skill_occupations + mid_skill_occupations + high_skill_occupations),
         mid_skill_share = mid_skill_occupations/(low_skill_occupations + mid_skill_occupations + high_skill_occupations),
         low_skill_share = low_skill_occupations/(low_skill_occupations + mid_skill_occupations + high_skill_occupations)) %>%
  # drop rows with missing geometry
  filter(!st_is_empty(geometry)) 

# NOTE: There are some tracts in each year with NAs for shares, these are tracts where there are 0s for occupations
# I checked

# Save ----

# save concorded tract occupation data as shapefile
st_write(tract_occupation_data_concorded, here(output_dir, "tract_occupation_data.gpkg"),
         append = FALSE, layer = "points", driver = "GPKG")

# # save non-concorded tract_population_data as shapefile
# st_write(tract_occupation_data_original_tracts,
#          here(original_output_dir, "tract_occupation_data_original_tracts.gpkg"),
#          append = FALSE, layer = "points", driver = "GPKG")
# 
