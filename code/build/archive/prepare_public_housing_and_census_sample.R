####
# Combine data and prep sample:
# 1. Read in census tract pop, housing, education, income data, and combine 
# 2. Define sample of cities
# 3. Geocode public housing data
# 3. Merge census tract data with public housing data 
# 4. Define treated status of Census tracts


# Output:
# 1. Census tract data sample
# 2. Public housing data sample
# 3. Treated tract sample 
####

# Preliminaries -----
library(tidyverse)
library(sf)
library(tigris)
library(crsuggest)
library(mapview)
library(maps)
library(ragg)
library(viridis)
library(geosphere)

# Prep Census data ----

## Read in census tract data ----

tract_id_variables <-  c("YEAR", "STATEA", "COUNTYA", "TRACTA")

### Population by race, total pop, population density, CBD designation
census_pop_data <- 
  read_sf("data/derived/census/tract_population_data.gpkg") 

## Housing: 
census_housing_data <-
  read_sf("data/derived/census/tract_housing_data.gpkg") %>% 
  # drop geometry 
  st_drop_geometry() %>% 
  # keep only merging variables and median variables
  select(any_of(tract_id_variables), contains("median"), total_units, total_units, vacancy_rate,
         share_needing_repair, share_no_water, housing_density)
  

## Median income
census_income_data <-
  read_sf("data/derived/census/tract_income_data.gpkg") %>% 
  # drop geometry
  st_drop_geometry() %>%
  # keep only merging variables and median variables
  select(any_of(tract_id_variables), contains("median"))

## Median education
# I have illiteracy in 1920 and 1930, but for now, don't use it
# Still need to clean illiteracy data
census_education_data <-
  read_sf("data/derived/census/tract_education_data.gpkg") %>% 
  filter(year >= 1940) %>% 
  # drop geometry
  st_drop_geometry() %>%
  # keep only merging variables and median variables
  select(any_of(tract_id_variables), contains("median"))

## employment 
census_employment_data <-
  read_sf("data/derived/census/tract_employment_data.gpkg") %>% 
  # drop geometry
  st_drop_geometry() %>%
  # keep only merging variables and median variables
  select(any_of(tract_id_variables), contains("pop"), contains("rate"))

## Combine census tract data ----
census_tract_data_full <-
  census_pop_data %>% 
  left_join(census_housing_data, by = c("YEAR", "STATEA", "COUNTYA", "TRACTA")) %>% 
  left_join(census_income_data, by = c("YEAR", "STATEA", "COUNTYA", "TRACTA")) %>%
  left_join(census_education_data, by = c("YEAR", "STATEA", "COUNTYA", "TRACTA")) %>%
  left_join(census_employment_data, by = c("YEAR", "STATEA", "COUNTYA", "TRACTA")) %>% 
  # create variables 
  mutate(employment_pop_ratio = employed_pop/total_pop)


## Get only cities in our sample ----
# list of cities: 
# 1.NYC,
# 2. Chicago
# 3. Detroit
# 4. Cleveland
# 5. Philadelphia
# 6. Baltimore
# 7. Pittsburgh
# 8. Cincinnati
# 9. Washington DC
# 10. Boston
# 11. St. Louis
# 12. San Francisco
# 13. Los Angeles
# 14. Miami
# 15. Atlanta

census_tract_sample <-
  census_tract_data_full %>% 
  # 1. new york city
  filter((COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York") |
           # 2. chicago
           (COUNTY %in% c("Cook") & STATE == "Illinois") | 
           # 3. detroit
           (COUNTY %in% c("Wayne") & STATE == "Michigan") |
           # 4. cleveland
           (COUNTY %in% c("Cuyahoga") & STATE == "Ohio") |
           # 5. philadelphia
           (COUNTY %in% ("Philadelphia") & STATE == "Pennsylvania") |
           # 6. baltimore
           (COUNTY %in% ("Baltimore City") & STATE == "Maryland") | 
           # 7. pittsburgh
           (COUNTY %in% ("Allegheny") & STATE == "Pennsylvania") |
           # 8. cincinnati
           (COUNTY %in% ("Hamilton") & STATE == "Ohio") |
           # 9. washington dc
           (STATE == "District Of Columbia") |
           # 10. boston
           (COUNTY %in% ("Suffolk") & STATE == "Massachusetts") |
           # 11. st. louis
           (COUNTY %in% ("St Louis City") & STATE == "Missouri") |
           # 12. san francisco
           (COUNTY %in% ("San Francisco") & STATE == "California") |
           # 13. los angeles
           (COUNTY %in% ("Los Angeles") & STATE == "California") |
           # 14. miami
           (COUNTY %in% ("Dade") & STATE == "Florida") |
           # 15. atlanta
           (COUNTY %in% ("Fulton") & STATE == "Georgia")) %>%
  # create an variable called "city"
  mutate(city = case_when(
    COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York" ~ "New York City",
    COUNTY %in% c("Cook") & STATE == "Illinois" ~ "Chicago",
    COUNTY %in% c("Wayne") & STATE == "Michigan" ~ "Detroit",
    COUNTY %in% c("Cuyahoga") & STATE == "Ohio" ~ "Cleveland",
    COUNTY %in% ("Philadelphia") & STATE == "Pennsylvania" ~ "Philadelphia",
    COUNTY %in% ("Baltimore City") & STATE == "Maryland" ~ "Baltimore",
    COUNTY %in% ("Allegheny") & STATE == "Pennsylvania" ~ "Pittsburgh",
    COUNTY %in% ("Hamilton") & STATE == "Ohio" ~ "Cincinnati",
    STATE == "District Of Columbia" ~ "Washington DC",
    COUNTY %in% ("Suffolk") & STATE == "Massachusetts" ~ "Boston",
    COUNTY %in% ("St Louis City") & STATE == "Missouri" ~ "St. Louis",
    COUNTY %in% ("San Francisco") & STATE == "California" ~ "San Francisco",
    COUNTY %in% ("Los Angeles") & STATE == "California" ~ "Los Angeles",
    COUNTY %in% ("Dade") & STATE == "Florida" ~ "Miami",
    COUNTY %in% ("Fulton") & STATE == "Georgia" ~ "Atlanta"
  ))


## Convert Census data to geographic coordinate system (from projected data) ----
census_tract_sample <-
  census_tract_sample %>% 
  st_as_sf() %>% 
  st_transform(4326) %>% 
  # fix the 21 broken geometries
  st_make_valid()
  


## Distance from nearest CBD tract -----
# get cbd tracts in sample
cbd_tracts <-
  census_tract_sample %>%
  filter(cbd == 1, YEAR == 1990) %>% 
  #mutate(centroid = st_centroid(geom)) %>% 
  select(any_of(tract_id_variables), geom)

# get all non-CBD tracts
all_sample_tracts <- 
  census_tract_sample %>%
  filter(cbd == 0, YEAR == 1990) %>% 
  select(any_of(tract_id_variables), -YEAR, geom)
# %>% 
#  mutate(centroid = st_centroid(geom)) 

# calculate minimum distance for each tract
y <- map_dbl(1:nrow(all_sample_tracts), function(i) {
  min(st_distance(all_sample_tracts[i, ], cbd_tracts))
})

all_sample_tracts <- 
  all_sample_tracts %>% 
  mutate(distance_from_cbd = y) %>% 
  st_drop_geometry()

# merge back to the full data
census_tract_sample <- 
  census_tract_sample %>% 
  left_join(all_sample_tracts) %>% 
  # set distance to 0 for CBD tracts
  mutate(distance_from_cbd = ifelse(cbd == 1, 0, distance_from_cbd))


# Prep Public housing data ----
# read data
cdd_projects_merged <- 
  read_csv("data/derived/public_housing/working/merged_cdd_projects.csv")

## geolocation ----
# Get longitude and latitude from NGDA and PSH 
cdd_projects_geolocated <-
  cdd_projects_merged %>% 
  # drop if missing latitude or longitude
  filter(!is.na(latitude), !is.na(longitude))

# convert to sf object
cdd_projects_geolocated_sf <- 
  cdd_projects_geolocated %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# note: Here, 4326 is the EPSG code for WGS 84, the standard coordinate reference system 
# for latitude and longitude. 

# make sure the CRS are matching 
cdd_projects_geolocated_sf <- 
  st_transform(cdd_projects_geolocated_sf, st_crs(census_tract_sample))

st_crs(cdd_projects_geolocated_sf)

# look at only the locations
cdd_projects_geolocated_sf_loc_only <- 
  cdd_projects_geolocated_sf %>% 
  select(project_code, geometry)

# get sample of projects in 15 locations 

cdd_project_sample <-
  cdd_projects_geolocated_sf %>% 
  filter(
    #1. new york city
    locality == "NEW YORK CITY" |
    #2. chicago  
    locality == "CHICAGO" | 
    # 3. detroit
    (locality == "DETROIT" & state == "MICHIGAN") |
    # 4. cleveland
    locality == "CLEVELAND-CUYAHOGA METROPOLITAN" |
    # 5. philadelphia
    locality == "PHILADELPHIA" |
    # 6. baltimore
    locality == "BALTIMORE CITY" |
    # 7. pittsburgh
    locality == "PITTSBURGH" |
    # 8. cincinnati
    locality == "CINCINNATI-CINCINNATI METROPOLITAN" | 
    # 9. washington dc
    locality == "WASHINGTON DC-NATIONAL CAPITOL" | 
    # 10. boston
    (locality == "BOSTON" & state == "MASSACHUSETTS") |
    # 11. st. louis
    locality == "ST LOUIS CITY" |
    # 12. san francisco
    locality == "SAN FRANCISCO" |
    # 13. los angeles
    locality == "LOS ANGELES" |
    # 14. miami
    locality == "MIAMI-DADE COUNTY HSG & URB DEV" |
    # 15. atlanta
    locality == "ATLANTA" ) %>% 
  # only keep if yearfullocc is not misisng
  filter(!is.na(yearfullocc))

# count number of projects by city 
cdd_project_sample %>% 
  st_drop_geometry() %>%
  count(locality)

# Merge Census and Public Housing data ----
# Here, we will merge and also define "treated" status for each tract in each year using the 
# year of full occupancy

# Define concordance between dates in census_tract_sample and cdd_project_sample
# In particular: Going to follow Guennwig-Moehnert (2023) and define any tract that 
# receives public housing in 1941-1950 as treated in 1950 onward.

cdd_project_sample <- 
  cdd_project_sample %>% 
  # create treatment year = 1940 if yearfullocc = 1931-1940, treatment_year = 1950 if yearfullocc = 1941-1950, etc, up through yearfullocc = 1970
  mutate(treatment_year = case_when(
    yearfullocc %in% 1931:1940 ~ 1940,
    yearfullocc %in% 1941:1950 ~ 1950,
    yearfullocc %in% 1951:1960 ~ 1960,
    yearfullocc %in% 1961:1970 ~ 1970,
    yearfullocc %in% 1971:1980 ~ 1980,
    TRUE ~ NA_real_
  ))


# index the census tract sample data
census_tract_sample_indexed <- 
  census_tract_sample %>% 
  mutate(tract_id = row_number())

# join on geometry, then filter only where YEAR >= treatment_year
# That's when we want to consider the tract treated (eg in 1950, treated if project was built 1941-1950)
treated_tracts <-
  st_join(census_tract_sample_indexed,
          cdd_project_sample, 
          join = st_intersects)

# 2/15/2024: Assume "treatment" happens once: So for each treated tract:
# 1. Sum up the total units planned for the tract in each yearfullocc (in case there's multiple projects planned)
# 2. Keep only the first instance of each year
# sort by yearfullocc and tract,
# group by state, county, tracta, year
# and keep only the first instance for each year

treated_tracts <-
  treated_tracts %>% 
  group_by(STATE, COUNTY, TRACTA, YEAR, yearfullocc) %>% 
  mutate(totunitsplanned = sum(totunitsplanned, na.rm = TRUE)) %>%
  arrange(STATE, COUNTY, TRACTA, yearfullocc) %>% 
  group_by(STATE, COUNTY, TRACTA, YEAR) %>%
  filter(row_number() == 1) %>%
  ungroup()

# get panel of treated tracts
treated_tracts_panel <-
  treated_tracts %>% 
  # keep if census year >= treatment year
  filter(YEAR >= treatment_year) %>% 
  #st_drop_geometry() %>% 
  select(tract_id, TRACTA, COUNTY, STATE, YEAR, treatment_year, totunitsplanned) %>%
  mutate(treated = 1)

# census sample with treated status
census_tract_sample_with_treatment_status <-
  left_join(census_tract_sample_indexed, treated_tracts_panel %>% select(tract_id, treated) %>% st_drop_geometry(), by = "tract_id") %>% 
  mutate(treated = ifelse(is.na(treated), 0, 1))



# Output datasets as GeoPackage files-------
# Output:
# 1. Census tract sample (with treated status)
# 2. Geolocated public housing project sample
# 3. Treated tracts panel only

# 1.
st_write(census_tract_sample_with_treatment_status, "data/derived/merged/census_tract_sample_with_treatment_status.gpkg",
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

# 2.
st_write(cdd_project_sample, "data/derived/public_housing/working/cdd_housing_project_sample.gpkg",
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

# 3.
st_write(treated_tracts_panel, "data/derived/merged/treated_tracts_panel.gpkg",
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)


