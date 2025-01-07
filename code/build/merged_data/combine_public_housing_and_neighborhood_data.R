# Combined Public Housing and Neighborhood data

# Preliminaries -----
library(sf)
library(tigris)
library(crsuggest)
library(mapview)
library(maps)
library(ragg)
library(viridis)
library(geosphere)
library(here)
library(tidyverse)


rm(list = ls())


# !! Enter dataset choice. Can be "digitized", "cdd_large" or "cdd_small", or "combined" 
dataset_choice <- "combined"


# directories
merged_data_dir <- here("data", "derived", "merged")
ph_data_dir <- here("data", "derived", "public_housing", "working")

# File paths based on dataset choice
tract_with_treatment_status_filepath <- here(merged_data_dir, dataset_choice, "census_tract_sample_with_treatment_status_all.gpkg")
treated_tracts_panel_filepath <- here(merged_data_dir, dataset_choice, "treated_tracts_panel_all.gpkg")

cleaned_projects_filepath <- here(ph_data_dir, "cleaned_housing_projects.gpkg")

# Common functions and variables
tract_id_variables <- c("YEAR", "STATEA", "COUNTYA", "TRACTA")

# Function to filter census data for specific cities -----
filter_census_data <- function(census_data, dataset_choice) {
  if (dataset_choice == "cdd_large" | dataset_choice == "combined") {
    # Add filter conditions for large sample and combined
    census_data %>%
      filter(
        (COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond", "Westchester") & STATE == "New York") |
          (COUNTY %in% c("Cook") & STATE == "Illinois") |
          (COUNTY %in% c("Philadelphia") & STATE == "Pennsylvania") |
          (COUNTY %in% c("Wayne", "Oakland", "Macomb") & STATE == "Michigan") |
          (COUNTY %in% c("Los Angeles") & STATE == "California") |
          (STATE == "District Of Columbia") |
          (COUNTY %in% c("Cuyahoga", "Lake", "Franklin", "Hamilton", "Lucas", "Montgomery", "Summit") & STATE == "Ohio") |
          (COUNTY %in% c("Suffolk", "Middlesex") & STATE == "Massachusetts") |
          (COUNTY %in% c("Allegheny") & STATE == "Pennsylvania") |
          (COUNTY %in% c("Baltimore City") & STATE == "Maryland") |
          (COUNTY %in% c("St Louis", "St Louis City", "Jackson") & STATE == "Missouri") |
          (COUNTY %in% c("San Francisco", "Alameda", "Contra Costa") & STATE == "California") |
          (COUNTY %in% c("Fulton", "DeKalb", "Clayton", "Bibb", "Chatham", "Richmond") & STATE == "Georgia") |
          (COUNTY %in% c("Hennepin", "Ramsey", "Anoka", "Dakota") & STATE == "Minnesota") |
          (COUNTY %in% c("King") & STATE == "Washington") |
          (COUNTY %in% c("Jefferson") & STATE == "Alabama") |
          (COUNTY %in% c("Shelby", "Davidson") & STATE == "Tennessee") |
          (COUNTY %in% c("Orleans") & STATE == "Louisiana") |
          (COUNTY %in% c("Atlantic", "Camden", "Essex", "Hudson", "Mercer", "Passaic", "Union") & STATE == "New Jersey") |
          (COUNTY %in% c("Dallas", "Harris", "Travis") & STATE == "Texas") |
          (COUNTY %in% c("Erie", "Monroe", "Onondaga") & STATE == "New York") |
          (COUNTY %in% c("Milwaukee") & STATE == "Wisconsin") |
          (COUNTY %in% c("Denver", "Adams") & STATE == "Colorado") |
          (COUNTY %in% c("Providence") & STATE == "Rhode Island") |
          (COUNTY %in% c("Marion") & STATE == "Indiana") |
          (COUNTY %in% c("Oklahoma") & STATE == "Oklahoma") |
          (COUNTY %in% c("Multnomah") & STATE == "Oregon") |
          (COUNTY %in% c("Henrico", "Richmond City") & STATE == "Virginia") |
          (COUNTY %in% c("Polk") & STATE == "Iowa") |
          (COUNTY %in% c("Hartford", "New Haven") & STATE == "Connecticut") |
          (COUNTY %in% c("Madison", "St Clair") & STATE == "Illinois") |
          (COUNTY %in% c("Genesee") & STATE == "Michigan") |
          (COUNTY %in% c("Aiken") & STATE == "South Carolina")
      )
  } else if (dataset_choice == "cdd_small") {
    census_data %>%
      filter(
        (COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York") |
          (COUNTY %in% c("Cook") & STATE == "Illinois") |
          (COUNTY %in% c("Wayne") & STATE == "Michigan") |
          (COUNTY %in% c("Cuyahoga") & STATE == "Ohio") |
          (COUNTY %in% c("Philadelphia") & STATE == "Pennsylvania") |
          (COUNTY %in% c("Baltimore City") & STATE == "Maryland") |
          (COUNTY %in% c("Allegheny") & STATE == "Pennsylvania") |
          (COUNTY %in% c("Hamilton") & STATE == "Ohio") |
          (STATE == "District Of Columbia") |
          (COUNTY %in% c("Suffolk", "Middlesex", "Norfolk") & STATE == "Massachusetts") |
          (COUNTY %in% c("St Louis City") & STATE == "Missouri") |
          (COUNTY %in% c("San Francisco") & STATE == "California") |
          (COUNTY %in% c("Los Angeles") & STATE == "California") |
          (COUNTY %in% c("Dade") & STATE == "Florida") |
          (COUNTY %in% c("Fulton") & STATE == "Georgia")
      )
  } else if (dataset_choice == "digitized") {
    census_data %>%
      filter(
        (COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York") |
          (COUNTY %in% c("Cook") & STATE == "Illinois") |
          (COUNTY %in% c("Baltimore City") & STATE == "Maryland") |
          (STATE == "District Of Columbia") |
          (COUNTY %in% c("Suffolk", "Middlesex", "Norfolk") & STATE == "Massachusetts") |
          (COUNTY %in% c("San Francisco") & STATE == "California") |
          (COUNTY %in% c("Los Angeles") & STATE == "California") |
          (COUNTY %in% c("Fulton") & STATE == "Georgia")
      )
  }
}

# Function to process public housing data -----
process_public_housing <- function(dataset_choice) {
  if (dataset_choice == "cdd_small" | dataset_choice == "cdd_large") {
    cdd_projects_merged <- read_csv(here(ph_data_dir, "merged_cdd_projects.csv"))

    cdd_projects_geolocated <- cdd_projects_merged %>%
      # keep only geolocated public housing projects
      filter(!is.na(latitude), !is.na(longitude)) %>% 
      # rename columns
      rename(total_public_housing_units = totunitsplanned,
             year_completed = yearfullocc,
             project_name = name) %>% 
      # drop if year completed is NA
      filter(!is.na(year_completed)) 

    
    # convert to sf object
    cdd_projects_geolocated_sf <- cdd_projects_geolocated %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    return(cdd_projects_geolocated_sf)
    
  } else if (dataset_choice == "digitized") {
    public_housing_projects <- read_csv(here(ph_data_dir, "geocoded_projects.csv"))
      
    public_housing_projects_geolocated <- public_housing_projects %>%
      # keep only geolocated public housing projects
      filter(!is.na(lat), !is.na(long)) %>% 
      # rename total_units to total_public_housing_units
      dplyr::rename(total_public_housing_units = total_units) %>% 
      # drop if year completed is NA
      filter(!is.na(year_completed)) %>% 
      # replace project_code_full with project_code if project_code_full is missing
      mutate(project_code_full = ifelse(is.na(project_code_full), project_code, project_code_full)) 
    
    
    # convert to sf object
    public_housing_projects_geolocated_sf <- public_housing_projects_geolocated %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326)
    
    return(public_housing_projects_geolocated_sf)
    
  } else if (dataset_choice == "combined") {
    public_housing_projects <- read_csv(here(ph_data_dir, "combined_cdd_and_digitized_projects.csv"))
    
    public_housing_projects_geolocated <- public_housing_projects %>%
      # keep only geolocated public housing projects
      filter(!is.na(latitude), !is.na(longitude)) %>% 
      # rename total_units to total_public_housing_units
      dplyr::rename(total_public_housing_units = total_units) %>% 
      # drop if year completed is NA
      filter(!is.na(year_completed)) 
    
    # convert to sf object
    public_housing_projects_geolocated_sf <- public_housing_projects_geolocated %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    return(public_housing_projects_geolocated_sf)
  }
} 


# Function to assign cities to Census counties -----
assign_census_cities <- function(census_data) {
  census_data %>% 
  mutate(city = case_when(
      # Birmingham
      COUNTY == "Jefferson" & STATE == "Alabama" ~ "Birmingham",
      
      # San Francisco Bay Area cities
      COUNTY == "Alameda" & STATE == "California" ~ "Oakland area",
      COUNTY == "Contra Costa" & STATE == "California" ~ "Concord area",
      COUNTY == "San Francisco" & STATE == "California" ~ "San Francisco",
      
      # Los Angeles
      COUNTY == "Los Angeles" & STATE == "California" ~ "Los Angeles",
      
      # Denver
      COUNTY %in% c("Adams", "Denver") & STATE == "Colorado" ~ "Denver",
      
      # Hartford
      COUNTY == "Hartford" & STATE == "Connecticut" ~ "Hartford",
      
      # New Haven
      COUNTY == "New Haven" & STATE == "Connecticut" ~ "New Haven",
      
      # Washington D.C.
      STATE == "District Of Columbia" ~ "Washington DC",
      
      # Atlanta
      COUNTY %in% c("Clayton", "DeKalb", "Fulton") & STATE == "Georgia" ~ "Atlanta",
      
      # Other Georgia cities
      COUNTY == "Bibb" & STATE == "Georgia" ~ "Macon",
      COUNTY == "Chatham" & STATE == "Georgia" ~ "Savannah",
      COUNTY == "Richmond" & STATE == "Georgia" ~ "Augusta",
      
      # Chicago
      COUNTY == "Cook" & STATE == "Illinois" ~ "Chicago",
      
      # East St. Louis area
      COUNTY %in% c("Madison", "St Clair") & STATE == "Illinois" ~ "East St. Louis",
      
      # Indianapolis
      COUNTY == "Marion" & STATE == "Indiana" ~ "Indianapolis",
      
      # Des Moines
      COUNTY == "Polk" & STATE == "Iowa" ~ "Des Moines",
      
      # New Orleans
      COUNTY == "Orleans" & STATE == "Louisiana" ~ "New Orleans",
      
      # Baltimore
      COUNTY == "Baltimore City" & STATE == "Maryland" ~ "Baltimore",
      
      # Boston
      COUNTY %in% c("Middlesex", "Suffolk") & STATE == "Massachusetts" ~ "Boston",
      
      # Detroit
      COUNTY %in% c("Macomb", "Oakland", "Wayne") & STATE == "Michigan" ~ "Detroit",
      
      # Flint
      COUNTY == "Genesee" & STATE == "Michigan" ~ "Flint",
      
      # Minneapolis-St. Paul
      COUNTY %in% c("Anoka", "Dakota", "Hennepin", "Ramsey") & STATE == "Minnesota" ~ "Minneapolis-St. Paul",
      
      # St. Louis
      COUNTY %in% c("St Louis", "St Louis City") & STATE == "Missouri" ~ "St. Louis",
      
      # Kansas City
      COUNTY == "Jackson" & STATE == "Missouri" ~ "Kansas City",
      
      # New Jersey cities
      COUNTY == "Atlantic" & STATE == "New Jersey" ~ "Atlantic City",
      COUNTY == "Camden" & STATE == "New Jersey" ~ "Camden",
      COUNTY == "Essex" & STATE == "New Jersey" ~ "Newark",
      COUNTY == "Hudson" & STATE == "New Jersey" ~ "Jersey City",
      COUNTY == "Mercer" & STATE == "New Jersey" ~ "Trenton",
      COUNTY == "Passaic" & STATE == "New Jersey" ~ "Paterson",
      COUNTY == "Union" & STATE == "New Jersey" ~ "Elizabeth",
      
      # New York City
      COUNTY %in% c("Bronx", "Kings", "New York", "Queens", "Richmond", "Westchester") & STATE == "New York" ~ "New York City",
      
      # Buffalo
      COUNTY == "Erie" & STATE == "New York" ~ "Buffalo",
      
      # Rochester
      COUNTY == "Monroe" & STATE == "New York" ~ "Rochester",
      
      # Syracuse
      COUNTY == "Onondaga" & STATE == "New York" ~ "Syracuse",
      
      # Cleveland
      COUNTY %in% c("Cuyahoga", "Lake") & STATE == "Ohio" ~ "Cleveland",
      
      # Columbus
      COUNTY == "Franklin" & STATE == "Ohio" ~ "Columbus",
      
      # Cincinnati
      COUNTY == "Hamilton" & STATE == "Ohio" ~ "Cincinnati",
      
      # Toledo
      COUNTY == "Lucas" & STATE == "Ohio" ~ "Toledo",
      
      # Dayton
      COUNTY == "Montgomery" & STATE == "Ohio" ~ "Dayton",
      
      # Akron
      COUNTY == "Summit" & STATE == "Ohio" ~ "Akron",
      
      # Oklahoma City
      COUNTY == "Oklahoma" & STATE == "Oklahoma" ~ "Oklahoma City",
      
      # Portland
      COUNTY == "Multnomah" & STATE == "Oregon" ~ "Portland",
      
      # Pittsburgh
      COUNTY == "Allegheny" & STATE == "Pennsylvania" ~ "Pittsburgh",
      
      # Philadelphia
      COUNTY == "Philadelphia" & STATE == "Pennsylvania" ~ "Philadelphia",
      
      # Providence
      COUNTY == "Providence" & STATE == "Rhode Island" ~ "Providence",
      
      # South Carolina
      COUNTY == "Aiken" & STATE == "South Carolina" ~ "Aiken",
      
      # Nashville
      COUNTY == "Davidson" & STATE == "Tennessee" ~ "Nashville",
      
      # Memphis
      COUNTY == "Shelby" & STATE == "Tennessee" ~ "Memphis",
      
      # Texas cities
      COUNTY == "Dallas" & STATE == "Texas" ~ "Dallas",
      COUNTY == "Harris" & STATE == "Texas" ~ "Houston",
      COUNTY == "Travis" & STATE == "Texas" ~ "Austin",
      
      # Richmond
      COUNTY %in% c("Henrico", "Richmond City") & STATE == "Virginia" ~ "Richmond",
      
      # Seattle
      COUNTY == "King" & STATE == "Washington" ~ "Seattle",
      
      # Milwaukee
      COUNTY == "Milwaukee" & STATE == "Wisconsin" ~ "Milwaukee",
      
      # Duluth
      COUNTY == "St Louis" & STATE == "Minnesota" ~ "Duluth",
      
      # Louisville 
      COUNTY == "Jefferson" & STATE == "Kentucky" ~ "Louisville",
      
      TRUE ~ NA_character_  # For any counties not specified
    ))
}

# Function to calculate distance between Census tract and nearest CBD tract ----
calculate_distance_to_cbd <- function(census_data) {  
  
  # get CBD tracts
  cbd_tracts <- census_data %>%
    filter(cbd == 1) %>%
    # keep one of each tract
    group_by(STATEA, COUNTYA, TRACTA) %>% 
    filter(row_number() ==1) %>%
    dplyr::select(STATEA, COUNTYA, TRACTA, geom) %>% 
    ungroup()
  
  # get all tracts
  all_sample_tracts <- census_data %>%
    # keep one of each tract
    group_by(STATEA, COUNTYA, TRACTA) %>% 
    filter(row_number() ==1) %>%
    dplyr::select(STATEA, COUNTYA, TRACTA, geom) %>% 
    ungroup()
  
  # calculate nearest CBD
  nearest_cbd <- st_nearest_feature(all_sample_tracts, cbd_tracts)
  cbd_distances <- st_distance(all_sample_tracts, cbd_tracts[nearest_cbd,], by_element = TRUE)
  
  all_sample_tracts <- 
    all_sample_tracts %>%
    mutate(distance_from_cbd = cbd_distances) %>%
    st_drop_geometry()
  
  output <- census_data %>%
    left_join(all_sample_tracts) %>%
    mutate(distance_from_cbd = ifelse(cbd == 1, 0, distance_from_cbd))
  
  return(output)
}


# Main processing pipeline: ----

## Read and process census data ----
census_tract_data_full <- 
  read_sf(here(merged_data_dir, "census_tract_data_full.gpkg")) %>% 
  # create state-county id
  mutate(state_county = paste0(STATE, COUNTY))


# Keep only cities for which tract-level data existed in 1940
# 09/13/2024: Keep counties that exist in 1940
counties_1940 <-
  census_tract_data_full %>% 
  filter(YEAR == 1940) %>%
  pull(state_county) %>% 
  unique()


# Samples that only contain counties that have Census tract data in 1940
# Note: I will do further balancing later
census_tract_sample <-
  census_tract_data_full %>%
  filter(state_county %in% counties_1940)

census_tract_sample <-
  assign_census_cities(census_tract_sample)

# Convert Census data to geographic coordinate system
census_tract_sample <-
  census_tract_sample %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  st_make_valid()

# get distance form nearest cbd, add as a column
census_tract_sample <-
  calculate_distance_to_cbd(census_tract_sample)

## Process public housing data----
public_housing_data <- process_public_housing(dataset_choice)

# Transform public housing data to match census data CRS
public_housing_data <- st_transform(public_housing_data, st_crs(census_tract_sample))

# Create treatment year variable
public_housing_data <-
  public_housing_data %>%
  mutate(treatment_year = case_when(
    year_completed %in% 1931:1940 ~ 1940,
    year_completed %in% 1941:1950 ~ 1950,
    year_completed %in% 1951:1960 ~ 1960,
    year_completed %in% 1961:1970 ~ 1970,
    year_completed %in% 1971:1980 ~ 1980,
    TRUE ~ NA_real_
  ))

## Merge Census and Public Housing data -----
### Identify which Census tracts received public housing by merging the two datasets -----
treated_tracts <-
  st_join(census_tract_sample, public_housing_data, join = st_intersects)




### Create a panel tract dataset of public housing population per year ----
# 1. Get unique treated tracts and years, all of them
unique_treated_tracts_panel <- 
  treated_tracts %>%
  dplyr::select(STATE, COUNTY, TRACTA, YEAR) %>% 
  st_drop_geometry() %>% 
  distinct()


# 2. Get dataset of public housing population per tract per year
public_housing_pop_estimates <-
  treated_tracts %>% 
  filter(!is.na(treatment_year)) %>% 
  st_drop_geometry() %>% 
  group_by(STATE, COUNTY, TRACTA, YEAR, treatment_year) %>%
  summarise(total_public_housing_pop_estimate = sum(proj_total_population_estimate)) %>%
  mutate(total_public_housing_pop_estimate = case_when(YEAR >= treatment_year ~ total_public_housing_pop_estimate,
                                                    TRUE ~ NA_real_)) %>%
  dplyr::select(STATE, COUNTY, TRACTA, YEAR, treatment_year, total_public_housing_pop_estimate) %>% 
  # collapse to each year
  group_by(STATE, COUNTY, TRACTA, YEAR) %>%
  summarise(total_public_housing_pop_estimate = sum(total_public_housing_pop_estimate, na.rm = TRUE)) %>% 
  # if a tract is 0 every year, replace with NA
  group_by(STATE, COUNTY, TRACTA) %>%
  mutate(all_years_estimate = sum(total_public_housing_pop_estimate)) %>%
  mutate(total_public_housing_pop_estimate = ifelse(all_years_estimate == 0, NA, total_public_housing_pop_estimate)) %>% 
  dplyr::select(-all_years_estimate) %>% 
  ungroup() 

### Filter which tracts we consider treated ----
# Consider a tract as treated if:
# 1. Share of units > 5% 
# 2. Number of units >= 30 (there are some tracts which have issue with total housing units measure from Census,
# so I filter out small number of units like this)
# aggregate public housing units constructed in each tract by year
treated_tracts_aggregated_by_year <- 
  treated_tracts %>%
  group_by(STATE, COUNTY, TRACTA, YEAR, treatment_year) %>%
  mutate(total_public_housing_units = sum(total_public_housing_units, na.rm = TRUE)) 



share_of_units_treated <- 
  treated_tracts_aggregated_by_year %>%
  filter(YEAR == treatment_year) %>% 
  mutate(share_of_housing_units_that_are_public_in_treatment_year =
           total_public_housing_units /total_units) %>%
  dplyr::select(STATE, COUNTY, TRACTA, treatment_year, share_of_housing_units_that_are_public_in_treatment_year) %>% 
  st_drop_geometry() %>% 
  distinct()

# filter by share of public housing units
treated_tracts_aggregated_by_year <- 
  treated_tracts_aggregated_by_year %>%
  left_join(share_of_units_treated) %>%
  filter(share_of_housing_units_that_are_public_in_treatment_year > 0.05 &
           total_public_housing_units >= 30)
  

#  keep only one observation per year
treated_tracts_aggregated_by_year <-
  treated_tracts_aggregated_by_year %>%
  arrange(STATE, COUNTY, TRACTA, treatment_year) %>%
  # for each treated tract and year, keep only the one with the smallest treatment year (eg keep first year of treatment)
  group_by(STATE, COUNTY, TRACTA, YEAR) %>%
  filter(row_number() == 1) %>%
  ungroup()

# keep a panel of treated tracts, only when they are treated
treated_tracts_panel <- 
  treated_tracts_aggregated_by_year %>%
  filter(YEAR >= treatment_year) %>%
  dplyr::select(TRACTA, COUNTY, STATE, YEAR, city, cbsa_title, treatment_year, total_public_housing_units) %>%
  mutate(treated = 1)

# Merge on treatment status of each tract to the Census data
census_tract_sample_with_treatment_status <- left_join(
  census_tract_sample, 
  treated_tracts_panel %>% dplyr::select(TRACTA, COUNTY, STATE, YEAR, treated) %>% st_drop_geometry()) %>%
  mutate(treated = ifelse(is.na(treated), 0, 1)) %>% 
  # merge on public housing population
  left_join(public_housing_pop_estimates, by = c("STATE", "COUNTY", "TRACTA", "YEAR")) %>% 
  # set total public housing population equal to 0 if treated == 0
  mutate(total_public_housing_pop_estimate = ifelse(treated == 0, 0, total_public_housing_pop_estimate)) %>%
  # calculate private population
  mutate(private_population_estimate = total_pop - total_public_housing_pop_estimate) %>% 
  # if private population is negative, set equal to 0 (these are cases where
  # estimated public housing population is greater than Census-reported total population)
  mutate(private_population_estimate = ifelse(private_population_estimate < 0, 0, private_population_estimate))


## Output datasets -----
# Census tracts with treatment status
st_write(census_tract_sample_with_treatment_status, tract_with_treatment_status_filepath,
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

# panel of treated tracts beginning with their treatment year
st_write(treated_tracts_panel, treated_tracts_panel_filepath,
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

# cleaned public housing data
st_write(public_housing_data, cleaned_projects_filepath,
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

