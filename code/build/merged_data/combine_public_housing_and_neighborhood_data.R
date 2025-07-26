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
tract_id_variables <- c("YEAR", "GISJOIN_1950")

# Public housing units threshold: 
# I will exclude tiny projects
public_housing_units_minimum <- 30


# Functions -----
## Function to filter census data for specific cities -----
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

## Function to process public housing data -----
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


## Function to assign cities to Census counties -----
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

## Function to calculate distance between Census tract and nearest CBD tract ----
calculate_distance_to_cbd <- function(census_data) {  
  
  # get CBD tracts
  cbd_tracts <- census_data %>%
    filter(cbd == 1) %>%
    # keep one of each tract
    group_by(GISJOIN_1950) %>% 
    filter(row_number() ==1) %>%
    dplyr::select(GISJOIN_1950, geom) %>% 
    ungroup()
  
  # get all tracts
  all_sample_tracts <- census_data %>%
    # keep one of each tract
    group_by(GISJOIN_1950) %>% 
    filter(row_number() ==1) %>%
    dplyr::select(GISJOIN_1950, geom) %>% 
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

## Calculate distance to nearest public housing project for all tracts-----
calculate_distance_to_project <- function(census_data) {  
  
  # get all tracts
  all_sample_tracts <- 
    census_data %>%
    # keep one of each tract
    group_by(GISJOIN_1950) %>% 
    filter(row_number() ==1) %>%
    dplyr::select(GISJOIN_1950, geom) %>% 
    ungroup()
    
    
    # calculate nearest CBD
  nearest_project <- st_nearest_feature(all_sample_tracts, public_housing_data)
  project_distances <- st_distance(all_sample_tracts, public_housing_data[nearest_project,], 
                                   by_element = TRUE)
  
  all_sample_tracts <- 
    all_sample_tracts %>%
    mutate(distance_from_project = project_distances) %>%
    st_drop_geometry()
  
  output <- all_sample_tracts 
  
  return(output)
}

## Function to visualize a specific project with buffer and affected tracts -----
visualize_project <- function(project_name_to_plot, buffer_distance = 50) {
  
  # Get the specific project
  selected_project <- public_housing_data %>%
    filter(project_name == project_name_to_plot)
  
  if(nrow(selected_project) == 0) {
    stop("Project not found. Check project name.")
  }
  
  # Create buffer for this project
  selected_project_buffered <- st_buffer(selected_project, dist = buffer_distance)
  
  # Get tracts that intersect this project's buffer
  affected_tracts <- st_join(census_tract_sample, selected_project_buffered, join = st_intersects) %>%
    filter(!is.na(treatment_year)) %>%
    filter(project_name == project_name_to_plot)
  
  # Create a larger area around the project for context
  project_area <- st_buffer(selected_project, dist = 500) # 500m for context
  nearby_tracts <- st_filter(census_tract_sample, project_area)
  
  # Plot
  ggplot() +
    # All nearby tracts (light gray)
    geom_sf(data = nearby_tracts, fill = "lightgray", color = "white", size = 0.5) +
    # Affected tracts (highlighted)
    geom_sf(data = affected_tracts, fill = "red", alpha = 0.5, color = "darkred", size = 1) +
    # Project buffer
    geom_sf(data = selected_project_buffered, fill = NA, color = "blue", size = 1.5, linetype = "dashed") +
    # Project point
    geom_sf(data = selected_project, color = "blue", size = 3) +
    labs(title = paste("Project:", project_name_to_plot),
         subtitle = paste("Buffer:", buffer_distance, "m |", nrow(affected_tracts), "tracts affected |", 
                         selected_project$total_public_housing_units, "units")) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank())
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

# Exclude small public housing projects
public_housing_data <-
  public_housing_data %>%
  filter(total_public_housing_units >= public_housing_units_minimum) %>%
  # Keep only projects that were completed before 1980
  filter(year_completed < 1980)

## Merge Census and Public Housing data -----
### Identify which Census tracts received public housing by merging the two datasets -----
### Method 1: Buffer intersection (PRIMARY METHOD)
# Create 100m buffer around public housing locations
public_housing_buffered <- st_buffer(public_housing_data, dist = 50) 

# Find tracts within .1 km of public housing
treated_tracts <- 
  st_join(census_tract_sample, public_housing_buffered, join = st_intersects)

treated_tracts <-
  treated_tracts %>% 
  filter(!is.na(treatment_year))

# Calculate number of tracts per project for population splitting
tracts_per_project <-
  treated_tracts %>% 
  st_drop_geometry() %>%
  group_by(project_name, year_completed, locality, treatment_year) %>%
  summarise(n_tracts = n_distinct(GISJOIN_1950), .groups = "drop") %>% 
  arrange(-n_tracts) 

# Add tract count and split population estimates and units evenly across tracts
treated_tracts <-
  treated_tracts %>%
  left_join(tracts_per_project, by = c("project_name", "year_completed", "treatment_year")) %>%
  mutate(
    # Split population estimates
    proj_total_population_estimate = proj_total_population_estimate / n_tracts,
    proj_black_population_estimate = proj_black_population_estimate / n_tracts,
    proj_white_population_estimate = proj_white_population_estimate / n_tracts,
    # Split unit counts evenly across tracts
    total_public_housing_units = total_public_housing_units / n_tracts
  )

### Method 2: Alternative definition - Exact intersection (ARCHIVED)
# treated_tracts_exact <-
#   st_join(census_tract_sample, public_housing_data, join = st_intersects)
# 
# treated_tracts_exact <-
#   treated_tracts_exact %>% filter(!is.na(treatment_year))


### Create a panel tract dataset of public housing population per year ----
# 1. Get unique treated tracts and years, all of them
unique_treated_tracts_panel <- 
  treated_tracts %>%
  dplyr::select(GISJOIN_1950, YEAR) %>% 
  st_drop_geometry() %>% 
  distinct()
  

# 2. Get dataset for every tract per year, of:
#. Public housing population
#  
public_housing_pop_estimates <-
  treated_tracts %>% 
  filter(!is.na(treatment_year)) %>% 
  st_drop_geometry() %>% 
  group_by(GISJOIN_1950, YEAR, treatment_year) %>%
  summarise(total_public_housing_pop_estimate = sum(proj_total_population_estimate, na.rm = TRUE),
            black_public_housing_pop_estimate = sum(proj_black_population_estimate, na.rm = TRUE),
            white_public_housing_pop_estimate = sum(proj_white_population_estimate, na.rm = TRUE),
            total_public_housing_units = sum(total_public_housing_units, na.rm = TRUE),
              ) %>%
  mutate(total_public_housing_pop_estimate = case_when(YEAR >= treatment_year ~ total_public_housing_pop_estimate,
                                                    TRUE ~ NA_real_),
         black_public_housing_pop_estimate = case_when(YEAR >= treatment_year ~ black_public_housing_pop_estimate,
                                                    TRUE ~ NA_real_),
         white_public_housing_pop_estimate = case_when(YEAR >= treatment_year ~ white_public_housing_pop_estimate,
                                                    TRUE ~ NA_real_),
         total_public_housing_units = case_when(YEAR >= treatment_year ~ total_public_housing_units,
                                                    TRUE ~ NA_real_)) %>%
  dplyr::select(GISJOIN_1950, YEAR, treatment_year,
                total_public_housing_pop_estimate, black_public_housing_pop_estimate,
                white_public_housing_pop_estimate, total_public_housing_units) %>% 
  # collapse to each year: This is necessary because some tracts have multiple public housing projects which appear in differnet
  # treatment years
  group_by(GISJOIN_1950, YEAR) %>%
  summarise(total_public_housing_pop_estimate = sum(total_public_housing_pop_estimate, na.rm = TRUE),
            black_public_housing_pop_estimate = sum(black_public_housing_pop_estimate, na.rm = TRUE),
            white_public_housing_pop_estimate = sum(white_public_housing_pop_estimate, na.rm = TRUE),
            total_public_housing_units = sum(total_public_housing_units, na.rm = TRUE)) %>% 
  # if a tract is 0 every year, replace with NA
  group_by(GISJOIN_1950) %>%
  mutate(all_years_pop_estimate = sum(total_public_housing_pop_estimate),
         all_years_unit_estimate = sum(total_public_housing_units)) %>%
  # 5/2025: Weird data issue: G1701630SCC0002 has non-zero project population, but zero PH units at any point
  mutate(total_public_housing_pop_estimate = ifelse(all_years_pop_estimate == 0, NA, total_public_housing_pop_estimate),
         black_public_housing_pop_estimate = ifelse(all_years_pop_estimate == 0, NA, black_public_housing_pop_estimate),
         white_public_housing_pop_estimate = ifelse(all_years_pop_estimate == 0, NA, white_public_housing_pop_estimate)) %>% 
  dplyr::select(-all_years_pop_estimate, -all_years_unit_estimate) %>% 
  ungroup() 

### Filter which tracts we consider treated ----
# Consider a tract as treated if:
# 1. Number of units >= 30 (for buffer method, share of units doesn't make sense since units are split)
# aggregate public housing units constructed in each tract by year
treated_tracts_aggregated_by_year <- 
  treated_tracts %>%
  group_by(GISJOIN_1950, YEAR, treatment_year) %>%
  mutate(total_public_housing_units = sum(total_public_housing_units, na.rm = TRUE)) %>%
  # filter by minimum units threshold
  filter(total_public_housing_units >= public_housing_units_minimum)

# OLD METHOD (exact intersection with share filtering) - keeping for reference:
# share_of_units_treated <- 
#   treated_tracts_aggregated_by_year %>%
#   filter(YEAR == treatment_year) %>% 
#   mutate(share_of_housing_units_that_are_public_in_treatment_year =
#            total_public_housing_units /total_units) %>%
#   dplyr::select(GISJOIN_1950, treatment_year, share_of_housing_units_that_are_public_in_treatment_year) %>% 
#   st_drop_geometry() %>% 
#   distinct()
# 
# # filter by share of public housing units
# treated_tracts_aggregated_by_year <- 
#   treated_tracts_aggregated_by_year %>%
#   left_join(share_of_units_treated) %>%
#   filter(share_of_housing_units_that_are_public_in_treatment_year > 0.05)

#  keep only one observation per year
treated_tracts_aggregated_by_year <-
  treated_tracts_aggregated_by_year %>%
  arrange(GISJOIN_1950, treatment_year) %>%
  # for each treated tract and year, keep only the one with the smallest treatment year (eg keep first year of treatment)
  group_by(GISJOIN_1950, YEAR) %>%
  filter(row_number() == 1) %>%
  ungroup()

# keep a panel of treated tracts, only when they are treated
treated_tracts_panel <- 
  treated_tracts_aggregated_by_year %>%
  filter(YEAR >= treatment_year) %>%
  dplyr::select(GISJOIN_1950, YEAR, city, cbsa_title, treatment_year, total_public_housing_units) %>%
  mutate(treated = 1)
  
# Merge on treatment status of each tract to the Census data
census_tract_sample_with_treatment_status <- left_join(
  census_tract_sample, 
  treated_tracts_panel %>% dplyr::select(GISJOIN_1950, YEAR, treated) %>% st_drop_geometry()) %>%
  mutate(treated = ifelse(is.na(treated), 0, 1)) %>% 
  # merge on public housing population
  left_join(public_housing_pop_estimates, by = c("GISJOIN_1950", "YEAR")) %>% 
  # set public housing population and units equal to 0 if treated == 0
  mutate(total_public_housing_pop_estimate = ifelse(treated == 0, 0, total_public_housing_pop_estimate),
         black_public_housing_pop_estimate = ifelse(treated == 0, 0, black_public_housing_pop_estimate),
         white_public_housing_pop_estimate = ifelse(treated == 0, 0, white_public_housing_pop_estimate),
         total_public_housing_units = ifelse(treated == 0, 0, total_public_housing_units)) %>%
  # calculate private population
  mutate(private_population_estimate = total_pop - total_public_housing_pop_estimate,
         private_black_population_estimate = black_pop - black_public_housing_pop_estimate,
         private_white_population_estimate = white_pop - white_public_housing_pop_estimate) %>%
  # if private population is negative, set equal to 0 (these are cases where
  # estimated public housing population is greater than Census-reported total population)
  mutate(private_population_estimate = ifelse(private_population_estimate < 0, 0, private_population_estimate),
         private_black_population_estimate = ifelse(private_black_population_estimate < 0, 0, private_black_population_estimate),
         private_white_population_estimate = ifelse(private_white_population_estimate < 0, 0, private_white_population_estimate)) %>%
  # calculating private housing units 
  mutate(total_private_units_estimate = total_units - total_public_housing_units) %>% 
  # if private housing units is negative, set equal to 0
  mutate(total_private_units_estimate = ifelse(total_private_units_estimate < 0, 0, total_private_units_estimate)) %>% 
  # calculate private black share 
  mutate(private_black_share = private_black_population_estimate / private_population_estimate)


# calculate distance from projects
distances_to_nearest_project <-
  calculate_distance_to_project(census_tract_sample) %>% 
  select(GISJOIN_1950, distance_from_project) %>% 
  st_drop_geometry()

# merge on distance to nearest project
census_tract_sample_with_treatment_status <- 
  left_join(census_tract_sample_with_treatment_status, distances_to_nearest_project) %>%
  # Set distance to 0 for treated tracts (they are within the 50m buffer)
  mutate(distance_from_project = ifelse(treated == 1, 0, distance_from_project))

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

## Create interactive map -----
# Uncomment to create clickable map (can be slow with large datasets)
# interactive_map <- mapview(public_housing_data, 
#                           zcol = "total_public_housing_units",
#                           cex = "total_public_housing_units",
#                           layer.name = "Public Housing Projects") + 
#                   mapview(census_tract_sample %>% filter(YEAR == 1970), 
#                           alpha.regions = 0.2, 
#                           color = "white",
#                           layer.name = "Census Tracts 1970")
# interactive_map

# NYC-specific interactive map
# nyc_projects <- public_housing_data %>%
#   filter(str_detect(locality, "NEW YORK"))
# 
# nyc_tracts <- census_tract_sample %>%
#   filter(city == "New York City", YEAR == 1970)
# 
# nyc_map <- mapview(nyc_projects,
#                    zcol = "total_public_housing_units",
#                    cex = "total_public_housing_units",
#                    layer.name = "NYC Public Housing") +
#            mapview(nyc_tracts,
#                    alpha.regions = 0.2,
#                    color = "white",
#                    layer.name = "NYC Census Tracts 1970")
# nyc_map

