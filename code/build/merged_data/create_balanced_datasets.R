# Create my balanced sample for analysis ----

# Here, I will apply several filters to ensure I have a good set of datasets for analysis
# 1. Dropping cities that have too few tracts
# 2. Dropping "always treated" tracts: Tracts which are treated in 1940 and only show up in the data in 1940
# I will put an indicator for these tracts in the data

# maybe for all of these, I can put indicators

# Preliminaries ----
library(sf)
library(tidyverse)
library(here)

# !! Enter dataset choice. Can be "digitized", "cdd_large" or "cdd_small", or "combined" 
dataset_choice <- "combined"

# directories
merged_data_dir <- here("data", "derived", "merged")
ph_data_dir <- here("data", "derived", "public_housing", "working")

# input filepaths 
tract_with_treatment_status_filepath <- here(merged_data_dir, dataset_choice,
                                             "census_tract_sample_with_treatment_status_all.gpkg")
treated_tracts_panel_filepath <- here(merged_data_dir, dataset_choice,
                                      "treated_tracts_panel_all.gpkg")

cleaned_projects_filepath <- here(ph_data_dir, "cleaned_housing_projects.gpkg")

# balanced sample filepaths
balanced_sample_filepath <- 
  here(merged_data_dir, dataset_choice, "census_tract_sample_with_treatment_status_balanced.gpkg")
balanced_treated_tracts_panel_filepath <- 
  here(merged_data_dir, dataset_choice, "treated_tracts_panel_balanced.gpkg")
public_housing_sample_filepath <-
  here(ph_data_dir, dataset_choice, "public_housing_sample_balanced.gpkg")

# read in data ----
# census tracts 
census_tract_sample_with_treatment_status_raw <- 
  st_read(tract_with_treatment_status_filepath)

# treated tracts panel
treated_tracts_panel <- 
  st_read(treated_tracts_panel_filepath)

# public housing data
public_housing_data <- 
  st_read(cleaned_projects_filepath)

# unique treated tracts and years
unique_treated_tracts <- 
  treated_tracts_panel %>% 
  distinct(COUNTY, STATE, TRACTA, treatment_year)

table(unique_treated_tracts$treatment_year)

# Create balanced dataset ----

census_tract_sample_with_treatment_status <-
  census_tract_sample_with_treatment_status_raw

# Checking:
# > View(tracts_in_original_not_balanced %>% filter(city == "Richmond", TRACTA  == "0411"))

## Create balanced sample of tracts -----
# Only keep tracts that are available from 1940-1990

# counties in 1930
counties_1930 <-
  census_tract_sample_with_treatment_status %>% 
  filter(YEAR == 1930) %>%
  pull(state_county) %>% 
  unique()

# counties in 1940 
counties_1940 <-
  census_tract_sample_with_treatment_status %>% 
  filter(YEAR == 1940) %>%
  pull(state_county) %>% 
  unique()

# tracts that exist in all years, 1940-1990

years_range <- seq(1940,1990, 10)

# get distinct tracts for each year, 1940-1990
tracts_by_year <- 
  census_tract_sample_with_treatment_status %>% 
  filter(YEAR %in% years_range) %>% 
  dplyr::select(STATE, COUNTY, TRACTA, YEAR) %>% 
  st_drop_geometry() %>% 
  distinct()

# count number of tracts in each year
tracts_all_years <- 
  tracts_by_year %>% 
  group_by(STATE, COUNTY, TRACTA) %>% 
  summarise(num_years = n()) %>% 
  ungroup() %>% 
  filter(num_years == length(years_range)) %>% 
  mutate(exists_all_years = 1)

# create variable for whether tract exists in all years
census_tract_sample_with_treatment_status <- 
  census_tract_sample_with_treatment_status %>% 
  left_join(tracts_all_years) %>%
  mutate(exists_all_years = ifelse(is.na(exists_all_years), 0, exists_all_years))
  
# balanced sample: keep only tracts that exist in all years
census_tract_sample_with_treatment_status_balanced <- 
  census_tract_sample_with_treatment_status %>% 
  filter(exists_all_years == 1) %>% 
  dplyr::select(-exists_all_years)

## Treatments that happen before 1941 (to ensure I have enough pre-trends) -----
# Tracts that are treated before treatment_year == 1960 (e.g. projects built 1951-1973)
tracts_treated_before_1960 <- 
  unique_treated_tracts %>% 
  mutate(treated_before_1960 = treatment_year < 1960) %>% 
  dplyr::select(COUNTY, STATE, TRACTA, treated_before_1960)

# merge with census tract data
census_tract_sample_with_treatment_status_balanced <- 
  census_tract_sample_with_treatment_status_balanced %>%
  left_join(tracts_treated_before_1960) %>% 
  # fill in missings
  mutate(treated_before_1960 = ifelse(is.na(treated_before_1960), FALSE, treated_before_1960))

## Counties with too few tracts ----
# number of tracts in 1940
num_of_tracts_1930 <-
  census_tract_sample_with_treatment_status_balanced %>%
  filter(YEAR == 1930) %>% 
  st_drop_geometry() %>% 
  group_by(cbsa_title) %>%
  # calculate total number of tracts and total number of treated tracts
  summarise(num_tracts = n()) %>% 
  ungroup() 

num_of_tracts_1940 <-
  census_tract_sample_with_treatment_status_balanced %>%
  filter(YEAR == 1940) %>% 
  st_drop_geometry() %>% 
  group_by(cbsa_title) %>%
  # calculate total number of tracts and total number of treated tracts
  summarise(num_tracts = n()) %>% 
  ungroup() 


# merge with census tract data
census_tract_sample_with_treatment_status_balanced <- 
  census_tract_sample_with_treatment_status_balanced %>%
  left_join(num_of_tracts_1940) %>% 
  mutate(num_tracts_geq_50 = ifelse(num_tracts >= 50, TRUE, FALSE))

## Identify urban renewal tracts -----
urban_renewal_tracts <-
  census_tract_sample_with_treatment_status %>% 
  select(STATE, COUNTY, TRACTA, ur_binary_5pp, ur_binary_10pp) %>% 
  st_drop_geometry() %>% 
  distinct()

census_tract_sample_with_treatment_status_balanced <-
  census_tract_sample_with_treatment_status_balanced %>% 
  left_join(urban_renewal_tracts)

## Apply filters ----
balanced_sample <- 
  census_tract_sample_with_treatment_status_balanced %>% 
  # number of tracts in 1940 greater than 50
  filter(num_tracts_geq_50 == TRUE)  %>% 
  # Exclude previously treated tracts
  filter(treated_before_1960 == FALSE) %>% 
  filter(ur_binary_5pp == 0) %>% 
  # keep only 1940 onward to avoid changes in the sample across cities and variables
  filter(YEAR >= 1940)

## filter out cities with too high a share of treated tracts ----
share_of_treated_tracts <- 
  balanced_sample %>% 
  # use 1990 tracts (end of period)
  filter(YEAR == 1990) %>% 
  st_drop_geometry() %>% 
  group_by(cbsa_title) %>% 
  summarise(share_treated = sum(treated) / n()) %>% 
  ungroup()

# merge with balanced sample, filter out cities with too high a share of treated tracts 
# NOTE (11/6): Now that I am using CBSAs, there are no cities above 20% treated
balanced_sample <- 
  balanced_sample %>% 
  left_join(share_of_treated_tracts) %>% 
  # drop if more than 20% of tracts are treated
  filter(share_treated < .20)

## Create new treated tracts panel ----
# filter out tracts that are not in balanced sample
treated_tracts_panel_balanced <- 
  treated_tracts_panel %>% 
    semi_join(balanced_sample %>% st_drop_geometry(), by = c("COUNTY", "STATE", "TRACTA"))

## analysis ----
# unique treated tracts original vs balanced
unique_treated_tracts_balanced <- 
  treated_tracts_panel_balanced %>% 
  distinct(COUNTY, STATE, TRACTA, treatment_year)

table(unique_treated_tracts_balanced$treatment_year)
table(unique_treated_tracts$treatment_year)


# cities in balanced sample: 44
cities_in_balanced_sample <- 
  balanced_sample %>% 
  filter(YEAR == 1990) %>% 
  st_drop_geometry() %>% 
  pull(cbsa_title) %>% 
  unique()

length(cities_in_balanced_sample)

# cities in original sample: 62
cities_in_original_sample <- 
  census_tract_sample_with_treatment_status %>% 
  filter(YEAR == 1990) %>% 
  st_drop_geometry() %>% 
  pull(cbsa_title) %>% 
  unique()

length(cities_in_original_sample)

# cities that are in original but not in balanced
cities_not_in_balanced <- 
  setdiff(cities_in_original_sample, cities_in_balanced_sample)

# Get the particular projects that are included in my smaple -----
# 1. Ensure public housing data is in sf format
public_housing_data <-
  public_housing_data %>%
  st_transform(st_crs(balanced_sample)) %>% 
  mutate(project_id = row_number())

# 2. Spatial join between treated tracts and public housing projects
treated_tracts_with_projects <- 
  treated_tracts_panel_balanced %>%
  filter(YEAR == treatment_year) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(balanced_sample)) %>%
  st_join(public_housing_data %>% dplyr::select(project_id, project_name, project_code,
                                                year_completed, treatment_year) %>% 
            dplyr::rename(project_treatment_year = treatment_year))

# 3. Clean up the result
treated_tracts_with_projects <-
  treated_tracts_with_projects %>%
  filter(!is.na(project_code)) %>%  # Remove any rows where no project was matched
  # remove projects where treatment_year of project is not the same as treatment_year of tract
  filter(treatment_year == project_treatment_year) 

# Public housing projects that are ultimately in the sample
public_housing_in_sample <- 
  public_housing_data %>%
  filter(project_code %in% treated_tracts_with_projects$project_code)

# 1940, excluding UR trats: 211,500 (1940)
sum(public_housing_in_sample$total_public_housing_units, na.rm = TRUE)
# Number of projects: 532
nrow(public_housing_in_sample %>% filter(!is.na(total_public_housing_units)))

# number of treated tracts


# Output files ---- 
# output files

# census sample
st_write(balanced_sample, balanced_sample_filepath,
         append = FALSE, layer = "tracts", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

# treated tracts sample
st_write(treated_tracts_panel_balanced, balanced_treated_tracts_panel_filepath,
         append = FALSE, layer = "tracts", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

# public housing projects that are in the sample
st_write(public_housing_in_sample, public_housing_sample_filepath,
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)
