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
  distinct(GISJOIN_1950, treatment_year)

table(unique_treated_tracts$treatment_year)

# Create balanced dataset ----

census_tract_sample_with_treatment_status <-
  census_tract_sample_with_treatment_status_raw

# Checking:
# > View(tracts_in_original_not_balanced %>% filter(city == "Richmond", TRACTA  == "0411"))

## Balance across years -----
# Only keep tracts that are available from 1930-1990

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

# counties in 1950 
counties_1950 <- 
  census_tract_sample_with_treatment_status %>% 
  filter(YEAR == 1950) %>%
  pull(state_county) %>% 
  unique()

# tracts that exist in all years, 1930-1990 (need 1930 for pre-treatment of 1941-1950 projects)

years_range <- seq(1930,1990, 10)

# get distinct tracts for each year, 1930-1990
tracts_by_year <- 
  census_tract_sample_with_treatment_status %>% 
  filter(YEAR %in% years_range) %>% 
  dplyr::select(GISJOIN_1950, YEAR) %>% 
  st_drop_geometry() %>% 
  distinct()

# count number of tracts in each year
tracts_all_years <- 
  tracts_by_year %>% 
  group_by(GISJOIN_1950) %>% 
  summarise(num_years = n()) %>% 
  ungroup() %>% 
  filter(num_years == length(years_range)) %>% 
  mutate(exists_all_years = 1)

# 1940: 10K tracts
# 1930: 9K tracts

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

## Balance on availability of variables, 1930-1990 ----
# Variables to balance on 
# Core variables available in all years including 1930
balance_vars_core <- c("total_pop", "black_share",
                      "median_income",
                      "median_rent_calculated",
                      "lfp_rate", "unemp_rate")
# Additional variables only available from 1940+
balance_vars_1940_plus <- c("total_units")

tracts_with_complete_vars <- 
  census_tract_sample_with_treatment_status_balanced %>%
  st_drop_geometry() %>%
  group_by(GISJOIN_1950) %>%
  summarise(
    # Check completeness for core variables across all years (1930-1990)
    core_complete = all(
      !is.na(total_pop[YEAR %in% years_range]) & 
      !is.na(black_share[YEAR %in% years_range]) &
      !is.na(median_income[YEAR %in% years_range]) &
      !is.na(median_rent_calculated[YEAR %in% years_range]) &
    #  !is.na(median_home_value_calculated[YEAR %in% years_range]) &
      !is.na(lfp_rate[YEAR %in% years_range]) &
      !is.na(unemp_rate[YEAR %in% years_range])
    )) %>% 
    
  #   ,
  #   # Check total_units completeness for 1940+ (not available in 1930)
  #   units_complete = all(!is.na(total_units[YEAR >= 1940 & YEAR %in% years_range]))
  # ) %>%
  # Require both conditions
  filter(core_complete) %>%
  dplyr::select(GISJOIN_1950) %>%
  mutate(has_complete_data = 1)

# 1930: 8016
# 1940: 8098! 
# Where did all of those go?


# checking
tracts_missing_1930 <- census_tract_sample_with_treatment_status_balanced %>%
  st_drop_geometry() %>%
  filter(YEAR == 1930) %>%
  filter(
    is.na(total_pop) |
      is.na(black_share) |
      is.na(median_income) |
      is.na(median_rent_calculated) |
      is.na(median_home_value_calculated) |
      is.na(lfp_rate) |
      is.na(unemp_rate)
  ) %>%
  select(GISJOIN_1950, STATE, COUNTY, total_pop, black_share, median_income,
         median_rent_calculated, median_home_value_calculated, lfp_rate, unemp_rate)

nrow(tracts_missing_1930)

# Note 7/2025: Mostly places with 0 pop

census_tract_sample_with_treatment_status_balanced <- 
  census_tract_sample_with_treatment_status_balanced %>%
  left_join(tracts_with_complete_vars, by = c("GISJOIN_1950")) %>%
  mutate(has_complete_data = ifelse(is.na(has_complete_data), 0, has_complete_data)) %>%
  filter(has_complete_data == 1) %>%
  dplyr::select(-has_complete_data)


## Treatments that happen in 1940 or earlier (to ensure I have enough pre-trends) -----
# Tracts that are treated in or before 1940 (e.g. projects built 1931-1940)
# Exclude these to have 1930 and 1940 as pre-treatment periods
tracts_treated_1940_or_earlier <- 
  unique_treated_tracts %>% 
  mutate(treated_1940_or_earlier = treatment_year <= 1940) %>% 
  dplyr::select(GISJOIN_1950, treated_1940_or_earlier)

# merge with census tract data
census_tract_sample_with_treatment_status_balanced <- 
  census_tract_sample_with_treatment_status_balanced %>%
  left_join(tracts_treated_1940_or_earlier) %>% 
  # fill in missings
  mutate(treated_1940_or_earlier = ifelse(is.na(treated_1940_or_earlier), FALSE, treated_1940_or_earlier))

## Counties with too few tracts ----
# number of tracts in 1930
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
  mutate(num_tracts_geq_50 = ifelse(num_tracts >= 30, TRUE, FALSE))

## Identify urban renewal tracts -----
urban_renewal_tracts <-
  census_tract_sample_with_treatment_status %>%
  select(GISJOIN_1950, ur_binary_5pp, ur_binary_10pp) %>%
  st_drop_geometry() %>%
  distinct()

census_tract_sample_with_treatment_status_balanced <-
  census_tract_sample_with_treatment_status_balanced %>% 
  select(-contains("ur_binary")) %>% 
  left_join(urban_renewal_tracts) %>%
  mutate(ur_binary_5pp = ifelse(is.na(ur_binary_5pp), 0, ur_binary_5pp),
         ur_binary_10pp = ifelse(is.na(ur_binary_10pp), 0, ur_binary_10pp))

## OLD: Tracts with too few people in the pre-period -----
# Now: Just doing a quantile based trimming
# Keep tracts with at least 100 people in 1940: Avoid these tiny tracts
# that could mess things up

small_tracts_1940 <- 
  census_tract_sample_with_treatment_status_balanced %>%
  filter(YEAR == 1940) %>%
  filter(total_pop < 100) %>%
  st_drop_geometry() %>%
  mutate(small_tract_1940 = TRUE) %>% 
  dplyr::select(GISJOIN_1950, small_tract_1940)

census_tract_sample_with_treatment_status_balanced <-
  census_tract_sample_with_treatment_status_balanced %>% 
  left_join(small_tracts_1940) %>% 
  mutate(small_tract_1940 = ifelse(is.na(small_tract_1940), FALSE, small_tract_1940))



# quantile(census_tract_sample_with_treatment_status_balanced$total_pop, probs = c(0.01,0.05, 0.1, 0.25, 0.75, 0.9, 0.99))

## Windorize bottom and top 2% of tract population -----
# Identify the bottom and top 2% of tract population in any year
# 07/2025: This is 67 and 14,308 people
pop_quantiles <- census_tract_sample_with_treatment_status_balanced %>%
  st_drop_geometry() %>%
  summarise(
    q02 = quantile(total_pop, 0.02, na.rm = TRUE),
    q05 = quantile(total_pop, 0.05, na.rm = TRUE),
    q10 = quantile(total_pop, 0.10, na.rm = TRUE),
    q95 = quantile(total_pop, 0.95, na.rm = TRUE),
    q98 = quantile(total_pop, 0.98, na.rm = TRUE)
  )


# List of tract IDs with population outside the bounds
tracts_to_remove <- 
  census_tract_sample_with_treatment_status_balanced %>%
  st_drop_geometry() %>%
  filter(total_pop < pop_quantiles$q02 | total_pop > pop_quantiles$q98) %>%
  pull(GISJOIN_1950) %>% 
  unique()


## Apply filters ----
balanced_sample <- 
  census_tract_sample_with_treatment_status_balanced %>% 
  # number of tracts in 1940 greater than 50
  filter(num_tracts_geq_50 == TRUE) %>% 
  # Exclude tracts treated 1940 or earlier (to preserve 1930 and 1940 as pre-treatment)
  filter(treated_1940_or_earlier == FALSE) %>% 
  # NOTE: Removed urban renewal filter - now controlling for it instead of excluding
  # filter(ur_binary_5pp == 0) %>% 
  # filter out tracts that are outside the population bounds 
  filter(!GISJOIN_1950 %in% tracts_to_remove) %>% 
  # keep only 1940 onward to avoid changes in the sample across cities and variables
  filter(YEAR >= 1930)

## filter out cities with too high a share of treated tracts ----
share_of_treated_tracts <- 
  balanced_sample %>% 
  # use 1990 tracts (end of period)
  filter(YEAR == 1990) %>% 
  st_drop_geometry() %>% 
  group_by(cbsa_title) %>% 
  summarise(share_treated = sum(treated) / n()) %>% 
  ungroup() %>% 
  arrange(-share_treated)

# merge with balanced sample, filter out cities with too high a share of treated tracts 
# NOTE (11/6/24): Now that I am using CBSAs, there are no cities above 20% treated
balanced_sample <- 
  balanced_sample %>% 
  left_join(share_of_treated_tracts) %>% 
  # drop if more than 30% of tracts are treated
  filter(share_treated <= .3)

## Create new treated tracts panel ----
# filter out tracts that are not in balanced sample
treated_tracts_panel_balanced <- 
  treated_tracts_panel %>% 
    semi_join(balanced_sample %>% st_drop_geometry(), by = c("GISJOIN_1950"))

## analysis ----
# unique treated tracts original vs balanced
unique_treated_tracts_balanced <- 
  treated_tracts_panel_balanced %>% 
  distinct(GISJOIN_1950, treatment_year)

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

# Get the particular projects that are included in my sample -----
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


# Numbers (Updated 7/24/2025)
# 294316
sum(public_housing_in_sample$total_public_housing_units, na.rm = TRUE)

# Number of projects: 690 (7/30)
nrow(public_housing_in_sample %>% filter(!is.na(total_public_housing_units)))

# projects per city
projects_per_city <- 
  public_housing_in_sample %>%
  pull(locality) %>% 
  table()

# number of treated tracts: 629 
nrow(treated_tracts_with_projects %>% select(GISJOIN_1950) %>% distinct())



# CHECKING BOSTON
  library(sf)
library(ggplot2)
library(dplyr)

# Get Boston CBSA data for mapping
boston_tracts <- census_tract_sample_with_treatment_status_balanced %>%
  filter(YEAR == 1990, str_detect(cbsa_title, "Chicago"))

# Create the map
boston_map <- ggplot(boston_tracts) +
  geom_sf(aes(fill = factor(treated)), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("0" = "lightgray", "1" = "red"),
    labels = c("0" = "Control", "1" = "Treated"),
    name = "Treatment Status"
  ) +
  labs(
    title = "Public Housing Treatment Status - Boston-Cambridge-Quincy CBSA",
    subtitle = paste0("Total tracts: ", nrow(boston_tracts),
                      " | Treated: ", sum(boston_tracts$treated),
                      " (", round(mean(boston_tracts$treated)*100, 1), "%)")
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom"
  )

print(boston_map)

# County-level breakdown
boston_by_county <- boston_tracts %>%
  st_drop_geometry() %>%
  group_by(COUNTY) %>%
  summarise(
    total_tracts = n(),
    treated_tracts = sum(treated),
    pct_treated = round(mean(treated) * 100, 1)
  ) %>%
  arrange(-pct_treated)

print(boston_by_county)
# 
# # Treatment year breakdown
# treatment_years <- boston_tracts %>%
#   filter(treated == 1) %>%
#   st_drop_geometry() %>%
#   count(treatment_year) %>%
#   arrange(treatment_year)
# 
# print(treatment_years)


# Output files ---- 

# census sample
st_write(balanced_sample, balanced_sample_filepath,
         append = FALSE, layer = "tracts", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

# treated tracts sample
st_write(treated_tracts_panel_balanced, balanced_treated_tracts_panel_filepath,
         append = FALSE, layer = "tracts", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

# public housing projects that are in the sample
st_write(public_housing_in_sample, public_housing_sample_filepath,
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

# # Create 1950-1990 balanced sample -----
# # As a robustness check, I can use this dataset for site selection analysis
# 
# # This isn't working, troubleshoot
# years_range_1950 <- seq(1950, 1990, 10)
# 
# # Identify tracts that appear in all years 1950–1990
# tracts_by_year_1950 <- 
#   census_tract_sample_with_treatment_status %>%
#   filter(YEAR %in% years_range_1950) %>%
#   select(GISJOIN_1950, YEAR) %>%
#   st_drop_geometry() %>%
#   distinct()
# 
# tracts_all_years_1950 <- 
#   tracts_by_year_1950 %>%
#   group_by(GISJOIN_1950) %>%
#   summarise(num_years = n()) %>%
#   ungroup() %>%
#   filter(num_years == length(years_range_1950)) %>%
#   mutate(exists_all_years_1950 = 1)
# 
# # Filter census tracts to those that exist all years 1950–1990
# census_tract_sample_balanced_1950 <- 
#   census_tract_sample_with_treatment_status %>%
#   select(-num_years) %>% 
#   left_join(tracts_all_years_1950) %>%
#   filter(!is.na(exists_all_years_1950)) %>%
#   select(-exists_all_years_1950)
# 
# # Check for complete variable coverage
# tracts_with_complete_vars_1950 <- 
#   census_tract_sample_balanced_1950 %>%
#   filter(YEAR %in% years_range_1950)%>%
#   st_drop_geometry() %>%
#   group_by(GISJOIN_1950) %>%
#   summarise(across(all_of(balance_vars_core), ~ sum(!is.na(.)) == length(years_range_1950))) %>%
#   mutate(has_complete_data_1950 = if_all(all_of(balance_vars_core), ~ . == TRUE)) %>%
#   filter(has_complete_data_1950) %>%
#   select(GISJOIN_1950) %>% 
#   mutate(has_complete_data_1950 = 1)
# 
# census_tract_sample_balanced_1950 <- 
#   census_tract_sample_balanced_1950 %>%
#   left_join(tracts_with_complete_vars_1950, by = c("GISJOIN_1950")) %>%
#   filter(!is.na(has_complete_data_1950)) %>%
#   select(-has_complete_data_1950)
# 
# # small tracts in 1950
# small_tracts_1950 <- 
#   census_tract_sample_balanced_1950 %>%
#   filter(YEAR == 1950) %>%
#   filter(total_pop < 100) %>%
#   st_drop_geometry() %>%
#   mutate(small_tract_1950 = TRUE) %>% 
#   dplyr::select(GISJOIN_1950, small_tract_1950)
# 
# census_tract_sample_balanced_1950 <-
#   census_tract_sample_balanced_1950 %>% 
#   left_join(small_tracts_1950) %>% 
#   mutate(small_tract_1950 = ifelse(is.na(small_tract_1950), FALSE, small_tract_1950))
# 
# 
# 
# # Reapply filters used in 1940–1990 version
# balanced_sample_1950 <- 
#   census_tract_sample_balanced_1950 %>%
#   # exclude early treated
#   left_join(tracts_treated_1940_or_earlier) %>%
#   mutate(treated_1940_or_earlier = ifelse(is.na(treated_1940_or_earlier), FALSE, treated_1940_or_earlier)) %>%
#   filter(treated_1940_or_earlier == FALSE) %>%
#   # exclude urban renewal tracts
#   select(-contains('ur_binary')) %>% 
#   left_join(urban_renewal_tracts) %>%
#   mutate(ur_binary_5pp = ifelse(is.na(ur_binary_5pp), 0, ur_binary_5pp),
#          ur_binary_10pp = ifelse(is.na(ur_binary_10pp), 0, ur_binary_10pp)) %>%
#   filter(ur_binary_5pp == 0) %>%
#   # exclude small tracts in 1950
#   filter(small_tract_1950 == FALSE)
#   
#   
# 
# # Rejoin city information and filter cities with too few tracts
# num_tracts_1950 <- 
#   balanced_sample_1950 %>%
#   filter(YEAR == 1950) %>%
#   st_drop_geometry() %>%
#   group_by(cbsa_title) %>%
#   summarise(num_tracts = n()) %>%
#   ungroup()
# 
# balanced_sample_1950 <- 
#   balanced_sample_1950 %>%
#   left_join(num_tracts_1950) %>%
#   filter(num_tracts >= 50)
# 
# 
# # Create treated tracts panel for 1950–1990 balanced sample
# treated_tracts_panel_balanced_1950 <- 
#   treated_tracts_panel %>%
#   semi_join(balanced_sample_1950 %>% st_drop_geometry(), by = c("GISJOIN_1950"))
# 
# 
# 
# # Save outputs
# 
# balanced_sample_filepath_1950 <- here(merged_data_dir, dataset_choice,
#                                         "census_tract_sample_with_treatment_status_balanced_1950.gpkg")
# 
# balanced_treated_panel_filepath_1950 <- here(merged_data_dir, dataset_choice,
#                                                "treated_tracts_panel_balanced_1950.gpkg")
# 
# 
# st_write(balanced_sample_1950, balanced_sample_filepath_1950,
#          append = FALSE, layer = "tracts", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)
# 
# st_write(treated_tracts_panel_balanced_1950, balanced_treated_panel_filepath_1950,
#          append = FALSE, layer = "tracts", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)\