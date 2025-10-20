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
public_housing_sample_first_year_filepath <-
  here(ph_data_dir, dataset_choice, "public_housing_sample_balanced_first_year.gpkg")

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
# Only keep tracts that are available from 1930-2000

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

# tracts that exist in all years, 1930-2000 (need 1930 for pre-treatment of 1941-1950 projects)

years_range <- seq(1930,2000, 10)

# get distinct tracts for each year, 1930-2000
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

## Balance on availability of variables, 1930-2000 ----
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
    # Check completeness for core variables across all years (1930-2000)
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

# small_tracts_1940 <- 
#   census_tract_sample_with_treatment_status_balanced %>%
#   filter(YEAR == 1940) %>%
#   filter(total_pop < 100) %>%
#   st_drop_geometry() %>%
#   mutate(small_tract_1940 = TRUE) %>% 
#   dplyr::select(GISJOIN_1950, small_tract_1940)
# 
# census_tract_sample_with_treatment_status_balanced <-
#   census_tract_sample_with_treatment_status_balanced %>% 
#   left_join(small_tracts_1940) %>% 
#   mutate(small_tract_1940 = ifelse(is.na(small_tract_1940), FALSE, small_tract_1940))



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
    q15 = quantile(total_pop, 0.15, na.rm = TRUE),
    q20 = quantile(total_pop, 0.20, na.rm = TRUE),
    q95 = quantile(total_pop, 0.95, na.rm = TRUE),
    q98 = quantile(total_pop, 0.98, na.rm = TRUE)
  )


# List of tract IDs with population outside the bounds
tracts_to_remove <- 
  census_tract_sample_with_treatment_status_balanced %>%
  st_drop_geometry() %>%
  filter(total_pop < pop_quantiles$q05 | total_pop > pop_quantiles$q98) %>%
  #filter(total_pop < pop_quantiles$q02 | total_pop > pop_quantiles$q98) %>%
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
  # use 2000 tracts (end of period)
  filter(YEAR == 2000) %>% 
  st_drop_geometry() %>% 
  group_by(cbsa_title) %>% 
  summarise(share_treated = sum(treated) / n()) %>% 
  ungroup() %>% 
  arrange(-share_treated)

# merge with balanced sample, filter out cities with too high a share of treated tracts 
# balanced_sample <- 
#   balanced_sample %>% 
#   left_join(share_of_treated_tracts) %>% 
# #   # drop if more than 30% of tracts are treated
#   filter(share_treated <= .3)

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


# cities in balanced sample: 45
cities_in_balanced_sample <- 
  balanced_sample %>% 
  filter(YEAR == 2000) %>% 
  st_drop_geometry() %>% 
  pull(cbsa_title) %>% 
  unique()

length(cities_in_balanced_sample)

# cities in original sample: 52
cities_in_original_sample <- 
  census_tract_sample_with_treatment_status %>% 
  filter(YEAR == 2000) %>% 
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
# This identifies "first-wave" projects used in the main DiD analysis
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

# 4. Public housing projects used in main analysis (first-wave only)
# These are projects whose treatment timing matches the tract's assigned treatment year
public_housing_in_analysis <-
  public_housing_data %>%
  filter(project_code %in% treated_tracts_with_projects$project_code)

# 5. ALL public housing projects in balanced sample tracts (for reporting/documentation)
# This includes "second-wave" projects built later in already-treated tracts
balanced_sample_tracts_geom <- balanced_sample %>%
  filter(YEAR == 2000) %>%
  select(GISJOIN_1950, geom)

projects_with_tracts <- st_join(
  public_housing_data,
  balanced_sample_tracts_geom,
  join = st_intersects
)

public_housing_in_sample <-
  projects_with_tracts %>%
  filter(!is.na(GISJOIN_1950)) %>%
  # Keep only the project columns (drop the joined tract ID)
  select(all_of(names(public_housing_data)))

# Final counts -----
# Numbers (Updated 10/13/2025)

# unique tracts
nrow(balanced_sample %>% filter(YEAR == 2000) %>% select(GISJOIN_1950) %>% distinct())

# unique CBSAs
n_distinct(balanced_sample$cbsa_title)

# Projects used in main DiD analysis (first-year treatment only)
cat("Projects in analysis (first-year):",
    nrow(public_housing_in_analysis %>% filter(!is.na(total_public_housing_units))), "\n")
cat("Units in analysis (first-year):",
    sum(public_housing_in_analysis$total_public_housing_units, na.rm = TRUE), "\n")

# ALL projects in balanced sample tracts (for documentation)
cat("Projects in sample (all):",
    nrow(public_housing_in_sample %>% filter(!is.na(total_public_housing_units))), "\n")
cat("Units in sample (all):",
    sum(public_housing_in_sample$total_public_housing_units, na.rm = TRUE), "\n")

# projects per city (all projects in sample)
projects_per_city <-
  public_housing_in_sample %>%
  pull(locality) %>%
  table()

# number of treated tracts
nrow(treated_tracts_panel_balanced %>% select(GISJOIN_1950) %>% distinct())



# CHECKING BOSTON MAP -----
library(sf)
library(ggplot2)
library(dplyr)

# Get Boston CBSA data for mapping
boston_tracts <- census_tract_sample_with_treatment_status_balanced %>%
  filter(YEAR == 2000, str_detect(cbsa_title, "Chicago"))

# Create the map
boston_map <- ggplot(boston_tracts) +
  geom_sf(aes(fill = factor(treated)), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("0" = "lightgray", "1" = "red"),
    labels = c("0" = "Control", "1" = "Treated"),
    name = "Treatment Status"
  ) +
  labs(
    title = "Public Housing Treatment Status - X CBSA",
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

# identify northmost chicago project
public_housing_data %>%
  filter(str_detect(locality, "CHICAGO")) %>%
  mutate(lat = as.numeric(st_coordinates(geom)[,2])) %>% 
  st_drop_geometry() %>%
  arrange(-lat) %>%
  slice(1)

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

# public housing projects in sample (ALL projects in balanced tracts, for documentation)
st_write(public_housing_in_sample, public_housing_sample_filepath,
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

# public housing projects used in analysis (first-year treatment only, for main DiD)
st_write(public_housing_in_analysis, public_housing_sample_first_year_filepath,
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)


# Create Descriptive table of cities, number of tracts in each city lost at each step ----
# Helper: function to summarize tracts by CBSA
summarize_by_cbsa <- function(df, step_name) {
  df %>%
    filter(YEAR == 2000) %>%    # fix to one year for consistency
    st_drop_geometry() %>%
    group_by(cbsa_title) %>%
    summarise(num_tracts = n(), .groups = "drop") %>%
    mutate(step = step_name)
}

# Step 0: original sample
summary_original <- summarize_by_cbsa(census_tract_sample_with_treatment_status, "Original")

# Step 1: balanced on years
summary_balanced_years <- summarize_by_cbsa(census_tract_sample_with_treatment_status %>% 
                                              filter(exists_all_years == 1), 
                                            "Balanced years")

# Step 2: balanced on complete vars
summary_balanced_vars <- summarize_by_cbsa(census_tract_sample_with_treatment_status_balanced, 
                                           "Balanced vars")

# Step 3: exclude treated 1940 or earlier
summary_exclude_early <- summarize_by_cbsa(census_tract_sample_with_treatment_status_balanced %>%
                                             filter(treated_1940_or_earlier == FALSE),
                                           "Exclude early treatments")

# Step 4: drop CBSAs with <30 tracts
summary_large_cities <- summarize_by_cbsa(census_tract_sample_with_treatment_status_balanced %>%
                                            filter(num_tracts_geq_50 == TRUE),
                                          "Drop small CBSAs")

# Step 5: drop tiny tracts (<100 pop 1940) + windsorize pops
summary_sizepop <- summarize_by_cbsa(balanced_sample, "Final balanced")

# Combine
descriptive_table <- bind_rows(summary_original,
                               summary_balanced_years,
                               summary_balanced_vars,
                               summary_exclude_early,
                               summary_large_cities,
                               summary_sizepop)

# Reshape to wide format
descriptive_table_wide <- descriptive_table %>%
  pivot_wider(names_from = step, values_from = num_tracts)

# Add column for tracts lost at each step
descriptive_table_wide <- descriptive_table_wide %>%
  arrange(cbsa_title) %>%
  mutate(across(-cbsa_title, ~replace_na(.x, 0))) %>%
  mutate(across(-cbsa_title, as.integer))

# View table
descriptive_table_wide


lost_final_step <- (census_tract_sample_with_treatment_status_balanced %>%
                      filter(num_tracts_geq_50 == TRUE)) %>%
  st_drop_geometry() %>%
  filter(YEAR == 2000) %>%
  anti_join(balanced_sample %>% st_drop_geometry() %>% filter(YEAR == 2000),
            by = "GISJOIN_1950")

table(lost_final_step$cbsa_title)

# Create sample attrition table ----
library(tinytable)

# Helper function to count treated tracts at each step
count_treated_tracts <- function(df, treated_panel) {
  tract_ids <- df %>%
    filter(YEAR == 2000) %>%
    st_drop_geometry() %>%
    pull(GISJOIN_1950) %>%
    unique()

  treated_panel %>%
    filter(GISJOIN_1950 %in% tract_ids) %>%
    pull(GISJOIN_1950) %>%
    unique() %>%
    length()
}

# Helper function to count projects and units
count_projects_units <- function(df, treated_panel, ph_data) {
  # Get treated tracts in this sample
  tract_ids <- df %>%
    filter(YEAR == 2000) %>%
    st_drop_geometry() %>%
    pull(GISJOIN_1950) %>%
    unique()

  # Filter treated panel
  panel_subset <- treated_panel %>%
    filter(GISJOIN_1950 %in% tract_ids) %>%
    filter(YEAR == treatment_year)

  # Spatial join with projects
  if(nrow(panel_subset) == 0) {
    return(list(n_projects = 0, n_units = 0))
  }

  panel_sf <- panel_subset %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ph_data))

  joined <- panel_sf %>%
    st_join(ph_data %>% select(project_code, total_public_housing_units)) %>%
    filter(!is.na(project_code))

  projects <- ph_data %>%
    filter(project_code %in% joined$project_code)

  list(
    n_projects = nrow(projects %>% filter(!is.na(total_public_housing_units))),
    n_units = sum(projects$total_public_housing_units, na.rm = TRUE)
  )
}

# Step 0: Original sample
step0 <- census_tract_sample_with_treatment_status_raw %>% filter(YEAR == 2000) %>% st_drop_geometry()
projects_units_0 <- count_projects_units(census_tract_sample_with_treatment_status_raw,
                                         treated_tracts_panel, public_housing_data)

# Step 1: Balanced on years (1930-2000)
step1_data <- census_tract_sample_with_treatment_status %>%
  filter(exists_all_years == 1)
step1 <- step1_data %>% filter(YEAR == 2000) %>% st_drop_geometry()
projects_units_1 <- count_projects_units(step1_data, treated_tracts_panel, public_housing_data)

# Step 2: Complete variables
step2_data <- census_tract_sample_with_treatment_status_balanced
step2 <- step2_data %>% filter(YEAR == 2000) %>% st_drop_geometry()
projects_units_2 <- count_projects_units(step2_data, treated_tracts_panel, public_housing_data)

# Step 3: Exclude treatments ≤1940
step3_data <- census_tract_sample_with_treatment_status_balanced %>%
  filter(treated_1940_or_earlier == FALSE)
step3 <- step3_data %>% filter(YEAR == 2000) %>% st_drop_geometry()
projects_units_3 <- count_projects_units(step3_data, treated_tracts_panel, public_housing_data)

# Step 4: Drop small CBSAs (<30 tracts)
step4_data <- census_tract_sample_with_treatment_status_balanced %>%
  filter(num_tracts_geq_50 == TRUE) %>%
  filter(treated_1940_or_earlier == FALSE)
step4 <- step4_data %>% filter(YEAR == 2000) %>% st_drop_geometry()
projects_units_4 <- count_projects_units(step4_data, treated_tracts_panel, public_housing_data)

# Step 5: Final balanced (population filters)
step5 <- balanced_sample %>% filter(YEAR == 2000) %>% st_drop_geometry()
projects_units_5 <- count_projects_units(balanced_sample, treated_tracts_panel, public_housing_data)

# Build attrition table
attrition_table <- tibble(
  Step = c(
    "Original sample",
    "Balanced on years (1930-2000)",
    "Complete variables",
    "Exclude treatments $\\leq$1940",
    "Drop CBSAs $<$30 tracts",
    "Drop population outliers"
  ),
  Tracts = c(
    n_distinct(step0$GISJOIN_1950),
    n_distinct(step1$GISJOIN_1950),
    n_distinct(step2$GISJOIN_1950),
    n_distinct(step3$GISJOIN_1950),
    n_distinct(step4$GISJOIN_1950),
    n_distinct(step5$GISJOIN_1950)
  ),
  `Pop. 1940 (M)` = c(
    sum(census_tract_sample_with_treatment_status_raw %>% filter(YEAR == 1940) %>%
          st_drop_geometry() %>% pull(total_pop), na.rm = TRUE) / 1e6,
    sum(step1_data %>% filter(YEAR == 1940) %>% st_drop_geometry() %>% pull(total_pop), na.rm = TRUE) / 1e6,
    sum(step2_data %>% filter(YEAR == 1940) %>% st_drop_geometry() %>% pull(total_pop), na.rm = TRUE) / 1e6,
    sum(step3_data %>% filter(YEAR == 1940) %>% st_drop_geometry() %>% pull(total_pop), na.rm = TRUE) / 1e6,
    sum(step4_data %>% filter(YEAR == 1940) %>% st_drop_geometry() %>% pull(total_pop), na.rm = TRUE) / 1e6,
    sum(balanced_sample %>% filter(YEAR == 1940) %>% st_drop_geometry() %>% pull(total_pop), na.rm = TRUE) / 1e6
  ),
  CBSAs = c(
    n_distinct(step0$cbsa_title),
    n_distinct(step1$cbsa_title),
    n_distinct(step2$cbsa_title),
    n_distinct(step3$cbsa_title),
    n_distinct(step4$cbsa_title),
    n_distinct(step5$cbsa_title)
  ),
  `Treated Tracts` = c(
    count_treated_tracts(census_tract_sample_with_treatment_status_raw, treated_tracts_panel),
    count_treated_tracts(step1_data, treated_tracts_panel),
    count_treated_tracts(step2_data, treated_tracts_panel),
    count_treated_tracts(step3_data, treated_tracts_panel),
    count_treated_tracts(step4_data, treated_tracts_panel),
    count_treated_tracts(balanced_sample, treated_tracts_panel)
  ),
  Projects = c(
    projects_units_0$n_projects,
    projects_units_1$n_projects,
    projects_units_2$n_projects,
    projects_units_3$n_projects,
    projects_units_4$n_projects,
    projects_units_5$n_projects
  ),
  Units = c(
    projects_units_0$n_units,
    projects_units_1$n_units,
    projects_units_2$n_units,
    projects_units_3$n_units,
    projects_units_4$n_units,
    projects_units_5$n_units
  )
)

# Load table utilities for removing wrappers
source(here("code", "helpers", "table_utilities.R"))

# Create LaTeX tables - Main version (caption/notes added manually in LaTeX)
attrition_latex <- attrition_table |>
  tt() |>
  format_tt(
    digits = 1,
    num_fmt = "decimal",
    num_mark_big = ",",
    escape = FALSE
  ) |>
  theme_tt(theme = "tabular")

# Create slides version without long note
attrition_slides <- attrition_table |>
  tt(caption = "Sample Attrition\\label{tab:sample_attrition}") |>
  format_tt(
    digits = 1,
    num_fmt = "decimal",
    num_mark_big = ","
  ) |>
  theme_tt(theme = "tabular")

# Save tables
output_dir <- here("output", "tables")
slides_dir <- here("output", "tables", "slides")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(slides_dir)) dir.create(slides_dir, recursive = TRUE)

# Save main version and remove table wrappers for threeparttable compatibility
save_tt(attrition_latex,
        here(output_dir, "sample_attrition_table.tex"),
        overwrite = TRUE)
remove_table_wrappers(here(output_dir, "sample_attrition_table.tex"))

# Save slides version
save_tt(attrition_slides,
        here(slides_dir, "sample_attrition_table.tex"),
        overwrite = TRUE)

# Print table for viewing
print(attrition_table)

