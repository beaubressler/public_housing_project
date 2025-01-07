####
# Run this program 
# This program prepares the data for the synthetic difference-in-differences analysis
# The output will be a balanced panel (or a set of balanced panel) of 
# 1. treated and never-treated tracts (ex. in)
# 2. Inner ring and never-treated tracts
# 3. Outer ring and never-treated tracts

#### 
library(tidyverse)
library(sf)
library(tigris)
library(crsuggest)
library(mapview)
library(maps)
library(ragg)
library(viridis)
library(stargazer)
library(Synth)
library(tidysynth)
library(haven)

data_out_dir <- "data/derived/data_for_sdid/"

# Abadie and LHour (2021)
source("lib/pensynth-master/functions/regsynth.R")

# Synthetic DiD (https://synth-inference.github.io/synthdid/)
devtools::install_github("synth-inference/synthdid")

# staggered synthetic did
remotes::install_github("tjhon/ssynthdid")

library(synthdid)

# Read and clean data:  --------------------------------------------

## Read in datasets -----
# read in Census tract sample with treatment status
census_tract_sample <-
  st_read("data/derived/merged/census_tract_sample_with_treatment_status.gpkg")

# panel of treated tracts with treatment year
treated_tracts_panel <-
  st_read("data/derived/merged/treated_tracts_panel.gpkg") %>% 
  st_drop_geometry()

# event study data with rings: Created by create_and_run_spatial_did.R
event_study_data_rings <-
  read_csv("data/derived/merged/event_study_data_rings.csv")

# treatment years 
treatment_years <- 
  treated_tracts_panel %>% 
  group_by(STATE, COUNTY, TRACTA) %>% 
  # keep first 
  filter(row_number() == 1) %>%
  ungroup() %>% 
  st_drop_geometry() %>% 
  select(STATE, COUNTY, TRACTA, treatment_year)

## Identify inner rings, first time they are nearby to treatment ----
inner_rings_full <-
  event_study_data_rings %>%
  filter(location_type == "inner") %>%
  select(STATE, COUNTY, TRACTA, treatment_year, location_type) 

# keep only the first time they are "treated"
inner_rings_first <-
  inner_rings_full %>%
  arrange(STATE, COUNTY, TRACTA, treatment_year) %>% 
  group_by(STATE, COUNTY, TRACTA) %>%
  # keep first
  filter(row_number() == 1) %>%
  ungroup() %>%
  st_drop_geometry()


## Identify outer rings, first time they are nearby to treatment -----

outer_rings_full <-
  event_study_data_rings %>%
  filter(location_type == "outer") %>%
  select(STATE, COUNTY, TRACTA, treatment_year, location_type)

# keep only the first time they are "treated"
outer_rings_first <-
  outer_rings_full %>%
  arrange(STATE, COUNTY, TRACTA, treatment_year) %>% 
  group_by(STATE, COUNTY, TRACTA) %>%
  # keep first
  filter(row_number() == 1) %>%
  ungroup() %>%
  st_drop_geometry()

# combined inner and outer rings
inner_and_outer_rings_first <-
  bind_rows(inner_rings_first, outer_rings_first) %>% 
  st_drop_geometry()


# Create datasets for synthetic control, all cities, each treatment year separate------

## donor pool: all tracts that are never treated, not in the inner or outer rings ----
donor_pool_tracts <- 
  census_tract_sample %>% 
  left_join(inner_and_outer_rings_first %>% select(-treatment_year))  %>% 
  group_by(STATEA, COUNTYA, TRACTA, city) %>%
  # remove treated tracts, inner ring, and outer ring tracts
  filter(all(treated == 0)) %>% 
  filter(is.na(location_type)) %>%
  ungroup() 


## Annual datasets  -----
# Treated, inner, and outer rings
for (y in c(1940, 1950, 1960, 1970, 1980)) {
  
  treated_output_name <- paste0("treated_synth_did_data_", y)
  inner_output_name <- paste0("inner_synth_did_data_", y)
  outer_output_name <- paste0("outer_synth_did_data_", y)
  
  
  first_treated_tracts <-
    census_tract_sample %>% 
    # join on treatment years
    left_join(treatment_years) %>%
    filter(treatment_year == y)
  
  inner_tracts_y <-
    census_tract_sample %>% 
    left_join(inner_rings_first) %>% 
    filter(treatment_year == y) %>%
    # treated if year >= treatment year
    mutate(treated = if_else(YEAR >= treatment_year, 1, 0))
  
  outer_tracts_y <-
    census_tract_sample %>% 
    left_join(outer_rings_first) %>% 
    filter(treatment_year == y) %>% 
    # treated if year >= treatment year
    mutate(treated = if_else(YEAR >= treatment_year, 1, 0))
  
  
  # treated 
  assign(treated_output_name,
         bind_rows(donor_pool_tracts, first_treated_tracts) %>% 
           st_drop_geometry() %>% 
           mutate(unit_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
           # include variables of interest
           select(unit_id, YEAR, black_share, treated,
                  city, population_density, housing_density,
                  median_rent, median_home_value, median_housing_age,
                  total_pop, distance_from_cbd, employment_pop_ratio) %>% 
           # round black_share
           mutate(black_share = round(black_share, 5)) %>%
           st_drop_geometry() %>% 
           # keep only unit_id that have non missing black_share in each year
           group_by(unit_id) %>%
           filter(all(!is.na(black_share))) %>%
           ungroup() %>%  
           # create balanced panel
           group_by(unit_id) %>%
           mutate(count = n()) %>% 
           ungroup() %>% 
           # Filter the data to keep only individuals with the same number of observations
           filter(count == max(count)) %>%
           select(-count) %>% 
           data.frame())
  
  # inner
  assign(inner_output_name,
         bind_rows(donor_pool_tracts, inner_tracts_y) %>% 
           st_drop_geometry() %>% 
           mutate(unit_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
           # include variables of interest
           select(unit_id, YEAR, black_share, treated,
                  city, population_density, housing_density,
                  median_rent, median_home_value, median_housing_age,
                  total_pop, distance_from_cbd, employment_pop_ratio) %>% 
           # round black_share
           mutate(black_share = round(black_share, 5)) %>%
           st_drop_geometry() %>% 
           # keep only unit_id that have non missing black_share in each year
           group_by(unit_id) %>%
           filter(all(!is.na(black_share))) %>%
           ungroup() %>%  
           # create balanced panel
           group_by(unit_id) %>%
           mutate(count = n()) %>% 
           ungroup() %>% 
           # Filter the data to keep only individuals with the same number of observations
           filter(count == max(count)) %>%
           select(-count) %>% 
           data.frame())
  
  # outer
  assign(outer_output_name,
         bind_rows(donor_pool_tracts, outer_tracts_y) %>% 
           st_drop_geometry() %>% 
           mutate(unit_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
           # include variables of interest
           select(unit_id, YEAR, black_share, treated,
                  city, population_density, housing_density,
                  median_rent, median_home_value, median_housing_age,
                  total_pop, distance_from_cbd, employment_pop_ratio) %>% 
           # round black_share
           mutate(black_share = round(black_share, 5)) %>%
           st_drop_geometry() %>% 
           # keep only unit_id that have non missing black_share in each year
           group_by(unit_id) %>%
           filter(all(!is.na(black_share))) %>%
           ungroup() %>%  
           # create balanced panel
           group_by(unit_id) %>%
           mutate(count = n()) %>% 
           ungroup() %>% 
           # Filter the data to keep only individuals with the same number of observations
           filter(count == max(count)) %>%
           select(-count) %>% 
           data.frame())
  
  
  # write output to stata
  write_dta(get(treated_output_name), paste0(data_out_dir, treated_output_name, ".dta"))
  write_dta(get(inner_output_name), paste0(data_out_dir, inner_output_name, ".dta"))
  write_dta(get(outer_output_name), paste0(data_out_dir, outer_output_name, ".dta"))
  
}

# write output to stata
# write_dta(synth_did_data_1940, "data/derived/synth_did_data_1940.dta")
# write_dta(synth_did_data_1950, "data/derived/synth_did_data_1950.dta")
# write_dta(synth_did_data_1960, "data/derived/synth_did_data_1960.dta")
# write_dta(synth_did_data_1970, "data/derived/synth_did_data_1970.dta")
# write_dta(synth_did_data_1980, "data/derived/synth_did_data_1980.dta")

## Create datasets for synthetic control, all cities, all treatment years------

first_treated_tracts_1940_1980 <-
  census_tract_sample %>% 
  # join on treatment years
  left_join(treatment_years) %>%
  filter(!is.na(treatment_year))

inner_tracts_1940_1980 <-
  census_tract_sample %>% 
  left_join(inner_rings_first) %>% 
  filter(!is.na(treatment_year)) %>% 
  # treated if YEAR >= treatment_year
  mutate(treated = YEAR >= treatment_year)

outer_tracts_1940_1980 <-
  census_tract_sample %>% 
  left_join(outer_rings_first) %>% 
  filter(!is.na(treatment_year)) %>%
  # treated if YEAR >= treatment_year
  mutate(treated = YEAR >= treatment_year)
  
# treated
treated_synth_did_data_1940_1980 <-
  # combine never_treated tracts and treated_tracts_panel
  bind_rows(donor_pool_tracts, first_treated_tracts_1940_1980) %>% 
  st_drop_geometry() %>% 
  mutate(unit_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
  # include variables of interest
  select(unit_id, YEAR, black_share, treated,
         city, population_density, housing_density,
         median_rent, median_home_value, median_housing_age,
         total_pop, distance_from_cbd, employment_pop_ratio) %>% 
  # round black_share
  mutate(black_share = round(black_share, 5)) %>%
  st_drop_geometry() %>% 
  # keep only unit_id that have non missing black_share in each year
  group_by(unit_id) %>%
  filter(all(!is.na(black_share))) %>%
  ungroup() %>%  
  # create balanced panel
  group_by(unit_id) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  # Filter the data to keep only individuals with the same number of observations
  filter(count == max(count)) %>%
  select(-count) %>% 
  data.frame()

# inner
inner_synth_did_data_1940_1980 <-
  bind_rows(donor_pool_tracts, inner_tracts_1940_1980) %>% 
  st_drop_geometry() %>% 
  mutate(unit_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
  # include variables of interest
  select(unit_id, YEAR, black_share, treated,
         city, population_density, housing_density,
         median_rent, median_home_value, median_housing_age,
         total_pop, distance_from_cbd, employment_pop_ratio) %>% 
  # round black_share
  mutate(black_share = round(black_share, 5)) %>%
  st_drop_geometry() %>% 
  # keep only unit_id that have non missing black_share in each year
  group_by(unit_id) %>%
  filter(all(!is.na(black_share))) %>%
  ungroup() %>%  
  # create balanced panel
  group_by(unit_id) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  # Filter the data to keep only individuals with the same number of observations
  filter(count == max(count)) %>%
  select(-count) %>% 
  data.frame()

# outer
outer_synth_did_data_1940_1980 <-
  bind_rows(donor_pool_tracts, outer_tracts_1940_1980) %>% 
  st_drop_geometry() %>% 
  mutate(unit_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
  # include variables of interest
  select(unit_id, YEAR, black_share, treated,
         city, population_density, housing_density,
         median_rent, median_home_value, median_housing_age,
         total_pop, distance_from_cbd, employment_pop_ratio) %>% 
  # round black_share
  mutate(black_share = round(black_share, 5)) %>%
  st_drop_geometry() %>% 
  # keep only unit_id that have non missing black_share in each year
  group_by(unit_id) %>%
  filter(all(!is.na(black_share))) %>%
  ungroup() %>%  
  # create balanced panel
  group_by(unit_id) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  # Filter the data to keep only individuals with the same number of observations
  filter(count == max(count)) %>%
  select(-count) %>% 
  data.frame()

# output to dta
write_dta(treated_synth_did_data_1940_1980, paste0(data_out_dir, "treated_synth_did_data_1940_1980.dta"))
write_dta(inner_synth_did_data_1940_1980,  paste0(data_out_dir, "inner_synth_did_data_1940_1980.dta"))
write_dta(outer_synth_did_data_1940_1980,  paste0(data_out_dir, "outer_synth_did_data_1940_1980.dta"))


# Create placebo dataset: pick random donor pool tracts and assign them to treatment years ----

#1950 
donor_pool_tract_ids <- 
  donor_pool_tracts %>% 
  select(STATE, COUNTY, TRACTA, city) %>% 
  st_drop_geometry() %>% 
  distinct()

placebo_tract_ids <- 
  donor_pool_tract_ids %>% 
  group_by(city) %>% 
  slice_sample(n = 10)

placebo_tracts_1950 <-
  donor_pool_tracts %>% 
  semi_join(placebo_tract_ids) %>%
  mutate(treatment_year = 1950) %>% 
  st_drop_geometry()

donor_pool_ex_placebo <- 
  donor_pool_tracts %>% 
  anti_join(placebo_tract_ids)

placebo_tracts_1950 <- 
  placebo_tracts_1950 %>% 
  mutate(treated = YEAR >= treatment_year) %>% 
  bind_rows(donor_pool_ex_placebo) %>% 
  st_drop_geometry() %>% 
  mutate(unit_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
  # include variables of interest
  select(unit_id, YEAR, black_share, treated,
         city, population_density, housing_density,
         median_rent, median_home_value, median_housing_age,
         total_pop, distance_from_cbd, employment_pop_ratio) %>% 
  # round black_share
  mutate(black_share = round(black_share, 5)) %>%
  st_drop_geometry() %>% 
  # keep only unit_id that have non missing black_share in each year
  group_by(unit_id) %>%
  filter(all(!is.na(black_share))) %>%
  ungroup() %>%  
  # create balanced panel
  group_by(unit_id) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  # Filter the data to keep only individuals with the same number of observations
  filter(count == max(count)) %>%
  select(-count) %>% 
  data.frame()

# 

write_dta(placebo_tracts_1950, paste0(data_out_dir, "placebo_1950.dta"))

# The bad news... my placebo exercise also picks up positive effects of the treatment...

# I will try to control for more things... maybe that will help 

## Misc ----

# panel_matrix <- panel.matrices(nyc_tracts_synthdid_data)
# 
# 
# # comput synth did estimate
# tau.hat = synthdid_estimate(panel_matrix$Y, panel_matrix$N0, panel_matrix$T0)

# Snyth Did with covariates ----
# See footnote 4 of Arkhangelsky et al 
# ${ }^4$ Some applications feature time-varying exogenous covariates $X_{i t} \in \mathbb{R}^p$. 
# We can incorporate adjustment for these covariates by applying SDID to the residuals 
# $Y_{i t}^{\text {res }}=Y_{i t}-X_{i t} \hat{\beta}$ of the regression of $Y_{i t}$ on $X_{i t}$.