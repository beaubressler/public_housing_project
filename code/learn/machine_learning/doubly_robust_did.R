
library(MatchIt)
library(Matching)
library(tidyverse)
library(sf)
library(lmtest)
library(fixest)
library(here)
library(RColorBrewer)

# libraries for covariate testing
library(cobalt)
library(tableone)
library(kableExtra)
#library for Sant'Anna and Zhou (2020)
library(DRDID)
# library for Callaway and Sant'Anna (2021)
library(did)

library(splines)


rm(list=ls())

# 0. Set seed and parameters -----

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small", or "combined"
data_type <- "combined"

set.seed(123456L)
# set number of nearest neighbors for KNN matching
knn_neighbors <- 1

# Notes: With CDD large, KNN = 2 seems to work well
# with the smaller dataset, KNN = 3 work

# define directories
merged_data_dir <- here("data", "derived", "merged", data_type)
map_output_dir <- here("output", "figures", "matched_did", data_type)
balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)
holc_data_dir <- here("data", "derived", "holc")

map_dir <- here("output", "figures", "matched_did", data_type, "with_rings")
results_dir <- here("output", "regression_results", "matched_did", data_type)
balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)

# directory to 
output_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset")

# define file paths
tract_with_treatment_status_filepath <-
  here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")
treated_tracts_panel_filepath <-
  here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")

event_study_rings_filepath <-
  here(merged_data_dir, "unique_tracts_and_rings.csv")

# read census data with treatment status
census_tract_sample_raw <-
  st_read(tract_with_treatment_status_filepath)

# read tract tracts panel
treated_tracts_panel_raw <-
  st_read(treated_tracts_panel_filepath) %>% 
  st_drop_geometry()

# read in unique rings and tracts
# These are the tracts that are treated, the ring of tracts touching those, and the ring of tracts
# touching the inner ring tracts
tracts_and_rings <-
  read_csv(event_study_rings_filepath) %>% 
  #try excluding outer
  filter(location_type != "outer")

# 1. Data Preparation ------

# read in HOLC data
holc_data <-
  read_csv(here(holc_data_dir, "tract_holc_classifications.csv")) %>% 
  select(-contains("city"))


# get treated tracts, unique, and merge onto census tract sample
treated_tracts_panel <-
  treated_tracts_panel_raw %>% 
  distinct(STATE, COUNTY, TRACTA, total_public_housing_units)

census_tract_data <-
  #left_join(census_tract_sample_raw, treated_tracts_panel) %>% 
  census_tract_sample_raw %>% 
  left_join(treated_tracts_panel) %>%
  # merge on HOLC data
  left_join(holc_data) %>% 
  # merge on rings
  left_join(tracts_and_rings) %>% 
  # set location_type = "donor_pool" if it's NA
  mutate(location_type = if_else(is.na(location_type), "donor_pool", location_type)) %>%
  # create unique id for each tract
  mutate(tract_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
  # # select relevant columns
  mutate(asinh_pop_total = asinh(total_pop),
         asinh_pop_white = asinh(white_pop),
         asinh_pop_black = asinh(black_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent = asinh(median_rent),
         asinh_median_home_value = asinh(median_home_value),
         asinh_distance_from_cbd = asinh(distance_from_cbd)) %>% 
  dplyr::rename(year = YEAR) %>% 
  st_drop_geometry() %>% 
  # Ensure data is sorted by tract_id and year
  arrange(tract_id, year)  %>% 
  # for HOLC variables (grade and category) if category is missisng ,set to "missing"
  mutate(category_most_overlap = ifelse(is.na(category_most_overlap), "missing", category_most_overlap),
         grade_most_overlap = ifelse(is.na(grade_most_overlap), "missing", grade_most_overlap))


# Tying Doubly-Robust DID -----
# Trying something...
test_data <-
  census_tract_data %>%
  # calculate lag values of some variables
  group_by(STATE, COUNTY, TRACTA) %>% 
  mutate(lag_black_share = lag(black_share),
         lag_asinh_pop_black = lag(asinh_pop_black),
         lag_asinh_pop_white = lag(asinh_pop_white),
         lag_asinh_pop_total = lag(asinh_pop_total),
         lag_population_density = lag(population_density),
         lag_pct_hs_grad = lag(pct_hs_grad),
         lag_low_skill_share = lag(low_skill_share),
         lag_median_income = lag(median_income),
         lag_median_rent = lag(median_rent),
         lag_median_home_value = lag(median_home_value),
         lag_total_units = lag(total_units),
         lag_high_skill_share = lag(high_skill_share),
         # create a numeric tract id 
         id_var = cur_group_id()) %>% 
  # calculate 2 period lags 
  mutate(lag2_black_share = lag(black_share, 2),
         lag2_asinh_pop_black = lag(asinh_pop_black, 2),
         lag2_asinh_pop_white = lag(asinh_pop_white, 2),
         lag2_asinh_pop_total = lag(asinh_pop_total, 2),
         lag2_population_density = lag(population_density, 2),
         lag2_pct_hs_grad = lag(pct_hs_grad, 2),
         lag2_low_skill_share = lag(low_skill_share, 2),
         lag2_median_income = lag(median_income, 2),
         lag2_median_rent = lag(median_rent, 2),
         lag2_median_home_value = lag(median_home_value, 2),
         lag2_high_skill_share = lag(high_skill_share, 2)) %>%
  ungroup() %>% 
  # set treatment_year == 0 if its missing
  mutate(treatment_year = if_else(is.na(treatment_year), 0, treatment_year)) %>% 
  mutate(city_FE = as.factor(paste0(city,COUNTY)),
         city_year_FE = as.factor(paste0(city,COUNTY,year)),
         redlining_FE = as.factor(category_most_overlap), 
         redlining_year_FE = as.factor(paste0(category_most_overlap,year)),
         redlining_city_year_FE = as.factor(paste0(category_most_overlap,city,year))) %>% 
  # interactions 
  mutate(distance_from_cbd_x_year  = distance_from_cbd * I(treatment_year <= year)) 
#%>%
 # filter(year >= 1940)


test_data_treated <-
  test_data %>% 
  filter(location_type != "inner")

test_data_inner <- 
  test_data %>% 
  filter(location_type != "treated") 


## 

drdid_result_treated <- att_gt(
  yname = "black_share",          # Outcome variable
  tname = "year",             # Time variable
  idname = "id_var",              # Unit identifier
  gname = "treatment_year",   # Group: when each unit first gets treated
  xformla = ~ lag_asinh_pop_total + lag_asinh_pop_black + 
    lag_population_density + lag_black_share + lag_high_skill_share +
    city_year_FE + distance_from_cbd +
    lag2_asinh_pop_total + lag2_asinh_pop_black +
    lag2_population_density + lag2_black_share + lag2_high_skill_share, # covariates
  data = test_data_treated,               # Full dataset
  control_group = "notyettreated",
  est_method = "dr",               # Doubly robust estimation
  clustervars = "id_var",         # Cluster at the unit level
  base_period = "universal"       # use universal base period
)

summary(drdid_result_treated)
agg_effects_treated <- aggte(drdid_result_treated, type = "dynamic", na.rm = TRUE)
summary(agg_effects_treated)

ggdid(agg_effects_treated) 



drdid_result_inner <- att_gt(
  yname = "asinh_pop_black",          # Outcome variable
  tname = "year",             # Time variable
  idname = "id_var",              # Unit identifier
  gname = "treatment_year",   # Group: when each unit first gets treated
  xformla = ~ lag_asinh_pop_total + lag_asinh_pop_black + 
    lag_population_density + lag_black_share + lag_high_skill_share +
    city_year_FE + distance_from_cbd  +
    lag2_asinh_pop_total + lag2_asinh_pop_black +
    lag2_population_density + lag2_black_share + lag2_high_skill_share, # covariates
  data = test_data_inner,               # Full dataset
   control_group = "notyettreated",
  est_method = "dr",               # Doubly robust estimation
  clustervars = "id_var",         # Cluster at the unit level
  base_period = "universal"       # use universal base period
)

summary(drdid_result_inner)
agg_effects_inner <- aggte(drdid_result_inner, type = "dynamic", na.rm = TRUE)
summary(agg_effects_inner)

ggdid(agg_effects_inner)


# Chaisemartin and D'Haultfoeuille (2024) Did ------
 # This doesn't seem liek a good option here
install.packages("DIDmultiplegtDYN") # Install the package (required only once)
library("DIDmultiplegtDYN") # Load the package into memory (required each new session)

did_multiplegt_dyn(df = test_data_treated,
                   outcome = "black_share",
                   effects = 3,
                   placebo = 2, 
                   group = "id_var",
                   cluster = "id_var",
                   time = "year",
                   treatment = "treated")

