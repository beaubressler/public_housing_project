####
#
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


# Abadie and LHour (2021)
source("lib/pensynth-master/functions/regsynth.R")


# Synthetic DiD (https://synth-inference.github.io/synthdid/)
devtools::install_github("synth-inference/synthdid")

# staggered synthetic did
remotes::install_github("tjhon/ssynthdid")


library(synthdid)

# My data: 
# read in Census tract sample with treatment status
census_tract_sample <-
  st_read("data/derived/merged/census_tract_sample_with_treatment_status.gpkg")

# panel of treated tracts with treatment year
treated_tracts_panel <-
  st_read("data/derived/merged/treated_tracts_panel.gpkg") %>% 
  st_drop_geometry()

# treatment years 
treatment_years <- 
  treated_tracts_panel %>% 
  group_by(STATE, COUNTY, TRACTA) %>% 
  # keep first 
  filter(row_number() == 1) %>%
  ungroup() %>% 
  st_drop_geometry() %>% 
  select(STATE, COUNTY, TRACTA, treatment_year)

# synthDid example with California ----
data('california_prop99')
setup = panel.matrices(california_prop99)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)


# Basic synth did example with chicago tracts ----
# 05/01/2024: The base SynthDiD ONLY works with
#  1. A balanced panel
#. 2. Non-staggered treatment timing

# There are now packages for doing synthetic DiD with staggered treatment timing
# However, for now, I will do this separately for each year of first-treatment
# This will also give me information about heterogeneous effects

# To do this I need to:
# 1. Identify the never treated tracts, who are the donor pool
# 2. For each year, create a dataset consisting of the panel of never treated tracts, and the panel of the tracts who are first treated 
# in that year


# note: to use panel.matrices, I want:
# 1. unit identifier as column 1
# 2. time identifier as column 2
# 3. Outcome as column 3
# 4. Treatment status as column 4

# keep only chicago tracts
nyc_tracts <- 
  census_tract_sample %>% filter(city == "New York City")

# keep only chicago never treated tracts
nyc_tracts_never_treated <- 
  nyc_tracts %>% 
  group_by(STATEA, COUNTYA, TRACTA) %>%
  filter(all(treated == 0)) %>% 
  ungroup()

# get tracts who were first treated in 1950
nyc_tracts_first_treated_1970 <- 
  nyc_tracts %>% 
  #filter(treated == 1) %>% 
  left_join(treatment_years) %>% 
  filter(treatment_year == 1970) 


# combine the two
nyc_tracts_synthdid_data <- 
  bind_rows(nyc_tracts_never_treated, nyc_tracts_first_treated_1970) %>% 
  select(STATEA, COUNTYA, TRACTA, YEAR, black_share, treated) %>% 
  st_drop_geometry() %>% 
  mutate(unit_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
  select(unit_id, YEAR, black_share, treated) %>% 
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
  # roun

# output the data for import to Stata
library(haven)
write_dta(nyc_tracts_synthdid_data, "data/derived/nyc_tracts_synthdid_data.dta")
  

## Create datasets for synthetic control, all cities, each treatment year separate------

# keep only never treated tracts
never_treated_tracts <- 
  census_tract_sample %>% 
  group_by(STATEA, COUNTYA, TRACTA, city) %>%
  filter(all(treated == 0)) %>% 
  ungroup() 
  
# get tracts who were first treated in 1940, 1950, 1960, 1970, 1980 
for (y in c(1940, 1950, 1960, 1970, 1980)) {
  
  output_name <- paste0("synth_did_data_", y)
  
  first_treated_tracts <-
    census_tract_sample %>% 
    # join on treatment years
    left_join(treatment_years) %>%
    filter(treatment_year == y)
  
  
  assign(output_name,
         bind_rows(never_treated_tracts, first_treated_tracts) %>% 
           st_drop_geometry() %>% 
           mutate(unit_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
           # include variables of interest
           select(unit_id, YEAR, black_share, treated,
                  city, population_density, housing_density,
                  median_rent, median_home_value, median_housing_age,
                  total_pop) %>% 
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
  
  
}

# write output to stata
write_dta(synth_did_data_1940, "data/derived/synth_did_data_1940.dta")
write_dta(synth_did_data_1950, "data/derived/synth_did_data_1950.dta")
write_dta(synth_did_data_1960, "data/derived/synth_did_data_1960.dta")
write_dta(synth_did_data_1970, "data/derived/synth_did_data_1970.dta")
write_dta(synth_did_data_1980, "data/derived/synth_did_data_1980.dta")

## Create datasets for synthetic control, all cities, all treatment years------

first_treated_tracts <-
  census_tract_sample %>% 
  # join on treatment years
  left_join(treatment_years) %>%
  filter(!is.na(treatment_year))


synth_did_data_1940_1980 <-
  # combine never_treated tracts and treated_tracts_panel
  bind_rows(never_treated_tracts, first_treated_tracts) %>% 
  st_drop_geometry() %>% 
  mutate(unit_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
  # include variables of interest
  select(unit_id, YEAR, black_share, treated,
         city, population_density, housing_density,
         median_rent, median_home_value, median_housing_age,
         total_pop) %>% 
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
write_dta(synth_did_data_1940_1980, "data/derived/synth_did_data_1940_1980.dta")

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