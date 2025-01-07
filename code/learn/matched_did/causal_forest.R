library(MatchIt)
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

# for causal forest
library(grf)

rm(list=ls())

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small", or "combined"
data_type <- "combined"

set.seed(123456L)

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


# Prep data for causal forest -----



# Heterogeneity: Casual Forest -----
library(grf)

# Assuming your dataset is called "data"
# Y is the outcome (e.g., property values)
# W is the treatment (e.g., public housing proximity)
# X are the covariates (e.g., neighborhood characteristics)

Y <- census_tract_data$asinh_pop_black     # Outcome variable
W <- data$treated             # Treatment variable (public housing)
X <- data[, c("income", "population_density", "asinh_distance_to_cbd")] # Covariates

# Train the causal forest
cf_model <- causal_forest(X, Y, W)
