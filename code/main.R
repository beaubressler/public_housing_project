#### 
# BATCH FILE FOR PUBLIC HOUSING PROJECT
####
library(here)

# Source tract configuration system
# source(here("code", "config", "tract_config.R"))

# Define directories
build_dir <- here("code", "build")
census_tract_build_dir <- here(build_dir, "census_tract")
full_count_build_dir <- here(build_dir, "full_count")
public_housing_build_dir <- here(build_dir, "public_housing")
other_data_build_dir <- here(build_dir, "other_data")
merged_build_dir <- here(build_dir, "merged_data")

learn_dir <- here("code", "learn")
exploratory_dir <- here(learn_dir, "exploratory")
site_selection_dir <- here(learn_dir, "site_selection")
stacked_did_dir <- here(learn_dir, "stacked_did")
matched_did_dir <- here(learn_dir, "matched_did")

exploratory_dir <- here(learn_dir, "exploratory")


# --------- BUILD -------
# Construct neighborhood data ----

## Construct tract crosswalks -----
###  crosswalks from 1930-1980 to 1950 tracts ----
source(here(census_tract_build_dir, "construct_tract_crosswalk_eglp_to_1950.R"))

### Create CBD indicators (based on 1980 CBDs) -----
source(here(census_tract_build_dir, "create_cbd_crosswalk.R"))

### 1930 and 1940 enumeration district -> 1950 Tract crosswalks -----
source(here(census_tract_build_dir, "construct_ed_to_tract_crosswalk_eglp_1950.R"))

##  Clean Census tract data from NHGIS -------

# NOTE: Pulled 2010 Census tract data from NHGIS API, commenting out 
#source(here(census_tract_build_dir, "download_2010_nhgis_data.R"))

# population by race
source(here(census_tract_build_dir, "clean_census_tract_population_data.R"))
# median years of education and share of people who finished HS
source(here(census_tract_build_dir, "clean_census_tract_education_data.R"))
# median incomes
source(here(census_tract_build_dir, "clean_census_tract_income_data.R"))
# median rent and house values 
source(here(census_tract_build_dir, "clean_census_tract_housing_data.R"))
# Occupation shares
source(here(census_tract_build_dir, "clean_census_tract_occupation_data.R"))
# employment 
source(here(census_tract_build_dir, "clean_census_tract_employment_data.R"))

## Construct tract-level data from full count -----
source(here(full_count_build_dir, "prep_grf_for_ed_merge.R"))
source(here(full_count_build_dir, "collapse_full_count_to_ed.R"))
source(here(full_count_build_dir, "collapse_ed_to_tract_data.R"))

## Clean other neighborhood data----
# clean holc map/redlining data
source(here(other_data_build_dir, "clean_holc_data.R"))

# clean urban renewal data 
source(here(other_data_build_dir, "clean_urban_renewal_data.R"))

# clean Zillow neighborhoods
source(here(other_data_build_dir, "clean_zillow_neighborhoods.R"))

## Combine Census and neighborhood data -----
source(here(merged_build_dir, "combine_neighborhood_data.R"))

# Prep public housing data -----
# clean PSH 1977 data (obtained from Yana Kucheva)
source(here(public_housing_build_dir, "clean_pic_77_kucheva_data.R"))

# Clean HUD 951 data 
source(here(public_housing_build_dir, "clean_hud_951_data.R"))

# Merge HUD data and CDD data to get public housing panel
source(here(public_housing_build_dir, "merge_hud_with_cdd.R"))

# Geolocate digitized public housing data
# Commenting out to not rerun geolocation unintentionally
#source(here(public_housing_build_dir, "geolocate_and_combine_digitized_data.R"))

# Create combined CDD + digitized public housing dataset
source(here(public_housing_build_dir, "create_final_public_housing_dataset.R"))

#  Combine Census and public housing data, create sample -----

# combine public housing and census, do some cleaning
source(here(merged_build_dir, "combine_public_housing_and_neighborhood_data.R"))

# create "balanced" sample 
source(here(merged_build_dir, "create_balanced_datasets.R"))

#  Create data for treated-inner-outer ring spatial diff-in-diff
source(here(merged_build_dir, "create_spatial_did_data.R"))


# --------- LEARN -------


# Summary stats and tables
source(here(learn_dir, "create_summary_statistics_tables.R"))
source(here(learn_dir, "create_project_attrition_table.R"))
source(here(learn_dir, "create_data_source_table.R"))

## Data exploration
# source(here(exploratory_dir, "explore_public_housing_data.R"))
source(here(exploratory_dir, "graph_census_data.R"))
source(here(exploratory_dir, "google_ngram_public_housing.R"))
source(here(exploratory_dir, "create_motivating_examples.R"))
source(here(exploratory_dir, "describe_public_housing_sample.R"))

## Regressions -----
### Site selection regressions
source(here(site_selection_dir, "site_selection_analysis.R"))
source(here(site_selection_dir, "project_demographics_analysis.R"))

### Run spatial DiD
# 5/2025: For now, don't really need to do this
#source(here(stacked_did_dir, "run_spatial_did_regressions.R"))

### Matched DiD -----
# Run complete matched DiD analysis (all matching variants + heterogeneity)
source(here(matched_did_dir, "run_complete_analysis.R"))

## Interpretation and context -----
source(here(exploratory_dir, "mechanical_vs_behavioral_decomposition.R"))

### Callaway-Sant'Anna staggered DiD -----
# source(here(learn_dir, "staggered_did", "run_callaway_santanna.R"))
# source(here(learn_dir, "staggered_did", "run_callaway_santanna_spillovers.R"))




