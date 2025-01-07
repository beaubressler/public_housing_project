####
# Standard diff-in-diff analysis
####
library(tidyverse)
library(sf)
library(fixest)
library(did)
library(DIDmultiplegt)
# read in Census tract sample with treatment status
census_tract_sample <-
  st_read("data/derived/merged/census_tract_sample_with_treatment_status_combined.gpkg")

# 
treated_tracts_panel <-
  st_read("data/derived/merged/treated_tracts_panel_combined.gpkg") %>% 
  st_drop_geometry()


### Prep data -----
# create unique numeric id for each tract (COUNTYA, STATEA, TRACTA)
census_tract_sample <-
  census_tract_sample %>% 
  group_by(STATEA, COUNTYA, TRACTA) %>% 
  mutate(unique_id = cur_group_id())

# merge on treated years
treatment_years <-
  treated_tracts_panel %>% 
  select(STATE, COUNTY, TRACTA, treatment_year) %>% 
  distinct()

# merge on treatment year
dd_dataset <-
  census_tract_sample %>% 
  left_join(treatment_years) %>% 
  # replace NA with INF
  mutate(treatment_year = ifelse(is.na(treatment_year), 0, treatment_year)) %>% 
  filter(!is.na(black_share)) %>% 
  st_drop_geometry() %>% 
  mutate(relative_time = ifelse(treatment_year != 0, (YEAR - treatment_year)/10, Inf))

# convert to balanced panel: keep only tracts that are in the sample for all years (1930-1990)
dd_dataset_balanced <-
  dd_dataset %>% 
  group_by(unique_id) %>% 
  filter(n() == 7) %>% 
  ungroup() %>% 
  select(black_share, YEAR, unique_id, treatment_year, treated, city, distance_from_cbd, population_density) # %>% 
#  filter(city == "Chicago")

## Standard DiD ----





## Callaway Sant'Anna (2021)-----
cs21 = att_gt(
  yname         = "black_share",
  tname         = "YEAR",
  idname        = "unique_id",
  gname         = "treatment_year",
  xformla       = ~ distance_from_cbd + population_density,           # controls
  #control_group = "notyettreated", # Too few groups for "nevertreated" default
  clustervars   = "unique_id", 
  data          = dd_dataset_balanced
)

cs21


cs21_es = aggte(cs21, type = "dynamic", na.rm = TRUE)
cs21_es


ggdid(cs21_es)

## Sun-Abraham ---

sa20 = feols(
  black_share ~ sunab(treatment_year, relative_time) + I(distance_from_cbd*YEAR) + total_pop + population_density | unique_id + YEAR, 
  data = dd_dataset, vcov = ~unique_id
)
sa20


sa20 |>
  iplot(
    main     = "fixest::sunab",
    xlab     = "Time to treatment",
    drop     = "[[:digit:]]{2}",    # Limit lead and lag periods to -9:9
    ref.line = 1
  )

## DD with DIDmultiplegt ----

did_multiplegt(df = dd_dataset_balanced, Y = "black_share", G = "unique_id", T = "YEAR", D = "treated",
                       dynamic = 4,  # no. of post-treatment periods
                       placebo = 4, # no. of pre-treatment periods
                       brep = 20,
                       cluster = "unique_id",
               parallel = TRUE)
