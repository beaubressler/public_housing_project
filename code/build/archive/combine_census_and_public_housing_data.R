### Right now, this is my main analysis file

# Load libraries
library(tidyverse)
library(sf)
library(tigris)
library(crsuggest)
library(mapview)
library(maps)
library(ragg)
library(viridis)

library(spdep)

# for event study
library(fixest)
library(ggfixest)
library(did2s)


library(Synth)
library(SCtools)

# 
did_output_dir <- "output/regression_results/stacked_did/"


# Read in and prep data ----

### census tract data ----
census_tract_data <- 
  read_sf("data/derived/census/tract_population_data.shp")

# check crs
crs_census <- st_crs(census_tract_data)
print(crs_census)


### public housing ----
# read data
cdd_projects_merged <- 
  read_csv("data/derived/public_housing/working/merged_cdd_projects.csv")

# Get longitude and latitude from NGDA and PSH 
cdd_projects_geolocated <-
  cdd_projects_merged %>% 
  # drop if missing latitude or longitude
  filter(!is.na(latitude), !is.na(longitude))

# convert to sf object
cdd_projects_geolocated_sf <- 
  cdd_projects_geolocated %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# note: Here, 4326 is the EPSG code for WGS 84, the standard coordinate reference system 
# for latitude and longitude. 

# make sure the CRS are matching 
cdd_projects_geolocated_sf <- 
  st_transform(cdd_projects_geolocated_sf, st_crs(census_tract_data))

st_crs(cdd_projects_geolocated_sf)

# look at only the locations
cdd_projects_geolocated_sf_loc_only <- 
  cdd_projects_geolocated_sf %>% 
  select(project_code, geometry)

# Define treated tracts, inner rings, and outer rings via spatial join ---- 
### Get only cities in our sample ----
# list of cities: NYC, Chicago, Detroit, Cleveland, St. Louis, Philadelphia, 
#                 Boston, Baltimore, Pittsburgh, Cincinnati, San Francisco

cdd_project_sample <-
  cdd_projects_geolocated_sf %>% 
  filter(locality == "NEW YORK CITY"|
         locality == "CHICAGO" | 
         locality == "DETROIT" & state == "MICHIGAN" |
         locality == "CLEVELAND-CUYAHOGA METROPOLITAN" |
         locality == "ST LOUIS CITY" |
         locality == "PHILADELPHIA" |
         (locality == "BOSTON" & state == "MASSACHUSETTS") |
         locality == "BALTIMORE CITY" |
         locality == "PITTSBURGH" |
         locality == "CINCINNATI-CINCINNATI METROPOLITAN" |
         locality == "SAN FRANCISCO"
          )

# get census tract city sample 
census_tract_sample <- 
  census_tract_data %>% 
  # new york city
  filter((COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York") |
           # chicago
           (COUNTY %in% c("Cook") & STATE == "Illinois") | 
            # detroit
            (COUNTY %in% c("Wayne") & STATE == "Michigan") |
             # cleveland
             (COUNTY %in% c("Cuyahoga") & STATE == "Ohio") |
              # st louis
              (COUNTY %in% ("St Louis City") & STATE == "Missouri") |
               # philadelphia
               (COUNTY %in% ("Philadelphia") & STATE == "Pennsylvania") |
                # boston
                (COUNTY %in% ("Suffolk") & STATE == "Massachusetts") |
                 # baltimore
                 (COUNTY %in% ("Baltimore City") & STATE == "Maryland") | 
                  # pittsburgh
                  (COUNTY %in% ("Allegheny") & STATE == "Pennsylvania") |
                   # cincinnati
                   (COUNTY %in% ("Hamilton") & STATE == "Ohio") |
                    # san francisco
                    (COUNTY %in% ("San Francisco") & STATE == "California"))
  

### Rings of tracts: Panel data -----
# Ultimately, I will want, in each decade, the tracts that are treated, the
# "first ring" (the tracts that are touching the treated tracts)
# and the "second ring" (the tracts that are touching the first ring tracts)

#.0. Get "treated tracts": Join Census tracts to public housing data 
# 1. Get tracts that touch those tracts ("inner ring")
# 1.1 Create a dataframe with all tract combinations and whether they touch or not
# 2. Get "outer ring"
# 2.1 Create a dataframe with all tract combinations and whether they touch or not


# Define concordance between dates in census_tract_sample and cdd_project_sample
# In particular: Going to follow Guennwig (2023) and define any tract that 
# receives public housing in 1941-1950 as treated in 1950 onward.

cdd_project_sample <- 
  cdd_project_sample %>% 
  # create treament year = 1940 if yearfullocc = 1931-1940, treatment_year = 1950 if yearfullocc = 1941-1950, etc, up through yearfullocc = 1970
  mutate(treatment_year = case_when(
    yearfullocc %in% 1931:1940 ~ 1940,
    yearfullocc %in% 1941:1950 ~ 1950,
    yearfullocc %in% 1951:1960 ~ 1960,
    yearfullocc %in% 1961:1970 ~ 1970,
    yearfullocc %in% 1971:1980 ~ 1980,
    TRUE ~ NA_real_
  ))
  

# spatial join by treatment_year (cdd_project_sample) and YEAR (census)

census_tract_sample_indexed <- 
  census_tract_sample %>% 
  mutate(tract_id = row_number())

# join on geometry, then filter only where treatment_year >= YEAR
# So this would imply that if treatment_year == 1950, we will count the tract as treated
# only in 1950 and 1940. I think we want the opposite: YEAR >= treatment_year
# That's when we want to consider the tract treated
treated_tracts <-
  st_join(census_tract_sample_indexed,
          cdd_project_sample, 
          join = st_intersects)

treated_tracts_panel <-
  treated_tracts %>% 
  # keep if census year >= treatment year
  filter(YEAR >= treatment_year) %>% 
  # identifier for year treated 
  #0.5 drop duplicates: County only treated once per year (for now)
  group_by(YEAR, TRACTA, COUNTY, STATE) %>% 
  filter(row_number() == 1)  %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  select(tract_id, TRACTA, COUNTY, STATE, YEAR, treatment_year) %>%
  mutate(treated = 1)

# Create, in each year, the treated, inner, and outer rings
# all neighbors among sample tracts

# 0. Choose 2000 as year to calculate neighbors. 
census_tract_sample_indexed_2000 <-
  census_tract_sample_indexed %>%
  filter(YEAR == 2000)

# get 1990 crosswalk
census_tract_2000_crosswalk <-
  census_tract_sample_indexed_2000 %>% 
  mutate(row_number = row_number()) %>%
  select(row_number, tract_id, TRACTA, COUNTY, STATE) 

tract_neighbors <- 
  spdep::poly2nb(census_tract_sample_indexed_2000) %>% 
  nb2mat(zero.policy = TRUE, style = "B") %>% 
  as_tibble() %>% 
  mutate(from = row_number()) %>%
  pivot_longer(cols = -from, names_to = "to", values_to = "neighbor") %>% 
  # remove the Vs from "to" column and convert to numeric
  mutate(to = as.numeric(gsub("V", "", to))) %>% 
  # Merge TRACTA, COUNTY, STATEA onto "from" of tract_neighbors, to merge to others later
  left_join(census_tract_2000_crosswalk, by = c("from" = "row_number")) %>% 
  select(-tract_id) %>% 
  st_drop_geometry() %>% 
  # rename tract information of "from" to "TRACTA_from" etc
  rename(TRACTA_from = TRACTA, COUNTY_from = COUNTY, STATE_from = STATE)


### Get neighbors of treated tracts (inner ring) and their neighbors (outer ring)-----
# There are two versions of this:
# 1. Time series datasets of all treated tracts, inner rings, outer rings
# 2. Stacked dataset with inner and outer rings linked to treated tracts directly
#    This would be for the econometrics

#  To do this:
# 1. Join tract_neighbors to treated_tracts_panel by tract information. This should
#    get, for each treated tract, the row_number in census_tract_sample_indexed_2000
#.  of all of it's neighbors
# 2. Filter to only those where neighbor == 1
# 3. Merge on the tract information of the "FROM"

inner_rings_panel <- 
  # Merge treated tracts time series onto tract_neighbors by tract information
  left_join(treated_tracts_panel,
            tract_neighbors,
            by = c("TRACTA" = "TRACTA_from", "COUNTY" = "COUNTY_from",
                   "STATE" = "STATE_from")) %>% 
  # 2. Filter to only those where neighbor == 1
  filter(neighbor == 1)  %>% 
  select(from, to, YEAR) %>%
  st_drop_geometry() %>%
  # 3. Merge on the tract information of the inner ring
  left_join(census_tract_2000_crosswalk, by = c("to" = "row_number")) %>% 
  dplyr::rename(TRACT_to = TRACTA, COUNTY_to = COUNTY,
                STATE_to = STATE) %>%
  mutate(inner_ring = 1) %>% 
  st_drop_geometry() %>% 
  distinct()
  
# 1. take neighbors with from in the list of "to" (in inner rings). This will get the list of neighbors for all of the inner ring tracts, in all years
# 1. get distinct list
# 2. left_join onto the inner rings to get year pairs by what? from (in outer_ring) to to (in inner_ring)
# 3. I think I am now missing the correct geometry for the new "to"... merge that on via the crosswalk
outer_rings_panel <-  
  tract_neighbors %>% 
  st_drop_geometry() %>% 
  filter(from %in% unique(inner_rings_panel$to)) %>% 
  filter(neighbor == 1) %>% 
  distinct() %>%
  right_join(inner_rings_panel, by = c("from" = "to")) %>% 
  select(YEAR, from, to) %>% 
  left_join(census_tract_2000_crosswalk, by = c("to" = "row_number")) %>% 
  mutate(outer_ring = 1) %>%
  st_drop_geometry() %>% 
  distinct()
  


# City-by-city maps of treated, inner, and outer ring tracts in each year ----
# To do this: 
#  1. Get tract data for each city (time series) 
#. For each year:
#   2. Get treated tracts, inner ring tracts, outer ring tracts for that city
#.  3. Create dataset with all tracts, and a variable for treated, inner, outer
   # 3.1 Treated takes precedence over inner takes precedence over outer
#   4. Plot map of tracts, colored by treated, inner, outer

### Define cities  ----

### list of counties in each city

nyc_counties <- c("New York", "Kings", "Queens", "Bronx", "Richmond")
nyc_state <- "New York"

chicago_counties <- c("Cook")
chicago_state <- "Illinois"

philadelphia_counties <- c("Philadelphia")
philadelphia_state <- "Pennsylvania"

cleveland_counties <- c("Cuyahoga")
cleveland_state <- "Ohio"

detroit_counties <- c("Wayne")
detroit_state <- "Michigan"

boston_counties <- c("Suffolk")
boston_state <- "Massachusetts"

baltimore_counties <- c("Baltimore City")
baltimore_state <- "Maryland"

pittsburgh_counties <- c("Allegheny")
pittsburgh_state <- "Pennsylvania"

stlouis_counties <- c("St Louis City")
stlouis_state <- "Missouri"

cincinnati_counties <- c("Hamilton")
cincinnati_state <- "Ohio"

sanfran_counties <- c("San Francisco")
sanfran_state <- "California"

# for each city, define tract data as census_tract_data where 
# county %in% relevant county list and state == relevant state

nyc_census_tract_data <-
  census_tract_data %>% 
  filter(COUNTY %in% nyc_counties, 
         STATE == nyc_state)

chicago_census_tract_data <-
  census_tract_data %>% 
  filter(COUNTY %in% chicago_counties, 
         STATE == chicago_state)
  
philadelphia_census_tract_data <-
  census_tract_data %>% 
  filter(COUNTY %in% philadelphia_counties, 
         STATE == philadelphia_state)

cleveland_census_tract_data <-
  census_tract_data %>% 
  filter(COUNTY %in% cleveland_counties, 
         STATE == cleveland_state)

detroit_census_tract_data <-
  census_tract_data %>% 
  filter(COUNTY %in% detroit_counties, 
         STATE == detroit_state)

boston_census_tract_data <-
  census_tract_data %>% 
  filter(COUNTY %in% boston_counties, 
         STATE == boston_state)

baltimore_census_tract_data <-
  census_tract_data %>% 
  filter(COUNTY %in% baltimore_counties, 
         STATE == baltimore_state)

pittsburgh_census_tract_data <-
  census_tract_data %>% 
  filter(COUNTY %in% pittsburgh_counties, 
         STATE == pittsburgh_state)

stlouis_census_tract_data <-
  census_tract_data %>% 
  filter(COUNTY %in% stlouis_counties, 
         STATE == stlouis_state)

cincinnati_census_tract_data <-
  census_tract_data %>% 
  filter(COUNTY %in% cincinnati_counties, 
         STATE == cincinnati_state)

sanfran_census_tract_data <-
  census_tract_data %>% 
  filter(COUNTY %in% sanfran_counties, 
         STATE == sanfran_state) %>% 
  # filter out islands off the coast ("Farallon Islands" and "Treasure Island")
  filter(TRACTA !=  "060400")


### Create maps for all cities and all years  ----

# 1. Get tract data for each city (time series)
cities <- c("NYC", "Chicago", "Philadelphia", "Cleveland", "Detroit",
                      "Boston", "Baltimore", "Pittsburgh", "StLouis",
            "Cincinnati", "SanFran")

# loop through all cities
for (city in cities) {
  
  # get census tract data for that city
  city_census_tract_data <- get(paste0(tolower(city), "_census_tract_data"))
  # get county and state for that city
  city_counties <- get(paste0(tolower(city), "_counties"))
  city_state <- get(paste0(tolower(city), "_state"))
  
# loop through 1940-1980
  for (y in seq(1940, 1980, 10)) {
    # 2. Get treated tracts, inner ring tracts, outer ring tracts for that city
    city_treated_tracts <-
      treated_tracts_panel %>% 
      filter(YEAR == y) %>%
      filter(STATE == city_state, COUNTY %in% city_counties) %>%
      select(treated, STATE, COUNTY, TRACTA)
    
    city_inner_rings <-
      inner_rings_panel %>% 
      filter(YEAR == y) %>%
      filter(STATE_to == city_state, COUNTY_to %in% city_counties)  %>%
      dplyr::rename(TRACTA = TRACT_to, COUNTY = COUNTY_to, STATE = STATE_to) %>% 
      select(inner_ring, STATE, COUNTY, TRACTA) %>% 
      # keep one of each state county tracta
      distinct()
    
    city_outer_rings <-
      outer_rings_panel %>% 
      filter(YEAR == y) %>%
      filter(STATE == city_state, COUNTY %in% city_counties) %>%
      select(outer_ring, STATE, COUNTY, TRACTA) %>% 
      distinct()
    
    # 3. Create dataset with all tracts, and a variable for treated, inner, outer
    city_map_data <-
      city_census_tract_data %>% 
      left_join(city_treated_tracts, by = c("STATE", "COUNTY", "TRACTA")) %>% 
      left_join(city_inner_rings, by = c("STATE", "COUNTY", "TRACTA")) %>% 
      left_join(city_outer_rings, by = c("STATE", "COUNTY", "TRACTA")) %>% 
      # 3.1 Treated takes precedence over inner takes precedence over outer
      mutate(treated = ifelse(is.na(treated), 0, 1),
             inner_ring = ifelse((is.na(inner_ring) | treated == 1), 0, 1),
             outer_ring = ifelse((is.na(outer_ring) | treated == 1 | inner_ring == 1), 0, 1)) %>% 
      # create string "status" based on treated , inner_ring, outer_ring
      mutate(status = case_when(
        treated == 1 ~ "treated",
        inner_ring == 1 ~ "inner_ring",
        outer_ring == 1 ~ "outer_ring",
        TRUE ~ "other"))
    
    # 4. Plot map of tracts, colored by treated, inner, outer
    ggplot(city_map_data) + geom_sf(aes(fill = status)) +
      # add line bordering each tract
      geom_sf(data = city_map_data, fill = NA, color = "black") +
      theme_minimal() + 
      scale_fill_manual(values = c("lightblue", "gray", "darkblue", "white")) +
      # add title
      ggtitle(paste0(city, " treated and control tracts, ", y)) +
      # add caption
      labs(caption = paste0("Tracts are considered treated if public housing project",
                            " was constructed in tract on the year of or before the year")) +
      # remove legend title
      guides(fill = guide_legend(title = NULL))
    
    # save plot 
    ggsave(paste0("output/figures/treated_and_control_ring_maps/", tolower(city), "/",
                  tolower(city), "_map_", y, ".png"), width = 10, height = 10, bg = "white")
  }

}


# Set up and run event study  ----
# What do I need in the data for this
# Panel dataset of Census tracts (with geographic info),
# black and white pop share, treated, inner, outer, year

# However, I need some way to relate the treated tracts with its inner rings and outer
# rings, since I specifically want to compare treated tracts with their innesanr and outer
# rings... 

# The difference in the datasets above is that now, I want to link the treated tracts
# to their inner and outer rings, so that I can compare the treated tracts with their
# inner and outer rings


## Set up event study dataset -----


### 1. Get treated tracts, inner ring tracts, outer ring tracts in a wide dataset ---- 
# inner ring and treated 
event_study_data <-
  # Merge treated tracts time series onto tract_neighbors by tract information
  left_join(treated_tracts_panel,
            tract_neighbors,
            by = c("TRACTA" = "TRACTA_from", "COUNTY" = "COUNTY_from",
                   "STATE" = "STATE_from")) %>% 
  # 2. Filter to only those where neighbor == 1
  filter(neighbor == 1)  %>% 
  select(-geometry, -neighbor, -tract_id) %>% 
  # rename columns
  dplyr::rename(treated_id = from, inner_ring_id = to,
                TRACTA_treated = TRACTA, COUNTY_treated = COUNTY,
                STATE_treated = STATE) %>%
  # 3. Merge on the tract information of the inner ring
  left_join(census_tract_2000_crosswalk, by = c("inner_ring_id" = "row_number")) %>% 
  dplyr::rename(TRACTA_inner = TRACTA, COUNTY_inner = COUNTY,
                STATE_inner = STATE) %>%
  select(-geometry, -tract_id) %>% 
  left_join(outer_rings_panel %>% select(-geometry, -tract_id),
            by = c("inner_ring_id" = "from", "YEAR")) %>% 
  # rename variables
  dplyr::rename(outer_ring_id = to, TRACTA_outer = TRACTA, COUNTY_outer = COUNTY,
                STATE_outer = STATE) %>% 
  select(-outer_ring)

#### Ensure only never-treated observations are in the controls (inner and outer rings) ----
# identify tracts that are treated at some point
ever_treated_tracts <- 
  treated_tracts_panel %>% 
  select(STATE, COUNTY, TRACTA) %>%
  distinct() %>% 
  mutate(ever_treated = 1)

event_study_data  <- 
  event_study_data %>% 
  # drop inner rings that are treated at some point
  left_join(ever_treated_tracts,
            by = c("TRACTA_inner" = "TRACTA", "COUNTY_inner" = "COUNTY",
                                   "STATE_inner" = "STATE")) %>%
  filter(is.na(ever_treated)) %>%
  select(-ever_treated) %>% 
  # drop outer rings that are treated at some point 
  left_join(ever_treated_tracts,
            by = c("TRACTA_outer" = "TRACTA", "COUNTY_outer" = "COUNTY",
                                   "STATE_outer" = "STATE")) %>%
  filter(is.na(ever_treated)) %>% 
  select(-ever_treated)
  
### Include 1930 data for all of the tracts for the event study
# I think I can get all unique tracts/ring combinations, set year = 1930, append onto dataset

tracts_and_rings <-
  event_study_data %>%
    select(TRACTA_treated, STATE_treated, COUNTY_treated,
           TRACTA_inner, STATE_inner, COUNTY_inner,
           TRACTA_outer, STATE_outer, COUNTY_outer,
           treatment_year, treated_id) %>%
    distinct()

year <- c(1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000)

# all combinations of x and y 
event_study_data_full <- expand_grid(tracts_and_rings, year)
    
  
# get all unique combinations of tract, state, county for treated, inner, and outer rings
# all_tract_combinations <-
#   event_study_data %>%
#   select(TRACTA_treated, STATE_treated, COUNTY_treated,
#          TRACTA_inner, STATE_inner, COUNTY_inner,
#          TRACTA_outer, STATE_outer, COUNTY_outer,
#          treatment_year) %>%
#   distinct() %>% 
#   # set year = 1930, treated = 0 
#   mutate(YEAR = 1930, treated = 0)
# 
# # append to event_study_data
# event_study_data_wide <- 
#   event_study_data %>% 
#   bind_rows(all_tract_combinations) 

### 2. Reshape long and merge on census data ----

# reshape to long 
event_study_long <- 
  event_study_data_full %>% 
  pivot_longer(cols = c(COUNTY_treated, STATE_treated, TRACTA_treated, 
                        COUNTY_inner, STATE_inner, TRACTA_inner, 
                        COUNTY_outer, STATE_outer, TRACTA_outer), 
               names_to = c(".value", "location_type"), 
               names_sep = "_")

census_data_for_event_study <-
  census_tract_sample %>% 
  select(STATE, COUNTY, TRACTA, YEAR, blck_sh, wht_shr, whit_pp, blck_pp, totl_pp) %>%
  st_drop_geometry() %>% 
  dplyr::rename(year = YEAR) %>% 
  mutate(ln_pop_tot = log(totl_pp),
         ln_pop_wht = log(whit_pp),
         ln_pop_blk = log(blck_pp))


event_study_final <-
  left_join(event_study_long, census_data_for_event_study) %>% 
  # define treated variable equal to 1 if year >= treatment year and location_type == treated
  mutate(treated = ifelse(year >= treatment_year & location_type == "treated", TRUE, FALSE)) %>%
  # create variables for sun abraham estimates
  mutate(cohort = as.factor(year), 
         relative_period = (year - treatment_year)/10) %>% 
  # keep distinct observations: One observation per treated_id-tract-year
  distinct()

## Run event study ----

### 1. Basic event study: Compare treated tracts to outer ring -----
event_study_data_basic <- 
  event_study_final %>%
  filter(location_type != "inner") %>% 
  mutate(event_time = as_factor(year - treatment_year)) %>%
  mutate(treated_group = case_when(location_type == "treated" ~ 1,
                                   location_type == "outer" ~ 0))

# Include project (treated_id)-year fixed effects

event_study_basic_blk_sh = feols(blck_sh ~ treated_group*event_time | treated_id*year + TRACTA,
                        data = event_study_data_basic, cluster = "treated_id")

ggcoefplot(event_study_basic_blk_sh, keep = "treated_group:") +
  labs(title = "Event Study: Treated Tracts vs. Outer Ring",
       subtitle = "Black Share of Population")

event_study_basic_wht_sh = feols(wht_shr ~ treated_group*event_time | treated_id*year + TRACTA,
                          data = event_study_data_basic, cluster = "treated_id")

ggcoefplot(event_study_basic_wht_sh, keep = "treated_group:") +
  labs(title = "Event Study: Treated Tracts vs. Outer Ring",
       subtitle = "White Share of Population")

# Total pop
event_study_basic_pop = feols(ln_pop_tot ~ treated_group*event_time | treated_id*year + TRACTA,
                          data = event_study_data_basic, cluster = "treated_id")

ggcoefplot(event_study_basic_pop, keep = "treated_group:") +
  labs(title = "Event Study: Treated Tracts vs. Outer Ring",
       subtitle = "Ln total Population")

# black pop 

event_study_basic_blk_pop = feols(ln_pop_blk ~ treated_group*event_time | treated_id*year + TRACTA,
                          data = event_study_data_basic, cluster = "treated_id")

ggcoefplot(event_study_basic_blk_pop, keep = "treated_group:") +
  labs(title = "Event Study: Treated Tracts vs. Outer Ring",
       subtitle = "Ln Black Population")

# white pop

event_study_basic_wht_pop = feols(ln_pop_wht ~ treated_group*event_time | treated_id*year + TRACTA,
                          data = event_study_data_basic, cluster = "treated_id")

ggcoefplot(event_study_basic_wht_pop, keep = "treated_group:") +
  labs(title = "Event Study: Treated Tracts vs. Outer Ring",
       subtitle = "Ln White Population")

#### 1.5 Gardner's 2-stage diff-in-diff using Did2s (2020) -----
# 01/2024: Since I can't use the ring x year fixed effects
# event_study_data_basic_did2s <-
#   event_study_data_basic %>%
#   # set event_time = event_time/10
#   mutate(event_time = (year - treatment_year)/10) %>%
#   # set event_time = Inf if treated_group == 0
#   mutate(event_time = ifelse(treated_group == 0, Inf, event_time))
# 
# # Note: This doesnt' work if i include the treated_id x year fixed effect
# # NEED TO BOOTSTRAP
# es <- did2s(event_study_data_basic_did2s,
#             yname = "blck_sh", first_stage = ~ 0 | year + treated_id + TRACTA, 
#             second_stage = ~i(event_time, ref=c(-1, Inf)), treatment = "treated", 
#             cluster_var = "treated_id", bootstrap = TRUE, n_bootstraps = 10)
# 
# ggcoefplot(es)


### 2. Event study with inner rings as well ----

# I think here, I need a ring variable, so not just treated group, but ring
event_study_data_rings <- 
  event_study_final %>%
  mutate(event_time = as_factor(year - treatment_year)) %>%
  mutate(ring = as.factor(case_when(location_type == "treated" ~ 0,
                          location_type == "inner" ~ 1,
                          location_type == "outer" ~ 2)),
         location_type_factor = factor(location_type, levels = c("outer", "treated", "inner")))

# get list of 

event_study_rings_blk_sh = feols(blck_sh ~ location_type_factor*event_time | treated_id*year + TRACTA,
                                 data = event_study_data_rings, cluster = "treated_id")

event_study_rings_blk_sh_tidy <-
broom::tidy(event_study_rings_blk_sh) %>% 
# round all numeric variables to 4 digits
mutate_if(is.numeric, round, 4) %>%
# create a variable called "location_type" equal to "inner" if term contains "inner"
mutate(location_type = case_when(str_detect(term, "inner") ~ "inner",
                                 str_detect(term, "treated") ~ "treated")) %>% 
# remove treated and inner from term
mutate(term = str_remove(term, "inner|treated")) %>%
# keep if term contains "location_type_factor:"
filter(str_detect(term, "location_type_factor:")) %>% 
# remove location_type_factor: from term
mutate(term = str_remove(term, "location_type_factor:")) %>% 
# remove event_time from term, and then convert term to numeric
mutate(term = as.numeric(str_remove(term, "event_time"))) %>%
# rename term to event_time
rename(event_time = term) %>% 
mutate(event_time = as.numeric(event_time))


# plot with even_time on x axis, estimate on y axis, 2 standard errors as error bars,
# and location_type as color
ggplot(event_study_rings_blk_sh_tidy, aes(x = event_time, y = estimate,
                                          color = location_type)) +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                width = 0.2, alpha = 0.4) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Effect of Public Housing on Black Share of Population",
       subtitle = "",
       x = "Years Relative to Treatment",
       y = "Difference in Difference Estimate") +
  # show x-axis labels every ten years from -40 to 60
  scale_x_continuous(breaks = seq(-40, 60, 10)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  # change legend title
  guides(color = guide_legend(title = "Location Type")) +
  # change legend labels
  scale_color_manual(values = c("inner" = "blue", "treated" = "red"),
                     labels = c("Inner Ring", "Treated")) +
  theme(plot.background = element_rect(fill = "white"))

# output graph
ggsave(paste0(did_output_dir, "event_study_rings_blk_sh.png"), width = 8, height = 6, units = "in", background = "white")

# white share 
event_study_rings_wht_sh = feols(wht_shr ~ location_type_factor*event_time | treated_id*year + TRACTA,
                                 data = event_study_data_rings, cluster = "treated_id")

event_study_rings_wht_sh_tidy <-
  broom::tidy(event_study_rings_wht_sh) %>% 
  # round all numeric variables to 4 digits
  mutate_if(is.numeric, round, 4) %>%
  # create a variable called "location_type" equal to "inner" if term contains "inner"
  mutate(location_type = case_when(str_detect(term, "inner") ~ "inner",
                                   str_detect(term, "treated") ~ "treated")) %>% 
  # remove treated and inner from term
  mutate(term = str_remove(term, "inner|treated")) %>%
  # keep if term contains "location_type_factor:"
  filter(str_detect(term, "location_type_factor:")) %>% 
  # remove location_type_factor: from term
  mutate(term = str_remove(term, "location_type_factor:")) %>% 
  # remove event_time from term, and then convert term to numeric
  mutate(term = as.numeric(str_remove(term, "event_time"))) %>%
  # rename term to event_time
  rename(event_time = term) %>% 
  mutate(event_time = as.numeric(event_time))
  

# plot with even_time on x axis, estimate on y axis, 2 standard errors as error bars,
# and location_type as color
ggplot(event_study_rings_wht_sh_tidy, aes(x = event_time, y = estimate,
                                        color = location_type)) +
geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
              width = 0.2, alpha = 0.4) +
geom_point() +
geom_hline(yintercept = 0, linetype = "dashed") +
labs(title = "Effect of Public Housing on White Share of Population",
     subtitle = "",
     x = "Years Relative to Treatment",
     y = "Difference in Difference Estimate") +
# show x-axis labels every ten years from -40 to 60
scale_x_continuous(breaks = seq(-40, 60, 10)) +
theme_minimal() +
theme(legend.position = "bottom") +
# change legend title
guides(color = guide_legend(title = "Location Type")) +
# change legend labels
scale_color_manual(values = c("inner" = "blue", "treated" = "red"),
                   labels = c("Inner Ring", "Treated")) +
  theme(plot.background = element_rect(fill = "white"))

  
# output graph
ggsave(paste0(did_output_dir, "event_study_rings_wht_sh.png"), width = 8, height = 6, units = "in")

# tot pop 

event_study_rings_tot_pop = feols(ln_pop_tot ~ location_type_factor*event_time | treated_id*year + TRACTA,
                                 data = event_study_data_rings, cluster = "treated_id")
  
event_study_rings_tot_pop_tidy <-
  broom::tidy(event_study_rings_tot_pop) %>% 
# round all numeric variables to 4 digits
  mutate_if(is.numeric, round, 4) %>%
  # create a variable called "location_type" equal to "inner" if term contains "inner"
  mutate(location_type = case_when(str_detect(term, "inner") ~ "inner",
                                   str_detect(term, "treated") ~ "treated")) %>% 
  # remove treated and inner from term
  mutate(term = str_remove(term, "inner|treated")) %>%
  # keep if term contains "location_type_factor:"
  filter(str_detect(term, "location_type_factor:")) %>% 
  # remove location_type_factor: from term
  mutate(term = str_remove(term, "location_type_factor:")) %>% 
  # remove event_time from term, and then convert term to numeric
  mutate(term = as.numeric(str_remove(term, "event_time"))) %>%
  # rename term to event_time
  rename(event_time = term) %>% 
  mutate(event_time = as.numeric(event_time))
    
  
# plot with even_time on x axis, estimate on y axis, 2 standard errors as error bars,
# and location_type as color
ggplot(event_study_rings_tot_pop_tidy, aes(x = event_time, y = estimate,
                                          color = location_type)) +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                width = 0.2, alpha = 0.4) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Effect of Public Housing on Total Population",
       subtitle = "",
       x = "Years Relative to Treatment",
       y = "Difference in Difference Estimate") +
  # show x-axis labels every ten years from -40 to 60
  scale_x_continuous(breaks = seq(-40, 60, 10)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  # change legend title
  guides(color = guide_legend(title = "Location Type")) +
  # change legend labels
  scale_color_manual(values = c("inner" = "blue", "treated" = "red"),
                     labels = c("Inner Ring", "Treated")) +
  theme(plot.background = element_rect(fill = "white"))


# output graph
ggsave(paste0(did_output_dir, "event_study_rings_tot_pop.png"), width = 8, height = 6, units = "in")

# black pop
event_study_rings_blck_pop = feols(ln_pop_blk ~ location_type_factor*event_time | treated_id*year + TRACTA,
                                 data = event_study_data_rings, cluster = "treated_id")
  
event_study_rings_blck_pop_tidy <-
  broom::tidy(event_study_rings_blck_pop) %>% 
    # round all numeric variables to 4 digits
    mutate_if(is.numeric, round, 4) %>%
    # create a variable called "location_type" equal to "inner" if term contains "inner"
    mutate(location_type = case_when(str_detect(term, "inner") ~ "inner",
                                     str_detect(term, "treated") ~ "treated")) %>% 
    # remove treated and inner from term
    mutate(term = str_remove(term, "inner|treated")) %>%
    # keep if term contains "location_type_factor:"
    filter(str_detect(term, "location_type_factor:")) %>% 
    # remove location_type_factor: from term
    mutate(term = str_remove(term, "location_type_factor:")) %>% 
    # remove event_time from term, and then convert term to numeric
    mutate(term = as.numeric(str_remove(term, "event_time"))) %>%
    # rename term to event_time
    rename(event_time = term) %>% 
    mutate(event_time = as.numeric(event_time))
    
  
# plot with even_time on x axis, estimate on y axis, 2 standard errors as error bars,
# and location_type as color
ggplot(event_study_rings_blck_pop_tidy, aes(x = event_time, y = estimate,
                                            color = location_type)) +
    geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                  width = 0.2, alpha = 0.4) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Effect of Public Housing on Black Population",
         subtitle = "",
         x = "Years Relative to Treatment",
         y = "Difference in Difference Estimate") +
    # show x-axis labels every ten years from -40 to 60
    scale_x_continuous(breaks = seq(-40, 60, 10)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    # change legend title
    guides(color = guide_legend(title = "Location Type")) +
    # change legend labels
    scale_color_manual(values = c("inner" = "blue", "treated" = "red"),
                       labels = c("Inner Ring", "Treated")) +
  theme(plot.background = element_rect(fill = "white"))

# output graph
ggsave(paste0(did_output_dir, "event_study_rings_blck_pop.png"), width = 8, height = 6, units = "in")

# white pop

event_study_rings_wht_pop = feols(ln_pop_wht ~ location_type_factor*event_time | treated_id*year + TRACTA,
                                 data = event_study_data_rings, cluster = "treated_id")
  
event_study_rings_wht_pop_tidy <-
    broom::tidy(event_study_rings_wht_pop) %>% 
    # round all numeric variables to 4 digits
    mutate_if(is.numeric, round, 4) %>%
    # create a variable called "location_type" equal to "inner" if term contains "inner"
    mutate(location_type = case_when(str_detect(term, "inner") ~ "inner",
                                     str_detect(term, "treated") ~ "treated")) %>% 
    # remove treated and inner from term
    mutate(term = str_remove(term, "inner|treated")) %>%
    # keep if term contains "location_type_factor:"
    filter(str_detect(term, "location_type_factor:")) %>% 
    # remove location_type_factor: from term
    mutate(term = str_remove(term, "location_type_factor:")) %>% 
    # remove event_time from term, and then convert term to numeric
    mutate(term = as.numeric(str_remove(term, "event_time"))) %>%
    # rename term to event_time
    rename(event_time = term) %>% 
    mutate(event_time = as.numeric(event_time))
    
  
# plot with even_time on x axis, estimate on y axis, 2 standard errors as error bars,
# and location_type as color
ggplot(event_study_rings_wht_pop_tidy, aes(x = event_time, y = estimate,
                                          color = location_type)) +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                width = 0.2, alpha = 0.2) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Effect of Public Housing on White Population",
       subtitle = "",
       x = "Years Relative to Treatment",
       y = "Difference in Difference Estimate") +
  # show x-axis labels every ten years from -40 to 60
  scale_x_continuous(breaks = seq(-40, 60, 10)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  # change legend title
  guides(color = guide_legend(title = "Location Type")) +
  # change legend labels
  scale_color_manual(values = c("inner" = "blue", "treated" = "red"),
                     labels = c("Inner Ring", "Treated")) +
  theme(plot.background = element_rect(fill = "white"))

# output graph
ggsave(paste0(did_output_dir, "event_study_rings_wht_pop.png"), width = 8, height = 6, units = "in")


## Repeat 2. above exlcuding SF, St Louis, Baltimore, Boston, and Philadelphia -----
event_study_data_rings_sample <- 
  event_study_final %>%
  mutate(event_time = as_factor(year - treatment_year)) %>%
  mutate(ring = as.factor(case_when(location_type == "treated" ~ 0,
                                    location_type == "inner" ~ 1,
                                    location_type == "outer" ~ 2)),
         location_type_factor = factor(location_type, levels = c("outer", "treated", "inner"))) %>% 
  filter(!(COUNTY %in% c(sanfran_counties, stlouis_counties, baltimore_counties, boston_counties, philadelphia_counties)))

# get list of 

event_study_rings_blk_sh_sample = feols(blck_sh ~ location_type_factor*event_time | treated_id*year + TRACTA,
                                 data = event_study_data_rings_sample, cluster = "treated_id")

event_study_rings_blk_sh_tidy_sample <-
  broom::tidy(event_study_rings_blk_sh_sample) %>% 
  # round all numeric variables to 4 digits
  mutate_if(is.numeric, round, 4) %>%
  # create a variable called "location_type" equal to "inner" if term contains "inner"
  mutate(location_type = case_when(str_detect(term, "inner") ~ "inner",
                                   str_detect(term, "treated") ~ "treated")) %>% 
  # remove treated and inner from term
  mutate(term = str_remove(term, "inner|treated")) %>%
  # keep if term contains "location_type_factor:"
  filter(str_detect(term, "location_type_factor:")) %>% 
  # remove location_type_factor: from term
  mutate(term = str_remove(term, "location_type_factor:")) %>% 
  # remove event_time from term, and then convert term to numeric
  mutate(term = as.numeric(str_remove(term, "event_time"))) %>%
  # rename term to event_time
  rename(event_time = term) %>% 
  mutate(event_time = as.numeric(event_time))


# plot with even_time on x axis, estimate on y axis, 2 standard errors as error bars,
# and location_type as color
ggplot(event_study_rings_blk_sh_tidy_sample, aes(x = event_time, y = estimate,
                                          color = location_type)) +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                width = 0.2, alpha = 0.4) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Effect of Public Housing on Black Share of Population",
       subtitle = "",
       x = "Years Relative to Treatment",
       y = "Difference in Difference Estimate") +
  # show x-axis labels every ten years from -40 to 60
  scale_x_continuous(breaks = seq(-40, 60, 10)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  # change legend title
  guides(color = guide_legend(title = "Location Type")) +
  # change legend labels
  scale_color_manual(values = c("inner" = "blue", "treated" = "red"),
                     labels = c("Inner Ring", "Treated")) +
  theme(plot.background = element_rect(fill = "white"))

# output graph
ggsave(paste0(did_output_dir, "event_study_rings_blk_sh_sample.png"),
       width = 8, height = 6, units = "in", background = "white")

# white share 
event_study_rings_wht_sh_sample = feols(wht_shr ~ location_type_factor*event_time | treated_id*year + TRACTA,
                                 data = event_study_data_rings_sample, cluster = "treated_id")

event_study_rings_wht_sh_tidy_sample <-
  broom::tidy(event_study_rings_wht_sh_sample) %>% 
  # round all numeric variables to 4 digits
  mutate_if(is.numeric, round, 4) %>%
  # create a variable called "location_type" equal to "inner" if term contains "inner"
  mutate(location_type = case_when(str_detect(term, "inner") ~ "inner",
                                   str_detect(term, "treated") ~ "treated")) %>% 
  # remove treated and inner from term
  mutate(term = str_remove(term, "inner|treated")) %>%
  # keep if term contains "location_type_factor:"
  filter(str_detect(term, "location_type_factor:")) %>% 
  # remove location_type_factor: from term
  mutate(term = str_remove(term, "location_type_factor:")) %>% 
  # remove event_time from term, and then convert term to numeric
  mutate(term = as.numeric(str_remove(term, "event_time"))) %>%
  # rename term to event_time
  rename(event_time = term) %>% 
  mutate(event_time = as.numeric(event_time))


# plot with even_time on x axis, estimate on y axis, 2 standard errors as error bars,
# and location_type as color

ggplot(event_study_rings_wht_sh_tidy_sample, aes(x = event_time, y = estimate,
                                          color = location_type)) +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                width = 0.2, alpha = 0.4) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Effect of Public Housing on White Share of Population",
       subtitle = "",
       x = "Years Relative to Treatment",
       y = "Difference in Difference Estimate") +
  # show x-axis labels every ten years from -40 to 60
  scale_x_continuous(breaks = seq(-40, 60, 10)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  # change legend title
  guides(color = guide_legend(title = "Location Type")) +
  # change legend labels
  scale_color_manual(values = c("inner" = "blue", "treated" = "red"),
                     labels = c("Inner Ring", "Treated")) +
  theme(plot.background = element_rect(fill = "white"))

# output graph
ggsave(paste0(did_output_dir, "event_study_rings_wht_sh_sample.png"),
       width = 8, height = 6, units = "in", background = "white")

#  log total population
event_study_rings_tot_pop_sample = feols(ln_pop_tot ~ location_type_factor*event_time | treated_id*year + TRACTA,
                                 data = event_study_data_rings_sample, cluster = "treated_id")

event_study_rings_tot_pop_tidy_sample <-
  broom::tidy(event_study_rings_tot_pop_sample) %>% 
  # round all numeric variables to 4 digits
  mutate_if(is.numeric, round, 4) %>%
  # create a variable called "location_type" equal to "inner" if term contains "inner"
  mutate(location_type = case_when(str_detect(term, "inner") ~ "inner",
                                   str_detect(term, "treated") ~ "treated")) %>% 
  # remove treated and inner from term
  mutate(term = str_remove(term, "inner|treated")) %>%
  # keep if term contains "location_type_factor:"
  filter(str_detect(term, "location_type_factor:")) %>% 
  # remove location_type_factor: from term
  mutate(term = str_remove(term, "location_type_factor:")) %>% 
  # remove event_time from term, and then convert term to numeric
  mutate(term = as.numeric(str_remove(term, "event_time"))) %>%
  # rename term to event_time
  rename(event_time = term) %>% 
  mutate(event_time = as.numeric(event_time))


# plot with even_time on x axis, estimate on y axis, 2 standard errors as error bars,
# and location_type as color

ggplot(event_study_rings_tot_pop_tidy_sample, aes(x = event_time, y = estimate,
                                          color = location_type)) +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                width = 0.2, alpha = 0.4) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Effect of Public Housing on Total Population",
       subtitle = "",
       x = "Years Relative to Treatment",
       y = "Difference in Difference Estimate") +
  # show x-axis labels every ten years from -40 to 60
  scale_x_continuous(breaks = seq(-40, 60, 10)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  # change legend title
  guides(color = guide_legend(title = "Location Type")) +
  # change legend labels
  scale_color_manual(values = c("inner" = "blue", "treated" = "red"),
                     labels = c("Inner Ring", "Treated")) +
  theme(plot.background = element_rect(fill = "white"))

# output graph
ggsave(paste0(did_output_dir, "event_study_rings_tot_pop_sample.png"),
       width = 8, height = 6, units = "in", background = "white")





# Balance tables -----
 
# Calculate averagevalues of variables for treated, inner ring, and outer ring tracts

## Create a function to calculate average values of variables for treated and control tracts ----

balance_table <- function(data, var, group_var) {
  data %>% 
    group_by(!!sym(group_var)) %>% 
    summarise(mean = mean(!!sym(var), na.rm = TRUE),
              sd = sd(!!sym(var), na.rm = TRUE),
              n = n()) %>% 
    mutate(var = var) 
}

## Balance test for full (stacked) sample -----
# Compare black share, white share, total pop and black pop for treated, inner ring, and outer ring tracts before treatment (event_time == -10)
pretreatment_means_by_location <-
  balance_table(event_study_data_rings %>% filter(event_time == -10), "blck_sh", "location_type_factor") %>% 
  bind_rows(balance_table(event_study_data_rings %>% filter(event_time == -10), "wht_shr", "location_type_factor")) %>% 
  bind_rows(balance_table(event_study_data_rings %>% filter(event_time == -10, totl_pp != 0), "ln_pop_tot", "location_type_factor")) %>% 
  bind_rows(balance_table(event_study_data_rings %>% filter(event_time == -10, blck_pp != 0), "ln_pop_blk", "location_type_factor")) %>% 
  select(-sd, -n) %>% 
  # reshape wide on var
  pivot_wider(names_from = var, values_from = mean) %>%
  # rename variables
  rename(black_share = blck_sh,
         white_share = wht_shr,
         ln_total_pop = ln_pop_tot,
         ln_black_pop = ln_pop_blk) %>% 
  # rearrange to show treated first, then inner, then outer
  mutate(location_type_factor = factor(location_type_factor, levels = c("treated", "inner", "outer"))) %>%
  arrange(location_type_factor)
  
pretreatment_means_by_location

## Repeat this individually for each city ----

### define cities ----
# New York City
event_study_data_rings_nyc <-
  event_study_data_rings %>% 
  filter(COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond"), 
         STATE == "New York")
# Chicago
event_study_data_rings_chi <-
  event_study_data_rings %>% 
  filter(COUNTY == "Cook", STATE == "Illinois")


# Detroit
event_study_data_rings_det <-
  event_study_data_rings %>% 
  filter(COUNTY == "Wayne", STATE == "Michigan")

# Cleveland
event_study_data_rings_cle <-
  event_study_data_rings %>% 
  filter(COUNTY == "Cuyahoga", STATE == "Ohio")


# St. Louis
event_study_data_rings_stl <-
  event_study_data_rings %>% 
  filter(COUNTY == "St Louis City", STATE == "Missouri")

# Philadelphia
event_study_data_rings_phl <-
  event_study_data_rings %>% 
  filter(COUNTY == "Philadelphia", STATE == "Pennsylvania")

# Boston
event_study_data_rings_bos <-
  event_study_data_rings %>% 
  filter(COUNTY == "Suffolk", STATE == "Massachusetts")

# Baltimore
event_study_data_rings_bal <-
  event_study_data_rings %>% 
  filter(COUNTY == "Baltimore City", STATE == "Maryland")


# Pittsburgh
event_study_data_rings_pit <-
  event_study_data_rings %>% 
  filter(COUNTY == "Allegheny", STATE == "Pennsylvania")

# Cincinnati
event_study_data_rings_cin <-
  event_study_data_rings %>% 
  filter(COUNTY == "Hamilton", STATE == "Ohio")

# San Francisco
event_study_data_rings_sfo <-
  event_study_data_rings %>% 
  filter(COUNTY == "San Francisco", STATE == "California") %>% 
  # filter out islands off the cooast ("Farallon Islands" and "Treasure Island")
  filter(TRACTA !=  "060400")

### function that takes a city df as an argument and returns a balance table for that city -----
balance_table_city <- function(data) {
  data %>% 
    filter(event_time == -10) %>% 
    balance_table("blck_sh", "location_type_factor") %>% 
    bind_rows(balance_table(data %>% filter(event_time == -10), "wht_shr", "location_type_factor")) %>% 
    bind_rows(balance_table(data %>% filter(event_time == -10, totl_pp != 0), "ln_pop_tot", "location_type_factor")) %>%
    bind_rows(balance_table(data %>% filter(event_time == -10, blck_pp != 0), "ln_pop_blk", "location_type_factor")) %>%
    select(-sd, -n) %>%
    # reshape wide on var
    pivot_wider(names_from = var, values_from = mean) %>%
    # rename variables
    rename(black_share = blck_sh,
           white_share = wht_shr,
           ln_total_pop = ln_pop_tot,
           ln_black_pop = ln_pop_blk) %>%
    # rearrange
    mutate(location_type_factor = factor(location_type_factor, levels = c("treated", "inner", "outer"))) %>%
    arrange(location_type_factor)
}


### Create balance tables for each city ----
pretreatment_means_by_location_nyc <- balance_table_city(event_study_data_rings_nyc)
pretreatment_means_by_location_chi <- balance_table_city(event_study_data_rings_chi)
pretreatment_means_by_location_det <- balance_table_city(event_study_data_rings_det)
pretreatment_means_by_location_cle <- balance_table_city(event_study_data_rings_cle)
pretreatment_means_by_location_stl <- balance_table_city(event_study_data_rings_stl)
pretreatment_means_by_location_phl <- balance_table_city(event_study_data_rings_phl)
pretreatment_means_by_location_bos <- balance_table_city(event_study_data_rings_bos)
pretreatment_means_by_location_bal <- balance_table_city(event_study_data_rings_bal)
pretreatment_means_by_location_pit <- balance_table_city(event_study_data_rings_pit)
pretreatment_means_by_location_cin <- balance_table_city(event_study_data_rings_cin)
pretreatment_means_by_location_sfo <- balance_table_city(event_study_data_rings_sfo)

# Create table with all of these balance tables
pretreatment_means_by_location_all <-
  pretreatment_means_by_location_nyc %>% 
  bind_rows(pretreatment_means_by_location_chi) %>% 
  bind_rows(pretreatment_means_by_location_det) %>% 
  bind_rows(pretreatment_means_by_location_cle) %>% 
  bind_rows(pretreatment_means_by_location_stl) %>% 
  bind_rows(pretreatment_means_by_location_phl) %>% 
  bind_rows(pretreatment_means_by_location_bos) %>% 
  bind_rows(pretreatment_means_by_location_bal) %>% 
  bind_rows(pretreatment_means_by_location_pit) %>% 
  bind_rows(pretreatment_means_by_location_cin) %>% 
  bind_rows(pretreatment_means_by_location_sfo) %>% 
  mutate(city = c(rep("New York City", 3), rep("Chicago", 3), rep("Detroit", 3), rep("Cleveland", 3), rep("St. Louis", 3), rep("Philadelphia", 3), rep("Boston", 3), rep("Baltimore", 3), rep("Pittsburgh", 3), rep("Cincinnati", 3), rep("San Francisco", 3))) %>% 
  mutate(city = factor(city, levels = c("New York City", "Chicago", "Detroit", "Cleveland", "St. Louis", "Philadelphia", "Boston", "Baltimore", "Pittsburgh", "Cincinnati", "San Francisco"))) %>% 
  select(city, location_type_factor, everything())

# Run synthetic control -----





# City-by-city maps of public housing locations against black population share in 1950 ----
# list of cities: NYC, Chicago, Detroit, Cleveland, St. Louis, Philadelphia,
#                 Boston, Baltimore, Pittsburgh, Cincinnati, San Francisco, Los Angeles

# TODO: I could do this in a loop probably, as above

### New York City ----

nyc_census_tract_data <-
  census_tract_data %>% 
  filter(YEAR == 1950, 
         COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond"), 
                       STATE == "New York")
           
nyc_map <-
  ggplot() +
  geom_sf(data = nyc_census_tract_data , aes(fill = blck_sh)) +  # Plot Census tracts with shading
  geom_sf(data = nyc_census_tract_data, fill = NA, color = "black", size = 0.3) +  # Draw outlines
  geom_sf(data = cdd_projects_geolocated_sf %>% filter(locality == "NEW YORK CITY"), color = "red", size = 0.1) +  # Plot public housing as red dots
scale_fill_distiller(direction = 1, palette = 4) + 
  theme_minimal()+ 
  labs(fill = "Black Share, 1950")  # Label for the legend

agg_png(filename = paste0("output/figures/nyc_map.png"), width = 1000, height = 500)
plot(nyc_map)
invisible(dev.off())

### Chicago ----

chicago_census_tract_data <-
  census_tract_data %>% 
  filter(YEAR == 1950, 
         COUNTY %in% c("Cook"), 
         STATE == "Illinois")

chicago_map <-
  ggplot() +
  geom_sf(data = chicago_census_tract_data , aes(fill = blck_sh)) +  # Plot Census tracts with shading
  geom_sf(data = chicago_census_tract_data, fill = NA, color = "black", size = 0.3) +  # Draw outlines
  geom_sf(data = cdd_projects_geolocated_sf %>% filter(locality == "CHICAGO"), color = "red", size = 0.1) +  # Plot public housing as red dots
scale_fill_distiller(direction = 1, palette = 4) + 
  theme_minimal()+ 
  labs(fill = "Black Share, 1950")  # Label for the legend

agg_png(filename = paste0("output/figures/chicago_map.png"), width = 1000, height = 500)
plot(chicago_map)
invisible(dev.off())


### Philadelphia ----

philadelphia_census_tract_data <-
  census_tract_data %>% 
  filter(YEAR == 1950, 
         COUNTY %in% c("Philadelphia"), 
         STATE == "Pennsylvania")
  
philadelphia_map <-
  ggplot() +
  geom_sf(data = philadelphia_census_tract_data , aes(fill = blck_sh)) +  # Plot Census tracts with shading
  geom_sf(data = philadelphia_census_tract_data, fill = NA, color = "black", size = 0.3) +  # Draw outlines
  geom_sf(data = cdd_projects_geolocated_sf %>% filter(locality == "PHILADELPHIA"), color = "red", size = 0.2) +  # Plot public housing as red dots
scale_fill_distiller(direction = 1, palette = 4) + 
  theme_minimal()+ 
  labs(fill = "Black Share, 1950")  # Label for the legend

agg_png(filename = paste0("output/figures/philadelphia_map.png"), width = 1000, height = 500)
plot(philadelphia_map)
invisible(dev.off())


### Detroit ----

detroit_census_tract_data <-
  census_tract_data %>% 
  filter(YEAR == 1950, 
         COUNTY %in% c("Wayne"), 
         STATE == "Michigan")

detroit_map <-
  ggplot() +
  geom_sf(data = detroit_census_tract_data , aes(fill = blck_sh)) +  # Plot Census tracts with shading
  geom_sf(data = detroit_census_tract_data, fill = NA, color = "black", size = 0.3) +  # Draw outlines
  geom_sf(data = cdd_projects_geolocated_sf %>% filter(locality == "DETROIT", state == "MICHIGAN"), color = "red", size = 0.2) +  # Plot public housing as red dots
scale_fill_distiller(direction = 1, palette = 4) + 
  theme_minimal()+ 
  labs(fill = "Black Share, 1950")  # Label for the legend

agg_png(filename = paste0("output/figures/detroit_map.png"), width = 1000, height = 500)
plot(detroit_map)
invisible(dev.off())


### Baltimore ----

baltimore_census_tract_data <-
  census_tract_data %>% 
  filter(YEAR == 1950, 
         COUNTY %in% c("Baltimore City"), 
         STATE == "Maryland")

baltimore_map <-
  ggplot() +
  geom_sf(data = baltimore_census_tract_data , aes(fill = blck_sh)) +  # Plot Census tracts with shading
  geom_sf(data = baltimore_census_tract_data, fill = NA, color = "black", size = 0.3) +  # Draw outlines
  geom_sf(data = cdd_projects_geolocated_sf %>% filter(locality == "BALTIMORE CITY"), color = "red", size = 0.5) +  # Plot public housing as red dots
scale_fill_distiller(direction = 1, palette = 4) + 
  theme_minimal()+ 
  labs(fill = "Black Share, 1950")  # Label for the legend

agg_png(filename = paste0("output/figures/baltimore_map.png"), width = 1000, height = 500)
plot(baltimore_map)
invisible(dev.off())


### Cleveland ----

cleveland_census_tract_data <-
  census_tract_data %>% 
  filter(YEAR == 1950, 
         COUNTY %in% c("Cuyahoga"), 
         STATE == "Ohio")

cleveland_map <-
  ggplot() +
  geom_sf(data = cleveland_census_tract_data , aes(fill = blck_sh)) +  # Plot Census tracts with shading
  geom_sf(data = cleveland_census_tract_data, fill = NA, color = "black", size = 0.5) +  # Draw outlines
  geom_sf(data = cdd_projects_geolocated_sf %>% filter(locality == "CLEVELAND-CUYAHOGA METROPOLITAN"),
          color = "red", size = .5) +  # Plot public housing as red dots
  scale_fill_distiller(direction = 1, palette = 4) + 
  theme_minimal()+ 
  labs(fill = "Black Share, 1950")  # Label for the legend

agg_png(filename = paste0("output/figures/cleveland_map.png"), width = 1000, height = 500)
plot(cleveland_map)
invisible(dev.off())


### St. Louis ----

stlouis_census_tract_data <-
  census_tract_data %>% 
  filter(YEAR == 1950, 
         COUNTY %in% c("St Louis City"), 
         STATE == "Missouri")

stlouis_map <-
  ggplot() +
  geom_sf(data = stlouis_census_tract_data , aes(fill = blck_sh)) +  # Plot Census tracts with shading
  geom_sf(data = stlouis_census_tract_data, fill = NA, color = "black", size = 0.3) +  # Draw outlines
  geom_sf(data = cdd_projects_geolocated_sf %>% filter(locality == "ST LOUIS CITY"), color = "red", size = 0.5) +  # Plot public housing as red dots
scale_fill_distiller(direction = 1, palette = 4) + 
  theme_minimal()+ 
  labs(fill = "Black Share, 1950")  # Label for the legend

agg_png(filename = paste0("output/figures/stlouis_map.png"), width = 1000, height = 500)
plot(stlouis_map)
invisible(dev.off())


### Pittsburgh ----

pittsburgh_census_tract_data <-
  census_tract_data %>% 
  filter(YEAR == 1950, 
         COUNTY %in% c("Allegheny"), 
         STATE == "Pennsylvania")

pittsburgh_map <-
  ggplot() +
  geom_sf(data = pittsburgh_census_tract_data , aes(fill = blck_sh)) +  # Plot Census tracts with shading
  geom_sf(data = pittsburgh_census_tract_data, fill = NA, color = "black", size = 0.3) +  # Draw outlines
  geom_sf(data = cdd_projects_geolocated_sf %>% filter(locality == "PITTSBURGH"), color = "red", size = 0.5) +  # Plot public housing as red dots
scale_fill_distiller(direction = 1, palette = 4) + 
  theme_minimal()+ 
  labs(fill = "Black Share, 1950")  # Label for the legend

agg_png(filename = paste0("output/figures/pittsburgh_map.png"), width = 1000, height = 500)
plot(pittsburgh_map)
invisible(dev.off())


### Boston ----

boston_census_tract_data <-
  census_tract_data %>% 
  filter(YEAR == 1950, 
         COUNTY %in% c("Suffolk", "Middlesex"), 
         STATE == "Massachusetts")

boston_map <-
  ggplot() +
  geom_sf(data = boston_census_tract_data , aes(fill = blck_sh)) +  # Plot Census tracts with shading
  geom_sf(data = boston_census_tract_data, fill = NA, color = "black", size = 0.3) +  # Draw outlines
  geom_sf(data = cdd_projects_geolocated_sf %>% 
            filter((locality == "BOSTON" | locality == "CAMBRIDGE"), state == "MASSACHUSETTS"),
          color = "red", size = 0.5) +  # Plot public housing as red dots
scale_fill_distiller(direction = 1, palette = 4) + 
  theme_minimal()+ 
  labs(fill = "Black Share, 1950")  # Label for the legend

agg_png(filename = paste0("output/figures/boston_map.png"), width = 1000, height = 500)
plot(boston_map)
invisible(dev.off())

### Cincinnati ----

cincinnati_census_tract_data <-
  census_tract_data %>% 
  filter(YEAR == 1950, 
         COUNTY %in% c("Hamilton"), 
         STATE == "Ohio")

cincinnati_map <-
  ggplot() +
  geom_sf(data = cincinnati_census_tract_data , aes(fill = blck_sh)) +  # Plot Census tracts with shading
  geom_sf(data = cincinnati_census_tract_data, fill = NA, color = "black", size = 0.3) +  # Draw outlines
  geom_sf(data = cdd_projects_geolocated_sf %>% filter(locality == "CINCINNATI"), color = "red", size = 0.5) +  # Plot public housing as red dots
  scale_fill_distiller(direction = 1, palette = 4) +
  theme_minimal()+
  labs(fill = "Black Share, 1950")  # Label for the legend

agg_png(filename = paste0("output/figures/cincinnati_map.png"), width = 1000, height = 500)
plot(cincinnati_map)
invisible(dev.off())

### San Francisco ----

sanfran_census_tract_data <-
  census_tract_data %>% 
  filter(YEAR == 1950, 
         COUNTY %in% c("San Francisco"), 
         STATE == "California") %>%
  # filter out islands off the cooast ("Farallon Islands" and "Treasure Island")
  filter(TRACTA !=  "060400")


sanfran_map <-
  ggplot() +
  geom_sf(data = sanfran_census_tract_data , aes(fill = blck_sh))  +  
  # Plot Census tracts with shading
  geom_sf(data = sanfran_census_tract_data, fill = NA, color = "black", size = 0.3) +  # Draw outlines
  geom_sf(data = cdd_projects_geolocated_sf %>% filter(locality == "SAN FRANCISCO"), color = "red", size = 0.5) +  # Plot public housing as red dots
  scale_fill_distiller(direction = 1, palette = 4) +
  theme_minimal()+
  labs(fill = "Black Share, 1950")  # Label for the legend

agg_png(filename = paste0("output/figures/sanfran_map.png"), width = 1000, height = 500)
plot(sanfran_map)
invisible(dev.off())
