#####
# Analysis for brownbag, 03/2024
######

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

# directory for regression output
did_output_dir <- "output/regression_results/brownbag_202402/"


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

#### geolocation ----
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

#### Create an estimate of population of project ---- 

# Define treated tracts, inner rings, and outer rings via spatial join ---- 
### Get only cities in our sample ----
# list of cities: NYC, Chicago, Detroit, Cleveland, St. Louis, Philadelphia, 
#                 Boston, Baltimore, Pittsburgh, Cincinnati, Washington DC
#                 

cdd_project_sample <-
  cdd_projects_geolocated_sf %>% 
  filter(locality == "NEW YORK CITY"|
           locality == "CHICAGO" | 
           locality == "DETROIT" & state == "MICHIGAN" |
           locality == "CLEVELAND-CUYAHOGA METROPOLITAN" |
           locality == "PHILADELPHIA" |
           locality == "BALTIMORE CITY" |
           locality == "PITTSBURGH" |
           locality == "CINCINNATI-CINCINNATI METROPOLITAN" | 
           locality == "WASHINGTON DC-NATIONAL CAPITOL") %>% 
  # only keep if yearfullocc is not misisng
  filter(!is.na(yearfullocc))

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
           # philadelphia
           (COUNTY %in% ("Philadelphia") & STATE == "Pennsylvania") |
           # baltimore
           (COUNTY %in% ("Baltimore City") & STATE == "Maryland") | 
           # pittsburgh
           (COUNTY %in% ("Allegheny") & STATE == "Pennsylvania") |
           # cincinnati
           (COUNTY %in% ("Hamilton") & STATE == "Ohio") |
           # washington dc
           (STATE == "District Of Columbia")) %>% 
  # create an variable called "city"
  mutate(city = case_when(
    COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York" ~ "New York City",
    COUNTY %in% c("Cook") & STATE == "Illinois" ~ "Chicago",
    COUNTY %in% c("Wayne") & STATE == "Michigan" ~ "Detroit",
    COUNTY %in% c("Cuyahoga") & STATE == "Ohio" ~ "Cleveland",
    COUNTY %in% ("Philadelphia") & STATE == "Pennsylvania" ~ "Philadelphia",
    COUNTY %in% ("Baltimore City") & STATE == "Maryland" ~ "Baltimore",
    COUNTY %in% ("Allegheny") & STATE == "Pennsylvania" ~ "Pittsburgh",
    COUNTY %in% ("Hamilton") & STATE == "Ohio" ~ "Cincinnati",
    STATE == "District Of Columbia" ~ "Washington DC"
  ))
  


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

# 2/15/2024: Assume "treatment" happens once: So for each treated tract:
# 1. Sum up the total units planned for the tract in each yearfullocc (in case there's multiple projects planned)
# 2. Keep only the first instance of each year
# sort by yearfullocc and tract,
# group by state, county, tracta, year
# and keep only the first instance for each year

treated_tracts <-
  treated_tracts %>% 
  group_by(STATE, COUNTY, TRACTA, YEAR, yearfullocc) %>% 
  mutate(totunitsplanned = sum(totunitsplanned, na.rm = TRUE)) %>%
  arrange(STATE, COUNTY, TRACTA, yearfullocc) %>% 
  group_by(STATE, COUNTY, TRACTA, YEAR) %>%
  filter(row_number() == 1) %>%
  ungroup()
  
# get panel of treated tracts
treated_tracts_panel <-
  treated_tracts %>% 
  # keep if census year >= treatment year
  filter(YEAR >= treatment_year) %>% 
  st_drop_geometry() %>% 
  select(tract_id, TRACTA, COUNTY, STATE, YEAR, treatment_year, totunitsplanned) %>%
  mutate(treated = 1)

# # get totunitsplanned for each tract
tot_units_planned_per_tract <-
  treated_tracts_panel %>%
  select(TRACTA, COUNTY, STATE, totunitsplanned) %>% 
  distinct()


# Create, in each year, the treated, inner, and outer rings
# all neighbors among sample tracts

# 0. Choose 1990 as year to calculate neighbors. 
census_tract_sample_indexed_1990 <-
  census_tract_sample_indexed %>%
  filter(YEAR == 1990)

# get 1990 crosswalk
census_tract_1990_crosswalk <-
  census_tract_sample_indexed_1990 %>% 
  mutate(row_number = row_number()) %>%
  select(row_number, tract_id, TRACTA, COUNTY, STATE) 

tract_neighbors <- 
  spdep::poly2nb(census_tract_sample_indexed_1990) %>% 
  nb2mat(zero.policy = TRUE, style = "B") %>% 
  as_tibble() %>% 
  mutate(from = row_number()) %>%
  pivot_longer(cols = -from, names_to = "to", values_to = "neighbor") %>% 
  # remove the Vs from "to" column and convert to numeric
  mutate(to = as.numeric(gsub("V", "", to))) %>% 
  # Merge TRACTA, COUNTY, STATEA onto "from" of tract_neighbors, to merge to others later
  left_join(census_tract_1990_crosswalk, by = c("from" = "row_number")) %>% 
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
#    get, for each treated tract, the row_number in census_tract_sample_indexed_1990
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
  left_join(census_tract_1990_crosswalk, by = c("to" = "row_number")) %>% 
  dplyr::rename(TRACT_to = TRACTA, COUNTY_to = COUNTY,
                STATE_to = STATE) %>%
  mutate(inner_ring = 1) %>% 
  st_drop_geometry() %>% 
  distinct()

# 1. take neighbors with from in the list of "to" (in inner rings). This will get the list of neighbors for all of the inner ring tracts, in all years
# 1. get distinct list
# 2. left_join onto the inner rings to get year pairs from (in outer_ring) to to (in inner_ring)
# 3. I think I am now missing the correct geometry for the new "to"... merge that on via the crosswalk
outer_rings_panel <-  
  tract_neighbors %>% 
  st_drop_geometry() %>% 
  filter(from %in% unique(inner_rings_panel$to)) %>% 
  filter(neighbor == 1) %>% 
  distinct() %>%
  right_join(inner_rings_panel, by = c("from" = "to")) %>% 
  select(YEAR, from, to) %>% 
  left_join(census_tract_1990_crosswalk, by = c("to" = "row_number")) %>% 
  mutate(outer_ring = 1) %>%
  st_drop_geometry() %>% 
  distinct()


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
  left_join(census_tract_1990_crosswalk, by = c("inner_ring_id" = "row_number")) %>% 
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

year <- c(1930, 1940, 1950, 1960, 1970, 1980, 1990)

# all combinations of x and y 
event_study_data_full <-
  expand_grid(tracts_and_rings, year) 

### 2. Reshape long and merge on census data ----

# reshape to long 
event_study_long <- 
  event_study_data_full %>% 
  pivot_longer(cols = c(COUNTY_treated, STATE_treated, TRACTA_treated, 
                        COUNTY_inner, STATE_inner, TRACTA_inner, 
                        COUNTY_outer, STATE_outer, TRACTA_outer), 
               names_to = c(".value", "location_type"), 
                names_sep = "_")  %>% 
  # # merge on total units in each year
  left_join(tot_units_planned_per_tract) %>%
  # fill by year and treated_id
  group_by(year, treated_id) %>%
  fill(totunitsplanned) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(totunitsplanned = ifelse(is.na(totunitsplanned), 0, totunitsplanned))


census_data_for_event_study <-
  census_tract_sample %>% 
  select(STATE, COUNTY, TRACTA, YEAR, blck_sh, wht_shr, whit_pp, blck_pp, totl_pp, city) %>%
  st_drop_geometry() %>% 
  dplyr::rename(year = YEAR) %>% 
  # inverse hyperbolic sine transformation
  mutate(asinh_pop_tot = asinh(totl_pp),
         asinh_pop_wht = asinh(whit_pp),
         asinh_pop_blk = asinh(blck_pp))

event_study_final <-
  left_join(event_study_long, census_data_for_event_study) %>% 
  # define treated variable equal to 1 if year >= treatment year and location_type == treated
  mutate(treated = ifelse(year >= treatment_year & location_type == "treated", TRUE, FALSE)) %>%
  # create variables for sun abraham estimates
  mutate(cohort = as.factor(year), 
         relative_period = (year - treatment_year)/10) %>% 
  # drop any tracts with 0 population
  filter(totl_pp > 0) %>%
  # keep distinct observations: One observation per treated_id-tract-year
  distinct()

## Run event study ----
### 1. Stacked diff-in-diff with inner and outer rings ----

# I think here, I need a ring variable, so not just treated group, but ring
event_study_data_rings <- 
  event_study_final %>%
  mutate(event_time = as_factor(year - treatment_year)) %>%
  mutate(ring = as.factor(case_when(location_type == "treated" ~ 0,
                                    location_type == "inner" ~ 1,
                                    location_type == "outer" ~ 2)),
         location_type_factor = factor(location_type, levels = c("outer", "treated", "inner")),
         location_type_dummy = as.numeric(location_type_factor))



# function for running event study regressions
run_event_study_regression <- function(data, dep_var) {
  
  # Construct the formula dynamically
  formula <- as.formula(paste(dep_var, "~ location_type_factor * event_time",
                              "| treated_id * year + TRACTA + treated_id * location_type_dummy"))
  
  #formula <- as.formula(paste(dep_var, "~ location_type_factor * event_time",
   #                           "| treated_id * year + TRACTA + treated_id * location_type_dummy"))
  
  
  # Run the regression
  model <- feols(formula, data = data, cluster = "treated_id")
  

  # Tidy the model output and process it
  tidy_output <- broom::tidy(model) %>%
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
  
  return(tidy_output)
}


# function for plotting the event study graphs
create_event_study_plot <- function(reg_results_df, data, dep_var, title) {
  
  # calculate the t = 0 coefficient divided by the mean of the filtered_data variable in t = -10
  # and store it in a variable called "t10_coefficient"
  t10_coefficient <-
    reg_results_df %>%
    filter(event_time == 10, location_type == "treated") %>%
    pull(estimate) / mean(data %>% filter(location_type== "treated", event_time == -10) %>% pull(dep_var))
  
  ggplot(reg_results_df, aes(x = event_time, y = estimate, color = location_type)) +
    geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                  width = 0.2, alpha = 0.4, position = position_dodge(width = 1)) +
    geom_point(position = position_dodge(width = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title,
         subtitle = "",
         x = "Years Relative to Treatment",
         y = "Difference in Difference Estimate") +
    scale_x_continuous(breaks = seq(-40, 60, 10)) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.background = element_rect(fill = "white"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    guides(color = guide_legend(title = "Location Type")) +
    scale_color_brewer(palette = "Set1", labels = c("Inner Ring", "Treated")) +
      # add text at bottom of plot that says "t = 10 coefficient is t10_coefficient of the mean at t = 0"
    labs(caption = paste("Treated coefficient at t = 10 is", round(t10_coefficient, 2), "of the treated tract mean at t = -10")) 
  
  
  
}

#### run regressions ---- 
# 
# n <- event_study_data_rings %>% 
# # create numeric dummy for location type 
#   mutate(location_type_num = as.numeric(location_type_factor))
#   
# # Run the regression
# model <- feols(blck_sh~ location_type_factor * event_time |
#                treated_id * year + TRACTA +  treated_id * location_type_num, data = n,
#                cluster = "treated_id")
# 

event_study_rings_blk_sh_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "blck_sh")

event_study_rings_wht_sh_tidy <-
  run_event_study_regression(
  data = event_study_data_rings, dep_var = "wht_shr")

event_study_rings_tot_pop_tidy <-
  run_event_study_regression(
  data = event_study_data_rings, dep_var = "asinh_pop_tot")

event_study_rings_blk_pop_tidy <-
  run_event_study_regression(
  data = event_study_data_rings, dep_var = "asinh_pop_blk")

event_study_rings_wht_pop_tidy <-
  run_event_study_regression(
  data = event_study_data_rings, dep_var = "asinh_pop_wht")

# Create plots and output to pdf in the output directory

### produce graphs ----

#### 1. Full sample
pdf(paste0(did_output_dir, "ring_event_study_plots.pdf"), width = 10, height = 5)

# black share did plot
create_event_study_plot(event_study_rings_blk_sh_tidy, 
                        data = event_study_data_rings,
                        dep_var = "blck_sh", 
                        "Effect of Public Housing on Black Share of Population")
# white share did plot
create_event_study_plot(event_study_rings_wht_sh_tidy,
                        data = event_study_data_rings,
                        dep_var = "wht_shr",
                        "Effect of Public Housing on White Share of Population")
# total population did plot
create_event_study_plot(event_study_rings_tot_pop_tidy, 
                        data = event_study_data_rings,
                        dep_var = "asinh_pop_tot",
                        "Effect of Public Housing on (asinh) Total Population")
# black population did plot
create_event_study_plot(event_study_rings_blk_pop_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_pop_blk",
                        "Effect of Public Housing on (asinh) Black Population")
# white population did plot
create_event_study_plot(event_study_rings_wht_pop_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_pop_wht",
                        "Effect of Public Housing on (asinh) White Population") 

dev.off()

#### 2. Heterogeneity by size of public housing development ----
# Using "total units planned" 
x <- 
  event_study_data_rings %>%
  filter(location_type == "treated") %>%
  group_by(treated_id) %>% filter(row_number() == 1)

# get 25th, 50th and 75th percentiles of totunitsplanned
units_25p <- quantile(x$totunitsplanned, probs = 0.25)
units_50p <- quantile(x$totunitsplanned, probs = 0.5)
units_75p <- quantile(x$totunitsplanned, probs = 0.75)

# create a variable called "size" equal to "25th" if totunitsplanned is less than 25th percentile,
# "50th" if totunitsplanned is between 25th and 50th percentile, "75th" if totunitsplanned is between
# 50th and 75th percentile and "large" otherwise
event_study_data_rings <- event_study_data_rings %>%
  mutate(size = case_when(totunitsplanned < units_25p ~ "25th",
                          totunitsplanned >= units_25p & totunitsplanned < units_50p ~ "50th",
                          totunitsplanned >= units_50p & totunitsplanned < units_75p ~ "75th",
                          totunitsplanned >= units_75p ~ "75th_to_max"),
         size_bunch = case_when(totunitsplanned < units_50p ~ "small",
                                totunitsplanned >= units_50p ~ "large"))

# re-run event studies separately for each size

for(size_group in unique(event_study_data_rings$size)) {
  # Filter the dataset for the current size
  filtered_data <- event_study_data_rings %>% filter(size == size_group)
  
  pdf_file_name <- paste0(did_output_dir, "ring_event_study_plots_size_", size_group, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  # Define variables and titles for plotting
  variables <- c("blck_sh", "wht_shr", "asinh_pop_tot", "asinh_pop_blk", "asinh_pop_wht")
  titles <- c("Effect of Public Housing on Black Share of Population",
              "Effect of Public Housing on White Share of Population",
              "Effect of Public Housing on (asinh) Total Population",
              "Effect of Public Housing on (asinh) Black Population",
              "Effect of Public Housing on (asinh) White Population")
  
  for(i in seq_along(variables)) {
    # Assuming you have a modified version of your function that returns the tidy data for the given variable
    tidy_output <- run_event_study_regression(filtered_data, variables[i])
    
    # Create and print the plot
    plot <- create_event_study_plot(tidy_output,
                                    data = filtered_data,
                                    dep_var = variables[i],
                                    titles[i])

    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}

# repeat but combine into 2
for(size_group in unique(event_study_data_rings$size_bunch)) {
  # Filter the dataset for the current size
  filtered_data <- event_study_data_rings %>% filter(size_bunch == size_group)
  
  pdf_file_name <- paste0(did_output_dir, "ring_event_study_plots_size_", size_group, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  # Define variables and titles for plotting
  variables <- c("blck_sh", "wht_shr", "asinh_pop_tot", "asinh_pop_blk", "asinh_pop_wht")
  titles <- c("Effect of Public Housing on Black Share of Population",
              "Effect of Public Housing on White Share of Population",
              "Effect of Public Housing on (asinh) Total Population",
              "Effect of Public Housing on (asinh) Black Population",
              "Effect of Public Housing on (asinh) White Population")
  
  for(i in seq_along(variables)) {
    # Assuming you have a modified version of your function that returns the tidy data for the given variable
    tidy_output <- run_event_study_regression(filtered_data, variables[i])
    
    # Create and print the plot
    plot <- create_event_study_plot(tidy_output,
                                    data = filtered_data,
                                    dep_var = variables[i],
                                    titles[i])
    
    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}



#### 3. Heterogeneity by pre-treatment racial composition of the treated tract -----

# get pre-treatment black shares: 
y <- 
  event_study_data_rings %>%
  filter(location_type == "treated") %>%
  filter(event_time == -10)

# get quantiles of pre-treatment black shares
blk_sh_25p <- quantile(y$blck_sh, probs = 0.25)
blk_sh_50p <- quantile(y$blck_sh,  probs = 0.5)
blk_sh_75p <- quantile(y$blck_sh, probs = 0.75)

# create a variable called "pre_treatment_blk_sh" equal to "25th" if pre-treatment
# black share of the treated tract is less than 25th percentile, "50th" if pre-treatment black share is between
# 25th and 50th percentile, "75th" if pre-treatment black share is between 50th and 75th
# percentile and "high" otherwise
pre_treatment_blk_sh <-
  event_study_data_rings %>% 
  filter(event_time == -10, location_type == "treated") %>% 
  select(blck_sh, treated_id) %>%
  dplyr::rename(pre_treatment_blk_sh_treated = blck_sh)

blk_sh_25p <- quantile(event_study_data_rings$pre_treatment_blk_sh_treated, probs = 0.25, na.rm = TRUE)
blk_sh_50p <- quantile(event_study_data_rings$pre_treatment_blk_sh_treated, probs = 0.5, na.rm = TRUE)
blk_sh_75p <- quantile(event_study_data_rings$pre_treatment_blk_sh_treated, probs = 0.75, na.rm = TRUE)


event_study_data_rings <- 
  left_join(event_study_data_rings, pre_treatment_blk_sh)  %>%
  mutate(pre_treatment_blk_sh = case_when(pre_treatment_blk_sh_treated < blk_sh_25p  ~ "low",
                                          pre_treatment_blk_sh_treated > blk_sh_25p & pre_treatment_blk_sh_treated < blk_sh_50p ~ "medium",
                                          pre_treatment_blk_sh_treated >= blk_sh_50p ~ "high"))


# re-run event studies separately for each pre-treatment black share group
for(race_group in unique(event_study_data_rings %>% filter(!is.na(pre_treatment_blk_sh)) %>% pull(pre_treatment_blk_sh))) {
  # Filter the dataset for the current size
  filtered_data <- event_study_data_rings %>% filter(pre_treatment_blk_sh == race_group)
  
  pdf_file_name <- paste0(did_output_dir, "ring_event_study_plots_by_blk_share_", race_group, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  # Define variables and titles for plotting
  variables <- c("blck_sh", "wht_shr", "asinh_pop_tot", "asinh_pop_blk", "asinh_pop_wht")
  titles <- c("Effect of Public Housing on Black Share of Population",
              "Effect of Public Housing on White Share of Population",
              "Effect of Public Housing on (asinh) Total Population",
              "Effect of Public Housing on (asinh) Black Population",
              "Effect of Public Housing on (asinh) White Population")
  
  for(i in seq_along(variables)) {
    # Assuming you have a modified version of your function that returns the tidy data for the given variable
    tidy_output <- run_event_study_regression(filtered_data, variables[i])
    
    # Create and print the plot
    plot <- create_event_study_plot(tidy_output,
                                    data = filtered_data,
                                    dep_var = variables[i],
                                    titles[i])
    
    # if the variable is a share, set the y-axis limits to be between -.25 and .25
    # if the variable is a population, set the y-axis limits to be between -2 and 2
    
    # if(i %in% c(1,2)) {
    #   plot <- plot + scale_y_continuous(limits = c(-.25,.25), breaks = seq(-.25, .25, .05))
    # } else {
    #   plot <- plot + scale_y_continuous(limits = c(-2.5,2.5), breaks = seq(-2, 2, .5))
    # }
    
    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}


#### 4. Heterogeneity by city -----
# loop through each of the cities in the dataset, run regressions separately
for(c in unique(event_study_data_rings$city)) {
  # Filter the dataset for the current size
  filtered_data <- event_study_data_rings %>% filter(city == c)
  
  pdf_file_name <- paste0(did_output_dir, "ring_event_study_plots_", c, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  # Define variables and titles for plotting
  variables <- c("blck_sh", "wht_shr", "asinh_pop_tot", "asinh_pop_blk", "asinh_pop_wht")
  titles <- c("Effect of Public Housing on Black Share of Population",
              "Effect of Public Housing on White Share of Population",
              "Effect of Public Housing on (asinh) Total Population",
              "Effect of Public Housing on (asinh) Black Population",
              "Effect of Public Housing on (asinh) White Population")
  
  for(i in seq_along(variables)) {
    # Assuming you have a modified version of your function that returns the tidy data for the given variable
    tidy_output <- run_event_study_regression(filtered_data, variables[i])
    
    # Create and print the plot
    plot <- create_event_study_plot(tidy_output,
                                    data = filtered_data,
                                    dep_var = variables[i],
                                    titles[i])
    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}




# Exploratory graphs ------

## Time series of project construction in my sample (histogram
cdd_project_sample %>%
  ggplot(aes(x = yearfullocc)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Public Housing Projects per Year",
       x = "Year",
       y = "Number of Projects") +
  # x axis every 5 years from 1940 to 1975
  scale_x_continuous(breaks = seq(1935, 1975, 5)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) 
  

ggsave(paste0(did_output_dir,"cdd_project_sample_time_series.png"), width = 8, height = 6, units = "in")

# time series of all projects
cdd_projects_geolocated_sf %>% 
  ggplot(aes(x = yearfullocc)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Date of Project Construction (all projects)",
       x = "Year",
       y = "Number of Projects") +
  # x axis every 5 years from 1940 to 1975
  scale_x_continuous(breaks = seq(1935, 1975, 5)) +
  theme_minimal()

# time series of total units in sample
cdd_project_sample %>%
  group_by(yearfullocc) %>% 
  summarise(total_units = sum(totunitsplanned, na.rm = TRUE)) %>%
  ggplot(aes(x = yearfullocc, y = total_units)) +
  geom_col(fill = "lightblue", color = "black") + 
  labs(title = "Public Housing Units Constructed Per Year",
       x = "Year",
       y = "Number of Public Housing Units") +
  # x axis every 5 years from 1940 to 1975
  scale_x_continuous(breaks = seq(1935, 1975, 5)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) 

ggsave(paste0(did_output_dir,"cdd_project_sample_units_time_series.png"), width = 8, height = 6, units = "in")
