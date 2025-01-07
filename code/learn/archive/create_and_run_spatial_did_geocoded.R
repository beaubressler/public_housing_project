####
# Spatial "inner and outer ring" DiD/event studies

# 1. Create inner ring and outer rings
# 2. 
####


### Right now, this is my main analysis file

# Preliminaries -----
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
did_output_dir <- "output/regression_results/stacked_did/digitized/"


# variables for event studies and names 
es_variables <- c("black_share", "white_share", "asinh_pop_total", "asinh_pop_black", "asinh_pop_white", 
                  "asinh_median_home_value", "asinh_median_rent", "asinh_median_income", "median_educ_years_25plus",
                  "unemp_rate", "lfp_rate", "employment_pop_ratio")
es_titles <- c("Effect of Public Housing on Black Share of Population",
               "Effect of Public Housing on White Share of Population",
               "Effect of Public Housing on (asinh) Total Population",
               "Effect of Public Housing on (asinh) Black Population",
               "Effect of Public Housing on (asinh) White Population",
               "Effect of Public Housing on (asinh) Median Home Value", 
               "Effect of Public Housing on (asinh) Median Rent",
               "Effect of Public Housing on (asinh) Median Income",
               "Effect of Public Housing on Median Years of Education",
               "Effect of Public Housing on Unemployment Rate",
               "Effect of Public Housing on Labor Force Participation Rate",
               "Effect of Public Housing on Employment-Population Ratio")



# Read in and prep data ----

# read treated_tracts_panel.gpkg
treated_tracts_panel <-
  st_read("data/derived/merged/treated_tracts_panel_digitized.gpkg") %>% 
  st_drop_geometry() 

# read in Census tract sample with treatment status
census_tract_sample <-
  st_read("data/derived/merged/census_tract_sample_with_treatment_status_digitized.gpkg")


tot_units_planned_per_tract <-
  treated_tracts_panel %>%
  select(TRACTA, COUNTY, STATE, total_public_housing_units) %>% 
  distinct()


# Create, in each year, the treated, inner, and outer rings
# all neighbors among sample tracts

# 0. Choose 1990 as year to calculate neighbors. 
census_tract_sample_indexed_1990 <-
  census_tract_sample %>%
  filter(YEAR == 1990) 

# get 1990 crosswalk
census_tract_1990_crosswalk <-
  census_tract_sample_indexed_1990 %>% 
  mutate(row_number = row_number()) %>%
  select(row_number, tract_id, TRACTA, COUNTY, STATE) %>% 
  st_drop_geometry()

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

# 1. take neighbors with from in the list of "to" (in inner rings). 
# This will get the list of neighbors for all of the inner ring tracts, in all years
# 1. get distinct list
# 2. right_join onto the inner rings to get year pairs from (in outer_ring) to to (in inner_ring)
# 3. Merge on geographic information via the crosswalk
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


# Set up stacked, spatial DiD dataset  ----
# What do I need in the data for this
# Panel dataset of Census tracts (with geographic info),
# black and white pop share, treated, inner, outer, year

# However, I need some way to relate the treated tracts with its inner rings and outer
# rings, since I specifically want to compare treated tracts with their inner and outer
# rings... 

# The difference in the datasets above is that now, I want to link the treated tracts
# to their inner and outer rings, so that I can compare the treated tracts with their
# inner and outer rings


### 1. Get treated tracts, inner ring tracts, outer ring tracts in a wide dataset ---- 
# inner ring and treated 
event_study_data <-
  # Merge treated tracts time series onto tract_neighbors by tract information
  left_join(treated_tracts_panel,
            tract_neighbors,
            by = c("TRACTA" = "TRACTA_from", "COUNTY" = "COUNTY_from",
                   "STATE" = "STATE_from")) %>% 
  # 2. Filter to only those where neighbor == 1
  filter(neighbor == 1) %>% 
  select( -neighbor, -tract_id) %>% 
  # rename columns
  dplyr::rename(treated_id = from, inner_ring_id = to,
                TRACTA_treated = TRACTA, COUNTY_treated = COUNTY,
                STATE_treated = STATE) %>%
  # 3. Merge on the tract information of the inner ring
  left_join(census_tract_1990_crosswalk, by = c("inner_ring_id" = "row_number")) %>% 
  dplyr::rename(TRACTA_inner = TRACTA, COUNTY_inner = COUNTY,
                STATE_inner = STATE) %>%
  select(-tract_id) %>% 
  left_join(outer_rings_panel %>% select(-tract_id),
            by = c("inner_ring_id" = "from", "YEAR")) %>% 
  # rename variables
  dplyr::rename(outer_ring_id = to, TRACTA_outer = TRACTA, COUNTY_outer = COUNTY,
                STATE_outer = STATE) %>% 
  select(-outer_ring)

#### Ensure clean comparisons ----
# I want the treatment-control to be clean:
# 1. Keep only never-treated observations are in the inner and outer rings
# 2. If a tract appears in the inner ring, it should not appear in the outer ring

##### 1. Keep only never-treated observations are in the inner and outer rings ----
# identify tracts that are treated at some point
ever_treated_tracts <- 
  treated_tracts_panel %>% 
  select(STATE, COUNTY, TRACTA) %>%
  distinct() %>% 
  mutate(ever_treated = 1)

# drop inner rings and outer rings that are treated at some point
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

##### 2. If a tract appears in the inner ring, it should not appear in the outer ring -----
# get unique inner ring and outer ring tracts
unique_inner_rings <- 
  event_study_data %>% 
  select(TRACTA_inner, COUNTY_inner, STATE_inner) %>% 
  distinct() %>% 
  mutate(inner_ring = 1)

unique_outer_rings <- 
  event_study_data %>% 
  select(TRACTA_outer, COUNTY_outer, STATE_outer) %>% 
  distinct() 

# join the inner rings onto the outer rings to see which outer ring tracts are also in an inner ring
inner_outer_rings <- 
  unique_outer_rings %>% 
  left_join(unique_inner_rings,
            by = c("TRACTA_outer" = "TRACTA_inner", "COUNTY_outer" = "COUNTY_inner",
                   "STATE_outer" = "STATE_inner")) %>% 
  filter(!is.na(inner_ring)) %>% 
  select(-inner_ring) %>% 
  mutate(inner_and_outer_ring = 1)

# drop outer ring tracts that are also in an inner ring
event_study_data <- 
  event_study_data %>% 
  left_join(inner_outer_rings,
            by = c("TRACTA_outer" = "TRACTA_outer", "COUNTY_outer" = "COUNTY_outer",
                   "STATE_outer" = "STATE_outer")) %>% 
  filter(is.na(inner_and_outer_ring)) %>%
  select(-inner_and_outer_ring)

### Include 1930 data for all of the tracts for the event study
# I think I can get all unique tracts/ring combinations, set year = 1930, append onto dataset
# ? Don't understand above comment, maybe outdated

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

## 2. Reshape long and merge on census data ----

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
  fill(total_public_housing_units) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(total_public_housing_units = ifelse(is.na(total_public_housing_units), 0, total_public_housing_units))

# keep relevant variables, transform vars
census_data_for_event_study <-
  census_tract_sample %>% 
  select(STATE, COUNTY, TRACTA, YEAR, city,
         black_share, white_share, white_pop, black_pop, total_pop, 
         median_income, median_rent, median_home_value, median_educ_years_25plus,
         employment_pop_ratio, unemp_rate, lfp_rate) %>%
  st_drop_geometry() %>% 
  dplyr::rename(year = YEAR) %>% 
  # inverse hyperbolic sine transformation of populations and medians
  mutate(asinh_pop_total = asinh(total_pop),
         asinh_pop_white = asinh(white_pop),
         asinh_pop_black = asinh(black_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent = asinh(median_rent),
         asinh_median_home_value = asinh(median_home_value))

event_study_final <-
  left_join(event_study_long, census_data_for_event_study) %>% 
  # define treated variable equal to 1 if year >= treatment year and location_type == treated
  mutate(treated = ifelse(year >= treatment_year & location_type == "treated", TRUE, FALSE)) %>%
  # create variables for sun abraham estimates
  mutate(cohort = as.factor(year), 
         relative_period = (year - treatment_year)/10) %>% 
  # drop any tracts with 0 population
  filter(total_pop > 0) %>%
  # keep distinct observations: One observation per treated_id-tract-year
  distinct()

# create ring variable for ring fixed effects
event_study_data_rings <- 
  event_study_final %>%
  mutate(event_time = as_factor(year - treatment_year)) %>%
  mutate(ring = as.factor(case_when(location_type == "treated" ~ 0,
                                    location_type == "inner" ~ 1,
                                    location_type == "outer" ~ 2)),
         location_type_factor = factor(location_type, levels = c("outer", "treated", "inner")),
         location_type_dummy = as.numeric(location_type_factor)) %>% 
  # create a unique tract variable which is COUNTY + TRACTA
  mutate(tract_id = paste0(COUNTY, TRACTA))

### 2.5 calculate and merge on baseline black share as a control ------
baseline_black_share <-
  event_study_data_rings %>%
  filter(event_time == -10) %>% 
  select(treated_id, STATE, COUNTY, TRACTA, black_share) %>% 
  dplyr::rename(black_share_baseline = black_share) %>% 
  distinct()

# merge one
event_study_data_rings <-
  event_study_data_rings %>%
  left_join(baseline_black_share, by = c("treated_id","STATE", "COUNTY", "TRACTA"))


### 3. output event study data with rings -----
write_csv(event_study_data_rings, "data/derived/merged/event_study_data_rings_geocoded.csv")

# Run stacked diff-in-diff with inner and outer rings ----

# function for running event study regressions
run_event_study_regression <- function(data, dep_var) {
  
  data <- data %>% 
    mutate(treated_group = ifelse(location_type != "outer", 1, 0))
  
  formula <- as.formula(paste(dep_var, "~ i(event_time, treated_group, -10)",
                              "| treated_id^year + treated_id^treated_group"))
  
  
  # model 
  model_treated <- feols(formula, data = data %>% filter(location_type != "inner"), cluster = c("treated_id", "tract_id"))
  model_inner <- feols(formula, data %>% filter(location_type != "treated"), cluster = c("treated_id", "tract_id"))
  
  
  
  # Tidy the model output and process it
  tidy_output <- 
    broom::tidy(model_treated) %>%
    mutate(location_type = "treated") %>%
    bind_rows(broom::tidy(model_inner) %>% mutate(location_type = "inner")) %>%
    mutate_if(is.numeric, round, 4) %>% 
    # remove event_time:: and :treated_group from term
    mutate(term = str_remove(term, "event_time::"),
           term = str_remove(term, ":treated_group")) %>% 
    #rename term event_time, convert to numeric
    dplyr::rename(event_time = term) %>%
    mutate(event_time = as.numeric(event_time))
  
  # Extract model summary using broom::glance()
  model_summary <- broom::glance(model) %>%
    mutate_if(is.numeric, round, 4)
  
  # Combine tidy_output and model_summary
  output <- list(
    coefficients = tidy_output,
    summary = model_summary
  )
  
  return(output)
  
}


# function for plotting the event study graphs
create_event_study_plot <- function(reg_results_df, data, dep_var, title) {
  
  # calculate the t = 0 coefficient divided by the mean of the filtered_data variable in t = -10
  # and store it in a variable called "t10_coefficient"
  # t10_coefficient <-
  #   reg_results_df$coefficients %>%
  #   filter(event_time == 10, location_type == "treated") %>%
  #   pull(estimate) / mean(data %>% filter(location_type== "treated", event_time == -10) %>% pull(dep_var))
  
  coefficients <- reg_results_df$coefficients
  
  # add a row for each location type with event_time = -10, estimate = 0, std.error = 0
  coefficients <- 
    bind_rows(coefficients,
              tibble(location_type = c("treated", "inner"), event_time = -10, estimate = 0, std.error = 0))
  
  
  # number of observations
  n_obs <- reg_results_df$summary %>%
    pull(nobs)
  
  # create the plot
  ggplot(coefficients, aes(x = event_time, y = estimate, color = location_type)) +
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
    scale_color_brewer(palette = "Set1", labels = c("Inner Ring", "Treated"))
  # # add text at bottom of plot that says "t = 10 coefficient is t10_coefficient of the mean at t = 0"
  # labs(caption = paste("Treated coefficient at t = 10 is", round(t10_coefficient, 2), "of the treated tract mean at t = -10")) +
  # # add text at top of plot that says "n = n_obs"
  # annotate("text", x = 50, y = max(reg_results_df$coefficients$estimate), label = paste("n =", n_obs), hjust = 1)
  
}

## population regressions ---- 

event_study_rings_blk_sh_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "black_share")

event_study_rings_wht_sh_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "white_share")

event_study_rings_tot_pop_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_pop_total")

event_study_rings_blk_pop_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_pop_black")

event_study_rings_wht_pop_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_pop_white")

## median regressions ---- 

# median house value
event_study_rings_asinh_median_home_value_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_median_home_value")

event_study_rings_median_home_value_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "median_home_value")

# median rent
event_study_rings_asinh_median_rent_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_median_rent")

event_study_rings_median_rent_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "median_rent")

# median income
event_study_rings_asinh_median_income_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_median_income")

event_study_rings_median_income_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "median_income")

# median years of education
event_study_rings_median_education_tidy <-
  run_event_study_regression(
    data = event_study_data_rings %>% filter(year != 1930), dep_var = "median_educ_years_25plus")

# unemployment rate
event_study_rings_unemployment_rate_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "unemp_rate")

# LFP rate
event_study_rings_lfp_rate_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "lfp_rate")

# employment pop ratio
event_study_rings_emp_pop_ratio_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "employment_pop_ratio")


# Create plots and output to pdf in the output directory ----

## produce graphs ----

#### 1. Full sample
pdf(paste0(did_output_dir, "ring_event_study_plots.pdf"), width = 10, height = 5)

# black share did plot
create_event_study_plot(reg_results_df = event_study_rings_blk_sh_tidy, 
                        data = event_study_data_rings,
                        dep_var = "black_share", 
                        "Effect of Public Housing on Black Share of Population")
# white share did plot
create_event_study_plot(event_study_rings_wht_sh_tidy,
                        data = event_study_data_rings,
                        dep_var = "white_share",
                        "Effect of Public Housing on White Share of Population")
# total population did plot
create_event_study_plot(event_study_rings_tot_pop_tidy, 
                        data = event_study_data_rings,
                        dep_var = "asinh_pop_total",
                        "Effect of Public Housing on (asinh) Total Population")
# black population did plot
create_event_study_plot(event_study_rings_blk_pop_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_pop_black",
                        "Effect of Public Housing on (asinh) Black Population")
# white population did plot
create_event_study_plot(event_study_rings_wht_pop_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_pop_white",
                        "Effect of Public Housing on (asinh) White Population") 

# median home value did plot
create_event_study_plot(event_study_rings_asinh_median_home_value_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_median_home_value",
                        "Effect of Public Housing on (asinh) Median Home Value")

create_event_study_plot(event_study_rings_median_home_value_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_median_home_value",
                        "Effect of Public Housing on Median Home Value")


# median rent did plot
create_event_study_plot(event_study_rings_asinh_median_rent_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_median_rent",
                        "Effect of Public Housing on (asinh) Median Rent")

create_event_study_plot(event_study_rings_median_rent_tidy,
                        data = event_study_data_rings,
                        dep_var = "median_rent",
                        "Effect of Public Housing on Median Rent")

# median income did plot
create_event_study_plot(event_study_rings_asinh_median_income_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_median_income",
                        "Effect of Public Housing on (asinh) Median Income")

create_event_study_plot(event_study_rings_median_income_tidy,
                        data = event_study_data_rings,
                        dep_var = "median_income",
                        "Effect of Public Housing on Median Income")

# median years of education did plot
create_event_study_plot(event_study_rings_median_education_tidy,
                        data = event_study_data_rings,
                        dep_var = "median_educ_years_25plus",
                        "Effect of Public Housing on Median Years of Education")

# unemployment rate did plot
create_event_study_plot(event_study_rings_unemployment_rate_tidy,
                        data = event_study_data_rings,
                        dep_var = "unemp_rate",
                        "Effect of Public Housing on Unemployment Rate")

# LFP rate did plot
create_event_study_plot(event_study_rings_lfp_rate_tidy,
                        data = event_study_data_rings,
                        dep_var = "lfp_rate",
                        "Effect of Public Housing on Labor Force Participation Rate")

# employment pop ratio did plot
create_event_study_plot(event_study_rings_emp_pop_ratio_tidy,
                        data = event_study_data_rings,
                        dep_var = "employment_pop_ratio",
                        "Effect of Public Housing on Employment-Population Ratio")

dev.off()

### 2. Heterogeneity by size of public housing development ----
# Using "total units planned" 
x <- 
  event_study_data_rings %>%
  filter(location_type == "treated") %>%
  group_by(treated_id) %>% filter(row_number() == 1)

# get 25th, 50th and 75th percentiles of total_public_housing_units
units_25p <- quantile(x$total_public_housing_units, probs = 0.25)
units_50p <- quantile(x$total_public_housing_units, probs = 0.5)
units_75p <- quantile(x$total_public_housing_units, probs = 0.75)

# create a variable called "size" equal to "25th" if total_public_housing_units is less than 25th percentile,
# "50th" if total_public_housing_units is between 25th and 50th percentile, "75th" if total_public_housing_units is between
# 50th and 75th percentile and "large" otherwise
event_study_data_rings <- event_study_data_rings %>%
  mutate(size = case_when(total_public_housing_units < units_25p ~ "25th",
                          total_public_housing_units >= units_25p & total_public_housing_units < units_50p ~ "50th",
                          total_public_housing_units >= units_50p & total_public_housing_units < units_75p ~ "75th",
                          total_public_housing_units >= units_75p ~ "75th_to_max"),
         size_bunch = case_when(total_public_housing_units < units_50p ~ "small",
                                total_public_housing_units >= units_50p ~ "large"))

# re-run event studies separately for each size

for(size_group in unique(event_study_data_rings$size)) {
  # Filter the dataset for the current size
  filtered_data <- event_study_data_rings %>% filter(size == size_group)
  
  pdf_file_name <- paste0(did_output_dir, "by_size/ring_event_study_plots_size_", size_group, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  
  for(i in seq_along(es_variables)) {
    # Assuming you have a modified version of your function that returns the tidy data for the given variable
    tidy_output <- run_event_study_regression(filtered_data, es_variables[i])
    
    # Create and print the plot
    plot <- create_event_study_plot(tidy_output,
                                    data = filtered_data,
                                    dep_var = es_variables[i],
                                    es_titles[i])
    
    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}

# repeat but combine into 2
for(size_group in unique(event_study_data_rings$size_bunch)) {
  # Filter the dataset for the current size
  filtered_data <- event_study_data_rings %>% filter(size_bunch == size_group)
  
  pdf_file_name <- paste0(did_output_dir, "by_size/ring_event_study_plots_size_", size_group, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  
  for(i in seq_along(es_variables)) {
    # Assuming you have a modified version of your function that returns the tidy data for the given variable
    tidy_output <- run_event_study_regression(filtered_data, es_variables[i])
    
    # Create and print the plot
    plot <- create_event_study_plot(tidy_output,
                                    data = filtered_data,
                                    dep_var = es_variables[i],
                                    es_titles[i])
    
    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}


### 3. Heterogeneity by pre-treatment racial composition of the treated tract -----

# get pre-treatment black shares: 
pre_treatment_data <- 
  event_study_data_rings %>%
  filter(location_type == "treated") %>%
  filter(event_time == -10)

# get quantiles of pre-treatment black shares
blk_sh_25p <- quantile(pre_treatment_data$black_share, probs = 0.25)
blk_sh_50p <- quantile(pre_treatment_data$black_share,  probs = 0.5)
blk_sh_75p <- quantile(pre_treatment_data$black_share, probs = 0.75)

# create a variable called "pre_treatment_blk_sh" equal to "25th" if pre-treatment
# black share of the treated tract is less than 25th percentile, "50th" if pre-treatment black share is between
# 25th and 50th percentile, "75th" if pre-treatment black share is between 50th and 75th
# percentile and "high" otherwise
pre_treatment_blk_sh <-
  event_study_data_rings %>% 
  filter(event_time == -10, location_type == "treated") %>% 
  select(black_share, treated_id) %>%
  dplyr::rename(pre_treatment_blk_sh_treated = black_share)

blk_sh_25p <- quantile(pre_treatment_blk_sh$pre_treatment_blk_sh_treated, probs = 0.25, na.rm = TRUE)
blk_sh_50p <- quantile(pre_treatment_blk_sh$pre_treatment_blk_sh_treated, probs = 0.5, na.rm = TRUE)
blk_sh_75p <- quantile(pre_treatment_blk_sh$pre_treatment_blk_sh_treated, probs = 0.75, na.rm = TRUE)


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
  
  for(i in seq_along(es_variables)) {
    # Assuming you have a modified version of your function that returns the tidy data for the given variable
    tidy_output <- run_event_study_regression(filtered_data, es_variables[i])
    
    # Create and print the plot
    plot <- create_event_study_plot(tidy_output,
                                    data = filtered_data,
                                    dep_var = es_variables[i],
                                    es_titles[i])
    
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


### 4. Heterogeneity by city -----
# loop through each of the cities in the dataset, run regressions separately
for(c in unique(event_study_data_rings$city)) {
  # Filter the dataset for the current size
  filtered_data <- event_study_data_rings %>% filter(city == c)
  
  pdf_file_name <- paste0(did_output_dir, "by_city/ring_event_study_plots_", c, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  for(i in seq_along(es_variables)) {
    # Assuming you have a modified version of your function that returns the tidy data for the given variable
    tidy_output <- run_event_study_regression(filtered_data, es_variables[i])
    
    # Create and print the plot
    plot <- create_event_study_plot(tidy_output,
                                    data = filtered_data,
                                    dep_var = es_variables[i],
                                    es_titles[i])
    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}


# Troubleshooting ----
# for each group, get average and standard deviation of (asinh) median income, rent, home value and education in each event time (removing NAs)
# as well as the number of observations for each group
y <- 
  event_study_data_rings %>%
  group_by(location_type, event_time) %>%
  summarise(mean_median_income = mean(median_income, na.rm = TRUE),
            sd_median_income = sd(median_income, na.rm = TRUE),
            mean_median_rent = mean(median_rent, na.rm = TRUE),
            sd_median_rent = sd(median_rent, na.rm = TRUE),
            mean_median_home_value = mean(median_home_value, na.rm = TRUE),
            sd_median_home_value = sd(median_home_value, na.rm = TRUE),
            mean_median_educ_years_25plus = mean(median_educ_years_25plus, na.rm = TRUE),
            sd_median_educ_years_25plus = sd(median_educ_years_25plus, na.rm = TRUE),
            n = n())

z <- event_study_data_rings



# 
# # Exploratory graphs ------
# 
# ## Time series of project construction in my sample (histogram
# cdd_project_sample %>%
#   ggplot(aes(x = yearfullocc)) +
#   geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
#   labs(title = "Public Housing Projects per Year",
#        x = "Year",
#        y = "Number of Projects") +
#   # x axis every 5 years from 1940 to 1975
#   scale_x_continuous(breaks = seq(1935, 1975, 5)) +
#   theme_minimal() +
#   theme(plot.background = element_rect(fill = "white")) 
# 
# 
# ggsave(paste0(did_output_dir,"cdd_project_sample_time_series.png"), width = 8, height = 6, units = "in")
# 
# # time series of all projects
# cdd_projects_geolocated_sf %>% 
#   ggplot(aes(x = yearfullocc)) +
#   geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
#   labs(title = "Date of Project Construction (all projects)",
#        x = "Year",
#        y = "Number of Projects") +
#   # x axis every 5 years from 1940 to 1975
#   scale_x_continuous(breaks = seq(1935, 1975, 5)) +
#   theme_minimal()
# 
# # time series of total units in sample
# cdd_project_sample %>%
#   group_by(yearfullocc) %>% 
#   summarise(total_units = sum(total_public_housing_units, na.rm = TRUE)) %>%
#   ggplot(aes(x = yearfullocc, y = total_units)) +
#   geom_col(fill = "lightblue", color = "black") + 
#   labs(title = "Public Housing Units Constructed Per Year",
#        x = "Year",
#        y = "Number of Public Housing Units") +
#   # x axis every 5 years from 1940 to 1975
#   scale_x_continuous(breaks = seq(1935, 1975, 5)) +
#   theme_minimal() +
#   theme(plot.background = element_rect(fill = "white")) 
# 
# ggsave(paste0(did_output_dir,"cdd_project_sample_units_time_series.png"), width = 8, height = 6, units = "in")
