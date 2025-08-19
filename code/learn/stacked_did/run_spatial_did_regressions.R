####
# Spatial "inner and outer ring" DiD/event studies

# 1. Read in spatial DiD data created by "create_spatial_did_data.R"
# 2. Run event studies 
# 3. Output event study plots

####


### Right now, this is my main analysis file

# Preliminaries -----
# Load libraries
library(tidyverse)
library(here)
library(sf)
library(tigris)
library(crsuggest)
library(mapview)
library(maps)
library(ragg)
library(viridis)

# for covariate balance testing
library(tableone)

library(spdep)

# for event study
library(fixest)
library(ggfixest)
library(did2s)
 
set.seed(123)

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small", or "combined"
data_type <- "combined"

## define filepaths  -----

# directoriees 
did_output_dir <- here("output", "regression_results", "stacked_did", data_type)
merged_data_dir <- here("data", "derived", "merged", data_type)

# event study data paths
event_study_rings_output_path <- here(merged_data_dir, "event_study_data_rings.csv")
unique_tracts_and_rings_output_path <- here(merged_data_dir, "unique_tracts_and_rings.csv")



## variables for event studies and names -----
es_variables <- c("black_share", "white_share", "asinh_pop_total", "asinh_pop_black", "asinh_pop_white", 
                  "median_home_value_calculated", "asinh_median_home_value_calculated",
                  "median_rent_calculated", "asinh_median_rent_calculated",
                  "median_income", "asinh_median_income", 
                  "pct_hs_grad",
                  "unemp_rate", "lfp_rate")
es_titles <- c("Effect of Public Housing on Black Share of Population",
               "Effect of Public Housing on White Share of Population",
               "Effect of Public Housing on (asinh) Total Population",
               "Effect of Public Housing on (asinh) Black Population",
               "Effect of Public Housing on (asinh) White Population",
               "Effect of Public Housing on Median Home Value",
               "Effect of Public Housing on (asinh) Median Home Value", 
               "Effect of Public Housing on Median Rent",
               "Effect of Public Housing on (asinh) Median Rent",
               "Effect of Public Housing on Median Income",
               "Effect of Public Housing on (asinh) Median Income",
               "Effect of Public Housing on Percent Graduating High School",
               "Effect of Public Housing on Median Years of Education",
               "Effect of Public Housing on Unemployment Rate",
               "Effect of Public Housing on Labor Force Participation Rate",
               "Effect of Public Housing on Employment-Population Ratio")



# Read in and prep data ----
event_study_data_rings <-
  read_csv(event_study_rings_output_path) 

# Investigating ----
# missing_controls <- c(243, 299, 300, 317, 325, 336, 367, 829, 831, 835, 842, 843, 844, 845, 1405, 1410, 1411, 1412, 1867, 1869, 1877, 1879, 1881, 1907,
# 1908, 1939, 1941, 1943, 1946, 1950, 1961, 2247, 2485, 2507, 2509, 2550, 2552, 2932, 2977, 2978, 3002, 3032, 3034, 3035, 3036, 3038,
# 3039, 3040, 3163, 3169, 3172, 3173, 3175, 3179, 3180, 3187, 3195, 3252, 3769, 3774, 3780, 3783, 4108, 4159, 4175, 4185, 4208,
# 4359, 4364, 4365, 4372, 4405, 4428, 4430, 4469, 4470, 4471, 4473, 4474, 4477, 4484, 4486, 4844, 4876, 4893, 4894, 4895, 4943, 4960,
# 4962, 4967, 4968, 4979, 5042, 5050, 5052, 5064, 5066, 5067, 5073, 5077, 5078, 5079, 5156, 5228, 5405, 5406, 5407, 5410, 5421,
# 5422, 5424, 5425, 5427, 5435, 5436, 5735, 5814, 5816, 5956, 6021, 6022, 6024, 6105, 6506, 6750, 6785, 6790, 6818, 6998, 7014, 7022,
# 7140, 7167, 7217, 7222, 7234, 7237, 7242, 7246, 7263, 7311)
# 
# x <-
#   event_study_data_rings %>% 
#   filter(treated_id %in% missing_controls) %>% 
#   filter(COUNTY == "New York")
#   
# # Summary of Manhattan missing controls
# x %>%
#   filter(year == treatment_year, location_type == "treated") %>%
#   distinct(treated_id, treatment_year, total_public_housing_units) %>%
#   summarise(
#     n_projects = n(),
#     avg_units = mean(total_public_housing_units, na.rm = TRUE),
#     total_units = sum(total_public_housing_units, na.rm = TRUE),
#     treatment_years = paste(sort(unique(treatment_year)), collapse = ", ")
#   )
# 
# # Plot by treatment year
# x %>%
#   filter(year == treatment_year, location_type == "treated") %>%
#   distinct(treated_id, treatment_year, total_public_housing_units) %>%
#   ggplot(aes(x = factor(treatment_year))) +
#   geom_bar(fill = "red", alpha = 0.7) +
#   labs(title = "Manhattan Projects Missing Controls by Treatment Year",
#        x = "Treatment Year", y = "Number of Projects") +
#   theme_minimal()
# 
# # Plot by project size
# x %>%
#   filter(year == treatment_year, location_type == "treated") %>%
#   distinct(treated_id, treatment_year, total_public_housing_units) %>%
#   ggplot(aes(x = total_public_housing_units)) +
#   geom_histogram(fill = "red", alpha = 0.7, bins = 10) +
#   labs(title = "Manhattan Projects Missing Controls by Size",
#        x = "Total Housing Units", y = "Number of Projects") +
#   theme_minimal()
# 
# # List the specific projects
# x %>%
#   filter(year == treatment_year, location_type == "treated") %>%
#   distinct(treated_id, treatment_year, total_public_housing_units) %>%
#   arrange(treatment_year, total_public_housing_units) %>%
#   print(n = Inf)
# 
# # Get Manhattan tract geometries from the balanced sample
# manhattan_tracts <- st_read(here("data", "derived", "merged", "combined",
#                                  "census_tract_sample_with_treatment_status_balanced.gpkg")) %>%
#   filter(COUNTY == "New York", YEAR == 1990)  # Manhattan county code
# 
# # Get treated tracts in Manhattan with control status
# manhattan_treated <- event_study_data_rings %>%
#   filter(COUNTY == "New York", location_type == "treated", year == treatment_year) %>%
#   mutate(has_controls = ifelse(treated_id %in% missing_controls, "Missing Controls", "Has Controls")) %>%
#   distinct(GISJOIN_1950, has_controls, treated_id, total_public_housing_units)
# 
# # Join treated status to tract geometries
# manhattan_map_data <- manhattan_tracts %>%
#   left_join(manhattan_treated, by = "GISJOIN_1950") %>%
#   mutate(
#     tract_type = case_when(
#       has_controls == "Missing Controls" ~ "Missing Controls",
#       has_controls == "Has Controls" ~ "Has Controls",
#       TRUE ~ "No Public Housing"
#     )
#   )
# 
# # Create the map
# ggplot(manhattan_map_data) +
#   geom_sf(aes(fill = tract_type), color = "white", size = 0.1) +
#   scale_fill_manual(
#     values = c("Missing Controls" = "red",
#                "Has Controls" = "blue",
#                "No Public Housing" = "lightgray"),
#     name = "Status"
#   ) +
#   labs(title = "Manhattan Public Housing Projects: Control Status",
#        subtitle = "Red = Missing outer ring controls") +
#   theme_void() +
#   theme(legend.position = "bottom")



# Run stacked diff-in-diff with inner and outer rings ----

# # function for running event study regressions
# run_event_study_regression <- function(data, dep_var) {
#   
#   # !! testing
#   data <- event_study_data_rings
#   dep_var <- "black_pop"
#   
#   data <-
#     data %>% 
#      mutate(ring = case_when(location_type == "treated" ~ 1,
#                             location_type == "inner"~  2,
#                             location_type == "outer"~ 3)) %>% 
#     mutate(ring = factor(ring, levels = c(3,2,1), labels = c("outer", "inner", "treated"))) %>% 
#     filter(event_time > -40, event_time != 50)
#   
#   # CALCULATE WING (2024) weights following Guennewig-Moehnert and Blanco-Neri
#   # Need to calculate within treatment cohorts
#   data <- data %>%
#     group_by(event_time, treated_id, ring) %>%
#     mutate(N_re = n()) %>%  # Observations in ring r around project e
#     ungroup() %>%
#     group_by(event_time, ring) %>%
#     mutate(N_r = n()) %>%  # Total observations in ring r across all projects
#     ungroup() %>%
#     mutate(wing_weight = N_re / N_r)  # Corrected formula: N_re / N_r
#   
#     
#   
#   
#   formula <- as.formula(paste(dep_var, "~ i(event_time, ring, ref = -10, ref2 = 'outer')",
#                               "| treated_id^year + treated_id^ring"))
#   
#   # + COUNTY^year 
#   # Project (treated_id)-census year
#   # Project (treated_id)-ring (location_type_factor)-neighborhood
#   
#   model <- 
#     feols(formula, data = data, weights= ~wing_weight, 
#           cluster = c("treated_id", "GISJOIN_1950"))
#   
#   model_treated <-
#     feols(formula, data = data %>% filter(location_type != "inner"), 
#           weight = ~wing_weight,
#           cluster = c("treated_id"))
#   model_inner <-
#     feols(formula, data %>% filter(location_type != "treated"),
#           weight = ~wing_weight,
#           cluster = c("treated_id"))
#   
#   model_treated
#   model_inner
#   
#   # Tidy the model output and process it
#   tidy_output <- 
#     broom::tidy(model_treated) %>%
#     mutate(location_type = "treated") %>%
#     bind_rows(broom::tidy(model_inner) %>% mutate(location_type = "inner")) %>%
#     mutate_if(is.numeric, round, 4) %>% 
#     # remove event_time:: and :treated_group from term
#     mutate(term = str_remove(term, "event_time::"),
#            term = str_remove(term, ":ring::treated"),
#            term = str_remove(term, ":ring::inner")) %>% 
#     #rename term event_time, convert to numeric
#     dplyr::rename(event_time = term) %>%
#     mutate(event_time = as.numeric(event_time))
#   
#   # Extract model summary using broom::glance()
#   model_summary <- 
#     broom::glance(model_treated) %>%
#     bind_rows(broom::glance(model_inner)) %>%
#     mutate_if(is.numeric, round, 4)
#   
#   # Combine tidy_output and model_summary
#   output <- list(
#     coefficients = tidy_output,
#     summary = model_summary
#   )
#   
#   return(output)
#   
# }


# function for plotting the event study graphs
create_event_study_plot <- function(reg_results_df, data, dep_var, title) {
  
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


# function for running event study regressions 
run_event_study_regression <- function(data, dep_var) {
  
  # testing
  # data <- event_study_data_rings
  # dep_var <- "black_share"
  
  # Prepare data
  data <- data %>%
    mutate(ring = case_when(location_type == "treated" ~ 1,
                            location_type == "inner" ~ 2,
                            location_type == "outer" ~ 3)) %>%
    mutate(ring = factor(ring, levels = c(3,2,1), labels = c("outer",
                                                             "inner", "treated"))) %>%
    filter(event_time > -30, event_time != 50)
  
  
  # Calculate WING (2024) weights following Guennewig-Moehnert andBlanco-Neri
  data <- data %>%
    group_by(treatment_year, treated_id, ring) %>%
    mutate(N_re = n()) %>%  # Observations in ring r around project e
    ungroup() %>%
    group_by(treatment_year, ring) %>%
    mutate(N_r = n()) %>%  # Total observations in ring r across all projects
    ungroup() %>%
    mutate(wing_weight = N_re / N_r)
  
  
  # Create baseline outcome control for current dependent variable
  # Using event_time = -20 to control for pre-existing differences that cause pretrends
  baseline_outcome <- data %>%
    filter(event_time == -10) %>%
    select(GISJOIN_1950, treated_id, !!sym(dep_var)) %>% 
    rename(baseline_value = !!sym(dep_var))
  
  # Merge back to main data
  data <- data %>%
    left_join(baseline_outcome, by = c("GISJOIN_1950", "treated_id"))
  
  # Build formula for single regression with all rings including project-specific baseline controls
  formula <- as.formula(paste(
    dep_var, "~ i(event_time, ring, ref = -10, ref2 = 'outer')",
    "| treated_id^year + treated_id^ring + treated_id^ur_binary_5pp^year + treated_id^has_highway_1km^year"
  ))
  
  # Run single regression with all rings
  model <- feols(formula, data = data,
                 weights = ~wing_weight,
                 cluster = c("treated_id"))
  
  # Extract coefficients for all rings
  coeffs <- coef(model)
  ses <- se(model)
  
  # Get ring types (excluding reference ring)
  ring_types <- c("treated", "inner")
  
  # Extract coefficients for each ring type
  all_ring_data <- map_dfr(ring_types, function(ring_name) {
    ring_idx <- str_detect(names(coeffs), paste0("ring::", ring_name))
    if(any(ring_idx)) {
      ring_coefs <- coeffs[ring_idx]
      ring_ses <- ses[ring_idx]
      ring_times <- as.numeric(str_extract(names(ring_coefs),
                                           "-?[0-9]+"))
      
      tibble(
        event_time = ring_times,
        estimate = ring_coefs,
        se = ring_ses,
        location_type = ring_name
      )
    } else {
      warning(paste("No coefficients found for ring:", ring_name))
      tibble()
    }
  })
  
  # Add reference points for all rings at event_time = -10
  tidy_output <- all_ring_data %>%
    bind_rows(
      tibble(
        event_time = -10,
        estimate = 0,
        se = 0,
        location_type = ring_types
      )
    ) %>%
    mutate(
      std.error = se,  # For compatibility with existing plotting code
      term = as.character(event_time),  # For compatibility
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    ) %>%
    arrange(location_type, event_time)
  
  # Extract model summary
  model_summary <- broom::glance(model) %>%
    mutate_if(is.numeric, round, 4)
  
  # Return results in same format as original function
  output <- list(
    coefficients = tidy_output,
    summary = model_summary,
    model = model  # Include the full model for additional analysis
  )
  
  return(output)
}

# x <- event_study_data_rings

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

# private pop
event_study_rings_asinh_pop_private_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_private_pop")

# private black pop 
event_study_rings_asinh_black_private_pop_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_private_black_pop")

# private white pop
event_study_rings_asinh_white_private_pop_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_private_white_pop")


## median regressions ---- 

# median house value
event_study_rings_asinh_median_home_value_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_median_home_value_calculated")

event_study_rings_median_home_value_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "median_home_value_calculated")

# median rent
event_study_rings_asinh_median_rent_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_median_rent_calculated")

event_study_rings_median_rent_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "median_rent_calculated")

# median income
event_study_rings_asinh_median_income_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_median_income")

event_study_rings_median_income_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "median_income")

# share with high school or more
event_study_rings_hs_grad_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "pct_hs_grad")


# unemployment rate
event_study_rings_unemployment_rate_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "unemp_rate")

# LFP rate
event_study_rings_lfp_rate_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "lfp_rate")


run_event_study_regression(
    data = event_study_data_rings %>% filter(COUNTY == "New York"),
    dep_var = "asinh_pop_white")


# Create plots and output to pdf in the output directory ----

## produce graphs ----

#### 1. Full sample
pdf(here(did_output_dir, "ring_event_study_plots.pdf"), width = 10, height = 5)

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

# private population: Total
create_event_study_plot(event_study_rings_asinh_pop_private_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_private_pop",
                        "Effect of Public Housing on (asinh) Private Population")

# private population: Black
create_event_study_plot(event_study_rings_asinh_black_private_pop_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_private_black_pop",
                        "Effect of Public Housing on (asinh) Private Black Population")

# private population: White
create_event_study_plot(event_study_rings_asinh_white_private_pop_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_private_white_pop",
                        "Effect of Public Housing on (asinh) Private White Population")


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

# share with high school or more
create_event_study_plot(event_study_rings_hs_grad_tidy,
                        data = event_study_data_rings,
                        dep_var = "pct_hs_grad",
                        "Effect of Public Housing on Percent with High School or More")

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


dev.off()

### 2. Heterogeneity by size of public housing development ----
# Create output directories
dir.create(here(did_output_dir, "by_size"), recursive = TRUE, showWarnings = FALSE)

# Using "total units planned" 
treated_first_row <- 
  event_study_data_rings %>%
  filter(location_type == "treated") %>%
  group_by(treated_id) %>% filter(row_number() == 1)

# get percentiles
units_25p <- quantile(treated_first_row$total_public_housing_units, probs = 0.25, na.rm = TRUE)
units_50p <- quantile(treated_first_row$total_public_housing_units, probs = 0.5, na.rm = TRUE)
units_75p <- quantile(treated_first_row$total_public_housing_units, probs = 0.75, na.rm = TRUE)

# create simple size and policy era variables
event_study_data_rings <- event_study_data_rings %>%
  mutate(size = case_when(total_public_housing_units < units_25p ~ "25th",
                          total_public_housing_units >= units_25p & total_public_housing_units < units_50p ~ "50th",
                          total_public_housing_units >= units_50p & total_public_housing_units < units_75p ~ "75th",
                          total_public_housing_units >= units_75p ~ "75th_to_max"),
         # Simple median split for size
         size_simple = case_when(total_public_housing_units < units_50p ~ "small",
                                 total_public_housing_units >= units_50p ~ "large"),
         # Policy era split
         policy_era = case_when(
           treatment_year < 1960 ~ "pre_1960",
           treatment_year >= 1960 ~ "post_1960"
         ))

# Define variables for heterogeneity - all key outcomes
hetero_variables <- c(
  # Population and racial composition (main outcomes)
  "black_share", "white_share", "total_pop", "black_pop", "white_pop",
  "asinh_pop_total", "asinh_pop_black", "asinh_pop_white",
  
  # Private population estimates (key for interpretation)
  "private_population_estimate", "private_black_population_estimate", "private_white_population_estimate",
  "asinh_private_pop", "asinh_private_black_pop", "asinh_private_white_pop",
  
  # Economic outcomes
  "median_income", "asinh_median_income",
  "unemp_rate", "lfp_rate",
  
  # Housing outcomes
  "median_rent_calculated", "median_home_value_calculated",
  "asinh_median_rent_calculated", "asinh_median_home_value_calculated",
  
  # Education
  "pct_hs_grad",
  
  # Density
  "population_density"
)

# Corresponding titles
hetero_titles <- c(
  # Population and racial composition
  "Black Share", "White Share", "Total Population", "Black Population", "White Population",
  "Log Total Population", "Log Black Population", "Log White Population",
  
  # Private population estimates  
  "Private Total Pop", "Private Black Pop", "Private White Pop",
  "Log Private Total Pop", "Log Private Black Pop", "Log Private White Pop",
  
  # Economic outcomes
  "Median Income", "Log Median Income",
  "Unemployment Rate", "Labor Force Participation",
  
  # Housing outcomes
  "Median Rent", "Median Home Value",
  "Log Median Rent", "Log Median Home Value", 
  
  # Education
  "% High School Grad",
  
  # Density
  "Population Density"
)

# Run by detailed size categories
for(size_group in unique(event_study_data_rings$size)) {
  if(is.na(size_group)) next  # Skip if NA
  
  filtered_data <- event_study_data_rings %>% filter(size == size_group)
  
  # Check if we have enough data
  if(nrow(filtered_data) < 100) {
    cat("Skipping", size_group, "- insufficient data (n =", nrow(filtered_data), ")\n")
    next
  }
  
  pdf_file_name <- here(did_output_dir, "by_size", paste0("ring_event_study_plots_size_", size_group, ".pdf"))
  pdf(pdf_file_name, width = 10, height = 5)
  
  for(i in seq_along(hetero_variables)) {
    tryCatch({
      tidy_output <- run_event_study_regression(filtered_data, hetero_variables[i])
      
      plot <- create_event_study_plot(tidy_output,
                                      data = filtered_data,
                                      dep_var = hetero_variables[i],
                                      paste(hetero_titles[i], "- Size:", size_group))
      print(plot)
    }, error = function(e) {
      cat("Error with", hetero_variables[i], "for size", size_group, ":", e$message, "\n")
    })
  }
  
  dev.off()
  cat("Completed size group:", size_group, "\n")
}

# Run by simple size categories (small/large)
for(size_group in unique(event_study_data_rings$size_simple)) {
  if(is.na(size_group)) next
  
  filtered_data <- event_study_data_rings %>% filter(size_simple == size_group)
  
  if(nrow(filtered_data) < 100) {
    cat("Skipping", size_group, "- insufficient data\n")
    next
  }
  
  pdf_file_name <- here(did_output_dir, "by_size", paste0("ring_event_study_plots_size_", size_group, ".pdf"))
  pdf(pdf_file_name, width = 10, height = 5)
  
  for(i in seq_along(hetero_variables)) {
    tryCatch({
      tidy_output <- run_event_study_regression(filtered_data, hetero_variables[i])
      
      plot <- create_event_study_plot(tidy_output,
                                      data = filtered_data,
                                      dep_var = hetero_variables[i],
                                      paste(hetero_titles[i], "- Size:", size_group))
      print(plot)
    }, error = function(e) {
      cat("Error with", hetero_variables[i], "for size", size_group, ":", e$message, "\n")
    })
  }
  
  dev.off()
  cat("Completed size group:", size_group, "\n")
}


### 3. Heterogeneity by pre-treatment racial composition - theory-motivated thresholds -----
# Create output directories
dir.create(here(did_output_dir, "by_initial_share"), recursive = TRUE, showWarnings = FALSE)

# Create pre-treatment racial composition variable with theory-motivated thresholds
pre_treatment_blk_sh <-
  event_study_data_rings %>% 
  filter(event_time == -10, location_type == "treated") %>% 
  select(black_share, treated_id) %>%
  dplyr::rename(pre_treatment_blk_sh_treated = black_share)

event_study_data_rings <- 
  left_join(event_study_data_rings, pre_treatment_blk_sh)  %>%
  # Theory-based tipping thresholds from Boustan (2010) and Card, Mas, Rothstein (2008)
  mutate(pre_treatment_blk_sh = case_when(
    pre_treatment_blk_sh_treated < 0.05 ~ "below_tipping",     # <5% - Safe from white flight
    pre_treatment_blk_sh_treated < 0.12 ~ "tipping_range",     # 5-12% - Tipping zone
    pre_treatment_blk_sh_treated >= 0.12 ~ "above_tipping"     # ≥12% - Already tipped
  ))


# re-run event studies separately for each pre-treatment black share group
for(race_group in unique(event_study_data_rings %>% filter(!is.na(pre_treatment_blk_sh)) %>% pull(pre_treatment_blk_sh))) {
  # Filter the dataset for the current size
  filtered_data <- event_study_data_rings %>% filter(pre_treatment_blk_sh == race_group)
  
  pdf_dir <- here(did_output_dir, "by_initial_share")
  pdf_file_name <- here(pdf_dir, paste0("ring_event_study_plots_by_blk_share_", race_group, ".pdf"))
  
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

### 4. Heterogeneity by policy era (pre/post 1960) -----
# Create output directories  
dir.create(here(did_output_dir, "by_policy_era"), recursive = TRUE, showWarnings = FALSE)

# Run by policy era
for(era_group in unique(event_study_data_rings$policy_era)) {
  if(is.na(era_group)) next
  
  filtered_data <- event_study_data_rings %>% filter(policy_era == era_group)
  
  if(nrow(filtered_data) < 100) {
    cat("Skipping policy era", era_group, "- insufficient data\n")
    next
  }
  
  pdf_file_name <- here(did_output_dir, "by_policy_era", paste0("ring_event_study_plots_era_", era_group, ".pdf"))
  pdf(pdf_file_name, width = 10, height = 5)
  
  for(i in seq_along(hetero_variables)) {
    tryCatch({
      tidy_output <- run_event_study_regression(filtered_data, hetero_variables[i])
      
      plot <- create_event_study_plot(tidy_output,
                                      data = filtered_data,
                                      dep_var = hetero_variables[i],
                                      paste(hetero_titles[i], "- Policy Era:", era_group))
      print(plot)
    }, error = function(e) {
      cat("Error with", hetero_variables[i], "for policy era", era_group, ":", e$message, "\n")
    })
  }
  
  dev.off()
  cat("Completed policy era:", era_group, "\n")
}

### 6. Heterogeneity by city: New York vs Others -----
# New York City vs non New York City
# Create output directories
### 6. Heterogeneity by city: New York vs Others -----
# New York City vs non New York City
# Create output directories
dir.create(here(did_output_dir, "by_city"), recursive = TRUE, showWarnings = FALSE)

# Create NYC vs non-NYC groups
city_groups <- list(
  "New_York_City" = "New York City",
  "Other_Cities" = event_study_data_rings %>%
    filter(city != "New York City") %>%
    pull(city) %>%
    unique()
)

# Run by city group
for(group_name in names(city_groups)) {
  
  if(group_name == "New_York_City") {
    filtered_data <- event_study_data_rings %>% filter(city == "New York City")
    title_suffix <- "NYC"
  } else {
    filtered_data <- event_study_data_rings %>% filter(city != "New York City")
    title_suffix <- "Other Cities"
  }
  
  if(nrow(filtered_data) < 100) {
    cat("Skipping", group_name, "- insufficient data\n")
    next
  }
  
  pdf_file_name <- here(did_output_dir, "by_city", paste0("ring_event_study_plots_", group_name, ".pdf"))
  pdf(pdf_file_name, width = 10, height = 5)
  
  for(i in seq_along(hetero_variables)) {
    tryCatch({
      tidy_output <- run_event_study_regression(filtered_data, hetero_variables[i])
      
      plot <- create_event_study_plot(tidy_output,
                                      data = filtered_data,
                                      dep_var = hetero_variables[i],
                                      paste(hetero_titles[i], "-", title_suffix))
      print(plot)
    }, error = function(e) {
      cat("Error with", hetero_variables[i], "for", group_name, ":", e$message, "\n")
    })
  }
  
  dev.off()
  cat("Completed city group:", title_suffix, "\n")
}
