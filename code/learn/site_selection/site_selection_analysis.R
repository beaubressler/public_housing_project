####
# Site selection analysis
# Using 1940 characteristics to predict public housing placement (1941-1973)
####

# Preliminaries -----
library(tidyverse)
library(here)
library(sf)
library(modelsummary)
library(tinytable) # for formatting output
library(fixest)

# Data setup
data_type <- "combined"
site_selection_output_dir <- here("output", "regression_results", "site_selection", data_type)
data_dir <- here("data", "derived", "merged", data_type)

# Read data ----
census_tract_sample <-
  st_read(here(data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")) %>% 
  mutate(centroid = st_centroid(geom)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  st_drop_geometry()

# Create reshaped data for 1940 analysis
reshaped_census_data <-
  census_tract_sample %>%
  filter(YEAR %in% c(1930, 1940, 1950, 1960)) %>%
  select(GISJOIN_1950, YEAR, share_needing_repair, black_share, population_density,
         pct_hs_grad, median_income, median_home_value_calculated, median_rent_calculated,
         lfp_rate, 
         black_pop, total_pop, unemp_rate) %>%
  mutate(asinh_total_pop = asinh(total_pop),
         asinh_black_pop = asinh(black_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent_calculated = asinh(median_rent_calculated),
         asinh_median_home_value_calculated = asinh(median_home_value_calculated),
         ) %>% 
  pivot_wider(
    names_from = YEAR,
    values_from = c(share_needing_repair, black_share, population_density,
                    pct_hs_grad, median_income, median_home_value_calculated, lfp_rate,
                    median_rent_calculated, total_pop, black_pop,
                    asinh_total_pop, asinh_black_pop, asinh_median_income, 
                    asinh_median_rent_calculated, asinh_median_home_value_calculated, unemp_rate),
    names_glue = "{.value}_{YEAR}"
  )

# Create final dataset with treatment status
site_selection_data <- 
  census_tract_sample %>% 
  filter(YEAR == 2000) %>% 
  st_drop_geometry() %>% 
  left_join(reshaped_census_data, by = "GISJOIN_1950") %>% 
  mutate(county_id = paste0(STATEA, COUNTYA),
         asinh_distance_from_cbd = asinh(distance_from_cbd),
         asinh_distance_to_highway_km = asinh(distance_to_highway_km)) %>%
  filter(!is.na(black_share_1940), !is.na(total_pop_1940))

# Variable labels for models
variable_labels_1940 <- c(
  "black_share_1940" = "Black Share (1940)",
  "asinh_total_pop_1940" = "Total Population (1940)", 
  "asinh_median_income_1940" = "Log Median Income (1940)",
  "pct_hs_grad_1940" = "% High School Graduates (1940)",
  "unemp_rate_1940" = "Unemployment Rate (1940)",
  "lfp_rate_1940" = "LFP Rate (1940)",
  "asinh_median_rent_calculated_1940" = "Log Median Rent (1940)",
  "redlined_binary_80pp" = "Redlined (HOLC Grade D)",
  "asinh_distance_from_cbd" = "Log Distance from CBD",
  "cbd" = "CBD Indicator", 
  "share_needing_repair_1940" = "Share Needing Major Repairs (1940)",
  "median_rent_calculated_1940" = "Median Rent (1940)",
  "asinh_distance_to_highway_km" = "Log Distance to Highway (km)",
  "ur_binary_5pp" = "Urban Renewal Area"
)

# Site selection models using 1940 characteristics -----

## Model 0: Parsimoneous
model_0_lpm <- feols(
  treated ~
    # demographics
    black_share_1940 +
    asinh_total_pop_1940 +
    # SES
    asinh_median_income_1940 | county_id,
    # housing
    #share_needing_repair_1940 +
    # pre-exsting discrimination 
    
  data = site_selection_data,
  vcov_conley(lat = "lat", lon = "lon", cutoff = 2)
)

## Model 1: Full model
model_1_lpm <- feols(
  treated ~
    # demographics
    black_share_1940 +
    asinh_total_pop_1940 +
    # SES
    asinh_median_income_1940 +
    pct_hs_grad_1940 +
    unemp_rate_1940 +
    lfp_rate_1940 +
    # housing
    asinh_median_rent_calculated_1940 +
    #share_needing_repair_1940 +
    # geography
    asinh_distance_from_cbd + 
    cbd + 
    # pre-exsting discrimination 
    redlined_binary_80pp | county_id,
  data = site_selection_data,
  vcov_conley(lat = "lat", lon = "lon", cutoff = 2)
)

# 8/21/2025: I don't include share needing major repairs in 1940 because 
# it is missing for some tracts. However, if I do include it, it is statistically insignificant


## Model 3: Add mid-century urban policies
model_2_lpm <- feols(
  treated ~
    # demographics
    black_share_1940 +
    asinh_total_pop_1940 +
    # SES
    asinh_median_income_1940 +
    pct_hs_grad_1940 +
    unemp_rate_1940 +
    lfp_rate_1940 +
    # housing
    asinh_median_rent_calculated_1940 +
    #share_needing_repair_1940 +
    # geography
    asinh_distance_from_cbd + 
    cbd + 
    # pre-exsting discrimination 
    redlined_binary_80pp + 
    # Mid-century urban policies
    asinh_distance_to_highway_km + ur_binary_5pp | county_id,
  data = site_selection_data,
  vcov_conley(lat = "lat", lon = "lon", cutoff = 2)
)

# Fixed effects row for output
fe_row_1940 <- tibble::tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "County fixed effects", "Yes", "Yes", "Yes"
)

# Output LPM results
models_1940_lpm <- list(
  "(1)" = model_0_lpm,
  "(2)" = model_1_lpm,
  "(3)" = model_2_lpm
)

site_selection_table <- modelsummary(
  models_1940_lpm,
  estimate = "{estimate} ({std.error}){stars}",
  statistic = NULL,
  coef_omit = "(Intercept)",
  coef_map = variable_labels_1940,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",
  title = "Determinants of Public Housing Site Selection\\label{tab:site_selection_1940_lpm}",
  add_rows = fe_row_1940,
  notes = paste("Linear probability model results predicting public housing placement using 1940 neighborhood characteristics.",
                "All specifications include county fixed effects. Standard errors account for spatial correlation following Conley (1999) with cutoff of 2km."),
  output = "tinytable",         # get a tinytable object for post-processing
  width  = 1,                   # pass-through to tinytable::tt(): use full \linewidth
  theme  = "resize",            # scale to fit if needed (LaTeX)
  align  = "lccc"               # wider first column for variable names, centered data columns
)


# slightly smaller font & a bit more row height for readability
site_selection_table <- style_tt(site_selection_table, font_size = 0.95, height = 1.2)

# save to .tex
tinytable::save_tt(site_selection_table, here(site_selection_output_dir, "site_selection_1940_lpm.tex"), overwrite = TRUE)

# Standardized coefficients ----
baseline_prob <- mean(site_selection_data$treated, na.rm = TRUE)  # 10.58

redlined_effect <- coef(model_1_lpm)["redlined_binary_80pp"]
redlined_effect_relative <- redlined_effect /baseline_prob

black_share_std <- sd(site_selection_data$black_share_1940, na.rm = TRUE)
black_share_effect <- (coef(model_1_lpm)["black_share_1940"] * black_share_std)/ baseline_prob

income_std <- sd(site_selection_data$asinh_median_income_1940, na.rm = TRUE)
income_effect <- (coef(model_1_lpm)["asinh_median_income_1940"] * income_std) / baseline_prob
income_effect

unemployment_std <- sd(site_selection_data$unemp_rate_1940, na.rm = TRUE)
unemployment_effect <- (coef(model_1_lpm)["unemp_rate_1940"] * unemployment_std) / baseline_prob
unemployment_effect

urban_renewal_effect <- coef(model_2_lpm)["ur_binary_5pp"]
urban_renewal_effect_relative <- urban_renewal_effect / baseline_prob


# Print summary -----
cat("Site selection analysis complete using 1940 characteristics\n")
cat("Sample size:", nrow(site_selection_data), "tracts\n")
cat("Treated tracts:", sum(site_selection_data$treated), "\n")

