####
# Spatial "inner and outer ring" DiD/event studies - DISTANCE-BASED VERSION
#
# 1. Read in distance-based spatial DiD data created by contamination cleaning
# 2. Run event studies with distance rings (100-400m, 400-700m, 700-1000m control)
# 3. Output event study plots
#
####


### Distance-based spatial DiD analysis

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

# directories 
did_output_dir <- here("output", "regression_results", "stacked_did", data_type, "distance_based")
merged_data_dir <- here("data", "derived", "merged", data_type)

# Create output directory
dir.create(did_output_dir, recursive = TRUE, showWarnings = FALSE)

# event study data paths - using distance-based rings
distance_rings_clean_path <- here(merged_data_dir, "distance_rings_clean_stacked.csv")



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
cat("Loading distance-based spatial DiD data...\n")
event_study_data_rings <- read_csv(distance_rings_clean_path, show_col_types = FALSE)

cat("Data loaded:\n")
cat("- Total observations:", nrow(event_study_data_rings), "\n")
cat("- Unique tracts:", n_distinct(event_study_data_rings$GISJOIN_1950), "\n")
cat("- Unique projects:", n_distinct(event_study_data_rings$treated_id), "\n")

# Summary by location type
data_summary <- event_study_data_rings %>%
  filter(YEAR == 1950) %>%
  count(location_type, name = "n_assignments") %>%
  arrange(desc(n_assignments))

cat("\nAssignment summary (1950):\n")
print(data_summary)


# Run stacked diff-in-diff with distance rings ----

# function for plotting the event study graphs
create_event_study_plot <- function(reg_results_df, data, dep_var, title) {
  
  coefficients <- reg_results_df$coefficients
  
  # add a row for each location type with event_time = -10, estimate = 0, std.error = 0
  coefficients <- 
    bind_rows(coefficients,
              tibble(location_type = c("ring_0_100", "ring_100_400", "ring_400_700"), event_time = -10, estimate = 0, std.error = 0))
  
  
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
         subtitle = paste("Distance-based rings: 100-400m (inner_1), 400-700m (inner_2), 700-1000m (control)"),
         x = "Years Relative to Treatment",
         y = "Difference in Difference Estimate") +
    scale_x_continuous(breaks = seq(-40, 60, 10)) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.background = element_rect(fill = "white"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    guides(color = guide_legend(title = "Location Type")) +
    scale_color_manual(
      values = c("ring_0_100" = "red", "ring_100_400" = "orange", "ring_400_700" = "blue"),
      labels = c("ring_0_100" = "0-100m", "ring_100_400" = "100-400m", "ring_400_700" = "400-700m"),
      name = "Distance to Project"
    )
}


# function for running event study regressions 
run_event_study_regression <- function(data, dep_var) {
  
  # Prepare data - Chicago approach with distance-based treatment rings
  data <- data %>%
    mutate(location_type = factor(location_type, 
                                levels = c("control_700_1000", "ring_400_700", "ring_100_400", "ring_0_100"),
                                labels = c("control_700_1000", "ring_400_700", "ring_100_400", "ring_0_100"))) %>%
    filter(event_time > -30, event_time != 50)
  
  # Calculate WING (2024) weights following Guennewig-Moehnert and Blanco-Neri
  data <- data %>%
    group_by(treatment_year, treated_id, location_type) %>%
    mutate(N_re = n()) %>%  # Observations in ring r around project e
    ungroup() %>%
    group_by(treatment_year, location_type) %>%
    mutate(N_r = n()) %>%  # Total observations in ring r across all projects
    ungroup() %>%
    mutate(wing_weight = N_re / N_r)
  
  # Build formula for single regression with all rings - Chicago approach
  formula <- as.formula(paste(dep_var, "~ i(event_time, location_type, ref = -10, 
  ref2 = 'control_700_1000')",
                              "| treated_id^YEAR + treated_id^location_type + treated_id^ur_binary_10pp + treated_id^has_highway_1km"))
  
  # Run single regression with all rings
  model <- feols(formula, data = data,
                 weights = ~wing_weight,
                 cluster = c("treated_id"))
  
  # Extract coefficients for all rings
  coeffs <- coef(model)
  ses <- se(model)
  
  # Get treatment ring types (excluding reference control)
  treatment_rings <- c("ring_0_100", "ring_100_400", "ring_400_700")
  
  # Extract coefficients for each treatment ring
  all_ring_data <- map_dfr(treatment_rings, function(ring_name) {
    ring_idx <- str_detect(names(coeffs), paste0("location_type::", ring_name))
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
        location_type = treatment_rings
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


## population regressions ---- 
cat("\nRunning population regressions...\n")

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
cat("Running housing value and income regressions...\n")

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


# Create plots and output to pdf in the output directory ----

## produce graphs ----
cat("\nCreating plots...\n")

#### 1. Full sample
pdf(here(did_output_dir, "distance_ring_event_study_plots.pdf"), width = 10, height = 5)

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

cat("\nDistance-based spatial DiD analysis complete!\n")
cat("Results saved to:", here(did_output_dir, "distance_ring_event_study_plots.pdf"), "\n")
cat("Using clean dataset with", nrow(data_summary), "ring assignments\n")