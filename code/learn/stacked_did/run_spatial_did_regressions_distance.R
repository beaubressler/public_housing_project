####
# Distance-Based Spatial Difference-in-Differences Event Studies
#
# 1. Read in distance-based spatial DiD data created by "create_spatial_did_data_distance.R"
# 2. Run event studies using treated/spillover/control rings
# 3. Output event study plots
#
# Ring structure: treated (0-50m), spillover (50-800m), control (800-1500m)
####

# Preliminaries -----
# Load libraries
library(tidyverse)
library(here)
library(fixest)
library(ggfixest)
library(viridis)

set.seed(123)

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small" or "combined"
data_type <- "combined"

## define filepaths -----

# directories 
did_output_dir <- here("output", "regression_results", "stacked_did", data_type, "distance_based")
merged_data_dir <- here("data", "derived", "merged", data_type)
figures_dir <- here("output", "figures", "stacked_did", data_type, "distance_based")

# Create directories
dir.create(did_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# event study data path
event_study_data_path <- here(merged_data_dir, "distance_rings_event_study.csv")

## variables for event studies and names -----
es_variables <- c("black_share", "white_share", "asinh_pop_total", "asinh_pop_black", "asinh_pop_white", 
                  "asinh_private_pop", "asinh_private_black_pop", "asinh_private_white_pop",
                  "asinh_median_home_value_calculated", "asinh_median_rent_calculated",
                  "asinh_median_income", "pct_hs_grad", "unemp_rate", "lfp_rate")

es_titles <- c("Effect of Public Housing on Black Share of Population",
               "Effect of Public Housing on White Share of Population",
               "Effect of Public Housing on (asinh) Total Population",
               "Effect of Public Housing on (asinh) Black Population",
               "Effect of Public Housing on (asinh) White Population",
               "Effect of Public Housing on (asinh) Private Population",
               "Effect of Public Housing on (asinh) Private Black Population", 
               "Effect of Public Housing on (asinh) Private White Population",
               "Effect of Public Housing on (asinh) Median Home Value",
               "Effect of Public Housing on (asinh) Median Rent",
               "Effect of Public Housing on (asinh) Median Income",
               "Effect of Public Housing on Percent Graduating High School",
               "Effect of Public Housing on Unemployment Rate",
               "Effect of Public Housing on Labor Force Participation Rate")

# Read in and prep data ----
event_study_data_distance <- read_csv(event_study_data_path)

# function for running event study regressions 
run_event_study_regression <- function(data, dep_var) {
  
  # Prepare data
  data <- data %>%
    mutate(ring = case_when(location_type == "treated_0_50" ~ 1,
                            location_type == "spillover_50_800" ~ 2,
                            location_type == "control_800_1500" ~ 3)) %>%
    mutate(ring = factor(ring, levels = c(3,2,1), labels = c("control",
                                                             "spillover", "treated"))) %>%
    filter(event_time >= -20, event_time <= 40)
  
  # Build formula for single regression with all rings
  formula <- as.formula(paste(
    dep_var, "~ i(event_time, ring, ref = -10, ref2 = 'control')",
    "| treated_id^YEAR + treated_id^ring + treated_id^ur_binary_10pp^YEAR + treated_id^has_highway_1km^YEAR"
  ))
  
  # Run single regression with all rings
  model <- feols(formula, data = data,
                 weights = ~wing_weight,
                 cluster = c("treated_id"))
  
  # Extract coefficients for all rings
  coeffs <- coef(model)
  ses <- se(model)
  
  # Get ring types (excluding reference ring)
  ring_types <- c("treated", "spillover")
  
  # Extract coefficients for each ring type
  all_ring_data <- map_dfr(ring_types, function(ring_name) {
    ring_idx <- str_detect(names(coeffs), paste0("ring::", ring_name))
    if(any(ring_idx)) {
      ring_coefs <- coeffs[ring_idx]
      ring_ses <- ses[ring_idx]
      ring_times <- as.numeric(str_extract(names(ring_coefs), "-?[0-9]+"))
      
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
  plot_data <- all_ring_data %>%
    bind_rows(
      tibble(
        event_time = -10,
        estimate = 0,
        se = 0,
        location_type = ring_types
      )
    ) %>%
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se,
      t_stat = estimate / se,
      p_value = 2 * (1 - pnorm(abs(t_stat)))  # Two-tailed p-value
    ) %>%
    arrange(location_type, event_time)
  
  return(plot_data)
}

# function for plotting the event study graphs
create_event_study_plot <- function(reg_results_df, data, dep_var, title) {
  
  plot <- ggplot(reg_results_df, aes(x = event_time, y = estimate, color = location_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
    geom_vline(xintercept = -0.5, linetype = "solid", alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.8, alpha = 0.7) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("treated" = "red", "spillover" = "blue"),
      labels = c("treated" = "Treated (0-50m)", "spillover" = "Spillover (50-800m)"),
      name = "Distance to Project"
    ) +
    labs(
      title = title,
      subtitle = "Distance-Based Spatial Difference-in-Differences Event Study",
      x = "Years Since Public Housing Completion", 
      y = "Effect",
      caption = "Reference: Control ring (800-1500m) at event_time = -10. 95% confidence intervals.\nClustered standard errors at project level."
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "bottom"
    )
  
  return(plot)
}

# Run event studies ----

# Black Share
event_study_distance_blk_sh_tidy <-
  run_event_study_regression(event_study_data_distance, "black_share")

# White Share  
event_study_distance_wht_sh_tidy <-
  run_event_study_regression(event_study_data_distance, "white_share")

# Total Population
event_study_distance_tot_pop_tidy <-
  run_event_study_regression(event_study_data_distance, "asinh_pop_total")

# Black Population
event_study_distance_blk_pop_tidy <-
  run_event_study_regression(event_study_data_distance, "asinh_pop_black")

# White Population
event_study_distance_wht_pop_tidy <-
  run_event_study_regression(event_study_data_distance, "asinh_pop_white")

# Private Population
event_study_distance_asinh_pop_private_tidy <-
  run_event_study_regression(event_study_data_distance, "asinh_private_pop")

# Private Black Population  
event_study_distance_asinh_black_private_pop_tidy <-
  run_event_study_regression(event_study_data_distance, "asinh_private_black_pop")

# Private White Population
event_study_distance_asinh_white_private_pop_tidy <-
  run_event_study_regression(event_study_data_distance, "asinh_private_white_pop")

# Median Home Value
event_study_distance_asinh_median_home_value_tidy <-
  run_event_study_regression(event_study_data_distance, "asinh_median_home_value_calculated")

# Median Rent
event_study_distance_asinh_median_rent_tidy <-
  run_event_study_regression(event_study_data_distance, "asinh_median_rent_calculated")

# Median Income
event_study_distance_asinh_median_income_tidy <-
  run_event_study_regression(event_study_data_distance, "asinh_median_income")

# High School Graduation Rate
event_study_distance_hs_grad_tidy <-
  run_event_study_regression(event_study_data_distance, "pct_hs_grad")

# Unemployment Rate
event_study_distance_unemployment_rate_tidy <-
  run_event_study_regression(event_study_data_distance, "unemp_rate")

# Labor Force Participation Rate
event_study_distance_lfp_rate_tidy <-
  run_event_study_regression(event_study_data_distance, "lfp_rate")

# Create and save plots ----

# Create plots
pdf(here(figures_dir, "distance_based_event_study_plots.pdf"), width = 10, height = 6)

create_event_study_plot(event_study_distance_blk_sh_tidy, 
                       event_study_data_distance, "black_share", es_titles[1])

create_event_study_plot(event_study_distance_wht_sh_tidy,
                       event_study_data_distance, "white_share", es_titles[2])

create_event_study_plot(event_study_distance_tot_pop_tidy, 
                       event_study_data_distance, "asinh_pop_total", es_titles[3])

create_event_study_plot(event_study_distance_blk_pop_tidy,
                       event_study_data_distance, "asinh_pop_black", es_titles[4])

create_event_study_plot(event_study_distance_wht_pop_tidy,
                       event_study_data_distance, "asinh_pop_white", es_titles[5])

create_event_study_plot(event_study_distance_asinh_pop_private_tidy,
                       event_study_data_distance, "asinh_private_pop", es_titles[6])

create_event_study_plot(event_study_distance_asinh_black_private_pop_tidy,
                       event_study_data_distance, "asinh_private_black_pop", es_titles[7])

create_event_study_plot(event_study_distance_asinh_white_private_pop_tidy,
                       event_study_data_distance, "asinh_private_white_pop", es_titles[8])

create_event_study_plot(event_study_distance_asinh_median_home_value_tidy,
                       event_study_data_distance, "asinh_median_home_value_calculated", es_titles[9])

create_event_study_plot(event_study_distance_asinh_median_rent_tidy,
                       event_study_data_distance, "asinh_median_rent_calculated", es_titles[10])

create_event_study_plot(event_study_distance_asinh_median_income_tidy,
                       event_study_data_distance, "asinh_median_income", es_titles[11])

create_event_study_plot(event_study_distance_hs_grad_tidy,
                       event_study_data_distance, "pct_hs_grad", es_titles[12])

create_event_study_plot(event_study_distance_unemployment_rate_tidy,
                       event_study_data_distance, "unemp_rate", es_titles[13])

create_event_study_plot(event_study_distance_lfp_rate_tidy,
                       event_study_data_distance, "lfp_rate", es_titles[14])

dev.off()

# Save regression results as tex tables ----

# Demographic results
etable(list(event_study_distance_blk_sh_tidy, event_study_distance_wht_sh_tidy, 
           event_study_distance_blk_pop_tidy, event_study_distance_wht_pop_tidy),
       tex = TRUE, replace = TRUE,
       file = here(did_output_dir, "distance_based_demographic_results.tex"),
       title = "Effect of Public Housing on Neighborhood Demographics - Distance-Based Spatial DiD",
       label = "tab:distance_demographics")

# Housing market results  
etable(list(event_study_distance_asinh_median_home_value_tidy,
           event_study_distance_asinh_median_rent_tidy,
           event_study_distance_asinh_median_income_tidy),
       tex = TRUE, replace = TRUE,
       file = here(did_output_dir, "distance_based_housing_results.tex"),
       title = "Effect of Public Housing on Housing Market Outcomes - Distance-Based Spatial DiD", 
       label = "tab:distance_housing")

cat("Analysis complete! Results saved to", figures_dir, "and", did_output_dir, "\n")

