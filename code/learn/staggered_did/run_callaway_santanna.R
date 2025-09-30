####
# Callaway and Sant'Anna (2021) Difference-in-Differences with Multiple Time Periods
# Uses the full balanced dataset (no matching)
####

library(did)
library(tidyverse)
library(here)
library(sf)

rm(list = ls())

# 0. Setup -----
data_type <- "combined"
set.seed(123456L)

# 1. Load full balanced dataset -----
cat("Loading full balanced dataset...\n")

balanced_data_raw <- st_read(here("data", "derived", "merged", data_type, 
                                  "census_tract_sample_with_treatment_status_balanced.gpkg"))

cat("Dataset loaded with", nrow(balanced_data_raw), "observations\n")
cat("Unique tracts:", n_distinct(balanced_data_raw$GISJOIN_1950), "\n")
cat("Years:", paste(sort(unique(balanced_data_raw$YEAR)), collapse = ", "), "\n")

# 1a. Load ring structure and exclude inner ring tracts -----
cat("\n=== EXCLUDING INNER RING TRACTS ===\n")

tracts_and_rings <- read_csv(here("data", "derived", "merged", data_type, 
                                  "unique_tracts_and_rings.csv"))

# Identify inner ring tracts to exclude (may be affected by spillovers)
inner_ring_tracts <- tracts_and_rings %>%
  filter(location_type == "inner") %>%
  distinct(GISJOIN_1950) %>%
  pull(GISJOIN_1950)

cat("Found", length(inner_ring_tracts), "inner ring tracts to exclude\n")

# Filter out inner ring tracts
balanced_data_raw <- balanced_data_raw %>%
  filter(!GISJOIN_1950 %in% inner_ring_tracts)

cat("After excluding inner rings:", nrow(balanced_data_raw), "observations\n")
cat("Unique tracts remaining:", n_distinct(balanced_data_raw$GISJOIN_1950), "\n")

# 2. Examine data structure -----
cat("\n=== EXAMINING DATA STRUCTURE ===\n")

# Check variable names
cat("Available variables:\n")
variable_names <- names(balanced_data_raw)
cat(paste(variable_names, collapse = ", "), "\n")

# Look for treatment-related variables
treatment_vars <- variable_names[grepl("treat|housing", variable_names, ignore.case = TRUE)]
cat("\nTreatment-related variables:\n")
cat(paste(treatment_vars, collapse = ", "), "\n")

# Check treatment status
treatment_check <- balanced_data_raw %>%
  st_drop_geometry() %>%
  distinct(GISJOIN_1950, treated) %>%
  count(treated)

cat("\nTreatment status counts:\n")
print(treatment_check)

# 3. Load treatment panel to get treatment years -----
cat("\n=== LOADING TREATMENT TIMING DATA ===\n")

treated_panel <- st_read(here("data", "derived", "merged", data_type, 
                              "treated_tracts_panel_balanced.gpkg")) %>%
  st_drop_geometry()

cat("Treated panel loaded with", nrow(treated_panel), "observations\n")

# Check what treatment year variables are available
treated_vars <- names(treated_panel)
cat("Variables in treated panel:\n")
cat(paste(treated_vars, collapse = ", "), "\n")

# Check treatment years available
treatment_years <- treated_panel %>%
  distinct(treatment_year) %>%
  arrange(treatment_year)

cat("\nTreatment years available:\n")
print(treatment_years)

# 4. Prepare data for Callaway-Sant'Anna -----
cat("\n=== PREPARING CS DATA ===\n")

# Create dataset with treatment timing
cs_data <- balanced_data_raw %>%
  st_drop_geometry() %>%
  # Rename YEAR to year for CS package
  rename(year = YEAR) %>%
  # Get unique treatment years by tract
  left_join(
    treated_panel %>% 
      distinct(GISJOIN_1950, treatment_year),
    by = "GISJOIN_1950"
  ) %>%
  # Create gname variable (group = first treatment year, 0 = never treated)
  mutate(
    gname = case_when(
      is.na(treatment_year) ~ 0,  # Never treated
      TRUE ~ treatment_year       # First treatment year
    )
  ) %>%
  # Remove missing outcome data
  filter(!is.na(black_share)) %>%
  # Create transformed variables (like in matching algorithm)
  mutate(
    asinh_pop_total = asinh(total_pop),
    asinh_pop_white = asinh(white_pop),
    asinh_pop_black = asinh(black_pop),
    asinh_median_income = asinh(median_income),
    asinh_median_rent_calculated = asinh(median_rent_calculated),
    asinh_median_home_value_calculated = asinh(median_home_value_calculated),
    asinh_distance_from_cbd = asinh(distance_from_cbd),
    ln_population_density = log(population_density),
    # Create county factor for fixed effects
    county_factor = as.factor(state_county),
    state_factor = as.factor(STATEA),
    cbsa_factor = as.factor(cbsa_title)
  ) %>%
  # Create numeric ID for CS package
  group_by(GISJOIN_1950) %>%
  mutate(numeric_id = cur_group_id()) %>%
  ungroup()
# %>%
#   # try dropping never treated
#   filter(gname != 0)

cat("CS data prepared with", nrow(cs_data), "observations\n")
cat("Unique tracts:", n_distinct(cs_data$GISJOIN_1950), "\n") 
cat("Years:", paste(sort(unique(cs_data$year)), collapse = ", "), "\n")

# Summary of treatment groups
gname_summary <- cs_data %>%
  distinct(GISJOIN_1950, gname) %>%
  count(gname, sort = TRUE)

cat("\nTreatment group sizes (gname):\n")
print(gname_summary)

# 4a. Create 1940 baseline characteristics for all units -----
cat("\n=== CREATING 1940 BASELINE CHARACTERISTICS ===\n")

# Create baseline (1940) variables for all units
baseline_vars <- c("black_share", "white_share", 
                   "total_pop", "asinh_pop_total",
                   "median_income",  "asinh_median_income",
                   "median_rent_calculated", "asinh_median_rent_calculated",
                   "black_pop", "asinh_pop_black",
                   "unemp_rate", "lfp_rate")

# Extract 1940 values for all units
baseline_1940 <- cs_data %>%
  filter(year == 1940) %>%
  select(GISJOIN_1950, all_of(baseline_vars)) %>%
  rename_with(~ paste0(.x, "_1940"), .cols = all_of(baseline_vars)) %>%
  distinct()

cat("1940 baseline data extracted for", nrow(baseline_1940), "tracts\n")

# Merge baseline characteristics back to full dataset
cs_data <- cs_data %>%
  left_join(baseline_1940, by = "GISJOIN_1950")

# Check coverage
baseline_coverage <- cs_data %>%
  summarise(
    total_obs = n(),
    with_baseline = sum(!is.na(black_share_1940)),
    coverage_rate = with_baseline / total_obs
  )

cat("1940 baseline coverage:", round(baseline_coverage$coverage_rate * 100, 1), "%\n")

# 5. Run Callaway-Sant'Anna estimation for all outcomes -----
cat("\n=== RUNNING CALLAWAY-SANT'ANNA ESTIMATION ===\n")

# Define outcome variables to analyze
outcome_vars <- c("black_share",  "asinh_pop_total", "asinh_pop_white", "asinh_pop_black",
                  "asinh_median_income", "asinh_median_rent_calculated", "asinh_median_home_value_calculated",
                  "unemp_rate", "lfp_rate")

# Define control formula
control_formula <- ~ asinh_distance_from_cbd + cbsa_factor + redlined_binary_80pp + ur_binary_5pp + 
                    black_share_1940 + asinh_median_income_1940 +
                    asinh_pop_total_1940 + asinh_pop_black_1940 + 
                    asinh_median_rent_calculated_1940 + unemp_rate_1940 + lfp_rate_1940

# Function to run CS analysis for any outcome
run_cs_outcome <- function(outcome_var, data = cs_data) {
  
  cat("\n--- Analyzing", outcome_var, "---\n")
  
  # Filter out missing outcome data
  analysis_data <- data %>% filter(!is.na(.data[[outcome_var]]))
  cat("Complete observations:", nrow(analysis_data), "\n")
  
  # Run att_gt
  cs_results <- att_gt(
    yname = outcome_var,
    tname = "year", 
    idname = "numeric_id",
    gname = "gname",
    xformla = control_formula,
    data = analysis_data,
    control_group = "notyettreated",
    base_period = "universal",
    est_method = "dr",
    clustervars = "numeric_id"
  )
  
  # Create aggregations
  cs_dynamic <- aggte(cs_results, type = "dynamic", min_e = -20, max_e = 30)
  cs_simple <- aggte(cs_results, type = "simple", min_e = -20, max_e = 30)
  
  return(list(
    att_gt = cs_results,
    dynamic = cs_dynamic,
    simple = cs_simple,
    outcome = outcome_var
  ))
}

# Run analysis for all outcomes
cat("Running CS analysis for", length(outcome_vars), "outcome variables...\n")

cs_results_all <- purrr::map(outcome_vars, run_cs_outcome)
names(cs_results_all) <- outcome_vars

# 6. Process and summarize results -----
cat("\n=== PROCESSING RESULTS ===\n")

# Function to extract and format results
extract_cs_results <- function(cs_result) {
  
  # Debug: check structure
  cat("Processing outcome:", cs_result$outcome, "\n")
  
  # Group-time effects (raw att_gt results)
  group_time_results <- cs_result$att_gt
  
  # Extract group-time effects
  if(!is.null(group_time_results$group) && !is.null(group_time_results$t) && 
     !is.null(group_time_results$att) && !is.null(group_time_results$se)) {
    
    group_time_df <- data.frame(
      group = group_time_results$group,
      time_period = group_time_results$t,
      att = group_time_results$att,
      se = group_time_results$se,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        ci_lower = att - 1.96 * se,
        ci_upper = att + 1.96 * se,
        outcome = cs_result$outcome
      ) %>%
      # Create more readable labels
      mutate(
        group_label = case_when(
          group == 0 ~ "Never Treated",
          TRUE ~ paste0("Treated ", group)
        )
      )
  } else {
    # Create empty data frame if no group-time results
    group_time_df <- data.frame(
      group = numeric(0),
      time_period = numeric(0),
      att = numeric(0),
      se = numeric(0),
      ci_lower = numeric(0),
      ci_upper = numeric(0),
      outcome = character(0),
      group_label = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  # Dynamic effects
  dynamic_results <- cs_result$dynamic
  
  # Check what we have in dynamic results
  cat("Dynamic results structure:\n")
  cat("  egt length:", length(dynamic_results$egt), "\n")
  cat("  att.egt length:", length(dynamic_results$att.egt), "\n")
  cat("  se.egt length:", length(dynamic_results$se.egt), "\n")
  cat("  crit.val.egt length:", length(dynamic_results$crit.val.egt), "\n")
  
  # Extract dynamic effects with proper handling of scalar critical value
  if(!is.null(dynamic_results$egt) && length(dynamic_results$egt) > 0 &&
     length(dynamic_results$egt) == length(dynamic_results$att.egt) &&
     length(dynamic_results$egt) == length(dynamic_results$se.egt)) {
    
    # Critical value is typically scalar - repeat it for all time periods
    crit_val_vector <- rep(dynamic_results$crit.val.egt[1], length(dynamic_results$egt))
    
    dynamic_df <- data.frame(
      event_time = dynamic_results$egt,
      att = dynamic_results$att.egt,
      se = dynamic_results$se.egt,
      crit_val = crit_val_vector,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        ci_lower = att - crit_val * se,
        ci_upper = att + crit_val * se,
        outcome = cs_result$outcome
      )
  } else {
    cat("  Creating empty dynamic_df due to length mismatch\n")
    # Create empty data frame if no dynamic results
    dynamic_df <- data.frame(
      event_time = numeric(0),
      att = numeric(0),
      se = numeric(0),
      crit_val = numeric(0),
      ci_lower = numeric(0),
      ci_upper = numeric(0),
      outcome = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  # Simple average
  simple_results <- cs_result$simple
  
  # Debug simple results
  cat("Simple results structure:\n")
  cat("  overall.att length:", length(simple_results$overall.att), "\n")
  cat("  overall.se length:", length(simple_results$overall.se), "\n")
  cat("  crit.val.egt length:", length(simple_results$crit.val.egt), "\n")
  cat("  outcome length:", length(cs_result$outcome), "\n")
  
  # Use 1.96 for simple results (95% CI) since crit.val.egt is not available
  simple_df <- data.frame(
    att = simple_results$overall.att,
    se = simple_results$overall.se,
    ci_lower = simple_results$overall.att - 1.96 * simple_results$overall.se,
    ci_upper = simple_results$overall.att + 1.96 * simple_results$overall.se,
    outcome = cs_result$outcome,
    stringsAsFactors = FALSE
  )
  
  return(list(group_time = group_time_df, dynamic = dynamic_df, simple = simple_df))
}

# Extract results for all outcomes
cs_processed <- purrr::map(cs_results_all, extract_cs_results)

# Combine results across outcomes
group_time_combined <- map_dfr(cs_processed, ~ .x$group_time)
dynamic_combined <- map_dfr(cs_processed, ~ .x$dynamic)
simple_combined <- map_dfr(cs_processed, ~ .x$simple)

cat("Results extracted for", length(unique(dynamic_combined$outcome)), "outcomes\n")
cat("Group-time effects extracted for", length(unique(group_time_combined$outcome)), "outcomes\n")

# 7. Create event study plots using ggdid -----
cat("\n=== CREATING EVENT STUDY PLOTS ===\n")

# Function to create event study plot using ggdid
create_event_study_plot <- function(outcome_var) {
  
  cs_result <- cs_results_all[[outcome_var]]
  dynamic_result <- cs_result$dynamic
  
  p <- ggdid(dynamic_result, ylim = NULL) +
    labs(
      title = paste("Event Study:", str_to_title(str_replace_all(outcome_var, "_", " "))),
      subtitle = "Callaway-Sant'Anna (2021) Estimator",
      x = "Years Relative to Public Housing Opening",
      y = "Average Treatment Effect",
      caption = "95% simultaneous confidence intervals"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  return(p)
}

# Create plots for main outcomes using ggdid
event_study_plots <- purrr::map(outcome_vars, create_event_study_plot)
names(event_study_plots) <- outcome_vars

# Display black share plot
print(event_study_plots$black_share)

# 7b. Create panel plots using ggdid plots -----
cat("\n=== CREATING PANEL PLOTS ===\n")

# Load required library for combining plots
library(patchwork)

# Define outcome groups for panel plots (matching your matched DiD structure)
plot1_outcomes <- c(
  "asinh_pop_total", 
  "black_share",
  "asinh_pop_white",
  "asinh_pop_black"
)

plot2_outcomes <- c(
  "asinh_median_rent_calculated",
  "lfp_rate", 
  "unemp_rate",
  "asinh_median_income"
)

# Function to create clean labels for outcomes
clean_outcome_labels <- function(outcome) {
  case_when(
    outcome == "black_share" ~ "Black Share",
    outcome == "asinh_pop_total" ~ "Log Total Population",
    outcome == "asinh_pop_white" ~ "Log White Population", 
    outcome == "asinh_pop_black" ~ "Log Black Population",
    outcome == "asinh_median_income" ~ "Log Median Income",
    outcome == "asinh_median_rent_calculated" ~ "Log Median Rent",
    outcome == "asinh_median_home_value_calculated" ~ "Log Home Value",
    outcome == "unemp_rate" ~ "Unemployment Rate",
    outcome == "lfp_rate" ~ "Labor Force Participation",
    TRUE ~ str_to_title(str_replace_all(outcome, "_", " "))
  )
}

# Function to create panel plots using existing ggdid plots
create_cs_panel_plot <- function(outcome_list, title_text) {
  
  # Get the ggdid plots for selected outcomes and add clean titles
  plots_list <- map(outcome_list, function(outcome) {
    p <- event_study_plots[[outcome]]
    clean_label <- clean_outcome_labels(outcome)
    p + labs(title = clean_label, subtitle = "", caption = "") + 
      theme(plot.title = element_text(size = 11, hjust = 0.5))
  })
  
  # Combine plots using patchwork
  combined_plot <- wrap_plots(plots_list, ncol = 2) +
    plot_annotation(
      title = title_text,
      subtitle = "Callaway-Sant'Anna (2021) Estimator",
      caption = "95% simultaneous confidence intervals",
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12)
      )
    )
  
  return(combined_plot)
}

# Create panel plots using ggdid plots
panel_plot_1 <- create_cs_panel_plot(
  outcome_list = plot1_outcomes,
  title_text = "Population and Racial Composition Effects"
)

panel_plot_2 <- create_cs_panel_plot(
  outcome_list = plot2_outcomes, 
  title_text = "Economic and Housing Effects"
)

# Display panel plots
print(panel_plot_1)
print(panel_plot_2)

# 7a. Create group-time plots -----
cat("\n=== CREATING GROUP-TIME PLOTS ===\n")

# Function to create group-time heatmap
create_group_time_plot <- function(outcome_var) {
  
  plot_data <- group_time_combined %>%
    filter(outcome == outcome_var, group > 0) %>%  # Exclude never-treated
    # Create significance indicator
    mutate(
      significant = ifelse(abs(att) > 1.96 * se, "Significant", "Not Significant"),
      group_factor = as.factor(group),
      time_factor = as.factor(time_period)
    )
  
  if (nrow(plot_data) == 0) {
    # Return empty plot if no data
    return(ggplot() + labs(title = paste("No Group-Time Data for", outcome_var)))
  }
  
  p <- ggplot(plot_data, aes(x = time_period, y = group_factor, fill = att)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.3f", att)), color = "black", size = 3) +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "blue",
      midpoint = 0, name = "ATT"
    ) +
    labs(
      title = paste("Group-Time Effects:", str_to_title(str_replace_all(outcome_var, "_", " "))),
      subtitle = "Callaway-Sant'Anna (2021) - Treatment Cohort by Time Period",
      x = "Time Period",
      y = "Treatment Cohort (First Treatment Year)",
      caption = "Numbers show point estimates"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

# Function to create group-time line plot (alternative visualization)
create_group_time_lines <- function(outcome_var) {
  
  plot_data <- group_time_combined %>%
    filter(outcome == outcome_var, group > 0) %>%
    mutate(group_factor = as.factor(group))
  
  if (nrow(plot_data) == 0) {
    return(ggplot() + labs(title = paste("No Group-Time Data for", outcome_var)))
  }
  
  p <- ggplot(plot_data, aes(x = time_period, y = att, color = group_factor)) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed", alpha = 0.5) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = group_factor), 
                alpha = 0.2, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Group-Time Effects by Cohort:", str_to_title(str_replace_all(outcome_var, "_", " "))),
      subtitle = "Callaway-Sant'Anna (2021) - Each Line is a Treatment Cohort",
      x = "Time Period",
      y = "Average Treatment Effect",
      color = "Treatment\nCohort",
      fill = "Treatment\nCohort",
      caption = "95% confidence intervals shown"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  return(p)
}

# Create group-time plots for main outcomes
group_time_heatmaps <- purrr::map(outcome_vars, create_group_time_plot)
names(group_time_heatmaps) <- outcome_vars

group_time_lines <- purrr::map(outcome_vars, create_group_time_lines)
names(group_time_lines) <- outcome_vars

# Display example group-time plots
print(group_time_heatmaps$black_share)
print(group_time_lines$black_share)

# 8. Save results -----
cat("\n=== SAVING RESULTS ===\n")

# Create output directory
output_dir <- here("output", "regression_results", "callaway_santanna")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save event study plots
for (outcome in names(event_study_plots)) {
  ggsave(
    filename = file.path(output_dir, paste0("event_study_", outcome, "_cs.pdf")),
    plot = event_study_plots[[outcome]],
    width = 10, height = 6, units = "in"
  )
}

# Save panel plots
ggsave(
  filename = file.path(output_dir, "cs_panel_population_demographics.pdf"),
  plot = panel_plot_1,
  width = 12, height = 8, units = "in"
)

ggsave(
  filename = file.path(output_dir, "cs_panel_economic_housing.pdf"),
  plot = panel_plot_2,
  width = 12, height = 8, units = "in"
)

# Save group-time plots
for (outcome in names(group_time_heatmaps)) {
  # Heatmap version
  ggsave(
    filename = file.path(output_dir, paste0("group_time_heatmap_", outcome, "_cs.pdf")),
    plot = group_time_heatmaps[[outcome]],
    width = 12, height = 8, units = "in"
  )
  
  # Line plot version
  ggsave(
    filename = file.path(output_dir, paste0("group_time_lines_", outcome, "_cs.pdf")),
    plot = group_time_lines[[outcome]],
    width = 10, height = 6, units = "in"
  )
}

# Save results as CSV
write_csv(group_time_combined, file.path(output_dir, "cs_group_time_results.csv"))
write_csv(dynamic_combined, file.path(output_dir, "cs_dynamic_results.csv"))
write_csv(simple_combined, file.path(output_dir, "cs_simple_results.csv"))

# 9. Create summary tables -----
cat("\n=== CREATING SUMMARY TABLES ===\n")

# Simple ATT table
simple_table <- simple_combined %>%
  mutate(
    outcome_label = case_when(
      outcome == "black_share" ~ "Black Share",
      outcome == "white_share" ~ "White Share", 
      outcome == "total_pop" ~ "Total Population",
      outcome == "median_income" ~ "Median Income",
      outcome == "median_rent_calculated" ~ "Median Rent",
      outcome == "median_home_value_calculated" ~ "Median Home Value",
      TRUE ~ outcome
    ),
    att_formatted = sprintf("%.4f", att),
    se_formatted = sprintf("(%.4f)", se),
    ci_formatted = sprintf("[%.4f, %.4f]", ci_lower, ci_upper)
  ) %>%
  select(outcome_label, att_formatted, se_formatted, ci_formatted)

# Print summary table
cat("\nSimple Average Treatment Effects:\n")
print(simple_table)

# Save as LaTeX
latex_table <- simple_table %>%
  mutate(
    row = paste(outcome_label, "&", att_formatted, "&", se_formatted, "&", ci_formatted, "\\\\")
  ) %>%
  pull(row)

latex_output <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Outcome & ATT & SE & 95\\% CI \\\\",
  "\\midrule",
  latex_table,
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(latex_output, file.path(output_dir, "cs_simple_results.tex"))

# Group-time summary table
cat("\n=== GROUP-TIME SUMMARY ===\n")

# Create group-time summary for key outcome
group_time_summary <- group_time_combined %>%
  filter(outcome == "black_share", group > 0) %>%
  mutate(
    significant = ifelse(abs(att) > 1.96 * se, "*", ""),
    att_formatted = paste0(sprintf("%.4f", att), significant),
    group_label = paste0("Treated ", group)
  ) %>%
  arrange(group, time_period) %>%
  select(group_label, time_period, att_formatted)

cat("Group-Time Effects for Black Share (significant effects marked with *):\n")
if (nrow(group_time_summary) > 0) {
  print(group_time_summary)
  
  # Save group-time table as CSV for further analysis
  write_csv(group_time_summary, file.path(output_dir, "cs_group_time_summary.csv"))
} else {
  cat("No group-time results available for black_share\n")
}

# 10. Summary statistics -----
cat("\n=== FINAL SUMMARY ===\n")

cat("Analysis completed successfully!\n")
cat("- Outcomes analyzed:", length(unique(dynamic_combined$outcome)), "\n")
cat("- Event time range:", min(dynamic_combined$event_time), "to", max(dynamic_combined$event_time), "\n")
cat("- Observations in final dataset:", nrow(cs_data), "\n")
cat("- Treated tracts:", n_distinct(cs_data$GISJOIN_1950), "\n")

