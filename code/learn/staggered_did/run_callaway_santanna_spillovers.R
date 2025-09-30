####
# Callaway and Sant'Anna (2021) Spillover Effects Analysis
# Analyzes spillover effects on inner ring tracts using CS methodology
####

library(did)
library(tidyverse)
library(here)
library(sf)

rm(list = ls())

# 0. Setup -----
data_type <- "combined"
set.seed(123456L)

# 1. Load data -----
cat("Loading spillover data...\n")

# Load main census data
balanced_data_raw <- st_read(here("data", "derived", "merged", data_type, 
                                  "census_tract_sample_with_treatment_status_balanced.gpkg"))

# Load ring structure data
tracts_and_rings_full <- read_csv(here("data", "derived", "merged", data_type, 
                                       "unique_tracts_and_rings.csv"))

# Identify directly treated tracts to exclude
treated_tracts <- tracts_and_rings_full %>%
  filter(location_type == "treated") %>%
  distinct(GISJOIN_1950) %>%
  pull(GISJOIN_1950)

cat("Found", length(treated_tracts), "directly treated tracts to exclude from spillover analysis\n")

# Focus on inner rings for spillover analysis
tracts_and_rings <- tracts_and_rings_full %>%
  filter(location_type == "inner") %>%
  # Rename treatment_year to spillover_year for clarity
  rename(spillover_year = treatment_year)

cat("Ring data loaded with", nrow(tracts_and_rings), "inner ring observations\n")
cat("Spillover treatment years:", paste(sort(unique(tracts_and_rings$spillover_year)), collapse = ", "), "\n")

# 2. Create spillover analysis dataset -----
cat("\n=== CREATING SPILLOVER DATASET ===\n")

# Create spillover dataset with inner rings as "treated" units
spillover_data <- balanced_data_raw %>%
  st_drop_geometry() %>%
  # Exclude directly treated tracts (they get direct treatment, not spillovers)
  filter(!GISJOIN_1950 %in% treated_tracts) %>%
  # Rename YEAR to year for CS package
  rename(year = YEAR) %>%
  # Add spillover treatment status
  left_join(tracts_and_rings %>% select(GISJOIN_1950, spillover_year), 
            by = "GISJOIN_1950") %>%
  # Create spillover indicator and gname
  mutate(
    spillover_treated = ifelse(!is.na(spillover_year), 1, 0),
    gname = case_when(
      is.na(spillover_year) ~ 0,  # Never affected by spillover
      TRUE ~ spillover_year       # First spillover year
    )
  ) %>%
  # Remove missing outcome data
  filter(!is.na(black_share)) %>%
  # Create transformed variables
  mutate(
    asinh_pop_total = asinh(total_pop),
    asinh_pop_white = asinh(white_pop),
    asinh_pop_black = asinh(black_pop),
    asinh_median_income = asinh(median_income),
    asinh_median_rent_calculated = asinh(median_rent_calculated),
    asinh_median_home_value_calculated = asinh(median_home_value_calculated),
    asinh_distance_from_cbd = asinh(distance_from_cbd),
    ln_population_density = log(population_density),
    # Create factors for fixed effects
    county_factor = as.factor(state_county),
    state_factor = as.factor(STATEA),
    cbsa_factor = as.factor(cbsa_title)
  ) %>%
  # Create numeric ID for CS package
  group_by(GISJOIN_1950) %>%
  mutate(numeric_id = cur_group_id()) %>%
  ungroup()

cat("Spillover data prepared with", nrow(spillover_data), "observations\n")
cat("Unique tracts:", n_distinct(spillover_data$GISJOIN_1950), "\n") 
cat("Years:", paste(sort(unique(spillover_data$year)), collapse = ", "), "\n")
cat("Directly treated tracts excluded:", length(treated_tracts), "\n")

# Summary of spillover treatment groups
gname_summary <- spillover_data %>%
  distinct(GISJOIN_1950, gname) %>%
  count(gname, sort = TRUE)

cat("\nSpillover treatment group sizes (gname):\n")
print(gname_summary)

# 3. Create 1940 baseline characteristics -----
cat("\n=== CREATING 1940 BASELINE CHARACTERISTICS ===\n")

baseline_vars <- c("black_share", "white_share", 
                   "total_pop", "asinh_pop_total",
                   "median_income",  "asinh_median_income",
                   "median_rent_calculated", "asinh_median_rent_calculated",
                   "black_pop", "asinh_pop_black",
                   "unemp_rate", "lfp_rate")

baseline_1940 <- spillover_data %>%
  filter(year == 1940) %>%
  select(GISJOIN_1950, all_of(baseline_vars)) %>%
  rename_with(~ paste0(.x, "_1940"), .cols = all_of(baseline_vars)) %>%
  distinct()

cat("1940 baseline data extracted for", nrow(baseline_1940), "tracts\n")

# Merge baseline characteristics back
spillover_data <- spillover_data %>%
  left_join(baseline_1940, by = "GISJOIN_1950")

# 4. Run Callaway-Sant'Anna spillover analysis -----
cat("\n=== RUNNING CALLAWAY-SANT'ANNA SPILLOVER ESTIMATION ===\n")

# Define outcome variables for spillover analysis
outcome_vars <- c("black_share", "asinh_pop_total", "asinh_pop_white", "asinh_pop_black",
                  "asinh_median_income", "asinh_median_rent_calculated", "asinh_median_home_value_calculated",
                  "unemp_rate", "lfp_rate")

# Define control formula for spillover analysis
control_formula <- ~ asinh_distance_from_cbd + cbsa_factor + redlined_binary_80pp + ur_binary_5pp + 
  black_share_1940 + asinh_median_income_1940 +
  asinh_pop_total_1940 + asinh_pop_black_1940 + 
  asinh_median_rent_calculated_1940 + unemp_rate_1940 + lfp_rate_1940

# Function to run CS spillover analysis
run_cs_spillover <- function(outcome_var, data = spillover_data) {
  
  cat("\n--- Analyzing spillover effects on", outcome_var, "---\n")
  
  # Filter out missing outcome data
  analysis_data <- data %>% filter(!is.na(.data[[outcome_var]]))
  cat("Complete observations:", nrow(analysis_data), "\n")
  
  # Run att_gt for spillover effects
  cs_results <- att_gt(
    yname = outcome_var,
    tname = "year", 
    idname = "numeric_id",
    gname = "gname",
    xformla = control_formula,
    data = analysis_data,
    control_group = "notyettreated",
    base_period =  "universal",
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

# Run spillover analysis for all outcomes
cat("Running CS spillover analysis for", length(outcome_vars), "outcome variables...\n")

cs_spillover_results <- purrr::map(outcome_vars, run_cs_spillover)
names(cs_spillover_results) <- outcome_vars

# 5. Process spillover results -----
cat("\n=== PROCESSING SPILLOVER RESULTS ===\n")

# Function to extract spillover results
extract_spillover_results <- function(cs_result) {
  
  cat("Processing spillover outcome:", cs_result$outcome, "\n")
  
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
        outcome = cs_result$outcome,
        group_label = case_when(
          group == 0 ~ "Never Treated",
          TRUE ~ paste0("Spillover ", group)
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

# Extract spillover results for all outcomes
cs_spillover_processed <- purrr::map(cs_spillover_results, extract_spillover_results)

# Combine results across outcomes
spillover_dynamic_combined <- map_dfr(cs_spillover_processed, ~ .x$dynamic)
spillover_simple_combined <- map_dfr(cs_spillover_processed, ~ .x$simple)

cat("Spillover results extracted for", length(unique(spillover_dynamic_combined$outcome)), "outcomes\n")

# 6. Create spillover event study plots using ggdid -----
cat("\n=== CREATING SPILLOVER EVENT STUDY PLOTS ===\n")

# Function to create spillover event study plot using ggdid
create_spillover_plot <- function(outcome_var) {
  
  cs_result <- cs_spillover_results[[outcome_var]]
  dynamic_result <- cs_result$dynamic
  
  p <- ggdid(dynamic_result, ylim = NULL) +
    labs(
      title = paste("Spillover Effects:", str_to_title(str_replace_all(outcome_var, "_", " "))),
      subtitle = "Callaway-Sant'Anna (2021) Estimator - Inner Ring Analysis",
      x = "Years Relative to Nearby Public Housing Opening",
      y = "Spillover Effect",
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
spillover_plots <- purrr::map(outcome_vars, create_spillover_plot)
names(spillover_plots) <- outcome_vars

# Display black share spillover plot
print(spillover_plots$black_share)

# 6b. Create spillover panel plots using ggdid plots -----
cat("\n=== CREATING SPILLOVER PANEL PLOTS ===\n")

# Load required library for combining plots
library(patchwork)

# Define outcome groups for spillover panel plots
spillover_plot1_outcomes <- c(
  "asinh_pop_total", 
  "black_share",
  "asinh_pop_white",
  "asinh_pop_black"
)

spillover_plot2_outcomes <- c(
  "asinh_median_rent_calculated",
  "asinh_median_home_value_calculated", 
  "unemp_rate",
  "asinh_median_income"
)

# Function to create clean labels for outcomes
clean_spillover_labels <- function(outcome) {
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

# Function to create spillover panel plots using existing ggdid plots
create_spillover_panel_plot <- function(outcome_list, title_text) {
  
  # Get the ggdid plots for selected outcomes and add clean titles
  plots_list <- map(outcome_list, function(outcome) {
    p <- spillover_plots[[outcome]]
    clean_label <- clean_spillover_labels(outcome)
    p + labs(title = clean_label, subtitle = "", caption = "") + 
      theme(plot.title = element_text(size = 11, hjust = 0.5))
  })
  
  # Combine plots using patchwork
  combined_plot <- wrap_plots(plots_list, ncol = 2) +
    plot_annotation(
      title = title_text,
      subtitle = "Callaway-Sant'Anna (2021) Estimator - Inner Ring Analysis",
      caption = "95% simultaneous confidence intervals",
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12)
      )
    )
  
  return(combined_plot)
}

# Create spillover panel plots using ggdid plots
spillover_panel_plot_1 <- create_spillover_panel_plot(
  outcome_list = spillover_plot1_outcomes,
  title_text = "Spillover Effects: Population and Racial Composition"
)

spillover_panel_plot_2 <- create_spillover_panel_plot(
  outcome_list = spillover_plot2_outcomes, 
  title_text = "Spillover Effects: Economic and Housing"
)

# Display spillover panel plots
print(spillover_panel_plot_1)
print(spillover_panel_plot_2)

# 6c. Create spillover group-time plots -----
cat("\n=== CREATING SPILLOVER GROUP-TIME PLOTS ===\n")

# Extract spillover group-time results
spillover_group_time_combined <- map_dfr(cs_spillover_processed, ~ .x$group_time)

# Function to create spillover group-time heatmap
create_spillover_group_time_plot <- function(outcome_var) {
  
  plot_data <- spillover_group_time_combined %>%
    filter(outcome == outcome_var, group > 0) %>%  # Exclude never-treated
    # Create significance indicator
    mutate(
      significant = ifelse(abs(att) > 1.96 * se, "Significant", "Not Significant"),
      group_factor = as.factor(group),
      time_factor = as.factor(time_period)
    )
  
  if (nrow(plot_data) == 0) {
    # Return empty plot if no data
    return(ggplot() + labs(title = paste("No Spillover Group-Time Data for", outcome_var)))
  }
  
  p <- ggplot(plot_data, aes(x = time_period, y = group_factor, fill = att)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.3f", att)), color = "black", size = 3) +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "darkgreen",
      midpoint = 0, name = "Spillover\nATT"
    ) +
    labs(
      title = paste("Spillover Group-Time Effects:", str_to_title(str_replace_all(outcome_var, "_", " "))),
      subtitle = "Callaway-Sant'Anna (2021) - Treatment Cohort by Time Period",
      x = "Time Period",
      y = "Spillover Cohort (First Treatment Year)",
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

# Function to create spillover group-time line plot (alternative visualization)
create_spillover_group_time_lines <- function(outcome_var) {
  
  plot_data <- spillover_group_time_combined %>%
    filter(outcome == outcome_var, group > 0) %>%
    mutate(group_factor = as.factor(group))
  
  if (nrow(plot_data) == 0) {
    return(ggplot() + labs(title = paste("No Spillover Group-Time Data for", outcome_var)))
  }
  
  p <- ggplot(plot_data, aes(x = time_period, y = att, color = group_factor)) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed", alpha = 0.5) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = group_factor), 
                alpha = 0.2, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Spillover Group-Time Effects by Cohort:", str_to_title(str_replace_all(outcome_var, "_", " "))),
      subtitle = "Callaway-Sant'Anna (2021) - Each Line is a Treatment Cohort",
      x = "Time Period",
      y = "Spillover Effect",
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

# Create spillover group-time plots for main outcomes
spillover_group_time_heatmaps <- purrr::map(outcome_vars, create_spillover_group_time_plot)
names(spillover_group_time_heatmaps) <- outcome_vars

spillover_group_time_lines <- purrr::map(outcome_vars, create_spillover_group_time_lines)
names(spillover_group_time_lines) <- outcome_vars

# Display example spillover group-time plots
print(spillover_group_time_heatmaps$black_share)
print(spillover_group_time_lines$black_share)

# 7. Save spillover results -----
cat("\n=== SAVING SPILLOVER RESULTS ===\n")

# Create output directory
output_dir <- here("output", "regression_results", "callaway_santanna_spillovers")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save spillover plots
for (outcome in names(spillover_plots)) {
  ggsave(
    filename = file.path(output_dir, paste0("spillover_", outcome, "_cs.pdf")),
    plot = spillover_plots[[outcome]],
    width = 10, height = 6, units = "in"
  )
}

# Save spillover panel plots
ggsave(
  filename = file.path(output_dir, "cs_spillover_panel_population_demographics.pdf"),
  plot = spillover_panel_plot_1,
  width = 12, height = 8, units = "in"
)

ggsave(
  filename = file.path(output_dir, "cs_spillover_panel_economic_housing.pdf"),
  plot = spillover_panel_plot_2,
  width = 12, height = 8, units = "in"
)

# Save spillover group-time plots
for (outcome in names(spillover_group_time_heatmaps)) {
  # Heatmap version
  ggsave(
    filename = file.path(output_dir, paste0("spillover_group_time_heatmap_", outcome, "_cs.pdf")),
    plot = spillover_group_time_heatmaps[[outcome]],
    width = 12, height = 8, units = "in"
  )
  
  # Line plot version
  ggsave(
    filename = file.path(output_dir, paste0("spillover_group_time_lines_", outcome, "_cs.pdf")),
    plot = spillover_group_time_lines[[outcome]],
    width = 10, height = 6, units = "in"
  )
}

# Save results as CSV
write_csv(spillover_dynamic_combined, file.path(output_dir, "cs_spillover_dynamic_results.csv"))
write_csv(spillover_simple_combined, file.path(output_dir, "cs_spillover_simple_results.csv"))

# 8. Create spillover summary table -----
cat("\n=== CREATING SPILLOVER SUMMARY TABLE ===\n")

spillover_table <- spillover_simple_combined %>%
  mutate(
    outcome_label = case_when(
      outcome == "black_share" ~ "Black Share",
      outcome == "asinh_pop_total" ~ "Log Population",
      outcome == "asinh_median_income" ~ "Log Median Income",
      outcome == "asinh_median_rent_calculated" ~ "Log Median Rent",
      outcome == "asinh_median_home_value_calculated" ~ "Log Home Value",
      outcome == "unemp_rate" ~ "Unemployment Rate",
      outcome == "lfp_rate" ~ "Labor Force Participation",
      TRUE ~ outcome
    ),
    att_formatted = sprintf("%.4f", att),
    se_formatted = sprintf("(%.4f)", se),
    ci_formatted = sprintf("[%.4f, %.4f]", ci_lower, ci_upper)
  ) %>%
  select(outcome_label, att_formatted, se_formatted, ci_formatted)

cat("\nSpillover Average Treatment Effects:\n")
print(spillover_table)

# Save as LaTeX
latex_spillover <- spillover_table %>%
  mutate(
    row = paste(outcome_label, "&", att_formatted, "&", se_formatted, "&", ci_formatted, "\\\\")
  ) %>%
  pull(row)

latex_spillover_output <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Outcome & Spillover ATT & SE & 95\\% CI \\\\",
  "\\midrule",
  latex_spillover,
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(latex_spillover_output, file.path(output_dir, "cs_spillover_results.tex"))

# 9. Final spillover summary -----
cat("\n=== SPILLOVER ANALYSIS SUMMARY ===\n")

cat("Spillover analysis completed successfully!\n")
cat("- Inner ring tracts analyzed:", sum(spillover_data$spillover_treated), "\n")
cat("- Control tracts:", sum(spillover_data$spillover_treated == 0), "\n")
cat("- Outcomes analyzed:", length(unique(spillover_dynamic_combined$outcome)), "\n")

# Print key spillover result
black_spillover <- spillover_simple_combined %>% filter(outcome == "black_share")
cat("\nKey spillover result - Black Share ATT:", round(black_spillover$att, 4), 
    "(SE:", round(black_spillover$se, 4), ")\n")

cat("\nSpillover results saved to:", output_dir, "\n")
