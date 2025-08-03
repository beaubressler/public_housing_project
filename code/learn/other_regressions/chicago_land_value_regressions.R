# ==============================================================================
# Chicago Land Value Spatial Difference-in-Differences Regressions
# 
# This script runs spatial DiD regressions using pre-prepared Chicago land value
# and public housing data. Implements stacked DiD with Wing (2024) weights.
#
# Prerequisites: Run chicago_land_value_data_prep.R first
# ==============================================================================

# SETUP ========================================================================

# Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(fixest)
  library(ggfixest)
  library(viridis)
})

set.seed(123)

# Configuration
highway_controls_available <- TRUE  # Set based on data prep script

# Output directories
figures_dir <- here("output", "figures", "chicago_land_values")
results_dir <- here("output", "regression_results", "chicago_land_values")
data_dir <- here("output", "data", "chicago_land_values")

# Create directories
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# HELPER FUNCTIONS =============================================================

#' Build fixed effects specification string
#' 
#' @param urban_renewal_treatment How to handle urban renewal: "baseline", "exclude", "control"
#' @param highway_controls Whether to include highway controls
#' @param highway_controls_available Whether highway controls are available in data
#' @param cohort_trends Whether to include cohort-specific linear trends
#' @param baseline_controls Whether to include baseline land value controls
build_fixed_effects <- function(urban_renewal_treatment, highway_controls, highway_controls_available, cohort_trends = FALSE, baseline_controls = FALSE) {
  
  # Base fixed effects: project-year interactions (crucial for stacked DiD)
  base_fe <- "project_id^year + project_id^location_type"
  
  # Add urban renewal controls
  if (urban_renewal_treatment == "control") {
    base_fe <- paste(base_fe, "project_id^in_urban_renewal", sep = " + ")
  }
  
  # Add highway controls if requested and available
  if (highway_controls && highway_controls_available) {
    base_fe <- paste(base_fe, "project_id^has_highway_1km", sep = " + ")
  }
  
  # Add cohort-specific linear trends to address pretrends
  if (cohort_trends) {
    base_fe <- paste(base_fe, "cohort^time_trend", sep = " + ")
  }
  
  # Add baseline controls as fixed effects
  if (baseline_controls) {
    base_fe <- paste(base_fe, "baseline_llv^location_type", sep = " + ")
  }
  
  return(base_fe)
}


#' Run spatial DiD regression with ALL rings in single regression
run_spatial_did_all_rings <- function(data, urban_renewal_treatment = "baseline", 
                                     highway_controls = FALSE, cohort_trends = FALSE, baseline_controls = FALSE, period_filter = NULL, spec_name = "All Rings") {
  
  # Apply time filters but keep ALL location types
  analysis_data <- data %>%
    filter(event_time >= -3, event_time <= 5)
  
  # Apply period filter if specified
  if (!is.null(period_filter)) {
    if (period_filter$type == "early") {
      analysis_data <- analysis_data %>% filter(treatment_year <= period_filter$cutoff)
    } else if (period_filter$type == "late") {
      analysis_data <- analysis_data %>% filter(treatment_year > period_filter$cutoff)
    }
  }
  
  # Handle urban renewal exclusion
  if (urban_renewal_treatment == "exclude") {
    analysis_data <- analysis_data %>% filter(!in_urban_renewal)
  }
  
  # Build fixed effects specification using helper function
  fixed_effects <- build_fixed_effects(urban_renewal_treatment, highway_controls, highway_controls_available, cohort_trends, baseline_controls)
  
  # Build main formula (baseline controls now handled in fixed effects)
  main_formula <- "llv ~ i(event_time, location_type, ref = -1, ref2 = 'control_800_1000')"
  
  # Run regression
  cat(paste("\n=== ", spec_name, " (Single Regression) ===\n"))
  cat("Sample size:", nrow(analysis_data), "observations\n")
  if (baseline_controls) {
    cat("Using baseline controls: ring-specific baseline land value effects\n")
    baseline_available <- sum(!is.na(analysis_data$baseline_llv))
    cat("Observations with baseline values:", baseline_available, "of", nrow(analysis_data), "\n")
  }
  
  # Check available event times and location types
  cat("Location types included:", paste(unique(analysis_data$location_type), collapse = ", "), "\n")
  available_event_times <- sort(unique(analysis_data$event_time))
  cat("Available event times:", paste(available_event_times, collapse = ", "), "\n")
  
  if(!(-1 %in% available_event_times)) {
    stop("Reference period -1 not found in data. Available event times: ", paste(available_event_times, collapse = ", "))
  }
  
  cat("Using reference: event_time = -1, location_type = control_800_1000\n")
  
  # Run single regression with all rings
  model <- feols(
    fml = as.formula(paste(main_formula, "|", fixed_effects)),
    data = analysis_data,
    weights = ~ wing_weight,
    cluster = ~ project_id
  )
  
  cat("Regression completed\n")
  
  # Show sample sizes by ring
  ring_counts <- analysis_data %>%
    group_by(location_type) %>%
    summarise(n = n(), .groups = "drop")
  cat("\nObservations by ring:\n")
  print(ring_counts)
  
  return(model)
}

#' Extract coefficients from all-rings regression for plotting
extract_all_rings_coefficients <- function(model) {
  coeffs <- coef(model)
  ses <- se(model)
  
  # Verify coefficients exist
  if (length(coeffs) == 0) {
    warning("No coefficients found in model")
    return(tibble())
  }
  
  # Get all ring types (excluding control) - 200m bands up to 800m
  ring_types <- c("ring_0_200", "ring_200_400", "ring_400_600", "ring_600_800")
  
  # Extract coefficients for each ring type
  all_ring_data <- map_dfr(ring_types, function(ring) {
    ring_idx <- str_detect(names(coeffs), paste0("location_type::", ring))
    if(any(ring_idx)) {
      ring_coefs <- coeffs[ring_idx]
      ring_ses <- ses[ring_idx]
      ring_times <- as.numeric(str_extract(names(ring_coefs), "-?[0-9]+"))
      
      tibble(
        event_time = ring_times,
        estimate = ring_coefs,
        se = ring_ses,
        location_type = ring
      )
    } else {
      warning(paste("No coefficients found for ring:", ring))
      tibble()
    }
  })
  
  # Add reference points for all rings at event_time = -1
  plot_data <- all_ring_data %>%
    bind_rows(
      tibble(
        event_time = -1,
        estimate = 0,
        se = 0,
        location_type = ring_types
      )
    ) %>%
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    ) %>%
    arrange(location_type, event_time)
  
  return(plot_data)
}

#' Create event study plot from regression results
create_event_study_plot <- function(model, title_suffix = "", all_rings = FALSE) {
  
  if(all_rings) {
    plot_data <- extract_all_rings_coefficients(model)
  } else {
    # Original extraction logic for separate regressions
    coeffs <- coef(model)
    ses <- se(model)
    
    # Get all event time coefficients
    event_coeffs <- coeffs[str_detect(names(coeffs), "event_time.*:ring_")]
    event_ses <- ses[str_detect(names(ses), "event_time.*:ring_")]
    
    if(length(event_coeffs) > 0) {
      # Extract event times and location types from coefficient names
      coef_info <- str_match(names(event_coeffs), "event_time::(-?[0-9]+):location_type::(ring_[0-9]+_[0-9]+)")
      event_times <- as.numeric(coef_info[,2])
      location_types <- coef_info[,3]
      
      # Create plot data
      plot_data <- tibble(
        event_time = event_times,
        estimate = event_coeffs,
        se = event_ses,
        location_type = location_types
      ) %>%
        # Add reference points at event_time = -1
        bind_rows(
          tibble(
            event_time = -1,
            estimate = 0,
            se = 0,
            location_type = unique(location_types)
          )
        ) %>%
        mutate(
          ci_lower = estimate - 1.96 * se,
          ci_upper = estimate + 1.96 * se
        )
    } else {
      # Fallback for simple two-group comparisons
      plot_data <- tibble(
        event_time = numeric(0),
        estimate = numeric(0),
        se = numeric(0),
        location_type = character(0),
        ci_lower = numeric(0),
        ci_upper = numeric(0)
      )
    }
  }
  
  # Create plot
  ggplot(plot_data, aes(x = event_time, y = estimate, color = location_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
    geom_vline(xintercept = -0.5, linetype = "solid", alpha = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, alpha = 0.7) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("ring_0_200" = "red", "ring_200_400" = "orange", "ring_400_600" = "green", "ring_600_800" = "blue",
                 "treated" = "red", "inner" = "orange", "outer" = "blue"),
      labels = c("ring_0_200" = "0-200m", "ring_200_400" = "200-400m", "ring_400_600" = "400-600m", "ring_600_800" = "600-800m",
                 "treated" = "Treated", "inner" = "Inner", "outer" = "Outer"),
      name = "Distance to Project"
    ) +
    labs(
      title = paste("Effect of Public Housing on Chicago Land Values", title_suffix),
      subtitle = "Stacked Difference-in-Differences Event Study",
      x = "Years Since Public Housing Completion", 
      y = "Effect on Log Land Value",
      caption = "Reference: Control ring (800-1000m). 95% confidence intervals.\nClustered standard errors at project level."
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "bottom"
    )
}

# DATA LOADING =================================================================

cat("=== LOADING PREPARED DATA ===\n")

# Load the prepared analysis dataset
data_path <- here(data_dir, "chicago_land_value_analysis_data.csv")
if (!file.exists(data_path)) {
  stop("Analysis data not found at: ", data_path, "\nPlease run chicago_land_value_data_prep.R first.")
}
event_study_data <- read_csv(data_path)

cat("Analysis data loaded:\n")
cat("Total observations:", nrow(event_study_data), "\n")
cat("Unique grid points:", n_distinct(event_study_data$grid_id), "\n")
cat("Unique projects:", n_distinct(event_study_data$project_id), "\n")
cat("Year range:", min(event_study_data$year), "-", max(event_study_data$year), "\n")

# Check if highway controls are available in the data
highway_controls_available <- "has_highway_1km" %in% names(event_study_data)
cat("Highway controls available:", highway_controls_available, "\n")

# REGRESSION ANALYSIS ==========================================================

cat("\n=== RUNNING SPATIAL DID REGRESSIONS ===\n")

# Skip single-ring specifications - only run all-rings (full) specifications

## All Rings Specifications (Single Regression Approach)
cat("\n=== ALL RINGS SINGLE REGRESSION ===\n")

all_rings_specs <- list(
  list(name = "All Rings Baseline", 
       urban_renewal_treatment = "baseline", highway_controls = FALSE, cohort_trends = FALSE),
  
  list(name = "All Rings Control UR + Highway", 
       urban_renewal_treatment = "control", highway_controls = TRUE, cohort_trends = FALSE),
  
  list(name = "All Rings Exclude UR", 
       urban_renewal_treatment = "exclude", highway_controls = FALSE, cohort_trends = FALSE),
  
  list(name = "All Rings Baseline + Cohort Trends", 
       urban_renewal_treatment = "baseline", highway_controls = FALSE, cohort_trends = TRUE),
  
  list(name = "All Rings Full Controls + Cohort Trends", 
       urban_renewal_treatment = "control", highway_controls = TRUE, cohort_trends = TRUE),
  
  list(name = "All Rings Baseline + Baseline Controls", 
       urban_renewal_treatment = "baseline", highway_controls = FALSE, baseline_controls = TRUE),
  
  list(name = "All Rings Full Controls + Baseline Controls", 
       urban_renewal_treatment = "control", highway_controls = TRUE, baseline_controls = TRUE)
)

all_rings_results <- map2(all_rings_specs, 1:length(all_rings_specs), ~{
  spec_name <- paste("All Rings Specification", .y, ":", .x$name)
  run_spatial_did_all_rings(
    data = event_study_data,
    urban_renewal_treatment = .x$urban_renewal_treatment,
    highway_controls = .x$highway_controls,
    cohort_trends = ifelse(is.null(.x$cohort_trends), FALSE, .x$cohort_trends),
    baseline_controls = ifelse(is.null(.x$baseline_controls), FALSE, .x$baseline_controls),
    spec_name = spec_name
  )
})

names(all_rings_results) <- c("all_rings_baseline", "all_rings_control_ur_highway", "all_rings_exclude_ur", 
                              "all_rings_baseline_cohort_trends", "all_rings_full_cohort_trends",
                              "all_rings_baseline_baseline_controls", "all_rings_full_baseline_controls")

## Heterogeneity Analysis
cat("\n=== HETEROGENEITY ANALYSIS ===\n")

# Calculate median project size across unique projects
project_median_units <- event_study_data %>%
  distinct(project_id, total_units) %>%
  pull(total_units) %>%
  median(na.rm = TRUE)

# Create heterogeneity indicators
event_study_data_hetero <- event_study_data %>%
  mutate(
    # Project size categories - 500-unit threshold
    project_size_category_500 = case_when(
      total_units < 500 ~ "below_500",
      total_units >= 500 ~ "above_500"
    ),
    # Project size categories - median split
    project_size_category_median = case_when(
      total_units <= project_median_units ~ "below_median",
      total_units > project_median_units ~ "above_median"
    ),
    # Treatment period categories - split at 1961 to balance sample sizes
    period_category = case_when(
      treatment_year <= 1961 ~ "early",  # Early: 1939, 1949, 1961
      treatment_year > 1961 ~ "late"     # Late: 1971 only
    )
  )

# Project size heterogeneity - Multiple specifications
cat("Project size distribution:\n")
cat("Median project size (across unique projects):", project_median_units, "units\n")

# 500-unit threshold analysis
cat("\n=== 500-UNIT THRESHOLD ANALYSIS ===\n")
size_500_counts <- event_study_data_hetero %>%
  distinct(project_id, total_units, project_size_category_500) %>%
  count(project_size_category_500, name = "n_projects")
print(size_500_counts)

size_hetero_500_results <- list()
for(size_cat in c("below_500", "above_500")) {
  size_data <- event_study_data_hetero %>% filter(project_size_category_500 == size_cat)
  
  cat("Checking", size_cat, "projects: sample size =", nrow(size_data), "\n")
  
  if(nrow(size_data) > 100) {  # Only run if sufficient data
    size_hetero_500_results[[size_cat]] <- run_spatial_did_all_rings(
      data = size_data,
      urban_renewal_treatment = "baseline",
      highway_controls = FALSE,
      spec_name = paste("500-unit threshold:", size_cat)
    )
  } else {
    cat("Skipping", size_cat, "projects: insufficient data (n =", nrow(size_data), ")\n")
  }
}

# Median split analysis
cat("\n=== MEDIAN SPLIT ANALYSIS ===\n")
size_median_counts <- event_study_data_hetero %>%
  distinct(project_id, total_units, project_size_category_median) %>%
  count(project_size_category_median, name = "n_projects")
print(size_median_counts)

size_hetero_median_results <- list()
for(size_cat in c("below_median", "above_median")) {
  size_data <- event_study_data_hetero %>% filter(project_size_category_median == size_cat)
  
  cat("Checking", size_cat, "projects: sample size =", nrow(size_data), "\n")
  
  if(nrow(size_data) > 100) {  # Only run if sufficient data
    size_hetero_median_results[[size_cat]] <- run_spatial_did_all_rings(
      data = size_data,
      urban_renewal_treatment = "baseline",
      highway_controls = FALSE,
      spec_name = paste("Median split:", size_cat, paste0("(median = ", project_median_units, " units)"))
    )
  } else {
    cat("Skipping", size_cat, "projects: insufficient data (n =", nrow(size_data), ")\n")
  }
}

# Treatment period heterogeneity - Updated for 200m bands (Pre/Post 1960)
period_hetero_results <- list()
for(period_cat in c("early", "late")) {
  period_data <- event_study_data_hetero %>% filter(period_category == period_cat)
  
  period_label <- ifelse(period_cat == "early", "Pre-1971 (1939,1949,1961)", "1971 Only")
  cat("Checking", period_label, "period: sample size =", nrow(period_data), "\n")
  
  if(nrow(period_data) > 100) {  # Only run if sufficient data
    period_hetero_results[[period_cat]] <- run_spatial_did_all_rings(
      data = period_data,
      urban_renewal_treatment = "control", 
      highway_controls = highway_controls_available,
      spec_name = paste("Treatment Period:", period_label)
    )
  } else {
    cat("Skipping", period_label, "period: insufficient data (n =", nrow(period_data), ")\n")
  }
}

## Continuous Size Interaction Analysis
cat("\n=== CONTINUOUS SIZE INTERACTION ANALYSIS ===\n")

# Create continuous size variable (scaled by 100 for interpretability)
event_study_data_size_continuous <- event_study_data %>%
  mutate(total_units_hundreds = total_units / 100)

cat("Project size range:", min(event_study_data_size_continuous$total_units), "to", max(event_study_data_size_continuous$total_units), "units\n")
cat("Scaled size range:", round(min(event_study_data_size_continuous$total_units_hundreds), 2), "to", round(max(event_study_data_size_continuous$total_units_hundreds), 2), "hundreds of units\n")

# Run continuous size interaction specification
size_continuous_result <- run_spatial_did_all_rings(
  data = event_study_data_size_continuous,
  urban_renewal_treatment = "baseline",
  highway_controls = FALSE,
  spec_name = "Continuous Size Interaction (units in hundreds)"
)

# Create custom regression for size interaction
cat("\n=== RUNNING CONTINUOUS SIZE INTERACTION REGRESSION ===\n")

# Prepare data for size interaction
size_interaction_data <- event_study_data_size_continuous %>%
  filter(event_time >= -3, event_time <= 5)

cat("Sample size for size interaction:", nrow(size_interaction_data), "observations\n")
cat("Project size variation: mean =", round(mean(size_interaction_data$total_units_hundreds), 2), 
    ", sd =", round(sd(size_interaction_data$total_units_hundreds), 2), "hundreds of units\n")

# Build the interaction formula
# Main effects: event_time × location_type (as in baseline)
# Interaction: (event_time × location_type) × total_units_hundreds
size_interaction_formula <- "llv ~ i(event_time, location_type, ref = -1, ref2 = 'control_800_1000') * total_units_hundreds"
size_interaction_fe <- "project_id^year + project_id^location_type"

cat("Running size interaction regression...\n")
size_interaction_model <- feols(
  fml = as.formula(paste(size_interaction_formula, "|", size_interaction_fe)),
  data = size_interaction_data,
  weights = ~ wing_weight,
  cluster = ~ project_id
)

cat("Size interaction regression completed\n")

# RESULTS OUTPUT ===============================================================

cat("\n=== SAVING RESULTS ===\n")

# Create all-rings plot
all_rings_plot <- create_event_study_plot(all_rings_results$all_rings_baseline, 
                                         title_suffix = " (All Rings)", 
                                         all_rings = TRUE)
ggsave(here(figures_dir, "chicago_land_value_event_study_all_rings.pdf"), all_rings_plot, width = 10, height = 6)

# Save all-rings regression tables with different specifications
etable(all_rings_results$all_rings_baseline, all_rings_results$all_rings_control_ur_highway, all_rings_results$all_rings_exclude_ur,
       tex = TRUE, replace = TRUE,
       file = here(results_dir, "chicago_land_value_all_rings_main_results.tex"),
       title = "Effect of Public Housing on Chicago Land Values - All Rings Main Specifications",
       label = "tab:chicago_all_rings_main")

# Save all-rings robustness with highway controls only
if(highway_controls_available) {
  # Create highway-only all rings specification
  all_rings_highway_only <- run_spatial_did_all_rings(
    data = event_study_data,
    urban_renewal_treatment = "baseline",
    highway_controls = TRUE,
    spec_name = "All Rings Highway Controls Only"
  )
  
  etable(all_rings_results$all_rings_baseline, all_rings_highway_only,
         tex = TRUE, replace = TRUE,
         file = here(results_dir, "chicago_land_value_all_rings_highway_robustness.tex"),
         title = "Effect of Public Housing on Chicago Land Values - All Rings Highway Robustness",
         label = "tab:chicago_all_rings_highway")
}

# Note: Cohort trends tables removed since we're only using all-rings specifications

# Save all-rings cohort trends comparison table
etable(all_rings_results$all_rings_baseline, all_rings_results$all_rings_baseline_cohort_trends, all_rings_results$all_rings_full_cohort_trends,
       tex = TRUE, replace = TRUE,
       file = here(results_dir, "chicago_land_value_all_rings_cohort_trends.tex"),
       title = "Effect of Public Housing on Chicago Land Values - All Rings with Cohort Trends",
       label = "tab:chicago_all_rings_cohort_trends")

# Note: Single-ring baseline controls tables removed since we're only using all-rings specifications

# Save all-rings baseline controls comparison table
etable(all_rings_results$all_rings_baseline, all_rings_results$all_rings_baseline_baseline_controls, all_rings_results$all_rings_full_baseline_controls,
       tex = TRUE, replace = TRUE,
       file = here(results_dir, "chicago_land_value_all_rings_baseline_controls.tex"),
       title = "Effect of Public Housing on Chicago Land Values - All Rings with Baseline Controls",
       label = "tab:chicago_all_rings_baseline_controls")

# Save heterogeneity results - 500-unit threshold
if(length(size_hetero_500_results) > 0) {
  etable(size_hetero_500_results,
         tex = TRUE, replace = TRUE,
         file = here(results_dir, "chicago_land_value_heterogeneity_size_500.tex"),
         title = "Heterogeneity by Project Size - 500 Unit Threshold",
         label = "tab:chicago_hetero_size_500")
}

# Save heterogeneity results - median split
if(length(size_hetero_median_results) > 0) {
  etable(size_hetero_median_results,
         tex = TRUE, replace = TRUE,
         file = here(results_dir, "chicago_land_value_heterogeneity_size_median.tex"),
         title = "Heterogeneity by Project Size - Median Split",
         label = "tab:chicago_hetero_size_median")
}

if(length(period_hetero_results) > 0) {
  etable(period_hetero_results,
         tex = TRUE, replace = TRUE,
         file = here(results_dir, "chicago_land_value_heterogeneity_period.tex"),
         title = "Heterogeneity by Treatment Period",
         label = "tab:chicago_hetero_period")
}

# Save continuous size interaction results
if(exists("size_interaction_model")) {
  etable(size_interaction_model,
         tex = TRUE, replace = TRUE,
         file = here(results_dir, "chicago_land_value_size_continuous_interaction.tex"),
         title = "Continuous Size Interaction Effects on Chicago Land Values",
         label = "tab:chicago_size_continuous",
         notes = "Project size measured in hundreds of units. Coefficients on interaction terms show how treatment effects vary with project size.")
  
  cat("Continuous size interaction results saved\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to:\n")
cat("- Figures:", figures_dir, "\n")
cat("- Regression tables:", results_dir, "\n")
cat("- Analysis data:", data_dir, "\n")

cat("\nKey findings:\n")
analysis_sample <- event_study_data %>% filter(event_time >= -3, event_time <= 5)
cat("Main sample size:", nrow(analysis_sample), "observations\n")
cat("Number of public housing projects:", n_distinct(event_study_data$project_id), "\n")
cat("Treatment periods:", paste(sort(unique(event_study_data$treatment_year)), collapse = ", "), "\n")
cat("Project size range:", min(event_study_data$total_units), "to", max(event_study_data$total_units), "units\n")
cat("Highway controls used:", highway_controls_available, "\n")
cat("\nAnalyses completed:\n")
cat("- Main all-rings specifications\n")
cat("- Heterogeneity by project size (500-unit threshold)\n")
cat("- Heterogeneity by project size (median split)\n")
cat("- Heterogeneity by treatment period\n")
cat("- Continuous size interaction effects\n")

