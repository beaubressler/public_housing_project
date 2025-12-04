## 03_heterogeneity_analysis.R
## Heterogeneity analysis of treatment effects at t=20
## Requires 01_setup_and_models.R to be run first
##
## FLEXIBILITY: This script supports multiple variants and datasets
##
## Usage examples:
## # Default (1-year matching, baseline variant)
## source("01_setup_and_models.R")
## source("03_heterogeneity_analysis.R")
##
## # Use 2-year matching
## MATCHED_DATASET <- "tract_data_matched_2_year.csv"
## source("01_setup_and_models.R")
## source("03_heterogeneity_analysis.R")
##
## # Use genetic matching with different variant
## VARIANT <- "no_cbsa"
## MATCHED_DATASET <- "tract_data_matched_2_year_genetic.csv"
## source("01_setup_and_models.R")
## source("03_heterogeneity_analysis.R")
##
## # Use inherited spillovers with replacement
## MATCHED_DATASET <- "tract_data_inherited_spillovers_2_year_replacement.csv"
## source("01_setup_and_models.R")
## source("03_heterogeneity_analysis.R")
##
## Output structure:
## output/baseline/heterogeneity/combined/baseline/1_year/
## output/baseline/heterogeneity/combined/baseline/2_year/
## output/baseline/heterogeneity/combined/no_cbsa/2_year_genetic/
##
## Available datasets:
## - tract_data_matched_1_year.csv (default)
## - tract_data_matched_2_year.csv
## - tract_data_matched_1_year_with_replacement.csv
## - tract_data_matched_2_year_replacement.csv
## - tract_data_matched_2_year_genetic.csv
## - tract_data_inherited_spillovers_2_year_replacement.csv
##
## Available variants: "baseline" (default), "no_cbsa"

if (!exists("PART1_DONE") || !PART1_DONE) {
  message("Running Part 1 first...")
  source(here("code", "learn", "matched_did", "01_setup_and_models.R"))
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(modelsummary)
  library(tinytable)
})

# Load color palettes
source(here("code", "helpers", "color_palettes.R"))

# Source helper functions
source(here("code", "learn", "matched_did", "helpers", "heterogeneity_analysis_functions.R"))

message("\n=== PART 3: HETEROGENEITY ANALYSIS ===")

# ---- switches ----
VARIANT <- get0("VARIANT", ifnotfound = "baseline")   # "baseline" | "no_cbsa"
MATCHED_DATASET <- get0("MATCHED_DATASET", ifnotfound = "tract_data_matched_1_year.csv")

# ---- basic knobs ----
data_type <- get0("data_type", ifnotfound = "combined")

# ---- full event study plot controls ----
# Set to TRUE to generate full event study plots (all time periods) for specific heterogeneity analyses
# FALSE = only generate t=20 coefficient plots (default behavior)
FULL_EVENT_STUDY_PLOTS <- list(
  baseline_black_share = TRUE,    # Neighborhood "tipping" analysis
  project_size_largest = FALSE,
  project_size_total = FALSE,
  baseline_income = FALSE,
  nyc = FALSE,
  urban_renewal = FALSE,
  highrise = FALSE,
  early_late = FALSE
)

# ---- directories ----
match_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset", VARIANT)

# Extract dataset type from filename for organized output
dataset_type <- str_remove(MATCHED_DATASET, "^tract_data_matched_") %>% str_remove("\\.csv$")
# Use local variable name to avoid overwriting results_dir from 01_setup_and_models.R
heterogeneity_results_dir <- here("output", "regression_results", "matched_did", data_type, VARIANT,  "heterogeneity", dataset_type)
dir.create(heterogeneity_results_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Use the dataset loaded by 01_setup ----
# 01_setup loads data into tract_data_matched regardless of which dataset it loads
# We just use that variable (which uses the dynamic matched_csv path from 01)

cat("Using dataset:", MATCHED_DATASET, "\n")
cat("Using variant:", VARIANT, "\n")
cat("Dataset type:", dataset_type, "\n")
cat("Results will be saved to:", heterogeneity_results_dir, "\n")

# ---- Heterogeneity by Project Characteristics ----

cat("\n=== HETEROGENEITY BY PROJECT CHARACTERISTICS ===\n")

# Outcome variables for heterogeneity analysis
heterogeneity_outcome_vars <-
  c("asinh_median_rent_calculated", "asinh_median_income",
    "black_share", "asinh_pop_total", "asinh_pop_black", "asinh_pop_white",
    "black_pop", "white_pop",
    "unemp_rate")

# Outcome variables for heterogeneity coefficient tables
table_outcome_vars <- c(
  "black_share",
  "asinh_median_rent_calculated",
  "asinh_median_income",
  "asinh_pop_total",
  "asinh_pop_black",
  "asinh_pop_white"
)

# Outcome labels for plots
race_income_labels <- c(
  "black_share" = "Black Pop Share",
  "asinh_median_income" = "Median Income (asinh)",
  "asinh_pop_black" = "Black Pop. (asinh)",
  "asinh_pop_white" = "White Pop. (asinh)",
  "black_pop" = "Black Pop.",
  "white_pop" = "White Pop.",
  "asinh_pop_total" = "Total Pop. (asinh)",
  "total_pop" = "Total Pop.",
  "asinh_median_rent_calculated" = "Median Rent (asinh)",
  "unemp_rate" = "Unemployment Rate"
)

# ---- Project Size Heterogeneity ----

# ---- A. Heterogeneity by LARGEST Project Size ----

cat("\n--- Project Size Heterogeneity (LARGEST PROJECT) ---\n")

# Get characteristics for treated tracts - using LARGEST project
treated_tract_chars_largest <- tract_data_matched %>%
  filter(group_type == "treated", location_type == "treated") %>%
  filter(year == matched_treatment_year) %>%
  select(GISJOIN_1950, match_group, matched_treatment_year, largest_original_project_size) %>%
  distinct() %>%
  filter(!is.na(largest_original_project_size)) %>%
  mutate(
    # Calculate terciles based on largest project size
    project_size_group = case_when(
      largest_original_project_size < quantile(largest_original_project_size, 1/3, na.rm = TRUE) ~ "small",
      largest_original_project_size >= quantile(largest_original_project_size, 1/3, na.rm = TRUE) &
        largest_original_project_size < quantile(largest_original_project_size, 2/3, na.rm = TRUE) ~ "medium",
      largest_original_project_size >= quantile(largest_original_project_size, 2/3, na.rm = TRUE) ~ "large"
    )
  )

# Report tercile cutoffs
tertile_1_largest <- quantile(treated_tract_chars_largest$largest_original_project_size, 1/3, na.rm = TRUE)
tertile_2_largest <- quantile(treated_tract_chars_largest$largest_original_project_size, 2/3, na.rm = TRUE)

cat("Largest project size terciles:\n")
cat("  Small: <", tertile_1_largest, "units\n")
cat("  Medium:", tertile_1_largest, "-", tertile_2_largest, "units\n")
cat("  Large: ≥", tertile_2_largest, "units\n")

# Split by project size into three groups
small_projects_largest <- treated_tract_chars_largest %>%
  filter(project_size_group == "small") %>%
  pull(match_group)

medium_projects_largest <- treated_tract_chars_largest %>%
  filter(project_size_group == "medium") %>%
  pull(match_group)

large_projects_largest <- treated_tract_chars_largest %>%
  filter(project_size_group == "large") %>%
  pull(match_group)

cat("Small projects:", length(small_projects_largest), "match groups\n")
cat("Medium projects:", length(medium_projects_largest), "match groups\n")
cat("Large projects:", length(large_projects_largest), "match groups\n")

# Filter data for each project size group
tract_data_small_projects_largest <- tract_data_matched %>%
  filter(match_group %in% small_projects_largest)

tract_data_medium_projects_largest <- tract_data_matched %>%
  filter(match_group %in% medium_projects_largest)

tract_data_large_projects_largest <- tract_data_matched %>%
  filter(match_group %in% large_projects_largest)

# Run event studies for small projects (by largest)
results_event_study_small_largest <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Small projects (by largest)\n")

    results <- did_event_study(
      input_data = tract_data_small_projects_largest,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_small_largest[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Run event studies for medium projects (by largest)
results_event_study_medium_largest <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Medium projects (by largest)\n")

    results <- did_event_study(
      input_data = tract_data_medium_projects_largest,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_medium_largest[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Run event studies for large projects (by largest)
results_event_study_large_largest <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Large projects (by largest)\n")

    results <- did_event_study(
      input_data = tract_data_large_projects_largest,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_large_largest[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Create three-group project size comparison plot (by largest)
p_size_three_groups_largest <- create_heterogeneity_plot_clean(
  results_event_study_small_largest, paste0("Small (<", round(tertile_1_largest, 0), ")"),
  results_event_study_medium_largest, paste0("Medium (", round(tertile_1_largest, 0), "-", round(tertile_2_largest, 0), ")"),
  results_event_study_large_largest, paste0("Large (≥", round(tertile_2_largest, 0), ")"),
  outcomes_to_plot = c("asinh_median_income", "asinh_pop_black", "asinh_pop_white"),
  outcome_labels_map = race_income_labels,
  title_text = "Heterogeneity by Largest Project Size",
  x_label = "Largest Project Size",
  save_name = "largest_project_size_heterogeneity",
  results_dir = heterogeneity_results_dir,
  width = 13,
  height = 5.5
)

print(p_size_three_groups_largest)

# Generate coefficient tables for all outcomes
cat("\n--- Creating Coefficient Tables for Project Size (Largest) Heterogeneity ---\n")
for (outcome in table_outcome_vars) {
  for (group in group_types) {
    create_heterogeneity_coefficient_table(
      results_event_study_small_largest, paste0("Small (<", round(tertile_1_largest, 0), " units)"),
      results_event_study_medium_largest, paste0("Medium (", round(tertile_1_largest, 0), "-", round(tertile_2_largest, 0), " units)"),
      results_event_study_large_largest, paste0("Large (≥", round(tertile_2_largest, 0), " units)"),
      outcome_var = outcome,
      treatment_group = group,
      save_name = "largest_project_size_heterogeneity",
      results_dir = heterogeneity_results_dir
    )
  }
}

# Full event study plots for project size (largest) heterogeneity
if (FULL_EVENT_STUDY_PLOTS$project_size_largest) {
  cat("\n--- Creating Full Event Study Plots for Project Size (Largest) Heterogeneity ---\n")

  size_colors <- project_size_colors

  for (outcome in c("black_pop", "white_pop", "asinh_median_income")) {
    outcome_label <- c("black_pop" = "Black Population", "white_pop" = "White Population",
                       "asinh_median_income" = "Log Median Income")[outcome]

    p_full_treated <- create_heterogeneity_event_study_plot(
      results_event_study_small_largest, paste0("Small (<", round(tertile_1_largest, 0), " units)"),
      results_event_study_medium_largest, paste0("Medium (", round(tertile_1_largest, 0), "-", round(tertile_2_largest, 0), " units)"),
      results_event_study_large_largest, paste0("Large (≥", round(tertile_2_largest, 0), " units)"),
      outcome_var = outcome,
      outcome_label = outcome_label,
      treatment_group = "treated",
      colors = size_colors,
      save_name = paste0("project_size_largest_", outcome, "_treated"),
      results_dir = heterogeneity_results_dir
    )

    p_full_inner <- create_heterogeneity_event_study_plot(
      results_event_study_small_largest, paste0("Small (<", round(tertile_1_largest, 0), " units)"),
      results_event_study_medium_largest, paste0("Medium (", round(tertile_1_largest, 0), "-", round(tertile_2_largest, 0), " units)"),
      results_event_study_large_largest, paste0("Large (≥", round(tertile_2_largest, 0), " units)"),
      outcome_var = outcome,
      outcome_label = outcome_label,
      treatment_group = "inner",
      colors = size_colors,
      save_name = paste0("project_size_largest_", outcome, "_spillover"),
      results_dir = heterogeneity_results_dir
    )
  }

  cat("Full event study plots saved to:", file.path(heterogeneity_results_dir, "full_event_studies"), "\n")
}

# ---- B. Heterogeneity by TOTAL Project Size ----

cat("\n--- Project Size Heterogeneity (TOTAL PROJECT UNITS) ---\n")

# Get characteristics for treated tracts - using TOTAL project units
treated_tract_chars_total <- tract_data_matched %>%
  filter(group_type == "treated", location_type == "treated") %>%
  filter(year == matched_treatment_year) %>%
  select(GISJOIN_1950, match_group, matched_treatment_year, total_original_project_size) %>%
  distinct() %>%
  filter(!is.na(total_original_project_size)) %>%
  mutate(
    # Calculate terciles based on total project size
    project_size_group = case_when(
      total_original_project_size < quantile(total_original_project_size, 1/3, na.rm = TRUE) ~ "small",
      total_original_project_size >= quantile(total_original_project_size, 1/3, na.rm = TRUE) &
        total_original_project_size < quantile(total_original_project_size, 2/3, na.rm = TRUE) ~ "medium",
      total_original_project_size >= quantile(total_original_project_size, 2/3, na.rm = TRUE) ~ "large"
    )
  )

# Report tercile cutoffs
tertile_1_total <- quantile(treated_tract_chars_total$total_original_project_size, 1/3, na.rm = TRUE)
tertile_2_total <- quantile(treated_tract_chars_total$total_original_project_size, 2/3, na.rm = TRUE)

cat("Total project size terciles:\n")
cat("  Small: <", tertile_1_total, "units\n")
cat("  Medium:", tertile_1_total, "-", tertile_2_total, "units\n")
cat("  Large: ≥", tertile_2_total, "units\n")

# Split by project size into three groups
small_projects_total <- treated_tract_chars_total %>%
  filter(project_size_group == "small") %>%
  pull(match_group)

medium_projects_total <- treated_tract_chars_total %>%
  filter(project_size_group == "medium") %>%
  pull(match_group)

large_projects_total <- treated_tract_chars_total %>%
  filter(project_size_group == "large") %>%
  pull(match_group)

cat("Small projects:", length(small_projects_total), "match groups\n")
cat("Medium projects:", length(medium_projects_total), "match groups\n")
cat("Large projects:", length(large_projects_total), "match groups\n")

# Filter data for each project size group
tract_data_small_projects_total <- tract_data_matched %>%
  filter(match_group %in% small_projects_total)

tract_data_medium_projects_total <- tract_data_matched %>%
  filter(match_group %in% medium_projects_total)

tract_data_large_projects_total <- tract_data_matched %>%
  filter(match_group %in% large_projects_total)

# Run event studies for small projects (by total)
results_event_study_small_total <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Small projects (by total)\n")

    results <- did_event_study(
      input_data = tract_data_small_projects_total,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_small_total[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Run event studies for medium projects (by total)
results_event_study_medium_total <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Medium projects (by total)\n")

    results <- did_event_study(
      input_data = tract_data_medium_projects_total,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_medium_total[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Run event studies for large projects (by total)
results_event_study_large_total <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Large projects (by total)\n")

    results <- did_event_study(
      input_data = tract_data_large_projects_total,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_large_total[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Create three-group project size comparison plot (by total)
p_size_three_groups_total <- create_heterogeneity_plot_clean(
  results_event_study_small_total, paste0("Small (<", round(tertile_1_total, 0), ")"),
  results_event_study_medium_total, paste0("Medium (", round(tertile_1_total, 0), "-", round(tertile_2_total, 0), ")"),
  results_event_study_large_total, paste0("Large (≥", round(tertile_2_total, 0), ")"),
  outcomes_to_plot = c("asinh_median_income", "asinh_pop_black", "asinh_pop_white"),
  outcome_labels_map = race_income_labels,
  title_text = "Heterogeneity by Total Project Size",
  x_label = "Total Project Size",
  save_name = "total_project_size_heterogeneity",
  results_dir = heterogeneity_results_dir,
  width = 13,
  height = 5.5
)

print(p_size_three_groups_total)

# Generate coefficient tables for all outcomes
cat("\n--- Creating Coefficient Tables for Project Size (Total) Heterogeneity ---\n")
for (outcome in table_outcome_vars) {
  for (group in group_types) {
    create_heterogeneity_coefficient_table(
      results_event_study_small_total, paste0("Small (<", round(tertile_1_total, 0), " units)"),
      results_event_study_medium_total, paste0("Medium (", round(tertile_1_total, 0), "-", round(tertile_2_total, 0), " units)"),
      results_event_study_large_total, paste0("Large (≥", round(tertile_2_total, 0), " units)"),
      outcome_var = outcome,
      treatment_group = group,
      save_name = "total_project_size_heterogeneity",
      results_dir = heterogeneity_results_dir
    )
  }
}

# Full event study plots for project size (total) heterogeneity
if (FULL_EVENT_STUDY_PLOTS$project_size_total) {
  cat("\n--- Creating Full Event Study Plots for Project Size (Total) Heterogeneity ---\n")

  size_colors <- project_size_colors

  for (outcome in c("black_pop", "white_pop", "asinh_median_income")) {
    outcome_label <- c("black_pop" = "Black Population", "white_pop" = "White Population",
                       "asinh_median_income" = "Log Median Income")[outcome]

    p_full_treated <- create_heterogeneity_event_study_plot(
      results_event_study_small_total, paste0("Small (<", round(tertile_1_total, 0), " units)"),
      results_event_study_medium_total, paste0("Medium (", round(tertile_1_total, 0), "-", round(tertile_2_total, 0), " units)"),
      results_event_study_large_total, paste0("Large (≥", round(tertile_2_total, 0), " units)"),
      outcome_var = outcome,
      outcome_label = outcome_label,
      treatment_group = "treated",
      colors = size_colors,
      save_name = paste0("project_size_total_", outcome, "_treated"),
      results_dir = heterogeneity_results_dir
    )

    p_full_inner <- create_heterogeneity_event_study_plot(
      results_event_study_small_total, paste0("Small (<", round(tertile_1_total, 0), " units)"),
      results_event_study_medium_total, paste0("Medium (", round(tertile_1_total, 0), "-", round(tertile_2_total, 0), " units)"),
      results_event_study_large_total, paste0("Large (≥", round(tertile_2_total, 0), " units)"),
      outcome_var = outcome,
      outcome_label = outcome_label,
      treatment_group = "inner",
      colors = size_colors,
      save_name = paste0("project_size_total_", outcome, "_spillover"),
      results_dir = heterogeneity_results_dir
    )
  }

  cat("Full event study plots saved to:", file.path(heterogeneity_results_dir, "full_event_studies"), "\n")
}

# ---- Baseline Neighborhood Characteristics Heterogeneity ----

cat("\n=== HETEROGENEITY BY BASELINE NEIGHBORHOOD CHARACTERISTICS ===\n")

# Calculate baseline characteristics for ALL tracts (treated, controls, spillovers)
baseline_chars_all <- tract_data_matched %>%
  mutate(baseline_year = matched_treatment_year - 10) %>%
  filter(year == baseline_year, location_type != "donor_pool") %>%
  select(match_group, group_type, black_share, median_income) %>%
  distinct(.keep_all = TRUE) %>%
  dplyr::rename(
    baseline_black_share = black_share,
    baseline_median_income = median_income
  ) %>%
  mutate(
    low_income_baseline = baseline_median_income <= quantile(baseline_median_income, 0.33, na.rm = TRUE)
  )
  

# Merge baseline characteristics back to full dataset
tract_data_with_all_baseline <- tract_data_matched %>%
  left_join(baseline_chars_all, by = c("match_group", "group_type"))

## ---- Three-Group Black Share at Baseline ----

cat("\n--- Three-Group Black Share at Baseline (<1%, 1-12%, ≥12%) ---\n")

# Filter data for each group using baseline characteristics for all tracts
tract_data_very_low_black <-
  tract_data_with_all_baseline %>%
  filter(baseline_black_share < 0.01)

tract_data_medium_black <- tract_data_with_all_baseline %>%
  filter(baseline_black_share >= 0.01 & baseline_black_share < 0.12)

tract_data_high_black <- tract_data_with_all_baseline %>%
  filter(baseline_black_share >= 0.12)

# Check sample sizes
cat("Very low black share (<1%):", nrow(tract_data_very_low_black %>% distinct(GISJOIN_1950, match_group)), "observations\n")
cat("Medium black share (1-12%):", nrow(tract_data_medium_black %>% distinct(GISJOIN_1950, match_group)), "observations\n")
cat("High black share (≥12%):", nrow(tract_data_high_black %>% distinct(GISJOIN_1950, match_group)), "observations\n")

# Run event studies for very low black share
results_event_study_very_low_black <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Very low black baseline\n")

    results <- did_event_study(
      input_data = tract_data_very_low_black,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_very_low_black[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Run event studies for medium black share
results_event_study_medium_black <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Medium black baseline\n")

    results <- did_event_study(
      input_data = tract_data_medium_black,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_medium_black[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Run event studies for high black share
results_event_study_high_black <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| High black baseline\n")

    results <- did_event_study(
      input_data = tract_data_high_black,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_high_black[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Create three-group black share comparison plot
p_black_three_groups <- create_heterogeneity_plot_clean(
  results_event_study_very_low_black, "Low (<1%)",
  results_event_study_medium_black, "Medium (1-12%)",
  results_event_study_high_black, "High (≥12%)",
  outcomes_to_plot = c("black_pop", "white_pop"),
  outcome_labels_map = race_income_labels,
  title_text = "Heterogeneity by Baseline Black Population Share",
  x_label = "Baseline Black Population Share",
  save_name = "baseline_black_three_groups_heterogeneity",
  results_dir = heterogeneity_results_dir,
  width = 11,
  height = 5.5
)

print(p_black_three_groups)

# Generate coefficient tables for all outcomes
cat("\n--- Creating Coefficient Tables for Baseline Black Share Heterogeneity ---\n")
for (outcome in table_outcome_vars) {
  for (group in group_types) {
    create_heterogeneity_coefficient_table(
      results_event_study_very_low_black, "Very Low (<1%)",
      results_event_study_medium_black, "Medium (1-12%)",
      results_event_study_high_black, "High (≥12%)",
      outcome_var = outcome,
      treatment_group = group,
      save_name = "baseline_black_three_groups_heterogeneity",
      results_dir = heterogeneity_results_dir
    )
  }
}

# Full event study plots for baseline black share heterogeneity
if (FULL_EVENT_STUDY_PLOTS$baseline_black_share) {
  cat("\n--- Creating Full Event Study Plots for Baseline Black Share Heterogeneity ---\n")

  # Define outcomes and labels for full event study plots
  full_event_outcomes <- c("black_pop", "white_pop", "asinh_median_income", "black_share")
  full_event_labels <- c(
    "black_pop" = "Black Population",
    "white_pop" = "White Population",
    "asinh_median_income" = "Log Median Income",
    "black_share" = "Black Population Share"
  )

  # Define colors for the three groups
  black_share_colors <- tipping_colors

  # Create full event study plots for each outcome
  for (outcome in full_event_outcomes) {
    # Treated neighborhoods
    p_full_treated <- create_heterogeneity_event_study_plot(
      results_event_study_very_low_black, "Very Low (<1%)",
      results_event_study_medium_black, "Medium (1-12%)",
      results_event_study_high_black, "High (≥12%)",
      outcome_var = outcome,
      outcome_label = full_event_labels[outcome],
      treatment_group = "treated",
      colors = black_share_colors,
      save_name = paste0("baseline_black_share_", outcome, "_treated"),
      results_dir = heterogeneity_results_dir
    )

    # Spillover neighborhoods
    p_full_inner <- create_heterogeneity_event_study_plot(
      results_event_study_very_low_black, "Very Low (<1%)",
      results_event_study_medium_black, "Medium (1-12%)",
      results_event_study_high_black, "High (≥12%)",
      outcome_var = outcome,
      outcome_label = full_event_labels[outcome],
      treatment_group = "inner",
      colors = black_share_colors,
      save_name = paste0("baseline_black_share_", outcome, "_spillover"),
      results_dir = heterogeneity_results_dir
    )
  }

  cat("Full event study plots saved to:", file.path(heterogeneity_results_dir, "full_event_studies"), "\n")
}

# ---- Low vs High Income at Baseline ----

cat("\n--- Low vs High Income at Baseline ---\n")

# Filter data using baseline characteristics for all tracts
tract_data_low_income <- tract_data_with_all_baseline %>%
  filter(low_income_baseline == TRUE)

tract_data_high_income <- tract_data_with_all_baseline %>%
  filter(low_income_baseline == FALSE)

# Check sample sizes
cat("Low income (bottom tercile):", nrow(tract_data_low_income %>% distinct(GISJOIN_1950, match_group)), "observations\n")
cat("High income (top 2 terciles):", nrow(tract_data_high_income %>% distinct(GISJOIN_1950, match_group)), "observations\n")

# Run event studies
results_event_study_low_income <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Low income baseline\n")

    results <- did_event_study(
      input_data = tract_data_low_income,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_low_income[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

results_event_study_high_income <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| High income baseline\n")

    results <- did_event_study(
      input_data = tract_data_high_income,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_high_income[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Create income comparison plot
p_income_comparison <- create_heterogeneity_plot_clean(
  results_event_study_low_income, "Low Income",
  results_event_study_high_income, "High Income",
  outcomes_to_plot = c("asinh_median_income", "asinh_pop_black", "asinh_pop_white"),
  outcome_labels_map = race_income_labels,
  title_text = "Heterogeneity by Baseline Income Level",
  x_label = "Baseline Median Income",
  save_name = "baseline_income_heterogeneity",
  results_dir = heterogeneity_results_dir,
  width = 13,
  height = 5.5
)

print(p_income_comparison)

# Generate coefficient tables for all outcomes
cat("\n--- Creating Coefficient Tables for Baseline Income Heterogeneity ---\n")
for (outcome in table_outcome_vars) {
  for (group in group_types) {
    create_heterogeneity_coefficient_table(
      results_event_study_low_income, "Low Income (Bottom Tercile)",
      results_event_study_high_income, "High Income (Top 2 Terciles)",
      outcome_var = outcome,
      treatment_group = group,
      save_name = "baseline_income_heterogeneity",
      results_dir = heterogeneity_results_dir
    )
  }
}

# Full event study plots for baseline income heterogeneity
if (FULL_EVENT_STUDY_PLOTS$baseline_income) {
  cat("\n--- Creating Full Event Study Plots for Baseline Income Heterogeneity ---\n")

  for (outcome in c("black_pop", "white_pop", "asinh_median_income")) {
    outcome_label <- c("black_pop" = "Black Population", "white_pop" = "White Population",
                       "asinh_median_income" = "Log Median Income")[outcome]

    p_full_treated <- create_heterogeneity_event_study_plot(
      results_event_study_low_income, "Low Income (Bottom Tercile)",
      results_event_study_high_income, "High Income (Top 2 Terciles)",
      outcome_var = outcome,
      outcome_label = outcome_label,
      treatment_group = "treated",
      colors = income_colors,
      save_name = paste0("baseline_income_", outcome, "_treated"),
      results_dir = heterogeneity_results_dir
    )

    p_full_inner <- create_heterogeneity_event_study_plot(
      results_event_study_low_income, "Low Income (Bottom Tercile)",
      results_event_study_high_income, "High Income (Top 2 Terciles)",
      outcome_var = outcome,
      outcome_label = outcome_label,
      treatment_group = "inner",
      colors = income_colors,
      save_name = paste0("baseline_income_", outcome, "_spillover"),
      results_dir = heterogeneity_results_dir
    )
  }

  cat("Full event study plots saved to:", file.path(heterogeneity_results_dir, "full_event_studies"), "\n")
}

# ---- NYC vs Non-NYC Heterogeneity ----

cat("\n=== HETEROGENEITY BY NYC VS NON-NYC ===\n")

if (!VARIANT %in% c("no_cbsa", "cem")) {
  # Extract NYC status from treated/inner tracts only (similar to baseline characteristics)
  nyc_chars <- tract_data_matched %>%
    mutate(baseline_year = matched_treatment_year - 10) %>%
    filter(year == baseline_year, location_type %in% c("treated", "inner")) %>%
    mutate(nyc_status = COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York") %>%
    select(match_group, group_type, nyc_status) %>%
    distinct(.keep_all = TRUE)
  
  # Merge NYC status back to full dataset
  tract_data_with_nyc <- tract_data_matched %>%
    left_join(nyc_chars, by = c("match_group", "group_type"))
  
  # Filter by NYC status (includes all matched controls)
  tract_data_nyc <- tract_data_with_nyc %>%
    filter(nyc_status == TRUE)
  
  tract_data_non_nyc <- tract_data_with_nyc %>%
    filter(nyc_status == FALSE)
  
  # Check sample sizes
  cat("NYC observations:", nrow(tract_data_nyc %>% distinct(GISJOIN_1950, match_group)), "\n")
  cat("Non-NYC observations:", nrow(tract_data_non_nyc %>% distinct(GISJOIN_1950, match_group)), "\n")
  
  # Run event studies for NYC
  results_event_study_nyc <- list()
  for (outcome in heterogeneity_outcome_vars) {
    for (group in group_types) {
      cat("Event study for", outcome, "|", group, "| NYC\n")
  
      results <- did_event_study(
        input_data = tract_data_nyc,
        outcome_var = outcome,
        treatment_group = group
      )
  
      results_event_study_nyc[[paste0(outcome, "_", group)]] <- results$twfe_conley
    }
  }
  
  # Run event studies for non-NYC
  results_event_study_non_nyc <- list()
  for (outcome in heterogeneity_outcome_vars) {
    for (group in group_types) {
      cat("Event study for", outcome, "|", group, "| Non-NYC\n")
  
      results <- did_event_study(
        input_data = tract_data_non_nyc,
        outcome_var = outcome,
        treatment_group = group
      )
  
      results_event_study_non_nyc[[paste0(outcome, "_", group)]] <- results$twfe_conley
    }
  }
  
  # Create NYC comparison plot
  p_nyc_comparison <- create_heterogeneity_plot_clean(
    results_event_study_nyc, "NYC",
    results_event_study_non_nyc, "Non-NYC",
    outcomes_to_plot = c("asinh_median_income", "asinh_pop_black", "asinh_pop_white"),
    outcome_labels_map = race_income_labels,
    title_text = "Heterogeneity by NYC vs Non-NYC",
      x_label = "Location",
    save_name = "nyc_heterogeneity",
    results_dir = heterogeneity_results_dir,
    width = 13,
    height = 5.5
  )

  print(p_nyc_comparison)

  # Generate coefficient tables for all outcomes
  cat("\n--- Creating Coefficient Tables for NYC Heterogeneity ---\n")
  for (outcome in table_outcome_vars) {
    for (group in group_types) {
      create_heterogeneity_coefficient_table(
        results_event_study_nyc, "NYC",
        results_event_study_non_nyc, "Non-NYC",
        outcome_var = outcome,
        treatment_group = group,
        save_name = "nyc_heterogeneity",
        results_dir = heterogeneity_results_dir
      )
    }
  }

  # Full event study plots for NYC heterogeneity
  if (FULL_EVENT_STUDY_PLOTS$nyc) {
    cat("\n--- Creating Full Event Study Plots for NYC Heterogeneity ---\n")

    for (outcome in c("black_pop", "white_pop", "asinh_median_income")) {
      outcome_label <- c("black_pop" = "Black Population", "white_pop" = "White Population",
                         "asinh_median_income" = "Log Median Income")[outcome]

      p_full_treated <- create_heterogeneity_event_study_plot(
        results_event_study_nyc, "NYC",
        results_event_study_non_nyc, "Non-NYC",
        outcome_var = outcome,
        outcome_label = outcome_label,
        treatment_group = "treated",
        colors = nyc_colors,
        save_name = paste0("nyc_", outcome, "_treated"),
        results_dir = heterogeneity_results_dir
      )

      p_full_inner <- create_heterogeneity_event_study_plot(
        results_event_study_nyc, "NYC",
        results_event_study_non_nyc, "Non-NYC",
        outcome_var = outcome,
        outcome_label = outcome_label,
        treatment_group = "inner",
        colors = nyc_colors,
        save_name = paste0("nyc_", outcome, "_spillover"),
        results_dir = heterogeneity_results_dir
      )
    }

    cat("Full event study plots saved to:", file.path(heterogeneity_results_dir, "full_event_studies"), "\n")
  }

}

# ---- Urban Renewal vs Non-Urban Renewal Heterogeneity ----

cat("\n=== HETEROGENEITY BY URBAN RENEWAL VS NON-URBAN RENEWAL ===\n")

# Filter data directly by urban renewal status
tract_data_ur <- tract_data_matched %>%
  filter(ur_binary_5pp == 1)

tract_data_non_ur <- tract_data_matched %>%
  filter(ur_binary_5pp == 0)

# Check sample sizes
cat("Urban renewal observations:", nrow(tract_data_ur %>% distinct(GISJOIN_1950, match_group)), "\n")
cat("Non-urban renewal observations:", nrow(tract_data_non_ur %>% distinct(GISJOIN_1950, match_group)), "\n")

# Run event studies for urban renewal
results_event_study_ur <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Urban renewal\n")

    results <- did_event_study(
      input_data = tract_data_ur,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_ur[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Run event studies for non-urban renewal
results_event_study_non_ur <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Non-urban renewal\n")

    results <- did_event_study(
      input_data = tract_data_non_ur,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_non_ur[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Create urban renewal comparison plot
p_ur_comparison <- create_heterogeneity_plot_clean(
  results_event_study_ur, "Urban Renewal",
  results_event_study_non_ur, "Non-Urban Renewal",
  outcomes_to_plot = c("asinh_median_income", "asinh_pop_black", "asinh_pop_white"),
  outcome_labels_map = race_income_labels,
  title_text = "Heterogeneity by Urban Renewal Status",
  x_label = "Urban Renewal Status",
  save_name = "urban_renewal_heterogeneity",
  results_dir = heterogeneity_results_dir,
  width = 13,
  height = 5.5
)

print(p_ur_comparison)

# Generate coefficient tables for all outcomes
cat("\n--- Creating Coefficient Tables for Urban Renewal Heterogeneity ---\n")
for (outcome in table_outcome_vars) {
  for (group in group_types) {
    create_heterogeneity_coefficient_table(
      results_event_study_ur, "Urban Renewal",
      results_event_study_non_ur, "Non-Urban Renewal",
      outcome_var = outcome,
      treatment_group = group,
      save_name = "urban_renewal_heterogeneity",
      results_dir = heterogeneity_results_dir
    )
  }
}

# Full event study plots for urban renewal heterogeneity
if (FULL_EVENT_STUDY_PLOTS$urban_renewal) {
  cat("\n--- Creating Full Event Study Plots for Urban Renewal Heterogeneity ---\n")

  ur_colors <- urban_renewal_colors

  for (outcome in c("black_pop", "white_pop", "asinh_median_income")) {
    outcome_label <- c("black_pop" = "Black Population", "white_pop" = "White Population",
                       "asinh_median_income" = "Log Median Income")[outcome]

    p_full_treated <- create_heterogeneity_event_study_plot(
      results_event_study_ur, "Urban Renewal",
      results_event_study_non_ur, "Non-Urban Renewal",
      outcome_var = outcome,
      outcome_label = outcome_label,
      treatment_group = "treated",
      colors = ur_colors,
      save_name = paste0("urban_renewal_", outcome, "_treated"),
      results_dir = heterogeneity_results_dir
    )

    p_full_inner <- create_heterogeneity_event_study_plot(
      results_event_study_ur, "Urban Renewal",
      results_event_study_non_ur, "Non-Urban Renewal",
      outcome_var = outcome,
      outcome_label = outcome_label,
      treatment_group = "inner",
      colors = ur_colors,
      save_name = paste0("urban_renewal_", outcome, "_spillover"),
      results_dir = heterogeneity_results_dir
    )
  }

  cat("Full event study plots saved to:", file.path(heterogeneity_results_dir, "full_event_studies"), "\n")
}

# ---- High-rise vs Non-High-rise Heterogeneity ----

cat("\n=== HETEROGENEITY BY HIGH-RISE VS NON-HIGH-RISE ===\n")

# Check if has_highrise_project exists (for treated groups)
if ("has_highrise_project" %in% names(tract_data_matched)) {

# TREATED GROUP: Use has_highrise_project from the treated tract
# Get high-rise status from treated tracts in the "treated" group_type
treated_highrise_chars <- tract_data_matched %>%
  filter(group_type == "treated", location_type == "treated") %>%
  select(match_group, group_type, has_highrise_project) %>%
  filter(!is.na(has_highrise_project)) %>% 
  distinct() %>%
  rename(match_group_has_highrise = has_highrise_project)

# INNER GROUP: Use has_nearby_highrise (spillover variable)
# This is already propagated to all inner ring match groups in matching code
inner_highrise_chars <- tract_data_matched %>%
  filter(group_type == "inner", location_type == "inner") %>%
  select(match_group, group_type, has_nearby_highrise) %>%
  filter(!is.na(has_nearby_highrise)) %>%
  distinct() %>%
  rename(match_group_has_highrise = has_nearby_highrise)

# Combine both: each match group + group_type gets its appropriate high-rise indicator
highrise_chars <- bind_rows(treated_highrise_chars, inner_highrise_chars)

# Merge high-rise status to entire dataset (join by match_group AND group_type)
tract_data_with_highrise <- tract_data_matched %>%
  left_join(highrise_chars, by = c("match_group", "group_type"))

# Filter by high-rise status
tract_data_highrise <- tract_data_with_highrise %>%
  filter(match_group_has_highrise == 1)

tract_data_non_highrise <- tract_data_with_highrise %>%
  filter(match_group_has_highrise == 0)

# Check sample sizes
cat("High-rise observations:", nrow(tract_data_highrise %>% distinct(GISJOIN_1950, match_group)), "\n")
cat("Non-high-rise observations:", nrow(tract_data_non_highrise %>% distinct(GISJOIN_1950, match_group)), "\n")

# Run event studies for high-rise
results_event_study_highrise <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| High-rise\n")

    results <- did_event_study(
      input_data = tract_data_highrise,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_highrise[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Run event studies for non-high-rise
results_event_study_non_highrise <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Non-high-rise\n")

    results <- did_event_study(
      input_data = tract_data_non_highrise,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_non_highrise[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Create high-rise comparison plot
p_highrise_comparison <- create_heterogeneity_plot_clean(
  results_list1 = results_event_study_highrise,
  results_list2 = results_event_study_non_highrise,
  label1 = "High-rise",
  label2 = "Non-high-rise",
  outcomes_to_plot = c("asinh_median_rent_calculated", "asinh_pop_black", "asinh_median_income"),
  outcome_labels_map = race_income_labels,
  title_text = "Heterogeneity by High-rise vs Non-high-rise Projects",
  x_label = "Project Type",
  save_name = "highrise_heterogeneity",
  results_dir = heterogeneity_results_dir,
  width = 15,   # 3 outcomes
  height = 5.5
)

print(p_highrise_comparison)

# Generate coefficient tables for all outcomes
cat("\n--- Creating Coefficient Tables for High-rise Heterogeneity ---\n")
for (outcome in table_outcome_vars) {
  for (group in group_types) {
    create_heterogeneity_coefficient_table(
      results_event_study_highrise, "High-rise",
      results_event_study_non_highrise, "Non-high-rise",
      outcome_var = outcome,
      treatment_group = group,
      save_name = "highrise_heterogeneity",
      results_dir = heterogeneity_results_dir
    )
  }
}

  # Full event study plots for high-rise heterogeneity
  if (FULL_EVENT_STUDY_PLOTS$highrise) {
    cat("\n--- Creating Full Event Study Plots for High-rise Heterogeneity ---\n")

    for (outcome in c("black_pop", "white_pop", "asinh_median_income", "asinh_median_rent_calculated")) {
      outcome_label <- c("black_pop" = "Black Population", "white_pop" = "White Population",
                         "asinh_median_income" = "Log Median Income",
                         "asinh_median_rent_calculated" = "Log Median Rent")[outcome]

      p_full_treated <- create_heterogeneity_event_study_plot(
        results_event_study_highrise, "High-rise",
        results_event_study_non_highrise, "Non-high-rise",
        outcome_var = outcome,
        outcome_label = outcome_label,
        treatment_group = "treated",
        colors = highrise_colors,
        save_name = paste0("highrise_", outcome, "_treated"),
        results_dir = heterogeneity_results_dir
      )

      p_full_inner <- create_heterogeneity_event_study_plot(
        results_event_study_highrise, "High-rise",
        results_event_study_non_highrise, "Non-high-rise",
        outcome_var = outcome,
        outcome_label = outcome_label,
        treatment_group = "inner",
        colors = highrise_colors,
        save_name = paste0("highrise_", outcome, "_spillover"),
        results_dir = heterogeneity_results_dir
      )
    }

    cat("Full event study plots saved to:", file.path(heterogeneity_results_dir, "full_event_studies"), "\n")
  }

} else {
  cat("Skipping high-rise heterogeneity - has_highrise_project variable not found in dataset\n")
  cat("Re-run matching algorithms to include has_highrise_project\n")
}

# ---- Early vs Late Projects Heterogeneity ----

cat("\n=== HETEROGENEITY BY EARLY VS LATE PROJECTS ===\n")

# Filter data for each time period
tract_data_early_projects <-
  tract_data_matched %>%
  filter(matched_treatment_year < 1960)

tract_data_late_projects <- tract_data_matched %>%
  filter(matched_treatment_year >= 1960)

# Run event studies for early projects
results_event_study_early <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Early projects\n")

    results <- did_event_study(
      input_data = tract_data_early_projects,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_early[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Run event studies for late projects
results_event_study_late <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Late projects\n")

    results <- did_event_study(
      input_data = tract_data_late_projects,
      outcome_var = outcome,
      treatment_group = group
    )

    results_event_study_late[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Create early vs late comparison plot
p_early_late_comparison <- create_heterogeneity_plot_clean(
  results_list1 = results_event_study_early,
  results_list2 = results_event_study_late,
  label1 = "Early (<1960)",
  label2 = "Late (≥1960)",
  outcomes_to_plot = c("asinh_median_income", "asinh_pop_black", "asinh_pop_white"),
  outcome_labels_map = race_income_labels,
  title_text = "Heterogeneity by Early vs Late Projects",
  x_label = "Construction Period",
  save_name = "early_late_projects_heterogeneity",
  results_dir = heterogeneity_results_dir,
  width = 15,   # 3 outcomes
  height = 5.5
)

print(p_early_late_comparison)

# Generate coefficient tables for all outcomes
cat("\n--- Creating Coefficient Tables for Early/Late Project Heterogeneity ---\n")
for (outcome in table_outcome_vars) {
  for (group in group_types) {
    create_heterogeneity_coefficient_table(
      results_event_study_early, "Early Projects (<1960)",
      results_event_study_late, "Late Projects (≥1960)",
      outcome_var = outcome,
      treatment_group = group,
      save_name = "early_late_projects_heterogeneity",
      results_dir = heterogeneity_results_dir
    )
  }
}

# Full event study plots for early vs late heterogeneity
if (FULL_EVENT_STUDY_PLOTS$early_late) {
  cat("\n--- Creating Full Event Study Plots for Early vs Late Heterogeneity ---\n")

  early_late_colors <- timing_colors

  for (outcome in c("black_pop", "white_pop", "asinh_median_income")) {
    outcome_label <- c("black_pop" = "Black Population", "white_pop" = "White Population",
                       "asinh_median_income" = "Log Median Income")[outcome]

    p_full_treated <- create_heterogeneity_event_study_plot(
      results_event_study_early, "Early Projects (<1960)",
      results_event_study_late, "Late Projects (≥1960)",
      outcome_var = outcome,
      outcome_label = outcome_label,
      treatment_group = "treated",
      colors = early_late_colors,
      save_name = paste0("early_late_", outcome, "_treated"),
      results_dir = heterogeneity_results_dir
    )

    p_full_inner <- create_heterogeneity_event_study_plot(
      results_event_study_early, "Early Projects (<1960)",
      results_event_study_late, "Late Projects (≥1960)",
      outcome_var = outcome,
      outcome_label = outcome_label,
      treatment_group = "inner",
      colors = early_late_colors,
      save_name = paste0("early_late_", outcome, "_spillover"),
      results_dir = heterogeneity_results_dir
    )
  }

  cat("Full event study plots saved to:", file.path(heterogeneity_results_dir, "full_event_studies"), "\n")
}

# ---- Mechanical vs Endogenous Effects: Low PH Population Share ----

cat("\n=== HETEROGENEITY BY PUBLIC HOUSING POPULATION SHARE (MECHANICAL EFFECTS) ===\n")

# Outcome variables for mechanical effects analysis
# Exclude unemp_rate (not a direct compositional outcome)
# Include population levels and totals for clearer interpretation
mechanical_effects_outcome_vars <- c(
  "asinh_median_rent_calculated", "asinh_median_income",
  "black_share", "asinh_pop_black", "asinh_pop_white",
  "asinh_pop_total"
)

# Outcomes to plot for mechanical effects (all outcomes, for individual plots)
mechanical_effects_plot_vars <- c(
  "asinh_median_income", "asinh_median_rent_calculated",
  "black_share", "asinh_pop_black", "asinh_pop_white",
  "asinh_pop_total"
)

# Outcomes for combined comparison plots (log-transformed only, for consistent scale)
mechanical_effects_combined_plot_vars <- c(
  "asinh_median_income", "asinh_median_rent_calculated",
  "black_share", "asinh_pop_black", "asinh_pop_white",
  "asinh_pop_total"
)

# Calculate PH population share at t=0 (treatment year) for each match_group
# This represents the maximum possible mechanical compositional effect
# Cap at 100% - mechanical effects cannot exceed 100pp by definition
ph_share_t0 <- tract_data_matched %>%
  filter(group_type == "treated", location_type == "treated") %>%
  filter(year == matched_treatment_year) %>%
  mutate(
    ph_share_of_tract_t0_raw = total_public_housing_pop_estimate / total_pop,
    ph_share_of_tract_t0 = pmin(ph_share_of_tract_t0_raw, 1.0)  # Cap at 100%
  ) %>%
  select(match_group, matched_treatment_year, ph_share_of_tract_t0, ph_share_of_tract_t0_raw,
         total_public_housing_pop_estimate, total_pop) %>%
  distinct() %>%
  filter(!is.na(ph_share_of_tract_t0))

# Report distribution of PH share at t=0
cat("\n--- PH Population Share at t=0 Distribution ---\n")
n_capped <- sum(ph_share_t0$ph_share_of_tract_t0_raw > 1.0, na.rm = TRUE)
cat("Tracts capped at 100%:", n_capped, "out of", nrow(ph_share_t0),
    sprintf("(%.1f%%)", n_capped/nrow(ph_share_t0)*100), "\n")
cat("Mean:", round(mean(ph_share_t0$ph_share_of_tract_t0, na.rm = TRUE), 3), "\n")
cat("Median:", round(median(ph_share_t0$ph_share_of_tract_t0, na.rm = TRUE), 3), "\n")
cat("P10:", round(quantile(ph_share_t0$ph_share_of_tract_t0, 0.10, na.rm = TRUE), 3), "\n")
cat("P25:", round(quantile(ph_share_t0$ph_share_of_tract_t0, 0.25, na.rm = TRUE), 3), "\n")
cat("P75:", round(quantile(ph_share_t0$ph_share_of_tract_t0, 0.75, na.rm = TRUE), 3), "\n")
cat("P90:", round(quantile(ph_share_t0$ph_share_of_tract_t0, 0.90, na.rm = TRUE), 3), "\n\n")

# Define thresholds for low mechanical effects
# If PH pop is <20% of tract population, mechanical effects are bounded at 20pp
# If we observe larger effects, must be endogenous responses
threshold_20 <- 0.20
threshold_10 <- 0.10

# Create low mechanical effect groups
low_mechanical_20 <- ph_share_t0 %>%
  filter(ph_share_of_tract_t0 < threshold_20) %>%
  pull(match_group)

high_mechanical_20 <- ph_share_t0 %>%
  filter(ph_share_of_tract_t0 >= threshold_20) %>%
  pull(match_group)

low_mechanical_10 <- ph_share_t0 %>%
  filter(ph_share_of_tract_t0 < threshold_10) %>%
  pull(match_group)

high_mechanical_10 <- ph_share_t0 %>%
  filter(ph_share_of_tract_t0 >= threshold_10) %>%
  pull(match_group)

cat("Match groups with PH share <20% at t=0:", length(low_mechanical_20), "\n")
cat("Match groups with PH share ≥20% at t=0:", length(high_mechanical_20), "\n")
cat("Match groups with PH share <10% at t=0:", length(low_mechanical_10), "\n")
cat("Match groups with PH share ≥10% at t=0:", length(high_mechanical_10), "\n\n")

# ---- Analysis 1: Low vs High Mechanical (20% threshold) ----

cat("\n--- Low vs High Mechanical Effects (20% threshold) ---\n")

# Filter datasets
tract_data_low_mech_20 <- tract_data_matched %>%
  filter(match_group %in% low_mechanical_20)

tract_data_high_mech_20 <- tract_data_matched %>%
  filter(match_group %in% high_mechanical_20)

# Run event studies for low mechanical effect sample
# Only run for treated neighborhoods (inner ring doesn't have PH)
results_event_study_low_mech_20 <- list()
for (outcome in mechanical_effects_outcome_vars) {
  cat("Event study for", outcome, "| treated | Low mechanical (<20%)\n")

  results <- did_event_study(
    input_data = tract_data_low_mech_20,
    outcome_var = outcome,
    treatment_group = "treated"
  )

  results_event_study_low_mech_20[[paste0(outcome, "_treated")]] <- results$twfe_conley
}

# Run event studies for high mechanical effect sample
# Only run for treated neighborhoods (inner ring doesn't have PH)
results_event_study_high_mech_20 <- list()
for (outcome in mechanical_effects_outcome_vars) {
  cat("Event study for", outcome, "| treated | High mechanical (≥20%)\n")

  results <- did_event_study(
    input_data = tract_data_high_mech_20,
    outcome_var = outcome,
    treatment_group = "treated"
  )

  results_event_study_high_mech_20[[paste0(outcome, "_treated")]] <- results$twfe_conley
}

# Create comparison plot
p_mechanical_20 <- create_heterogeneity_plot(
  results_list1 = results_event_study_low_mech_20,
  results_list2 = results_event_study_high_mech_20,
  label1 = "Low Mechanical (<20% at t=0)",
  label2 = "High Mechanical (>=20% at t=0)",
  outcomes_to_plot = mechanical_effects_combined_plot_vars,
  outcome_labels_map = race_income_labels,
  title_text = "Mechanical vs Endogenous Effects: 20% Threshold",
  colors = c("#2E7D32", "#C62828"),  # Green for low, red for high
  save_name = "mechanical_effects_20pct_heterogeneity",
  results_dir = heterogeneity_results_dir
)

print(p_mechanical_20)

# ---- Analysis 2: Low vs High Mechanical (10% threshold) ----

cat("\n--- Low vs High Mechanical Effects (10% threshold) ---\n")

# Filter datasets
tract_data_low_mech_10 <- tract_data_matched %>%
  filter(match_group %in% low_mechanical_10)

tract_data_high_mech_10 <- tract_data_matched %>%
  filter(match_group %in% high_mechanical_10)

# Run event studies for low mechanical effect sample
# Only run for treated neighborhoods (inner ring doesn't have PH)
results_event_study_low_mech_10 <- list()
for (outcome in mechanical_effects_outcome_vars) {
  cat("Event study for", outcome, "| treated | Low mechanical (<10%)\n")

  results <- did_event_study(
    input_data = tract_data_low_mech_10,
    outcome_var = outcome,
    treatment_group = "treated"
  )

  results_event_study_low_mech_10[[paste0(outcome, "_treated")]] <- results$twfe_conley
}

# Run event studies for high mechanical effect sample
# Only run for treated neighborhoods (inner ring doesn't have PH)
results_event_study_high_mech_10 <- list()
for (outcome in mechanical_effects_outcome_vars) {
  cat("Event study for", outcome, "| treated | High mechanical (≥10%)\n")

  results <- did_event_study(
    input_data = tract_data_high_mech_10,
    outcome_var = outcome,
    treatment_group = "treated"
  )

  results_event_study_high_mech_10[[paste0(outcome, "_treated")]] <- results$twfe_conley
}

# Create comparison plot
p_mechanical_10 <- create_heterogeneity_plot(
  results_list1 = results_event_study_low_mech_10,
  results_list2 = results_event_study_high_mech_10,
  label1 = "Low Mechanical (<10% at t=0)",
  label2 = "High Mechanical (>=10% at t=0)",
  outcomes_to_plot = mechanical_effects_combined_plot_vars,
  outcome_labels_map = race_income_labels,
  title_text = "Mechanical vs Endogenous Effects: 10% Threshold",
  colors = c("#2E7D32", "#C62828"),  # Green for low, red for high
  save_name = "mechanical_effects_10pct_heterogeneity",
  results_dir = heterogeneity_results_dir
)

print(p_mechanical_10)

# ---- Analysis 3: Three-way split (low/medium/high mechanical) ----

cat("\n--- Three-way Mechanical Effects Split ---\n")

# Create three groups based on terciles
tertile_1_mech <- quantile(ph_share_t0$ph_share_of_tract_t0, 1/3, na.rm = TRUE)
tertile_2_mech <- quantile(ph_share_t0$ph_share_of_tract_t0, 2/3, na.rm = TRUE)

cat("Mechanical effect terciles (PH share at t=0):\n")
cat("  Low: <", round(tertile_1_mech, 3), "\n")
cat("  Medium:", round(tertile_1_mech, 3), "-", round(tertile_2_mech, 3), "\n")
cat("  High: ≥", round(tertile_2_mech, 3), "\n\n")

very_low_mech <- ph_share_t0 %>%
  filter(ph_share_of_tract_t0 < tertile_1_mech) %>%
  pull(match_group)

medium_mech <- ph_share_t0 %>%
  filter(ph_share_of_tract_t0 >= tertile_1_mech & ph_share_of_tract_t0 < tertile_2_mech) %>%
  pull(match_group)

very_high_mech <- ph_share_t0 %>%
  filter(ph_share_of_tract_t0 >= tertile_2_mech) %>%
  pull(match_group)

cat("Very low mechanical:", length(very_low_mech), "match groups\n")
cat("Medium mechanical:", length(medium_mech), "match groups\n")
cat("Very high mechanical:", length(very_high_mech), "match groups\n\n")

# Filter datasets
tract_data_very_low_mech <- tract_data_matched %>%
  filter(match_group %in% very_low_mech)

tract_data_medium_mech <- tract_data_matched %>%
  filter(match_group %in% medium_mech)

tract_data_very_high_mech <- tract_data_matched %>%
  filter(match_group %in% very_high_mech)

# Run event studies for very low mechanical
# Only run for treated neighborhoods (inner ring doesn't have PH)
results_event_study_very_low_mech <- list()
for (outcome in mechanical_effects_outcome_vars) {
  cat("Event study for", outcome, "| treated | Very low mechanical\n")

  results <- did_event_study(
    input_data = tract_data_very_low_mech,
    outcome_var = outcome,
    treatment_group = "treated"
  )

  results_event_study_very_low_mech[[paste0(outcome, "_treated")]] <- results$twfe_conley
}

# Run event studies for medium mechanical
# Only run for treated neighborhoods (inner ring doesn't have PH)
results_event_study_medium_mech <- list()
for (outcome in mechanical_effects_outcome_vars) {
  cat("Event study for", outcome, "| treated | Medium mechanical\n")

  results <- did_event_study(
    input_data = tract_data_medium_mech,
    outcome_var = outcome,
    treatment_group = "treated"
  )

  results_event_study_medium_mech[[paste0(outcome, "_treated")]] <- results$twfe_conley
}

# Run event studies for very high mechanical
# Only run for treated neighborhoods (inner ring doesn't have PH)
results_event_study_very_high_mech <- list()
for (outcome in mechanical_effects_outcome_vars) {
  cat("Event study for", outcome, "| treated | Very high mechanical\n")

  results <- did_event_study(
    input_data = tract_data_very_high_mech,
    outcome_var = outcome,
    treatment_group = "treated"
  )

  results_event_study_very_high_mech[[paste0(outcome, "_treated")]] <- results$twfe_conley
}

# Create three-group comparison plot
p_mechanical_three <- create_heterogeneity_plot(
  results_event_study_very_low_mech, paste0("Low (<", round(tertile_1_mech*100, 1), "%)"),
  results_event_study_medium_mech, paste0("Medium (", round(tertile_1_mech*100, 1), "-", round(tertile_2_mech*100, 1), "%)"),
  results_event_study_very_high_mech, paste0("High (>=", round(tertile_2_mech*100, 1), "%)"),
  outcomes_to_plot = mechanical_effects_combined_plot_vars,
  outcome_labels_map = race_income_labels,
  title_text = "Mechanical Effects by PH Population Share at t=0 (Terciles)",
  colors = c("#1B5E20", "#F57C00", "#B71C1C"),  # Dark green, orange, dark red
  save_name = "mechanical_effects_terciles_heterogeneity",
  results_dir = heterogeneity_results_dir
)

print(p_mechanical_three)

# ---- Analysis 4: Five-way split by quintiles ----

cat("\n--- Five-way Mechanical Effects Split (Quintiles) ---\n")

# Create five groups based on quintiles
quintile_1_mech <- quantile(ph_share_t0$ph_share_of_tract_t0, 0.20, na.rm = TRUE)
quintile_2_mech <- quantile(ph_share_t0$ph_share_of_tract_t0, 0.40, na.rm = TRUE)
quintile_3_mech <- quantile(ph_share_t0$ph_share_of_tract_t0, 0.60, na.rm = TRUE)
quintile_4_mech <- quantile(ph_share_t0$ph_share_of_tract_t0, 0.80, na.rm = TRUE)

cat("Mechanical effect quintiles (PH share at t=0):\n")
cat("  Q1: <", round(quintile_1_mech*100, 1), "%\n")
cat("  Q2:", round(quintile_1_mech*100, 1), "-", round(quintile_2_mech*100, 1), "%\n")
cat("  Q3:", round(quintile_2_mech*100, 1), "-", round(quintile_3_mech*100, 1), "%\n")
cat("  Q4:", round(quintile_3_mech*100, 1), "-", round(quintile_4_mech*100, 1), "%\n")
cat("  Q5: ≥", round(quintile_4_mech*100, 1), "%\n\n")

q1_mech <- ph_share_t0 %>%
  filter(ph_share_of_tract_t0 < quintile_1_mech) %>%
  pull(match_group)

q2_mech <- ph_share_t0 %>%
  filter(ph_share_of_tract_t0 >= quintile_1_mech & ph_share_of_tract_t0 < quintile_2_mech) %>%
  pull(match_group)

q3_mech <- ph_share_t0 %>%
  filter(ph_share_of_tract_t0 >= quintile_2_mech & ph_share_of_tract_t0 < quintile_3_mech) %>%
  pull(match_group)

q4_mech <- ph_share_t0 %>%
  filter(ph_share_of_tract_t0 >= quintile_3_mech & ph_share_of_tract_t0 < quintile_4_mech) %>%
  pull(match_group)

q5_mech <- ph_share_t0 %>%
  filter(ph_share_of_tract_t0 >= quintile_4_mech) %>%
  pull(match_group)

cat("Q1 (very low mechanical):", length(q1_mech), "match groups\n")
cat("Q2 (low mechanical):", length(q2_mech), "match groups\n")
cat("Q3 (moderate mechanical):", length(q3_mech), "match groups\n")
cat("Q4 (moderate-high mechanical):", length(q4_mech), "match groups\n")
cat("Q5 (high mechanical):", length(q5_mech), "match groups\n\n")

# Filter datasets
tract_data_q1_mech <- tract_data_matched %>%
  filter(match_group %in% q1_mech)

tract_data_q2_mech <- tract_data_matched %>%
  filter(match_group %in% q2_mech)

tract_data_q3_mech <- tract_data_matched %>%
  filter(match_group %in% q3_mech)

tract_data_q4_mech <- tract_data_matched %>%
  filter(match_group %in% q4_mech)

tract_data_q5_mech <- tract_data_matched %>%
  filter(match_group %in% q5_mech)

# Run event studies for Q1
# Only run for treated neighborhoods (inner ring doesn't have PH)
results_event_study_q1_mech <- list()
for (outcome in mechanical_effects_outcome_vars) {
  cat("Event study for", outcome, "| treated | Q1 mechanical\n")

  results <- did_event_study(
    input_data = tract_data_q1_mech,
    outcome_var = outcome,
    treatment_group = "treated"
  )

  results_event_study_q1_mech[[paste0(outcome, "_treated")]] <- results$twfe_conley
}

# Run event studies for Q2
# Only run for treated neighborhoods (inner ring doesn't have PH)
results_event_study_q2_mech <- list()
for (outcome in mechanical_effects_outcome_vars) {
  cat("Event study for", outcome, "| treated | Q2 mechanical\n")

  results <- did_event_study(
    input_data = tract_data_q2_mech,
    outcome_var = outcome,
    treatment_group = "treated"
  )

  results_event_study_q2_mech[[paste0(outcome, "_treated")]] <- results$twfe_conley
}

# Run event studies for Q3
# Only run for treated neighborhoods (inner ring doesn't have PH)
results_event_study_q3_mech <- list()
for (outcome in mechanical_effects_outcome_vars) {
  cat("Event study for", outcome, "| treated | Q3 mechanical\n")

  results <- did_event_study(
    input_data = tract_data_q3_mech,
    outcome_var = outcome,
    treatment_group = "treated"
  )

  results_event_study_q3_mech[[paste0(outcome, "_treated")]] <- results$twfe_conley
}

# Run event studies for Q4
# Only run for treated neighborhoods (inner ring doesn't have PH)
results_event_study_q4_mech <- list()
for (outcome in mechanical_effects_outcome_vars) {
  cat("Event study for", outcome, "| treated | Q4 mechanical\n")

  results <- did_event_study(
    input_data = tract_data_q4_mech,
    outcome_var = outcome,
    treatment_group = "treated"
  )

  results_event_study_q4_mech[[paste0(outcome, "_treated")]] <- results$twfe_conley
}

# Run event studies for Q5
# Only run for treated neighborhoods (inner ring doesn't have PH)
results_event_study_q5_mech <- list()
for (outcome in mechanical_effects_outcome_vars) {
  cat("Event study for", outcome, "| treated | Q5 mechanical\n")

  results <- did_event_study(
    input_data = tract_data_q5_mech,
    outcome_var = outcome,
    treatment_group = "treated"
  )

  results_event_study_q5_mech[[paste0(outcome, "_treated")]] <- results$twfe_conley
}

# Create five-group comparison plot
# Note: create_heterogeneity_plot only handles 2-3 groups, so we'll create a custom plot
# For now, create pairwise comparisons: Q1 vs Q3 vs Q5 (bottom, middle, top)

p_mechanical_quintiles <- create_heterogeneity_plot(
  results_event_study_q1_mech, paste0("Q1 (<", round(quintile_1_mech*100, 1), "%)"),
  results_event_study_q3_mech, paste0("Q3 (", round(quintile_2_mech*100, 1), "-", round(quintile_3_mech*100, 1), "%)"),
  results_event_study_q5_mech, paste0("Q5 (>=", round(quintile_4_mech*100, 1), "%)"),
  outcomes_to_plot = mechanical_effects_combined_plot_vars,
  outcome_labels_map = race_income_labels,
  title_text = "Mechanical Effects by PH Population Share at t=0 (Quintiles: Q1, Q3, Q5)",
  colors = c("#1B5E20", "#F57C00", "#B71C1C"),  # Dark green, orange, dark red
  save_name = "mechanical_effects_quintiles_135_heterogeneity",
  results_dir = heterogeneity_results_dir
)

print(p_mechanical_quintiles)

# Additional plot: Q1 vs Q2 vs Q3 vs Q4 vs Q5 using all five quintiles
# Create separate plots for treated and spillover, showing all 5 quintiles
cat("\n--- Creating 5-quintile comparison plots ---\n")

# We'll need to manually create these since create_heterogeneity_plot only handles 2-3 groups
# Extract estimates at t=1 and t=3 for all quintiles
het_q1 <- extract_event_time_estimates(results_event_study_q1_mech, paste0("Q1 (<", round(quintile_1_mech*100, 1), "%)"))
het_q2 <- extract_event_time_estimates(results_event_study_q2_mech, paste0("Q2 (", round(quintile_1_mech*100, 1), "-", round(quintile_2_mech*100, 1), "%)"))
het_q3 <- extract_event_time_estimates(results_event_study_q3_mech, paste0("Q3 (", round(quintile_2_mech*100, 1), "-", round(quintile_3_mech*100, 1), "%)"))
het_q4 <- extract_event_time_estimates(results_event_study_q4_mech, paste0("Q4 (", round(quintile_3_mech*100, 1), "-", round(quintile_4_mech*100, 1), "%)"))
het_q5 <- extract_event_time_estimates(results_event_study_q5_mech, paste0("Q5 (>=", round(quintile_4_mech*100, 1), "%)"))

all_quintiles_het <- bind_rows(het_q1, het_q2, het_q3, het_q4, het_q5) %>%
  mutate(
    group = str_extract(outcome_group, "(treated|inner)$"),
    outcome_clean = str_remove(outcome_group, "_(treated|inner)$")
  ) %>%
  filter(outcome_clean %in% mechanical_effects_plot_vars)

# Plot for treated neighborhoods only (spillovers not needed for mechanical effects test)
quintile_colors <- c("#1B5E20", "#66BB6A", "#FFA726", "#F57C00", "#B71C1C")

# Create slides subdirectory
slides_dir <- here(heterogeneity_results_dir, "slides")
dir.create(slides_dir, recursive = TRUE, showWarnings = FALSE)

for (outcome_var in mechanical_effects_plot_vars) {
  outcome_label <- race_income_labels[outcome_var]

  plot_data_treated <- all_quintiles_het %>%
    filter(outcome_clean == outcome_var, group == "treated")

  # Regular version
  p <- ggplot(plot_data_treated, aes(x = location, y = estimate, color = location)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                  width = 0.2, linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = quintile_colors) +
    labs(
      title = paste0("Treated Neighborhoods: ", outcome_label),
      subtitle = "Effect at t=20 by PH Population Share Quintile",
      x = "PH Share Quintile at t=0",
      y = "Coefficient at t=20",
      color = "Quintile"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  ggsave(
    here(heterogeneity_results_dir, paste0("mechanical_quintiles_all5_", outcome_var, "_treated.pdf")),
    p, width = 8, height = 6
  )

  # Slides version - larger text, cleaner
  p_slides <- ggplot(plot_data_treated, aes(x = location, y = estimate, color = location)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                  width = 0.3, linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    scale_color_manual(values = quintile_colors) +
    labs(
      x = "PH Share Quintile at t=0",
      y = "Coefficient at t=20"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14),
      axis.title = element_text(face = "bold", size = 16),
      legend.position = "none",
      panel.grid.minor = element_blank()
    )

  ggsave(
    here(slides_dir, paste0("mechanical_quintiles_all5_", outcome_var, "_treated.pdf")),
    p_slides, width = 8, height = 6
  )
}

cat("Five-quintile plots saved (treated neighborhoods only, slides versions in /slides/)\n")

# ---- Extract and Compare Heterogeneity Coefficients (t=1 and t=3) ----

cat("\n=== HETEROGENEITY COEFFICIENT EXTRACTION (t=1 and t=3) ===\n")

# Extract estimates at t=1 and t=3 for all heterogeneity analyses
het_small_largest <- extract_event_time_estimates(results_event_study_small_largest, "Small Projects (Largest)")
het_medium_largest <- extract_event_time_estimates(results_event_study_medium_largest, "Medium Projects (Largest)")
het_large_largest <- extract_event_time_estimates(results_event_study_large_largest, "Large Projects (Largest)")

het_small_total <- extract_event_time_estimates(results_event_study_small_total, "Small Projects (Total)")
het_medium_total <- extract_event_time_estimates(results_event_study_medium_total, "Medium Projects (Total)")
het_large_total <- extract_event_time_estimates(results_event_study_large_total, "Large Projects (Total)")

het_very_low_black <- extract_event_time_estimates(results_event_study_very_low_black, "Very Low Black Share")
het_medium_black <- extract_event_time_estimates(results_event_study_medium_black, "Medium Black Share")
het_high_black <- extract_event_time_estimates(results_event_study_high_black, "High Black Share")

het_low_income <- extract_event_time_estimates(results_event_study_low_income, "Low Income")
het_high_income <- extract_event_time_estimates(results_event_study_high_income, "High Income")

het_nyc <- extract_event_time_estimates(results_event_study_nyc, "NYC")
het_non_nyc <- extract_event_time_estimates(results_event_study_non_nyc, "Non-NYC")

# Only extract high-rise estimates if they were computed
if ("has_highrise_project" %in% names(tract_data_matched)) {
  het_highrise <- extract_event_time_estimates(results_event_study_highrise, "High-rise")
  het_non_highrise <- extract_event_time_estimates(results_event_study_non_highrise, "Non-high-rise")
}

het_ur <- extract_event_time_estimates(results_event_study_ur, "Urban Renewal")
het_non_ur <- extract_event_time_estimates(results_event_study_non_ur, "Non-Urban Renewal")

het_early <- extract_event_time_estimates(results_event_study_early, "Early Projects")
het_late <- extract_event_time_estimates(results_event_study_late, "Late Projects")

# Extract mechanical effects estimates
het_low_mech_20 <- extract_event_time_estimates(results_event_study_low_mech_20, "Low Mechanical (<20%)")
het_high_mech_20 <- extract_event_time_estimates(results_event_study_high_mech_20, "High Mechanical (≥20%)")
het_low_mech_10 <- extract_event_time_estimates(results_event_study_low_mech_10, "Low Mechanical (<10%)")
het_high_mech_10 <- extract_event_time_estimates(results_event_study_high_mech_10, "High Mechanical (≥10%)")
het_very_low_mech <- extract_event_time_estimates(results_event_study_very_low_mech, "Very Low Mechanical (Tercile)")
het_medium_mech <- extract_event_time_estimates(results_event_study_medium_mech, "Medium Mechanical (Tercile)")
het_very_high_mech <- extract_event_time_estimates(results_event_study_very_high_mech, "Very High Mechanical (Tercile)")

# Combine all estimates and save
all_het_estimates <- bind_rows(
  het_small_largest, het_medium_largest, het_large_largest,
  het_small_total, het_medium_total, het_large_total,
  het_very_low_black, het_medium_black, het_high_black,
  het_low_income, het_high_income,
  het_nyc, het_non_nyc,
  het_ur, het_non_ur,
  het_early, het_late,
  het_low_mech_20, het_high_mech_20,
  het_low_mech_10, het_high_mech_10,
  het_very_low_mech, het_medium_mech, het_very_high_mech
)

# Add high-rise estimates if available
if ("has_highrise_project" %in% names(tract_data_matched)) {
  all_het_estimates <- bind_rows(all_het_estimates, het_highrise, het_non_highrise)
}

# Save results
write_csv(all_het_estimates, here(heterogeneity_results_dir, "all_heterogeneity_estimates.csv"))
cat("Saved heterogeneity estimates to:", here(heterogeneity_results_dir, "all_heterogeneity_estimates.csv"), "\n")

message("\n=== PART 3 COMPLETE: Heterogeneity analysis finished ===")
message("Files saved to: ", heterogeneity_results_dir)
message("\n--- Project Characteristics ---")
message("- project_size_three_groups_heterogeneity.pdf")
message("\n--- Baseline Neighborhood Characteristics ---")
message("- baseline_black_three_groups_heterogeneity.pdf")
message("- baseline_income_heterogeneity.pdf")
message("\n--- Geographic/Policy Characteristics ---")
message("- nyc_heterogeneity.pdf")
message("- highrise_heterogeneity.pdf")
message("- urban_renewal_heterogeneity.pdf")
message("- early_late_projects_heterogeneity.pdf")
message("\n--- Mechanical Effects Analysis ---")
message("- mechanical_effects_10pct_heterogeneity.pdf")
message("- mechanical_effects_20pct_heterogeneity.pdf")
message("- mechanical_effects_terciles_heterogeneity.pdf")
message("- mechanical_effects_quintiles_135_heterogeneity.pdf (Q1, Q3, Q5)")
message("- mechanical_quintiles_all5_asinh_median_income_treated.pdf")
message("- mechanical_quintiles_all5_asinh_median_income_spillover.pdf")
message("- mechanical_quintiles_all5_asinh_median_rent_calculated_treated.pdf")
message("- mechanical_quintiles_all5_asinh_median_rent_calculated_spillover.pdf")
message("- mechanical_quintiles_all5_asinh_pop_black_treated.pdf")
message("- mechanical_quintiles_all5_asinh_pop_black_spillover.pdf")
message("- mechanical_quintiles_all5_asinh_pop_white_treated.pdf")
message("- mechanical_quintiles_all5_asinh_pop_white_spillover.pdf")
# Summary table commented out to avoid timeout errors
