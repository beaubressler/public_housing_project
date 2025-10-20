## 04_heterogeneity_analysis.R
## Spatial DiD: Heterogeneity analysis skeleton
## Part 4 of the spatial DiD analysis pipeline
## TO BE IMPLEMENTED: Full heterogeneity analysis

# This script sets up the structure for heterogeneity analysis
# Full implementation can follow the pattern in run_spatial_did_regressions.R lines 652-944

if (!exists("PART2_DONE") || !PART2_DONE) {
  message("Running Parts 1 and 2 first...")
  source(here("code", "learn", "stacked_did", "01_setup_and_balance.R"))
  source(here("code", "learn", "stacked_did", "02_run_regressions.R"))
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(fixest)
})

message("\n=== SPATIAL DID PART 4: HETEROGENEITY ANALYSIS (SKELETON) ===")

# Load helper functions
source(here("code", "learn", "stacked_did", "helpers", "spatial_did_event_study.R"))
source(here("code", "learn", "stacked_did", "helpers", "plot_spatial_did_event_studies.R"))

# ---- Output directories ----
het_output_dir <- here("output", "regression_results", "stacked_did", data_type, "heterogeneity")
dir.create(file.path(het_output_dir, "by_size"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(het_output_dir, "by_initial_share"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(het_output_dir, "by_policy_era"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(het_output_dir, "by_city"), recursive = TRUE, showWarnings = FALSE)

# ---- Define heterogeneity dimensions ----
message("\nSetting up heterogeneity dimensions...")

# 1. Project size categories
treated_first_row <- event_study_data_rings %>%
  filter(location_type == "treated") %>%
  group_by(treated_id) %>%
  filter(row_number() == 1)

units_25p <- quantile(treated_first_row$total_public_housing_units, probs = 0.25, na.rm = TRUE)
units_50p <- quantile(treated_first_row$total_public_housing_units, probs = 0.50, na.rm = TRUE)
units_75p <- quantile(treated_first_row$total_public_housing_units, probs = 0.75, na.rm = TRUE)

event_study_data_rings <- event_study_data_rings %>%
  mutate(
    size = case_when(
      total_public_housing_units < units_25p ~ "25th",
      total_public_housing_units >= units_25p & total_public_housing_units < units_50p ~ "50th",
      total_public_housing_units >= units_50p & total_public_housing_units < units_75p ~ "75th",
      total_public_housing_units >= units_75p ~ "75th_to_max"
    ),
    size_simple = case_when(
      total_public_housing_units < units_50p ~ "small",
      total_public_housing_units >= units_50p ~ "large"
    ),
    policy_era = case_when(
      treatment_year < 1960 ~ "pre_1960",
      treatment_year >= 1960 ~ "post_1960"
    )
  )

# 2. Pre-treatment racial composition (tipping thresholds)
pre_treatment_blk_sh <- event_study_data_rings %>%
  filter(event_time == -10, location_type == "treated") %>%
  select(black_share, treated_id) %>%
  rename(pre_treatment_blk_sh_treated = black_share)

event_study_data_rings <- left_join(event_study_data_rings, pre_treatment_blk_sh) %>%
  mutate(pre_treatment_blk_sh = case_when(
    pre_treatment_blk_sh_treated < 0.05 ~ "below_tipping",     # <5% - Safe from white flight
    pre_treatment_blk_sh_treated < 0.12 ~ "tipping_range",     # 5-12% - Tipping zone
    pre_treatment_blk_sh_treated >= 0.12 ~ "above_tipping"     # >=12% - Already tipped
  ))

message("\nHeterogeneity dimensions created:")
message("- Project size: ", length(unique(event_study_data_rings$size_simple)), " categories")
message("- Pre-treatment black share: ", length(unique(na.omit(event_study_data_rings$pre_treatment_blk_sh))), " categories")
message("- Policy era: ", length(unique(na.omit(event_study_data_rings$policy_era))), " categories")

# ---- TODO: Implement heterogeneity analysis ----
message("\n=== TO BE IMPLEMENTED ===")
message("Heterogeneity analysis will be implemented following this structure:")
message("1. By project size (small vs large)")
message("2. By baseline racial composition (below/in/above tipping point)")
message("3. By policy era (pre-1960 vs post-1960)")
message("4. By city (NYC vs others)")
message("\nRefer to run_spatial_did_regressions.R lines 652-944 for implementation template")
message("\nOutput directories created:")
message("- ", het_output_dir)

# ---- Example implementation structure (commented out) ----
# For each heterogeneity dimension:
# 1. Filter data by category
# 2. Run event study regressions for each category
# 3. Create plots comparing categories
# 4. Save results to appropriate subdirectories

# Example for size heterogeneity:
# for(size_group in unique(event_study_data_rings$size_simple)) {
#   filtered_data <- event_study_data_rings %>% filter(size_simple == size_group)
#
#   # Run regressions for key outcomes
#   black_share_result <- run_event_study_regression(filtered_data, "black_share")
#
#   # Create and save plots
#   plot <- create_event_study_plot(black_share_result, filtered_data, "black_share",
#                                   paste("Black Share - Size:", size_group))
#
#   ggsave(file.path(het_output_dir, "by_size",
#                    paste0("black_share_", size_group, ".pdf")),
#          plot, width = 10, height = 5)
# }

message("\n=== PART 4 SKELETON COMPLETE ===")
