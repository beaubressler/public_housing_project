## 02_run_regressions.R
## Spatial DiD: Core regression analysis
## Part 2 of the spatial DiD analysis pipeline
## Requires 01_setup_and_balance.R to be run first

if (!exists("PART1_DONE") || !PART1_DONE) {
  message("Running Part 1 first...")
  source(here("code", "learn", "stacked_did", "01_setup_and_balance.R"))
}

PART2_DONE <- FALSE

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(fixest)
})

message("\n=== SPATIAL DID PART 2: REGRESSIONS ===")

# ---- Load helper functions ----
source(here("code", "learn", "stacked_did", "helpers", "spatial_did_event_study.R"))
source(here("code", "learn", "stacked_did", "helpers", "save_spatial_did_estimates.R"))

# ---- Output directory ----
did_output_dir <- here("output", "regression_results", "stacked_did", data_type)
dir.create(did_output_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Define outcomes to analyze ----
outcome_variables <- c(
  # Population shares
  "black_share", "white_share",

  # Log population
  "asinh_pop_total", "asinh_pop_black", "asinh_pop_white",

  # Population levels
  "total_pop", "black_pop", "white_pop",

  # Private population (log)
  "asinh_private_pop", "asinh_private_black_pop", "asinh_private_white_pop",

  # Private population (levels)
  "private_population_estimate", "private_black_population_estimate",
  "private_white_population_estimate",

  # Economic outcomes
  "median_income", "asinh_median_income",
  "median_rent_calculated", "asinh_median_rent_calculated",
  "median_home_value_calculated", "asinh_median_home_value_calculated",

  # Labor market
  "unemp_rate", "lfp_rate",

  # Education
  "pct_hs_grad"
)

# ---- Run regressions for all outcomes ----
message("\nRunning event study regressions...")
all_spatial_did_results <- list()

for (outcome in outcome_variables) {
  message(sprintf("  Running: %s", outcome))

  tryCatch({
    result <- run_event_study_regression(event_study_data_rings, outcome)

    # Store results for both treated and inner
    all_spatial_did_results[[paste0(outcome, "_treated")]] <- list(
      coefficients = result$coefficients %>% filter(location_type == "treated"),
      summary = result$summary
    )

    all_spatial_did_results[[paste0(outcome, "_inner")]] <- list(
      coefficients = result$coefficients %>% filter(location_type == "inner"),
      summary = result$summary
    )
  }, error = function(e) {
    message(sprintf("    ERROR: %s - %s", outcome, e$message))
  })
}

message(sprintf("\nCompleted %d outcome regressions", length(outcome_variables)))

# ---- Save coefficient estimates ----
message("\nFormatting and saving estimates...")

# Create tidy coefficient data frame
all_coefficients <- save_estimates(all_spatial_did_results, "Spatial DiD")

# Format for robustness comparison
spatial_did_coefficients <- format_for_robustness_comparison(
  all_coefficients,
  spec = "spatial_did",
  spec_label = "Spatial DiD (Rings)"
)

# Save to CSV
write_csv(spatial_did_coefficients,
          here(did_output_dir, "event_study_coefficients.csv"))

message("Saved coefficients to: ", here(did_output_dir, "event_study_coefficients.csv"))

# ---- Summary statistics ----
cat("\n=== REGRESSION SUMMARY ===\n")
cat("Outcomes analyzed:", length(outcome_variables), "\n")
cat("Total regressions run:", length(all_spatial_did_results), "\n")
cat("Output directory:", did_output_dir, "\n")

# ---- Export results for next script ----
message("\n=== Part 2 complete: Regressions estimated and saved ===")

PART2_DONE <- TRUE
