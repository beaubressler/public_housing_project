## run_complete_analysis.R
## Runs the complete matched DiD pipeline for both baseline and no_cbsa variants

suppressPackageStartupMessages({
  library(here)
})

#variants_to_run <- c("baseline", "no_cbsa", "cem", "caliper", "genetic")
#variants_to_run <- c("baseline", "no_cbsa", "caliper")  # Uncomment to run only genetic variant for testing
variants_to_run <- c("baseline")  # Uncomment to run only genetic variant for testing

# datasets_to_run <- c("tract_data_matched_1_year.csv", "tract_data_matched_2_year.csv")
datasets_to_run <- "tract_data_matched_2_year.csv"  # Uncomment to run only 2-year dataset for testing

# weighting_to_run <- c("unweighted", "pop_weighted")
#weighting_to_run <- c("unweighted","pop_weighted")
weighting_to_run <- c("unweighted")

# Control whether to skip genetic matching if output already exists
# This is because the genetic matching step can take hours, depending on the pop.size parameter
SKIP_GENETIC_IF_EXISTS <- TRUE  # Set to FALSE to force regeneration of genetic matching

for (variant in variants_to_run) {
  for (dataset in datasets_to_run) {

    # Matching only needs to run once per variant/dataset combo (not per weighting)
    cat("\n=== RUNNING VARIANT:", variant, "| DATASET:", dataset, "===\n")

    VARIANT <<- variant
    MATCHED_DATASET <<- dataset

    cat("Running matching...\n")
    source(here("code", "learn", "matched_did", "00_matching_wrapper.R"))

    # Now run analysis for each weighting type
    for (weighting in weighting_to_run) {
      cat("\n--- WEIGHTING:", weighting, "---\n")

      WEIGHTING <<- weighting

      cat("Running models...\n")
      source(here("code", "learn", "matched_did", "01_setup_and_models.R"))

      cat("Creating results...\n")
      source(here("code", "learn", "matched_did", "02_main_results.R"))

      # cat("Running heterogeneity analysis...\n")
      source(here("code", "learn", "matched_did", "03_heterogeneity_analysis.R"))
    }
  }
}

cat("\nRunning robustness comparison across matching specifications...\n")
source(here("code", "learn", "matched_did", "02b_robustness_comparison.R"))

cat("\nRunning heterogeneity robustness comparison across matching specifications...\n")
source(here("code", "learn", "matched_did", "03b_heterogeneity_robustness.R"))

cat("\nALL DONE!\n")
rm(VARIANT, MATCHED_DATASET, WEIGHTING, envir = .GlobalEnv)
