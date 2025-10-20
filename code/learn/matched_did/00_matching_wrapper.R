## part0_matching_wrapper.R
## Run this *only* when you want to regenerate the matched CSV(s).
## Set VARIANT <- "baseline", "no_cbsa", "cem", or "weighted" in the Console before sourcing, or leave default.
## Set SKIP_GENETIC_IF_EXISTS <- TRUE to skip genetic matching if output already exists (default: TRUE)

if (!exists("VARIANT")) VARIANT <- "baseline"  # "baseline" | "no_cbsa" | "cem" | "weighted" | "genetic" | "caliper"
if (!exists("SKIP_GENETIC_IF_EXISTS")) SKIP_GENETIC_IF_EXISTS <- TRUE

# Paths
suppressPackageStartupMessages({
  library(here)
})

matching_dir    <- here("code", "learn", "matched_did", "matching")
script_baseline <- file.path(matching_dir, "run_matching_algorithms.R")
script_no_cbsa  <- file.path(matching_dir, "run_matching_algorithms_no_cbsa.R")
script_cem      <- file.path(matching_dir, "run_matching_cem.R")
script_weighted <- file.path(matching_dir, "run_matching_weighted.R")
script_genetic  <- file.path(matching_dir, "run_matching_genetic.R")

# Where Part 1 expects the matched CSV
data_type  <- get0("data_type", ifnotfound = "combined")
matched_dir <- here("data", "derived", "merged", data_type, "matched_dataset", VARIANT)
dir.create(matched_dir, recursive = TRUE, showWarnings = FALSE)
canonical_csv <- file.path(matched_dir, "tract_data_matched_1_year.csv")

message(">>> Running matching for VARIANT = ", VARIANT)

if (identical(VARIANT, "genetic")) {
  if (SKIP_GENETIC_IF_EXISTS && file.exists(canonical_csv)) {
    message(">>> Genetic matching output already exists. Skipping matching step.")
    message(">>> Set SKIP_GENETIC_IF_EXISTS <- FALSE to regenerate, or delete ", canonical_csv)
  } else {
    message(">>> Running genetic matching (this may take a long time)...")
    source(script_genetic)
  }
} else if (identical(VARIANT, "weighted")) {
  source(script_weighted)
} else if (identical(VARIANT, "no_cbsa")) {
  source(script_no_cbsa)
} else if (identical(VARIANT, "cem")) {
  source(script_cem)
} else if (identical(VARIANT, "caliper")) {
  # Caliper matching uses baseline script with caliper flag
  variant <- "caliper"  # Set variant for the script to pick up
  source(script_baseline)
} else {
  source(script_baseline)
}

## If your matching script writes elsewhere, uncomment+edit the copy below:
# file.copy(here("data","derived","tmp","tract_data_matched_1_year_replacement.csv"),
#           canonical_csv, overwrite = TRUE)

message("Matching step finished.")