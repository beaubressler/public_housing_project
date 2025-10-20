## 00_run_complete_analysis.R
## Spatial DiD: Complete analysis pipeline wrapper
## Runs all spatial DiD analysis scripts in sequence

library(here)

message("\n" , paste(rep("=", 70), collapse = ""))
message("SPATIAL DIFFERENCE-IN-DIFFERENCES ANALYSIS PIPELINE")
message(paste(rep("=", 70), collapse = ""), "\n")

# ---- Set data type ----
data_type <- "combined"  # Options: "combined", "digitized", "cdd_large", "cdd_small"

message("Data type: ", data_type)
message("Start time: ", Sys.time(), "\n")

# ---- Run analysis scripts ----

# Part 1: Setup and balance tables
message("\n>>> PART 1: Setup and Balance Tables <<<")
source(here("code", "learn", "stacked_did", "01_setup_and_balance.R"))

# Part 2: Run regressions
message("\n>>> PART 2: Regressions <<<")
source(here("code", "learn", "stacked_did", "02_run_regressions.R"))

# Part 3: Create plots and tables
message("\n>>> PART 3: Plots and Tables <<<")
source(here("code", "learn", "stacked_did", "03_create_plots_and_tables.R"))

# Part 4: Heterogeneity analysis (currently skeleton only)
# Uncomment to run when implemented:
# message("\n>>> PART 4: Heterogeneity Analysis <<<")
# source(here("code", "learn", "stacked_did", "04_heterogeneity_analysis.R"))

# ---- Summary ----
message("\n", paste(rep("=", 70), collapse = ""))
message("SPATIAL DID ANALYSIS COMPLETE")
message(paste(rep("=", 70), collapse = ""))
message("\nEnd time: ", Sys.time())
message("\nOutputs saved to:")
message("  - Balance tables: output/balance_tables/stacked_did/", data_type)
message("  - Regression results: output/regression_results/stacked_did/", data_type)
message("  - Plots: output/regression_results/stacked_did/", data_type, "/slides")
message("  - Tables: output/regression_results/stacked_did/", data_type, "/tables")
