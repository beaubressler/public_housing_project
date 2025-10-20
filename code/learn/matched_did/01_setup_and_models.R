## part1_setup_and_models.R
## Loads data, runs matched DiD event-studies, builds tidy coef frames & magnitudes.
PART1_DONE <- FALSE

suppressPackageStartupMessages({
  library(MatchIt)
  library(tidyverse)
  library(sf)
  library(lmtest)
  library(fixest)
  library(here)
  library(RColorBrewer)
  library(cobalt)
  library(tableone)
  library(kableExtra)
  library(DRDID)
  library(did)
  library(splines)
  library(modelsummary)
  library(stringr)
})

set.seed(123456L)

# ---- switches ----
VARIANT <- get0("VARIANT", ifnotfound = "baseline")   # "baseline" | "no_cbsa"
MATCHED_DATASET <- get0("MATCHED_DATASET", ifnotfound = "tract_data_matched_1_year.csv")

# ---- basic knobs ----
data_type   <- get0("data_type", ifnotfound = "combined")
group_types <- c("treated", "inner")

# ---- directories ----
helper_dir       <- here("code", "learn", "matched_did",  "helpers")
merged_data_dir  <- here("data", "derived", "merged", data_type)
match_data_dir   <- here("data", "derived", "merged", data_type, "matched_dataset", VARIANT)
holc_data_dir    <- here("data", "derived", "holc")

results_dir       <- here("output", "regression_results", "matched_did", data_type, VARIANT)
figures_dir       <- here("output", "figures", "matched_did", data_type, VARIANT)
balance_table_dir <- here("output", "balance_tables", "matched_did", data_type, VARIANT)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(balance_table_dir, recursive = TRUE, showWarnings = FALSE)

# ---- file paths ----
tract_with_treatment_status_filepath <- here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")
treated_tracts_panel_filepath        <- here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")
event_study_rings_filepath           <- here(merged_data_dir, "unique_tracts_and_rings.csv")
matched_csv                          <- here(match_data_dir, MATCHED_DATASET)

# ---- outcomes & labels ----
outcome_variables <- c(
  "asinh_private_population_estimate","private_population_estimate",
  "asinh_private_black_population_estimate","private_black_population_estimate",
  "asinh_private_white_population_estimate","private_white_population_estimate",
  "black_share","white_share","total_pop","black_pop","white_pop",
  "asinh_pop_total","asinh_pop_black","asinh_pop_white",
  "median_income","asinh_median_income",
  "median_home_value_calculated","asinh_median_home_value_calculated",
  "median_rent_calculated","asinh_median_rent_calculated",
  "total_units","ln_total_units","lfp_rate","unemp_rate","pct_hs_grad"
)

outcome_labels <- c(
  "asinh_private_population_estimate" = "Log Private Population",
  "asinh_private_black_population_estimate" = "Log Private Black Population",
  "asinh_private_white_population_estimate" = "Log Private White Population",
  "private_population_estimate" = "Private Population",
  "private_black_population_estimate" = "Private Black Population",
  "private_white_population_estimate" = "Private White Population",
  "black_share" = "Black Population Share",
  "white_share" = "White Population Share",
  "total_pop" = "Total Population",
  "black_pop" = "Black Population",
  "white_pop" = "White Population",
  "asinh_pop_total" = "Log Total Population",
  "asinh_pop_black" = "Log Black Population",
  "asinh_pop_white" = "Log White Population",
  "median_income" = "Median Income",
  "asinh_median_income" = "Asinh Median Income",
  "median_home_value_calculated" = "Median Home Value",
  "asinh_median_home_value_calculated" = "Asinh Median Home Value",
  "median_rent_calculated" = "Median Rent",
  "asinh_median_rent_calculated" = "Log Median Rent",
  "total_units" = "Total Housing Units",
  "ln_total_units" = "Log Housing Units",
  "lfp_rate" = "Labor Force Participation Rate",
  "unemp_rate" = "Unemployment Rate",
  "pct_hs_grad" = "HS Graduation Rate"
)

# ---- load helpers ----
source(here(helper_dir, "matched_did_event_study.R"))
source(here(helper_dir, "save_matched_did_estimates.R"))
source(here(helper_dir, "plot_matched_did_event_studies.R"))
source(here(helper_dir, "save_event_study_plots.R"))

# ---- data ----
census_tract_sample_raw <- st_read(tract_with_treatment_status_filepath, quiet = TRUE)
treated_tracts_panel_raw <- st_read(treated_tracts_panel_filepath, quiet = TRUE) %>% st_drop_geometry()
tracts_and_rings <- readr::read_csv(event_study_rings_filepath, show_col_types = FALSE) %>% filter(location_type != "outer")

tract_data_matched <- readr::read_csv(matched_csv, show_col_types = FALSE) %>%
  mutate(ln_total_units = log(total_units))

# ---- baseline analysis: event studies ----
did_results_event_study <- list()
did_results_event_study_conley <- list()
did_results_event_study_no_match_conley <- list()

for (outcome in outcome_variables) {
  for (group in group_types) {
    message("Running: ", outcome, " | ", group)
    results <- did_event_study(tract_data_matched, outcome, group)
    key <- paste(outcome, group, sep = "_")
    did_results_event_study[[key]]            <- results$twfe
    did_results_event_study_conley[[key]]     <- results$twfe_conley
    did_results_event_study_no_match_conley[[key]] <- results$twfe_nomatch_conley
  }
}

# tidy coef frames
event_study_coefs <- save_estimates(did_results_event_study, "TWFE") %>% mutate(standard_errors = "Clustered SE")
event_study_conley_coefs <- save_estimates(did_results_event_study_conley, "TWFE") %>% mutate(standard_errors = "Conley SE")
event_study_no_match_conley <- save_estimates(did_results_event_study_no_match_conley, "TWFE") %>% mutate(standard_errors = "No match, Conley SE")

combined_results <- bind_rows(event_study_coefs, event_study_conley_coefs, event_study_no_match_conley)

# key magnitudes at t=+20 (treated)
effects_t20 <- event_study_conley_coefs %>%
  filter(term == "20", str_ends(outcome, "_treated")) %>%
  select(outcome, estimate, std.error, p.value) %>%
  mutate(outcome_clean = str_remove(outcome, "_treated"))

baseline_means <- tract_data_matched %>%
  filter(group_type == "treated") %>%
  mutate(baseline_year = matched_treatment_year - 10) %>%
  filter(year == baseline_year) %>%
  summarise(
    black_share = mean(black_share, na.rm = TRUE),
    black_pop   = mean(black_pop,   na.rm = TRUE),
    white_pop   = mean(white_pop,   na.rm = TRUE),
    unemp_rate  = mean(unemp_rate,  na.rm = TRUE),
    lfp_rate    = mean(lfp_rate,    na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "outcome_clean", values_to = "baseline_mean")

magnitudes_t20 <- effects_t20 %>%
  left_join(baseline_means, by = "outcome_clean") %>%
  mutate(percent_change = (estimate / baseline_mean) * 100)

cat("\n=== KEY EFFECT MAGNITUDES AT t=+20 (treated) ===\n")
print(
  magnitudes_t20 %>%
    filter(!is.na(baseline_mean)) %>%
    select(outcome, estimate, baseline_mean, percent_change) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
)

# cleaned coefs for plotting
coefs_clean <- event_study_conley_coefs %>%
  mutate(
    group = ifelse(str_ends(outcome, "_treated"), "treated",
                   ifelse(str_ends(outcome, "_inner"), "inner", NA)),
    outcome_clean = str_remove(outcome, "_treated|_inner")
  )

# cleaned coefs for non-stacked Conley plotting
coefs_clean_no_match <- event_study_no_match_conley %>%
  mutate(
    group = ifelse(str_ends(outcome, "_treated"), "treated",
                   ifelse(str_ends(outcome, "_inner"), "inner", NA)),
    outcome_clean = str_remove(outcome, "_treated|_inner")
  )

# Save non-stacked Conley estimates separately -----
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

PART1_DONE <- TRUE
message("\nPart 1 complete: models estimated & tidy frames ready.")
