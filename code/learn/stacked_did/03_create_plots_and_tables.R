## 03_create_plots_and_tables.R
## Spatial DiD: Visualization and tables
## Part 3 of the spatial DiD analysis pipeline
## Requires 01 and 02 to be run first

if (!exists("PART2_DONE") || !PART2_DONE) {
  message("Running Parts 1 and 2 first...")
  source(here("code", "learn", "stacked_did", "01_setup_and_balance.R"))
  source(here("code", "learn", "stacked_did", "02_run_regressions.R"))
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(ggplot2)
  library(patchwork)
  library(tinytable)
})

message("\n=== SPATIAL DID PART 3: PLOTS AND TABLES ===")

# Load helpers
source(here("code", "learn", "stacked_did", "helpers", "plot_spatial_did_event_studies.R"))
source(here("code", "helpers", "color_palettes.R"))

# ---- Output directories ----
results_dir <- here("output", "regression_results", "stacked_did", data_type)
slides_dir <- file.path(results_dir, "slides")
tables_dir <- file.path(results_dir, "tables")

dir.create(slides_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Prepare coefficient data for plotting ----
message("\nPreparing coefficient data...")

# Read saved coefficients
spatial_coefficients <- read_csv(here(results_dir, "event_study_coefficients.csv"),
                                 show_col_types = FALSE)

# ---- Main Publication Plots: Treated Neighborhoods ----
message("\n=== Creating Treated Neighborhood Plots ===\n")

# Plot 1: Log population by race (treated)
treated_pop_log_labels <- c(
  "asinh_pop_total" = "Log Total Population",
  "asinh_pop_white" = "Log White Population",
  "asinh_pop_black" = "Log Black Population"
)

treated_pop_log_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("asinh_pop_total", "asinh_pop_white", "asinh_pop_black"),
  outcome_labels_map = treated_pop_log_labels,
  title_text = NULL,
  group = "treated"
)

ggsave(file.path(results_dir, "event_study_pop_log_by_race_treated.pdf"),
       treated_pop_log_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_pop_log_by_race_treated.pdf"),
       treated_pop_log_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 2: Population levels by race (treated)
treated_pop_levels_labels <- c(
  "total_pop" = "Total Population",
  "white_pop" = "White Population",
  "black_pop" = "Black Population"
)

treated_pop_levels_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("total_pop", "white_pop", "black_pop"),
  outcome_labels_map = treated_pop_levels_labels,
  title_text = NULL,
  group = "treated"
)

ggsave(file.path(results_dir, "event_study_pop_levels_by_race_treated.pdf"),
       treated_pop_levels_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_pop_levels_by_race_treated.pdf"),
       treated_pop_levels_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 3: Log private population (treated)
treated_private_pop_log_labels <- c(
  "asinh_private_pop" = "Private Population",
  "asinh_private_white_pop" = "Private White Population",
  "asinh_private_black_pop" = "Private Black Population"
)

treated_private_pop_log_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("asinh_private_pop", "asinh_private_white_pop", "asinh_private_black_pop"),
  outcome_labels_map = treated_private_pop_log_labels,
  title_text = NULL,
  group = "treated"
)

ggsave(file.path(results_dir, "event_study_private_pop_log_treated.pdf"),
       treated_private_pop_log_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_private_pop_log_treated.pdf"),
       treated_private_pop_log_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 4: Population shares (treated)
treated_pop_shares_labels <- c(
  "black_share" = "Black Population Share",
  "white_share" = "White Population Share"
)

treated_pop_shares_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("black_share", "white_share"),
  outcome_labels_map = treated_pop_shares_labels,
  title_text = NULL,
  group = "treated",
  colors = c(okabe_ito[["orange"]], okabe_ito[["sky_blue"]])
)

ggsave(file.path(results_dir, "event_study_pop_shares_treated.pdf"),
       treated_pop_shares_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_pop_shares_treated.pdf"),
       treated_pop_shares_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 5: Rent and income (treated)
treated_rent_income_labels <- c(
  "asinh_median_rent_calculated" = "Log Median Rent",
  "asinh_median_income" = "Log Median Income"
)

treated_rent_income_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("asinh_median_rent_calculated", "asinh_median_income"),
  outcome_labels_map = treated_rent_income_labels,
  title_text = NULL,
  group = "treated",
  colors = c(okabe_ito[["sky_blue"]], okabe_ito[["bluish_green"]])
)

ggsave(file.path(results_dir, "event_study_rent_income_treated.pdf"),
       treated_rent_income_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_rent_income_treated.pdf"),
       treated_rent_income_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 6: Labor market outcomes (treated)
treated_labor_market_labels <- c(
  "unemp_rate" = "Unemployment Rate",
  "lfp_rate" = "Labor Force Participation Rate"
)

treated_labor_market_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("unemp_rate", "lfp_rate"),
  outcome_labels_map = treated_labor_market_labels,
  title_text = NULL,
  group = "treated",
  colors = c(okabe_ito[["vermillion"]], okabe_ito[["sky_blue"]])
)

ggsave(file.path(results_dir, "event_study_labor_market_treated.pdf"),
       treated_labor_market_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_labor_market_treated.pdf"),
       treated_labor_market_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 7: Home values and education (treated)
treated_home_education_labels <- c(
  "asinh_median_home_value_calculated" = "Log Median Home Value",
  "pct_hs_grad" = "HS Graduation Rate"
)

treated_home_education_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("asinh_median_home_value_calculated", "pct_hs_grad"),
  outcome_labels_map = treated_home_education_labels,
  title_text = NULL,
  group = "treated",
  colors = c(okabe_ito[["reddish_purple"]], okabe_ito[["orange"]])
)

ggsave(file.path(results_dir, "event_study_home_education_treated.pdf"),
       treated_home_education_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_home_education_treated.pdf"),
       treated_home_education_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# ---- Spillover Plots (Inner Ring) ----
message("\n=== Creating Spillover Neighborhood Plots ===\n")

# Create same plots for inner ring
spillover_pop_log_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("asinh_pop_total", "asinh_pop_white", "asinh_pop_black"),
  outcome_labels_map = treated_pop_log_labels,
  title_text = NULL,
  group = "inner"
)
ggsave(file.path(results_dir, "event_study_pop_log_by_race_spillover.pdf"),
       spillover_pop_log_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_pop_log_by_race_spillover.pdf"),
       spillover_pop_log_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

spillover_pop_levels_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("total_pop", "white_pop", "black_pop"),
  outcome_labels_map = treated_pop_levels_labels,
  title_text = NULL,
  group = "inner"
)
ggsave(file.path(results_dir, "event_study_pop_levels_by_race_spillover.pdf"),
       spillover_pop_levels_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_pop_levels_by_race_spillover.pdf"),
       spillover_pop_levels_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

spillover_private_pop_log_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("asinh_private_pop", "asinh_private_white_pop", "asinh_private_black_pop"),
  outcome_labels_map = treated_private_pop_log_labels,
  title_text = NULL,
  group = "inner"
)
ggsave(file.path(results_dir, "event_study_private_pop_log_spillover.pdf"),
       spillover_private_pop_log_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_private_pop_log_spillover.pdf"),
       spillover_private_pop_log_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

spillover_pop_shares_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("black_share", "white_share"),
  outcome_labels_map = treated_pop_shares_labels,
  title_text = NULL,
  group = "inner",
  colors = c(okabe_ito[["orange"]], okabe_ito[["sky_blue"]])
)
ggsave(file.path(results_dir, "event_study_pop_shares_spillover.pdf"),
       spillover_pop_shares_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_pop_shares_spillover.pdf"),
       spillover_pop_shares_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

spillover_rent_income_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("asinh_median_rent_calculated", "asinh_median_income"),
  outcome_labels_map = treated_rent_income_labels,
  title_text = NULL,
  group = "inner",
  colors = c(okabe_ito[["sky_blue"]], okabe_ito[["bluish_green"]])
)
ggsave(file.path(results_dir, "event_study_rent_income_spillover.pdf"),
       spillover_rent_income_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_rent_income_spillover.pdf"),
       spillover_rent_income_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

spillover_labor_market_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("unemp_rate", "lfp_rate"),
  outcome_labels_map = treated_labor_market_labels,
  title_text = NULL,
  group = "inner",
  colors = c(okabe_ito[["vermillion"]], okabe_ito[["sky_blue"]])
)
ggsave(file.path(results_dir, "event_study_labor_market_spillover.pdf"),
       spillover_labor_market_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_labor_market_spillover.pdf"),
       spillover_labor_market_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

spillover_home_education_plot <- make_overlay_plot(
  df = spatial_coefficients,
  outcomes = c("asinh_median_home_value_calculated", "pct_hs_grad"),
  outcome_labels_map = treated_home_education_labels,
  title_text = NULL,
  group = "inner",
  colors = c(okabe_ito[["reddish_purple"]], okabe_ito[["orange"]])
)
ggsave(file.path(results_dir, "event_study_home_education_spillover.pdf"),
       spillover_home_education_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_home_education_spillover.pdf"),
       spillover_home_education_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

message("All plots saved successfully!")
message("- Main plots: ", results_dir)
message("- Slide versions: ", slides_dir)

# ---- Summary Tables ----
message("\n=== Creating summary tables ===")

# Calculate baseline means
baseline_means <- event_study_data_rings %>%
  filter(event_time == -10, location_type == "treated") %>%
  summarise(
    black_share = mean(black_share, na.rm = TRUE),
    total_pop = mean(total_pop, na.rm = TRUE),
    median_income = mean(median_income, na.rm = TRUE),
    median_rent_calculated = mean(median_rent_calculated, na.rm = TRUE),
    unemp_rate = mean(unemp_rate, na.rm = TRUE)
  )

message("Baseline means calculated")

# Effect at t=+20 table
effects_t20 <- spatial_coefficients %>%
  filter(term == "20", group == "treated") %>%
  filter(outcome_clean %in% c("black_share", "total_pop", "median_income",
                              "median_rent_calculated", "unemp_rate")) %>%
  select(outcome_clean, estimate, std.error, p.value)

# Add baseline means and percentage changes
effects_t20_table <- effects_t20 %>%
  mutate(
    baseline_mean = case_when(
      outcome_clean == "black_share" ~ baseline_means$black_share,
      outcome_clean == "total_pop" ~ baseline_means$total_pop,
      outcome_clean == "median_income" ~ baseline_means$median_income,
      outcome_clean == "median_rent_calculated" ~ baseline_means$median_rent_calculated,
      outcome_clean == "unemp_rate" ~ baseline_means$unemp_rate
    ),
    percent_change = (estimate / baseline_mean) * 100,
    outcome_label = case_when(
      outcome_clean == "black_share" ~ "Black Population Share",
      outcome_clean == "total_pop" ~ "Total Population",
      outcome_clean == "median_income" ~ "Median Income",
      outcome_clean == "median_rent_calculated" ~ "Median Rent",
      outcome_clean == "unemp_rate" ~ "Unemployment Rate"
    )
  ) %>%
  select(outcome_label, estimate, std.error, p.value, baseline_mean, percent_change)

# Save table
main_table <- tt(effects_t20_table,
                caption = "Main Treatment Effects: Impact 20 Years After Public Housing Construction (Spatial DiD)\\label{tab:spatial_did_main_results}") %>%
  style_tt(font_size = 0.9) %>%
  format_tt(j = c("estimate", "std.error", "baseline_mean"), digits = 3, escape = FALSE) %>%
  format_tt(j = "percent_change", digits = 1, escape = FALSE) %>%
  format_tt(j = "p.value", digits = 3, escape = FALSE) %>%
  theme_tt("resize")

save_tt(main_table, here(tables_dir, "main_results_table.tex"), overwrite = TRUE)
message("Main results table saved to: ", here(tables_dir, "main_results_table.tex"))

message("\n=== PART 3 COMPLETE: Plots and tables created ===")
message("Files saved to: ", results_dir)
