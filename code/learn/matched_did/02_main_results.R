## 02_main_results.R
## Creates publication-quality plots and tables from the main event study estimates
## Requires 01_setup_and_models.R to be run first

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

# Load color palettes and table utilities
source(here("code", "helpers", "color_palettes.R"))
source(here("code", "helpers", "table_utilities.R"))

message("\n=== PART 2: MAIN RESULTS VISUALIZATION ===")

# ---- Helper function for overlay event study plots ----
make_overlay_plot <- function(df, outcomes, outcome_labels_map, title_text, group = "treated",
                             colors = outcome_colors,
                             x_breaks = seq(-30, 30, 10)) {

  plot_data <- df %>%
    filter(group == !!group, outcome_clean %in% outcomes) %>%
    mutate(
      clean_label = outcome_labels_map[outcome_clean],
      clean_label = factor(clean_label, levels = outcome_labels_map[outcomes]),
      event_time = as.numeric(term)
    )

  # Add reference points at event_time = -10
  ref_rows <- plot_data %>%
    distinct(outcome_clean, clean_label) %>%
    mutate(event_time = -10, estimate = 0, std.error = 0)

  plot_data <- bind_rows(plot_data, ref_rows) %>%
    arrange(clean_label, event_time)

  ggplot(plot_data, aes(x = event_time, y = estimate, color = clean_label)) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                      ymax = estimate + 1.96*std.error),
                  width = 0.6, linewidth = 1.3, alpha = 0.7,
                  position = position_dodge(width = 2.5)) +
    # geom_line(aes(group = clean_label), linewidth = 0.9,
    #           position = position_dodge(width = 2.5)) +
    geom_point(size = 3.5, stroke = 0,
               position = position_dodge(width = 2.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey50") +
    labs(
      title = title_text,
      x = "Years Relative to Construction",
      y = "Difference-in-Differences Estimate",
      color = ""
    ) +
    scale_x_continuous(breaks = x_breaks) +
    scale_color_manual(values = colors) +
    theme_classic(base_size = 14) +
    theme(
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.margin = margin(t = 10),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8))
    )
}

# ---- Output Directories Setup ----

dataset_type <- str_remove(get0("MATCHED_DATASET", ifnotfound = "tract_data_matched_1_year.csv"), "^tract_data_matched_") %>% str_remove("\\.csv$")
# Use results_dir from 01_setup_and_models.R, append dataset_type
# Note: results_dir = here("output", "regression_results", "matched_did", data_type, VARIANT)
results_dir_with_dataset <- here("output", "regression_results", "matched_did", data_type, VARIANT, dataset_type)
dir.create(results_dir_with_dataset, recursive = TRUE, showWarnings = FALSE)

slides_dir <- file.path(results_dir_with_dataset, "slides")
dir.create(slides_dir, recursive = TRUE, showWarnings = FALSE)

tables_dir <- file.path(results_dir_with_dataset, "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

non_conley_dir <- file.path(results_dir_with_dataset, "non-conley")
dir.create(non_conley_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(coefs_clean, file.path(results_dir_with_dataset, "event_study_coefficients.csv"))
write_csv(coefs_clean_no_match, file.path(results_dir_with_dataset, "event_study_coefficients_no_match_conley.csv"))
message("Saved coefficient estimates to: ", results_dir_with_dataset)

# ---- Main Publication Plots: Treated Neighborhoods ----

message("\n=== Creating Treated Neighborhood Plots ===\n")

# Plot 1: Log population by race (treated)
treated_pop_log_labels <- c(
  "asinh_pop_total" = "Log Total Population",
  "asinh_pop_white" = "Log White Population",
  "asinh_pop_black" = "Log Black Population"
)

treated_pop_log_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("asinh_pop_total", "asinh_pop_white", "asinh_pop_black"),
  outcome_labels_map = treated_pop_log_labels,
  title_text = NULL
)

ggsave(file.path(results_dir_with_dataset, "event_study_pop_log_by_race_treated.pdf"),
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
  df = coefs_clean,
  outcomes = c("total_pop", "white_pop", "black_pop"),
  outcome_labels_map = treated_pop_levels_labels,
  title_text = NULL
)

ggsave(file.path(results_dir_with_dataset, "event_study_pop_levels_by_race_treated.pdf"),
       treated_pop_levels_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_pop_levels_by_race_treated.pdf"),
       treated_pop_levels_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 3: Log private population (treated)
treated_private_pop_log_labels <- c(
  "asinh_private_population_estimate" = "Private Population",
  "asinh_private_white_population_estimate" = "Private White Population",
  "asinh_private_black_population_estimate" = "Private Black Population"
)

treated_private_pop_log_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("asinh_private_population_estimate", "asinh_private_white_population_estimate",
               "asinh_private_black_population_estimate"),
  outcome_labels_map = treated_private_pop_log_labels,
  title_text = NULL
)

ggsave(file.path(results_dir_with_dataset, "event_study_private_pop_log_treated.pdf"),
       treated_private_pop_log_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_private_pop_log_treated.pdf"),
       treated_private_pop_log_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 4: Private population levels (treated)
treated_private_pop_levels_labels <- c(
  "private_population_estimate" = "Private Population",
  "private_white_population_estimate" = "Private White Population",
  "private_black_population_estimate" = "Private Black Population"
)

treated_private_pop_levels_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("private_population_estimate", "private_white_population_estimate",
               "private_black_population_estimate"),
  outcome_labels_map = treated_private_pop_levels_labels,
  title_text = NULL
)

ggsave(file.path(results_dir_with_dataset, "event_study_private_pop_levels_treated.pdf"),
       treated_private_pop_levels_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_private_pop_levels_treated.pdf"),
       treated_private_pop_levels_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 5: Population shares (treated)
treated_pop_shares_labels <- c(
  "black_share" = "Black Population Share",
  "white_share" = "White Population Share"
)

treated_pop_shares_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("black_share", "white_share"),
  outcome_labels_map = treated_pop_shares_labels,
  title_text = NULL,
  colors = c(okabe_ito[["orange"]], okabe_ito[["sky_blue"]])
)

ggsave(file.path(results_dir_with_dataset, "event_study_pop_shares_treated.pdf"),
       treated_pop_shares_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_pop_shares_treated.pdf"),
       treated_pop_shares_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 6: Rent and income (treated)
treated_rent_income_labels <- c(
  "asinh_median_rent_calculated" = "Log Median Rent",
  "asinh_median_income" = "Log Median Income"
)

treated_rent_income_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("asinh_median_rent_calculated", "asinh_median_income"),
  outcome_labels_map = treated_rent_income_labels,
  title_text = NULL,
  colors = c(okabe_ito[["sky_blue"]], okabe_ito[["bluish_green"]])
)

ggsave(file.path(results_dir_with_dataset, "event_study_rent_income_treated.pdf"),
       treated_rent_income_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_rent_income_treated.pdf"),
       treated_rent_income_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 7: Labor market outcomes (treated)
treated_labor_market_labels <- c(
  "unemp_rate" = "Unemployment Rate",
  "lfp_rate" = "Labor Force Participation Rate"
)

treated_labor_market_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("unemp_rate", "lfp_rate"),
  outcome_labels_map = treated_labor_market_labels,
  title_text = NULL,
  colors = c(okabe_ito[["vermillion"]], okabe_ito[["sky_blue"]])
)

ggsave(file.path(results_dir_with_dataset, "event_study_labor_market_treated.pdf"),
       treated_labor_market_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_labor_market_treated.pdf"),
       treated_labor_market_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 8: Home values and education (treated)
treated_home_education_labels <- c(
  "asinh_median_home_value_calculated" = "Log Median Home Value",
  "pct_hs_grad" = "HS Graduation Rate"
)

treated_home_education_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("asinh_median_home_value_calculated", "pct_hs_grad"),
  outcome_labels_map = treated_home_education_labels,
  title_text = NULL,
  colors = c(okabe_ito[["reddish_purple"]], okabe_ito[["orange"]])
)

ggsave(file.path(results_dir_with_dataset, "event_study_home_education_treated.pdf"),
       treated_home_education_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_home_education_treated.pdf"),
       treated_home_education_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# ---- Spillover Plots ----

message("\n=== Creating Spillover Neighborhood Plots ===\n")

# Plot 8: Log population by race (spillover)
spillover_pop_log_labels <- c(
  "asinh_pop_total" = "Log Total Population",
  "asinh_pop_white" = "Log White Population",
  "asinh_pop_black" = "Log Black Population"
)

spillover_pop_log_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("asinh_pop_total", "asinh_pop_white", "asinh_pop_black"),
  outcome_labels_map = spillover_pop_log_labels,
  title_text = NULL,
  group = "inner"
)

ggsave(file.path(results_dir_with_dataset, "event_study_pop_log_by_race_spillover.pdf"),
       spillover_pop_log_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_pop_log_by_race_spillover.pdf"),
       spillover_pop_log_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 9: Population levels by race (spillover)
spillover_pop_levels_labels <- c(
  "total_pop" = "Total Population",
  "white_pop" = "White Population",
  "black_pop" = "Black Population"
)

spillover_pop_levels_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("total_pop", "white_pop", "black_pop"),
  outcome_labels_map = spillover_pop_levels_labels,
  title_text = NULL,
  group = "inner"
)

ggsave(file.path(results_dir_with_dataset, "event_study_pop_levels_by_race_spillover.pdf"),
       spillover_pop_levels_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_pop_levels_by_race_spillover.pdf"),
       spillover_pop_levels_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 10: Population shares (spillover)
spillover_pop_shares_labels <- c(
  "black_share" = "Black Population Share",
  "white_share" = "White Population Share"
)

spillover_pop_shares_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("black_share", "white_share"),
  outcome_labels_map = spillover_pop_shares_labels,
  title_text = NULL,
  colors = c(okabe_ito[["orange"]], okabe_ito[["sky_blue"]]),
  group = "inner"
)

ggsave(file.path(results_dir_with_dataset, "event_study_pop_shares_spillover.pdf"),
       spillover_pop_shares_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_pop_shares_spillover.pdf"),
       spillover_pop_shares_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 11: Rent and income (spillover)
spillover_rent_income_labels <- c(
  "asinh_median_rent_calculated" = "Log Median Rent",
  "asinh_median_income" = "Log Median Income"
)

spillover_rent_income_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("asinh_median_rent_calculated", "asinh_median_income"),
  outcome_labels_map = spillover_rent_income_labels,
  title_text = NULL,
  colors = c(okabe_ito[["sky_blue"]], okabe_ito[["bluish_green"]]),
  group = "inner"
)

ggsave(file.path(results_dir_with_dataset, "event_study_rent_income_spillover.pdf"),
       spillover_rent_income_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_rent_income_spillover.pdf"),
       spillover_rent_income_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 12: Labor market outcomes (spillover)
spillover_labor_market_labels <- c(
  "unemp_rate" = "Unemployment Rate",
  "lfp_rate" = "Labor Force Participation Rate"
)

spillover_labor_market_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("unemp_rate", "lfp_rate"),
  outcome_labels_map = spillover_labor_market_labels,
  title_text = NULL,
  colors = c(okabe_ito[["vermillion"]], okabe_ito[["sky_blue"]]),
  group = "inner"
)

ggsave(file.path(results_dir_with_dataset, "event_study_labor_market_spillover.pdf"),
       spillover_labor_market_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_labor_market_spillover.pdf"),
       spillover_labor_market_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# Plot 13: Home values and education (spillover)
spillover_home_education_labels <- c(
  "asinh_median_home_value_calculated" = "Log Median Home Value",
  "pct_hs_grad" = "HS Graduation Rate"
)

spillover_home_education_plot <- make_overlay_plot(
  df = coefs_clean,
  outcomes = c("asinh_median_home_value_calculated", "pct_hs_grad"),
  outcome_labels_map = spillover_home_education_labels,
  title_text = NULL,
  colors = c(okabe_ito[["reddish_purple"]], okabe_ito[["orange"]]),
  group = "inner"
)

ggsave(file.path(results_dir_with_dataset, "event_study_home_education_spillover.pdf"),
       spillover_home_education_plot, width = 8.5, height = 7, device = cairo_pdf)
ggsave(file.path(slides_dir, "event_study_home_education_spillover.pdf"),
       spillover_home_education_plot + labs(title = NULL), width = 8.5, height = 7, device = cairo_pdf)

# ---- Non-Conley Robustness Plots ----

message("\n=== Creating Non-Conley Robustness Plots ===\n")

# Treated plots with non-conley SEs
nc_treated_pop_log_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("asinh_pop_total", "asinh_pop_white", "asinh_pop_black"),
  outcome_labels_map = treated_pop_log_labels,
  title_text = "Effects on (Log) Population by Race, Treated (Non-Conley SE)"
)
ggsave(file.path(non_conley_dir, "event_study_pop_log_by_race_treated.pdf"),
       nc_treated_pop_log_plot, width = 8.5, height = 7, device = cairo_pdf)

nc_treated_pop_levels_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("total_pop", "white_pop", "black_pop"),
  outcome_labels_map = treated_pop_levels_labels,
  title_text = "Effects on Population by Race (Levels), Treated (Non-Conley SE)"
)
ggsave(file.path(non_conley_dir, "event_study_pop_levels_by_race_treated.pdf"),
       nc_treated_pop_levels_plot, width = 8.5, height = 7, device = cairo_pdf)

nc_treated_private_pop_log_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("asinh_private_population_estimate", "asinh_private_white_population_estimate",
               "asinh_private_black_population_estimate"),
  outcome_labels_map = treated_private_pop_log_labels,
  title_text = "Effects on (Log) Private Population, Treated (Non-Conley SE)"
)
ggsave(file.path(non_conley_dir, "event_study_private_pop_log_treated.pdf"),
       nc_treated_private_pop_log_plot, width = 8.5, height = 7, device = cairo_pdf)

nc_treated_private_pop_levels_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("private_population_estimate", "private_white_population_estimate",
               "private_black_population_estimate"),
  outcome_labels_map = treated_private_pop_levels_labels,
  title_text = "Effects on Private Population (Levels), Treated (Non-Conley SE)"
)
ggsave(file.path(non_conley_dir, "event_study_private_pop_levels_treated.pdf"),
       nc_treated_private_pop_levels_plot, width = 8.5, height = 7, device = cairo_pdf)

nc_treated_pop_shares_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("black_share", "white_share"),
  outcome_labels_map = treated_pop_shares_labels,
  title_text = "Effects on Population Shares, Treated (Non-Conley SE)",
  colors = c(okabe_ito[["orange"]], okabe_ito[["sky_blue"]])
)
ggsave(file.path(non_conley_dir, "event_study_pop_shares_treated.pdf"),
       nc_treated_pop_shares_plot, width = 8.5, height = 7, device = cairo_pdf)

nc_treated_rent_income_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("asinh_median_rent_calculated", "asinh_median_income"),
  outcome_labels_map = treated_rent_income_labels,
  title_text = "Effects on Rent and Income, Treated (Non-Conley SE)",
  colors = c(okabe_ito[["sky_blue"]], okabe_ito[["bluish_green"]])
)
ggsave(file.path(non_conley_dir, "event_study_rent_income_treated.pdf"),
       nc_treated_rent_income_plot, width = 8.5, height = 7, device = cairo_pdf)

nc_treated_labor_market_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("unemp_rate", "lfp_rate"),
  outcome_labels_map = treated_labor_market_labels,
  title_text = "Effects on Labor Market, Treated (Non-Conley SE)",
  colors = c(okabe_ito[["vermillion"]], okabe_ito[["sky_blue"]])
)
ggsave(file.path(non_conley_dir, "event_study_labor_market_treated.pdf"),
       nc_treated_labor_market_plot, width = 8.5, height = 7, device = cairo_pdf)

nc_treated_home_education_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("asinh_median_home_value_calculated", "pct_hs_grad"),
  outcome_labels_map = treated_home_education_labels,
  title_text = "Effects on Home Values and Education, Treated (Non-Conley SE)",
  colors = c(okabe_ito[["reddish_purple"]], okabe_ito[["orange"]])
)
ggsave(file.path(non_conley_dir, "event_study_home_education_treated.pdf"),
       nc_treated_home_education_plot, width = 8.5, height = 7, device = cairo_pdf)

# Spillover plots with non-conley SEs
nc_spillover_pop_log_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("asinh_pop_total", "asinh_pop_white", "asinh_pop_black"),
  outcome_labels_map = spillover_pop_log_labels,
  title_text = "Effects on (Log) Population by Race, Spillover (Non-Conley SE)",
  group = "inner"
)
ggsave(file.path(non_conley_dir, "event_study_pop_log_by_race_spillover.pdf"),
       nc_spillover_pop_log_plot, width = 8.5, height = 7, device = cairo_pdf)

nc_spillover_pop_levels_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("total_pop", "white_pop", "black_pop"),
  outcome_labels_map = spillover_pop_levels_labels,
  title_text = "Effects on Population by Race (Levels), Spillover (Non-Conley SE)",
  group = "inner"
)
ggsave(file.path(non_conley_dir, "event_study_pop_levels_by_race_spillover.pdf"),
       nc_spillover_pop_levels_plot, width = 8.5, height = 7, device = cairo_pdf)

nc_spillover_pop_shares_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("black_share", "white_share"),
  outcome_labels_map = spillover_pop_shares_labels,
  title_text = "Effects on Population Shares, Spillover (Non-Conley SE)",
  colors = c(okabe_ito[["orange"]], okabe_ito[["sky_blue"]]),
  group = "inner"
)
ggsave(file.path(non_conley_dir, "event_study_pop_shares_spillover.pdf"),
       nc_spillover_pop_shares_plot, width = 8.5, height = 7, device = cairo_pdf)

nc_spillover_rent_income_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("asinh_median_rent_calculated", "asinh_median_income"),
  outcome_labels_map = spillover_rent_income_labels,
  title_text = "Effects on Rent and Income, Spillover (Non-Conley SE)",
  colors = c(okabe_ito[["sky_blue"]], okabe_ito[["bluish_green"]]),
  group = "inner"
)
ggsave(file.path(non_conley_dir, "event_study_rent_income_spillover.pdf"),
       nc_spillover_rent_income_plot, width = 8.5, height = 7, device = cairo_pdf)

nc_spillover_labor_market_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("unemp_rate", "lfp_rate"),
  outcome_labels_map = spillover_labor_market_labels,
  title_text = "Effects on Labor Market, Spillover (Non-Conley SE)",
  colors = c(okabe_ito[["vermillion"]], okabe_ito[["sky_blue"]]),
  group = "inner"
)
ggsave(file.path(non_conley_dir, "event_study_labor_market_spillover.pdf"),
       nc_spillover_labor_market_plot, width = 8.5, height = 7, device = cairo_pdf)

nc_spillover_home_education_plot <- make_overlay_plot(
  df = coefs_clean_no_match,
  outcomes = c("asinh_median_home_value_calculated", "pct_hs_grad"),
  outcome_labels_map = spillover_home_education_labels,
  title_text = "Effects on Home Values and Education, Spillover (Non-Conley SE)",
  colors = c(okabe_ito[["reddish_purple"]], okabe_ito[["orange"]]),
  group = "inner"
)
ggsave(file.path(non_conley_dir, "event_study_home_education_spillover.pdf"),
       nc_spillover_home_education_plot, width = 8.5, height = 7, device = cairo_pdf)

message("All plots saved successfully!")
message("- Main plots: ", results_dir_with_dataset)
message("- Slide versions: ", slides_dir)
message("- Non-Conley robustness: ", non_conley_dir)

# ---- Results Tables ----

# Table 1: Main estimates at t=+20 for key outcomes
main_estimates_t20 <- effects_t20 %>%
  filter(outcome_clean %in% c("black_share", "total_pop", "median_income", "median_rent_calculated",
                               "asinh_median_home_value_calculated", "pct_hs_grad")) %>%
  left_join(baseline_means, by = "outcome_clean") %>%
  mutate(
    percent_change = (estimate / baseline_mean) * 100,
    outcome_label = case_when(
      outcome_clean == "black_share" ~ "Black Population Share",
      outcome_clean == "total_pop" ~ "Total Population",
      outcome_clean == "median_income" ~ "Median Income",
      outcome_clean == "median_rent_calculated" ~ "Median Rent",
      outcome_clean == "asinh_median_home_value_calculated" ~ "Log Median Home Value",
      outcome_clean == "pct_hs_grad" ~ "HS Graduation Rate",
      TRUE ~ outcome_clean
    )
  ) %>%
  select(outcome_label, estimate, std.error, p.value, baseline_mean, percent_change)

main_table <- tt(main_estimates_t20,
                caption = "Main Treatment Effects: Impact 20 Years After Public Housing Construction\\label{tab:main_results}") %>%
  style_tt(font_size = 0.9) %>%
  format_tt(
    j = c("estimate", "std.error", "baseline_mean"),
    digits = 3,
    escape = FALSE
  ) %>%
  format_tt(
    j = "percent_change",
    digits = 1,
    escape = FALSE
  ) %>%
  format_tt(
    j = "p.value",
    digits = 3,
    escape = FALSE
  ) %>%
  theme_tt("resize")

save_tt(main_table, here(results_dir_with_dataset, "main_results_table.tex"), overwrite = TRUE)
message("Main results table saved to: ", here(results_dir_with_dataset, "main_results_table.tex"))

# ---- Summary Statistics Table ----
# Calculate baseline statistics for both treated and spillover neighborhoods
summary_stats_treated <- tract_data_matched %>%
  filter(group_type == "treated") %>%
  mutate(baseline_year = matched_treatment_year - 10) %>%
  filter(year == baseline_year) %>%
  summarise(
    across(c(black_share, total_pop, median_income, median_rent_calculated,
             asinh_median_home_value_calculated, pct_hs_grad, unemp_rate),
           list(mean = ~ mean(.x, na.rm = TRUE),
                sd = ~ sd(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything()) %>%
  separate(name, into = c("variable", "statistic"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(group = "Treated")

summary_stats_spillover <- tract_data_matched %>%
  filter(group_type == "inner") %>%
  mutate(baseline_year = matched_treatment_year - 10) %>%
  filter(year == baseline_year) %>%
  summarise(
    across(c(black_share, total_pop, median_income, median_rent_calculated,
             asinh_median_home_value_calculated, pct_hs_grad, unemp_rate),
           list(mean = ~ mean(.x, na.rm = TRUE),
                sd = ~ sd(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything()) %>%
  separate(name, into = c("variable", "statistic"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(group = "Spillover")

# Combine and format
summary_stats_combined <- bind_rows(summary_stats_treated, summary_stats_spillover) %>%
  mutate(
    variable_label = case_when(
      variable == "black_share" ~ "Black Population Share",
      variable == "total_pop" ~ "Total Population",
      variable == "median_income" ~ "Median Income (1950$)",
      variable == "median_rent_calculated" ~ "Median Rent (1950$)",
      variable == "asinh_median_home_value_calculated" ~ "Log Median Home Value",
      variable == "pct_hs_grad" ~ "HS Graduation Rate",
      variable == "unemp_rate" ~ "Unemployment Rate",
      TRUE ~ variable
    ),
    mean_sd = sprintf("%.2f (%.2f)", mean, sd)
  ) %>%
  select(variable_label, group, mean_sd) %>%
  pivot_wider(names_from = group, values_from = mean_sd) %>%
  select(Variable = variable_label, Treated, Spillover)

summary_table <- tt(summary_stats_combined,
                   caption = "Summary Statistics: Baseline Characteristics (t = -10)\\label{tab:summary_stats}") %>%
  style_tt(font_size = 0.9) %>%
  format_tt(escape = FALSE) %>%
  theme_tt("resize")

save_tt(summary_table, here(results_dir_with_dataset, "summary_statistics_table.tex"), overwrite = TRUE)
message("Summary statistics table saved to: ", here(results_dir_with_dataset, "summary_statistics_table.tex"))

# ---- Event Study Coefficient Tables ----
# Create tables showing coefficients at each event time for key outcomes
# Layout: rows = event times, columns = outcomes

# Key outcomes for the table
key_outcomes <- c("black_share", "asinh_pop_black", "asinh_median_income",
                  "asinh_median_rent_calculated", "pct_hs_grad", "unemp_rate")

outcome_labels_for_table <- c(
  "black_share" = "Black Share",
  "asinh_pop_black" = "Log Black Pop",
  "asinh_median_income" = "Log Income",
  "asinh_median_rent_calculated" = "Log Rent",
  "pct_hs_grad" = "HS Grad Rate",
  "unemp_rate" = "Unemp Rate"
)

# Event times to include in table
event_times <- c(-20, -10, 0, 10, 20, 30)

# Function to create coefficient table for a given group
create_coef_table <- function(coef_data, group_name, outcomes, event_times, outcome_labels) {

  # Filter for the specified group and outcomes
  table_data <- coef_data %>%
    filter(group == group_name,
           outcome_clean %in% outcomes,
           as.numeric(term) %in% event_times) %>%
    mutate(
      event_time = as.numeric(term),
      # Format estimate with stars and SE in parentheses
      coef_formatted = sprintf("%.3f%s (%.3f)",
                               estimate,
                               case_when(
                                 p.value < 0.01 ~ "***",
                                 p.value < 0.05 ~ "**",
                                 p.value < 0.10 ~ "*",
                                 TRUE ~ ""
                               ),
                               std.error)
    ) %>%
    select(outcome_clean, event_time, coef_formatted)

  # Reshape to wide format (rows=event times, columns=outcomes)
  table_wide <- table_data %>%
    pivot_wider(names_from = outcome_clean, values_from = coef_formatted) %>%
    arrange(event_time) %>%
    mutate(event_time = sprintf("t = %+d", event_time)) %>%
    rename(`Event Time` = event_time)

  # Reorder columns and rename with labels
  cols_to_select <- c("Event Time", outcomes)
  table_wide <- table_wide %>%
    select(all_of(cols_to_select[cols_to_select %in% names(table_wide)]))

  # Rename outcome columns to use labels
  for (i in seq_along(outcomes)) {
    if (outcomes[i] %in% names(table_wide)) {
      names(table_wide)[names(table_wide) == outcomes[i]] <- outcome_labels[outcomes[i]]
    }
  }

  return(table_wide)
}

# Create table for treated neighborhoods
coef_table_treated <- create_coef_table(
  coef_data = coefs_clean,
  group_name = "treated",
  outcomes = key_outcomes,
  event_times = event_times,
  outcome_labels = outcome_labels_for_table
)

# Save treated table
treated_coef_table <- tt(coef_table_treated,
                        caption = "Event Study Coefficients: Treated Neighborhoods\\label{tab:event_study_coefs_treated}",
                        notes = "Conley standard errors (2km radius) in parentheses. * p<0.10, ** p<0.05, *** p<0.01") %>%
  style_tt(font_size = 0.85) %>%
  format_tt(escape = FALSE) %>%
  theme_tt("resize")

save_tt(treated_coef_table, here(results_dir_with_dataset, "event_study_coefficients_treated.tex"), overwrite = TRUE)
message("Treated event study coefficient table saved to: ", here(results_dir_with_dataset, "event_study_coefficients_treated.tex"))

# Create table for spillover neighborhoods
coef_table_spillover <- create_coef_table(
  coef_data = coefs_clean,
  group_name = "inner",
  outcomes = key_outcomes,
  event_times = event_times,
  outcome_labels = outcome_labels_for_table
)

# Save spillover table
spillover_coef_table <- tt(coef_table_spillover,
                          caption = "Event Study Coefficients: Spillover Neighborhoods\\label{tab:event_study_coefs_spillover}",
                          notes = "Conley standard errors (2km radius) in parentheses. * p<0.10, ** p<0.05, *** p<0.01") %>%
  style_tt(font_size = 0.85) %>%
  format_tt(escape = FALSE) %>%
  theme_tt("resize")

save_tt(spillover_coef_table, here(results_dir_with_dataset, "event_study_coefficients_spillover.tex"), overwrite = TRUE)
message("Spillover event study coefficient table saved to: ", here(results_dir_with_dataset, "event_study_coefficients_spillover.tex"))

# ---- Percentage Change Tables for Asinh Coefficients ----
# Convert asinh coefficients to approximate percentage changes
# For asinh(x), coefficient β ≈ percentage change when expressed as (exp(β) - 1) * 100
# Or more simply, β ≈ percentage change / 100 for small changes

# Function to create percentage change table
create_pct_change_table <- function(coef_data, group_name, outcomes, event_times, outcome_labels) {

  # Filter for the specified group and outcomes
  table_data <- coef_data %>%
    filter(group == group_name,
           outcome_clean %in% outcomes,
           as.numeric(term) %in% event_times) %>%
    mutate(
      event_time = as.numeric(term),
      # Convert asinh coefficient to approximate percentage change
      # Use sinh(β) for more accurate conversion, but β*100 is close for small β
      pct_change = sinh(estimate) * 100,
      # Format percentage with stars and SE in parentheses
      pct_formatted = sprintf("%.1f\\%%%s (%.1f\\%%)",
                              pct_change,
                              case_when(
                                p.value < 0.01 ~ "***",
                                p.value < 0.05 ~ "**",
                                p.value < 0.10 ~ "*",
                                TRUE ~ ""
                              ),
                              sinh(std.error) * 100)
    ) %>%
    select(outcome_clean, event_time, pct_formatted)

  # Reshape to wide format
  table_wide <- table_data %>%
    pivot_wider(names_from = outcome_clean, values_from = pct_formatted) %>%
    arrange(event_time) %>%
    mutate(event_time = sprintf("t = %+d", event_time)) %>%
    rename(`Event Time` = event_time)

  # Reorder and rename columns
  cols_to_select <- c("Event Time", outcomes)
  table_wide <- table_wide %>%
    select(all_of(cols_to_select[cols_to_select %in% names(table_wide)]))

  for (i in seq_along(outcomes)) {
    if (outcomes[i] %in% names(table_wide)) {
      names(table_wide)[names(table_wide) == outcomes[i]] <- outcome_labels[outcomes[i]]
    }
  }

  return(table_wide)
}

# Asinh outcomes for percentage conversion
asinh_outcomes <- c("asinh_pop_total", "asinh_pop_black", "asinh_pop_white",
                    "asinh_median_income", "asinh_median_rent_calculated")

asinh_outcome_labels <- c(
  "asinh_pop_total" = "Total Pop (\\% chg)",
  "asinh_pop_black" = "Black Pop (\\% chg)",
  "asinh_pop_white" = "White Pop (\\% chg)",
  "asinh_median_income" = "Income (\\% chg)",
  "asinh_median_rent_calculated" = "Rent (\\% chg)"
)

# Create percentage change table for treated
pct_table_treated <- create_pct_change_table(
  coef_data = coefs_clean,
  group_name = "treated",
  outcomes = asinh_outcomes,
  event_times = event_times,
  outcome_labels = asinh_outcome_labels
)

# Save treated percentage table
treated_pct_table <- tt(pct_table_treated,
                       caption = "Event Study Effects as Percentage Changes: Treated Neighborhoods\\label{tab:event_study_pct_treated}",
                       notes = "Percentage changes calculated from asinh coefficients using sinh(β)*100. Conley standard errors (2km radius) in parentheses. * p<0.10, ** p<0.05, *** p<0.01") %>%
  style_tt(font_size = 0.85) %>%
  format_tt(escape = FALSE) %>%
  theme_tt("resize")

save_tt(treated_pct_table, here(results_dir_with_dataset, "event_study_percentages_treated.tex"), overwrite = TRUE)
message("Treated percentage change table saved to: ", here(results_dir_with_dataset, "event_study_percentages_treated.tex"))

# Create percentage change table for spillover
pct_table_spillover <- create_pct_change_table(
  coef_data = coefs_clean,
  group_name = "inner",
  outcomes = asinh_outcomes,
  event_times = event_times,
  outcome_labels = asinh_outcome_labels
)

# Save spillover percentage table
spillover_pct_table <- tt(pct_table_spillover,
                         caption = "Event Study Effects as Percentage Changes: Spillover Neighborhoods\\label{tab:event_study_pct_spillover}",
                         notes = "Percentage changes calculated from asinh coefficients using sinh(β)*100. Conley standard errors (2km radius) in parentheses. * p<0.10, ** p<0.05, *** p<0.01") %>%
  style_tt(font_size = 0.85) %>%
  format_tt(escape = FALSE) %>%
  theme_tt("resize")

save_tt(spillover_pct_table, here(results_dir_with_dataset, "event_study_percentages_spillover.tex"), overwrite = TRUE)
message("Spillover percentage change table saved to: ", here(results_dir_with_dataset, "event_study_percentages_spillover.tex"))

# ---- Detailed Tables for Appendix (Split by Outcome Type) ----
message("\n=== Creating Detailed Tables for Appendix ===\n")

# Define outcome groups for detailed tables
population_demo_outcomes <- c("asinh_pop_black", "asinh_pop_total", "asinh_pop_white", "black_share")
economic_housing_outcomes <- c("asinh_median_income", "asinh_median_rent_calculated",
                               "pct_hs_grad", "unemp_rate")

# LaTeX-compatible outcome labels for appendix tables
pop_demo_labels <- c(
  "asinh_pop_black" = "Log Black Pop",
  "asinh_pop_total" = "Log Total Pop",
  "asinh_pop_white" = "Log White Pop",
  "black_share" = "Black Share"
)

econ_housing_labels <- c(
  "asinh_median_income" = "Log Median Income",
  "asinh_median_rent_calculated" = "Log Median Rent",
  "pct_hs_grad" = "HS Grad Rate",
  "unemp_rate" = "Unemp Rate"
)

# Create detailed tables with Conley SEs
# Treated - Population/Demographics
treated_pop_table <- create_coef_table(
  coef_data = coefs_clean,
  group_name = "treated",
  outcomes = population_demo_outcomes,
  event_times = event_times,
  outcome_labels = pop_demo_labels
)
tt(treated_pop_table) %>%
  theme_tt("tabular") %>%
  save_tt(file.path(tables_dir, "treated_population_demographics.tex"), overwrite = TRUE)
remove_table_wrappers(file.path(tables_dir, "treated_population_demographics.tex"))

# Treated - Economic/Housing
treated_econ_table <- create_coef_table(
  coef_data = coefs_clean,
  group_name = "treated",
  outcomes = economic_housing_outcomes,
  event_times = event_times,
  outcome_labels = econ_housing_labels
)
tt(treated_econ_table) %>%
  theme_tt("tabular") %>%
  save_tt(file.path(tables_dir, "treated_economic_housing.tex"), overwrite = TRUE)
remove_table_wrappers(file.path(tables_dir, "treated_economic_housing.tex"))

# Spillover - Population/Demographics
spillover_pop_table <- create_coef_table(
  coef_data = coefs_clean,
  group_name = "inner",
  outcomes = population_demo_outcomes,
  event_times = event_times,
  outcome_labels = pop_demo_labels
)
tt(spillover_pop_table) %>%
  theme_tt("tabular") %>%
  save_tt(file.path(tables_dir, "spillover_population_demographics.tex"), overwrite = TRUE)
remove_table_wrappers(file.path(tables_dir, "spillover_population_demographics.tex"))

# Spillover - Economic/Housing
spillover_econ_table <- create_coef_table(
  coef_data = coefs_clean,
  group_name = "inner",
  outcomes = economic_housing_outcomes,
  event_times = event_times,
  outcome_labels = econ_housing_labels
)
tt(spillover_econ_table) %>%
  theme_tt("tabular") %>%
  save_tt(file.path(tables_dir, "spillover_economic_housing.tex"), overwrite = TRUE)
remove_table_wrappers(file.path(tables_dir, "spillover_economic_housing.tex"))

# Create detailed tables with Robust SEs (non-Conley)
# Treated - Population/Demographics (Robust)
treated_pop_table_robust <- create_coef_table(
  coef_data = coefs_clean_no_match,
  group_name = "treated",
  outcomes = population_demo_outcomes,
  event_times = event_times,
  outcome_labels = pop_demo_labels
)
tt(treated_pop_table_robust) %>%
  theme_tt("tabular") %>%
  save_tt(file.path(tables_dir, "no_match_conley_treated_population_demographics.tex"), overwrite = TRUE)
remove_table_wrappers(file.path(tables_dir, "no_match_conley_treated_population_demographics.tex"))

# Treated - Economic/Housing (Robust)
treated_econ_table_robust <- create_coef_table(
  coef_data = coefs_clean_no_match,
  group_name = "treated",
  outcomes = economic_housing_outcomes,
  event_times = event_times,
  outcome_labels = econ_housing_labels
)
tt(treated_econ_table_robust) %>%
  theme_tt("tabular") %>%
  save_tt(file.path(tables_dir, "no_match_conley_treated_economic_housing.tex"), overwrite = TRUE)
remove_table_wrappers(file.path(tables_dir, "no_match_conley_treated_economic_housing.tex"))

# Spillover - Population/Demographics (Robust)
spillover_pop_table_robust <- create_coef_table(
  coef_data = coefs_clean_no_match,
  group_name = "inner",
  outcomes = population_demo_outcomes,
  event_times = event_times,
  outcome_labels = pop_demo_labels
)
tt(spillover_pop_table_robust) %>%
  theme_tt("tabular") %>%
  save_tt(file.path(tables_dir, "no_match_conley_spillover_population_demographics.tex"), overwrite = TRUE)
remove_table_wrappers(file.path(tables_dir, "no_match_conley_spillover_population_demographics.tex"))

# Spillover - Economic/Housing (Robust)
spillover_econ_table_robust <- create_coef_table(
  coef_data = coefs_clean_no_match,
  group_name = "inner",
  outcomes = economic_housing_outcomes,
  event_times = event_times,
  outcome_labels = econ_housing_labels
)
tt(spillover_econ_table_robust) %>%
  theme_tt("tabular") %>%
  save_tt(file.path(tables_dir, "no_match_conley_spillover_economic_housing.tex"), overwrite = TRUE)
remove_table_wrappers(file.path(tables_dir, "no_match_conley_spillover_economic_housing.tex"))

message("Detailed tables for appendix saved to: ", tables_dir)

# ---- Baseline Tables with Tract-Clustered SEs ----
message("\n=== Creating Baseline Tables with Tract-Clustered SEs ===\n")

# Create cleaned coefs from event_study_coefs (matched FEs + tract-clustered SEs)
coefs_clean_tract_clustered <- event_study_coefs %>%
  mutate(
    group = ifelse(str_ends(outcome, "_treated"), "treated",
                   ifelse(str_ends(outcome, "_inner"), "inner", NA)),
    outcome_clean = str_remove(outcome, "_treated|_inner")
  )

# Treated - Population/Demographics (Tract-Clustered)
treated_pop_table_tract <- create_coef_table(
  coef_data = coefs_clean_tract_clustered,
  group_name = "treated",
  outcomes = population_demo_outcomes,
  event_times = event_times,
  outcome_labels = pop_demo_labels
)
tt(treated_pop_table_tract) %>%
  theme_tt("tabular") %>%
  save_tt(file.path(tables_dir, "tract_clustered_treated_population_demographics.tex"), overwrite = TRUE)
remove_table_wrappers(file.path(tables_dir, "tract_clustered_treated_population_demographics.tex"))

# Treated - Economic/Housing (Tract-Clustered)
treated_econ_table_tract <- create_coef_table(
  coef_data = coefs_clean_tract_clustered,
  group_name = "treated",
  outcomes = economic_housing_outcomes,
  event_times = event_times,
  outcome_labels = econ_housing_labels
)
tt(treated_econ_table_tract) %>%
  theme_tt("tabular") %>%
  save_tt(file.path(tables_dir, "tract_clustered_treated_economic_housing.tex"), overwrite = TRUE)
remove_table_wrappers(file.path(tables_dir, "tract_clustered_treated_economic_housing.tex"))

# Spillover - Population/Demographics (Tract-Clustered)
spillover_pop_table_tract <- create_coef_table(
  coef_data = coefs_clean_tract_clustered,
  group_name = "inner",
  outcomes = population_demo_outcomes,
  event_times = event_times,
  outcome_labels = pop_demo_labels
)
tt(spillover_pop_table_tract) %>%
  theme_tt("tabular") %>%
  save_tt(file.path(tables_dir, "tract_clustered_spillover_population_demographics.tex"), overwrite = TRUE)
remove_table_wrappers(file.path(tables_dir, "tract_clustered_spillover_population_demographics.tex"))

# Spillover - Economic/Housing (Tract-Clustered)
spillover_econ_table_tract <- create_coef_table(
  coef_data = coefs_clean_tract_clustered,
  group_name = "inner",
  outcomes = economic_housing_outcomes,
  event_times = event_times,
  outcome_labels = econ_housing_labels
)
tt(spillover_econ_table_tract) %>%
  theme_tt("tabular") %>%
  save_tt(file.path(tables_dir, "tract_clustered_spillover_economic_housing.tex"), overwrite = TRUE)
remove_table_wrappers(file.path(tables_dir, "tract_clustered_spillover_economic_housing.tex"))

message("Baseline tract-clustered tables saved to: ", tables_dir)

# ---- Effect Magnitudes Summary ----
cat("\n=== EFFECT MAGNITUDES SUMMARY ===\n")
cat("Key results at t=+20 for treated neighborhoods:\n\n")

magnitudes_summary <- magnitudes_t20 %>%
  filter(!is.na(baseline_mean)) %>%
  mutate(
    significance = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(outcome_clean, estimate, percent_change, significance)

print(magnitudes_summary)

message("\n=== PART 2 COMPLETE: Main results visualization finished ===")
message("Files saved to: ", results_dir_with_dataset)
