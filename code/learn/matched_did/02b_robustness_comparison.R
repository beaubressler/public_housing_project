## 02b_robustness_comparison.R
## Combines event study estimates from multiple matching specifications
## to show robustness of results across different matching strategies

library(tidyverse)
library(here)
library(ggplot2)
library(patchwork)

message("\n=== ROBUSTNESS COMPARISON: COMBINING MATCHING SPECS ===")

# Configuration -----
data_type <- "combined"
dataset_years <- c("1_year", "2_year")  # Run for both
#specs_to_compare <- c("baseline", "no_cbsa", "cem", "caliper", "genetic")
# for job market talk
specs_to_compare <- c("baseline", "no_cbsa", "caliper")
spec_labels <- c(
  "baseline" = "Baseline (within county)",
  "no_cbsa" = "PS (other CBSA)",
  "cem" = "CEM (other CBSA)",
  "caliper" = "Caliper (within county)",
  "genetic" = "Genetic (within county)"
)

# Directories -----
results_base_dir <- here("output", "regression_results", "matched_did", data_type)

# Load outcome labels (from 01_setup_and_models.R)
outcome_labels <- c(
  "asinh_pop_total" = "Population (asinh)",
  "black_share" = "Black Share",
  "white_share" = "White Share",
  "asinh_pop_black" = "Black Pop. (asinh)",
  "asinh_pop_white" = "White Pop. (asinh)",
  "asinh_median_income" = "Median Income (asinh)",
  "asinh_median_rent_calculated" = "Median Rent (asinh)",
  "asinh_median_home_value_calculated" = "Median Home Value (asinh)",
  "median_home_value" = "Median Home Value",
  "share_college_plus" = "Share College+"
)

# Loop over dataset years -----
for (dataset_year in dataset_years) {

  cat("\n\n=== Processing", dataset_year, "===\n")

  output_dir <- here(results_base_dir, "robustness_comparison", dataset_year)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Read and combine coefficients from all specs -----
all_coefs <- map_dfr(specs_to_compare, function(spec) {
  coef_file <- here(results_base_dir, spec, dataset_year, "event_study_coefficients.csv")

  if (!file.exists(coef_file)) {
    warning("File not found: ", coef_file)
    return(NULL)
  }

  read_csv(coef_file, show_col_types = FALSE) %>%
    mutate(
      spec = spec,
      spec_label = spec_labels[spec]
    )
})

if (nrow(all_coefs) == 0) {
  stop("No coefficient files found. Make sure to run 02_main_results.R for each spec first.")
}

cat("Loaded coefficients from", length(unique(all_coefs$spec)), "specifications\n")
cat("Specifications:", paste(unique(all_coefs$spec), collapse = ", "), "\n")

# Publication theme -----
pub_theme <- function(base_size = 14){
  theme_classic(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8))
    )
}

# Function to create robustness comparison plot -----
make_robustness_plot <- function(df, outcome_var, group_filter = "treated",
                                 title_text = NULL, show_legend = TRUE) {

  df_use <- df %>%
    filter(group == group_filter, outcome_clean == outcome_var) %>%
    mutate(
      event_time = as.numeric(term),
      spec_label = factor(spec_label, levels = spec_labels)
    )

  # Add reference point at t=-10
  ref_rows <- df_use %>%
    distinct(spec, spec_label) %>%
    mutate(event_time = -10, estimate = 0, std.error = 0,
           outcome_clean = outcome_var, group = group_filter)

  df_use <- bind_rows(df_use, ref_rows) %>%
    arrange(spec_label, event_time)

  # Add staggered x positions for each spec to avoid overlap
  # Stagger by +/- 0.5 years for 3 specs for better visibility
  stagger_amounts <- c("baseline" = -0.5, "no_cbsa" = 0, "caliper" = 0.5, "cem" = 0.25, "genetic" = 0.5)
  df_use <- df_use %>%
    mutate(event_time_staggered = event_time + stagger_amounts[spec])

  # Create plot with error bars instead of ribbons, no connecting lines
  p <- ggplot(df_use, aes(x = event_time_staggered, y = estimate,
                          color = spec_label)) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error),
                  width = 0.5, linewidth = 0.6, alpha = 0.8) +
    geom_point(size = 2.5, stroke = 0) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = seq(-40, 40, 10)) +
    labs(
      title = title_text,
      x = "Years Relative to Construction",
      y = "Difference-in-Differences Estimate"
    ) +
    pub_theme(14)

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }

  return(p)
}

# Create comparison plots for key outcomes -----
key_outcomes_treated <- c("black_share", "asinh_pop_total", "asinh_pop_white", "asinh_median_income", "asinh_median_rent_calculated", "asinh_median_home_value_calculated")

cat("\n=== Creating robustness comparison plots for treated neighborhoods ===\n")

# Individual plots
for (outcome in key_outcomes_treated) {
  p <- make_robustness_plot(
    df = all_coefs,
    outcome_var = outcome,
    group_filter = "treated",
    title_text = paste0("Robustness: ", outcome_labels[outcome], " (Treated Neighborhoods)"),
    show_legend = TRUE
  )

  filename <- paste0("robustness_", outcome, "_treated")
  ggsave(file.path(output_dir, paste0(filename, ".pdf")),
         p, width = 8, height = 5.5, device = cairo_pdf)

  cat("Saved:", filename, "\n")
}

# Combined 4-panel plots with shared legend
# Panel A: Demographics/Population
p1_demo <- make_robustness_plot(all_coefs, "black_share", "treated",
                                outcome_labels["black_share"], FALSE)
p2_demo <- make_robustness_plot(all_coefs, "asinh_pop_total", "treated",
                                outcome_labels["asinh_pop_total"], FALSE)
p3_demo <- make_robustness_plot(all_coefs, "asinh_pop_black", "treated",
                                outcome_labels["asinh_pop_black"], FALSE)
p4_demo <- make_robustness_plot(all_coefs, "asinh_pop_white", "treated",
                                outcome_labels["asinh_pop_white"], TRUE)

# Combine demographics panel
p_combined_demo <- (p1_demo | p2_demo) / (p3_demo | p4_demo) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "robustness_four_panel_treated_demographics.pdf"),
       p_combined_demo, width = 11, height = 9, device = cairo_pdf)

cat("\nSaved 4-panel demographics plot (treated)\n")

# Panel B: Income and Rent (2-panel)
p1_income_rent <- make_robustness_plot(all_coefs, "asinh_median_income", "treated",
                                       outcome_labels["asinh_median_income"], FALSE)
p2_income_rent <- make_robustness_plot(all_coefs, "asinh_median_rent_calculated", "treated",
                                       outcome_labels["asinh_median_rent_calculated"], TRUE)

# Combine income/rent panel
p_combined_income_rent <- (p1_income_rent | p2_income_rent) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "robustness_two_panel_treated_income_rent.pdf"),
       p_combined_income_rent, width = 11, height = 5.5, device = cairo_pdf)

cat("Saved 2-panel income/rent plot (treated)\n")

# Panel C: Labor Market (2-panel)
p1_labor <- make_robustness_plot(all_coefs, "unemp_rate", "treated",
                                 "Unemployment Rate", FALSE)
p2_labor <- make_robustness_plot(all_coefs, "lfp_rate", "treated",
                                 "Labor Force Participation Rate", TRUE)

# Combine labor market panel
p_combined_labor <- (p1_labor | p2_labor) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "robustness_two_panel_treated_labor_market.pdf"),
       p_combined_labor, width = 11, height = 5.5, device = cairo_pdf)

cat("Saved 2-panel labor market plot (treated)\n")

# Inner ring spillovers (if available) -----
if ("inner" %in% all_coefs$group) {
  cat("\n=== Creating robustness comparison plots for spillovers ===\n")

  for (outcome in key_outcomes_treated) {
    p <- make_robustness_plot(
      df = all_coefs,
      outcome_var = outcome,
      group_filter = "inner",
      title_text = paste0("Robustness: ", outcome_labels[outcome], " (Spillover Effects)"),
      show_legend = TRUE
    )

    filename <- paste0("robustness_", outcome, "_inner")
    ggsave(file.path(output_dir, paste0(filename, ".pdf")),
           p, width = 8, height = 5.5, device = cairo_pdf)

    cat("Saved:", filename, "\n")
  }

  # Combined 4-panel plots for spillovers
  # Panel A: Demographics/Population
  p1_demo_inner <- make_robustness_plot(all_coefs, "black_share", "inner",
                                        outcome_labels["black_share"], FALSE)
  p2_demo_inner <- make_robustness_plot(all_coefs, "asinh_pop_total", "inner",
                                        outcome_labels["asinh_pop_total"], FALSE)
  p3_demo_inner <- make_robustness_plot(all_coefs, "asinh_pop_black", "inner",
                                        outcome_labels["asinh_pop_black"], FALSE)
  p4_demo_inner <- make_robustness_plot(all_coefs, "asinh_pop_white", "inner",
                                        outcome_labels["asinh_pop_white"], TRUE)

  # Combine demographics panel for spillovers
  p_combined_demo_inner <- (p1_demo_inner | p2_demo_inner) / (p3_demo_inner | p4_demo_inner) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  ggsave(file.path(output_dir, "robustness_four_panel_inner_demographics.pdf"),
         p_combined_demo_inner, width = 11, height = 9, device = cairo_pdf)

  cat("\nSaved 4-panel demographics plot (spillover)\n")

  # Panel B: Income and Rent (2-panel)
  p1_income_rent_inner <- make_robustness_plot(all_coefs, "asinh_median_income", "inner",
                                               outcome_labels["asinh_median_income"], FALSE)
  p2_income_rent_inner <- make_robustness_plot(all_coefs, "asinh_median_rent_calculated", "inner",
                                               outcome_labels["asinh_median_rent_calculated"], TRUE)

  # Combine income/rent panel for spillovers
  p_combined_income_rent_inner <- (p1_income_rent_inner | p2_income_rent_inner) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  ggsave(file.path(output_dir, "robustness_two_panel_inner_income_rent.pdf"),
         p_combined_income_rent_inner, width = 11, height = 5.5, device = cairo_pdf)

  cat("Saved 2-panel income/rent plot (spillover)\n")

  # Panel C: Labor Market (2-panel)
  p1_labor_inner <- make_robustness_plot(all_coefs, "unemp_rate", "inner",
                                         "Unemployment Rate", FALSE)
  p2_labor_inner <- make_robustness_plot(all_coefs, "lfp_rate", "inner",
                                         "Labor Force Participation Rate", TRUE)

  # Combine labor market panel for spillovers
  p_combined_labor_inner <- (p1_labor_inner | p2_labor_inner) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  ggsave(file.path(output_dir, "robustness_two_panel_inner_labor_market.pdf"),
         p_combined_labor_inner, width = 11, height = 5.5, device = cairo_pdf)

  cat("Saved 2-panel labor market plot (spillover)\n")
}

  cat("\n=== ROBUSTNESS COMPARISON COMPLETE FOR", dataset_year, "===\n")
  cat("Output directory:", output_dir, "\n")

} # End loop over dataset_years

cat("\n\n=== ALL ROBUSTNESS COMPARISONS COMPLETE ===\n")
