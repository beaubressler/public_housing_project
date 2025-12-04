## 03b_heterogeneity_robustness.R
## Compare heterogeneity estimates (t=20) across matching specifications

library(tidyverse)
library(here)
library(ggplot2)
library(patchwork)

message("\n=== HETEROGENEITY ROBUSTNESS COMPARISON ===")

# Configuration -----
data_type <- "combined"
dataset_years <- c("1_year", "2_year")
#specs_to_compare <- c("baseline", "no_cbsa", "cem", "caliper", "genetic")
# For talk, will just do three
specs_to_compare <- c("baseline", "no_cbsa", "caliper")
spec_labels <- c(
  "baseline" = "Baseline (within county)",
  "no_cbsa" = "Cross-metro",
  "cem" = "CEM (other CBSA)",
  "caliper" = "Caliper (within county)",
  "genetic" = "Genetic (within county)"
)

# Outcome labels
outcome_labels <- c(
  "black_share" = "Black Share",
  "asinh_median_income" = "Median Income (asinh)",
  "asinh_pop_black" = "Black Pop. (asinh)",
  "asinh_pop_white" = "White Pop. (asinh)",
  "black_pop" = "Black Pop.",
  "white_pop" = "White Pop."
)

# Directories -----
results_base_dir <- here("output", "regression_results", "matched_did", data_type)

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

# Loop over dataset years -----
for (dataset_year in dataset_years) {

  cat("\n\n=== Processing", dataset_year, "===\n")

  output_dir <- here(results_base_dir, "heterogeneity_robustness", dataset_year)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Read and combine heterogeneity estimates from all specs -----
  all_het <- map_dfr(specs_to_compare, function(spec) {
    het_file <- here(results_base_dir, spec, "heterogeneity", dataset_year,
                     "all_heterogeneity_t20_estimates.csv")

    if (!file.exists(het_file)) {
      warning("File not found: ", het_file)
      return(NULL)
    }

    read_csv(het_file, show_col_types = FALSE) %>%
      mutate(
        spec = spec,
        spec_label = spec_labels[spec]
      )
  })

  if (nrow(all_het) == 0) {
    stop("No heterogeneity files found. Make sure to run 03_heterogeneity_analysis.R for each spec first.")
  }

  cat("Loaded heterogeneity estimates from", length(unique(all_het$spec)), "specifications\n")

  # Function to create heterogeneity robustness plot -----
  make_het_robustness_plot <- function(df, outcome_filter,
                                       het_dimension,
                                       title_text = NULL,
                                       show_legend = TRUE) {

    df_use <- df %>%
      filter(outcome_clean == outcome_filter,
             location %in% het_dimension) %>%
      mutate(
        spec_label = factor(spec_label, levels = spec_labels),
        location = factor(location, levels = het_dimension),
        group = factor(group, levels = c("treated", "inner"))
      )

    if (nrow(df_use) == 0) {
      warning("No data for ", outcome_filter, " with dimensions: ", paste(het_dimension, collapse = ", "))
      return(NULL)
    }

    # Add staggered x positions
    n_specs <- length(unique(df_use$spec))
    if (n_specs == 4) {
      stagger_amounts <- c("baseline" = -0.15, "no_cbsa" = -0.05, "cem" = 0.05, "genetic" = 0.15)
    } else {
      stagger_amounts <- setNames(seq(-0.1, 0.1, length.out = n_specs), unique(df_use$spec))
    }

    df_use <- df_use %>%
      mutate(location_num = as.numeric(location),
             location_staggered = location_num + stagger_amounts[spec])

    # Create plot with facets for treated vs spillover
    p <- ggplot(df_use, aes(x = location_staggered, y = estimate,
                            color = spec_label)) +
      geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
      geom_errorbar(
        aes(ymin = estimate - 1.96 * std.error,
            ymax = estimate + 1.96 * std.error),
        width = 0.1,
        linewidth = 0.6,
        alpha = 0.8
      ) +
      geom_point(size = 2.5, stroke = 0) +
      scale_color_brewer(palette = "Dark2") +
      scale_x_continuous(
        breaks = 1:length(het_dimension),
        labels = het_dimension
      ) +
      facet_wrap(~ group, labeller = labeller(group = c("treated" = "Treated Tracts", "inner" = "Spillover Tracts"))) +
      labs(
        title = title_text,
        x = NULL,
        y = "Treatment Effect at t=20"
      ) +
      pub_theme(14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    if (!show_legend) {
      p <- p + theme(legend.position = "none")
    }

    return(p)
  }

  # Project size heterogeneity -----
  cat("\n=== Project Size Heterogeneity ===\n")

  key_outcomes <- c("black_share", "asinh_median_income", "asinh_pop_black", "asinh_pop_white")

  # Detect LARGEST project size labels (dynamic with actual tercile values)
  largest_size_labels <- all_het %>%
    filter(str_detect(location, "^(Small|Medium|Large) \\(<|\\(\\d")) %>%
    pull(location) %>%
    unique() %>%
    sort()

  # Detect TOTAL project size labels (if different pattern exists)
  # For now, try both patterns

  if (length(largest_size_labels) >= 3) {
    cat("\n--- Heterogeneity by LARGEST Project Size ---\n")

    for (outcome in key_outcomes) {
      p_size_largest <- make_het_robustness_plot(
        all_het,
        outcome_filter = outcome,
        het_dimension = largest_size_labels,
        title_text = paste0(outcome_labels[outcome], " by Largest Project Size"),
        show_legend = TRUE
      )

      if (!is.null(p_size_largest)) {
        ggsave(file.path(output_dir, paste0("het_size_largest_", outcome, ".pdf")),
               p_size_largest, width = 10, height = 6, device = cairo_pdf)
        cat("Saved: het_size_largest_", outcome, ".pdf\n")
      }
    }
  }

  # Try to find TOTAL project size labels (might have different format)
  # Check if there are size labels that don't match the pattern above
  total_size_labels <- all_het %>%
    filter(str_detect(location, "Small|Medium|Large"),
           !str_detect(location, "Black Share|Income|NYC|Urban|Early|Late|Nearby|High-rise")) %>%
    pull(location) %>%
    unique()

  # Remove largest labels to get total labels
  total_size_labels <- setdiff(total_size_labels, largest_size_labels)

  if (length(total_size_labels) >= 3) {
    cat("\n--- Heterogeneity by TOTAL Project Size ---\n")

    for (outcome in key_outcomes) {
      p_size_total <- make_het_robustness_plot(
        all_het,
        outcome_filter = outcome,
        het_dimension = total_size_labels,
        title_text = paste0(outcome_labels[outcome], " by Total Project Size"),
        show_legend = TRUE
      )

      if (!is.null(p_size_total)) {
        ggsave(file.path(output_dir, paste0("het_size_total_", outcome, ".pdf")),
               p_size_total, width = 10, height = 6, device = cairo_pdf)
        cat("Saved: het_size_total_", outcome, ".pdf\n")
      }
    }
  }

  # Baseline black share heterogeneity -----
  cat("\n=== Baseline Black Share Heterogeneity ===\n")

  # Use all outcomes for baseline black share
  black_share_outcomes <- c(key_outcomes, "black_pop", "white_pop")

  for (outcome in black_share_outcomes) {
    p_black_baseline <- make_het_robustness_plot(
      all_het,
      outcome_filter = outcome,
      het_dimension = c("Very Low Black Share", "Medium Black Share", "High Black Share"),
      title_text = paste0(outcome_labels[outcome], " by Baseline Black Share"),
      show_legend = TRUE
    )

    if (!is.null(p_black_baseline)) {
      ggsave(file.path(output_dir, paste0("het_baseline_black_", outcome, ".pdf")),
             p_black_baseline, width = 10, height = 6, device = cairo_pdf)
      cat("Saved: het_baseline_black_", outcome, ".pdf\n")
    }
  }

  # Income heterogeneity -----
  cat("\n=== Baseline Income Heterogeneity ===\n")

  for (outcome in key_outcomes) {
    p_income_het <- make_het_robustness_plot(
      all_het,
      outcome_filter = outcome,
      het_dimension = c("Low Income", "High Income"),
      title_text = paste0(outcome_labels[outcome], " by Baseline Income"),
      show_legend = TRUE
    )

    if (!is.null(p_income_het)) {
      ggsave(file.path(output_dir, paste0("het_baseline_income_", outcome, ".pdf")),
             p_income_het, width = 10, height = 6, device = cairo_pdf)
      cat("Saved: het_baseline_income_", outcome, ".pdf\n")
    }
  }

  # NYC heterogeneity (if available) -----
  if (any(all_het$location %in% c("NYC", "Non-NYC"))) {
    cat("\n=== NYC Heterogeneity ===\n")

    for (outcome in key_outcomes) {
      p_nyc <- make_het_robustness_plot(
        all_het,
        outcome_filter = outcome,
        het_dimension = c("NYC", "Non-NYC"),
        title_text = paste0(outcome_labels[outcome], ": NYC vs Non-NYC"),
        show_legend = TRUE
      )

      if (!is.null(p_nyc)) {
        ggsave(file.path(output_dir, paste0("het_nyc_", outcome, ".pdf")),
               p_nyc, width = 10, height = 6, device = cairo_pdf)
        cat("Saved: het_nyc_", outcome, ".pdf\n")
      }
    }
  }

  # Urban Renewal heterogeneity -----
  if (any(all_het$location %in% c("Urban Renewal", "Non-Urban Renewal"))) {
    cat("\n=== Urban Renewal Heterogeneity ===\n")

    for (outcome in key_outcomes) {
      p_ur <- make_het_robustness_plot(
        all_het,
        outcome_filter = outcome,
        het_dimension = c("Urban Renewal", "Non-Urban Renewal"),
        title_text = paste0(outcome_labels[outcome], ": UR vs Non-UR"),
        show_legend = TRUE
      )

      if (!is.null(p_ur)) {
        ggsave(file.path(output_dir, paste0("het_urban_renewal_", outcome, ".pdf")),
               p_ur, width = 10, height = 6, device = cairo_pdf)
        cat("Saved: het_urban_renewal_", outcome, ".pdf\n")
      }
    }
  }

  # Early vs Late projects -----
  if (any(all_het$location %in% c("Early Projects", "Late Projects"))) {
    cat("\n=== Early vs Late Projects ===\n")

    for (outcome in key_outcomes) {
      p_timing <- make_het_robustness_plot(
        all_het,
        outcome_filter = outcome,
        het_dimension = c("Early Projects", "Late Projects"),
        title_text = paste0(outcome_labels[outcome], ": Early vs Late"),
        show_legend = TRUE
      )

      if (!is.null(p_timing)) {
        ggsave(file.path(output_dir, paste0("het_timing_", outcome, ".pdf")),
               p_timing, width = 10, height = 6, device = cairo_pdf)
        cat("Saved: het_timing_", outcome, ".pdf\n")
      }
    }
  }

  # High-rise heterogeneity (if available) -----
  if (any(all_het$location %in% c("High-rise", "Non-high-rise"))) {
    cat("\n=== High-rise Heterogeneity ===\n")

    for (outcome in key_outcomes) {
      p_highrise <- make_het_robustness_plot(
        all_het,
        outcome_filter = outcome,
        het_dimension = c("High-rise", "Non-high-rise"),
        title_text = paste0(outcome_labels[outcome], ": High-rise vs Non-high-rise"),
        show_legend = TRUE
      )

      if (!is.null(p_highrise)) {
        ggsave(file.path(output_dir, paste0("het_highrise_", outcome, ".pdf")),
               p_highrise, width = 10, height = 6, device = cairo_pdf)
        cat("Saved: het_highrise_", outcome, ".pdf\n")
      }
    }
  }

  # Save combined estimates -----
  write_csv(all_het, file.path(output_dir, "all_heterogeneity_robustness.csv"))

  cat("\n=== HETEROGENEITY ROBUSTNESS COMPLETE FOR", dataset_year, "===\n")
  cat("Output directory:", output_dir, "\n")

} # End loop over dataset_years

cat("\n\n=== ALL HETEROGENEITY ROBUSTNESS COMPARISONS COMPLETE ===\n")
