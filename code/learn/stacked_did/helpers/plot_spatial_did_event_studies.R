# Spatial DiD Event Study Plotting Functions
# Creates event study plots for spatial difference-in-differences analysis

library(ggplot2)
library(dplyr)
library(tidyr)
library(here)

# Load color palettes
source(here("code", "helpers", "color_palettes.R"))

# Basic event study plot function
create_event_study_plot <- function(reg_results_df, data, dep_var, title) {

  coefficients <- reg_results_df$coefficients

  # Add a row for each location type with event_time = -10, estimate = 0, std.error = 0
  coefficients <-
    bind_rows(coefficients,
              tibble(location_type = c("treated", "inner"), event_time = -10, estimate = 0, std.error = 0))


  # Number of observations
  n_obs <- reg_results_df$summary %>%
    pull(nobs)

  # Create the plot
  ggplot(coefficients, aes(x = event_time, y = estimate, color = location_type)) +
    geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                  width = 0.2, alpha = 0.4, position = position_dodge(width = 1)) +
    geom_point(position = position_dodge(width = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title,
         subtitle = "",
         x = "Decades Relative to Treatment",
         y = "Difference in Difference Estimate") +
    scale_x_continuous(breaks = seq(-40, 60, 10), labels = seq(-4, 6, 1)) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.background = element_rect(fill = "white"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    guides(color = guide_legend(title = "Location Type")) +
    scale_color_brewer(palette = "Set1", labels = c("Inner Ring", "Treated"))
}

# Overlay plot function (like matched_did style)
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
    geom_point(size = 3.5, stroke = 0,
               position = position_dodge(width = 2.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey50") +
    labs(
      title = title_text,
      x = "Decades Relative to Treatment",
      y = "Difference-in-Differences Estimate",
      color = ""
    ) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks / 10) +
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
