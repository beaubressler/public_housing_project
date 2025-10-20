## heterogeneity_analysis_functions.R
## Helper functions for heterogeneity analysis
## Contains plotting and data extraction functions for treatment effect heterogeneity

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(broom)
  library(ggplot2)
  library(scales)
  library(RColorBrewer)
})

# Extract t=20 estimates from event study results
extract_t20_estimates <- function(results_list, location_label) {
  estimates <- map_dfr(names(results_list), function(name) {
    model <- results_list[[name]]
    coefs <- tidy(model)

    # Extract t=20 estimate
    t20_coef <- coefs %>%
      filter(str_detect(term, "event_time::20:treated")) %>%
      select(estimate, std.error, p.value)

    if(nrow(t20_coef) > 0) {
      tibble(
        outcome_group = name,
        location = location_label,
        estimate = t20_coef$estimate,
        std.error = t20_coef$std.error,
        p.value = t20_coef$p.value,
        conf.low = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error
      )
    } else {
      NULL
    }
  })
  return(estimates)
}

# Function to create heterogeneity comparison plots
create_heterogeneity_plot <- function(...,
                                      outcomes_to_plot,
                                      outcome_labels_map,
                                      title_text,
                                      subtitle_text = "Treatment effects at t=20",
                                      colors = NULL,
                                      save_name = NULL,
                                      results_dir = NULL) {

  # Get arguments as list
  args <- list(...)

  # Check if old named argument format is being used
  if (all(c("results_list1", "results_list2", "label1", "label2") %in% names(args))) {
    # Convert old format to new format
    results_lists <- list(args$results_list1, args$results_list2)
    labels <- c(args$label1, args$label2)
  } else {
    # New format: alternating results_list, label pairs
    if (length(args) %% 2 != 0) {
      stop("Arguments must be alternating results_list, label pairs")
    }

    results_lists <- args[seq(1, length(args), 2)]
    labels <- unlist(args[seq(2, length(args), 2)])
  }

  # Set default colors if not provided
  if (is.null(colors)) {
    colors <- RColorBrewer::brewer.pal(max(3, length(labels)), "Set1")[1:length(labels)]
  }

  # Extract t=20 estimates for all groups
  all_estimates <- map2_dfr(results_lists, labels, ~extract_t20_estimates(.x, .y))

  # Combine and clean
  comparison_data <- all_estimates %>%
    # Parse outcome and group
    mutate(
      group = str_extract(outcome_group, "(treated|inner)$"),
      outcome_clean = str_remove(outcome_group, "_(treated|inner)$")
    ) %>%
    # Filter to specified outcomes
    filter(outcome_clean %in% outcomes_to_plot) %>%
    mutate(
      # Apply outcome labels
      clean_label = case_when(
        outcome_clean %in% names(outcome_labels_map) ~ outcome_labels_map[outcome_clean],
        TRUE ~ outcome_clean
      ),
      # Clean group labels
      group_label = case_when(
        group == "treated" ~ "Treated Tracts",
        group == "inner" ~ "Nearby Tracts",
        TRUE ~ group
      ),
      # Order factors
      clean_label = factor(clean_label, levels = outcome_labels_map[outcomes_to_plot]),
      group_label = factor(group_label, levels = c("Treated Tracts", "Nearby Tracts")),
      location = factor(location, levels = labels)
    )

  # Create plot with publication-quality formatting
  p <- ggplot(comparison_data, aes(x = location, y = estimate, color = location)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray30", linewidth = 0.4) +
    geom_point(size = 3.5, alpha = 0.9, shape = 16) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.15, linewidth = 1, alpha = 0.8) +
    facet_grid(group_label ~ clean_label, scales = "free_y",
               labeller = labeller(clean_label = label_wrap_gen(width = 12))) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "",
      y = "Treatment Effect",
      color = ""
    ) +
    scale_color_manual(values = setNames(colors, labels)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = ".",
                                                      big.mark = ",")) +
    theme_minimal(base_size = 14, base_family = "sans") +
    theme(
      # Plot titles and text
      plot.title = element_text(size = 18, face = "bold", color = "black",
                               margin = margin(b = 10)),
      plot.subtitle = element_text(size = 14, color = "gray40",
                                  margin = margin(b = 15)),
      plot.title.position = "plot",

      # Axes
      axis.title.y = element_text(size = 14, color = "black",
                                 margin = margin(r = 10)),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.3),
      axis.ticks = element_line(color = "black", linewidth = 0.3),
      axis.ticks.length = unit(2, "pt"),

      # Facets/strips
      strip.background = element_rect(fill = "gray95", color = "gray80", linewidth = 0.3),
      strip.text = element_text(face = "bold", size = 13, color = "black",
                               margin = margin(6, 6, 6, 6)),

      # Panel
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor.y = element_line(color = "gray95", linewidth = 0.2),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.spacing = unit(12, "pt"),

      # Legend
      legend.position = "none",

      # Plot margins
      plot.margin = margin(15, 15, 10, 10)
    )

  # Save if requested
  if (!is.null(save_name) & !is.null(results_dir)) {
    # Save main version with title
    ggsave(file.path(results_dir, paste0(save_name, ".pdf")),
           p, width = 10, height = 7, device = cairo_pdf,
           units = "in", dpi = 300)

    # Save slide version without title/subtitle
    slides_dir <- file.path(results_dir, "slides")
    dir.create(slides_dir, recursive = TRUE, showWarnings = FALSE)
    p_slides <- p + labs(title = NULL, subtitle = NULL)
    ggsave(file.path(slides_dir, paste0(save_name, ".pdf")),
           p_slides, width = 10, height = 7, device = cairo_pdf,
           units = "in", dpi = 300)
  }

  return(p)
}

# Function to extract all coefficients (not just t=20) from event study results
extract_all_event_time_coefs <- function(results_list, group_label) {
  estimates <- map_dfr(names(results_list), function(name) {
    model <- results_list[[name]]
    coefs <- tidy(model)

    # Extract all event time coefficients
    event_time_coefs <- coefs %>%
      filter(str_detect(term, "event_time::[-]?[0-9]+:treated")) %>%
      mutate(
        event_time = as.numeric(str_extract(term, "(?<=event_time::)[-]?[0-9]+")),
        outcome_group = name,
        heterogeneity_group = group_label
      ) %>%
      select(outcome_group, heterogeneity_group, event_time, estimate, std.error, p.value)

    event_time_coefs
  })

  return(estimates)
}

# Function to create full event study plots comparing heterogeneity groups
create_heterogeneity_event_study_plot <- function(...,
                                                   outcome_var,
                                                   outcome_label,
                                                   treatment_group = "treated",
                                                   colors = NULL,
                                                   save_name = NULL,
                                                   results_dir = NULL,
                                                   x_breaks = seq(-30, 30, 10)) {

  # Get arguments as list
  args <- list(...)

  # Parse alternating results_list, label pairs
  if (length(args) %% 2 != 0) {
    stop("Arguments must be alternating results_list, label pairs")
  }

  results_lists <- args[seq(1, length(args), 2)]
  labels <- unlist(args[seq(2, length(args), 2)])

  # Set default colors if not provided
  if (is.null(colors)) {
    colors <- RColorBrewer::brewer.pal(max(3, length(labels)), "Set1")[1:length(labels)]
  }

  # Extract all event time coefficients for each group
  all_coefs <- map2_dfr(results_lists, labels, ~extract_all_event_time_coefs(.x, .y))

  # Filter to specified outcome and treatment group
  outcome_key <- paste0(outcome_var, "_", treatment_group)

  plot_data <- all_coefs %>%
    filter(outcome_group == outcome_key) %>%
    mutate(
      heterogeneity_group = factor(heterogeneity_group, levels = labels),
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    )

  # Add reference points at event_time = -10
  ref_rows <- plot_data %>%
    distinct(heterogeneity_group) %>%
    mutate(event_time = -10, estimate = 0, std.error = 0, conf.low = 0, conf.high = 0)

  plot_data <- bind_rows(plot_data, ref_rows) %>%
    arrange(heterogeneity_group, event_time)

  # Create plot
  group_label_title <- ifelse(treatment_group == "treated", "Treated Neighborhoods", "Spillover Neighborhoods")

  p <- ggplot(plot_data, aes(x = event_time, y = estimate, color = heterogeneity_group)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.4, linewidth = 0.8, alpha = 0.7,
                  position = position_dodge(width = 2.5)) +
    geom_line(aes(group = heterogeneity_group), linewidth = 0.9,
              position = position_dodge(width = 2.5)) +
    geom_point(size = 2.2, stroke = 0,
               position = position_dodge(width = 2.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey50") +
    labs(
      title = paste0(outcome_label, ", ", group_label_title),
      x = "Years Relative to Construction",
      y = "Difference-in-Differences Estimate",
      color = ""
    ) +
    scale_x_continuous(breaks = x_breaks) +
    scale_color_manual(values = setNames(colors, labels)) +
    theme_classic(base_size = 14) +
    theme(
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.margin = margin(t = 10),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8))
    )

  # Save if requested
  if (!is.null(save_name) && !is.null(results_dir)) {
    full_event_dir <- file.path(results_dir, "full_event_studies")
    dir.create(full_event_dir, recursive = TRUE, showWarnings = FALSE)

    # Save main version with title
    ggsave(file.path(full_event_dir, paste0(save_name, ".pdf")),
           p, width = 8.5, height = 7, device = cairo_pdf)

    # Save slide version without title
    slides_dir <- file.path(full_event_dir, "slides")
    dir.create(slides_dir, recursive = TRUE, showWarnings = FALSE)
    p_slides <- p + labs(title = NULL)
    ggsave(file.path(slides_dir, paste0(save_name, ".pdf")),
           p_slides, width = 8.5, height = 7, device = cairo_pdf)
  }

  return(p)
}