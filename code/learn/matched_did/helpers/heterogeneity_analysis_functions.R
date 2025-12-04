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
  library(ggh4x)  # For facetted_pos_scales to set per-facet y-axis limits
})

# Extract estimates at specified event time(s) from event study results
# event_times can be a single value or vector (e.g., c(10, 30) for t=1 and t=3)
extract_event_time_estimates <- function(results_list, location_label, event_times = c(10, 30)) {
  estimates <- map_dfr(names(results_list), function(name) {
    model <- results_list[[name]]
    coefs <- tidy(model)

    # Extract estimates at each specified event time
    map_dfr(event_times, function(et) {
      et_coef <- coefs %>%
        filter(str_detect(term, paste0("event_time::", et, ":treated"))) %>%
        select(estimate, std.error, p.value)

      if(nrow(et_coef) > 0) {
        tibble(
          outcome_group = name,
          location = location_label,
          event_time = et,
          event_time_label = paste0("t=", et / 10),
          estimate = et_coef$estimate,
          std.error = et_coef$std.error,
          p.value = et_coef$p.value,
          conf.low = estimate - 1.96 * std.error,
          conf.high = estimate + 1.96 * std.error
        )
      } else {
        NULL
      }
    })
  })
  return(estimates)
}

# Function to create heterogeneity comparison plots
create_heterogeneity_plot <- function(...,
                                      outcomes_to_plot,
                                      outcome_labels_map,
                                      title_text,
                                      subtitle_text = NULL,
                                      colors = NULL,
                                      save_name = NULL,
                                      results_dir = NULL,
                                      event_times = c(10, 30)) {

  # Set default subtitle if not provided
  if (is.null(subtitle_text)) {
    time_labels <- paste0("t=", event_times / 10, collapse = " and ")
    subtitle_text <- paste0("Treatment effects at ", time_labels)
  }

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

  # Extract estimates at specified event times for all groups
  all_estimates <- map2_dfr(results_lists, labels, ~extract_event_time_estimates(.x, .y, event_times = event_times))

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
      location = factor(location, levels = labels),
      event_time_label = factor(event_time_label, levels = paste0("t=", event_times / 10))
    )

  # Create plot with publication-quality formatting
  # Facet by event_time (rows within group) and outcome (columns)
  p <- ggplot(comparison_data, aes(x = location, y = estimate, color = location)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray30", linewidth = 0.4) +
    geom_point(size = 3.5, alpha = 0.9, shape = 16) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.15, linewidth = 1, alpha = 0.8) +
    facet_grid(event_time_label + group_label ~ clean_label, scales = "free_y",
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

# Cleaner heterogeneity plot (Option B format)
# Facets by outcome and event time, colors by group (Treated vs Nearby)
# Better for publication - standard applied micro format
create_heterogeneity_plot_clean <- function(...,
                                            outcomes_to_plot,
                                            outcome_labels_map,
                                            title_text = NULL,
                                            subtitle_text = NULL,
                                            x_label = "",
                                            save_name = NULL,
                                            results_dir = NULL,
                                            width = 11,
                                            height = 5.5,
                                            event_times = c(10, 30)) {

  # Set default subtitle if not provided
  if (is.null(subtitle_text)) {
    time_labels <- paste0("t=", event_times / 10, collapse = " and ")
    subtitle_text <- paste0("Treatment effects at ", time_labels)
  }

  # Get arguments as list
  args <- list(...)

  # Check if old named argument format is being used
  if (all(c("results_list1", "results_list2", "label1", "label2") %in% names(args))) {
    # Convert old format to new format
    results_lists <- list(args$results_list1, args$results_list2)
    labels <- c(args$label1, args$label2)

    # Handle 3-group case
    if ("results_list3" %in% names(args) && "label3" %in% names(args)) {
      results_lists <- c(results_lists, list(args$results_list3))
      labels <- c(labels, args$label3)
    }
  } else {
    # New format: alternating results_list, label pairs
    if (length(args) %% 2 != 0) {
      stop("Arguments must be alternating results_list, label pairs")
    }

    results_lists <- args[seq(1, length(args), 2)]
    labels <- unlist(args[seq(2, length(args), 2)])
  }

  # Extract estimates at specified event times for all groups
  all_estimates <- map2_dfr(results_lists, labels, ~extract_event_time_estimates(.x, .y, event_times = event_times))

  # Combine and clean
  comparison_data <- all_estimates %>%
    mutate(
      group = str_extract(outcome_group, "(treated|inner)$"),
      outcome_clean = str_remove(outcome_group, "_(treated|inner)$")
    ) %>%
    filter(outcome_clean %in% outcomes_to_plot) %>%
    mutate(
      clean_label = case_when(
        outcome_clean %in% names(outcome_labels_map) ~ outcome_labels_map[outcome_clean],
        TRUE ~ outcome_clean
      ),
      group_label = case_when(
        group == "treated" ~ "Treated",
        group == "inner" ~ "Nearby",
        TRUE ~ group
      ),
      clean_label = factor(clean_label, levels = outcome_labels_map[outcomes_to_plot]),
      group_label = factor(group_label, levels = c("Treated", "Nearby")),
      location = factor(location, levels = labels),
      event_time_label = factor(event_time_label, levels = paste0("t=", event_times / 10))
    )

  # Professional colors: dark blue for treated, orange for nearby
  group_colors <- c("Treated" = "#0072B2", "Nearby" = "#E69F00")

  # Create plot - facet by outcome (columns) and event time (rows)
  p <- ggplot(comparison_data, aes(x = location, y = estimate,
                                   color = group_label, group = group_label)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.5) +
    geom_point(size = 4, alpha = 0.9, position = position_dodge(width = 0.4)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.25, linewidth = 0.9, alpha = 0.8,
                  position = position_dodge(width = 0.4)) +
    facet_grid(event_time_label ~ clean_label, scales = "free_y") +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = x_label,
      y = "Treatment Effect",
      color = ""
    ) +
    scale_color_manual(values = group_colors) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0,
                               margin = margin(b = 8)),
      plot.subtitle = element_text(size = 12, color = "gray40",
                                  margin = margin(b = 12)),
      plot.title.position = "plot",

      axis.title.x = element_text(size = 13, margin = margin(t = 10)),
      axis.title.y = element_text(size = 13, margin = margin(r = 10)),
      axis.text.x = element_text(size = 11, color = "black"),
      axis.text.y = element_text(size = 11, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.4),
      axis.ticks = element_line(color = "black", linewidth = 0.4),

      strip.background = element_rect(fill = "gray92", color = "gray70", linewidth = 0.4),
      strip.text = element_text(face = "bold", size = 12, color = "black",
                               margin = margin(4, 4, 4, 4)),

      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.6),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.spacing.x = unit(18, "pt"),

      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.key.size = unit(1.2, "lines"),
      legend.margin = margin(t = 10),

      plot.margin = margin(12, 15, 10, 12)
    )

  # Save if requested - save separate files for each event time
  if (!is.null(save_name) & !is.null(results_dir)) {
    # Save combined version with both event times
    ggsave(file.path(results_dir, paste0(save_name, ".pdf")),
           p, width = width, height = height * length(event_times) / 2, device = cairo_pdf)

    # Save slide version without title/subtitle
    slides_dir <- file.path(results_dir, "slides")
    dir.create(slides_dir, recursive = TRUE, showWarnings = FALSE)
    p_slides <- p + labs(title = NULL, subtitle = NULL)
    ggsave(file.path(slides_dir, paste0(save_name, ".pdf")),
           p_slides, width = width, height = height * length(event_times) / 2, device = cairo_pdf)

    # Also save separate files for each event time with matched y-axes
    # First, calculate y-axis limits for each outcome across both event times
    y_limits_by_outcome <- comparison_data %>%
      group_by(clean_label) %>%
      summarize(
        y_min = min(conf.low, na.rm = TRUE),
        y_max = max(conf.high, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Add some padding
      mutate(
        y_range = y_max - y_min,
        y_min = y_min - 0.05 * y_range,
        y_max = y_max + 0.05 * y_range
      )

    for (et in event_times) {
      et_label <- paste0("t", et / 10)
      et_data <- comparison_data %>% filter(event_time == et)

      p_single <- ggplot(et_data, aes(x = location, y = estimate,
                                       color = group_label, group = group_label)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.5) +
        geom_point(size = 4, alpha = 0.9, position = position_dodge(width = 0.4)) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                      width = 0.25, linewidth = 0.9, alpha = 0.8,
                      position = position_dodge(width = 0.4)) +
        facet_wrap(~ clean_label, scales = "free_y", nrow = 1) +
        labs(
          title = title_text,
          subtitle = paste0("Treatment effects at ", et_label),
          x = x_label,
          y = paste0("Treatment Effect at ", et_label),
          color = ""
        ) +
        scale_color_manual(values = group_colors) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
        # Apply consistent y-axis limits across event times using ggh4x or manual limits
        ggh4x::facetted_pos_scales(
          y = lapply(unique(et_data$clean_label), function(outcome) {
            limits <- y_limits_by_outcome %>% filter(clean_label == outcome)
            scale_y_continuous(limits = c(limits$y_min, limits$y_max),
                             labels = scales::number_format(accuracy = 0.01))
          })
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 8)),
          plot.subtitle = element_text(size = 12, color = "gray40", margin = margin(b = 12)),
          plot.title.position = "plot",
          axis.title.x = element_text(size = 13, margin = margin(t = 10)),
          axis.title.y = element_text(size = 13, margin = margin(r = 10)),
          axis.text.x = element_text(size = 11, color = "black"),
          axis.text.y = element_text(size = 11, color = "black"),
          axis.line = element_line(color = "black", linewidth = 0.4),
          axis.ticks = element_line(color = "black", linewidth = 0.4),
          strip.background = element_rect(fill = "gray92", color = "gray70", linewidth = 0.4),
          strip.text = element_text(face = "bold", size = 12, color = "black", margin = margin(4, 4, 4, 4)),
          panel.background = element_rect(fill = "white", color = NA),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.6),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.spacing.x = unit(18, "pt"),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.2, "lines"),
          legend.margin = margin(t = 10),
          plot.margin = margin(12, 15, 10, 12)
        )

      ggsave(file.path(results_dir, paste0(save_name, "_", et_label, ".pdf")),
             p_single, width = width, height = height, device = cairo_pdf)

      # Slides version
      p_single_slides <- p_single + labs(title = NULL, subtitle = NULL)
      ggsave(file.path(slides_dir, paste0(save_name, "_", et_label, ".pdf")),
             p_single_slides, width = width, height = height, device = cairo_pdf)
    }
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

  # Check if old named argument format is being used
  if (all(c("results_list1", "results_list2", "label1", "label2") %in% names(args))) {
    # Convert old format to new format
    results_lists <- list(args$results_list1, args$results_list2)
    labels <- c(args$label1, args$label2)

    # Handle 3-group case
    if ("results_list3" %in% names(args) && "label3" %in% names(args)) {
      results_lists <- c(results_lists, list(args$results_list3))
      labels <- c(labels, args$label3)
    }
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
      x = "Decades Relative to Treatment",
      y = "Difference-in-Differences Estimate",
      color = ""
    ) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks / 10) +
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

# Create heterogeneity coefficient table showing all event times
create_heterogeneity_coefficient_table <- function(...,
                                                    outcome_var,
                                                    treatment_group = "treated",
                                                    save_name = NULL,
                                                    results_dir = NULL) {

  # Get arguments as list
  args <- list(...)

  # Check if old named argument format is being used
  if (all(c("results_list1", "results_list2", "label1", "label2") %in% names(args))) {
    # Convert old format to new format
    results_lists <- list(args$results_list1, args$results_list2)
    labels <- c(args$label1, args$label2)

    # Handle 3-group case
    if ("results_list3" %in% names(args) && "label3" %in% names(args)) {
      results_lists <- c(results_lists, list(args$results_list3))
      labels <- c(labels, args$label3)
    }
  } else {
    # New format: alternating results_list, label pairs
    if (length(args) %% 2 != 0) {
      stop("Arguments must be alternating results_list, label pairs")
    }

    results_lists <- args[seq(1, length(args), 2)]
    labels <- unlist(args[seq(2, length(args), 2)])
  }

  # Extract all event time coefficients for each group
  all_coefs <- map2_dfr(results_lists, labels, ~extract_all_event_time_coefs(.x, .y))

  # Filter to specified outcome and treatment group
  outcome_key <- paste0(outcome_var, "_", treatment_group)

  table_data <- all_coefs %>%
    filter(outcome_group == outcome_key) %>%
    select(heterogeneity_group, event_time, estimate, std.error, p.value)

  # Check if there's any data for this outcome
  if (nrow(table_data) == 0) {
    cat("No data found for outcome:", outcome_var, "| group:", treatment_group, "- skipping table\n")
    return(NULL)
  }

  # Create table with event times as rows, groups as columns
  # Format: estimate with significance stars
  table_formatted <- table_data %>%
    mutate(
      # Add significance stars
      stars = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.10 ~ "*",
        TRUE ~ ""
      ),
      # Format as "estimate (se)stars"
      coef_formatted = sprintf("%.3f%s", estimate, stars),
      se_formatted = sprintf("(%.3f)", std.error)
    ) %>%
    select(heterogeneity_group, event_time, coef_formatted, se_formatted)

  # Pivot to wide format - coefficients
  coefs_wide <- table_formatted %>%
    select(heterogeneity_group, event_time, coef_formatted) %>%
    pivot_wider(names_from = heterogeneity_group, values_from = coef_formatted)

  # Pivot to wide format - standard errors
  ses_wide <- table_formatted %>%
    select(heterogeneity_group, event_time, se_formatted) %>%
    pivot_wider(names_from = heterogeneity_group, values_from = se_formatted)

  # Interleave coefficients and standard errors
  event_times <- sort(unique(table_formatted$event_time))

  final_table <- map_dfr(event_times, function(et) {
    coef_row <- coefs_wide %>% filter(event_time == et)
    se_row <- ses_wide %>%
      filter(event_time == et) %>%
      select(-event_time) %>%
      mutate(event_time = NA_real_, .before = 1)

    bind_rows(coef_row, se_row)
  })

  # Replace NA in event_time for SE rows with empty string
  final_table <- final_table %>%
    mutate(event_time = ifelse(is.na(event_time), "", as.character(event_time)))

  # Rename event_time column
  final_table <- final_table %>%
    rename("Event Time" = event_time)

  # Create tinytable
  tt_table <- tt(final_table) %>%
    format_tt(escape = FALSE) %>%
    theme_tt(theme = "tabular")

  # Save if requested
  if (!is.null(save_name) & !is.null(results_dir)) {
    # Create tables subdirectory
    tables_dir <- file.path(results_dir, "tables")
    dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

    # Save table
    save_path <- file.path(tables_dir, paste0(save_name, "_", outcome_var, "_", treatment_group, ".tex"))
    save_tt(tt_table, save_path, overwrite = TRUE)

    # Remove table wrappers
    source(here::here("code", "helpers", "table_utilities.R"))
    remove_table_wrappers(save_path)

    cat("Saved heterogeneity table:", save_path, "\n")
  }

  return(tt_table)
}