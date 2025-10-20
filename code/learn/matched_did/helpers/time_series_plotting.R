library(ggplot2)
library(dplyr)
library(stringr)
library(readr)
library(here)
library(scales)

# Load color palettes
source(here("code", "helpers", "color_palettes.R"))

# Function to create time series graphs for any variable
create_time_series_plots <- function(data, variable_name, variable_label = NULL,
                                   use_percent = FALSE, save_plots = TRUE,
                                   figures_dir = NULL, output_data_dir = NULL) {

  # Set default label if not provided
  if (is.null(variable_label)) {
    variable_label <- str_to_title(str_replace_all(variable_name, "_", " "))
  }

  # Prepare data for time series analysis
  time_series_data <- data %>%
    filter(location_type %in% c("treated", "inner", "donor_pool")) %>%
    group_by(year, location_type, group_type) %>%
    summarise(
      mean_value = mean(.data[[variable_name]], na.rm = TRUE),
      median_value = median(.data[[variable_name]], na.rm = TRUE),
      n_tracts = n(),
      .groups = "drop"
    ) %>%
    # Create cleaner labels
    mutate(location_label = case_when(
      location_type == "treated" ~ "Treated Tracts",
      location_type == "inner" ~ "Nearby neighborhoods",
      location_type == "donor_pool" ~ "Matched Controls"
    ))

  # Set y-axis formatting
  if (use_percent) {
    y_scale <- scale_y_continuous(labels = scales::percent_format())
  } else {
    y_scale <- scale_y_continuous(labels = scales::comma_format())
  }

  # Create faceted plot: Treated vs Controls and Nearby vs Controls
  facet_data <- time_series_data %>%
    mutate(
      comparison = case_when(
        group_type == "treated" ~ "Treated vs Controls",
        group_type == "inner" ~ "Nearby vs Controls"
      )
    ) %>%
    filter(!is.na(comparison))

  p_faceted <- facet_data %>%
    ggplot(aes(x = year, y = mean_value, color = location_label, linetype = location_label)) +
    geom_line(linewidth = 0.8, alpha = 0.9) +
    geom_point(size = 1.5, alpha = 0.9) +
    scale_color_manual(
      values = time_series_colors,
      name = ""
    ) +
    scale_linetype_manual(
      values = c("Treated Tracts" = "solid", "Nearby neighborhoods" = "solid", "Matched Controls" = "longdash"),
      name = ""
    ) +
    y_scale +
    scale_x_continuous(
      breaks = seq(1930, 1990, 10),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    facet_wrap(~ comparison, ncol = 2, scales = "free_y") +
    labs(
      x = "Year",
      y = variable_label,
      caption = "Note: Based on matched difference-in-differences sample with propensity score matching."
    ) +
    theme_minimal() +
    theme(
      # Text elements
      text = element_text(family = "serif", color = "black"),
      plot.caption = element_text(size = 9, color = "gray40", hjust = 0, margin = margin(t = 10)),
      axis.title = element_text(size = 11, color = "black"),
      axis.text = element_text(size = 10, color = "gray20"),

      # Legend
      legend.position = "bottom",
      legend.margin = margin(t = 15),
      legend.text = element_text(size = 10),
      legend.key.width = unit(1.5, "cm"),

      # Facet strips
      strip.text = element_text(size = 11, face = "bold", color = "black", margin = margin(b = 8)),
      strip.background = element_rect(fill = "gray95", color = NA),

      # Panel and grid
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),

      # Margins and spacing
      plot.margin = margin(20, 25, 15, 20),
      panel.spacing = unit(0.8, "cm")
    )

  # Save plots if requested
  if (save_plots) {
    # Check that figures directory is provided
    if (is.null(figures_dir)) {
      stop("figures_dir must be provided when save_plots = TRUE")
    }

    clean_var_name <- str_replace_all(variable_name, "[^a-zA-Z0-9_]", "_")
    ggsave(here(figures_dir, paste0("time_series_faceted_", clean_var_name, ".pdf")),
           p_faceted, width = 12, height = 6)

    cat("\nTime series faceted graph for", variable_name, "saved to:", here(figures_dir), "\n")

    # Save time series data only if output_data_dir is provided
    if (!is.null(output_data_dir)) {
      write_csv(time_series_data,
                here(output_data_dir, paste0("time_series_", clean_var_name, "_by_group.csv")))
      cat("Time series data saved to:", here(output_data_dir, paste0("time_series_", clean_var_name, "_by_group.csv")), "\n")
    }
  }

  # Print summary statistics
  cat("\nTime series summary for", variable_label, ":\n")
  print(time_series_data %>% arrange(year, location_type) %>% select(year, location_label, mean_value, n_tracts))

  # Return plot and data
  return(list(
    faceted_plot = p_faceted,
    data = time_series_data
  ))
}

# Wrapper function to create multiple time series plots
create_multiple_time_series <- function(data, variables_to_plot, figures_dir, output_data_dir = NULL) {

  cat("\n=== CREATING TIME SERIES GRAPHS ===\n")

  # Create directories
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(output_data_dir)) {
    dir.create(output_data_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Generate plots for each variable
  for (var_info in variables_to_plot) {
    if (var_info$var %in% names(data)) {
      cat("\nCreating plots for:", var_info$label, "\n")
      plots <- create_time_series_plots(
        data = data,
        variable_name = var_info$var,
        variable_label = var_info$label,
        use_percent = var_info$percent,
        save_plots = TRUE,
        figures_dir = figures_dir,
        output_data_dir = output_data_dir
      )
    } else {
      cat("\nVariable", var_info$var, "not found in data\n")
    }
  }

  cat("\n=== TIME SERIES ANALYSIS COMPLETE ===\n")

  return(TRUE)
}