# File: event_study_plots.R
# -------------------------------------------------
# Functions for visualizing matched DiD event study results
# 1. Basic (create_event_study_plot)
# 2. For heterogeneity analysis 
# -------------------------------------------------

create_event_study_plot <- function(reg_results_df, dep_var, title,
                                    category_filter = NULL,
                                    heterogeneity = NULL) {
  
  # Get cleaned name (fallback to original if not found)
  clean_label <- outcome_labels[[dep_var]]
  if (is.null(clean_label)) clean_label <- dep_var  # Fallback to original name
  
  # Replace the variable name in the title
  title <- str_replace(title, dep_var, clean_label)
  
  # Extract coefficients for the specified dependent variable
  coefficients <- reg_results_df %>%
    filter(str_starts(outcome, dep_var)) %>%
    mutate(category = str_remove(outcome, paste0(dep_var, "_")),
           outcome = dep_var,
           event_time = as.numeric(str_remove(term, "year::"))) %>%
    select(-term)
  
    # If heterogeneity is provided, it's a heterogeneity analysis
    if (!is.null(heterogeneity)) {

      # Dynamically create rows for all categories at event_time = -10
      categories <- coefficients %>% distinct(category) %>% pull(category)
      additional_rows <- tibble(
        event_time = -10,
        estimate = 0,
        std.error = 0,
        category = categories
      )

      coefficients <- coefficients %>%
        bind_rows(additional_rows)

      dodge_width <- 1.5  # Wider dodge for better separation

    } else {

      # For standard event study, add reference points for treated and inner groups
      coefficients <- coefficients %>%
        bind_rows(
          tibble(event_time = -10, estimate = 0, std.error = 0, category = "treated"),
          tibble(event_time = -10, estimate = 0, std.error = 0, category = "inner")
        )

      dodge_width <- 1  # Default width for baseline event study
    }
  
  
  # Apply category filter if provided
  if (!is.null(category_filter)) {
    coefficients <- coefficients %>% filter(category == category_filter)
  }
  
  # Create the plot
  plot <- ggplot(coefficients, aes(x = event_time, y = estimate, color = category)) +
    geom_errorbar(aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 * std.error),
                  width = 0.3, size = 1.1, alpha = 0.7, position = position_dodge(width = 1)) +
    geom_point(size = 2, position = position_dodge(width = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title, x = "Decades Relative to Treatment", y = "Difference-in-Difference Estimate") +
    scale_x_continuous(breaks = seq(-40, 40, 10), labels = seq(-4, 4, 1)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "white"),
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    scale_color_brewer(type = "qual", palette = "Dark2")
  
  return(plot)
}
