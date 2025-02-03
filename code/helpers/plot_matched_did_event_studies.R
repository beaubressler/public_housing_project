# File: event_study_plots.R
# -------------------------------------------------
# Functions for visualizing matched DiD event study results
# 1. Basic (create_event_study_plot)
# 2. For heterogeneity analysis 
# -------------------------------------------------



create_event_study_plot <- function(reg_results_df, dep_var, title,
                                    # category filter: "treated" or "inner"
                                    category_filter = NULL,
                                    # grouping variable: for heterogeneity analysis
                                    heterogeneity = NULL) {
  
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
  
  # Apply category filter if provided (useful for standard event studies)
  if (!is.null(category_filter)) {
    coefficients <- coefficients %>% filter(category == category_filter)
  }
  
  # Create the plot
  plot <- ggplot(coefficients, aes(x = event_time, y = estimate, color = category)) +
    geom_errorbar(aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 * std.error),
                  width = 0.3, size = 1.1, alpha = 0.7, position = position_dodge(width = dodge_width)) +
    geom_point(size = 2, position = position_dodge(width = dodge_width)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title, x = "Years Relative to Treatment", y = "Difference-in-Difference Estimate") +
    scale_x_continuous(breaks = seq(-40, 40, 10)) +
    theme_minimal() +
    theme(legend.position = "bottom", 
          plot.background = element_rect(fill = "white"),
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  # Adjust color scale depending on whether heterogeneity analysis is used
  if (!is.null(heterogeneity)) {
    plot <- plot + scale_color_brewer(type = "seq", palette = "Reds")  # Use sequential colors for groups
  } else {
    plot <- plot + scale_color_brewer(type = "qual", palette = "Dark2")  # Use qualitative colors for standard plots
  }
  
  return(plot)
}




# create_event_study_plot <- function(reg_results_df, dep_var, category_filter = NULL, title) {
#   # for testing
#   reg_results_df = event_study_sunab_coefs
#   dep_var = "black_share"
#   title = paste0("Matched DiD: Effect of public housing on ", outcome)
#   category_filter = "treated"
# 
#   
#   # Extract coefficients for the specified dependent variable
#   coefficients <- reg_results_df %>% 
#     filter(str_starts(outcome, dep_var)) %>% 
#     # create a column with everyhting after dep_var_
#     mutate(category = str_remove(outcome, paste0(dep_var, "_"))) %>% 
#     mutate(outcome = dep_var) %>% 
#     # remove year:: from term
#     mutate(event_time = as.numeric(str_remove(term, "year::"))) %>% 
#     # remove term
#     dplyr::select(-term)
#   
#   # Add a reference point at event_time = -10 for treated and inner groups
#   coefficients <- coefficients %>% 
#     # add row for 0s for treated and inner
#     bind_rows(tibble(event_time = -10, estimate = 0, std.error = 0, category = "treated"),
#               tibble(event_time = -10, estimate = 0, std.error = 0, category = "inner"))
#   
#   
#   # Apply optional category filter (category = "treated" or "inner")
#   if (!is.null(category_filter)) {
#     coefficients <- coefficients %>% 
#       filter(category == category_filter)
#   }
#   
#   
#   # create the plot
#   plot <- 
#     coefficients %>% 
#     ggplot(aes(x = event_time, y = estimate, color = category))  +
#     geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
#                   width = 0.3,
#                   size = 1.1,
#                   alpha = 0.7,
#                   position = position_dodge(width = 1)) +
#     geom_point(size = 2,
#                position = position_dodge(width = 1)) +
#     geom_hline(yintercept = 0, linetype = "dashed") +
#     labs(title = title,
#          subtitle = "",
#          x = "Years Relative to Treatment",
#          y = "Difference in Difference Estimate") +
#     scale_x_continuous(breaks = seq(-40, 40, 10)) +
#     scale_color_brewer(type = "qual", palette = "Dark2") +
#     theme_minimal() +
#     theme(legend.position = "bottom", plot.background = element_rect(fill = "white"),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_blank()) 
#   
#   plot
# }


# Function for plotting event study results by group, for heterogeneity analysis
create_event_study_plot_by_group <- function(reg_results_df, dep_var, title) {
  
  # for testing
  # reg_results_df =  event_study_sunab_size_coefs
  # data = tract_data_matched_with_size_treated
  # dep_var = "asinh_pop_black"
  # title = paste0("Matched DiD: Effect of public housing on ", outcome)
  
  coefficients <- reg_results_df %>% 
    filter(str_starts(outcome, dep_var)) %>% 
    # create a column with everything after dep_var_
    mutate(category = str_remove(outcome, paste0(dep_var, "_"))) %>% 
    mutate(outcome = dep_var) %>% 
    # remove year:: from term
    mutate(event_time = as.numeric(str_remove(term, "year::"))) %>% 
    # remove term
    dplyr::select(-term)
  
  # dynamically add row for all categories 
  categories <- coefficients %>% distinct(category) %>% pull(category)
  
  # Create a tibble with a row for each category
  additional_rows <- tibble(
    event_time = -10,
    estimate = 0,
    std.error = 0,
    category = categories
  )
  
  coefficients <- coefficients %>% 
    # add row for 0s for all categories
    bind_rows(additional_rows)
  
  
  # create the plot
  dodge_width <- 1.5
  
  plot <- 
    coefficients %>% 
    ggplot(aes(x = event_time, y = estimate, color = category))  +
    geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                  width = 0.3,
                  size = 1.1,
                  alpha = 0.7, 
                  position = position_dodge(width = dodge_width)) +
    geom_point(size = 2,
               position = position_dodge(width = dodge_width)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title,
         subtitle = "",
         x = "Years Relative to Treatment",
         y = "Difference in Difference Estimate") +
    scale_x_continuous(breaks = seq(-40, 40, 10)) +
    scale_color_brewer(type = "seq", palette = "Reds") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "white"),
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) 
  
  plot

}
