# File: create_regression_summary_tables.R
# -------------------------------------------------
# Functions for creating regression summary tables from event study models
# Shows selected event times with full regression statistics (R², N, etc.)
# -------------------------------------------------

#' Create regression summary table for event study models
#'
#' @param model_list List of fitted feols models (from did_event_study)
#' @param outcomes_to_show Character vector of outcome variable names
#' @param outcome_labels Named vector mapping outcome names to labels
#' @param group_type Character, either "treated" or "inner"
#' @param event_times Numeric vector of event times to display
#'
#' @return A tinytable object ready to save
#'
create_event_study_regression_table <- function(model_list,
                                                 outcomes_to_show,
                                                 outcome_labels,
                                                 group_type,
                                                 event_times = c(-20, 0, 10, 20, 30)) {

  # Select models for specified outcomes and group
  models <- list()
  for (outcome in outcomes_to_show) {
    key <- paste(outcome, group_type, sep = "_")
    if (key %in% names(model_list)) {
      # Use the label as the column name
      col_name <- if (outcome %in% names(outcome_labels)) {
        outcome_labels[outcome]
      } else {
        outcome
      }
      models[[col_name]] <- model_list[[key]]
    }
  }

  if (length(models) == 0) {
    stop("No models found for specified outcomes and group")
  }

  # Create coefficient map showing only selected event times
  # The term names in feols event studies are like "event_time::20:treated"
  coef_names <- paste0("event_time::", event_times, ":treated")
  coef_labels <- paste0("t = ", ifelse(event_times >= 0, paste0("+", event_times), event_times))
  coef_map <- setNames(coef_labels, coef_names)

  # Use modelsummary with GOF map
  table <- modelsummary(
    models,
    coef_map = coef_map,
    gof_map = get_gof_map_regression(),
    gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within|FE",
    stars = TRUE,
    fmt = 3,
    output = "tinytable"
  ) |>
    theme_tt("tabular")

  return(table)
}
