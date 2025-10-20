# File: save_estimates.R
# -------------------------------------------------
# Functions for processing and saving model results
# -------------------------------------------------


# Function to standardize the "term" format between TWFE and Sun and Abraham
standardize_terms <- function(term, model_type) {
  if (model_type == "TWFE") {
    # For TWFE, remove "treated" and just keep event_time value
    return(gsub("event_time::|:treated", "", term))
  } else  {
    # For Sun and Abraham, rename "year" to "event_time" to match TWFE
    return(gsub("year::", "", term))  # This step is just to ensure consistency
  }
  return(term)
}

# function to save model results in df
save_estimates <- function(model_list, estimator_name) {
  map_dfr(names(model_list), function(outcome_var_name) {
    tidy(model_list[[outcome_var_name]]) %>%
      mutate(outcome = outcome_var_name,
             estimator = estimator_name,
             term = standardize_terms(term, estimator_name)) %>% 
      mutate_if(is.numeric, round, 4) 
  })
}
