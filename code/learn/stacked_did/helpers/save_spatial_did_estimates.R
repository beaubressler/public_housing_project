# Save Spatial DiD Estimates
# Formats and exports coefficient estimates from spatial DiD regressions

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# Save estimates to tidy data frame
save_estimates <- function(results_list, model_type = "Spatial DiD") {

  # Extract coefficient estimates from all results
  all_estimates <- map_dfr(names(results_list), function(result_name) {

    # Get the regression result
    result <- results_list[[result_name]]

    # Extract coefficients
    coefficients <- result$coefficients

    # Add outcome and group information
    coefficients %>%
      mutate(
        outcome = result_name,
        model = model_type
      )
  })

  return(all_estimates)
}

# Format estimates for robustness comparison with matched DiD
format_for_robustness_comparison <- function(estimates, spec = "spatial_did", spec_label = "Spatial DiD (Rings)") {

  estimates %>%
    mutate(
      # Extract outcome and group from combined name
      outcome_parts = str_split(outcome, "_(?=treated$|inner$)"),
      outcome_clean = map_chr(outcome_parts, ~ .x[1]),
      group = map_chr(outcome_parts, ~ if(length(.x) > 1) .x[2] else NA_character_),
      # Add specification labels
      spec = spec,
      spec_label = spec_label
    ) %>%
    select(outcome_clean, group, term, estimate, std.error, p.value, spec, spec_label) %>%
    filter(!is.na(group))  # Remove rows without group assignment
}
