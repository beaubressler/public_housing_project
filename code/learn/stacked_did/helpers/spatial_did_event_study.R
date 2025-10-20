# Spatial DiD Event Study Regression Function
# Runs stacked difference-in-differences with inner and outer rings
# Uses Wing (2024) weighting following Guennewig-Moehnert and Blanco-Neri

library(fixest)
library(dplyr)
library(stringr)
library(broom)

# Main regression function
run_event_study_regression <- function(data, dep_var) {

  # Prepare data
  data <- data %>%
    mutate(ring = case_when(location_type == "treated" ~ 1,
                            location_type == "inner" ~ 2,
                            location_type == "outer" ~ 3)) %>%
    mutate(ring = factor(ring, levels = c(3,2,1), labels = c("outer",
                                                             "inner", "treated"))) %>%
    filter(event_time > -30, event_time < 40)


  # Calculate WING (2024) weights following Guennewig-Moehnert and Blanco-Neri
  data <- data %>%
    group_by(treatment_year, treated_id, ring) %>%
    mutate(N_re = n()) %>%  # Observations in ring r around project e
    ungroup() %>%
    group_by(treatment_year, ring) %>%
    mutate(N_r = n()) %>%  # Total observations in ring r across all projects
    ungroup() %>%
    mutate(wing_weight = N_re / N_r)


  # Create baseline outcome control for current dependent variable
  # Using event_time = -10 to control for pre-existing differences
  baseline_outcome <- data %>%
    filter(event_time == -10) %>%
    select(GISJOIN_1950, treated_id, !!sym(dep_var)) %>%
    rename(baseline_value = !!sym(dep_var)) %>%
    distinct() %>%
    mutate(base_decile = ntile(baseline_value, 10))

  initial_pop <- data %>%
    filter(event_time == -10) %>%
    select(GISJOIN_1950, treated_id, total_pop) %>%
    rename(initial_pop = total_pop) %>%
    distinct()

  # Merge back to main data
  data <- data %>%
    left_join(baseline_outcome, by = c("GISJOIN_1950", "treated_id")) %>%
    left_join(initial_pop, by = c("GISJOIN_1950", "treated_id"))

  # Build formula with project-specific fixed effects including redlining
  formula <- as.formula(paste(
    dep_var, "~ i(event_time, ring, ref = -10, ref2 = 'outer')",
    "| treated_id^year + treated_id^ring + cluster^treated_id^year + baseline_value^treated_id",
    "+ treated_id^ur_binary_5pp + treated_id^has_highway_1km + treated_id^redlined_binary_80pp"
  ))

  # Run regression with all rings
  model <- feols(formula, data = data,
                 weights = ~wing_weight,
                 cluster = c("treated_id"))

  # Extract coefficients for all rings
  coeffs <- coef(model)
  ses <- se(model)

  # Get ring types (excluding reference ring)
  ring_types <- c("treated", "inner")

  # Extract coefficients for each ring type
  all_ring_data <- map_dfr(ring_types, function(ring_name) {
    ring_idx <- str_detect(names(coeffs), paste0("ring::", ring_name))
    if(any(ring_idx)) {
      ring_coefs <- coeffs[ring_idx]
      ring_ses <- ses[ring_idx]
      ring_times <- as.numeric(str_extract(names(ring_coefs),
                                           "-?[0-9]+"))

      tibble(
        event_time = ring_times,
        estimate = ring_coefs,
        se = ring_ses,
        location_type = ring_name
      )
    } else {
      warning(paste("No coefficients found for ring:", ring_name))
      tibble()
    }
  })

  # Add reference points for all rings at event_time = -10
  tidy_output <- all_ring_data %>%
    bind_rows(
      tibble(
        event_time = -10,
        estimate = 0,
        se = 0,
        location_type = ring_types
      )
    ) %>%
    mutate(
      std.error = se,  # For compatibility with existing plotting code
      term = as.character(event_time),  # For compatibility
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se,
      p.value = 2 * (1 - pnorm(abs(estimate / se)))  # Two-tailed p-value
    ) %>%
    arrange(location_type, event_time)

  # Extract model summary
  model_summary <- broom::glance(model) %>%
    mutate_if(is.numeric, round, 4)

  # Return results
  output <- list(
    coefficients = tidy_output,
    summary = model_summary,
    model = model  # Include the full model for additional analysis
  )

  return(output)
}
