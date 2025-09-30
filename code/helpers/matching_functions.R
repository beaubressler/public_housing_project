# File: matching_functions.R
# -------------------------------------------------
# Helper functions for propensity score matching
# -------------------------------------------------

perform_matching_with_replacement <- function(data, treatment_year, match_vars, nearest_neighbors, group_type,
                                              match_type = "nearest", distance_option = "glm",
                                              match_link = "logit", 
                                              pre_treatment_decades = 2,
                                              exact_match_vars = c("county_id", "redlined_binary_80pp", "ur_binary_5pp"),
                                              caliper = NULL) {
  


  # for testing
  # data <- census_tract_data
  # treatment_year <- 1970  # or 1960, 1980 - pick one to test
  # match_vars <- matching_vars
  # nearest_neighbors <- knn_neighbors
  # distance_option <- "glm"
  # match_link <- "logit"
  # group_type <- "treated"  # test treated group first
  # match_type <- "nearest"
  # pre_treatment_decades <- 2
  # exact_match_vars <- exact_matching_vars
  
  
  
  # Determine pre-treatment years
  if (pre_treatment_decades == 1) {
    pre_treatment_years <- c(treatment_year - 10)
  } else if (pre_treatment_decades == 2) {
    pre_treatment_years <- c(treatment_year - 10, treatment_year - 20)
  }
  
  # Separate time-invariant and time-varying variables
  time_invariant_vars <- c("asinh_distance_from_cbd")
  time_varying_vars <- setdiff(match_vars, time_invariant_vars)
  
  # Filter data for matching
  matching_data <- data %>%
    filter(
      (location_type == group_type & treatment_year == !!treatment_year) | # Treatment group
        (location_type == "donor_pool" & year %in% pre_treatment_years))     # Control pool
  
  # Convert category to character
  matching_data <- matching_data %>%
    mutate(category_most_overlap = as.character(category_most_overlap))
  
  # Reshape to wide format
  matching_data_wide <- matching_data %>%
    dplyr::select(GISJOIN_1950, city, cbd, cbsa_title, county_id,
                  redlined_binary_80pp, ur_binary_5pp, category_most_overlap, location_type, 
                  year, all_of(c(time_varying_vars, time_invariant_vars))) %>%
    pivot_wider(
      names_from = year,
      values_from = all_of(time_varying_vars),
      names_glue = "year_{year}_{.value}"
    )
  
  # Get relevant variables for matching
  relevant_vars <- c(
    paste0("year_", rep(pre_treatment_years, each = length(time_varying_vars)),
           "_", rep(time_varying_vars, times = length(pre_treatment_years))),
    time_invariant_vars
  )
  
  # Filter complete cases
  complete_cases <- matching_data_wide %>%
    dplyr::select(all_of(relevant_vars)) %>%
    complete.cases()
  
  matching_data_wide <- matching_data_wide %>%
    filter(complete_cases)
  
  # Create treatment indicator
  matching_data_wide <- matching_data_wide %>%
    mutate(treatment_group = as.factor(ifelse(location_type == group_type, 1, 0)))
  
  # Create matching formula
  formula_str <- paste("treatment_group ~", paste(relevant_vars, collapse = " + "))
  match_formula <- as.formula(formula_str)
  
  cat("Matching with replacement for", group_type, "tracts treated in", treatment_year, "\n")
  cat("Treatment units:", sum(matching_data_wide$treatment_group == 1), "\n")
  cat("Control units:", sum(matching_data_wide$treatment_group == 0), "\n")
  
  # Perform matching WITH REPLACEMENT
  m.out <- matchit(match_formula,
                   data = matching_data_wide,
                   exact = exact_match_vars,
                   method = match_type,
                   distance = distance_option,
                   ratio = nearest_neighbors,
                   link = match_link,
                   caliper = caliper,
                   replace = TRUE)  # KEY CHANGE: WITH REPLACEMENT
  
  # Print matching summary
  cat("Matched units:", sum(m.out$weights[m.out$weights > 0]), "\n")
  cat("Control units used:", sum(m.out$weights[matching_data_wide$treatment_group == 0] > 0), "\n")
  cat("Controls used multiple times:", 
      sum(m.out$weights[matching_data_wide$treatment_group == 0] > 1), "\n\n")
  
  # Get matched data using get_matches() for replacement matching
  matched_data <- get_matches(m.out, data = matching_data_wide)
  
  # Add treatment year and group type information
  matched_data$matched_treatment_year <- treatment_year
  matched_data$group_type <- group_type
  matched_data$match_type <- match_type
  
  # Get variable names for reshaping
  year_vars <- grep("^year_", names(matched_data), value = TRUE)
  
  # Reshape back to long format
  matched_data_long <- matched_data %>%
    mutate(across(contains("category"), as.character)) %>%
    pivot_longer(
      cols = all_of(year_vars),
      names_to = c("year", ".value"),
      names_pattern = "year_([0-9]+)_(.+)"
    ) %>%
    mutate(year = as.integer(year))
  
  return(list(matched_data = matched_data_long, m.out = m.out))
}