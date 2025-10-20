####
# Compute weighted matches for a single treated tract
# Creates a synthetic control from all control tracts in same county
####

compute_weighted_matches <- function(
  data,
  treated_tract,
  treatment_year,
  outcome_vars,
  time_invariant_vars = c("asinh_distance_from_cbd"),
  exact_match_vars = c("redlined_binary_80pp", "ur_binary_5pp"),
  weight_method = "overlap",  # "overlap", "kernel", "ipw"
  bandwidth = 0.06,           # for kernel method
  use_trends = FALSE,         # whether to match on trends (t-20 to t-10)
  group_type = "treated"      # "treated" or "inner"
) {

  # Get county and exact match characteristics of treated tract
  treated_chars <- data %>%
    filter(GISJOIN_1950 == treated_tract) %>%
    select(county_id, all_of(exact_match_vars)) %>%
    distinct()

  county <- treated_chars$county_id

  # Filter to county with exact matching on redlining/urban renewal
  # Keep treated tract + never-treated or not-yet-treated controls
  county_data <- data %>%
    filter(
      county_id == county,
      (GISJOIN_1950 == treated_tract) |
      (is.na(treatment_year) | treatment_year > !!treatment_year)
    )

  # Apply exact matching constraints
  for (var in exact_match_vars) {
    county_data <- county_data %>%
      filter(!!sym(var) == treated_chars[[var]])
  }

  # Create treatment indicator
  county_data <- county_data %>%
    mutate(treated = ifelse(GISJOIN_1950 == treated_tract, 1, 0))

  # Get pre-treatment data for propensity score
  if (use_trends) {
    # Use both t-10 and t-20 for trends
    pre_data_t10 <- county_data %>%
      filter(year == treatment_year - 10) %>%
      select(GISJOIN_1950, treated, all_of(outcome_vars), all_of(time_invariant_vars)) %>%
      rename_with(~paste0("t10_", .), .cols = all_of(outcome_vars))

    pre_data_t20 <- county_data %>%
      filter(year == treatment_year - 20) %>%
      select(GISJOIN_1950, all_of(outcome_vars)) %>%
      rename_with(~paste0("t20_", .), .cols = all_of(outcome_vars))

    # Merge and compute trends
    pre_data <- pre_data_t10 %>%
      left_join(pre_data_t20, by = "GISJOIN_1950")

    # Create trend variables
    for (var in outcome_vars) {
      trend_var <- paste0("trend_", var)
      pre_data[[trend_var]] <- pre_data[[paste0("t10_", var)]] - pre_data[[paste0("t20_", var)]]
    }

    # Variables for PS model: levels at t-10 + trends + time-invariant
    ps_vars <- c(
      paste0("t10_", outcome_vars),
      paste0("trend_", outcome_vars),
      time_invariant_vars
    )

  } else {
    # Just use t-10 levels
    pre_data <- county_data %>%
      filter(year == treatment_year - 10) %>%
      select(GISJOIN_1950, treated, all_of(outcome_vars), all_of(time_invariant_vars))

    ps_vars <- c(outcome_vars, time_invariant_vars)
  }

  # Drop any observations with missing values
  pre_data <- pre_data %>%
    filter(complete.cases(across(all_of(ps_vars))))

  # Check if we have both treated and controls
  n_treated <- sum(pre_data$treated == 1)
  n_control <- sum(pre_data$treated == 0)

  if (n_treated == 0) {
    warning("No treated tract found in pre-period data")
    return(NULL)
  }

  if (n_control == 0) {
    warning("No control tracts found in county ", county, " for treated tract ", treated_tract)
    return(NULL)
  }

  # Estimate propensity score within county
  ps_formula <- as.formula(paste("treated ~", paste(ps_vars, collapse = " + ")))

  ps_model <- tryCatch({
    suppressWarnings(glm(ps_formula, data = pre_data, family = binomial()))
  }, error = function(e) {
    warning("Propensity score model failed for tract ", treated_tract, ": ", e$message, call. = FALSE)
    return(NULL)
  })

  if (is.null(ps_model)) return(NULL)

  # Check for convergence issues
  if (!ps_model$converged) {
    # Use Mahalanobis distance instead of PS for this county
    # Simple approach: equal weights for all controls
    n_controls <- sum(pre_data$treated == 0)
    pre_data$ps <- ifelse(pre_data$treated == 1, 0.5, 0.5)
  } else {
    pre_data$ps <- predict(ps_model, type = "response")
  }

  # Bound propensity scores to avoid extreme values
  pre_data$ps <- pmin(pmax(pre_data$ps, 0.01), 0.99)

  # Compute weights based on method
  if (weight_method == "overlap") {
    # Overlap weights (Li & Thomas 2019)
    # Treated: weight by probability of being control
    # Control: weight by probability of being treated
    pre_data <- pre_data %>%
      mutate(weight = ifelse(treated == 1,
                            1 - ps,      # Treated: prob of being control
                            ps))         # Control: prob of being treated

    # Normalize control weights to sum to 1 within county
    sum_control_weights <- sum(pre_data$weight[pre_data$treated == 0])
    if (sum_control_weights > 0) {
      pre_data <- pre_data %>%
        mutate(weight = ifelse(treated == 0,
                              weight / sum_control_weights,
                              weight))
    }

  } else if (weight_method == "kernel") {
    # Kernel weights (Heckman et al. 1998)
    # Weight controls by distance to treated tract's PS using Epanechnikov kernel
    treated_ps <- pre_data %>% filter(treated == 1) %>% pull(ps)

    pre_data <- pre_data %>%
      mutate(
        ps_distance = abs(ps - treated_ps),
        # Epanechnikov kernel: K(u) = 0.75 * (1 - u^2) for |u| <= 1, else 0
        kernel_value = ifelse(ps_distance / bandwidth <= 1,
                             0.75 * (1 - (ps_distance / bandwidth)^2),
                             0),
        weight = ifelse(treated == 1,
                       1,                    # Treated always weight 1
                       kernel_value)
      )

    # Normalize control weights to sum to 1
    sum_control_weights <- sum(pre_data$weight[pre_data$treated == 0])
    if (sum_control_weights > 0) {
      pre_data <- pre_data %>%
        mutate(weight = ifelse(treated == 0,
                              weight / sum_control_weights,
                              weight))
    } else {
      warning("No controls within bandwidth for tract ", treated_tract)
      return(NULL)
    }

  } else if (weight_method == "ipw") {
    # Inverse propensity weights
    # Add small epsilon to avoid division by zero
    epsilon <- 0.001
    pre_data <- pre_data %>%
      mutate(
        ps_bounded = pmin(pmax(ps, epsilon), 1 - epsilon),
        weight = ifelse(treated == 1,
                       1 / ps_bounded,           # Treated: 1/ps
                       1 / (1 - ps_bounded))     # Control: 1/(1-ps)
      )

    # Normalize control weights
    sum_control_weights <- sum(pre_data$weight[pre_data$treated == 0])
    if (sum_control_weights > 0) {
      pre_data <- pre_data %>%
        mutate(weight = ifelse(treated == 0,
                              weight / sum_control_weights,
                              weight))
    }
  }

  # Merge weights back to full panel
  weights_df <- pre_data %>%
    select(GISJOIN_1950, weight, ps)

  # Keep only tracts that got non-zero weights
  tracts_to_keep <- weights_df %>%
    filter(weight > 0) %>%
    pull(GISJOIN_1950)

  matched_panel <- county_data %>%
    filter(GISJOIN_1950 %in% tracts_to_keep) %>%
    left_join(weights_df, by = "GISJOIN_1950") %>%
    # Create match group identifier
    mutate(
      match_group = paste0(treatment_year, "_", treated_tract),
      matched_treatment_year = treatment_year,
      group_type = group_type,
      match_type = paste0("weighted_", weight_method)
    )

  return(matched_panel)
}
