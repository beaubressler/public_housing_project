# File: matched_did_event_study.R
# -------------------------------------------------
# Function to estimate TWFE and Sun & Abraham Event study
# Models in my matched difference-in-difference setting
# -------------------------------------------------

did_event_study <- function(input_data, outcome_var, treatment_group,
                            # heterogeneity filters
                            size = NULL, city_filter = NULL, initial_share = NULL) {
  
  
  # For testing, comment out otherwise
  # input_data <- tract_data_matched_2_year
  # outcome_var <- "asinh_pop_black"
  # treatment_group <- "inner"
  # size = NULL
  # city_filter = NULL
  # initial_share = NULL
  # 
  # -----------------------------
  # Filter Data
  # -----------------------------
  
  
  data <- input_data %>% 
    filter(!is.na(match_group), # keep only treated units and matched controls
           group_type == treatment_group) # keep only specified treatment group and associated controls
  
  # Apply optional heterogeneity filters
  if (!is.null(size)) data <- data %>% filter(size_group == size)
  if (!is.null(city_filter)) data <- data %>% filter(city == city_filter)
  if (!is.null(initial_share)) data <- data %>% filter(race_group == initial_share)
  
  
  data <-
    data %>%
    group_by(match_group) %>%
    mutate(
      group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
      event_time = year - group_treatment_year,
      treated = ifelse(location_type == treatment_group, 1, 0)) %>%  # treated always on if in treatment group
    ungroup() %>% 
    # exclude event times for which we don't have full overlap
    filter(event_time >= -20, event_time <= 30)
  
  
  # calculate pre-period mean of variable
  pre_period_mean <-
    data %>%
    filter(event_time == -10) %>%
    summarize(mean_outcome = mean(!!sym(outcome_var), na.rm = TRUE)) %>%
    pull(mean_outcome)
  
  # calculate baseline outcome variable (in event_time == 10)
  baseline_outcome <-
    data %>%
    filter(event_time == -10) %>%
    select(GISJOIN_1950, match_group, !!sym(outcome_var)) %>%
    dplyr::rename(baseline_outcome = !!sym(outcome_var)) %>% 
    distinct()
  
  data <- 
    data %>%
    left_join(baseline_outcome, by = c("GISJOIN_1950", "match_group") )
  
  # ---
  # Estimate Models
  # ---
  
  # Create the formula for the event study
  # baseline model
  # Conditional fixed effects based on variant
  variant <- get0("VARIANT", ifnotfound = "baseline")
  if (variant %in% c("no_cbsa", "cem")) {
    # Cross-city matching (no_cbsa) or CEM with anti-exact CBSA:
    # Add cbsa^year to control for city-specific calendar-year shocks
    # match_group^event_time already ensures within-match-group identification
    formula <- as.formula(paste(outcome_var,
                                "~ i(event_time, treated, ref = -10)",
                               #"+ i(event_time, baseline_outcome, ref = -10)",
    "| GISJOIN_1950^match_group +  match_group^event_time"))
    # Wing et al 2024: equation (3)
    # formula <- as.formula(paste0(
    #   outcome_var, " ~ i(event_time, treated, ref = -10) | treated +  match_group"
    # ))

  } else {
    # Within-city matching (baseline): standard specification
    formula <- as.formula(paste(outcome_var,
                                "~ i(event_time, treated, ref = -10) ",
                             #   "+ i(event_time, baseline_outcome, ref = -10)",
                                "| GISJOIN_1950^match_group +  match_group^event_time"))

    # Wing et al 2024 equation 3
    # formula <- as.formula(paste0(
    #   outcome_var, " ~ i(event_time, treated, ref = -10) | treated + match_group + cbsa_title^year"))
  }
  
  model <- feols(formula, data = data, weights = ~weights, cluster = ~GISJOIN_1950)
  model_conley <- feols(formula, data = data, weights = ~weights,
                        vcov = vcov_conley(lat = "lat", lon = "lon", 
                                           cutoff = 2)) 
  
  # Model without matching
  formula_no_match <- as.formula(paste(outcome_var, "~ i(event_time, treated, ref = -10) | GISJOIN_1950 + cbsa_title^year"))
  model_no_match <- feols(formula_no_match, data = data, weights = ~weights, cluster = ~GISJOIN_1950)
  model_no_match_conley <- feols(formula_no_match, data = data, weights = ~weights,
                                 vcov = vcov_conley(lat = "lat", lon = "lon", 
                                                    cutoff = 2)) 
  
  # # Sun and Abraham model, with matching
  # 5/2025: In current matched DiD set-up, Sun and Abraham is the same as TWFE
  # sunab_data <- 
  #   data %>% 
  #   # set treatment_year = 100 if missing
  #   mutate(treatment_year = ifelse(is.na(treatment_year), 100, treatment_year))
  # 
  # formula_sunab <- as.formula(paste(outcome_var, "~ sunab(treatment_year, year, ref.p = -10) | cbsa_title^year + GISJOIN_1950 + match_group^year"))
  # model_sunab <- feols(formula_sunab, data = sunab_data, weights = ~weights, cluster = ~GISJOIN_1950)
  # model_sunab_conley <- feols(formula_sunab, data = sunab_data, weights = ~weights,
  #                             vcov = vcov_conley(lat = "lat", lon = "lon", 
  #                                                cutoff = 1)) 
  # 
  # # sun and abraham model without matching
  # formula_sunab_no_match <- 
  #   as.formula(paste(outcome_var, "~ sunab(treatment_year, year, ref.p = -10) | cbsa_title^year + GISJOIN_1950"))
  # model_sunab_no_match <- feols(formula_sunab_no_match, data = sunab_data, weights = ~weights,
  #                               cluster = ~GISJOIN_1950)
  # model_sunab_no_match_conley <- feols(formula_sunab_no_match, data = sunab_data, weights = ~weights,
  #                                               vcov = vcov_conley(lat = "lat", lon = "lon",
  #                                                                  cutoff = 1)) 
  
  # ---
  # Return Results
  # ---
  
  return(list(twfe = model, twfe_conley =  model_conley, 
              twfe_nomatch_conley = model_no_match_conley))
}
