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
  # outcome_var <- "black_share"
  # treatment_group <- "treated"
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
      treated = ifelse(location_type == treatment_group, 1, 0)) %>% 
    ungroup() %>% 
    # exclude extreme event times
    filter(event_time > -30, event_time <= 30)
  
  
  # calculate pre-period mean of variable
  pre_period_mean <-
    data %>%
    filter(event_time == -10) %>%
    summarize(mean_outcome = mean(!!sym(outcome_var), na.rm = TRUE)) %>%
    pull(mean_outcome)
  
  # ---
  # Estimate Models
  # ---
  
  # Create the formula for the event study
  # baseline model
  # Note: city-year does nothing in this model with matched FE
  formula <- as.formula(paste(outcome_var, "~ i(event_time, treated, ref = -10) | GISJOIN_1950 +  match_group^event_time"))
  model <- feols(formula, data = data, weights = ~weights, cluster = ~GISJOIN_1950)
  model_conley <- feols(formula, data = data, weights = ~weights,
                        vcov = vcov_conley(lat = "lat", lon = "lon", 
                                           cutoff = 2)) 
  
  # Model without matching
  formula_no_match <- as.formula(paste(outcome_var, "~ i(event_time, treated, ref = -10) | GISJOIN_1950"))
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
