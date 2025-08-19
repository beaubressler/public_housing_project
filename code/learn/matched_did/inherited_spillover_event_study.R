# Spillover Effects Analysis - Public Housing
####
# This script analyzes spillover effects of public housing on neighboring tracts
# by comparing inner rings of treated tracts vs inner rings of matched control tracts.
# Uses Wing et al (2024) stacked DiD weights for proper inference.
# - Data loading and Wing weight computation
# - Event study regressions for spillover effects  
# - Visualization and results saving
####

library(tidyverse)
library(fixest)
library(here)
library(tableone)
library(data.table)

rm(list = ls())

# Wing et al (2024) Stacked DiD Weights Function -----
# https://rawcdn.githack.com/hollina/stacked-did-weights/18a5e1155506cbd754b78f9cef549ac96aef888b/stacked-example-r-and-stata.html

compute_weights <- function(dataset, treatedVar, eventTimeVar, subexpVar) {
  
  # Step 1: Compute stack-time counts for treated and control
  stack_totals <- dataset %>%
    group_by(!!sym(eventTimeVar)) %>%
    summarise(
      stack_n = n(),
      stack_treat_n = sum(!!sym(treatedVar)),
      stack_control_n = sum(1 - !!sym(treatedVar)),
      .groups = 'drop'
    )
  
  # Step 2: Compute sub-experiment-level counts  
  sub_totals <- dataset %>%
    group_by(!!sym(subexpVar), !!sym(eventTimeVar)) %>%
    summarise(
      sub_n = n(),
      sub_treat_n = sum(!!sym(treatedVar)),
      sub_control_n = sum(1 - !!sym(treatedVar)),
      .groups = 'drop'
    )
  
  # Step 3: Merge and compute shares
  weighted_data <- dataset %>%
    left_join(stack_totals, by = eventTimeVar) %>%
    left_join(sub_totals, by = c(subexpVar, eventTimeVar)) %>%
    mutate(
      sub_share = sub_n / stack_n,
      sub_treat_share = sub_treat_n / stack_treat_n,
      sub_control_share = sub_control_n / stack_control_n
    ) %>%
    # Step 4: Compute weights for treated and control groups
    mutate(
      stack_weight = ifelse(!!sym(treatedVar) == 1, 
                           1, 
                           sub_treat_share / sub_control_share)
    )
  
  return(weighted_data)
}

# Parameters -----
data_type <- "combined"
outcome_vars <- c("black_share", "white_share",
                  "asinh_pop_total", "asinh_pop_black", "asinh_pop_white",
                  "asinh_median_income", "median_rent_calculated", 
                  "asinh_median_rent_calculated", "lfp_rate", "unemp_rate")

# Define directories
match_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset")
results_dir <- here("output", "regression_results", "spillover", data_type)
figures_dir <- here("output", "figures", "spillover", data_type)

# Create output directories
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# Load and prepare data -----
cat("Loading spillover dataset...\n")
spillover_data_raw <- read_csv(here(match_data_dir, "tract_data_inherited_spillovers_2_year_replacement.csv"))

cat("Preparing analysis data with Wing weights...\n")
spillover_analysis_base <- spillover_data_raw %>%
  # Extract treatment year and create event time
  mutate(
    treatment_year = as.numeric(str_extract(match_group, "\\d{4}")),
    event_time = YEAR - treatment_year,
    treated_spillover = ifelse(location_type == "treated_spillover", 1, 0)
  ) %>%
  # Filter to reasonable event time window
  filter(event_time > -30, event_time <= 40) %>%
  # Compute Wing et al (2024) stacked DiD weights
  compute_weights(
    treatedVar = "treated_spillover", 
    eventTimeVar = "event_time",
    subexpVar = "match_group"
  ) %>%
  mutate(weights = stack_weight)

cat("Analysis data prepared:", nrow(spillover_analysis_base), "observations\n")
cat("Event time range:", min(spillover_analysis_base$event_time), "to", 
    max(spillover_analysis_base$event_time), "\n")
cat("Treatment years:", paste(sort(unique(spillover_analysis_base$treatment_year)), collapse = ", "), "\n")

# Spillover analysis function -----
run_spillover_analysis <- function(data, outcome_var) {
  
  cat("Analyzing", outcome_var, "...\n")
  
  # Filter for available outcome data
  analysis_data <- data %>%
    filter(!is.na(!!sym(outcome_var)))
  
  # Summary statistics
  summary_stats <- analysis_data %>%
    group_by(location_type) %>%
    summarise(
      n_obs = n(),
      n_tracts = n_distinct(GISJOIN_1950),
      n_match_groups = n_distinct(match_group),
      mean_outcome = mean(!!sym(outcome_var), na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("Summary for", outcome_var, ":\n")
  print(summary_stats)
  
  # Balance check at baseline (event_time = -10)
  balance_data <- analysis_data %>%
    filter(event_time == -10)
  
  if (nrow(balance_data) > 0) {
    covariates <- c(outcome_var, "asinh_pop_total", "asinh_median_income", 
                    "asinh_distance_from_cbd", "asinh_median_rent_calculated")
    
    balance_table <- CreateTableOne(
      vars = covariates,
      strata = "location_type", 
      data = balance_data,
      test = TRUE
    )
    
    cat("Balance check for", outcome_var, "at baseline (event_time = -10):\n")
    print(balance_table, smd = TRUE)
  }
  
  # Main spillover regression
  spillover_formula <- as.formula(paste(
    outcome_var, "~ i(event_time, treated_spillover, ref = -10) | GISJOIN_1950 + match_group^event_time"
  ))
  
  spillover_model <- feols(
    spillover_formula,
    data = analysis_data,
    weights = ~weights,
    cluster = ~GISJOIN_1950
  )
  
  # Extract results
  spillover_results <- coeftable(spillover_model) %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    filter(str_detect(term, "event_time")) %>%
    mutate(
      event_time = as.numeric(str_extract(term, "-?\\d+")),
      coef = Estimate,
      se = `Std. Error`,
      ci_lower = coef - 1.96 * se,
      ci_upper = coef + 1.96 * se,
      significant = abs(coef / se) > 1.96,
      outcome_var = outcome_var
    ) %>%
    select(outcome_var, event_time, coef, se, ci_lower, ci_upper, significant) %>%
    # Add reference period
    bind_rows(data.frame(
      outcome_var = outcome_var,
      event_time = -10,
      coef = 0, se = 0, ci_lower = 0, ci_upper = 0, significant = FALSE
    )) %>%
    arrange(event_time)
  
  # Pretrend diagnostic
  pretrend_periods <- spillover_results %>%
    filter(event_time >= -30, event_time < 0, event_time != -10)
  
  if (nrow(pretrend_periods) > 0) {
    significant_pretrends <- sum(pretrend_periods$significant)
    total_pretrends <- nrow(pretrend_periods)
    
    cat("Pre-treatment periods with significant effects:", significant_pretrends, 
        "out of", total_pretrends, "for", outcome_var, "\n")
    
    if (significant_pretrends / total_pretrends > 0.1) {
      cat("WARNING: Multiple significant pre-treatment effects detected for", outcome_var, "\n")
    }
  }
  
  return(list(
    model = spillover_model,
    results = spillover_results,
    summary_stats = summary_stats
  ))
}

# Create spillover event study plot -----
plot_spillover_results <- function(results_df, outcome_var) {
  
  # Create outcome labels lookup (matching your baseline style)
  outcome_labels <- c(
    "black_share" = "Black Population Share",
    "white_share" = "White Population Share", 
    "asinh_pop_total" = "Log Total Population",
    "asinh_pop_black" = "Log Black Population",
    "asinh_pop_white" = "Log White Population",
    "asinh_median_income" = "Log Median Income",
    "median_rent_calculated" = "Median Rent",
    "asinh_median_rent_calculated" = "Log Median Rent",
    "lfp_rate" = "Labor Force Participation Rate",
    "unemp_rate" = "Unemployment Rate"
  )
  
  # Get clean label
  clean_label <- outcome_labels[[outcome_var]]
  if (is.null(clean_label)) clean_label <- outcome_var
  
  ggplot(results_df, aes(x = event_time, y = coef)) +
    geom_errorbar(aes(ymin = coef - 2 * se, ymax = coef + 2 * se),
                  width = 0.3, size = 1.1, alpha = 0.7, color = "#1B9E77") +
    geom_point(size = 2, color = "#1B9E77") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste("Spillover Effects: Effect of public housing on", clean_label),
      x = "Years Relative to Treatment", 
      y = "Difference-in-Difference Estimate"
    ) +
    scale_x_continuous(breaks = seq(-30, 40, 10)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.background = element_rect(fill = "white"),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}

# Main Analysis Loop -----
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("STARTING SPILLOVER ANALYSIS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

all_results <- list()
all_models <- list()
all_plots <- list()

for (outcome_var in outcome_vars) {
  
  cat("\n", paste(rep("-", 40), collapse = ""), "\n")
  
  # Run analysis
  analysis_output <- run_spillover_analysis(spillover_analysis_base, outcome_var)
  
  # Store results
  all_results[[outcome_var]] <- analysis_output$results
  all_models[[outcome_var]] <- analysis_output$model
  
  # Create and save plot
  spillover_plot <- plot_spillover_results(analysis_output$results, outcome_var)
  all_plots[[outcome_var]] <- spillover_plot
  
  # Save plot
  ggsave(
    filename = file.path(figures_dir, paste0("spillover_", outcome_var, ".pdf")),
    plot = spillover_plot,
    width = 10, height = 7
  )
  
  # Save individual results
  write_csv(analysis_output$results, 
            file.path(results_dir, paste0("spillover_results_", outcome_var, ".csv")))
  
  cat("Completed analysis for", outcome_var, "\n")
}

# Save combined results -----
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SAVING COMBINED RESULTS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Combine all results
combined_results <- bind_rows(all_results, .id = "outcome_variable") 
write_csv(combined_results, file.path(results_dir, "spillover_results_all_outcomes.csv"))

# Summary statistics
cat("Successfully analyzed", length(all_results), "outcomes:\n")
cat(paste(names(all_results), collapse = ", "), "\n")

cat("\nResults saved to:", results_dir, "\n")
cat("Plots saved to:", figures_dir, "\n")

# Heterogeneity Analysis -----
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("STARTING SPILLOVER HETEROGENEITY ANALYSIS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Check that baseline characteristics are available
cat("Checking availability of treated tract baseline characteristics:\n")
baseline_vars <- c("total_public_housing_units_t0", "black_share_tminus10", 
                   "asinh_median_income_tminus10")

for (var in baseline_vars) {
  if (var %in% names(spillover_analysis_base)) {
    n_non_missing <- sum(!is.na(spillover_analysis_base[[var]]))
    cat(paste0(var, ": ", n_non_missing, " non-missing values\n"))
  } else {
    cat(paste0(var, ": NOT FOUND - need to re-run spillover dataset creation\n"))
  }
}

# Define heterogeneity variables for spillover analysis
# Note: These are characteristics of the TREATED TRACTS that might affect spillover intensity
spillover_het_specs <- list(
  total_public_housing_units_t0 = list(binning_ntile = 3),       # Project size (at treatment)
  black_share_tminus10 = list(binning_ntile = 3),               # Baseline neighborhood demographics  
  asinh_median_income_tminus10 = list(binning_ntile = 3)        # Baseline income
)

# Function to run spillover heterogeneity analysis (exactly following baseline file approach)
run_spillover_heterogeneity <- function(input_data, outcome_var, het_var, binning_ntile = 3) {
  
  # het_var already has _tminus10 suffix, so use it directly
  data <- input_data %>%
    mutate(
      year = YEAR,  # Create lowercase year for fixed effects
      post = as.integer(event_time >= 0)
    ) %>%
    filter(!is.na(match_group),
           !is.na(!!sym(het_var)),
           event_time > -30,
           event_time < 50)
  
  # Use ntile binning (same as baseline)
  bin_var <- paste0(het_var, "_bin")
  data <- data %>%
    mutate(!!bin_var := ntile(!!sym(het_var), binning_ntile))
  het_term <- paste0("factor(", bin_var, ")")
  
  formula <- as.formula(paste0(
    outcome_var, " ~ treated_spillover * post * ", het_term,
    " | GISJOIN_1950 + match_group^year"
  ))
  
  model <- feols(
    formula,
    data = data,
    weights = ~weights,
    cluster = ~GISJOIN_1950
  )
  
  return(model)
}

# Create heterogeneity plots (simple coefficient comparison)
plot_spillover_heterogeneity <- function(results_df, outcome_var, het_var) {
  
  # Get clean labels
  outcome_labels <- c(
    "black_share" = "Black Population Share",
    "white_share" = "White Population Share", 
    "asinh_pop_total" = "Log Total Population",
    "asinh_pop_black" = "Log Black Population",
    "asinh_pop_white" = "Log White Population",
    "asinh_median_income" = "Log Median Income",
    "asinh_median_rent_calculated" = "Log Median Rent",
    "lfp_rate" = "Labor Force Participation Rate",
    "unemp_rate" = "Unemployment Rate"
  )
  
  het_labels <- c(
    "total_public_housing_units_tminus10" = "Project Size",
    "black_share_tminus10" = "Baseline Black Share",
    "asinh_median_income_tminus10" = "Baseline Log Income")
  
  clean_outcome <- outcome_labels[[outcome_var]]
  if (is.null(clean_outcome)) clean_outcome <- outcome_var
  
  clean_het <- het_labels[[het_var]]
  if (is.null(clean_het)) clean_het <- het_var
  
  # Ensure het_group is factor with correct ordering
  results_df <- results_df %>%
    mutate(het_group = factor(het_group, levels = c("Low", "Medium", "High")))
  
  ggplot(results_df, aes(x = het_group, y = coef, fill = het_group)) +
    geom_col(alpha = 0.7) +
    geom_errorbar(aes(ymin = coef - 2 * se, ymax = coef + 2 * se),
                  width = 0.3, size = 1.1, alpha = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste("Spillover Effects by", clean_het),
      subtitle = paste("Outcome:", clean_outcome),
      x = clean_het, 
      y = "Spillover Effect (Post-Treatment)",
      fill = clean_het
    ) +
    scale_fill_brewer(type = "qual", palette = "Dark2") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "white"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}

# Run heterogeneity analysis (exactly following baseline file approach)
key_outcomes_het <- c("black_share", "white_share", "asinh_median_income", "asinh_pop_total")

results_spillover_heterogeneity <- list()

for (het_var in names(spillover_het_specs)) {
  spec <- spillover_het_specs[[het_var]]
  
  for (outcome in key_outcomes_het) {
    results_spillover_heterogeneity[[paste0(outcome, "_het_by_initial_", het_var)]] <-
      run_spillover_heterogeneity(
        input_data = spillover_analysis_base,
        outcome_var = outcome,
        het_var = het_var,
        binning_ntile = spec$binning_ntile
      )
  }
}

# Summary of heterogeneity results
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SPILLOVER HETEROGENEITY RESULTS SUMMARY\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("Successfully analyzed", length(results_spillover_heterogeneity), "spillover heterogeneity models\n")
cat("Models stored in results_spillover_heterogeneity list\n")

# Show available models
cat("\nAvailable heterogeneity models:\n")
for (name in names(results_spillover_heterogeneity)) {
  cat(" -", name, "\n")
}

cat("\n=== SPILLOVER ANALYSIS COMPLETE ===\n")