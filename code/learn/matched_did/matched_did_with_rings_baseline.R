# Public Housing Event Study Analysis
####
# This script performs a matched difference-in-differences (DiD) analysis to estimate 
# the effects of public housing construction on neighborhood outcomes. It includes:
# - Data loading and preprocessing
# - Matched event study regressions (TWFE and Sun & Abraham)
# - Heterogeneity analyses by project size, minority share, income, and population density
# - Visualization of event study results
# - Automated saving of results


# Relevant helper functions in code/helpers/


####

library(MatchIt)
library(tidyverse)
library(sf)
library(lmtest)
library(fixest)
library(here)
library(RColorBrewer)

# libraries for covariate testing
library(cobalt)
library(tableone)
library(kableExtra)
#library for Sant'Anna and Zhou (2020)
library(DRDID)
# library for Callaway and Sant'Anna (2021)
library(did)

library(splines)
library(modelsummary)

rm(list=ls())

# Preliminaries -----

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small", or "combined"
data_type <- "combined"

set.seed(123456L)
# set number of nearest neighbors for KNN matching
knn_neighbors <- 1

# Notes: With CDD large, KNN = 2 seems to work well
# with the smaller dataset, KNN = 3 work

# define directories
helper_dir <- here("code", "helpers")
merged_data_dir <- here("data", "derived", "merged", data_type)
match_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset")

map_output_dir <- here("output", "figures", "matched_did", data_type)
balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)
holc_data_dir <- here("data", "derived", "holc")

map_dir <- here("output", "figures", "matched_did", data_type, "with_rings")
results_dir <- here("output", "regression_results", "matched_did", data_type)
balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)


# define file paths
tract_with_treatment_status_filepath <-
  here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")
treated_tracts_panel_filepath <-
  here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")

event_study_rings_filepath <-
  here(merged_data_dir, "unique_tracts_and_rings.csv")

# read census data with treatment status
census_tract_sample_raw <-
  st_read(tract_with_treatment_status_filepath)

# read tract tracts panel
treated_tracts_panel_raw <-
  st_read(treated_tracts_panel_filepath) %>% 
  st_drop_geometry()

# read in unique rings and tracts
# These are the tracts that are treated, the ring of tracts touching those, and the ring of tracts
# touching the inner ring tracts
tracts_and_rings <-
  read_csv(event_study_rings_filepath) %>% 
   #try excluding outer
  filter(location_type != "outer")

# get datasets for regressions
tract_data_matched_1_year <-
  read_csv(here(match_data_dir, "tract_data_matched_1_year.csv")) %>% 
  mutate(ln_total_units = log(total_units))

tract_data_matched_2_year <-
  read_csv(here(match_data_dir, "tract_data_matched_2_year.csv")) %>% 
  # adding this:
  mutate(ln_total_units = log(total_units))


# tract_data_matched_1_year <- read_csv(here(match_data_dir, "tract_data_matched_1_year_genetic.csv"))
# tract_data_matched_2_year <- read_csv(here(match_data_dir, "tract_data_matched_2_year_genetic.csv"))

# load helper functions
source(here(helper_dir, "matched_did_event_study.R"))
source(here(helper_dir, "save_matched_did_estimates.R"))
source(here(helper_dir, "plot_matched_did_event_studies.R"))
source(here(helper_dir, "save_event_study_plots.R"))

## !! Define outcome variables and group types -----
outcome_variables <- c("asinh_private_population_estimate", 
                       "private_population_estimate",
                       "asinh_private_black_population_estimate",
                       "private_black_population_estimate",
                       "asinh_private_white_population_estimate",
                       "private_white_population_estimate",
                       "local_dissimilarity_index",
                       "local_dissimilarity_index_std",
                       "black_share", "white_share",
                       "total_pop", "black_pop", "white_pop",
                       "asinh_pop_total", "asinh_pop_black",
                       "asinh_pop_white",
                       "median_income", "asinh_median_income",
                       "median_home_value_calculated",
                       "asinh_median_home_value_calculated",
                       "median_rent_calculated",
                       "asinh_median_rent_calculated",
                       "total_units",
                       "ln_total_units",
                       "lfp_rate", "unemp_rate", 
                       "pct_hs_grad")

# Create a lookup table (named vector) for clean labels
outcome_labels <- c(
  "asinh_private_population_estimate" = "Log Private Population",
  "asinh_private_black_population_estimate" = "Log Private Black Population",
  "asinh_private_white_population_estimate" = "Log Private White Population",
  "private_population_estimate" = "Private Population",
  "private_black_population_estimate" = "Private Black Population",
  "private_white_population_estimate" = "Private White Population",
  "private_black_share" = "Private Black Share",
  "local_dissimilarity_index" = "Local Dissimilarity Index",
  "local_dissimilarity_index_std" = "Std Local Dissimilarity Index",
  "black_share" = "Black Population Share",
  "white_share" = "White Population Share",
  "total_pop" = "Total Population",
  "black_pop" = "Black Population",
  "white_pop" = "White Population",
  "asinh_pop_total" = "Log Total Population",
  "asinh_pop_black" = "Log Black Population",
  "asinh_pop_white" = "Log White Population",
  "median_income" = "Median Income",
  "asinh_median_income" = "Asinh Median Income",
  "median_home_value_calculated" = "Median Home Value",
  "asinh_median_home_value_calculated" = "Asinh Median Home Value",
  "median_rent_calculated" = "Calculated Median Rent",
  "asinh_median_rent_calculated" = "Log Calculated Median Rent",
  "total_units" = "Total Housing Units",
  "ln_total_units" = "Log Housing Units",
  "lfp_rate" = "Labor Force Participation Rate",
  "unemp_rate" = "Unemployment Rate",
  "pct_hs_grad" = "HS Graduation Rate")


group_types <- c("treated", "inner")


# Baseline analysis ----

## Full sample -----
# initialize lists
did_results_event_study <- list() 
did_results_event_study_conley <- list()
did_results_event_study_no_match_conley <- list()

# save regression output to lists
for (outcome in outcome_variables) {
  for (group in  group_types) {
  
    # display which outcome and group is being worked on
    print(paste("Outcome:", outcome, "Group:", group))
    
    results <- did_event_study(tract_data_matched_2_year,
                               outcome, group)
    did_results_event_study[[paste(outcome, group, sep = "_")]] <- results$twfe
    did_results_event_study_conley[[paste(outcome, group, sep = "_")]] <- results$twfe_conley
    did_results_event_study_no_match_conley[[paste(outcome, group, sep = "_")]] <- results$twfe_nomatch_conley
  }
}

# save results in df
event_study_coefs <- 
  save_estimates(did_results_event_study, "TWFE") %>% 
  mutate(standard_errors = "Clustered SE")

event_study_conley_coefs <-
  save_estimates(did_results_event_study_conley, "TWFE") %>% 
  mutate(standard_errors = "Conley SE")
  
event_study_no_match_conley <-
  save_estimates(did_results_event_study_no_match_conley, "TWFE") %>% 
  mutate(standard_errors = "No match, Conley SE")

combined_results <- 
  bind_rows(event_study_coefs, event_study_conley_coefs,
            event_study_no_match_conley)


# look at significant pretrends
#combined_results %>% filter(str_detect(term, "-20"), p.value < .05) %>% View()

# look at significant post trends
#combined_results %>% filter(p.value < .05) %>% View()

## Plot results -----

# Save baseline event study plots
save_event_study_plots(
  reg_results_df = event_study_coefs,
  outcome_variables = outcome_variables,
  results_dir = results_dir,
  prefix = "event_study_plots_baseline",
  title_suffix = "",
  heterogeneity = NULL
)

save_event_study_plots(
  reg_results_df = event_study_conley_coefs,
  outcome_variables = outcome_variables,
  results_dir = results_dir,
  prefix = "event_study_plots_baseline_conley",
  title_suffix = ", Conley SE",
  heterogeneity = NULL
)

# Save baseline results separately for treated and inner tracts

save_event_study_plots(
  reg_results_df = event_study_coefs,
  outcome_variables = outcome_variables,
  results_dir = results_dir,
  prefix = "event_study_plots_baseline_treated",
  title_suffix = " (Treated)",
  heterogeneity = NULL,
  category_filter = "treated"
)

save_event_study_plots(
  reg_results_df = event_study_coefs,
  outcome_variables = outcome_variables,
  results_dir = results_dir,
  prefix = "event_study_plots_baseline_inner",
  title_suffix = " (Inner)",
  heterogeneity = NULL,
  category_filter = "inner"
)

### Plots with several outcomes on same plot -----
outcomes_for_combined_plots_treated <- 
  c("asinh_pop_total", "ln_total_units",
    "black_share",
    "asinh_median_income",
    "asinh_median_rent_calculated")

coefs_clean <- 
  event_study_conley_coefs %>% 
  mutate(
    group = ifelse(str_ends(outcome, "_treated"), "treated",
                   ifelse(str_ends(outcome, "_inner"), "inner", NA)),
    outcome_clean = str_remove(outcome, "_treated|_inner")
  )
  
#### Treated-----

combined_treated_results <- 
  coefs_clean %>%
  filter(group == "treated", outcome_clean %in% outcomes_for_combined_plots_treated) %>%
  mutate(
    clean_label = outcome_labels[outcome_clean],
    clean_label = factor(clean_label, levels = outcome_labels[outcomes_for_combined_plots_treated]),
    event_time = as.numeric(term)
  )

# Add reference points at event_time = -10 if needed
ref_rows <- combined_treated_results %>%
  distinct(outcome_clean, clean_label) %>%
  mutate(event_time = -10, estimate = 0, std.error = 0)

combined_treated_results <- bind_rows(combined_treated_results, ref_rows)

ggplot(combined_treated_results, aes(x = event_time, y = estimate, color = clean_label)) +
  geom_errorbar(aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 * std.error),
                width = 0.3, size = 1.1, alpha = 0.5, position = position_dodge(width = 2)) +
  geom_point(size = 2, alpha = 0.9, position = position_dodge(width = 2)) +
  geom_line(alpha = 0.7, position = position_dodge(width = 2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of public housing projects, treated neighborhoods",
    x = "Years Relative to Construction",
    y = "Difference-in-Difference Estimate",
    color = "Outcome"
  ) +
  scale_x_continuous(breaks = seq(-40, 40, 10)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_brewer(type = "qual", palette = "Dark2")

# Save
ggsave(
  filename = file.path(results_dir, "event_study_plots_combined_treated.png"),
  width = 14, height = 8
)

#### Inner ----
 outcomes_for_combined_plots_inner_1 <- 
  c("asinh_pop_total", "ln_total_units",
    "asinh_median_rent_calculated", "asinh_median_home_value_calculated"
  )

outcomes_for_combined_plots_inner_2 <- 
  c("asinh_median_income", "black_share", "pct_hs_grad", "unemp_rate", "lfp_rate")

combined_inner_results_1 <- 
  coefs_clean %>%
  filter(group == "inner", outcome_clean %in% outcomes_for_combined_plots_inner_1) %>%
  mutate(
    clean_label = outcome_labels[outcome_clean],
    clean_label = factor(clean_label, levels = outcome_labels[outcomes_for_combined_plots_inner_1]),
    event_time = as.numeric(term)
  )

ref_rows_inner <- combined_inner_results_1 %>%
  distinct(outcome_clean, clean_label) %>%
  mutate(event_time = -10, estimate = 0, std.error = 0)

combined_inner_results_1 <- bind_rows(combined_inner_results_1, ref_rows_inner)

ggplot(combined_inner_results_1, aes(x = event_time, y = estimate, color = clean_label)) +
  geom_errorbar(aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 * std.error),
                width = 0.3, size = 1.1, alpha = 0.5, position = position_dodge(width = 2)) +
  geom_point(size = 2, alpha = 0.9, position = position_dodge(width = 2)) +
  geom_line(alpha = 0.7, position = position_dodge(width = 2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of public housing projects,  nearby neighborhoods",
    x = "Years Relative to Treatment",
    y = "Difference-in-Difference Estimate",
    color = "Outcome"
  ) +
  scale_x_continuous(breaks = seq(-40, 40, 10)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

ggsave(
  filename = file.path(results_dir, "event_study_plots_combined_inner_1.png"),
  width = 14, height = 8
)



# inner 2
combined_inner_results_2 <- 
  coefs_clean %>%
  filter(group == "inner", outcome_clean %in% outcomes_for_combined_plots_inner_2) %>%
  mutate(
    clean_label = outcome_labels[outcome_clean],
    clean_label = factor(clean_label, levels = outcome_labels[outcomes_for_combined_plots_inner_2]),
    event_time = as.numeric(term)
  )

ref_rows_inner_2 <- combined_inner_results_2 %>%
  distinct(outcome_clean, clean_label) %>%
  mutate(event_time = -10, estimate = 0, std.error = 0)

combined_inner_results_2 <- bind_rows(combined_inner_results_2, ref_rows_inner_2)

ggplot(combined_inner_results_2, aes(x = event_time, y = estimate, color = clean_label)) +
  geom_errorbar(aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 * std.error),
                width = 0.3, size = 1.1, alpha = 0.5, position = position_dodge(width = 2)) +
  geom_point(size = 2, alpha = 0.9, position = position_dodge(width = 2)) +
  geom_line(alpha = 0.7, position = position_dodge(width = 2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of public housing projects, neighborhood comp, nearby neighborhoods",
    x = "Years Relative to Treatment",
    y = "Difference-in-Difference Estimate",
    color = "Outcome"
  ) +
  scale_x_continuous(breaks = seq(-40, 40, 10)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

ggsave(
  filename = file.path(results_dir, "event_study_plots_combined_inner_2.png"),
  width = 14, height = 8
)


### Plot each separately, TWFE ----

save_individual_event_study_plots <- function(reg_results_df, 
                                              outcome_variables, 
                                              results_dir, prefix,
                                              heterogeneity = FALSE) {
  
  # Define group types (used in category_filter)
   group_types <- c("treated", "inner")
  
  # Loop over outcome variables
  walk(outcome_variables, function(outcome) {
    
    # Get cleaned name (fallback to original if not found)
    clean_label <- outcome_labels[[outcome]]
    if (is.null(clean_label)) clean_label <- outcome  # Fallback to original name
    
    if (!heterogeneity) {
    # Loop over group types (treated & inner)
    walk(group_types, function(group) {
      
        print(paste("Saving plot for:", clean_label, "Group:", group))
        
        save_event_study_plots(
          reg_results_df = reg_results_df,  # Full dataset
          outcome_variables = outcome,  # Single outcome
          results_dir = results_dir,
          prefix = paste0(prefix, "_", group, "_", outcome),
          title_suffix = paste0(" (", group, ")"),  # Just add group name, no variable name
          category_filter = group, # Pass the group as the category filter
          heterogeneity = heterogeneity
        )
      })
    } else {
        save_event_study_plots(
          reg_results_df = reg_results_df,  # Full dataset
          outcome_variables = outcome,  # Single outcome
          results_dir = results_dir,
          prefix = paste0(prefix, "_", outcome),
          title_suffix =  "",  # Just add group name, no variable name
          category_filter = NULL, # Pass the group as the category filter
          heterogeneity = heterogeneity
        )
      }
  })
  
  return("Individual event study plots saved successfully!")
}

# Generate individual plots for TWFE estimates for both "treated" and "inner"
save_individual_event_study_plots(
  reg_results_df = event_study_conley_coefs, 
  outcome_variables = outcome_variables, 
  results_dir = here(results_dir, "individual_plots"), 
  prefix = "event_study_plots_twfe"
)

## Create tables of results  -----
# Filter combined_results to include just the matched results
results_with_se <- combined_results %>%
  dplyr::rename(event_time = term) %>% 
  filter(standard_errors == "Conley SE") %>% 
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    ), 
    # combine coef and SE and stars
    coef_with_se = paste0(
      round(estimate, 2), " (", round(std.error, 2), ") ", significance
    )
  ) %>%
  select(event_time, outcome, coef_with_se)



### Treated tracts -----

# "first stage"
first_stage_models <- list(
  "Log Total Population" = did_results_event_study_conley[["asinh_pop_total_treated"]],
  "Log Private Population" = did_results_event_study_conley[["asinh_private_population_estimate_treated"]],
  "Log Housing Units" = did_results_event_study_conley[["ln_total_units_treated"]],
  "Log Median Rent" = did_results_event_study_conley[["asinh_median_rent_calculated_treated"]]
)

modelsummary(
  first_stage_models,
  stars = TRUE,
  fmt = 2, # Round to 2 decimal places
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",     # Omits unneeded goodness-of-fit statistics
  title = "",
  output = here(results_dir, "tables", "first_stage_models_twfe.tex")
) 

# Neighborhood composition in treated tracts
neighborhood_comp_treated_models <- list(
  "LDI" = did_results_event_study_conley[["local_dissimilarity_index_treated"]],
  "LDI (std)" = did_results_event_study_conley[["local_dissimilarity_index_std_treated"]],
  "Log Black Pop." = did_results_event_study_conley[["asinh_pop_black_treated"]],
  "Log White Pop." = did_results_event_study_conley[["asinh_pop_white_treated"]],
  "Black Share" = did_results_event_study_conley[["black_share_treated"]],
  "Log Median Income" = did_results_event_study_conley[["asinh_median_income_treated"]],
  "HS Grad Rate" = did_results_event_study_conley[["pct_hs_grad_treated"]],
  "LFP Rate" = did_results_event_study_conley[["lfp_rate_treated"]],
  "Unemp. Rate" = did_results_event_study_conley[["unemp_rate_treated"]]
)

modelsummary(
  neighborhood_comp_treated_models,
  stars = TRUE,
  fmt = 2, # Round to 2 decimal places
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",     # Omits unneeded goodness-of-fit statistics
  title = "",
  output = here(results_dir, "tables", "neighborhood_comp_treated_models_twfe.tex")
)

### Neighboring tracts -----
# housing, pop and units

housing_and_pop_inner_models <-
  list(
    "Log Total Pop." = did_results_event_study_conley[["asinh_pop_total_inner"]],
    "Log Housing Units" = did_results_event_study_conley[["ln_total_units_inner"]],
    "Log Median Rent" = did_results_event_study_conley[["asinh_median_rent_calculated_inner"]],
    "Median Rent" = did_results_event_study_conley[["median_rent_calculated_inner"]],
    "Log Median Home Value" = did_results_event_study_conley[["asinh_median_home_value_calculated_inner"]],
    "Median Home Value" = did_results_event_study_conley[["median_home_value_calculated_inner"]]
  )

modelsummary(
  housing_and_pop_inner_models,
  stars = TRUE,
  fmt = 2, # Round to 2 decimal places
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",     # Omits unneeded goodness-of-fit statistics
  title = "",
  output = here(results_dir, "tables", "housing_and_pop_inner_models_twfe.tex")
)


# Neighborhood composition
neighborhood_comp_inner_models <-
  list(
    "LDI" = did_results_event_study_conley[["local_dissimilarity_index_inner"]],
    "LDI (standard)" = did_results_event_study_conley[["local_dissimilarity_index_std_inner"]],
    "Log Black Population" = did_results_event_study_conley[["asinh_pop_black_inner"]],
    "Log White Population" = did_results_event_study_conley[["asinh_pop_white_inner"]],
    "Black Share" = did_results_event_study_conley[["black_share_inner"]],
    "Log Median Income" = did_results_event_study_conley[["asinh_median_income_inner"]],
    "HS Grad. Rate" = did_results_event_study_conley[["pct_hs_grad_inner"]],
    "LFP Rate" = did_results_event_study_conley[["lfp_rate_inner"]],
    "Unemp. Rate" = did_results_event_study_conley[["unemp_rate_inner"]]
  )

modelsummary(
  neighborhood_comp_inner_models,
  stars = TRUE,
  fmt = 2, # Round to 2 decimal places
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",     # Omits unneeded goodness-of-fit statistics
  title = "",
  output = here(results_dir, "tables", "neighborhood_comp_inner_models_twfe.tex")
)



# Run pooled DiD -----
tract_data_matched_2_year <- tract_data_matched_2_year %>%
  group_by(match_group) %>%
  # define event time for both treated and control
  mutate(
    group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
    event_time = year - group_treatment_year) %>% 
  ungroup()



matched_did_pre_post <- function(input_data, outcome_var, treatment_group,
                         size = NULL, city_filter = NULL, initial_share = NULL) {
  
  data <- input_data %>%
    filter(!is.na(match_group),
           group_type == treatment_group)
  
  if (!is.null(size)) data <- data %>% filter(size_group == size)
  if (!is.null(city_filter)) data <- data %>% filter(city == city_filter)
  if (!is.null(initial_share)) data <- data %>% filter(race_group == initial_share)
  
  data <- data %>%
    group_by(match_group) %>%
    mutate(
      group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
      event_time = year - group_treatment_year,
      treated = ifelse(location_type == treatment_group, 1, 0),
      post = ifelse(event_time >= 0, 1, 0)
    ) %>%
    ungroup() %>%
    filter(event_time > -40, event_time < 50, event_time > -30)
  
  # Basic DiD interaction model: post × treated
  formula <- as.formula(paste0(
    outcome_var, " ~ i(treated, post, ref = c(0,0)) | GISJOIN_1950 + match_group^year"
  ))
  
  model <- feols(formula, data = data, weights = ~weights, cluster = ~GISJOIN_1950)
  
  model_conley <- feols(formula, data = data, weights = ~weights,
                        vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 2))
  
  formula_nomatch <- as.formula(paste0(
    outcome_var, " ~ i(treated, post, ref = c(0,0)) | GISJOIN_1950"
  ))
  
  model_nomatch_conley <- feols(formula_nomatch, data = data, weights = ~weights,
                                vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 2))
  
  return(list(pre_post = model, pre_post_conley = model_conley,
              pre_post_nomatch_conley = model_nomatch_conley))
}


results_pre_post <- list()

for (outcome in outcome_variables) {
  for (group in group_types) {
    cat("Pre/post DiD for", outcome, "|", group, "\n")
    
    results <- matched_did_pre_post(
      input_data = tract_data_matched_2_year,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_pre_post[[paste0(outcome, "_", group)]] <- results$pre_post_conley
  }
}


# Standard heterogeneity analysis in pooled DiD-----

## Heterogeneity by t=0 project characteristics -----
# post-period total PH units
ph_characteristics_t0 <- 
  tract_data_matched_2_year %>%
  filter(event_time == 0, group_type == "treated",
         location_type == "treated") %>% 
  # calculate project racial share
  mutate(ph_black_share_t0 = black_public_housing_pop_estimate/total_public_housing_pop_estimate) %>% 
  select(match_group, total_public_housing_units, ph_black_share_t0) %>% 
  dplyr::rename(total_public_housing_units_t0 = total_public_housing_units) %>%
  mutate(log_total_public_housing_units_t0 = log(total_public_housing_units_t0),
         ph_units_t0_bin = ntile(total_public_housing_units_t0, 3),
         ph_units_t0_bin = factor(ph_units_t0_bin),
         # Manual binning of black share in the projects
           ph_black_share_t0_cat = case_when(
             ph_black_share_t0 == 0 ~ "None",
             ph_black_share_t0 < 0.5 ~ "Some",
             TRUE ~ "Majority"
           ) %>% factor(levels = c("None", "Some", "Majority"))
         ) 
         
  
tract_data_matched_2_year <- 
  left_join(tract_data_matched_2_year, ph_characteristics_t0, by = "match_group")


# Triple interaction between treated, post, and size_group (or other dimension of interest)
run_heterogeneity_did_ph <- function(input_data, outcome_var, treatment_group, het_var) {
  
  # Create a local variable for the bin column to use in formula
  het_var_sym <- rlang::ensym(het_var)
  het_var_chr <- rlang::as_string(het_var_sym)
  
  
  data <- input_data %>%
    filter(!is.na(match_group),
           group_type == treatment_group,
           !is.na(!!het_var_sym)) %>%
    group_by(match_group) %>%
    mutate(
      group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
      event_time = year - group_treatment_year,
      treated = ifelse(location_type == treatment_group, 1, 0),
      post = ifelse(event_time >= 0, 1, 0)
    ) %>%
    ungroup() %>%
    filter(event_time > -40, event_time < 50, event_time > -30)
  
  # Triple interaction: treated x post x log_total_units
  formula <- as.formula(paste0(
    outcome_var, 
    " ~ treated * post * ", het_var, " | GISJOIN_1950 + match_group^year"
  ))
  
  model <- feols(formula, data = data, weights = ~weights, 
                 vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 2))
  
  return(model)
}

results_het_logunits <- list()
for (outcome in outcome_variables) {
  for (group in "treated") {
    cat("Heterogeneity DiD for", outcome, "|", group, "\n")
    
    results <- run_heterogeneity_did_ph(
      input_data = tract_data_matched_2_year,
      outcome_var = outcome,
      treatment_group = group,
      het_var = "log_total_public_housing_units_t0"
    )
    
    results_het_logunits[[paste0(outcome, "_", group)]] <- results
  }
}



results_het_logunits_bin <- list()

for (outcome in outcome_variables) {
  for (group in "treated") {
    cat("Heterogeneity DiD for", outcome, "|", group, "\n")
    
    results <- run_heterogeneity_did_ph(
      input_data = tract_data_matched_2_year,
      outcome_var = outcome,
      treatment_group = group,
      het_var = "ph_units_t0_bin"
    )
    
    results_het_logunits_bin[[paste0(outcome, "_", group)]] <- results
  }
}

results_ph_black_share <- list()

for (outcome in outcome_variables) {
  for (group in "treated") {
    cat("Heterogeneity DiD for", outcome, "|", group, "\n")
    
    results <- run_heterogeneity_did_ph(
      input_data = tract_data_matched_2_year,
      outcome_var = outcome,
      treatment_group = group,
      het_var = "ph_black_share_t0"
    )
    
    results_ph_black_share[[paste0(outcome, "_", group)]] <- results
  }
}

results_ph_black_share_bin <- list()

for (outcome in outcome_variables) {
  for (group in "treated") {
    cat("Heterogeneity DiD for", outcome, "|", group, "\n")
    
    results <- run_heterogeneity_did_ph(
      input_data = tract_data_matched_2_year,
      outcome_var = outcome,
      treatment_group = group,
      het_var = "ph_black_share_t0_cat" 
    )
    
    results_ph_black_share_bin[[paste0(outcome, "_", group)]] <- results
  }
}

### Create table -----
# variables: 
# dissimilarity 
# 


## Heterogeneity by t=-10 neighborhood characteristics -----
tract_data_matched_2_year <-
  tract_data_matched_2_year %>% 
  select(-contains("_tminus"))

run_heterogeneity_did <- function(input_data,
                                  outcome_var,
                                  treatment_group = "treated",
                                  het_var,
                                  het_var_time = -10,
                                  use_bspline = FALSE,
                                  bs_df = 3,
                                  event_time_min = -30,
                                  event_time_max = 50) {
  
  # Pull the initial value of the heterogeneity variable
  het_var_tminus <- paste0(het_var, "_tminus10")
  
  initial_het <- input_data %>%
    filter(event_time == het_var_time,
           group_type == treatment_group,
           location_type == treatment_group) %>%
    select(match_group, !!sym(het_var)) %>%
    rename(!!het_var_tminus := !!sym(het_var))
  
  # Merge early, before filtering
  data <- input_data %>%
    left_join(initial_het, by = "match_group") %>%
    group_by(match_group) %>%
    mutate(
      group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
      event_time = year - group_treatment_year,
      treated = ifelse(location_type == treatment_group, 1, 0),
      post = ifelse(event_time >= 0, 1, 0)
    ) %>%
    ungroup() %>%
    filter(!is.na(match_group),
           group_type == treatment_group,
           !is.na(!!sym(het_var_tminus)),
           event_time > event_time_min, event_time < event_time_max)  
  # Construct formula
  het_term <- if (use_bspline) {
    paste0("bs(", het_var_tminus, ", df = ", bs_df, ")")
  } else {
    het_var_tminus
  }
  
  formula <- as.formula(paste0(
    outcome_var, " ~ treated * post * ", het_term,
    " | GISJOIN_1950 + match_group^year"
  ))
  
  model <- feols(
    formula,
    data = data,
    weights = ~weights,
    vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 2)
  )
  
  return(model)
}

# Define your heterogeneity variables
heterogeneity_specs <- list(
  total_public_housing_units = list(use_bspline = TRUE),
  black_share = list(use_bspline = TRUE),
  local_dissimilarity_index = 
  asinh_pop_white = list(use_bspline = TRUE),
  asinh_median_income = list(use_bspline = FALSE),
  pct_hs_grad = list(use_bspline = FALSE),
  unemp_rate = list(use_bspline = FALSE)
)


# Alternative
run_heterogeneity_did <- function(input_data,
                                  outcome_var,
                                  treatment_group = "treated",
                                  het_var,
                                  het_var_time = -10,
                                  use_bspline = FALSE,
                                  binning = FALSE,
                                  binning_ntile = NULL,
                                  bs_df = 3,
                                  event_time_min = -30,
                                  event_time_max = 50) {
  
  het_var_tminus <- paste0(het_var, "_tminus10")
  
  initial_het <- input_data %>%
    filter(event_time == het_var_time,
           group_type == treatment_group,
           location_type == treatment_group) %>%
    select(match_group, !!sym(het_var)) %>%
    rename(!!het_var_tminus := !!sym(het_var))
  
  data <- input_data %>%
    left_join(initial_het, by = "match_group") %>%
    group_by(match_group) %>%
    mutate(
      group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
      event_time = year - group_treatment_year,
      treated = as.integer(location_type == treatment_group),
      post = as.integer(event_time >= 0)
    ) %>%
    ungroup() %>%
    filter(!is.na(match_group),
           group_type == treatment_group,
           !is.na(!!sym(het_var_tminus)),
           event_time > event_time_min,
           event_time < event_time_max)
  
  # Use ntile binning
  if (!is.null(binning_ntile)) {
    bin_var <- paste0(het_var_tminus, "_bin")
    data <- data %>%
      mutate(!!bin_var := ntile(!!sym(het_var_tminus), binning_ntile))
    het_term <- paste0("factor(", bin_var, ")")
  } else if (use_bspline) {
    het_term <- paste0("bs(", het_var_tminus, ", df = ", bs_df, ")")
  } else if (binning) {
    stop("You set binning = TRUE but didn't provide binning_ntile.")
  } else {
    het_term <- het_var_tminus
  }
  
  formula <- as.formula(paste0(
    outcome_var, " ~ treated * post * ", het_term,
    " | GISJOIN_1950 + match_group^year"
  ))
  
  model <- feols(
    formula,
    data = data,
    weights = ~weights,
    vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 2)
  )
  
  return(model)
}

heterogeneity_specs <- list(
  total_public_housing_units = list(binning_ntile = 3, use_bspline = TRUE),
  black_share = list(binning_ntile = 3),
  local_dissimilarity_index = list(binning_ntile = 3),
  asinh_pop_white = list(binning_ntile = 3),
  asinh_median_income = list(binning_ntile = 3),  
  pct_hs_grad = list(binning_ntile = 3),
  unemp_rate = list(binning_ntile = 3)
)



results_heterogeneity <- list()

for (het_var in names(heterogeneity_specs)) {
  spec <- heterogeneity_specs[[het_var]]
  
  for (outcome in outcome_variables) {
    results_heterogeneity[[paste0(outcome, "_het_by_initial_", het_var)]] <-
      run_heterogeneity_did(
        input_data = tract_data_matched_2_year,
        outcome_var = outcome,
        treatment_group = "treated",
        het_var = het_var,
        use_bspline = spec$use_bspline %||% FALSE,
        binning_ntile = spec$binning_ntile %||% NULL
      )
  }
}

models_black_share <- results_heterogeneity[grepl("_het_by_initial_black_share", names(results_heterogeneity))]
models_median_income <- results_heterogeneity[grepl("_het_by_initial_asinh_median_income", names(results_heterogeneity))]
models_unemp_rate <- results_heterogeneity[grepl("_het_by_initial_unemp_rate", names(results_heterogeneity))]
models_dissimilarity <- results_heterogeneity[grepl("_het_by_initial_local_dissimilarity_index", names(results_heterogeneity))]


# Individual Treatment effects ---------
## Project-specific DiD ----- 

estimate_project_did <- function(data, outcome_var) {
  # # testing 
  # data <- tract_data_matched_2_year
  # outcome_var <- "black_share"
  
  results <- list()
  match_groups <- unique(data$match_group)
  
  for (group in match_groups) {
    # !! Testing
    # group <- "New York-Northern New Jersey-Long Island, NY-NJ-PA_1980_29"

    
    df <- data %>% 
      filter(match_group == group) %>% 
      mutate(
        event_time = year - matched_treatment_year,
        treated = as.integer(location_type == "treated"),
        post = as.integer(event_time >= 0)
      )
    
    if (n_distinct(df$treated) < 2) next  # skip groups with only treated or only controls
    
    # Exclude GISJOIN FE, year FE, and do not cluster in these 2 unit regressions
    formula <- as.formula(paste0(outcome_var, " ~ treated * post"))
    
    model <- tryCatch({
      feols(formula, data = df, vcov = "hetero")
    }, error = function(e) return(NULL))
    
    if (!is.null(model)) {
      coef <- coef(model)["treated:post"]
      se <- se(model)["treated:post"]
      results[[group]] <- tibble(
        match_group = group,
        estimate = coef,
        std_error = se,
        outcome = outcome_var
      )
    }
  }
  
  bind_rows(results)
}

project_het_effects <- map_dfr(outcome_variables, function(outcome) {
  estimate_project_did(tract_data_matched_2_year, outcome)
})


## E
# pre-period characteristics
pre_period_treated_characteristics <- 
  tract_data_matched_2_year %>%
  filter(event_time == -10, group_type == "treated",
         location_type == "treated") %>% 
  # select characteristics of interest
  select(
    match_group,
    black_share,
    asinh_pop_white, 
    asinh_pop_total, 
    asinh_median_income,
    pct_hs_grad,
    unemp_rate,
    median_rent_calculated,
    asinh_median_rent_calculated,
    asinh_median_home_value_calculated, 
    vacancy_rate,
    asinh_distance_from_cbd,
    redlined_binary_80pp
  )

  

# merge on to project_het_effects by match_group
project_het_effects_merged <- 
  left_join(project_het_effects, pre_period_treated_characteristics, by = "match_group") %>% 
  left_join(ph_characteristics_t0, by = "match_group")


# run meta model following Korb (2024)
# black_share
hist(project_het_effects_merged %>% filter(outcome == "black_share") %>% pull(estimate))

meta_model <- lm(
  estimate ~ black_share + asinh_pop_total + asinh_pop_white +
    median_rent_calculated + asinh_median_home_value_calculated +
    asinh_distance_from_cbd + pct_hs_grad+ redlined_binary_80pp + log_total_public_housing_units_t0 + unemp_rate,
  data = project_het_effects_merged %>% filter(outcome == "black_share"),
  weights = 1 / (std_error^2)  # precision weights
)
meta_model %>% summary()





## Korb (2024) ML method -----
library(tidymodels)
library(rsample)
library(glmnet)
library(ranger)       # for random forest
library(rpart)        # for CART
library(baguette)     # for bagged trees
library(vip)          # for variable importance later


df <-  project_het_effects_merged %>%
  mutate(
    log_total_units = log(total_public_housing_units),
  ) %>%
  select(
    outcome, # outcome variable
    estimate,                     # outcome
    black_share,
    asinh_pop_total,
    asinh_pop_white,
    median_rent_calculated,
    asinh_median_home_value_calculated,
    asinh_distance_from_cbd,
    pct_hs_grad,
    unemp_rate, 
    redlined_binary_80pp,
    log_total_units,
    asinh_median_income
  ) %>%
  drop_na()



compare_models_for_outcome <- function(df, outcome_var, standardize_outcome = FALSE, seed = 123) {
  

  set.seed(seed)

  ### Step  1. split data into training and testing sets ----
  # Randomly split data into 75% training data, 25% testing data
  # Note: Test set is created but not used. All model selection is based on CV within training data.
  df_filtered <-
    df %>% 
    filter(outcome == outcome_var)
    
  split <- initial_split(df_filtered, prop = 0.75)
  train <- training(split)
  test <- testing(split)
  
  # --- Step 2: Preprocessing recipe ---
  predictors <- df_filtered %>%
    select(-estimate, -outcome) %>%
    names()
  
  recipe_spec <- recipe(estimate ~ ., data = train) %>%
    step_rm(outcome) %>%  # drop the outcome label column, not a predictor
    step_normalize(all_numeric_predictors())    # standardize all predictors 

  
  # standardize the outcome if specified
  if (standardize_outcome) {
    recipe_spec <- recipe_spec %>% step_normalize(all_outcomes())
  }
   

  # --- Step 3: Model specifications ---
  models <- list(
    lasso = linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet"),
    ridge = linear_reg(penalty = tune(), mixture = 0) %>% set_engine("glmnet"),
    enet  = linear_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet"),
    cart  = decision_tree(cost_complexity = tune(), tree_depth = tune()) %>% set_engine("rpart") %>% set_mode("regression"),
    rf    = rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% set_engine("ranger", importance = "permutation") %>% set_mode("regression"),
    bagged = bag_tree() %>% set_engine("rpart", times = 25) %>% set_mode("regression"),
    xgb   = boost_tree(
      trees = 1000,
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      mtry = tune()
    ) %>% set_engine("xgboost") %>% set_mode("regression")
  )
  
  
  ### Step 4: Set up cross-validation -----
  # Create 5 fold cross-validation folds from training data
  # Model is trained 5 times 
  folds <- vfold_cv(train, v = 5)


### Step 5: Create workflows and tuning grids for each model ----
  # define grids
  grids <- list(
    lasso = grid_regular(penalty(range = c(-4, 0)), levels = 25),
    ridge = grid_regular(penalty(range = c(-4, 0)), levels = 25),
    enet  = grid_regular(penalty(range = c(-4, 0)), mixture(range = c(0, 1)), levels = 5),
    cart  = grid_regular(cost_complexity(range = c(-3, -1)), tree_depth(range = c(2, 10)), levels = 5),
    rf    = grid_regular(mtry(range = c(2, min(8, length(predictors)))), min_n(range = c(2, 10)), levels = 5),
    xgb   = grid_latin_hypercube(
      tree_depth(range = c(2, 10)),
      learn_rate(range = c(0.01, 0.3)),
      loss_reduction(),
      sample_size = sample_prop(),
      finalize(mtry(), train),
      size = 25
    )
  )
  
  # tune models
  results <- list()
  
  for (model_name in names(models)) {
    wf <- workflow() %>%
      add_model(models[[model_name]]) %>%
      add_recipe(recipe_spec)
    
    message("Tuning model: ", model_name)
    
    if (model_name == "bagged") {
      res <- fit_resamples(wf, resamples = folds, metrics = metric_set(rmse))
    } else {
      res <- tune_grid(wf,
                       resamples = folds,
                       grid = grids[[model_name]],
                       metrics = metric_set(rmse),
                       control = control_grid(save_pred = TRUE))
    }
    
    results[[model_name]] <- res
  }
  
  

  # --- Step 6: Compare RMSEs ---
  rmses <- bind_rows(
    lapply(names(results), function(name) {
      collect_metrics(results[[name]]) %>%
        filter(.metric == "rmse") %>%
        mutate(model = name)
    })
  ) %>%
    group_by(model) %>%
    slice_min(mean, with_ties = FALSE) %>%
    arrange(mean)
  
  return(list(
    rmse_table = rmses,
    model_results = results,
    split = split,
    recipe = recipe_spec,
    train = train,
    test = test
  ))
}


library(broom)
library(vip)

# TODO: (5/2025): Xgboost and bagged CART dont currently work

get_final_fit_and_importance <- function(results, outcome_var) {
  
  
  df_filtered <-
    df %>% 
    filter(outcome == outcome_var)
  
  
  best_model <- results$rmse_table$model[1]
  best_result <- results$model_results[[best_model]]
  best_params <- select_best(best_result, metric = "rmse")
  
  models <- list(
    lasso = linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet"),
    ridge = linear_reg(penalty = tune(), mixture = 0) %>% set_engine("glmnet"),
    enet  = linear_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet"),
    cart  = decision_tree(cost_complexity = tune(), tree_depth = tune()) %>% 
      set_engine("rpart") %>% set_mode("regression"),
    rf    = rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
      set_engine("ranger", importance = "permutation") %>% set_mode("regression"),
    bagged = bag_tree() %>% set_engine("rpart", times = 25) %>% set_mode("regression"),
    xgb   = boost_tree(
      trees = 1000, tree_depth = tune(), learn_rate = tune(),
      loss_reduction = tune(), sample_size = tune(), mtry = tune()
    ) %>% set_engine("xgboost") %>% set_mode("regression")
  )
  
  final_model <- finalize_model(models[[best_model]], best_params)
  final_wf <- workflow() %>%
    add_model(final_model) %>%
    add_recipe(results$recipe)
  
  final_fit <- fit(final_wf, data = df_filtered)
  
  # Get importance depending on model type
  if (best_model %in% c("ridge", "lasso", "enet")) {
    importance <- tidy(final_fit) %>%
      filter(term != "(Intercept)") %>%
      mutate(importance = abs(estimate)) %>%
      arrange(desc(importance)) %>%
      select(term, estimate, importance)
  } else {
    # Use vip for tree-based models
    importance <- vi(extract_fit_parsnip(final_fit)) %>%
      arrange(desc(Importance)) %>%
      rename(term = Variable, importance = Importance)
  }
  
  return(list(
    best_model = best_model,
    final_fit = final_fit,
    importance = importance
  ))
}

results_black_share <- compare_models_for_outcome(df, outcome_var = "black_share")
results_asinh_pop_black <- compare_models_for_outcome(df, outcome_var = "asinh_pop_black")
results_asinh_pop_white <- compare_models_for_outcome(df, outcome_var = "asinh_pop_white")
results_median_rent <- compare_models_for_outcome(df, outcome_var = "median_rent_calculated")
results_pct_hs_grad <- compare_models_for_outcome(df, outcome_var = "pct_hs_grad")

out_black_share <- get_final_fit_and_importance(results_black_share, "black_share") 
out_asinh_pop_black <- get_final_fit_and_importance(results_asinh_pop_black, "asinh_pop_black")
out_asinh_pop_white <- get_final_fit_and_importance(results_asinh_pop_white, "asinh_pop_white")
out_median_rent <- get_final_fit_and_importance(results_median_rent, "median_rent_calculated")
out_pct_hs_grad <- get_final_fit_and_importance(results_pct_hs_grad, "pct_hs_grad")
