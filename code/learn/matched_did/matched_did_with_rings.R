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
outcome_variables <- c("asinh_private_population_estimate", "private_population_estimate",
                       "black_share", "white_share",
                       "total_pop", "black_pop", "white_pop",
                       "asinh_pop_total", "asinh_pop_black",
                       "asinh_pop_white",
                       "median_income", "asinh_median_income",
                       "median_home_value_calculated",
                       "asinh_median_home_value_calculated",
                       "median_rent_calculated",
                       "asinh_median_rent_calculated",
                       "population_density", 
                       "total_units",
                       "ln_total_units",
                       "vacancy_rate", 
                       "lfp_rate", "unemp_rate", 
                       "pct_hs_grad")
# Create a lookup table (named vector) for clean labels
outcome_labels <- c(
  "asinh_private_population_estimate" = "Log Private Population",
  "private_population_estimate" = "Private Population",
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
  "population_density" = "Population Density",
  "total_units" = "Total Housing Units",
  "ln_total_units" = "Log Housing Units",
  "vacancy_rate" = "Vacancy Rate",
  "lfp_rate" = "Labor Force Participation Rate",
  "unemp_rate" = "Unemployment Rate",
  "pct_hs_grad" = "HS Graduation Rate")


group_types <- c("treated", "inner")


# Baseline analysis ----

## Full sample -----
# initialize lists
did_results_event_study_twfe <- list() 
did_results_event_study_sunab <- list() 
did_results_event_study_sunab_no_match <- list()

# with conley standard errors
did_results_event_study_twfe_conley <- list()
did_results_event_study_sunab_conley <- list()
did_results_event_study_sunab_no_match_conley <- list()

# save regression output to lists
for (outcome in outcome_variables) {
  for (group in  group_types) {
  
    # display which outcome and group is being worked on
    print(paste("Outcome:", outcome, "Group:", group))
    
    results <- did_event_study(tract_data_matched_2_year,
                               outcome, group)
    did_results_event_study_twfe[[paste(outcome, group, sep = "_")]] <- results$twfe
    did_results_event_study_sunab[[paste(outcome, group, sep = "_")]] <- results$sunab
    did_results_event_study_sunab_no_match[[paste(outcome, group, sep = "_")]] <- results$sunab_no_match
    
    # with conley standard errors
    did_results_event_study_twfe_conley[[paste(outcome, group, sep = "_")]] <- results$twfe_conley
    did_results_event_study_sunab_conley[[paste(outcome, group, sep = "_")]] <- results$sunab_conley
    did_results_event_study_sunab_no_match_conley[[paste(outcome, group, sep = "_")]] <- results$sunab_no_match_conley
  }
}

# save results in df
event_study_twfe_coefs <- save_estimates(did_results_event_study_twfe, "TWFE")
event_study_sunab_coefs <- save_estimates(did_results_event_study_sunab, "Sun-Abraham")
event_study_sunab_no_match_coefs <- save_estimates(did_results_event_study_sunab_no_match, "Sun-Abraham without matching")

event_study_twfe_conley_coefs <- save_estimates(did_results_event_study_twfe_conley, "TWFE")
event_study_sunab_conley_coefs <- save_estimates(did_results_event_study_sunab_conley, "Sun-Abraham")
event_study_sunab_no_match_conley_coefs <- save_estimates(did_results_event_study_sunab_no_match_conley, "Sun-Abraham without matching")

combined_results <- 
  bind_rows(event_study_twfe_coefs, event_study_sunab_coefs,
            event_study_sunab_no_match_coefs)

combined_results_conley <- 
  bind_rows(event_study_twfe_conley_coefs, event_study_sunab_conley_coefs,
            event_study_sunab_no_match_conley_coefs)

# look at significant pretrends
#combined_results %>% filter(str_detect(term, "-20"), p.value < .05) %>% View()

# look at significant post trends
#combined_results %>% filter(p.value < .05) %>% View()

## Plot results -----

# Save baseline event study plots

save_event_study_plots(
  reg_results_df = event_study_sunab_coefs,
  outcome_variables = outcome_variables,
  results_dir = results_dir,
  prefix = "event_study_plots_baseline_sunab_matching",
  title_suffix = " Sun-Abraham estimator",
  heterogeneity = NULL
)

save_event_study_plots(
  reg_results_df = event_study_twfe_coefs,
  outcome_variables = outcome_variables,
  results_dir = results_dir,
  prefix = "event_study_plots_baseline_twfe_matching",
  title_suffix = "",
  heterogeneity = NULL
)

save_event_study_plots(
  reg_results_df = event_study_sunab_no_match_coefs,
  outcome_variables = outcome_variables,
  results_dir = results_dir,
  prefix = "event_study_plots_baseline_sunab_no_match",
  title_suffix = ", no match FE",
  heterogeneity = NULL
)

# Save baseline results separately for treated and inner tracts

save_event_study_plots(
  reg_results_df = event_study_twfe_coefs,
  outcome_variables = outcome_variables,
  results_dir = results_dir,
  prefix = "event_study_plots_baseline_twfe_matching_treated",
  title_suffix = " (Treated)",
  heterogeneity = NULL,
  category_filter = "treated"
)

save_event_study_plots(
  reg_results_df = event_study_twfe_coefs,
  outcome_variables = outcome_variables,
  results_dir = results_dir,
  prefix = "event_study_plots_baseline_twfe_matching_inner",
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
  event_study_twfe_conley_coefs %>% 
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
  reg_results_df = event_study_twfe_conley_coefs, 
  outcome_variables = outcome_variables, 
  results_dir = here(results_dir, "individual_plots"), 
  prefix = "event_study_plots_twfe"
)

## Create tables of results  -----
# Filter combined_results
results_with_se <- combined_results_conley %>%
  dplyr::rename(event_time = term) %>% 
  filter(estimator == "TWFE") %>% 
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
  "Log Total Population" = did_results_event_study_twfe_conley[["asinh_pop_total_treated"]],
  "Log Private Population" = did_results_event_study_twfe_conley[["asinh_private_population_estimate_treated"]],
  "Log Housing Units" = did_results_event_study_twfe_conley[["ln_total_units_treated"]],
  "Log Median Rent" = did_results_event_study_twfe_conley[["asinh_median_rent_calculated_treated"]]
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
  "Log Black Pop." = did_results_event_study_twfe_conley[["asinh_pop_black_treated"]],
  "Log White Pop." = did_results_event_study_twfe_conley[["asinh_pop_white_treated"]],
  "Black Share" = did_results_event_study_twfe_conley[["black_share_treated"]],
  "Log Median Income" = did_results_event_study_twfe_conley[["asinh_median_income_treated"]],
  "HS Grad Rate" = did_results_event_study_twfe_conley[["pct_hs_grad_treated"]],
  "LFP Rate" = did_results_event_study_twfe_conley[["lfp_rate_treated"]],
  "Unemp. Rate" = did_results_event_study_twfe_conley[["unemp_rate_treated"]]
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
    "Log Total Pop." = did_results_event_study_twfe_conley[["asinh_pop_total_inner"]],
    "Log Housing Units" = did_results_event_study_twfe_conley[["ln_total_units_inner"]],
    "Log Median Rent" = did_results_event_study_twfe_conley[["asinh_median_rent_calculated_inner"]],
    "Median Rent" = did_results_event_study_twfe_conley[["median_rent_calculated_inner"]],
    "Log Median Home Value" = did_results_event_study_twfe_conley[["asinh_median_home_value_calculated_inner"]],
    "Median Home Value" = did_results_event_study_twfe_conley[["median_home_value_calculated_inner"]]
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
    "Log Black Population" = did_results_event_study_twfe_conley[["asinh_pop_black_inner"]],
    "Log White Population" = did_results_event_study_twfe_conley[["asinh_pop_white_inner"]],
    "Black Share" = did_results_event_study_twfe_conley[["black_share_inner"]],
    "Log Median Income" = did_results_event_study_twfe_conley[["asinh_median_income_inner"]],
    "HS Grad. Rate" = did_results_event_study_twfe_conley[["pct_hs_grad_inner"]],
    "LFP Rate" = did_results_event_study_twfe_conley[["lfp_rate_inner"]],
    "Unemp. Rate" = did_results_event_study_twfe_conley[["unemp_rate_inner"]]
  )

modelsummary(
  neighborhood_comp_inner_models,
  stars = TRUE,
  fmt = 2, # Round to 2 decimal places
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",     # Omits unneeded goodness-of-fit statistics
  title = "",
  output = here(results_dir, "tables", "neighborhood_comp_inner_models_twfe.tex")
)



# Heterogeneity analysis ----
# TODO: Move this to another program...
## By Project size -----

number_of_size_groups <- 2

### Treated  -----

# Get only the treated + their donor pool
tract_data_matched_2_year_treated <- 
  tract_data_matched_2_year %>%
  filter(group_type == "treated")

# Get the size group for the treated units
size_groups <- tract_data_matched_2_year_treated %>% 
  select(match_group, total_public_housing_units) %>% 
  filter(!is.na(total_public_housing_units)) %>% 
  distinct() %>% 
  # assign quartiles
  mutate(size_group = ntile(total_public_housing_units, number_of_size_groups)) %>%
  # mutate(size_group = case_when(
  #   total_public_housing_units < 100 ~ 1,
  #   total_public_housing_units < 500 ~ 2,
  #   TRUE ~ 3
  # )) %>% 
select(-total_public_housing_units)


# Merge onto the treated units
tract_data_matched_with_size_treated <- 
  tract_data_matched_2_year_treated %>% 
  left_join(size_groups, by = "match_group")

# get cutoffs for size groups
size_group_cutoffs_treated <- 
  tract_data_matched_with_size_treated %>% 
  select(size_group, total_public_housing_units) %>%
  filter(!is.na(total_public_housing_units)) %>%
  group_by(size_group) %>% 
  summarise(min = min(total_public_housing_units),
            max = max(total_public_housing_units))

# Initialize lists
did_results_event_study_twfe_size_treated <- list()
did_results_event_study_sunab_size_treated <- list()
did_results_event_study_sunab_no_match_size_treated <- list()

# with conley standard errors
did_results_event_study_twfe_conley <- list()
did_results_event_study_sunab_conley <-  list()
did_results_event_study_sunab_no_match_conley <- list()



# Loop over outcome variables and size categories
for (outcome in outcome_variables) {
  for (s in 1:max(tract_data_matched_with_size_treated$size_group, na.rm = TRUE)) {


      results <- did_event_study(input_data = tract_data_matched_with_size_treated,
                                 outcome_var = outcome,
                                treatment_group = "treated",
                                size = s)
      did_results_event_study_twfe_size_treated[[paste(outcome, s, sep = "_")]] <- results$twfe
      did_results_event_study_sunab_size_treated[[paste(outcome, s,  sep = "_")]] <- results$sunab
      did_results_event_study_sunab_no_match_size_treated[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
      
      did_results_event_study_twfe_conley[[paste(outcome, s, sep = "_")]] <- results$twfe_conley
      did_results_event_study_sunab_conley[[paste(outcome, s, sep = "_")]] <- results$sunab_conley
      did_results_event_study_sunab_no_match_conley[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match_conley
  }
}

# save estimates in df
event_study_twfe_size_coefs_treated <- 
  save_estimates(did_results_event_study_twfe_size_treated, "TWFE")
event_study_sunab_size_coefs_treated <- 
  save_estimates(did_results_event_study_sunab_size_treated, "Sun-Abraham")
event_study_sunab_no_match_size_coefs_treated <-
  save_estimates(did_results_event_study_sunab_no_match_size_treated,
                                                                "Sun-Abraham without matching")

# conley 
event_study_twfe_size_conley_coefs_treated <- 
  save_estimates(did_results_event_study_twfe_conley, "TWFE")
event_study_sunab_size_conley_coefs_treated <-
  save_estimates(did_results_event_study_sunab_conley, "Sun-Abraham")
event_study_sunab_no_match_size_conley_coefs_treated <-
  save_estimates(did_results_event_study_sunab_no_match_conley, "Sun-Abraham without matching")


# combine all results
combined_size_results_treated <- 
  bind_rows(event_study_twfe_size_coefs_treated, event_study_sunab_size_coefs_treated,
            event_study_sunab_no_match_size_coefs_treated)

combined_size_results_treated_conley <- 
  bind_rows(event_study_twfe_size_conley_coefs_treated, event_study_sunab_size_conley_coefs_treated,
            event_study_sunab_no_match_size_conley_coefs_treated)

# check pretrends
#combined_size_results %>% filter(str_detect(term, "-20"),  p.value < .05) %>% View()


### Inner ring: -----
# read in inner ring -> treatment size correspondence
# This is created in create_spatial_did_data.R, and defines 
# the number of public housing units near each inner ring tract at first 
# time it's treated

inner_ring_tracts_first_treated <- 
  read_csv(here(merged_data_dir, "inner_ring_tracts_first_treated.csv")) %>% 
  select(-treatment_year)

tract_data_matched_2_year_inner <- 
  tract_data_matched_2_year %>%
  filter(group_type == "inner") 

# 
tract_data_matched_with_size_inner <- 
  tract_data_matched_2_year_inner %>% 
  left_join(inner_ring_tracts_first_treated) 


# Initialize lists
size_groups_inner <- 
  tract_data_matched_with_size_inner %>% 
  select(match_group, total_public_housing_units_built_nearby) %>% 
  filter(!is.na(total_public_housing_units_built_nearby)) %>% 
  distinct() %>% 
  # assign quartiles
   mutate(size_group = ntile(total_public_housing_units_built_nearby, number_of_size_groups)) %>% 
  # mutate(size_group = case_when(
  #   total_public_housing_units_built_nearby < 100 ~ 1,
  #   total_public_housing_units_built_nearby < 500 ~ 2,
  #   TRUE ~ 3
  # )) %>% 
  select(-total_public_housing_units_built_nearby)

# Merge onto size groups
tract_data_matched_with_size_inner <- 
  tract_data_matched_with_size_inner %>% 
  left_join(size_groups_inner, by = "match_group")

# Get cutoffs for size groups
size_group_cutoffs_inner <- 
  tract_data_matched_with_size_inner %>% 
  select(size_group, total_public_housing_units_built_nearby) %>%
  filter(!is.na(total_public_housing_units_built_nearby)) %>%
  group_by(size_group) %>% 
  summarise(min = min(total_public_housing_units_built_nearby),
            max = max(total_public_housing_units_built_nearby))

# Initialize lists
did_results_event_study_twfe_size_inner <- list()
did_results_event_study_sunab_size_inner <- list()
did_results_event_study_sunab_no_match_size_inner <- list()

# Loop over outcome variables and size categories
for (outcome in outcome_variables) {
  for (s in 1:max(tract_data_matched_with_size_inner$size_group, na.rm = TRUE)) {
    
    results <- did_event_study(input_data = tract_data_matched_with_size_inner,
                               outcome_var = outcome,
                               treatment_group = "inner",
                               size = s)
    did_results_event_study_twfe_size_inner[[paste(outcome, s, sep = "_")]] <- results$twfe
    did_results_event_study_sunab_size_inner[[paste(outcome, s,  sep = "_")]] <- results$sunab
    did_results_event_study_sunab_no_match_size_inner[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
  }
}

# save estimates in df
event_study_twfe_size_coefs_inner <- save_estimates(did_results_event_study_twfe_size_inner, "TWFE")
event_study_sunab_size_coefs_inner <- save_estimates(did_results_event_study_sunab_size_inner, "Sun-Abraham")
event_study_sunab_no_match_size_coefs_inner <- save_estimates(did_results_event_study_sunab_no_match_size_inner,
                                                                "Sun-Abraham without matching")

# combine all results
combined_size_results_inner <- 
  bind_rows(event_study_twfe_size_coefs_inner, event_study_sunab_size_coefs_inner,
            event_study_sunab_no_match_size_coefs_inner)

# check pretrends
#combined_size_results_inner %>% filter(str_detect(term, "-20"),  p.value < .05) %>% View()

### Plot results  ----

# treated 
save_event_study_plots(
  reg_results_df = event_study_twfe_size_coefs_treated,
  outcome_variables = outcome_variables,
  results_dir = results_dir,
  prefix = "event_study_plots_by_size_treated_twfe_matching",
  title_suffix = " by project size tercile, treated tracts",
  category_filter = NULL,
  heterogeneity = TRUE
)


# inner
save_event_study_plots(
  reg_results_df = event_study_twfe_size_coefs_inner,
  outcome_variables = outcome_variables,
  results_dir = results_dir,
  prefix = "event_study_plots_by_size_inner_twfe_matching",
  title_suffix = " by project size tercile, inner ring tracts",
  category_filter = NULL,
  heterogeneity = TRUE
)

#### plot each separately ----

# treated

save_individual_event_study_plots(
  reg_results_df = event_study_twfe_size_coefs_treated, 
  outcome_variables = outcome_variables, 
  results_dir = here(results_dir,"individual_plots", "by_size"), 
  prefix = "event_study_plots_by_size_treated_twfe",
  heterogeneity = TRUE
)

# inner
save_individual_event_study_plots(
  reg_results_df = event_study_twfe_size_coefs_inner, 
  outcome_variables = outcome_variables, 
  results_dir = here(results_dir,"individual_plots", "by_size"), 
  prefix = "event_study_plots_by_size_inner_twfe",
  heterogeneity = TRUE
)


### Tables ----

results_with_se_size_treated <- combined_size_results_treated %>%
  dplyr::rename(event_time = term) %>% 
  filter(estimator == "TWFE") %>% 
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

results_with_se_size_inner <- combined_size_results_inner %>%
  dplyr::rename(event_time = term) %>% 
  filter(estimator == "TWFE") %>% 
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


# housing: inner
housing_inner_models_size <-
  list(
    "Total units: Low-Exp" = summary(did_results_event_study_sunab_size_inner$total_units_1, agg ="ATT"),
    "Total units: High-Exp" = summary(did_results_event_study_sunab_size_inner$total_units_2, agg ="ATT"),
    "Log Home Value: Low-Exp" =  summary(did_results_event_study_sunab_size_inner$asinh_median_home_value_calculated_1, agg ="ATT"),
    "Log Home Value: High-Exp" =  summary(did_results_event_study_sunab_size_inner$asinh_median_home_value_calculated_2, agg ="ATT"),
    "Log Rent: Low-Exp" =  summary(did_results_event_study_sunab_size_inner$asinh_median_rent_calculated_1, agg ="ATT"),
    "Log Rent: High-Exp" =  summary(did_results_event_study_sunab_size_inner$asinh_median_rent_calculated_2, agg ="ATT")
  )


modelsummary(
  housing_inner_models_size,
  stars = TRUE,
  fmt = 2, # Round to 2 decimal places
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",     # Omits unneeded goodness-of-fit statistics
  title = "",
  output = here(results_dir, "tables", "housing_inner_models_size.tex")
)


# Neighborhood comp: inner
neighborhood_comp_inner_models_size <-
  list(
    
    "Black Share: Low-exp" = summary(did_results_event_study_sunab_size_inner$black_share_1, agg ="ATT"),
    "Black Share: High-exp" = summary(did_results_event_study_sunab_size_inner$black_share_2, agg ="ATT"),
    "Log Median Income: Low-exp" = summary(did_results_event_study_sunab_size_inner$asinh_median_income_1, agg ="ATT"),
    "Log Median Income: High-exp" = summary(did_results_event_study_sunab_size_inner$asinh_median_income_2, agg ="ATT"),
    "HS Grad. Rate: Low-exp" = summary(did_results_event_study_sunab_size_inner$pct_hs_grad_1, agg ="ATT"),
    "HS Grad. Rate: High-exp" = summary(did_results_event_study_sunab_size_inner$pct_hs_grad_2, agg ="ATT"),
    "LFP Rate: Low-exp" = summary(did_results_event_study_sunab_size_inner$lfp_rate_1, agg ="ATT"),
    "LFP Rate: High-exp" = summary(did_results_event_study_sunab_size_inner$lfp_rate_2, agg ="ATT"),
    "Unemp. Rate: Low-exp" = summary(did_results_event_study_sunab_size_inner$unemp_rate_1, agg ="ATT"),
    "Unemp. Rate: High-exp" = summary(did_results_event_study_sunab_size_inner$unemp_rate_2, agg ="ATT"))

modelsummary(
  neighborhood_comp_inner_models_size,
  stars = TRUE,
  fmt = 2, # Round to 2 decimal places
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",     # Omits unneeded goodness-of-fit statistics
  title = "",
  output = here(results_dir, "tables", "neighborhood_comp_inner_models_size.tex")
)

# 
# ## By minority share ----
# 
# # get pre-treatment black share for treated units, create bins
# initial_nonwhite_share  <- 
#   tract_data_matched_2_year %>%
#   filter(location_type == "treated" | location_type == "inner") %>%
#   mutate(event_time = year - treatment_year) %>% 
#   filter(event_time == -10) %>%
#   mutate(nonwhite_share = 1-white_share) %>% 
#   dplyr::rename(pre_treatment_nonwhite_share = nonwhite_share) %>% 
#   select(match_group, group_type, pre_treatment_nonwhite_share) %>% 
#   distinct() %>% 
#   # create bins
#   mutate(nonwhite_bin = case_when(
#     pre_treatment_nonwhite_share <= 0.05 ~ 1,
#     pre_treatment_nonwhite_share > 0.05 & pre_treatment_nonwhite_share <= .2  ~ 2,
#     pre_treatment_nonwhite_share > .2 & pre_treatment_nonwhite_share <= 0.5 ~ 3,
#     pre_treatment_nonwhite_share > 0.5 ~ 4))
# 
# # merge with matched data
# tract_data_matched_with_nonwhite_share <- 
#   tract_data_matched_2_year %>%
#   left_join(initial_nonwhite_share)
# 
# ### treated ----
# # Initialize lists
# did_results_event_study_twfe_nonwhite_treated <- list()
# did_results_event_study_sunab_nonwhite_treated <- list()
# did_results_event_study_sunab_no_match_nonwhite_treated <- list()
# 
# # Loop over outcome variables and race categories
# for (outcome in outcome_variables) {
#   for (s in 1:max(tract_data_matched_with_nonwhite_share$nonwhite_bin, na.rm = TRUE)) {
#     
#     
#     results <- did_event_study(input_data = tract_data_matched_with_nonwhite_share %>%
#                                  filter(nonwhite_bin == s),
#                                outcome_var = outcome,
#                                treatment_group = "treated")
#     did_results_event_study_twfe_nonwhite_treated[[paste(outcome, s, sep = "_")]] <- results$twfe
#     did_results_event_study_sunab_nonwhite_treated[[paste(outcome, s,  sep = "_")]] <- results$sunab
#     did_results_event_study_sunab_no_match_nonwhite_treated[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
#   }
# }
# 
# # save estimates in df
# event_study_twfe_nonwhite_coefs_treated <- save_estimates(did_results_event_study_twfe_nonwhite_treated, "TWFE")
# event_study_sunab_nonwhite_coefs_treated <- save_estimates(did_results_event_study_sunab_nonwhite_treated, "Sun-Abraham")
# event_study_sunab_no_match_nonwhite_coefs_treated <- save_estimates(did_results_event_study_sunab_no_match_nonwhite_treated,
#                                                                 "Sun-Abraham without matching")
# 
# # combine all results
# combined_nonwhite_results_treated <- 
#   bind_rows(event_study_twfe_nonwhite_coefs_treated, event_study_sunab_nonwhite_coefs_treated,
#             event_study_sunab_no_match_nonwhite_coefs_treated)
# 
# # check pretrends
# combined_nonwhite_results_treated %>% filter(str_detect(term, "-20"),  p.value < .05) %>% View()
# 
# ### inner ring -----
# 
# # Initialize lists
# did_results_event_study_twfe_nonwhite_inner <- list()
# did_results_event_study_sunab_nonwhite_inner <- list()
# did_results_event_study_sunab_no_match_nonwhite_inner <- list()
# 
# # Loop over
# for (outcome in outcome_variables) {
#   for (s in 1:max(tract_data_matched_with_nonwhite_share$nonwhite_bin, na.rm = TRUE)) {
#     
#     
#     results <- did_event_study(input_data = tract_data_matched_with_nonwhite_share %>%
#                                  filter(nonwhite_bin == s),
#                                outcome_var = outcome,
#                                treatment_group = "inner")
#     did_results_event_study_twfe_nonwhite_inner[[paste(outcome, s, sep = "_")]] <- results$twfe
#     did_results_event_study_sunab_nonwhite_inner[[paste(outcome, s,  sep = "_")]] <- results$sunab
#     did_results_event_study_sunab_no_match_nonwhite_inner[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
#   }
# }
# 
# # save estimates in df
# event_study_twfe_nonwhite_coefs_inner <- save_estimates(did_results_event_study_twfe_nonwhite_inner, "TWFE")
# event_study_sunab_nonwhite_coefs_inner <- save_estimates(did_results_event_study_sunab_nonwhite_inner, "Sun-Abraham")
# event_study_sunab_no_match_nonwhite_coefs_inner <- save_estimates(did_results_event_study_sunab_no_match_nonwhite_inner,
#                                                                 "Sun-Abraham without matching")
# 
# # combine all results
# combined_nonwhite_results_inner <- 
#   bind_rows(event_study_twfe_nonwhite_coefs_inner, event_study_sunab_nonwhite_coefs_inner,
#             event_study_sunab_no_match_nonwhite_coefs_inner)
# 
# # check pretrends
# # combined_nonwhite_results_inner %>% filter(str_detect(term, "-20"),  p.value < .05) %>% View()
# 
# ### plot results -----
# 
# # treated
# save_event_study_plots(
#   reg_results_df = event_study_twfe_nonwhite_coefs_treated,
#   outcome_variables = outcome_variables,
#   results_dir = results_dir,
#   prefix = "event_study_plots_by_minority_share_treated_twfe_matching.pdf",
#   title_suffix = " by initial minority share tercile, treated tracts",
#   category_filter = NULL,
#   heterogeneity = TRUE
# )
# 
# 
# # inner
# save_event_study_plots(
#   reg_results_df = event_study_twfe_nonwhite_coefs_inner,
#   outcome_variables = outcome_variables,
#   results_dir = results_dir,
#   prefix = "event_study_plots_by_minority_share_inner_twfe_matching.pdf",
#   title_suffix = " by initial minority share tercile, inner ring tracts",
#   category_filter = NULL,
#   heterogeneity = TRUE
# )
# 
# 
# 
# ## By initial income -----
# # get pre-treatment income for treated units, create bins
# initial_income <- 
#   tract_data_matched_2_year %>%
#   filter(location_type == "treated" | location_type == "inner") %>%
#   mutate(event_time = year - treatment_year) %>% 
#   filter(event_time == -10) %>%
#   dplyr::rename(pre_treatment_income = median_income) %>% 
#   select(match_group, group_type, pre_treatment_income) %>% 
#   distinct() %>% 
#   # create terciles
#   mutate(income_tercile = ntile(pre_treatment_income, 3)) %>% 
#   select(match_group, group_type, income_tercile)
# 
# # merge with data
# tract_data_matched_with_income_tercile <- 
#   tract_data_matched_2_year %>%
#   left_join(initial_income, by = c("match_group", "group_type"))
# 
# 
# #### Treated ----
# # Initialize lists
# did_results_event_study_twfe_income_treated <- list()
# did_results_event_study_sunab_income_treated <- list()
# did_results_event_study_sunab_no_match_income_treated <- list()
# 
# # Loop over
# for (outcome in outcome_variables) {
#   for (s in 1:max(tract_data_matched_with_income_tercile$income_tercile, na.rm = TRUE)) {
#     
#     
#     results <- did_event_study(input_data = tract_data_matched_with_income_tercile %>%
#                                  filter(income_tercile == s),
#                                outcome_var = outcome,
#                                treatment_group = "treated")
#     did_results_event_study_twfe_income_treated[[paste(outcome, s, sep = "_")]] <- results$twfe
#     did_results_event_study_sunab_income_treated[[paste(outcome, s,  sep = "_")]] <- results$sunab
#     did_results_event_study_sunab_no_match_income_treated[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
#   }
# }
# 
# # combine all results
# event_study_twfe_income_coefs_treated <- save_estimates(did_results_event_study_twfe_income_treated, "TWFE")
# event_study_sunab_income_coefs_treated <- save_estimates(did_results_event_study_sunab_income_treated, "Sun-Abraham")
# event_study_sunab_no_match_income_coefs_treated <- save_estimates(did_results_event_study_sunab_no_match_income_treated,
#                                                                 "Sun-Abraham without matching")
# 
# # combine all results
# combined_income_results_treated <- 
#   bind_rows(event_study_twfe_income_coefs_treated, event_study_sunab_income_coefs_treated,
#             event_study_sunab_no_match_income_coefs_treated)
# 
# #### Inner -----
# # Initialize lists
# did_results_event_study_twfe_income_inner <- list()
# did_results_event_study_sunab_income_inner <- list()
# did_results_event_study_sunab_no_match_income_inner <- list()
# 
# # Loop over
# for (outcome in outcome_variables) {
#   for (s in 1:max(tract_data_matched_with_income_tercile$income_tercile, na.rm = TRUE)) {
#     
#     
#     results <- did_event_study(input_data = tract_data_matched_with_income_tercile %>%
#                                  filter(income_tercile == s),
#                                outcome_var = outcome,
#                                treatment_group = "inner")
#     did_results_event_study_twfe_income_inner[[paste(outcome, s, sep = "_")]] <- results$twfe
#     did_results_event_study_sunab_income_inner[[paste(outcome, s,  sep = "_")]] <- results$sunab
#     did_results_event_study_sunab_no_match_income_inner[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
#   }
# }
# 
# # combine all results
# event_study_twfe_income_coefs_inner <- save_estimates(did_results_event_study_twfe_income_inner, "TWFE")
# event_study_sunab_income_coefs_inner <- save_estimates(did_results_event_study_sunab_income_inner, "Sun-Abraham")
# event_study_sunab_no_match_income_coefs_inner <- save_estimates(did_results_event_study_sunab_no_match_income_inner,
#                                                                 "Sun-Abraham without matching")
# 
# # combine all results
# combined_income_results_inner <- 
#   bind_rows(event_study_twfe_income_coefs_inner, event_study_sunab_income_coefs_inner,
#             event_study_sunab_no_match_income_coefs_inner)
# 
# ### Plot results -----
# # treated
# save_event_study_plots(
#   reg_results_df = event_study_twfe_income_coefs_treated,
#   outcome_variables = outcome_variables,
#   results_dir = results_dir,
#   prefix = "event_study_plots_by_income_treated_twfe_matching",
#   title_suffix = " by initial income tercile, treated tracts",
#   category_filter = NULL,
#   heterogeneity = TRUE
# )
# 
# 
# # inner
# save_event_study_plots(
#   reg_results_df = event_study_twfe_income_coefs_inner,
#   outcome_variables = outcome_variables,
#   results_dir = results_dir,
#   prefix = "event_study_plots_by_income_inner_twfe_matching",
#   title_suffix = " by initial income tercile, inner ring tracts",
#   category_filter = NULL,
#   heterogeneity = TRUE
# )
# 
# 
# 
# ## By initial population density -----
# # get pre-treatment income for treated units, create bins
# initial_pop_density <- 
#   tract_data_matched_2_year %>%
#   filter(location_type == "treated" | location_type == "inner") %>%
#   mutate(event_time = year - treatment_year) %>% 
#   filter(event_time == -10) %>%
#   dplyr::rename(pre_treatment_pop_density = population_density) %>% 
#   select(match_group, group_type, pre_treatment_pop_density) %>% 
#   distinct() %>% 
#   # create terciles
#   mutate(pop_density_tercile = ntile(pre_treatment_pop_density, 3)) %>% 
#   select(match_group, group_type, pop_density_tercile)
# 
# 
# # merge with data
# tract_data_matched_with_pop_density_tercile <- 
#   tract_data_matched_2_year %>%
#   left_join(initial_pop_density, by = c("match_group", "group_type"))
# 
# ### Treated ----
# # Initialize lists
# did_results_event_study_twfe_pop_density_treated <- list()
# did_results_event_study_sunab_pop_density_treated <- list()
# did_results_event_study_sunab_no_match_pop_density_treated <- list()
# 
# # Loop over
# for (outcome in outcome_variables) {
#   for (s in 1:max(tract_data_matched_with_pop_density_tercile$pop_density_tercile, na.rm = TRUE)) {
#     
#     
#     results <- did_event_study(input_data = tract_data_matched_with_pop_density_tercile %>%
#                                  filter(pop_density_tercile == s),
#                                outcome_var = outcome,
#                                treatment_group = "treated")
#     did_results_event_study_twfe_pop_density_treated[[paste(outcome, s, sep = "_")]] <- results$twfe
#     did_results_event_study_sunab_pop_density_treated[[paste(outcome, s,  sep = "_")]] <- results$sunab
#     did_results_event_study_sunab_no_match_pop_density_treated[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
#   }
# }
# 
# event_study_twfe_pop_density_coefs_treated <- save_estimates(did_results_event_study_twfe_pop_density_treated, "TWFE")
# event_study_sunab_pop_density_coefs_treated <- save_estimates(did_results_event_study_sunab_pop_density_treated, "Sun-Abraham")
# event_study_sunab_no_match_pop_density_coefs_treated <- save_estimates(did_results_event_study_sunab_no_match_pop_density_treated,
#                                                                 "Sun-Abraham without matching")
# 
# # combine all results
# combined_pop_density_results_treated <- 
#   bind_rows(event_study_twfe_pop_density_coefs_treated, event_study_sunab_pop_density_coefs_treated,
#             event_study_sunab_no_match_pop_density_coefs_treated)
# 
# ### Inner ----
# # Initialize lists
# did_results_event_study_twfe_pop_density_inner <- list()
# did_results_event_study_sunab_pop_density_inner <- list()
# did_results_event_study_sunab_no_match_pop_density_inner <- list()
# 
# # Loop over
# for (outcome in outcome_variables) {
#   for (s in 1:max(tract_data_matched_with_pop_density_tercile$pop_density_tercile, na.rm = TRUE)) {
#     
#     
#     results <- did_event_study(input_data = tract_data_matched_with_pop_density_tercile %>%
#                                  filter(pop_density_tercile == s),
#                                outcome_var = outcome,
#                                treatment_group = "inner")
#     did_results_event_study_twfe_pop_density_inner[[paste(outcome, s, sep = "_")]] <- results$twfe
#     did_results_event_study_sunab_pop_density_inner[[paste(outcome, s,  sep = "_")]] <- results$sunab
#     did_results_event_study_sunab_no_match_pop_density_inner[[paste(outcome, s, sep = "_")]] <- results$sunab_no_match
#   }
# }
# 
# event_study_twfe_pop_density_coefs_inner <- save_estimates(did_results_event_study_twfe_pop_density_inner, "TWFE")
# event_study_sunab_pop_density_coefs_inner <- save_estimates(did_results_event_study_sunab_pop_density_inner, "Sun-Abraham")
# event_study_sunab_no_match_pop_density_coefs_inner <- save_estimates(did_results_event_study_sunab_no_match_pop_density_inner,
#                                                                 "Sun-Abraham without matching")
# 
# # combine all results
# combined_pop_density_results_inner <- 
#   bind_rows(event_study_twfe_pop_density_coefs_inner, event_study_sunab_pop_density_coefs_inner,
#             event_study_sunab_no_match_pop_density_coefs_inner)
# 
# ### plot results ----
# 
# # treated
# save_event_study_plots(
#   reg_results_df = event_study_twfe_pop_density_coefs_treated,
#   outcome_variables = outcome_variables,
#   results_dir = results_dir,
#   prefix = "event_study_plots_by_density_treated_twfe_matching",
#   title_suffix = " by initial pop density, treated tracts",
#   category_filter = NULL,
#   heterogeneity = TRUE
# )
# 
# 
# # inner
# save_event_study_plots(
#   reg_results_df = event_study_twfe_pop_density_coefs_inner,
#   outcome_variables = outcome_variables,
#   results_dir = results_dir,
#   prefix = "event_study_plots_by_density_inner_ring_twfe_matching",
#   title_suffix = " by initial pop density, inner ring tracts",
#   category_filter = NULL,
#   heterogeneity = TRUE
# )
# 
# 
# ## TODO Separately for New York City and Non-NYC-----
# tract_data_matched_2_year_nyc <- 
#   tract_data_matched_2_year %>%
#   filter(city == "New York City")
# 
# tract_data_matched_non_nyc <- 
#   tract_data_matched_2_year %>%
#   filter(city != "New York City")  
# 
# 
# 
