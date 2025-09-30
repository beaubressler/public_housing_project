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

tract_data_matched_1_year <-
  read_csv(here(match_data_dir, "tract_data_matched_1_year_replacement.csv")) %>% 
  # adding this:
  mutate(ln_total_units = log(total_units))


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
  "median_rent_calculated" = "Median Rent",
  "asinh_median_rent_calculated" = "Log Median Rent",
  "total_units" = "Total Housing Units",
  "ln_total_units" = "Log Housing Units",
  "lfp_rate" = "Labor Force Participation Rate",
  "unemp_rate" = "Unemployment Rate",
  "pct_hs_grad" = "HS Graduation Rate")


group_types <- c("treated", "inner")


## function for computing wing weights
# 08/18/2025: With 1-1 KNN matching, do not need this: All weights are 1
# compute_weights <- function(dataset, treatedVar, eventTimeVar, subexpVar) {
#   
#   # Step 1: Compute stack-time counts for treated and control
#   stack_totals <- dataset %>%
#     group_by(!!sym(eventTimeVar)) %>%
#     summarise(
#       stack_n = n(),
#       stack_treat_n = sum(!!sym(treatedVar)),
#       stack_control_n = sum(1 - !!sym(treatedVar)),
#       .groups = 'drop'
#     )
#   
#   # Step 2: Compute sub-experiment-level counts  
#   sub_totals <- dataset %>%
#     group_by(!!sym(subexpVar), !!sym(eventTimeVar)) %>%
#     summarise(
#       sub_n = n(),
#       sub_treat_n = sum(!!sym(treatedVar)),
#       sub_control_n = sum(1 - !!sym(treatedVar)),
#       .groups = 'drop'
#     )
#   
#   # Step 3: Merge and compute shares
#   weighted_data <- dataset %>%
#     left_join(stack_totals, by = eventTimeVar) %>%
#     left_join(sub_totals, by = c(subexpVar, eventTimeVar)) %>%
#     mutate(
#       sub_share = sub_n / stack_n,
#       sub_treat_share = sub_treat_n / stack_treat_n,
#       sub_control_share = sub_control_n / stack_control_n
#     ) %>%
#     # Step 4: Compute weights for treated and control groups
#     mutate(
#       stack_weight = ifelse(!!sym(treatedVar) == 1, 
#                             1, 
#                             sub_treat_share / sub_control_share)
#     )
#   
#   return(weighted_data)
# }
# 
# tract_data_matched_1_year <-
#   tract_data_matched_1_year %>% 
#   # Add event time and treated indicator for Wing weights
#   group_by(match_group) %>%
#   mutate(
#     group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
#     event_time = year - group_treatment_year)
# 
# 
# treated_data <- tract_data_matched_1_year %>%
#   filter(group_type == "treated") %>%
#   mutate(treated_indicator = ifelse(location_type == "treated", 1, 0)) %>%
#   ungroup() %>%
#   # Apply Wing weights
#   compute_weights(
#     treatedVar = "treated_indicator",
#     eventTimeVar = "event_time",
#     subexpVar = "match_group"
#   )
# 
# inner_data <- 
# tract_data_matched_1_year %>%
#   filter(group_type == "inner") %>%
#   mutate(inner_indicator = ifelse(location_type == "inner", 1, 0)) %>%
#   ungroup() %>%
#   # Apply Wing weights
#   compute_weights(
#     treatedVar = "inner_indicator",
#     eventTimeVar = "event_time",
#     subexpVar = "match_group"
#   )



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
    
    results <- did_event_study(tract_data_matched_1_year,
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

## Calculate Effect Magnitudes AT t=+20 -----

# Extract estimates at t=+20 for TREATED neighborhoods only
effects_t20 <- event_study_conley_coefs %>%
  filter(term == "20", str_ends(outcome, "_treated")) %>%  # Only treated
  select(outcome, estimate, std.error, p.value) %>%
  # Remove the "_treated" suffix to match baseline variable names
  mutate(outcome_clean = str_remove(outcome, "_treated"))

# Calculate baseline means (at t=-10) - use clean variable names
baseline_means <- tract_data_matched_1_year %>%
  filter(group_type == "treated") %>%
  mutate(baseline_year = matched_treatment_year - 10) %>%
  filter(year == baseline_year) %>%
  summarise(
    black_share = mean(black_share, na.rm = TRUE),
    black_pop = mean(black_pop, na.rm = TRUE),
    white_pop = mean(white_pop, na.rm = TRUE),
    unemp_rate = mean(unemp_rate, na.rm = TRUE),
    lfp_rate = mean(lfp_rate, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "outcome_clean", values_to = "baseline_mean")

# Merge and calculate magnitudes
magnitudes_t20 <- effects_t20 %>%
  left_join(baseline_means, by = "outcome_clean") %>%
  mutate(
    percent_change = (estimate / baseline_mean) * 100)


# Print key magnitudes
cat("\n=== KEY EFFECT MAGNITUDES AT t=+20 ===\n")
print(magnitudes_t20 %>%
        filter(!is.na(baseline_mean)) %>% 
        select(outcome, estimate, baseline_mean, percent_change) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))))


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
  c("asinh_pop_total", 
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

# Publication-ready theme
pub_theme <- function(base_size = 14){
  theme_classic(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8))
    )
}

# Function for publication-ready faceted event studies
make_event_facets <- function(df, outcomes, title_text, group = "treated",
                              x_breaks = seq(-40, 40, 10)) {
  df_use <- df %>%
    filter(group == !!group, outcome_clean %in% outcomes) %>%
    mutate(
      clean_label = outcome_labels[outcome_clean],
      clean_label = factor(clean_label, levels = outcome_labels[outcomes]),
      event_time = as.numeric(term)
    )
  
  # Add a 0-at-(-10) reference row per series for visual anchoring
  ref_rows <- df_use %>%
    distinct(outcome_clean, clean_label) %>%
    mutate(event_time = -10, estimate = 0, std.error = 0)
  
  df_use <- bind_rows(df_use, ref_rows) %>% arrange(clean_label, event_time)
  
  ggplot(df_use, aes(x = event_time, y = estimate, group = clean_label)) +
    geom_ribbon(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                alpha = 0.18, linewidth = 0, fill = "grey60") +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8, stroke = 0) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.4) +
    labs(
      title = title_text,
      x = "Years Relative to Construction",
      y = "Difference-in-Differences Estimate"
    ) +
    scale_x_continuous(breaks = x_breaks) +
    facet_wrap(~ clean_label, ncol = 2, scales = "free_y") +
    pub_theme(14)
}


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
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
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



# Plot 1: Population and racial composition
plot1_outcomes <- c(
  "asinh_pop_total",
  "black_share",
  "asinh_pop_white",
  "asinh_pop_black"
)

p1 <- make_event_facets(
  df = coefs_clean,
  outcomes = plot1_outcomes,
  title_text = "Population and Racial Composition Effects, Treated Neighborhoods"
)

# Export as vector PDF and PNG
ggsave(file.path(results_dir, "event_study_population_demographics_treated.pdf"),
       p1, width = 8.5, height = 7.5, device = cairo_pdf)
ggsave(file.path(results_dir, "event_study_population_demographics_treated.png"),
       p1, width = 8.5, height = 7.5, dpi = 320)

# Plot 2: Economic and housing outcomes
plot2_outcomes <- c(
  "asinh_median_rent_calculated",
  "unemp_rate",
  "lfp_rate",
  "asinh_median_income"
)

p2 <- make_event_facets(
  df = coefs_clean,
  outcomes = plot2_outcomes,
  title_text = "Economic and Housing Effects, Treated Neighborhoods"
)

ggsave(file.path(results_dir, "event_study_economic_housing_treated.pdf"),
       p2, width = 8.5, height = 7.5, device = cairo_pdf)
ggsave(file.path(results_dir, "event_study_economic_housing_treated.png"),
       p2, width = 8.5, height = 7.5, dpi = 320)


# Plot 3: Single-panel population plot - Private vs Total populations
p3_outcomes <- c(
  "asinh_private_population_estimate",
  "asinh_private_white_population_estimate",
  "asinh_private_black_population_estimate"
)

# Pretty labels for p3
p3_labels <- c(
  "asinh_private_population_estimate" = "Private Population",
  "asinh_private_white_population_estimate" = "Private White Population",
  "asinh_private_black_population_estimate" = "Private Black Population"
)

# Create data for p3
p3_data <- coefs_clean %>%
  filter(group == "treated", outcome_clean %in% p3_outcomes) %>%
  mutate(
    clean_label = p3_labels[outcome_clean],
    clean_label = factor(clean_label, levels = p3_labels[p3_outcomes]),
    event_time = as.numeric(term)
  )

# Add reference points at event_time = -10
p3_ref_rows <- p3_data %>%
  distinct(outcome_clean, clean_label) %>%
  mutate(event_time = -10, estimate = 0, std.error = 0)

p3_data <- bind_rows(p3_data, p3_ref_rows) %>%
  arrange(clean_label, event_time)

# Create p3 plot
p3 <- ggplot(p3_data, aes(x = event_time, y = estimate,
                          color = clean_label)) +
  # Error bars instead of ribbons
  geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                    ymax = estimate + 1.96*std.error),
                width = 0.4, linewidth = 0.8, alpha = 0.7,
                position = position_dodge(width = 2.5)) +
  # Lines and points with staggered positioning
  geom_line(aes(group = clean_label), linewidth = 0.9,
            position = position_dodge(width = 2.5)) +
  geom_point(size = 2.2, stroke = 0,
             position = position_dodge(width = 2.5)) +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.4, color = "grey50") +
  # Labels and formatting
  labs(
    title = "Effects on (Log) Estimated Private Population, Treated Neighborhoods",
    x = "Years Relative to Construction",
    y = "Difference-in-Differences Estimate",
    color = ""
  ) +
  scale_x_continuous(breaks = seq(-30, 30, 10)) +
  scale_color_manual(values = c("#2171b5", "#6baed6", "#9ecae1")) +
  theme_classic(base_size = 14) +
  theme(
    plot.title.position = "plot",
    legend.position = "bottom",
    legend.margin = margin(t = 10),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )

# Export p3
ggsave(file.path(results_dir, "event_study_population_private_treated.pdf"),
       p3, width = 9, height = 6.5, device = cairo_pdf)
ggsave(file.path(results_dir, "event_study_population_private_treated.png"),
       p3, width = 9, height = 6.5, dpi = 320)




#### Inner ----
# Spillover/Inner Ring Plots - Same structure as treated plots

# Plot 3: Spillover Population and racial composition
spillover_plot1_outcomes <- c(
  "asinh_pop_total",
  "black_share",
  "asinh_pop_white",
  "asinh_pop_black"
)


# # Need to modify the function call to use inner group
# spillover_p1_data <- coefs_clean %>%
#   filter(group == "inner", outcome_clean %in% spillover_plot1_outcomes) %>%
#   mutate(
#     clean_label = outcome_labels[outcome_clean],
#     clean_label = factor(clean_label, levels =
#                            outcome_labels[spillover_plot1_outcomes]),
#     event_time = as.numeric(term)
#   )
# 
# # Add reference rows
# spillover_ref_rows1 <- spillover_p1_data %>%
#   distinct(outcome_clean, clean_label) %>%
#   mutate(event_time = -10, estimate = 0, std.error = 0)
# 
# spillover_p1_data <- bind_rows(spillover_p1_data, spillover_ref_rows1) %>%
#   arrange(clean_label, event_time)
# 
# # Create spillover plot 1
# sp1 <- ggplot(spillover_p1_data, aes(x = event_time, y = estimate, group =
#                                        clean_label)) +
#   geom_ribbon(aes(ymin = estimate - 1.96*std.error,
#                   ymax = estimate + 1.96*std.error),
#               alpha = 0.18, linewidth = 0, fill = "grey60") +
#   geom_line(linewidth = 0.9) +
#   geom_point(size = 1.8, stroke = 0) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
#   geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.4) +
#   labs(
#     title = "Effects of Public Housing on Nearby Neighborhoods: Population and Racial Composition",
#     x = "Years Relative to Construction",
#     y = "Difference-in-Differences Estimate"
#   ) +
#   scale_x_continuous(breaks = seq(-40, 40, 10)) +
#   facet_wrap(~ clean_label, ncol = 2, scales = "free_y") +
#   pub_theme(14)

sp1 <- make_event_facets(
  df = coefs_clean,
  outcomes = spillover_plot1_outcomes,
  title = "Effects of Public Housing on Nearby Neighborhoods: Population and Racial Composition",
  group = "inner"
)

# Export spillover plot 1
ggsave(file.path(results_dir,
                 "event_study_spillover_population_demographics.pdf"),
       sp1, width = 12, height = 9, device = cairo_pdf)
ggsave(file.path(results_dir,
                 "event_study_spillover_population_demographics.png"),
       sp1, width = 12, height = 9, dpi = 320)

# Plot 4: Spillover Economic and housing outcomes
spillover_plot2_outcomes <- c(
  "asinh_median_rent_calculated",
  "unemp_rate",
  "lfp_rate",
  "asinh_median_income"
)

# # Prepare spillover plot 2 data
# spillover_p2_data <- coefs_clean %>%
#   filter(group == "inner", outcome_clean %in% spillover_plot2_outcomes) %>%
#   mutate(
#     clean_label = outcome_labels[outcome_clean],
#     clean_label = factor(clean_label, levels =
#                            outcome_labels[spillover_plot2_outcomes]),
#     event_time = as.numeric(term)
#   )
# 
# # Add reference rows
# spillover_ref_rows2 <- spillover_p2_data %>%
#   distinct(outcome_clean, clean_label) %>%
#   mutate(event_time = -10, estimate = 0, std.error = 0)
# 
# spillover_p2_data <- bind_rows(spillover_p2_data, spillover_ref_rows2) %>%
#   arrange(clean_label, event_time)
# 
# # Create spillover plot 2
# sp2 <- ggplot(spillover_p2_data, aes(x = event_time, y = estimate, group =
#                                        clean_label)) +
#   geom_ribbon(aes(ymin = estimate - 1.96*std.error,
#                   ymax = estimate + 1.96*std.error),
#               alpha = 0.18, linewidth = 0, fill = "grey60") +
#   geom_line(linewidth = 0.9) +
#   geom_point(size = 1.8, stroke = 0) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
#   geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.4) +
#   labs(
#     title = "Effects of Public Housing on Nearby Neighborhoods: Economic and Housing Outcomes",
#     x = "Years Relative to Construction",
#     y = "Difference-in-Differences Estimate"
#   ) +
#   scale_x_continuous(breaks = seq(-40, 40, 10)) +
#   facet_wrap(~ clean_label, ncol = 2, scales = "free_y") +
#   pub_theme(14)


sp2 <- make_event_facets(
  df = coefs_clean, 
  outcomes = spillover_plot2_outcomes,
  title_text = "Effects of Public Housing on Nearby Neighborhoods: Economic and Housing Outcomes",
  group = "inner"
)

# Export spillover plot 2
ggsave(file.path(results_dir,
                 "event_study_spillover_economic_housing.pdf"),
       sp2, width = 12, height = 9, device = cairo_pdf)


#### Poster Plots: Key Variables Only ----

# Define the 4 key variables for poster
poster_outcomes <- c(
  "asinh_pop_total",
  "black_share", 
  "asinh_median_income",
  "asinh_median_rent_calculated"
)

# Create treated neighborhoods plot with key variables
poster_treated_plot <- make_event_facets(
  df = coefs_clean,
  outcomes = poster_outcomes,
  title_text = ""
) + 
  pub_theme(16)

ggsave(file.path(results_dir, "event_study_treated_key_variables.pdf"),
       poster_treated_plot, width = 12, height = 7, device = cairo_pdf)

# Create spillover neighborhoods plot with key variables
poster_spillover_data <- coefs_clean %>%
  filter(group == "inner", outcome_clean %in% poster_outcomes) %>%
  mutate(
    clean_label = outcome_labels[outcome_clean],
    clean_label = factor(clean_label, levels = outcome_labels[poster_outcomes]),
    event_time = as.numeric(term)
  )

# Add reference rows for spillover plot
poster_spillover_ref_rows <- poster_spillover_data %>%
  distinct(outcome_clean, clean_label) %>%
  mutate(event_time = -10, estimate = 0, std.error = 0)

poster_spillover_data <- bind_rows(poster_spillover_data, poster_spillover_ref_rows) %>%
  arrange(clean_label, event_time) %>%
  filter(event_time >= -20)  # Only show one pretrend as discussed

poster_spillover_plot <- ggplot(poster_spillover_data, aes(x = event_time, y = estimate, group = clean_label)) +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error,
                  ymax = estimate + 1.96*std.error),
              alpha = 0.18, linewidth = 0, fill = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
  facet_wrap(~clean_label, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(-20, 30, 10)) +
  labs(
    x = "Decades Relative to Public Housing Construction",
    y = "Difference-in-Differences Estimate"
  ) +
  pub_theme(16)

ggsave(file.path(results_dir, "event_study_spillover_key_variables.pdf"),
       poster_spillover_plot, width = 12, height = 7, device = cairo_pdf)


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
  "Log Black Pop." = did_results_event_study_conley[["asinh_pop_black_treated"]],
  "Log White Pop." = did_results_event_study_conley[["asinh_pop_white_treated"]],
  "Black Share" = did_results_event_study_conley[["black_share_treated"]],
  "Log Median Income" = did_results_event_study_conley[["asinh_median_income_treated"]],
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


# Neighborhood composition
inner_models <-
  list(
    "Log Total Pop." = did_results_event_study_conley[["asinh_pop_total_inner"]],
    "Log Median Rent" = did_results_event_study_conley[["asinh_median_rent_calculated_inner"]],
    "Log Black Population" = did_results_event_study_conley[["asinh_pop_black_inner"]],
    "Log White Population" = did_results_event_study_conley[["asinh_pop_white_inner"]],
    "Black Share" = did_results_event_study_conley[["black_share_inner"]],
    "Log Median Income" = did_results_event_study_conley[["asinh_median_income_inner"]],
    "LFP Rate" = did_results_event_study_conley[["lfp_rate_inner"]],
    "Unemp. Rate" = did_results_event_study_conley[["unemp_rate_inner"]]
  )

modelsummary(
  inner_models,
  stars = TRUE,
  fmt = 2, # Round to 2 decimal places
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",     # Omits unneeded goodness-of-fit statistics
  title = "",
  output = here(results_dir, "tables", "inner_models_twfe.tex")
)



# # Run pooled DiD -----
# tract_data_matched_1_year <- tract_data_matched_1_year %>%
#   group_by(match_group) %>%
#   # define event time for both treated and control
#   mutate(
#     group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
#     event_time = year - group_treatment_year) %>% 
#   ungroup()
# 
# 
# 
# matched_did_pre_post <- function(input_data, outcome_var, treatment_group,
#                          size = NULL, city_filter = NULL, initial_share = NULL) {
#   
#   data <- input_data %>%
#     filter(!is.na(match_group),
#            group_type == treatment_group)
#   
#   if (!is.null(size)) data <- data %>% filter(size_group == size)
#   if (!is.null(city_filter)) data <- data %>% filter(city == city_filter)
#   if (!is.null(initial_share)) data <- data %>% filter(race_group == initial_share)
#   
#   data <- data %>%
#     group_by(match_group) %>%
#     mutate(
#       group_treatment_year = min(matched_treatment_year, na.rm = TRUE),
#       event_time = year - group_treatment_year,
#       treated = ifelse(location_type == treatment_group, 1, 0),
#       post = ifelse(event_time >= 0, 1, 0)
#     ) %>%
#     ungroup() %>%
#     filter(event_time > -40, event_time <= 30, event_time > -30)
#   
#   # Basic DiD interaction model: post × treated
#   formula <- as.formula(paste0(
#     outcome_var, " ~ i(treated, post, ref = c(0,0)) | GISJOIN_1950 + match_group^year"
#   ))
#   
#   model <- feols(formula, data = data, weights = ~weights, cluster = ~GISJOIN_1950)
#   
#   model_conley <- feols(formula, data = data, weights = ~weights,
#                         vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 2))
#   
#   formula_nomatch <- as.formula(paste0(
#     outcome_var, " ~ i(treated, post, ref = c(0,0)) | GISJOIN_1950"
#   ))
#   
#   model_nomatch_conley <- feols(formula_nomatch, data = data, weights = ~weights,
#                                 vcov = vcov_conley(lat = "lat", lon = "lon", cutoff = 2))
#   
#   return(list(pre_post = model, pre_post_conley = model_conley,
#               pre_post_nomatch_conley = model_nomatch_conley))
# }
# 
# 
# results_pre_post <- list()
# 
# for (outcome in outcome_variables) {
#   for (group in group_types) {
#     cat("Pre/post DiD for", outcome, "|", group, "\n")
#     
#     results <- matched_did_pre_post(
#       input_data = tract_data_matched_1_year,
#       outcome_var = outcome,
#       treatment_group = group
#     )
#     
#     results_pre_post[[paste0(outcome, "_", group)]] <- results$pre_post_conley
#   }
# }
# 
# 
# Heterogeneity: Event study t= 20 -----
# Extract t=20 estimates
extract_t20_estimates <- function(results_list, location_label) {
  estimates <- map_dfr(names(results_list), function(name) {
    model <- results_list[[name]]
    coefs <- tidy(model)
    
    # Extract t=20 estimate
    t20_coef <- coefs %>%
      filter(str_detect(term, "event_time::20:treated")) %>%
      select(estimate, std.error, p.value)
    
    if(nrow(t20_coef) > 0) {
      tibble(
        outcome_group = name,
        location = location_label,
        estimate = t20_coef$estimate,
        std.error = t20_coef$std.error,
        p.value = t20_coef$p.value,
        conf.low = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error
      )
    } else {
      NULL
    }
  })
  return(estimates)
}

# Function to create heterogeneity comparison plots (multiple groups)
create_heterogeneity_plot <- function(..., 
                                      outcomes_to_plot,
                                      outcome_labels_map,
                                      title_text,
                                      subtitle_text = "Treatment effects at t=20",
                                      colors = NULL,
                                      save_name = NULL,
                                      results_dir = NULL) {
  
  # Get arguments as list
  args <- list(...)
  
  # Check if old named argument format is being used
  if (all(c("results_list1", "results_list2", "label1", "label2") %in% names(args))) {
    # Convert old format to new format
    results_lists <- list(args$results_list1, args$results_list2)
    labels <- c(args$label1, args$label2)
  } else {
    # New format: alternating results_list, label pairs
    if (length(args) %% 2 != 0) {
      stop("Arguments must be alternating results_list, label pairs")
    }
    
    results_lists <- args[seq(1, length(args), 2)]
    labels <- unlist(args[seq(2, length(args), 2)])
  }
  
  # Set default colors if not provided
  if (is.null(colors)) {
    colors <- RColorBrewer::brewer.pal(max(3, length(labels)), "Set1")[1:length(labels)]
  }
  
  # Extract t=20 estimates for all groups
  all_estimates <- map2_dfr(results_lists, labels, ~extract_t20_estimates(.x, .y))
  
  # Combine and clean
  comparison_data <- all_estimates %>%
    # Parse outcome and group
    mutate(
      group = str_extract(outcome_group, "(treated|inner)$"),
      outcome_clean = str_remove(outcome_group, "_(treated|inner)$")
    ) %>%
    # Filter to specified outcomes
    filter(outcome_clean %in% outcomes_to_plot) %>%
    mutate(
      # Apply outcome labels
      clean_label = case_when(
        outcome_clean %in% names(outcome_labels_map) ~ outcome_labels_map[outcome_clean],
        TRUE ~ outcome_clean
      ),
      # Clean group labels
      group_label = case_when(
        group == "treated" ~ "Treated Neighborhoods",
        group == "inner" ~ "Spillover Neighborhoods", 
        TRUE ~ group
      ),
      # Order factors
      clean_label = factor(clean_label, levels = outcome_labels_map[outcomes_to_plot]),
      group_label = factor(group_label, levels = c("Treated Neighborhoods", "Spillover Neighborhoods")),
      location = factor(location, levels = labels)
    )
  
  # Create plot with publication-quality formatting
  p <- ggplot(comparison_data, aes(x = location, y = estimate, color = location)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray30", linewidth = 0.4) +
    geom_point(size = 3.5, alpha = 0.9, shape = 16) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.15, linewidth = 1, alpha = 0.8) +
    facet_grid(group_label ~ clean_label, scales = "free_y",
               labeller = labeller(clean_label = label_wrap_gen(width = 12))) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "",
      y = "Treatment Effect",
      color = ""
    ) +
    scale_color_manual(values = setNames(colors, labels)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                      decimal.mark = ".",
                                                      big.mark = ",")) +
    theme_minimal(base_size = 14, base_family = "sans") +
    theme(
      # Plot titles and text
      plot.title = element_text(size = 18, face = "bold", color = "black",
                               margin = margin(b = 10)),
      plot.subtitle = element_text(size = 14, color = "gray40",
                                  margin = margin(b = 15)),
      plot.title.position = "plot",

      # Axes
      axis.title.y = element_text(size = 14, color = "black",
                                 margin = margin(r = 10)),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.3),
      axis.ticks = element_line(color = "black", linewidth = 0.3),
      axis.ticks.length = unit(2, "pt"),

      # Facets/strips
      strip.background = element_rect(fill = "gray95", color = "gray80", linewidth = 0.3),
      strip.text = element_text(face = "bold", size = 13, color = "black",
                               margin = margin(6, 6, 6, 6)),

      # Panel
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor.y = element_line(color = "gray95", linewidth = 0.2),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.spacing = unit(12, "pt"),

      # Legend
      legend.position = "none",

      # Plot margins
      plot.margin = margin(15, 15, 10, 10)
    )
  
  # Save if requested
  if (!is.null(save_name) & !is.null(results_dir)) {
    ggsave(file.path(results_dir, paste0(save_name, ".pdf")),
           p, width = 10, height = 7, device = cairo_pdf,
           units = "in", dpi = 300)
  }
  
  return(p)
}


## NYC vs non-NYC event study ----
heterogeneity_outcome_vars <- 
  c("asinh_median_rent_calculated", "asinh_median_income",
    "black_share", "asinh_pop_black", "asinh_pop_white",
    "black_pop", "white_pop",
    "unemp_rate")

tract_data_matched_1_year_nyc <- 
  tract_data_matched_1_year %>% 
  filter(COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York")

tract_data_matched_1_year_non_nyc <- 
  tract_data_matched_1_year %>% 
  filter(!(COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York"))

results_event_study_nyc <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| NYC only\n")
    
    results <- did_event_study(
      input_data = tract_data_matched_1_year_nyc,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_nyc[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

results_event_study_non_nyc <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Non-NYC only\n")
    
    results <- did_event_study(
      input_data = tract_data_matched_1_year_non_nyc,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_non_nyc[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}



# Create NYC vs Other Cities comparison
race_income_labels <- c(
  "black_share" = "Black Pop Share",
  "asinh_median_income" = "Log Median Income",
  "asinh_pop_black" = "Log Black Pop",
  "asinh_pop_white" = "Log White Pop",
  "black_pop" = "Black Pop",
  "white_pop" = "White Pop",
  "asinh_median_rent_calculated" = "Log Median Rent",
  "unemp_rate" = "Unemployment Rate"
)

p_nyc_comparison <- create_heterogeneity_plot(
  results_list1 = results_event_study_nyc,
  results_list2 = results_event_study_non_nyc,
  label1 = "NYC",
  label2 = "Other Cities", 
  outcomes_to_plot = c("asinh_median_income", "asinh_pop_white", "asinh_pop_black"),
  # outcomes_to_plot = "black_share",
  outcome_labels_map = race_income_labels,
  title_text = "Geographic Heterogeneity: NYC vs Other Cities",
  save_name = "nyc_vs_others_race_income_t20",
  results_dir = results_dir
)

p_nyc_comparison

## Urban Renewal vs Non-Urban Renewal event study ----

tract_data_matched_1_year_ur <- 
  tract_data_matched_1_year %>% 
  filter(ur_binary_5pp == 1)

tract_data_matched_1_year_no_ur <- 
  tract_data_matched_1_year %>% 
  filter(ur_binary_5pp == 0)

results_event_study_ur <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| UR areas only\n")
    
    results <- did_event_study(
      input_data = tract_data_matched_1_year_ur,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_ur[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

results_event_study_no_ur <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Non-UR areas only\n")
    
    results <- did_event_study(
      input_data = tract_data_matched_1_year_no_ur,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_no_ur[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Create Urban Renewal vs Non-Urban Renewal comparison
p_ur_comparison <- create_heterogeneity_plot(
  results_list1 = results_event_study_ur,
  results_list2 = results_event_study_no_ur,
  label1 = "Urban Renewal Areas",
  label2 = "Non-UR Areas", 
  outcomes_to_plot = c("asinh_median_income", "asinh_pop_black", "asinh_pop_white"),
  outcome_labels_map = race_income_labels,
  title_text = "Urban Renewal vs Non-Urban Renewal Areas",
  colors = c("#FF7F00", "#377EB8"),
  save_name = "ur_vs_no_ur_race_income_t20",
  results_dir = results_dir
)

p_ur_comparison

# output p_ur_comparison to pdf
ggsave(
  filename = here("output", "regression_results",  "urban_renewal_heterogeneity_race_income_t20.pdf"),
  plot = p_ur_comparison,
  width = 12,
  height = 8,
  device = "pdf"
)


## Early vs Late Programs (Time Period) event study ----

tract_data_matched_1_year_early <- 
  tract_data_matched_1_year %>% 
  filter(matched_treatment_year <= 1960)

tract_data_matched_1_year_late <- 
  tract_data_matched_1_year %>% 
  filter(matched_treatment_year > 1960)

results_event_study_early <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Early programs (≤1960)\n")
    
    results <- did_event_study(
      input_data = tract_data_matched_1_year_early,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_early[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

results_event_study_late <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Late programs (≥1970)\n")
    
    results <- did_event_study(
      input_data = tract_data_matched_1_year_late,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_late[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Create Early vs Late Programs comparison
p_time_comparison <- create_heterogeneity_plot(
  results_list1 = results_event_study_early,
  results_list2 = results_event_study_late,
  label1 = "1941-1960",
  label2 = "1961-1973", 
  outcomes_to_plot = c("asinh_pop_black", "asinh_median_income", "asinh_pop_white"),
  outcome_labels_map = race_income_labels,
  title_text = "Early vs Late Public Housing Projects",
  colors = c("#2E8B57", "#8B0000"),
  save_name = "early_vs_late_programs_race_income_t20",
  results_dir = results_dir
)

p_time_comparison

# output p_time_comparison to pdf 
ggsave(
  filename = here("output", "regression_results",  "early_vs_late_programs_race_income_t20.pdf"),
  plot = p_time_comparison,
  width = 12,
  height = 8,
  device = "pdf"
)

  
## Redlined vs Non-Redlined Areas event study ----

tract_data_matched_1_year_redlined <- 
  tract_data_matched_1_year %>% 
  filter(redlined_binary_80pp == 1)

tract_data_matched_1_year_not_redlined <- 
  tract_data_matched_1_year %>% 
  filter(redlined_binary_80pp == 0)

results_event_study_redlined <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Redlined areas only\n")
    
    results <- did_event_study(
      input_data = tract_data_matched_1_year_redlined,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_redlined[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

results_event_study_not_redlined <- list()
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    cat("Event study for", outcome, "|", group, "| Non-redlined areas only\n")
    
    results <- did_event_study(
      input_data = tract_data_matched_1_year_not_redlined,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_not_redlined[[paste0(outcome, "_", group)]] <- results$twfe_conley
  }
}

# Create Redlined vs Non-Redlined comparison
p_redlined_comparison <- create_heterogeneity_plot(
  results_list1 = results_event_study_redlined,
  results_list2 = results_event_study_not_redlined,
  label1 = "Redlined Areas",
  label2 = "Non-Redlined Areas", 
  outcomes_to_plot = c("asinh_pop_white", "asinh_pop_black", "asinh_median_income"),
  outcome_labels_map = race_income_labels,
  title_text = "Redlined vs Non-Redlined Areas",
  colors = c("#DC143C", "#4682B4"),
  save_name = "redlined_vs_not_redlined_race_income_t20",
  results_dir = results_dir
)

p_redlined_comparison


## Within Early: Baseline Black Share Heterogeneity event study ----
# Filter to early treatment period only
tract_data_early_projects <-
  tract_data_matched_1_year 
# %>%
#   filter(matched_treatment_year >= 1941 & matched_treatment_year <= 1960)

# Calculate baseline Black share for each tract (at t=-10)
baseline_black_share <-
  tract_data_early_projects %>%
  mutate(baseline_year = matched_treatment_year - 10) %>%
  filter(year == baseline_year, location_type != "donor_pool") %>%
  select(match_group, group_type, black_share) %>%
  rename(baseline_black_share = black_share) %>%
  distinct(baseline_black_share, group_type, match_group, .keep_all = TRUE)  # Keep only unique combinations

# 
baseline_black_share %>% filter(baseline_black_share >.01) %>% pull(baseline_black_share) %>% summary()



# Merge baseline Black share back
tract_data_early_with_baseline <-
  tract_data_matched_1_year %>%
  left_join(baseline_black_share, by = c("group_type", "match_group"))

# Define cutoffs for baseline Black share - using median split
baseline_summary <- baseline_black_share %>%
  summarise(
    p33 = quantile(baseline_black_share, 0.33, na.rm = TRUE),
    p67 = quantile(baseline_black_share, 0.67, na.rm = TRUE),
    median = median(baseline_black_share, na.rm = TRUE)
  )

# Create three meaningful baseline groups
tract_data_very_low_baseline <-
  tract_data_early_with_baseline %>%
  filter(baseline_black_share < 0.01)                         # <1% Black (essentially white)

tract_data_medium_baseline <- tract_data_early_with_baseline %>%
  filter(baseline_black_share >= 0.01 & baseline_black_share <= 0.15)  # 1-15% Black (marginal)

tract_data_high_baseline <- tract_data_early_with_baseline %>%
  filter(baseline_black_share >= 0.15)                        # >15% Black (integrated)

# Check sample sizes
cat("Sample sizes by baseline Black share:\n")
cat("Very Low (<1%):", nrow(tract_data_very_low_baseline %>% distinct(GISJOIN_1950, match_group)), "\n")
cat("Medium (1-15%):", nrow(tract_data_medium_baseline %>% distinct(GISJOIN_1950, match_group)), "\n")
cat("High (>15%):", nrow(tract_data_high_baseline %>% distinct(GISJOIN_1950, match_group)), "\n")

# Run event studies for all three groups
results_event_study_very_low_baseline <- list()
results_event_study_medium_baseline <- list()
results_event_study_high_baseline <- list()

# Very low baseline Black share areas (<1%)
cat("Running event studies for VERY LOW baseline Black share areas (<1%)...\n")
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    model_name <- paste0(outcome, "_", group)
    cat("Running", model_name, "\n")
    
    results <- did_event_study(
      input_data = tract_data_very_low_baseline,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_very_low_baseline[[model_name]] <- results$twfe_conley
  }
}

# Medium baseline Black share areas (1-15%)
cat("Running event studies for MEDIUM baseline Black share areas (1-15%)...\n")
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    model_name <- paste0(outcome, "_", group)
    cat("Running", model_name, "\n")
    
    results <- did_event_study(
      input_data = tract_data_medium_baseline,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_medium_baseline[[model_name]] <- results$twfe_conley
  }
}

# High baseline Black share areas (>15%)  
cat("Running event studies for HIGH baseline Black share areas (>15%)...\n")
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    model_name <- paste0(outcome, "_", group)
    cat("Running", model_name, "\n")
    
    results <- did_event_study(
      input_data = tract_data_high_baseline,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_high_baseline[[model_name]] <- results$twfe_conley
  }
}

results_event_study_very_low_baseline$black_share_treated
results_event_study_medium_baseline$black_share_treated
results_event_study_high_baseline$black_share_treated


results_event_study_very_low_baseline$asinh_pop_black_treated
results_event_study_medium_baseline$asinh_pop_black_treated
results_event_study_high_baseline$asinh_pop_black_treated

## Baseline black share, alt: ----
# Calculate baseline Black share for each tract (at t=-10)
baseline_black_share <-
  tract_data_matched_1_year %>%
  mutate(baseline_year = matched_treatment_year - 10) %>%
  filter(year == baseline_year, location_type != "donor_pool") %>%
  select(match_group, group_type, black_share) %>%
  rename(baseline_black_share = black_share) %>%
  distinct(baseline_black_share, group_type, match_group, .keep_all = TRUE)

# Merge baseline Black share back
tract_data_with_baseline <-
  tract_data_matched_1_year %>%
  left_join(baseline_black_share, by = c("group_type", "match_group"))

# Define three meaningful baseline groups
tract_data_very_low_baseline <-
  tract_data_with_baseline %>%
  filter(baseline_black_share < 0.01)  # <1% Black (essentially all white)

tract_data_medium_baseline <-
  tract_data_with_baseline %>%
  filter(baseline_black_share >= 0.01 & baseline_black_share < 0.12)  # 1-12% Black (tipping range)

tract_data_high_baseline <-
  tract_data_with_baseline %>%
  filter(baseline_black_share >= 0.12)  # >=12% Black (integrated/majority Black)

# Check sample sizes
cat("Sample sizes by baseline Black share:\n")
cat("Very Low (<1%):", nrow(tract_data_very_low_baseline %>% distinct(GISJOIN_1950, match_group)), "\n")
cat("Medium (1-12%):", nrow(tract_data_medium_baseline %>% distinct(GISJOIN_1950, match_group)), "\n")
cat("High (>=12%):", nrow(tract_data_high_baseline %>% distinct(GISJOIN_1950, match_group)), "\n")

# Run event studies for all three groups
results_event_study_very_low_baseline <- list()
results_event_study_medium_baseline <- list()
results_event_study_high_baseline <- list()

# Very low baseline Black share areas
cat("Running event studies for VERY LOW baseline Black share areas (<1%)...\n")
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    model_name <- paste0(outcome, "_", group)
    cat("Running", model_name, "\n")
    
    results <- did_event_study(
      input_data = tract_data_very_low_baseline,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_very_low_baseline[[model_name]] <- results$twfe_conley
  }
}

# Medium baseline Black share areas
cat("Running event studies for MEDIUM baseline Black share areas (1-12%)...\n")
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    model_name <- paste0(outcome, "_", group)
    cat("Running", model_name, "\n")
    
    results <- did_event_study(
      input_data = tract_data_medium_baseline,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_medium_baseline[[model_name]] <- results$twfe_conley
  }
}

# High baseline Black share areas
cat("Running event studies for HIGH baseline Black share areas (>=12%)...\n")
for (outcome in heterogeneity_outcome_vars) {
  for (group in group_types) {
    model_name <- paste0(outcome, "_", group)
    cat("Running", model_name, "\n")
    
    results <- did_event_study(
      input_data = tract_data_high_baseline,
      outcome_var = outcome,
      treatment_group = group
    )
    
    results_event_study_high_baseline[[model_name]] <- results$twfe_conley
  }
}

# Function to create three-group comparison plot
# Create the three-group plot using your existing function
p_three_groups <- create_heterogeneity_plot(
  results_event_study_very_low_baseline, "Very Low (<1%)",
  results_event_study_medium_baseline, "Medium (1-12%)",
  results_event_study_high_baseline, "High (≥12%)",
  outcomes_to_plot = c("black_pop", "white_pop"),
  outcome_labels_map = race_income_labels,
  title_text = "Public Housing Effects by Baseline Black Share",
  colors = c("#2E8B57", "#8B4513", "#8B0000"),
  save_name = "baseline_three_groups_t20",
  results_dir = results_dir
)

# Save the plot
ggsave(
  filename = here("output", "regression_results", "baseline_black_share_heterogeneity_t20.pdf"),
  plot = p_three_groups,
  width = 12,
  height = 8,
  device = "pdf"
)

p_three_groups



## By number of units: Spillovers -----

# Check spillover project size data availability
cat("\n=== SPILLOVER PROJECT SIZE DATA CHECK ===\n")
spillover_units_summary <- tract_data_matched_1_year %>%
  filter(group_type == "inner", !is.na(total_public_housing_units_built_nearby)) %>%
  select(total_public_housing_units_built_nearby) %>%
  summary()

print(spillover_units_summary)

# Create project size categories for spillover tracts - 500 unit cutoff
tract_data_matched_1_year <- tract_data_matched_1_year %>%
  mutate(
    # Tertile approach
    spillover_size_bin = case_when(
      !is.na(total_public_housing_units_built_nearby) ~ 
        ntile(total_public_housing_units_built_nearby, 3),
      TRUE ~ NA_integer_
    ),
    spillover_size_group = factor(case_when(
      spillover_size_bin == 1 ~ "Small Nearby",
      spillover_size_bin == 2 ~ "Medium Nearby", 
      spillover_size_bin == 3 ~ "Large Nearby"
    ), levels = c("Small Nearby", "Medium Nearby", "Large Nearby")),
    
    log_spillover_units = ifelse(!is.na(total_public_housing_units_built_nearby),
                                log(total_public_housing_units_built_nearby), NA)
  )

# Check tertile distribution
cat("\n=== SPILLOVER SIZE TERTILE DISTRIBUTION ===\n")
spillover_size_check <- tract_data_matched_1_year %>%
  filter(group_type == "inner", !is.na(spillover_size_group)) %>%
  group_by(spillover_size_group) %>%
  summarise(
    n_observations = n(),
    n_tracts = n_distinct(GISJOIN_1950),
    min_units = min(total_public_housing_units_built_nearby, na.rm = TRUE),
    median_units = median(total_public_housing_units_built_nearby, na.rm = TRUE),
    max_units = max(total_public_housing_units_built_nearby, na.rm = TRUE),
    .groups = 'drop'
  )
print(spillover_size_check)


# Run spillover event studies by project size
spillover_outcomes <- c("black_share", "asinh_pop_black", "asinh_pop_white", 
                       "asinh_median_rent_calculated", "asinh_median_income")

# Tertile splits
tract_data_small_spillover <- 
  tract_data_matched_1_year %>% 
  filter(spillover_size_bin == 1)

tract_data_medium_spillover <- 
  tract_data_matched_1_year %>% 
  filter(spillover_size_bin == 2)

tract_data_large_spillover <- 
  tract_data_matched_1_year %>% 
  filter(spillover_size_bin == 3)




# Run event studies for small spillover projects (tertile approach)
results_small_spillover <- list()
for (outcome in spillover_outcomes) {
  cat("Event study for", outcome, "| Small spillover projects (tertile)\n")
  
  results <- did_event_study(
    input_data = tract_data_small_spillover,
    outcome_var = outcome,
    treatment_group = "inner"
  )
  
  results_small_spillover[[paste0(outcome, "_inner")]] <- results$twfe_conley
}

# Run event studies for medium spillover projects
results_medium_spillover <- list()
for (outcome in spillover_outcomes) {
  cat("Event study for", outcome, "| Medium spillover projects\n")
  
  results <- did_event_study(
    input_data = tract_data_medium_spillover,
    outcome_var = outcome,
    treatment_group = "inner"
  )
  
  results_medium_spillover[[paste0(outcome, "_inner")]] <- results$twfe_conley
}

# Run event studies for large spillover projects
results_large_spillover <- list()
for (outcome in spillover_outcomes) {
  cat("Event study for", outcome, "| Large spillover projects\n")
  
  results <- did_event_study(
    input_data = tract_data_large_spillover,
    outcome_var = outcome,
    treatment_group = "inner"
  )
  
  results_large_spillover[[paste0(outcome, "_inner")]] <- results$twfe_conley
}

# Create spillover size comparison plots
spillover_size_labels <- c(
  "black_share" = "Black Population Share",
  "asinh_pop_black" = "Log Black Population",
  "asinh_pop_white" = "Log White Population", 
  "asinh_median_rent_calculated" = "Log Median Rent",
  "asinh_median_income" = "Log Median Income"
)

# Small vs Large spillover projects (tertile approach)
p_small_vs_large_spillover <- create_heterogeneity_plot(
  results_list1 = results_small_spillover,
  results_list2 = results_large_spillover,
  label1 = "Small Nearby Projects",
  label2 = "Large Nearby Projects", 
  outcomes_to_plot = c("black_share", "asinh_pop_black", "asinh_pop_white", "asinh_median_income"),
  outcome_labels_map = spillover_size_labels,
  title_text = "Spillover Effects by Nearby Project Size: Small vs Large",
  colors = c("#4575b4", "#d73027"),
  save_name = "spillover_small_vs_large_tertiles_t20", 
  results_dir = results_dir
)

cat("\n=== PROJECT SIZE SPILLOVER HETEROGENEITY COMPLETE ===\n")
p_small_vs_large_spillover

# Create dose-response plot with all three tertile categories
p_spillover_dose_response <- create_heterogeneity_plot(
  results_small_spillover, "Small (50-218 units)",
  results_medium_spillover, "Medium (218-525 units)", 
  results_large_spillover, "Large (525+ units)",
  outcomes_to_plot = c("asinh_median_income", "asinh_median_rent_calculated", "black_share"),
  outcome_labels_map = spillover_size_labels,
  title_text = "Spillover Income Effects by Nearby Project Size: Dose-Response",
  colors = c("#4575b4", "#fdae61", "#d73027"),
  save_name = "spillover_income_dose_response_tertiles_t20",
  results_dir = results_dir
)

cat("\n=== DOSE-RESPONSE PLOT CREATED ===\n")
cat("Income spillover effects at t=+20:\n")
cat("Small projects (50-218): -5.9% (p=0.068)\n") 
cat("Medium projects (218-525): -7.9% (p=0.038)\n")
cat("Large projects (500+): -10.6% (p=0.006)\n")



# Standard heterogeneity analysis in pooled DiD-----


## Heterogeneity by t=0 project characteristics -----
# post-period total PH units
ph_characteristics_t0 <- 
  tract_data_matched_1_year %>%
  filter(event_time == 0, group_type == "treated",
         location_type == "treated") %>% 
  # calculate project racial share
  mutate(ph_black_share_t0 = black_public_housing_pop_estimate/total_public_housing_pop_estimate) %>% 
  select(match_group, total_public_housing_units, ph_black_share_t0) %>% 
  dplyr::rename(total_public_housing_units_t0 = total_public_housing_units) %>%
  mutate(log_total_public_housing_units_t0 = log(total_public_housing_units_t0),
         ph_units_t0_bin = ntile(total_public_housing_units_t0, 3),
         ph_units_t0_bin = factor(ph_units_t0_bin),
         # Median split for units
         ph_units_median = median(total_public_housing_units_t0, na.rm = TRUE),
         ph_units_above_median = ifelse(total_public_housing_units_t0 >= ph_units_median, "Above Median", "Below Median"),
         ph_units_above_median = factor(ph_units_above_median, levels = c("Below Median", "Above Median")),
         # Manual binning of black share in the projects
           ph_black_share_t0_cat = case_when(
             ph_black_share_t0 == 0 ~ "None",
             ph_black_share_t0 < 0.5 ~ "Some",
             TRUE ~ "Majority"
           ) %>% factor(levels = c("None", "Some", "Majority"))
         ) 
         
  
tract_data_matched_1_year <- 
  left_join(tract_data_matched_1_year, ph_characteristics_t0, by = "match_group")


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
    filter(event_time > -40, event_time <= 30, event_time > -30)
  
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
      input_data = tract_data_matched_1_year,
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
      input_data = tract_data_matched_1_year,
      outcome_var = outcome,
      treatment_group = group,
      het_var = "ph_units_t0_bin"
    )

    results_het_logunits_bin[[paste0(outcome, "_", group)]] <- results
  }
}

# Median split analysis
results_het_median_split <- list()

for (outcome in outcome_variables) {
  for (group in "treated") {
    cat("Median split DiD for", outcome, "|", group, "\n")

    results <- run_heterogeneity_did_ph(
      input_data = tract_data_matched_1_year,
      outcome_var = outcome,
      treatment_group = group,
      het_var = "ph_units_above_median"
    )

    results_het_median_split[[paste0(outcome, "_", group)]] <- results
  }
}

results_ph_black_share <- list()

for (outcome in outcome_variables) {
  for (group in "treated") {
    cat("Heterogeneity DiD for", outcome, "|", group, "\n")
    
    results <- run_heterogeneity_did_ph(
      input_data = tract_data_matched_1_year,
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
      input_data = tract_data_matched_1_year,
      outcome_var = outcome,
      treatment_group = group,
      het_var = "ph_black_share_t0_cat" 
    )
    
    results_ph_black_share_bin[[paste0(outcome, "_", group)]] <- results
  }
}



## Heterogeneity by t=-10 neighborhood characteristics -----
tract_data_matched_1_year <-
  tract_data_matched_1_year %>% 
  select(-contains("_tminus"))

run_heterogeneity_did <- function(input_data,
                                  outcome_var,
                                  treatment_group = "treated",
                                  het_var,
                                  het_var_time = -10,
                                  use_bspline = FALSE,
                                  bs_df = 3,
                                  event_time_min = -30,
                                  event_time_max = 30) {
  
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
                                  event_time_max = 30) {
  
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
        input_data = tract_data_matched_1_year,
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


# Individual Treatment effects ---------
## Project-specific DiD ----- 

estimate_project_did <- function(data, outcome_var) {
  # # testing 
  # data <- tract_data_matched_1_year
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
  estimate_project_did(tract_data_matched_1_year, outcome)
})


## E
# pre-period characteristics
pre_period_treated_characteristics <- 
  tract_data_matched_1_year %>%
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
