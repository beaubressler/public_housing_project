# Modern Staggered Difference-in-Differences using Callaway & Sant'Anna (2021)
# Uses the 'did' package which handles heterogeneous treatment effects properly

# Preliminaries -----
library(tidyverse)
library(did)        # Callaway & Sant'Anna
library(here)
library(ggplot2)

rm(list = ls())

# !! Choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small" or "combined"  
data_type <- "combined"

# File paths -----
merged_data_dir <- here("data", "derived", "merged", data_type)
output_dir <- here("output", "regression_results", "stacked_did", data_type)
figure_dir <- here("output", "figures", "stacked_did", data_type)


# Read data -----
event_study_data_rings <- read_csv(here(merged_data_dir, "event_study_data_rings.csv"))

cat("Original sample size:", nrow(event_study_data_rings), "\n")

# Prepare data for Callaway & Sant'Anna -----
cs_data <- event_study_data_rings %>%
  # Keep only treated tracts for later-treated controls approach
  filter(location_type == "treated") %>%
  # Remove missing outcomes
  filter(!is.na(black_share)) %>%
  # Create unique unit identifier (required by did package)
  mutate(unit_id = as.numeric(factor(GISJOIN_1950))) %>%
  # Clean treatment year (did package needs this clean)
  mutate(
    # Set never-treated units to 0 (not applicable here since we only have treated)
    g = treatment_year,  # treatment cohort
    # Ensure years are numeric
    year = as.numeric(year)
  ) %>%
  # Keep necessary variables
  select(
    unit_id, GISJOIN_1950, year, g, black_share, white_share,
    black_share_baseline, city, STATE, COUNTY, 
    total_pop, median_income, distance_from_cbd
  ) %>%
  # Create balanced panel (did package requires this)
  arrange(unit_id, year)

# Check data structure
cat("CS data sample size:", nrow(cs_data), "\n")
cat("Number of units:", length(unique(cs_data$unit_id)), "\n")
cat("Years:", sort(unique(cs_data$year)), "\n")
cat("Treatment cohorts:", sort(unique(cs_data$g)), "\n")

# Check for balanced panel
panel_check <- cs_data %>%
  group_by(unit_id) %>%
  summarise(n_years = n(), .groups = "drop")

cat("Panel balance check - observations per unit:\n")
print(table(panel_check$n_years))

# Main Callaway & Sant'Anna estimation -----
cat("\nRunning Callaway & Sant'Anna estimation...\n")

# Basic CS estimation with later-treated controls
cs_results <- att_gt(
  yname = "black_share",
  tname = "year", 
  idname = "unit_id",
  gname = "g",
  data = cs_data,
  control_group = "notyettreated",  # Use not-yet-treated as controls
  anticipation = 0,  # No anticipation effects
  clustervars = "unit_id",  # Cluster at unit level
  bstrap = TRUE,  # Bootstrap inference
  cband = TRUE,   # Simultaneous confidence bands
  biters = 1000   # Number of bootstrap iterations
)

# Print basic results  
cat("\nCallaway & Sant'Anna Results:\n")
summary(cs_results)

# Aggregate treatment effects -----

# Overall ATT (average treatment effect on treated)
cs_overall <- aggte(cs_results, type = "simple")
cat("\nOverall ATT:\n")
summary(cs_overall)

# Group-specific ATT (by treatment cohort) 
cs_group <- aggte(cs_results, type = "group")
cat("\nGroup-specific ATT:\n")
summary(cs_group)

# Dynamic effects (event study)
cs_dynamic <- aggte(cs_results, type = "dynamic")
cat("\nDynamic Treatment Effects:\n") 
summary(cs_dynamic)

# Calendar time effects
cs_calendar <- aggte(cs_results, type = "calendar")
cat("\nCalendar Time Effects:\n")
summary(cs_calendar)

# Create plots -----

# Event study plot
dynamic_plot <- ggdid(cs_dynamic) +
  labs(
    title = "Event Study: Effect of Public Housing on Black Share",
    subtitle = "Callaway & Sant'Anna (2021) Estimator",
    x = "Years Relative to Treatment",
    y = "Average Treatment Effect",
    caption = "95% pointwise and simultaneous confidence intervals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11)
  )

# ggsave(
#   file.path(figure_dir, "cs_event_study.pdf"),
#   dynamic_plot,
#   width = 10, height = 6
# )

# Group-specific effects plot
group_plot <- ggdid(cs_group) +
  labs(
    title = "Treatment Effects by Cohort",
    subtitle = "Callaway & Sant'Anna (2021) Estimator", 
    x = "Treatment Cohort",
    y = "Average Treatment Effect"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "cs_group_effects.pdf"),
  group_plot, 
  width = 10, height = 6
)

# Calendar time plot
calendar_plot <- ggdid(cs_calendar) +
  labs(
    title = "Treatment Effects by Calendar Time",
    subtitle = "Callaway & Sant'Anna (2021) Estimator",
    x = "Calendar Year", 
    y = "Average Treatment Effect"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "cs_calendar_effects.pdf"),
  calendar_plot,
  width = 10, height = 6
)

# Save results -----

# Extract coefficients and save
results_list <- list(
  group_time_effects = cs_results,
  overall_att = cs_overall,
  group_att = cs_group,
  dynamic_effects = cs_dynamic,
  calendar_effects = cs_calendar,
  data = cs_data
)

saveRDS(results_list, file.path(output_dir, "cs_results.rds"))

# Create results summary table
results_summary <- bind_rows(
  # Overall ATT
  tibble(
    estimator = "Overall ATT",
    estimate = cs_overall$overall.att,
    se = cs_overall$overall.se,
    pvalue = 2 * (1 - pnorm(abs(cs_overall$overall.att / cs_overall$overall.se)))
  ),
  # Dynamic effects
  tibble(
    estimator = paste("Dynamic, e =", cs_dynamic$egt),
    estimate = cs_dynamic$att.egt,
    se = cs_dynamic$se.egt, 
    pvalue = 2 * (1 - pnorm(abs(cs_dynamic$att.egt / cs_dynamic$se.egt)))
  ),
  # Group effects  
  tibble(
    estimator = paste("Group", cs_group$egt),
    estimate = cs_group$att.egt,
    se = cs_group$se.egt,
    pvalue = 2 * (1 - pnorm(abs(cs_group$att.egt / cs_group$se.egt)))
  )
) %>%
  mutate(
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se
  )

write_csv(results_summary, file.path(output_dir, "cs_results_summary.csv"))

cat("\n=== CALLAWAY & SANT'ANNA RESULTS SUMMARY ===\n")
print(results_summary)

cat("\nResults saved to:", output_dir, "\n") 
cat("Figures saved to:", figure_dir, "\n")