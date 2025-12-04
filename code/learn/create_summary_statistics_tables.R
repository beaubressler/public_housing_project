####
# Create Summary Statistics Tables
#
# This script creates comprehensive summary statistics for:
# 1. Census tract characteristics (1940 vs 1990)
# 2. Public housing project characteristics
####

# Preliminaries ----
library(tidyverse)
library(sf)
library(here)
library(tinytable)
library(patchwork)

# Load data ----
# Census tract data (balanced panel)
tract_data <- st_read(
  here("data", "derived", "merged", "combined",
       "census_tract_sample_with_treatment_status_balanced.gpkg"),
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  # Drop tracts with area > 10 km² (geographic outliers, 13 tracts = 0.2%)
  mutate(area_km2 = area_m2 / 1000000) %>%
  filter(area_km2 <= 10) %>%
  select(-area_km2)

# Public housing project data - full cleaned dataset
project_data_full <- st_read(
  here("data", "derived", "public_housing", "working", "cleaned_housing_projects.gpkg"),
  quiet = TRUE
) %>%
  st_drop_geometry()

# Sample of projects in balanced panel tracts (ALL projects for documentation)
project_data_sample <- st_read(
  here("data", "derived", "public_housing", "working", "combined", "public_housing_sample_balanced.gpkg"),
  quiet = TRUE
) %>%
  st_drop_geometry()

# Projects used in main DiD analysis (first-year treatment only)
project_data_analysis <- st_read(
  here("data", "derived", "public_housing", "working", "combined", "public_housing_sample_balanced_first_year.gpkg"),
  quiet = TRUE
) %>%
  st_drop_geometry()

# Helper function for summary stats ----
create_summary_stats <- function(data, vars, year_label = NULL) {
  data %>%
    select(all_of(vars)) %>%
    summarise(across(
      everything(),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        min = ~if(all(is.na(.x))) NA_real_ else min(.x, na.rm = TRUE),
        max = ~if(all(is.na(.x))) NA_real_ else max(.x, na.rm = TRUE),
        n = ~sum(!is.na(.x))
      ),
      .names = "{.col}_{.fn}"
    )) %>%
    pivot_longer(
      everything(),
      names_to = c("variable", "statistic"),
      names_pattern = "(.+)_(.+)$"
    ) %>%
    pivot_wider(
      names_from = statistic,
      values_from = value
    ) %>%
    mutate(year = year_label, .before = 1)
}

# 1. Census Tract Summary Statistics ----

# Define variable groups
# Time-invariant geographic characteristics
geographic_vars_constant <- c(
  "area_km2",
  "distance_from_cbd_km"
)

# Time-varying variables
population_vars <- c(
  "total_pop",
  "population_density"
)

demographic_vars <- c(
  "black_share",
  "other_share"
)

economic_vars <- c(
  "median_income",
  "median_rent_calculated",
  "unemp_rate"
)

housing_vars <- c(
  "median_home_value_calculated",
  "share_needing_repair"
)

education_vars <- c(
  "pct_hs_grad"
)


# Summary stats for time-invariant variables (just use 1940)
tract_summary_constant <- tract_data %>%
  filter(YEAR == 1940) %>%
  mutate(
    area_km2 = area_m2 / 1000000,
    distance_from_cbd_km = distance_from_cbd / 1000
  ) %>%
  create_summary_stats(geographic_vars_constant, year_label = "Constant")

# Create summary stats for 1940 and 2010 (time-varying)
tract_summary_1940 <- tract_data %>%
  filter(YEAR == 1940) %>%
  create_summary_stats(
    c(population_vars, demographic_vars, economic_vars,
      housing_vars, education_vars),
    year_label = "1940"
  )

tract_summary_2010 <- tract_data %>%
  filter(YEAR == 2010) %>%
  create_summary_stats(
    c(population_vars, demographic_vars, economic_vars,
      housing_vars, education_vars),
    year_label = "2010"
  )

# Combine all years
tract_summary_combined <- bind_rows(
  tract_summary_constant,
  tract_summary_1940,
  tract_summary_2010
) %>%
  arrange(variable, year)

# 2. Public Housing Project Summary Statistics ----

# Define project summary variables
project_summary_vars <- c(
  "total_public_housing_units",
  "treatment_year",
  "proj_total_population_estimate",
  "proj_black_share"
)

# Helper function to count non-missing observations
count_non_missing <- function(x) sum(!is.na(x))

# Full dataset (50+ units, treated after 1940)
project_summary_full <- project_data_full %>%
  filter(
    total_public_housing_units >= 50,
    treatment_year > 1940,
    !is.na(total_public_housing_units)
  ) %>%
  create_summary_stats(project_summary_vars, year_label = "Full dataset")


# Analysis sample (projects in balanced panel tracts - ALL projects)
project_summary_sample <- project_data_sample %>%
  filter(
    total_public_housing_units >= 50,
    treatment_year > 1940,
    !is.na(total_public_housing_units)
  ) %>%
  create_summary_stats(project_summary_vars, year_label = "Balanced sample (all)")

# Analysis sample (projects used in DiD - first-year only)
project_summary_analysis <- project_data_analysis %>%
  filter(
    total_public_housing_units >= 50,
    treatment_year > 1940,
    !is.na(total_public_housing_units)
  ) %>%
  create_summary_stats(project_summary_vars, year_label = "DiD analysis (first-year)")

# Combine for comparison
project_summary_comparison <- bind_rows(
  project_summary_full,
  project_summary_sample,
  project_summary_analysis
) %>%
  arrange(variable, year) %>%
  mutate(across(c(mean, median, sd, min, max), ~round(.x, 2)))

# Data source breakdown
project_source_summary <- project_data_sample %>%
  filter(
    total_public_housing_units >= 50,
    treatment_year > 1940,
    !is.na(total_public_housing_units)
  ) %>%
  group_by(source) %>%
  summarise(
    n_projects = n(),
    total_units = sum(total_public_housing_units, na.rm = TRUE),
    mean_units = mean(total_public_housing_units, na.rm = TRUE),
    median_units = median(total_public_housing_units, na.rm = TRUE)
  ) %>%
  arrange(desc(n_projects)) %>%
  mutate(across(c(mean_units, median_units), ~round(.x, 2)))

# Building type breakdown (using concorded variable from all sources)
building_type_summary <- project_data_sample %>%
  filter(
    total_public_housing_units >= 50,
    treatment_year > 1940,
    !is.na(total_public_housing_units)
  ) %>%
  mutate(building_type_concorded = if_else(is.na(building_type_concorded), "Missing", building_type_concorded)) %>%
  group_by(building_type_concorded) %>%
  summarise(
    n_projects = n(),
    total_units = sum(total_public_housing_units, na.rm = TRUE),
    mean_units = mean(total_public_housing_units, na.rm = TRUE),
    median_units = median(total_public_housing_units, na.rm = TRUE)
  ) %>%
  arrange(desc(n_projects)) %>%
  mutate(across(c(mean_units, median_units), ~round(.x, 2)))

# Treatment timing distribution
# Get treated tracts and their first treatment year
treatment_timing_data <- tract_data %>%
  filter(treated == 1, !is.na(num_years)) %>%
  group_by(GISJOIN_1950) %>%
  # Get first treatment year for each tract
  summarise(first_treatment_year = min(YEAR[num_years > 0], na.rm = TRUE)) %>%
  filter(is.finite(first_treatment_year))

# By decade (for table)
treatment_timing <- treatment_timing_data %>%
  group_by(decade = floor(first_treatment_year / 10) * 10) %>%
  summarise(
    n_tracts = n(),
    share_tracts = n() / nrow(treatment_timing_data)
  ) %>%
  mutate(share_tracts = round(share_tracts, 2))

# By year (for graph) - using project completion years - BALANCED SAMPLE
treatment_timing_by_year <- project_data_sample %>%
  filter(
    total_public_housing_units >= 50,
    treatment_year > 1940,
    treatment_year <= 1980,
    !is.na(treatment_year)
  ) %>%
  group_by(year_completed) %>%
  summarise(
    n_projects = n(),
    total_units = sum(total_public_housing_units, na.rm = TRUE)
  ) %>%
  arrange(year_completed)

# By year (for graph) - FULL SAMPLE (not restricted to balanced panel)
treatment_timing_by_year_full <- project_data_full %>%
  filter(
    total_public_housing_units >= 50,
    treatment_year > 1940,
    treatment_year <= 1980,
    !is.na(treatment_year)
  ) %>%
  group_by(year_completed) %>%
  summarise(
    n_projects = n(),
    total_units = sum(total_public_housing_units, na.rm = TRUE)
  ) %>%
  arrange(year_completed)

# Create treatment timing graph - paper version (with title)
treatment_timing_plot <- ggplot(treatment_timing_by_year, aes(x = year_completed, y = n_projects)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(
    x = "Year of Project Completion",
    y = "Number of Projects",
    title = "Public Housing Project Completions by Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Slide version (no title)
treatment_timing_plot_slides <- ggplot(treatment_timing_by_year, aes(x = year_completed, y = n_projects)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(
    x = "Year of Project Completion",
    y = "Number of Projects"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Units version - paper (with title)
treatment_timing_units_plot <- ggplot(treatment_timing_by_year, aes(x = year_completed, y = total_units)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(
    x = "Year of Project Completion",
    y = "Total Housing Units",
    title = "Public Housing Units Completed by Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Units version - slides (no title)
treatment_timing_units_plot_slides <- ggplot(treatment_timing_by_year, aes(x = year_completed, y = total_units)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(
    x = "Year of Project Completion",
    y = "Total Housing Units"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Combined plots using patchwork - BALANCED SAMPLE
# Paper version with title
treatment_timing_combined_plot <- treatment_timing_plot / treatment_timing_units_plot +
  plot_annotation(title = "Public Housing Project Completions by Year (Analysis Sample)")

# Slides version without title (horizontal layout)
treatment_timing_combined_plot_slides <- treatment_timing_plot_slides | treatment_timing_units_plot_slides

## FULL SAMPLE plots (not restricted to balanced panel)
# Projects plot - paper
treatment_timing_plot_full <- ggplot(treatment_timing_by_year_full, aes(x = year_completed, y = n_projects)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(
    x = "Year of Project Completion",
    y = "Number of Projects",
    title = "Public Housing Project Completions by Year (Full Sample)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Projects plot - slides
treatment_timing_plot_full_slides <- ggplot(treatment_timing_by_year_full, aes(x = year_completed, y = n_projects)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(
    x = "Year of Project Completion",
    y = "Number of Projects"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Units plot - paper
treatment_timing_units_plot_full <- ggplot(treatment_timing_by_year_full, aes(x = year_completed, y = total_units)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(
    x = "Year of Project Completion",
    y = "Total Housing Units",
    title = "Public Housing Units Completed by Year (Full Sample)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Units plot - slides
treatment_timing_units_plot_full_slides <- ggplot(treatment_timing_by_year_full, aes(x = year_completed, y = total_units)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  labs(
    x = "Year of Project Completion",
    y = "Total Housing Units"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Combined plots - full sample
treatment_timing_combined_plot_full <- treatment_timing_plot_full / treatment_timing_units_plot_full +
  plot_annotation(title = "Public Housing Project Completions by Year (Full Sample)")

treatment_timing_combined_plot_full_slides <- treatment_timing_plot_full_slides | treatment_timing_units_plot_full_slides

# 3. Create formatted tables ----

# Tract summary table
tract_table <- tract_summary_combined %>%
  # Filter out rows with all NaN/Inf values (variables not available in all years)
  filter(!(is.nan(mean) | is.infinite(mean))) %>%
  mutate(across(c(mean, median, sd, min, max), ~round(.x, 2))) %>%
  # Convert population variables to integers (no decimals)
  mutate(across(c(mean, median, sd, min, max),
                ~if_else(variable == "total_pop", as.numeric(as.integer(.x)), .x))) %>%
  mutate(
    variable = case_when(
      variable == "area_km2" ~ "Tract area (sq km)",
      variable == "distance_from_cbd_km" ~ "Distance from CBD (km)",
      variable == "total_pop" ~ "Total population",
      variable == "population_density" ~ "Population density (per sq m)",
      variable == "black_share" ~ "Black share",
      variable == "foreign_white_share" ~ "Foreign-born white share",
      variable == "median_income" ~ "Median income (1000s, 2000\\$)",
      variable == "median_rent_calculated" ~ "Median rent (2000\\$)",
      variable == "unemp_rate" ~ "Unemployment rate",
      variable == "median_home_value_calculated" ~ "Median home value (1000s, 2000\\$)",
      variable == "vacancy_rate" ~ "Vacancy rate",
      variable == "share_needing_repair" ~ "Share needing repair",
      variable == "share_no_water" ~ "Share without water",
      variable == "pct_hs_grad" ~ "High school graduate \\%",
      variable == "pct_some_college" ~ "Some college \\%",
      variable == "median_educ_years_25plus" ~ "Median education (years)",
      variable == "high_skill_share" ~ "High skill occupation share",
      variable == "mid_skill_share" ~ "Mid skill occupation share",
      variable == "low_skill_share" ~ "Low skill occupation share",
      TRUE ~ variable
    )
  ) %>%
  select(Year = year, Variable = variable, Mean = mean, Median = median,
         SD = sd, `Min.` = min, `Max.` = max)

# 4. Create slide-friendly versions ----

# Compact tract summary for slides (key variables only)
tract_table_slides <- tract_summary_combined %>%
  filter(variable %in% c(
    "area_km2",
    "total_pop",
    "black_share"
  )) %>%
  filter(!(is.nan(mean) | is.infinite(mean))) %>%
  mutate(across(c(median, mean, sd, min, max), ~round(.x, 2))) %>%
  # Convert population variables to integers (no decimals)
  mutate(across(c(mean, median, sd, min, max),
                ~if_else(variable == "total_pop", as.numeric(as.integer(.x)), .x))) %>%
  mutate(
    variable = case_when(
      variable == "area_km2" ~ "Tract area (sq km)",
      variable == "total_pop" ~ "Total population",
      variable == "black_share" ~ "Black share",
      TRUE ~ variable
    ),
    # Add ordering factor to control display order
    variable_order = case_when(
      variable == "Tract area (sq km)" ~ 1,
      variable == "Total population" ~ 2,
      variable == "Black share" ~ 3,
      TRUE ~ 99
    )
  ) %>%
  arrange(variable_order, year) %>%
  select(Year = year, Variable = variable, Mean = mean, Median = median, SD = sd, `Min.` = min, `Max.` = max)

# City-level summary for slides (top 10 by total units) - using full sample
project_summary_by_city <- project_data_full %>%
  filter(
    total_public_housing_units >= 50,
    treatment_year > 1940,
    !is.na(total_public_housing_units),
    !is.na(locality)
  ) %>%
  mutate(locality = str_to_title(locality)) %>%
  group_by(locality) %>%
  summarise(
    n_projects = n(),
    total_units = sum(total_public_housing_units, na.rm = TRUE),
    mean_units = mean(total_public_housing_units, na.rm = TRUE),
    median_units = median(total_public_housing_units, na.rm = TRUE)
  ) %>%
  arrange(desc(total_units)) %>%
  head(10) %>%
  mutate(across(c(mean_units, median_units), ~round(.x, 0))) %>%
  select(City = locality, `N Projects` = n_projects, `Total Units` = total_units,
         `Mean Units` = mean_units, `Median Units` = median_units)

# Load table utilities for removing wrappers
source(here("code", "helpers", "table_utilities.R"))

# Save tables and figures ----
output_dir <- here("output", "tables", "summary_statistics")
slides_dir <- here("output", "tables", "summary_statistics", "slides")
figures_dir <- here("output", "figures", "summary_statistics")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(slides_dir)) dir.create(slides_dir, recursive = TRUE)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

# Save as CSV - full tables
write_csv(tract_table, here(output_dir, "tract_summary_statistics_1940_2010.csv"))
write_csv(project_summary_comparison, here(output_dir, "project_summary_comparison.csv"))
write_csv(project_source_summary, here(output_dir, "project_source_summary.csv"))
write_csv(building_type_summary, here(output_dir, "building_type_summary.csv"))
write_csv(treatment_timing, here(output_dir, "treatment_timing_distribution.csv"))

# Save as CSV - slides versions
write_csv(tract_table_slides, here(slides_dir, "tract_summary_statistics_slides.csv"))
write_csv(treatment_timing, here(slides_dir, "treatment_timing_distribution.csv"))

# Save as tinytable (LaTeX) - full tables
tract_table |>
  tt() |>
  theme_tt(theme = "tabular") |>
  save_tt(here(output_dir, "tract_summary_statistics_1940_2010.tex"), overwrite = TRUE)
remove_table_wrappers(here(output_dir, "tract_summary_statistics_1940_2010.tex"))

# Post-process to remove .00 from Total population rows
full_table_path <- here(output_dir, "tract_summary_statistics_1940_2010.tex")
full_table_content <- readLines(full_table_path)
# Apply gsub in a loop to catch all .00 occurrences in Total population rows
for (i in 1:10) {
  full_table_content <- gsub("(Total population[^\\\\]*?)(\\d+)\\.00", "\\1\\2", full_table_content, perl = TRUE)
}
writeLines(full_table_content, full_table_path)

project_summary_comparison |>
  tt() |>
  theme_tt(theme = "tabular") |>
  save_tt(here(output_dir, "project_summary_comparison.tex"), overwrite = TRUE)

project_source_summary |>
  tt() |>
  theme_tt(theme = "tabular") |>
  save_tt(here(output_dir, "project_source_summary.tex"), overwrite = TRUE)

building_type_summary |>
  tt() |>
  theme_tt(theme = "tabular") |>
  save_tt(here(output_dir, "building_type_summary.tex"), overwrite = TRUE)

treatment_timing |>
  tt() |>
  theme_tt(theme = "tabular") |>
  save_tt(here(output_dir, "treatment_timing_distribution.tex"), overwrite = TRUE)

# Save as tinytable (LaTeX) - slides versions
tract_table_slides |>
  tt() |>
  style_tt(fontsize = 0.8) |>
  theme_tt(theme = "tabular") |>
  save_tt(here(slides_dir, "tract_summary_statistics_slides.tex"), overwrite = TRUE)

# Post-process to remove .00 from Total population rows
slides_table_path <- here(slides_dir, "tract_summary_statistics_slides.tex")
slides_table_content <- readLines(slides_table_path)
# Apply gsub in a loop to catch all .00 occurrences in Total population rows
for (i in 1:10) {
  slides_table_content <- gsub("(Total population[^\\\\]*?)(\\d+)\\.00", "\\1\\2", slides_table_content, perl = TRUE)
}
writeLines(slides_table_content, slides_table_path)

project_summary_by_city |>
  tt() |>
  style_tt(fontsize = 0.8) |>
  theme_tt(theme = "tabular") |>
  save_tt(here(slides_dir, "project_summary_by_city.tex"), overwrite = TRUE)

treatment_timing |>
  tt() |>
  style_tt(fontsize = 0.9) |>
  theme_tt(theme = "tabular") |>
  save_tt(here(slides_dir, "treatment_timing_distribution.tex"), overwrite = TRUE)

# Save figures - paper versions
ggsave(
  here(figures_dir, "treatment_timing_by_year.pdf"),
  plot = treatment_timing_plot,
  width = 10, height = 6
)

ggsave(
  here(figures_dir, "treatment_timing_units_by_year.pdf"),
  plot = treatment_timing_units_plot,
  width = 10, height = 6
)

ggsave(
  here(figures_dir, "treatment_timing_combined.pdf"),
  plot = treatment_timing_combined_plot,
  width = 10, height = 10
)

# Full sample combined plot
ggsave(
  here(figures_dir, "treatment_timing_combined_full.pdf"),
  plot = treatment_timing_combined_plot_full,
  width = 10, height = 10
)

# Save figures - slides versions
slides_figures_dir <- here("output", "figures", "summary_statistics", "slides")
if (!dir.exists(slides_figures_dir)) dir.create(slides_figures_dir, recursive = TRUE)

ggsave(
  here(slides_figures_dir, "treatment_timing_by_year.pdf"),
  plot = treatment_timing_plot_slides,
  width = 10, height = 6
)

ggsave(
  here(slides_figures_dir, "treatment_timing_units_by_year.pdf"),
  plot = treatment_timing_units_plot_slides,
  width = 10, height = 6
)

ggsave(
  here(slides_figures_dir, "treatment_timing_combined.pdf"),
  plot = treatment_timing_combined_plot_slides,
  width = 12, height = 5
)

# Full sample combined plot - slides
ggsave(
  here(slides_figures_dir, "treatment_timing_combined_full.pdf"),
  plot = treatment_timing_combined_plot_full_slides,
  width = 12, height = 5
)

# Print summary to console
cat("\n=== Census Tract Summary Statistics (1940 vs 2010) ===\n")
print(tract_table)

cat("\n\n=== Public Housing Project Summary: Full vs Analysis Sample ===\n")
print(project_summary_comparison)

cat("\n\n=== Project Data Sources ===\n")
print(project_source_summary)

cat("\n\n=== Building Type Summary ===\n")
print(building_type_summary)

cat("\n\n=== Treatment Timing Distribution ===\n")
print(treatment_timing)

cat("\n\n=== Slides Versions ===\n")
cat("\nTract summary (slides):\n")
print(tract_table_slides)

cat("\n\nTables saved to:", output_dir, "\n")
cat("Slides tables saved to:", slides_dir, "\n")

# 5. Pre-Matching Balance Table ----
cat("\n\n=== PRE-MATCHING BALANCE TABLE ===\n")

library(tableone)

# Get treatment assignments
tracts_and_rings <- read_csv(
  here("data", "derived", "merged", "combined", "unique_tracts_and_rings.csv"),
  show_col_types = FALSE
)

# 1940 data with treatment group labels
balance_data <- tract_data %>%
  filter(YEAR == 1940) %>%
  left_join(tracts_and_rings, by = "GISJOIN_1950") %>%
  mutate(
    group = case_when(
      location_type == "treated" ~ "Treated",
      location_type != "treated" | is.na(location_type) ~ "Untreated"
    ),
    # Transform variables
    asinh_pop_total = asinh(total_pop),
    asinh_pop_black = asinh(black_pop),
    asinh_median_income = asinh(median_income),
    asinh_median_rent_calculated = asinh(median_rent_calculated),
    asinh_distance_from_cbd = asinh(distance_from_cbd)
  ) %>%
  filter(!is.na(group))

# Variables to compare
balance_vars <- c("asinh_pop_total", "black_share", "asinh_pop_black",
                  "asinh_median_income", "asinh_median_rent_calculated",
                  "unemp_rate", "lfp_rate", "asinh_distance_from_cbd",
                  "share_needing_repair")

# Create balance table
prematching_tab <- CreateTableOne(vars = balance_vars, strata = "group",
                                  data = balance_data, test = TRUE)
print(prematching_tab, smd = TRUE)

# Format for LaTeX
prematching_tab_df <- print(prematching_tab, smd = TRUE, test = TRUE, printToggle = FALSE) %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  filter(Variable != "n") %>%
  select(Variable, Untreated, Treated, SMD, p) %>%
  mutate(
    Variable = recode(Variable,
      "asinh_pop_total (mean (SD))" = "Total Population (asinh)",
      "black_share (mean (SD))" = "Black Share",
      "asinh_pop_black (mean (SD))" = "Black Population (asinh)",
      "asinh_median_income (mean (SD))" = "Median Income (asinh)",
      "asinh_median_rent_calculated (mean (SD))" = "Median Rent (asinh)",
      "unemp_rate (mean (SD))" = "Unemployment Rate",
      "lfp_rate (mean (SD))" = "Labor Force Participation Rate",
      "asinh_distance_from_cbd (mean (SD))" = "Distance from CBD (asinh)",
      "share_needing_repair (mean (SD))" = "Share Needing Major Repairs"
    ),
    # Add stars to SMD based on p-values
    # p-values come as strings like "<0.001", "0.045", etc.
    stars = case_when(
      p == "<0.001" ~ "***",
      TRUE ~ {
        p_num <- suppressWarnings(as.numeric(p))
        case_when(
          is.na(p_num) ~ "",
          p_num < 0.01 ~ "***",
          p_num < 0.05 ~ "**",
          p_num < 0.1 ~ "*",
          TRUE ~ ""
        )
      }
    ),
    SMD = paste0(SMD, stars)
  ) %>%
  select(-p, -stars) %>%
  rename("Std. Diff." = SMD)

# Add N row at top
n_donor <- sum(balance_data$group == "Untreated")
n_treated <- sum(balance_data$group == "Treated")

prematching_tab_df <- bind_rows(
  tibble(Variable = "N (Tracts)", Untreated = as.character(n_donor),
         Treated = as.character(n_treated), `Std. Diff.` = ""),
  prematching_tab_df
)

# Save

prematching_tab_df |>
  tt(caption = "Initial Characteristics: Public Housing vs Non-Public Housing Tracts (1940)") |>
  style_tt(font_size = 0.8) |>
  format_tt(escape = FALSE) |>
  theme_tt(theme = "tabular") |>
  save_tt(file.path(slides_dir, "treated_vs_untreated_balance_1940.tex"), overwrite = TRUE)

cat("Pre-matching balance table saved to:", file.path(slides_dir, "treated_vs_untreated_balance_1940.tex"), "\n")
