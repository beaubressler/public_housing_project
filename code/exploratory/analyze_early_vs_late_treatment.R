# Analyze Early vs Late Treatment Timing
# Compare baseline characteristics of tracts that got projects earlier vs later

library(here)
library(tidyverse)
library(sf)

# Load balanced dataset
data <- st_read(here("data", "derived", "merged", "combined",
                     "census_tract_sample_with_treatment_status_balanced.gpkg"))

# Remove geometry for easier analysis
data_df <- data %>% st_drop_geometry()

# Load treated tracts panel to get treatment years
treated_tracts <- st_read(here("data", "derived", "merged", "combined",
                               "treated_tracts_panel_balanced.gpkg"))

# Remove geometry and examine structure
treated_df <- treated_tracts %>% st_drop_geometry()

cat("Treated tracts data shape:", nrow(treated_df), "rows,", ncol(treated_df), "cols\n")
cat("Years available:", sort(unique(treated_df$YEAR)), "\n")

# Check for treatment year variables
treatment_year_cols <- names(treated_df)[grepl("year|treat", names(treated_df), ignore.case = TRUE)]
cat("Treatment-related columns:\n")
print(treatment_year_cols)

# Look at first few rows
cat("\nFirst few treated tracts:\n")
print(head(treated_df %>% select(GISJOIN_1950, YEAR, contains("year"), contains("treat"))))

# Get unique treatment years per tract
treatment_timing <- treated_df %>%
  select(GISJOIN_1950, treatment_year) %>%
  distinct()

cat("\nTreatment year distribution:\n")
print(table(treatment_timing$treatment_year))

# Merge with 1940 baseline data from main dataset
baseline_1940 <- data_df %>%
  filter(YEAR == 1940) %>%
  select(GISJOIN_1950, black_share, white_share, median_income, median_rent_calculated,
         total_pop, population_density, unemp_rate, distance_from_cbd,
         redlined_binary_80pp, cbsa_title)

# Merge treatment timing with 1940 baseline characteristics
analysis_data <- treatment_timing %>%
  inner_join(baseline_1940, by = "GISJOIN_1950")

cat("\nNumber of tracts with both treatment year and 1940 baseline data:", nrow(analysis_data), "\n")

# Define early vs late treatment groups (using median year)
median_treatment_year <- median(analysis_data$treatment_year, na.rm = TRUE)
cat("Median treatment year:", median_treatment_year, "\n")

analysis_data <- analysis_data %>%
  mutate(
    treatment_timing_group = case_when(
      treatment_year <= median_treatment_year ~ "Early Treatment",
      treatment_year > median_treatment_year ~ "Late Treatment",
      TRUE ~ NA_character_
    )
  )

cat("\nDistribution of early vs late treatment:\n")
print(table(analysis_data$treatment_timing_group, useNA = "always"))

# Show treatment years by group
cat("\nTreatment years by group:\n")
early_years <- analysis_data %>% filter(treatment_timing_group == "Early Treatment") %>% pull(treatment_year)
late_years <- analysis_data %>% filter(treatment_timing_group == "Late Treatment") %>% pull(treatment_year)

cat("Early treatment years:", sort(unique(early_years)), "\n")
cat("Late treatment years:", sort(unique(late_years)), "\n")

# Compare baseline characteristics
cat("\n=== COMPARISON OF 1940 BASELINE CHARACTERISTICS ===\n")

comparison_vars <- c("black_share", "white_share", "median_income", "median_rent_calculated",
                     "total_pop", "population_density", "unemp_rate", "distance_from_cbd")

# Summary statistics by group
for(var in comparison_vars) {
  cat("\n", var, ":\n")

  # Calculate means and basic stats
  stats <- analysis_data %>%
    filter(!is.na(!!sym(var))) %>%
    group_by(treatment_timing_group) %>%
    summarise(
      n = n(),
      mean = mean(!!sym(var), na.rm = TRUE),
      median = median(!!sym(var), na.rm = TRUE),
      sd = sd(!!sym(var), na.rm = TRUE),
      .groups = "drop"
    )
  print(stats)

  # T-test
  early_vals <- analysis_data %>%
    filter(treatment_timing_group == "Early Treatment", !is.na(!!sym(var))) %>%
    pull(!!sym(var))
  late_vals <- analysis_data %>%
    filter(treatment_timing_group == "Late Treatment", !is.na(!!sym(var))) %>%
    pull(!!sym(var))

  if(length(early_vals) > 0 & length(late_vals) > 0) {
    t_result <- t.test(early_vals, late_vals)
    cat("  T-test p-value:", round(t_result$p.value, 4), "\n")
    cat("  Difference (Early - Late):", round(t_result$estimate[1] - t_result$estimate[2], 4), "\n")
  }
}

# Binary variable: redlining
cat("\nRedlining (binary):\n")
redline_table <- table(analysis_data$treatment_timing_group, analysis_data$redlined_binary_80pp)
print(redline_table)

# Chi-square test for redlining
if(all(redline_table > 0)) {
  chi_result <- chisq.test(redline_table)
  cat("Chi-square p-value:", round(chi_result$p.value, 4), "\n")

  # Calculate proportions
  prop_table <- prop.table(redline_table, margin = 1)
  cat("Proportion redlined by group:\n")
  print(round(prop_table, 3))
}

# Show some examples
cat("\nExamples of early treatment tracts (1940 baseline):\n")
early_examples <- analysis_data %>%
  filter(treatment_timing_group == "Early Treatment") %>%
  arrange(treatment_year) %>%
  select(treatment_year, cbsa_title, black_share, median_income, redlined_binary_80pp) %>%
  slice_head(n = 5)
print(early_examples)

cat("\nExamples of late treatment tracts (1940 baseline):\n")
late_examples <- analysis_data %>%
  filter(treatment_timing_group == "Late Treatment") %>%
  arrange(desc(treatment_year)) %>%
  select(treatment_year, cbsa_title, black_share, median_income, redlined_binary_80pp) %>%
  slice_head(n = 5)
print(late_examples)