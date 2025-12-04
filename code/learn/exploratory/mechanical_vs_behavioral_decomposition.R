## Public Housing Scale Analysis
## This script analyzes the scale of public housing populations relative to
## tract populations to provide context for interpreting treatment effects.
## Key question: How large are PH populations compared to neighborhood populations?
## This provides upper bounds on possible mechanical effects.

library(tidyverse)
library(here)
library(sf)
library(scales)

# Directories
data_dir <- here("data", "derived", "merged", "combined")
matched_data_dir <- here(data_dir, "matched_dataset")
ph_data_dir <- here("data", "derived", "public_housing", "working", "combined")
output_dir <- here("output", "tables")

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load matched tract data
# Using the 2-year specification as it's standard in the analysis
# Note: This dataset already contains public housing population estimates
# (total_public_housing_pop_estimate, black_public_housing_pop_estimate, white_public_housing_pop_estimate)
matched_data <- read_csv(here(matched_data_dir, "tract_data_matched_2_year.csv")) %>%
  mutate(years_since_treatment = year - treatment_year)

# Focus on treated tracts at t=20 (20 years post-treatment)
# This matches the timing of our main DiD estimates
# Filter for observations where:
# 1. treated == 1 (tract has public housing)
# 2. years_since_treatment == 20 (20 years after opening)
# 3. total_public_housing_pop_estimate > 0 (there are residents)
# 4. We have actual racial composition data (not missing/NA)
treated_data <- matched_data %>%
  filter(
    treated == 1,
    years_since_treatment == 20,
    !is.na(total_public_housing_pop_estimate),
    total_public_housing_pop_estimate > 0
  )

cat("\n=== FILTERING TO T=20 (20 YEARS POST-TREATMENT) ===\n")
cat("Total treated tracts at t=20 with PH present:", nrow(treated_data), "\n")

# Check for missing racial composition data
missing_comp <- treated_data %>%
  summarise(
    n_total = n(),
    n_missing_black = sum(is.na(black_public_housing_pop_estimate)),
    n_missing_white = sum(is.na(white_public_housing_pop_estimate)),
    n_zero_black = sum(black_public_housing_pop_estimate == 0, na.rm = TRUE),
    n_has_racial_data = sum(!is.na(black_public_housing_pop_estimate) &
                           !is.na(white_public_housing_pop_estimate))
  )

cat("Racial composition data availability:\n")
cat("  Missing Black pop:", missing_comp$n_missing_black, "\n")
cat("  Missing White pop:", missing_comp$n_missing_white, "\n")
cat("  Zero Black pop (actual data):", missing_comp$n_zero_black, "\n")
cat("  Have racial composition data:", missing_comp$n_has_racial_data, "\n\n")

# Calculate PH scale metrics
# Only use observations where we have racial composition data
ph_scale_data <- treated_data %>%
  filter(
    !is.na(black_public_housing_pop_estimate),
    !is.na(white_public_housing_pop_estimate)
  ) %>%
  mutate(
    # Calculate PH racial composition
    ph_black_share = black_public_housing_pop_estimate / total_public_housing_pop_estimate,
    ph_white_share = white_public_housing_pop_estimate / total_public_housing_pop_estimate,

    # PH population as share of tract population (key metric)
    ph_share_of_tract = total_public_housing_pop_estimate / total_pop,

    # PH Black/White pop as share of tract total pop (for context)
    ph_black_pop_share_of_tract = black_public_housing_pop_estimate / total_pop,
    ph_white_pop_share_of_tract = white_public_housing_pop_estimate / total_pop,

    # Project size categories for stratification
    project_size_category = case_when(
      total_public_housing_units < 200 ~ "50-200 units",
      total_public_housing_units < 500 ~ "200-500 units",
      total_public_housing_units < 1000 ~ "500-1000 units",
      TRUE ~ "1000+ units"
    ),

    # Treatment decade
    treatment_decade = floor(treatment_year / 10) * 10
  )

cat("Sample used for analysis (t=20, with racial composition data):", nrow(ph_scale_data), "tracts\n")
cat("This represents", round(nrow(ph_scale_data)/nrow(treated_data)*100, 1), "% of t=20 treated observations\n\n")

# Summary statistics: Overall
overall_summary <- ph_scale_data %>%
  summarise(
    n_tracts = n(),
    n_unique_tracts = n_distinct(GISJOIN_1950),

    # Public housing racial composition
    mean_ph_black_share = mean(ph_black_share, na.rm = TRUE),
    median_ph_black_share = median(ph_black_share, na.rm = TRUE),
    pct_ph_majority_black = mean(ph_black_share > 0.5, na.rm = TRUE),

    # PH as share of tract population
    mean_ph_share_of_tract = mean(ph_share_of_tract, na.rm = TRUE),
    median_ph_share_of_tract = median(ph_share_of_tract, na.rm = TRUE),

    # PH Black/White pop as share of tract total pop
    mean_ph_black_pop_share = mean(ph_black_pop_share_of_tract, na.rm = TRUE),
    median_ph_black_pop_share = median(ph_black_pop_share_of_tract, na.rm = TRUE),
    mean_ph_white_pop_share = mean(ph_white_pop_share_of_tract, na.rm = TRUE),
    median_ph_white_pop_share = median(ph_white_pop_share_of_tract, na.rm = TRUE)
  ) %>%
  mutate(stratification = "Overall (t=20)")

# Summary by project size
size_summary <- ph_scale_data %>%
  group_by(project_size_category) %>%
  summarise(
    n_tracts = n(),
    n_unique_tracts = n_distinct(GISJOIN_1950),

    # PH racial composition
    mean_ph_black_share = mean(ph_black_share, na.rm = TRUE),
    median_ph_black_share = median(ph_black_share, na.rm = TRUE),

    # PH as share of tract population
    mean_ph_share_of_tract = mean(ph_share_of_tract, na.rm = TRUE),
    median_ph_share_of_tract = median(ph_share_of_tract, na.rm = TRUE),

    # PH Black/White pop as share of tract total pop
    mean_ph_black_pop_share = mean(ph_black_pop_share_of_tract, na.rm = TRUE),
    median_ph_black_pop_share = median(ph_black_pop_share_of_tract, na.rm = TRUE),
    mean_ph_white_pop_share = mean(ph_white_pop_share_of_tract, na.rm = TRUE),
    median_ph_white_pop_share = median(ph_white_pop_share_of_tract, na.rm = TRUE)
  ) %>%
  rename(stratification = project_size_category)

# Summary by city (top cities only)
city_summary <- ph_scale_data %>%
  group_by(cbsa_title) %>%
  summarise(
    n_tracts = n(),
    n_unique_tracts = n_distinct(GISJOIN_1950),

    mean_ph_black_share = mean(ph_black_share, na.rm = TRUE),
    median_ph_black_share = median(ph_black_share, na.rm = TRUE),

    mean_ph_share_of_tract = mean(ph_share_of_tract, na.rm = TRUE),
    median_ph_share_of_tract = median(ph_share_of_tract, na.rm = TRUE),

    mean_ph_black_pop_share = mean(ph_black_pop_share_of_tract, na.rm = TRUE),
    median_ph_black_pop_share = median(ph_black_pop_share_of_tract, na.rm = TRUE),
    mean_ph_white_pop_share = mean(ph_white_pop_share_of_tract, na.rm = TRUE),
    median_ph_white_pop_share = median(ph_white_pop_share_of_tract, na.rm = TRUE)
  ) %>%
  rename(stratification = cbsa_title) %>%
  arrange(desc(n_tracts)) %>%
  head(10)  # Top 10 cities by number of observations

# Summary by treatment decade
decade_summary <- ph_scale_data %>%
  group_by(treatment_decade) %>%
  summarise(
    n_tracts = n(),
    n_unique_tracts = n_distinct(GISJOIN_1950),

    mean_ph_black_share = mean(ph_black_share, na.rm = TRUE),
    median_ph_black_share = median(ph_black_share, na.rm = TRUE),

    mean_ph_share_of_tract = mean(ph_share_of_tract, na.rm = TRUE),
    median_ph_share_of_tract = median(ph_share_of_tract, na.rm = TRUE),

    mean_ph_black_pop_share = mean(ph_black_pop_share_of_tract, na.rm = TRUE),
    median_ph_black_pop_share = median(ph_black_pop_share_of_tract, na.rm = TRUE),
    mean_ph_white_pop_share = mean(ph_white_pop_share_of_tract, na.rm = TRUE),
    median_ph_white_pop_share = median(ph_white_pop_share_of_tract, na.rm = TRUE)
  ) %>%
  mutate(stratification = paste0(treatment_decade, "s")) %>%
  select(stratification, everything(), -treatment_decade)

# Combine all summaries
full_summary <- bind_rows(
  overall_summary,
  size_summary,
  city_summary,
  decade_summary
)

# Print summary
cat("\n=== PUBLIC HOUSING SCALE ANALYSIS ===\n\n")
cat("Overall Summary:\n")
print(overall_summary)

cat("\n\nBy Project Size:\n")
print(size_summary)

cat("\n\nBy City (Top 10):\n")
print(city_summary)

cat("\n\nBy Treatment Decade:\n")
print(decade_summary)

# Save results
write_csv(
  full_summary,
  here(output_dir, "public_housing_scale_summary.csv")
)

cat("\n\nResults saved to:", here(output_dir, "public_housing_scale_summary.csv"), "\n")

# Additional analysis: Distribution plots
cat("\n\nGenerating distribution plots...\n")

# Create figures directory if it doesn't exist
figures_dir <- here("output", "figures")
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# Plot: Distribution of PH as share of tract
p1 <- ggplot(ph_scale_data, aes(x = ph_share_of_tract)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = median(ph_scale_data$ph_share_of_tract, na.rm = TRUE),
             linetype = "dashed", color = "red", size = 1) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Distribution of Public Housing as Share of Tract Population",
    subtitle = "Public housing population / Tract population (t=20)",
    x = "PH Share of Tract",
    y = "Count",
    caption = "Red line = median"
  ) +
  theme_classic()

ggsave(
  here(figures_dir, "ph_share_of_tract_distribution.pdf"),
  p1,
  width = 8,
  height = 6
)

# Plot: PH Black pop as share of tract by project size
p2 <- ggplot(ph_scale_data, aes(x = project_size_category, y = ph_black_pop_share_of_tract)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "PH Black Population as Share of Tract Total Population",
    subtitle = "By project size (t=20)",
    x = "Project Size",
    y = "PH Black Pop / Tract Total Pop"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  here(figures_dir, "ph_black_pop_share_by_size.pdf"),
  p2,
  width = 8,
  height = 6
)

cat("Plots saved to:", figures_dir, "\n")

# ============================================================================
# ANALYSIS 2: PROJECT-LEVEL VS AVERAGE TRACT POPULATIONS
# ============================================================================
# This analysis compares unsplit project populations to average tract populations
# to understand how large projects are relative to typical tracts in general

cat("\n\n=== ANALYSIS 2: PROJECT VS AVERAGE TRACT COMPARISON ===\n\n")

# Load balanced tract data to calculate average tract populations
tract_data <- st_read(here(data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg"), quiet = TRUE)

# Load public housing data with UNSPLIT populations
ph_data <- st_read(here(ph_data_dir, "public_housing_sample_balanced.gpkg"), quiet = TRUE) %>%
  st_drop_geometry()

# Calculate average tract populations by year
avg_tract_pops_by_year <- tract_data %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(
    mean_total_pop = mean(total_pop, na.rm = TRUE),
    median_total_pop = median(total_pop, na.rm = TRUE),
    mean_black_pop = mean(black_pop, na.rm = TRUE),
    median_black_pop = median(black_pop, na.rm = TRUE),
    mean_white_pop = mean(white_pop, na.rm = TRUE),
    median_white_pop = median(white_pop, na.rm = TRUE)
  )

# Calculate average tract populations by city and year
avg_tract_pops_by_city <- tract_data %>%
  st_drop_geometry() %>%
  group_by(cbsa_title, YEAR) %>%
  summarise(
    mean_total_pop = mean(total_pop, na.rm = TRUE),
    median_total_pop = median(total_pop, na.rm = TRUE),
    mean_black_pop = mean(black_pop, na.rm = TRUE),
    median_black_pop = median(black_pop, na.rm = TRUE),
    mean_white_pop = mean(white_pop, na.rm = TRUE),
    median_white_pop = median(white_pop, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare public housing data
ph_analysis <- ph_data %>%
  filter(!is.na(proj_total_population_estimate)) %>%
  mutate(
    # Create project size categories
    project_size_category = case_when(
      total_public_housing_units < 200 ~ "50-200 units",
      total_public_housing_units < 500 ~ "200-500 units",
      total_public_housing_units < 1000 ~ "500-1000 units",
      TRUE ~ "1000+ units"
    ),
    # Treatment decade
    treatment_decade = floor(treatment_year / 10) * 10
  )

# Merge with average tract populations (by year for overall comparison)
ph_with_avg_tracts <- ph_analysis %>%
  left_join(avg_tract_pops_by_year, by = c("treatment_year" = "YEAR")) %>%
  mutate(
    # Calculate ratios: project population / average tract population
    ratio_total_vs_mean_tract = proj_total_population_estimate / mean_total_pop,
    ratio_total_vs_median_tract = proj_total_population_estimate / median_total_pop,
    ratio_black_vs_mean_tract = proj_black_population_estimate / mean_black_pop,
    ratio_black_vs_median_tract = proj_black_population_estimate / median_black_pop,
    ratio_white_vs_mean_tract = proj_white_population_estimate / mean_white_pop,
    ratio_white_vs_median_tract = proj_white_population_estimate / median_white_pop
  )

# Overall summary
overall_proj_summary <- ph_with_avg_tracts %>%
  summarise(
    n_projects = n(),

    # Total population ratios
    mean_ratio_total_vs_mean_tract = mean(ratio_total_vs_mean_tract, na.rm = TRUE),
    median_ratio_total_vs_mean_tract = median(ratio_total_vs_mean_tract, na.rm = TRUE),

    # Black population ratios
    mean_ratio_black_vs_mean_tract = mean(ratio_black_vs_mean_tract, na.rm = TRUE),
    median_ratio_black_vs_mean_tract = median(ratio_black_vs_mean_tract, na.rm = TRUE),

    # White population ratios
    mean_ratio_white_vs_mean_tract = mean(ratio_white_vs_mean_tract, na.rm = TRUE),
    median_ratio_white_vs_mean_tract = median(ratio_white_vs_mean_tract, na.rm = TRUE)
  ) %>%
  mutate(stratification = "Overall")

# Summary by project size
size_proj_summary <- ph_with_avg_tracts %>%
  group_by(project_size_category) %>%
  summarise(
    n_projects = n(),

    mean_ratio_total_vs_mean_tract = mean(ratio_total_vs_mean_tract, na.rm = TRUE),
    median_ratio_total_vs_mean_tract = median(ratio_total_vs_mean_tract, na.rm = TRUE),

    mean_ratio_black_vs_mean_tract = mean(ratio_black_vs_mean_tract, na.rm = TRUE),
    median_ratio_black_vs_mean_tract = median(ratio_black_vs_mean_tract, na.rm = TRUE),

    mean_ratio_white_vs_mean_tract = mean(ratio_white_vs_mean_tract, na.rm = TRUE),
    median_ratio_white_vs_mean_tract = median(ratio_white_vs_mean_tract, na.rm = TRUE)
  ) %>%
  rename(stratification = project_size_category)

# Summary by treatment decade
decade_proj_summary <- ph_with_avg_tracts %>%
  group_by(treatment_decade) %>%
  summarise(
    n_projects = n(),

    mean_ratio_total_vs_mean_tract = mean(ratio_total_vs_mean_tract, na.rm = TRUE),
    median_ratio_total_vs_mean_tract = median(ratio_total_vs_mean_tract, na.rm = TRUE),

    mean_ratio_black_vs_mean_tract = mean(ratio_black_vs_mean_tract, na.rm = TRUE),
    median_ratio_black_vs_mean_tract = median(ratio_black_vs_mean_tract, na.rm = TRUE),

    mean_ratio_white_vs_mean_tract = mean(ratio_white_vs_mean_tract, na.rm = TRUE),
    median_ratio_white_vs_mean_tract = median(ratio_white_vs_mean_tract, na.rm = TRUE)
  ) %>%
  mutate(stratification = paste0(treatment_decade, "s")) %>%
  select(stratification, everything(), -treatment_decade)

# Combine all summaries
full_proj_summary <- bind_rows(
  overall_proj_summary,
  size_proj_summary,
  decade_proj_summary
)

# Print summaries
cat("Overall Summary (Project vs Average Tract):\n")
print(overall_proj_summary)

cat("\n\nBy Project Size:\n")
print(size_proj_summary)

cat("\n\nBy Treatment Decade:\n")
print(decade_proj_summary)

# Save results
write_csv(
  full_proj_summary,
  here(output_dir, "mechanical_project_vs_average_tract.csv")
)

cat("\n\nResults saved to:", here(output_dir, "mechanical_project_vs_average_tract.csv"), "\n")

# Additional plots for Analysis 2
p3 <- ggplot(ph_with_avg_tracts, aes(x = project_size_category, y = ratio_total_vs_mean_tract)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Project Population vs Average Tract Population",
    subtitle = "Ratio of unsplit project population to mean tract population",
    x = "Project Size",
    y = "Project Pop / Mean Tract Pop",
    caption = "Red line = 1 (project equals average tract)"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  here(figures_dir, "project_vs_average_tract_total.pdf"),
  p3,
  width = 8,
  height = 6
)

# Plot comparing black vs white project populations relative to average tracts
p4_data <- ph_with_avg_tracts %>%
  select(project_size_category, ratio_black_vs_mean_tract, ratio_white_vs_mean_tract) %>%
  pivot_longer(
    cols = c(ratio_black_vs_mean_tract, ratio_white_vs_mean_tract),
    names_to = "race",
    values_to = "ratio"
  ) %>%
  mutate(race = ifelse(race == "ratio_black_vs_mean_tract", "Black", "White"))

p4 <- ggplot(p4_data, aes(x = project_size_category, y = ratio, fill = race)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("Black" = "navy", "White" = "orange")) +
  labs(
    title = "Project Population vs Average Tract Population by Race",
    subtitle = "Ratio of project population (by race) to mean tract population (by race)",
    x = "Project Size",
    y = "Project Pop / Mean Tract Pop",
    fill = "Race",
    caption = "Red line = 1 (project equals average tract)"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  here(figures_dir, "project_vs_average_tract_by_race.pdf"),
  p4,
  width = 10,
  height = 6
)

cat("\nAdditional plots saved to:", figures_dir, "\n")

# ============================================================================
# PRESENTATION MATERIALS: PUBLIC HOUSING SCALE
# ============================================================================
# This section creates simple figures showing PH population scale relative to tracts
# to provide context for understanding maximum possible mechanical effects

cat("\n\n=== CREATING PRESENTATION MATERIALS ===\n\n")

# Create presentation output directory
presentation_dir <- here("output", "figures", "presentation")
if (!dir.exists(presentation_dir)) {
  dir.create(presentation_dir, recursive = TRUE)
}

# ---- FIGURE 1: Public Housing Scale by Project Size ----
cat("Generating Figure 1: PH as share of tract population by project size...\n")

size_data <- ph_scale_data %>%
  group_by(project_size_category) %>%
  summarise(
    median_ph_share = median(ph_share_of_tract, na.rm = TRUE),
    n_obs = n()
  ) %>%
  mutate(
    project_size_category = factor(
      project_size_category,
      levels = c("50-200 units", "200-500 units", "500-1000 units", "1000+ units")
    )
  )

p_scale_by_size <- ggplot(size_data, aes(x = project_size_category, y = median_ph_share)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = percent(median_ph_share, accuracy = 0.1)),
            vjust = -0.5, size = 5, fontface = "bold") +
  geom_text(aes(label = paste0("n=", n_obs)),
            vjust = 1.5, size = 4, color = "white") +
  geom_hline(yintercept = 0.20, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 1.5, y = 0.22, label = "20% threshold", color = "red", size = 4) +
  scale_y_continuous(labels = percent, limits = c(0, 1.2), breaks = seq(0, 1.2, 0.2)) +
  labs(
    title = "Public Housing as Share of Tract Population",
    subtitle = "Median share by project size | Provides upper bound for mechanical effects",
    x = "Project Size",
    y = "Median: Public Housing Pop / Tract Pop",
    caption = "Based on t=20 observations (20 years post-treatment)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

ggsave(
  here(presentation_dir, "ph_scale_by_project_size.pdf"),
  p_scale_by_size,
  width = 10,
  height = 7
)

cat("\n=== PRESENTATION MATERIALS COMPLETE ===\n")
cat("Figure saved to:", presentation_dir, "\n")
cat("  - ph_scale_by_project_size.pdf\n\n")

# Return both datasets for further analysis
invisible(list(
  ph_scale_data = ph_scale_data,
  project_vs_avg_tract = ph_with_avg_tracts
))
