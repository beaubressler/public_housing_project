## Public Housing Sample Description
## Creates tables and figures describing the public housing projects in the sample
## Focuses on: project characteristics, racial composition, size distribution, geographic coverage

library(tidyverse)
library(here)
library(sf)
library(scales)
library(knitr)

# Directories
data_dir <- here("data", "derived")
output_dir <- here("output", "tables")
figures_dir <- here("output", "figures", "presentation")

# Create output directories
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

cat("=== PUBLIC HOUSING SAMPLE DESCRIPTION ===\n\n")

# Load public housing data with full project information
ph_data <- st_read(
  here(data_dir, "public_housing", "working", "combined", "public_housing_sample_balanced.gpkg"),
  quiet = TRUE
) %>%
  st_drop_geometry()

cat("Loaded", nrow(ph_data), "public housing projects\n\n")

# ============================================================================
# TABLE 1: Overall Sample Characteristics
# ============================================================================

cat("Creating Table 1: Overall Sample Characteristics...\n")

overall_stats <- tibble(
  Characteristic = c(
    "Number of projects",
    "Number of cities",
    "Total housing units",
    "Mean units per project",
    "Median units per project",
    "",
    "Treatment period:",
    "  Earliest project",
    "  Latest project",
    "  Median year",
    "",
    "Geographic coverage:",
    "  Number of states"
  ),
  Value = c(
    format(nrow(ph_data), big.mark = ","),
    format(n_distinct(ph_data$locality), big.mark = ","),
    format(sum(ph_data$total_public_housing_units, na.rm = TRUE), big.mark = ","),
    format(round(mean(ph_data$total_public_housing_units, na.rm = TRUE), 0), big.mark = ","),
    format(round(median(ph_data$total_public_housing_units, na.rm = TRUE), 0), big.mark = ","),
    "",
    "",
    as.character(min(ph_data$year_completed, na.rm = TRUE)),
    as.character(max(ph_data$year_completed, na.rm = TRUE)),
    as.character(median(ph_data$year_completed, na.rm = TRUE)),
    "",
    "",
    format(n_distinct(ph_data$state), big.mark = ",")
  )
)

print(overall_stats)
write_csv(overall_stats, here(output_dir, "ph_sample_overall_characteristics.csv"))

# ============================================================================
# TABLE 2: Racial Composition of Projects
# ============================================================================

cat("\n\nCreating Table 2: Racial Composition...\n")

racial_comp <- ph_data %>%
  filter(!is.na(proj_total_population_estimate)) %>%
  mutate(
    has_black_residents = proj_black_population_estimate > 0,
    has_white_residents = proj_white_population_estimate > 0,
    black_share_if_present = if_else(has_black_residents,
                                      proj_black_population_estimate / proj_total_population_estimate,
                                      NA_real_)
  ) %>%
  summarise(
    n_projects = n(),

    # Overall composition
    mean_black_pop = mean(proj_black_population_estimate, na.rm = TRUE),
    mean_white_pop = mean(proj_white_population_estimate, na.rm = TRUE),
    mean_total_pop = mean(proj_total_population_estimate, na.rm = TRUE),

    # Prevalence
    pct_with_black = mean(has_black_residents, na.rm = TRUE) * 100,
    pct_with_white = mean(has_white_residents, na.rm = TRUE) * 100,
    pct_all_black = mean(black_share_if_present == 1, na.rm = TRUE) * 100,
    pct_all_white = mean(proj_white_share == 1, na.rm = TRUE) * 100,
    pct_mixed = mean(has_black_residents & has_white_residents, na.rm = TRUE) * 100,

    # Conditional shares
    mean_black_share_if_present = mean(black_share_if_present, na.rm = TRUE) * 100,
    median_black_share_if_present = median(black_share_if_present, na.rm = TRUE) * 100
  )

racial_comp_table <- tibble(
  Measure = c(
    "Sample size (projects with population data)",
    "",
    "Average population per project:",
    "  Total",
    "  Black",
    "  White",
    "",
    "Prevalence:",
    "  % projects with any Black residents",
    "  % projects with any White residents",
    "  % projects that are racially mixed",
    "  % projects that are 100% Black",
    "  % projects that are 100% White",
    "",
    "Among projects with Black residents:",
    "  Mean Black share",
    "  Median Black share"
  ),
  Value = c(
    format(racial_comp$n_projects, big.mark = ","),
    "",
    "",
    format(round(racial_comp$mean_total_pop, 0), big.mark = ","),
    format(round(racial_comp$mean_black_pop, 0), big.mark = ","),
    format(round(racial_comp$mean_white_pop, 0), big.mark = ","),
    "",
    "",
    sprintf("%.1f%%", racial_comp$pct_with_black),
    sprintf("%.1f%%", racial_comp$pct_with_white),
    sprintf("%.1f%%", racial_comp$pct_mixed),
    sprintf("%.1f%%", racial_comp$pct_all_black),
    sprintf("%.1f%%", racial_comp$pct_all_white),
    "",
    "",
    sprintf("%.1f%%", racial_comp$mean_black_share_if_present),
    sprintf("%.1f%%", racial_comp$median_black_share_if_present)
  )
)

print(racial_comp_table)
write_csv(racial_comp_table, here(output_dir, "ph_sample_racial_composition.csv"))

# ============================================================================
# TABLE 3: Racial Composition Over Time
# ============================================================================

cat("\n\nCreating Table 3: Racial Composition by Decade...\n")

by_decade <- ph_data %>%
  filter(!is.na(proj_total_population_estimate)) %>%
  mutate(
    decade = floor(year_completed / 10) * 10,
    has_black = proj_black_population_estimate > 0,
    black_share_if_present = if_else(has_black,
                                      proj_black_population_estimate / proj_total_population_estimate,
                                      NA_real_)
  ) %>%
  group_by(decade) %>%
  summarise(
    n_projects = n(),
    mean_units = mean(total_public_housing_units, na.rm = TRUE),
    pct_with_black = mean(has_black, na.rm = TRUE) * 100,
    mean_black_share_conditional = mean(black_share_if_present, na.rm = TRUE) * 100,
    median_black_share_conditional = median(black_share_if_present, na.rm = TRUE) * 100
  ) %>%
  mutate(
    decade_label = paste0(decade, "s"),
    across(where(is.numeric) & !matches("decade"), ~round(., 1))
  )

print(by_decade)
write_csv(by_decade, here(output_dir, "ph_sample_racial_composition_by_decade.csv"))

# ============================================================================
# TABLE 4: Project Size Distribution
# ============================================================================

cat("\n\nCreating Table 4: Project Size Distribution...\n")

size_categories <- ph_data %>%
  mutate(
    size_category = case_when(
      total_public_housing_units < 200 ~ "50-200 units",
      total_public_housing_units < 500 ~ "200-500 units",
      total_public_housing_units < 1000 ~ "500-1000 units",
      TRUE ~ "1000+ units"
    ),
    size_category = factor(size_category,
                           levels = c("50-200 units", "200-500 units",
                                      "500-1000 units", "1000+ units"))
  ) %>%
  group_by(size_category) %>%
  summarise(
    n_projects = n(),
    pct_of_sample = n() / nrow(ph_data) * 100,
    total_units = sum(total_public_housing_units, na.rm = TRUE),
    mean_units = mean(total_public_housing_units, na.rm = TRUE),
    median_units = median(total_public_housing_units, na.rm = TRUE)
  )

print(size_categories)
write_csv(size_categories, here(output_dir, "ph_sample_size_distribution.csv"))

# ============================================================================
# TABLE 5: Top Cities
# ============================================================================

cat("\n\nCreating Table 5: Coverage by City...\n")

top_cities <- ph_data %>%
  group_by(locality) %>%
  summarise(
    n_projects = n(),
    total_units = sum(total_public_housing_units, na.rm = TRUE),
    mean_units = mean(total_public_housing_units, na.rm = TRUE)
  ) %>%
  arrange(desc(n_projects)) %>%
  head(15) %>%
  mutate(
    pct_of_sample = n_projects / nrow(ph_data) * 100
  )

print(top_cities)
write_csv(top_cities, here(output_dir, "ph_sample_top_cities.csv"))

# ============================================================================
# FIGURE 1: Racial Composition Evolution
# ============================================================================

cat("\n\nCreating Figure 1: Racial composition over time...\n")

racial_comp_time <- ph_data %>%
  filter(!is.na(proj_total_population_estimate)) %>%
  mutate(
    has_black = proj_black_population_estimate > 0,
    black_share = proj_black_population_estimate / proj_total_population_estimate,
    white_share = proj_white_population_estimate / proj_total_population_estimate
  ) %>%
  group_by(year_completed) %>%
  summarise(
    n = n(),
    pct_with_black = mean(has_black) * 100,
    mean_black_share_all = mean(black_share, na.rm = TRUE) * 100,
    mean_black_share_conditional = mean(black_share[has_black], na.rm = TRUE) * 100
  ) %>%
  filter(n >= 3)  # Only years with at least 3 projects

p1 <- ggplot(racial_comp_time) +
  geom_line(aes(x = year_completed, y = pct_with_black),
            color = "steelblue", size = 1.2) +
  geom_point(aes(x = year_completed, y = pct_with_black, size = n),
             color = "steelblue", alpha = 0.6) +
  scale_size_continuous(range = c(2, 10), name = "# Projects") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  labs(
    title = "Prevalence of Black Residents in Public Housing Projects",
    subtitle = "By year of project completion",
    x = "Year Completed",
    y = "% of Projects with Any Black Residents"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(here(figures_dir, "ph_racial_composition_over_time.pdf"), p1,
       width = 10, height = 7)

# ============================================================================
# FIGURE 2: Black Share Distribution (for projects with Black residents)
# ============================================================================

cat("Creating Figure 2: Black share distribution...\n")

black_share_data <- ph_data %>%
  filter(!is.na(proj_total_population_estimate),
         proj_black_population_estimate > 0) %>%
  mutate(black_share = proj_black_population_estimate / proj_total_population_estimate)

p2 <- ggplot(black_share_data, aes(x = black_share)) +
  geom_histogram(bins = 30, fill = "navy", alpha = 0.7, color = "white") +
  geom_vline(xintercept = median(black_share_data$black_share),
             linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = median(black_share_data$black_share) + 0.05,
           y = Inf, vjust = 1.5,
           label = sprintf("Median = %.0f%%", median(black_share_data$black_share) * 100),
           color = "red", fontface = "bold") +
  scale_x_continuous(labels = percent, breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Distribution of Black Share in Public Housing Projects",
    subtitle = "Among projects with any Black residents (n = {nrow(black_share_data)})" %>%
               str_glue(),
    x = "Black Share of Project Population",
    y = "Number of Projects"
  ) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

ggsave(here(figures_dir, "ph_black_share_distribution.pdf"), p2,
       width = 10, height = 7)

# ============================================================================
# FIGURE 3: Project Size Distribution
# ============================================================================

cat("Creating Figure 3: Project size distribution...\n")

p3 <- ggplot(ph_data, aes(x = total_public_housing_units)) +
  geom_histogram(bins = 50, fill = "darkgreen", alpha = 0.7, color = "white") +
  geom_vline(xintercept = median(ph_data$total_public_housing_units, na.rm = TRUE),
             linetype = "dashed", color = "red", size = 1) +
  annotate("text",
           x = median(ph_data$total_public_housing_units, na.rm = TRUE) + 100,
           y = Inf, vjust = 1.5,
           label = sprintf("Median = %d units",
                           median(ph_data$total_public_housing_units, na.rm = TRUE)),
           color = "red", fontface = "bold") +
  scale_x_continuous(labels = comma, limits = c(0, 2500)) +
  labs(
    title = "Distribution of Public Housing Project Sizes",
    subtitle = sprintf("Mean = %d units | Median = %d units",
                       round(mean(ph_data$total_public_housing_units, na.rm = TRUE)),
                       median(ph_data$total_public_housing_units, na.rm = TRUE)),
    x = "Number of Housing Units",
    y = "Number of Projects"
  ) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

ggsave(here(figures_dir, "ph_size_distribution.pdf"), p3,
       width = 10, height = 7)

# ============================================================================
# FIGURE 4: Timeline of Project Completions
# ============================================================================

cat("Creating Figure 4: Timeline of completions...\n")

completion_timeline <- ph_data %>%
  group_by(year_completed) %>%
  summarise(
    n_projects = n(),
    total_units = sum(total_public_housing_units, na.rm = TRUE)
  )

p4 <- ggplot(completion_timeline, aes(x = year_completed)) +
  geom_col(aes(y = n_projects), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = total_units / 100), color = "red", size = 1.2) +
  scale_y_continuous(
    name = "Number of Projects",
    sec.axis = sec_axis(~. * 100, name = "Total Units Completed",
                        labels = comma)
  ) +
  labs(
    title = "Timeline of Public Housing Construction",
    subtitle = "Blue bars = number of projects | Red line = total units",
    x = "Year of Completion"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y.left = element_text(color = "steelblue"),
    axis.title.y.right = element_text(color = "red")
  )

ggsave(here(figures_dir, "ph_completion_timeline.pdf"), p4,
       width = 12, height = 7)

# ============================================================================
# Summary Output
# ============================================================================

cat("\n\n=== SUMMARY ===\n")
cat("Tables saved to:", output_dir, "\n")
cat("  - ph_sample_overall_characteristics.csv\n")
cat("  - ph_sample_racial_composition.csv\n")
cat("  - ph_sample_racial_composition_by_decade.csv\n")
cat("  - ph_sample_size_distribution.csv\n")
cat("  - ph_sample_top_cities.csv\n\n")

cat("Figures saved to:", figures_dir, "\n")
cat("  - ph_racial_composition_over_time.pdf\n")
cat("  - ph_black_share_distribution.pdf\n")
cat("  - ph_size_distribution.pdf\n")
cat("  - ph_completion_timeline.pdf\n\n")

cat("=== COMPLETE ===\n")
