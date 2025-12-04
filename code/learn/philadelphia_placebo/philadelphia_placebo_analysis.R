####
# Philadelphia Placebo Site Selection Analysis
# Uses proposed-but-not-built locations for counterfactual matched DiD
####

library(tidyverse)
library(here)
library(sf)
library(modelsummary)
library(tinytable)
library(tableone)

rm(list = ls())

# Parameters -----
data_type <- "combined"
merged_data_dir <- here("data", "derived", "merged", data_type)
results_dir <- here("output", "regression_results", "philadelphia_placebo", data_type)


treated_tracts_panel_filepath <-
  here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")

# Create output directories
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)


# load matching function
source(here("code", "helpers", "matching_functions.R"))

# load table utilities
source(here("code", "helpers", "table_utilities.R"))

# Step 1: Load proposed locations shapefile -----
cat("Loading proposed Philadelphia public housing locations...\n")

proposed_locations <- st_read(here("georeferencing", "philadelphia", "proposed_locations_bauman.shp"))

cat("Proposed locations shapefile structure:\n")
print(str(proposed_locations))
cat("\nColumn names:\n")
print(names(proposed_locations))
cat("\nFirst few rows:\n")
print(head(proposed_locations))
cat("\nCRS information:\n")
print(st_crs(proposed_locations))

# Step 2: Load census tract data -----
cat("\nLoading census tract sample...\n")

# Use non-balanced sample for descriptive analysis (Steps 2-6.5)
# This ensures all proposed locations map to tracts
census_tract_sample_descriptive <- st_read(here(merged_data_dir, "census_tract_sample_with_treatment_status_all.gpkg"))

treated_tracts_panel_raw <- st_read(treated_tracts_panel_filepath)

# Filter to Philadelphia only
philly_tracts <- census_tract_sample_descriptive %>%
  filter(city == "Philadelphia")

cat("Philadelphia tracts found:", nrow(philly_tracts %>% distinct(GISJOIN_1950)), "unique tracts\n")

# Check CRS compatibility
cat("\nCRS comparison:\n")
cat("Proposed locations CRS:", st_crs(proposed_locations)$input, "\n")
cat("Census tracts CRS:", st_crs(philly_tracts)$input, "\n")

# Transform to common CRS if needed
if(st_crs(proposed_locations) != st_crs(philly_tracts)) {
  cat("Transforming proposed locations to match census tracts CRS...\n")
  proposed_locations <- st_transform(proposed_locations, st_crs(philly_tracts))
}

# Step 3: Spatial join to identify tracts containing proposed locations -----
cat("\nPerforming spatial join to identify census tracts with proposed locations...\n")

# Find which tracts contain proposed locations
proposed_tract_join <- st_join(proposed_locations, philly_tracts, join = st_within)

cat("Spatial join results:\n")
cat("Total proposed locations:", nrow(proposed_locations), "\n")
cat("Proposed locations matched to tracts:", sum(!is.na(proposed_tract_join$GISJOIN_1950)), "\n")
cat("Unmatched proposed locations:", sum(is.na(proposed_tract_join$GISJOIN_1950)), "\n")

# Get unique tracts with proposed locations
proposed_tracts <- proposed_tract_join %>%
  filter(!is.na(GISJOIN_1950)) %>%
  st_drop_geometry() %>%
  distinct(GISJOIN_1950) %>%
  pull(GISJOIN_1950)

cat("Unique census tracts with proposed locations:", length(proposed_tracts), "\n")

# Step 4: Check overlap with actual public housing tracts -----
actual_philly_treated <- philly_tracts %>%
  st_drop_geometry() %>%
  filter(treated == 1) %>%
  distinct(GISJOIN_1950) %>%
  pull(GISJOIN_1950)

cat("Actual Philadelphia public housing tracts:", length(actual_philly_treated), "\n")

overlap_tracts <- intersect(proposed_tracts, actual_philly_treated)
cat("Tracts with both proposed and actual public housing:", length(overlap_tracts), "\n")

proposed_only <- setdiff(proposed_tracts, actual_philly_treated)
cat("Tracts with only proposed (not actual) public housing:", length(proposed_only), "\n")

# Step 5: Create summary table of proposed vs actual locations -----
location_summary <- tibble(
  location_type = c("Proposed locations (total)", 
                    "Proposed locations matched to tracts",
                    "Unique tracts with proposed locations",
                    "Actual Philadelphia public housing tracts",
                    "Tracts with both proposed and actual",
                    "Tracts with only proposed locations"),
  count = c(nrow(proposed_locations),
            sum(!is.na(proposed_tract_join$GISJOIN_1950)),
            length(proposed_tracts),
            length(actual_philly_treated),
            length(overlap_tracts),
            length(proposed_only))
)

print(location_summary)

# Step 6: Descriptive comparison - Proposed vs Actual Philadelphia locations -----
cat("\n=== DESCRIPTIVE COMPARISON: PROPOSED VS ACTUAL LOCATIONS ===\n")

# Get 1940 characteristics for comparison
philly_1940_data <- philly_tracts %>%
  filter(YEAR == 1940) %>%
  st_drop_geometry() %>%
  mutate(
    location_type = case_when(
      GISJOIN_1950 %in% proposed_only ~ "Proposed Only",
      GISJOIN_1950 %in% actual_philly_treated ~ "Actual Public Housing",
      TRUE ~ "Neither"
    ),
    asinh_pop_total = asinh(total_pop),
    asinh_median_income = asinh(median_income),
    asinh_median_rent_calculated = asinh(median_rent_calculated),
    asinh_distance_from_cbd = asinh(distance_from_cbd)
  ) %>%
  filter(location_type != "Neither")

cat("Sample sizes for comparison:\n")
table(philly_1940_data$location_type)

# Variables for comparison (key placebo test variables only)
comparison_vars <- c(
  "black_share", "asinh_pop_total", "asinh_median_income",
  "unemp_rate", "asinh_median_rent_calculated", 
  "asinh_distance_from_cbd", "redlined_binary_80pp", "ur_binary_5pp"
)

# Calculate descriptive statistics by location type
descriptive_comparison <- philly_1940_data %>%
  select(location_type, all_of(comparison_vars)) %>%
  pivot_longer(cols = -location_type, names_to = "variable", values_to = "value") %>%
  group_by(location_type, variable) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    sd_val = sd(value, na.rm = TRUE),
    n_obs = sum(!is.na(value)),
    .groups = 'drop'
  ) %>%
  mutate(
    mean_sd = ifelse(variable %in% c("redlined_binary_80pp", "ur_binary_5pp"),
                     sprintf("%.3f", mean_val),
                     sprintf("%.3f (%.3f)", mean_val, sd_val))
  ) %>%
  select(location_type, variable, mean_sd) %>%
  pivot_wider(names_from = location_type, values_from = mean_sd)

# Use CreateTableOne for consistency with balance tables
philadelphia_tableone <- CreateTableOne(
  vars = comparison_vars,
  strata = "location_type",
  data = philly_1940_data,
  test = TRUE
)

cat("Philadelphia comparison using tableone:\n")
print(philadelphia_tableone, smd = TRUE, test = TRUE)

# Extract SMD and p-values from tableone object (following balance table approach)
table_matrix <- print(philadelphia_tableone, smd = TRUE, test = TRUE, printToggle = FALSE)

# Convert to data frame and format
philadelphia_comparison_df <- as.data.frame(table_matrix) %>%
  rownames_to_column("variable") %>%
  filter(variable != "n") %>%  # Remove sample size row
  mutate(
    # Clean variable names
    variable = str_replace_all(variable, "\\s*\\(mean \\(SD\\)\\)", "")
  ) %>%
  select(variable, `Actual Public Housing`, `Proposed Only`, SMD, p) %>%
  rename(
    "Std. Diff." = SMD,
    "p_value" = p
  ) %>%
  # Add significance stars to Std. Diff. based on p-values
  mutate(
    p_numeric = suppressWarnings(as.numeric(p_value)),
    `Std. Diff.` = case_when(
      is.na(p_numeric) ~ `Std. Diff.`,
      p_numeric < 0.01 ~ paste0(`Std. Diff.`, "***"),
      p_numeric < 0.05 ~ paste0(`Std. Diff.`, "**"),
      p_numeric < 0.10 ~ paste0(`Std. Diff.`, "*"),
      TRUE ~ `Std. Diff.`
    )
  ) %>%
  select(-p_value, -p_numeric) %>%
  # Map variable names to clean labels
  mutate(
    Variable = case_when(
      variable == "black_share" ~ "Black Share",
      variable == "asinh_pop_total" ~ "Log Total Population",
      variable == "asinh_median_income" ~ "Log Median Income",
      variable == "unemp_rate" ~ "Unemployment Rate",
      variable == "asinh_median_rent_calculated" ~ "Log Median Rent",
      variable == "asinh_distance_from_cbd" ~ "Log Distance from CBD",
      variable == "redlined_binary_80pp" ~ "Redlined (HOLC)",
      variable == "ur_binary_5pp" ~ "Urban Renewal Area",
      TRUE ~ variable
    )
  ) %>%
  select(Variable, `Actual Public Housing`, `Proposed Only`, `Std. Diff.`)

# Final table for output
philadelphia_comparison_table <- philadelphia_comparison_df

cat("=== PHILADELPHIA: PROPOSED VS ACTUAL LOCATIONS (1940) ===\n")
print(philadelphia_comparison_table)

# Add sample size note
n_proposed <- sum(philly_1940_data$location_type == "Proposed Only")
n_actual <- sum(philly_1940_data$location_type == "Actual Public Housing")

# Create publication-ready table
# Note: Don't include caption/notes in tt() call - these will be added manually in LaTeX
# with threeparttable wrapper
philly_comparison_table_formatted <- philadelphia_comparison_table |>
  tt() |>
  format_tt(escape = FALSE) |>
  theme_tt(theme = "tabular")

# Print formatted table
print(philly_comparison_table_formatted)

# Save to LaTeX file and strip table wrappers for threeparttable compatibility
philly_table_path <- file.path(results_dir, "philadelphia_proposed_vs_actual_characteristics.tex")
save_tt(philly_comparison_table_formatted, philly_table_path, overwrite = TRUE)
remove_table_wrappers(philly_table_path)

# Create slides version (simpler version for presentations)
slides_dir <- file.path(results_dir, "slides")
dir.create(slides_dir, recursive = TRUE, showWarnings = FALSE)

philly_comparison_table_slides <- philadelphia_comparison_table |>
  tt(caption = "1940 Characteristics: Proposed vs Actual Locations") |>
  style_tt(font_size = 1.0) |>
  format_tt(escape = FALSE) |>
  theme_tt(theme = "tabular")

save_tt(philly_comparison_table_slides,
        file.path(slides_dir, "philadelphia_proposed_vs_actual_characteristics.tex"),
        overwrite = TRUE)



# Step 6.5 Map of Philadelphia -----
cat("\n=== CREATING PHILADELPHIA MAP ===\n")

# Get Philadelphia tracts with 1940 data (already using non-balanced sample from Step 2)
philly_tracts_1940_all <- philly_tracts %>%
  filter(YEAR == 1940)

cat("Philadelphia tracts (1940):", nrow(philly_tracts_1940_all), "\n")

# Create map
philly_map <- ggplot() +
  # Base layer: Philadelphia census tracts with 1940 Black share
  geom_sf(data = philly_tracts_1940_all,
          aes(fill = black_share),
          color = "gray80", linewidth = 0.2) +
  scale_fill_viridis_c(option = "plasma",
                       name = "Black Share\n(1940)",
                       labels = scales::percent_format()) +
  # All proposed locations
  geom_sf(data = proposed_locations,
          color = "#d62728", size = 2.5, shape = 16) +
  # Styling
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "Philadelphia: Proposed Public Housing Locations (1956)",
    caption = "Red dots = Proposed locations"
  )

philly_map
# Save map
ggsave(file.path(results_dir, "philadelphia_proposed_locations_map.pdf"),
       philly_map, width = 10, height = 8)

ggsave(file.path(slides_dir, "philadelphia_proposed_locations_map.pdf"),
       philly_map, width = 10, height = 8)

cat("Philadelphia map saved\n")


# Step 7: Set up placebo matching following existing methodology -----
cat("\n=== PHILADELPHIA PLACEBO MATCHING SETUP ===\n")

# Load balanced sample for placebo analysis (needs balanced panel for event study)
census_tract_sample <- st_read(here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg"))

# First, get the actual Philadelphia treatment years to inform placebo timing
actual_philly_treatment_timing <- census_tract_sample %>%
  st_drop_geometry() %>%
  filter(city == "Philadelphia", treated == 1) %>%
  # You'll need to get treatment year from treated_tracts_panel
  left_join(treated_tracts_panel_raw %>% st_drop_geometry(), by = "GISJOIN_1950") %>%
  filter(!is.na(treatment_year)) %>%
  distinct(GISJOIN_1950, treatment_year)

cat("Philadelphia actual treatment years:\n")
print(table(actual_philly_treatment_timing$treatment_year))

# Use median year for placebo treatment
placebo_treatment_year <- 1960

# Create placebo dataset following your structure
cat("\n--- CREATING PLACEBO DATASET ---\n")

# Create placebo version of tracts_and_rings
placebo_tracts_and_rings <- tibble(
  GISJOIN_1950 = proposed_only,
  location_type = "treated"  # Treat proposed locations as "treated" 
)

# Create placebo version of treated_tracts_panel  
placebo_treated_tracts_panel <- tibble(
  GISJOIN_1950 = proposed_only,
  total_public_housing_units = 100,  # Assign placeholder units
  treatment_year = placebo_treatment_year
)

# Create placebo census tract data following your exact structure
placebo_census_tract_data <- census_tract_sample %>%
  # Same geographic processing as your original
  mutate(centroid = st_centroid(geom)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  select(-centroid) %>%
  # Merge placebo treatment info instead of actual
  left_join(placebo_treated_tracts_panel, by = "GISJOIN_1950") %>%
  # Merge placebo rings
  left_join(placebo_tracts_and_rings, by = "GISJOIN_1950") %>%
  # Same location_type logic as original
  mutate(location_type = if_else(is.na(location_type), "donor_pool", location_type)) %>%
  # Same variable transformations as your original
  mutate(asinh_pop_total = asinh(total_pop),
         asinh_pop_white = asinh(white_pop),
         asinh_pop_black = asinh(black_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent_calculated = asinh(median_rent_calculated),
         asinh_median_home_value_calculated = asinh(median_home_value_calculated),
         asinh_distance_from_cbd = asinh(distance_from_cbd),
         ln_population_density = log(population_density),
         asinh_private_population_estimate = asinh(private_population_estimate),
         asinh_private_black_population_estimate = asinh(private_black_population_estimate),
         asinh_private_white_population_estimate = asinh(private_white_population_estimate),
         log_private_units_estimate = asinh(total_private_units_estimate)) %>%
  # Same county ID and other processing
  mutate(county_id = paste0(STATEA, COUNTYA)) %>%
  dplyr::rename(year = YEAR) %>%
  st_drop_geometry() %>%
  arrange(GISJOIN_1950, year) %>%
  mutate(category_most_overlap = ifelse(is.na(category_most_overlap), "missing", category_most_overlap),
         grade_most_overlap = ifelse(is.na(grade_most_overlap), "missing", grade_most_overlap))

cat("Placebo dataset created:\n")
cat("- Placebo treated tracts:", sum(placebo_census_tract_data$location_type == "treated", na.rm = TRUE), "\n")
cat("- Donor pool tracts:", sum(placebo_census_tract_data$location_type == "donor_pool", na.rm = TRUE), "\n")

# Now you can run your existing perform_matching_with_replacement function on this placebo dataset!
cat("\n=== READY FOR PLACEBO MATCHING ===\n")
cat("Use placebo_census_tract_data with your existing perform_matching_with_replacement() function\n")
cat("Treatment year:", placebo_treatment_year, "\n")
cat("Group types: 'treated' (placebo locations)\n")


# Step 8: Run placebo matching using existing function -----
cat("\n=== RUNNING PLACEBO MATCHING ===\n")

# Use your existing matching parameters
matching_vars <- c(
  "total_pop",
  "black_share",
  "unemp_rate",
  "median_income",
  "median_rent_calculated"
)

exact_matching_vars <- c(
  "county_id",
  "redlined_binary_80pp",
  "ur_binary_5pp"
)

knn_neighbors <- 1

# Run placebo matching (only "treated" group since no inner ring for placebo)
placebo_matched_result <- perform_matching_with_replacement(
  data = placebo_census_tract_data,
  treatment_year = placebo_treatment_year,  # 1960
  match_vars = matching_vars,
  nearest_neighbors = knn_neighbors,
  group_type = "treated",  # Placebo treated locations
  match_type = "nearest",
  distance_option = "glm",
  match_link = "logit",
  pre_treatment_decades = 1,
  exact_match_vars = exact_matching_vars,
  caliper = NULL
)

cat("Placebo matching complete!\n")
cat("Matched placebo treated units:", sum(placebo_matched_result$matched_data$treatment_group == 1), "\n")
cat("Matched control units:", sum(placebo_matched_result$matched_data$treatment_group == 0), "\n")

# Create placebo matched panel dataset (following your structure)
placebo_matched_tract_ids <- placebo_matched_result$matched_data %>%
  select(GISJOIN_1950, subclass) %>%
  mutate(
    matched_treatment_year = placebo_treatment_year,
    group_type = "treated",
    match_type = "nearest"
  ) %>%
  distinct()

# Create full placebo matched panel
placebo_tract_data_matched <- placebo_census_tract_data %>%
  inner_join(placebo_matched_tract_ids, by = "GISJOIN_1950") %>%
  mutate(
    match_group = paste(county_id, matched_treatment_year, subclass, sep = "_"),
    weights = 1  # Equal weights for now
  )

cat("Placebo matched panel created:\n")
cat("- Total observations:", nrow(placebo_tract_data_matched), "\n")
cat("- Unique tracts:", length(unique(placebo_tract_data_matched$GISJOIN_1950)), "\n")
cat("- Treatment observations:", sum(placebo_tract_data_matched$location_type == "treated"), "\n")
cat("- Control observations:", sum(placebo_tract_data_matched$location_type == "donor_pool"), "\n")

# Step 9: Run placebo event study analysis -----
cat("\n=== RUNNING PLACEBO EVENT STUDY ===\n")

vars_to_compare <- c("black_share", "asinh_pop_total", "asinh_median_income", "asinh_median_rent_calculated")

# Load helper functions
source(here("code", "helpers", "matched_did_event_study.R"))

# Function to run event study and extract results
run_event_study_comparison <- function(data, vars_list, analysis_name) {
  results_list <- list()
  
  for (var in vars_list) {
    cat("Running event study for", var, "...\n")
    
    event_study_result <- did_event_study(
      input_data = data,
      outcome_var = var,
      treatment_group = "treated"
    )
    
    # Extract coefficients
    results_df <- broom::tidy(event_study_result$twfe_conley) %>%
      filter(str_detect(term, "event_time::-?\\d+:treated")) %>%
      mutate(
        event_time = as.numeric(str_extract(term, "(?<=event_time::)-?\\d+(?=:treated)")),
        variable = var,
        analysis = analysis_name,
        significant = p.value < 0.05
      )
    
    results_list[[var]] <- results_df
  }
  
  return(bind_rows(results_list))
}

# Run placebo analysis for all variables
cat("\n--- Running placebo analysis for all variables ---\n")
placebo_results_all <- run_event_study_comparison(
  data = placebo_tract_data_matched,
  vars_list = vars_to_compare,
  analysis_name = "Philadelphia Placebo"
)

cat("Placebo analysis completed for all variables\n")


# Step 10: Philadelphia-only treated analysis for comparison -----
cat("\n=== PHILADELPHIA TREATED ANALYSIS (FOR COMPARISON) ===\n")


# Read in your existing matched dataset and filter to Philadelphia
philly_tract_data_matched <- read_csv(here("data", "derived", "merged", "combined", "matched_dataset",
                                           "tract_data_matched_1_year_replacement.csv")) %>%
  filter(city == "Philadelphia", group_type == "treated")

cat("Philadelphia treated matched data loaded:\n")
cat("- Total observations:", nrow(philly_tract_data_matched), "\n")
cat("- Unique tracts:", length(unique(philly_tract_data_matched$GISJOIN_1950)), "\n")
cat("- Treatment observations:", sum(philly_tract_data_matched$location_type == "treated"), "\n")
cat("- Control observations:", sum(philly_tract_data_matched$location_type == "donor_pool"), "\n")

# Run Philadelphia treated analysis for all variables  
cat("\n--- Running treated analysis for all variables ---\n")
treated_results_all <- run_event_study_comparison(
  data = philly_tract_data_matched,
  vars_list = vars_to_compare,
  analysis_name = "Philadelphia Treated"
)

cat("Philadelphia treated analysis completed for all variables\n")

# Step 11: Produce output -----
# Combine results for comparison
combined_results <- bind_rows(placebo_results_all, treated_results_all) %>%
  mutate(
    variable_label = case_when(
      variable == "black_share" ~ "Black Share",
      variable == "white_share" ~ "White Share", 
      variable == "total_pop" ~ "Total Population",
      variable == "asinh_pop_total" ~ "Log Total Population",
      variable == "median_income" ~ "Median Income",
      variable == "asinh_median_income" ~ "Log Median Income",
      variable == "median_rent_calculated" ~ "Median Rent",
      variable == "asinh_median_rent_calculated" ~ "Log Median Rent",
      TRUE ~ variable
    ),
    analysis_short = case_when(
      analysis == "Philadelphia Placebo" ~ "Placebo",
      analysis == "Philadelphia Treated" ~ "Treated",
      TRUE ~ analysis
    )
  )

# Create comparison plots
cat("\n--- Creating comparison plots ---\n")

# Add reference point at event_time = -10 with estimate = 0
ref_point <- combined_results %>%
  distinct(variable, analysis, variable_label, analysis_short) %>%
  mutate(
    term = "ref_point",
    estimate = 0,
    std.error = 0,
    event_time = -10,
    significant = FALSE
  )

# Combine with actual results
plot_data <- bind_rows(combined_results, ref_point) %>%
  filter(variable %in% c("black_share", "asinh_median_income", "asinh_pop_total", "asinh_median_rent_calculated")) %>%
  arrange(variable_label, analysis_short, event_time)

# Publication theme function (matching your existing style)
pub_theme <- function(base_size = 14){
  theme_classic(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 11),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Create paper-ready comparison plot
comparison_plot <- plot_data %>%
  ggplot(aes(x = event_time, y = estimate, color = analysis_short, group = analysis_short)) +
  # Error bars
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 1, linewidth = 0.8, alpha = 0.5,
                position = position_dodge(width = 1.5)) +
  # Lines and points
  geom_line(linewidth = 0.9, position = position_dodge(width = 1.5)) +
  geom_point(size = 2.2, position = position_dodge(width = 1.5)) +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.4) +
  # Styling
  scale_color_manual(values = c("Placebo" = "gray50", "Treated" = "#d62728"), 
                     name = "Analysis") +
  scale_x_continuous(breaks = seq(-20, 30, 10), labels = seq(-2, 3, 1)) +
  facet_wrap(~ variable_label, scales = "free_y", ncol = 2) +
  labs(
    title = "Philadelphia Placebo Test: Proposed vs Actual Public Housing Locations",
    x = "Decades Relative to Treatment",
    y = "Difference-in-Differences Estimate"
  ) +
  pub_theme(14)
comparison_plot

ggsave(file.path(results_dir, "philadelphia_placebo_vs_treated_comparison.pdf"),
       comparison_plot, width = 12, height = 8)

# Summary table
summary_table <- combined_results %>%
  group_by(analysis, variable_label) %>%
  summarise(
    n_coefs = n(),
    n_significant = sum(significant),
    pre_significant = sum(significant & event_time < 0),
    post_significant = sum(significant & event_time >= 0),
    .groups = 'drop'
  ) %>%
  mutate(
    sig_summary = paste0(n_significant, "/", n_coefs, " (", 
                        pre_significant, " pre, ", post_significant, " post)")
  )

cat("\n=== PHILADELPHIA COMPARISON SUMMARY ===\n")
print(summary_table)

