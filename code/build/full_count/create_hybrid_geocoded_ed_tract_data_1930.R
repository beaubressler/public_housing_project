####
# Create hybrid 1930 tract dataset using geocoded data for problematic tracts
# and ED-based data for well-covered tracts
#
# Standalone script - not yet integrated into main workflow
####

library(tidyverse)
library(sf)
library(here)

# Directories
census_data_dir <- here("data", "derived", "census")
output_dir <- here(census_data_dir, "full_count", "tract")
crosswalk_dir <- here("data", "derived", "geographic_crosswalks", "ed_to_tract")

message("=== Creating Hybrid Geocoded/ED Tract Dataset ===\n")

# Threshold for identifying problematic tracts
# Use geocoded when it finds more people than ED-based, despite systematic 30% undercount
# This indicates ED linkage is broken for that tract

message("Step 1: Loading datasets...")

# Load geocoded data
geocoded_data <- read_csv(here(output_dir, "tract_data_1930_geocoded.csv"), show_col_types = FALSE)
message(sprintf("  Loaded geocoded data: %d tracts", nrow(geocoded_data)))

# Load ED-based data
ed_data <- read_sf(here(output_dir, "tract_data_concorded_from_ed_1930_1940.gpkg")) %>%
  filter(YEAR == 1930) %>%
  st_drop_geometry()
message(sprintf("  Loaded ED-based data: %d tracts", nrow(ed_data)))

message("\nStep 2: Comparing population coverage...")

# Compare populations
pop_comparison <- geocoded_data %>%
  select(GISJOIN_1950,
         total_pop_geocoded = total_pop,
         white_pop_geocoded = white_pop,
         black_pop_geocoded = black_pop) %>%
  inner_join(
    ed_data %>% select(GISJOIN_1950,
                       total_pop_ed = total_pop,
                       white_pop_ed = white_pop,
                       black_pop_ed = black_pop),
    by = "GISJOIN_1950"
  ) %>%
  mutate(
    pop_diff = total_pop_geocoded - total_pop_ed,
    pop_ratio = total_pop_geocoded / (total_pop_ed + 1),
    geocoded_has_more = total_pop_geocoded > total_pop_ed
  )

message(sprintf("  Tracts where geocoded > ED: %d (%.1f%%)",
                sum(pop_comparison$geocoded_has_more, na.rm = TRUE),
                100 * mean(pop_comparison$geocoded_has_more, na.rm = TRUE)))
message(sprintf("  Tracts where ED > geocoded: %d (%.1f%%)",
                sum(!pop_comparison$geocoded_has_more, na.rm = TRUE),
                100 * mean(!pop_comparison$geocoded_has_more, na.rm = TRUE)))

message("\nStep 3: Identifying which tracts to use geocoded vs ED...")

# Simple rule: use whichever finds more people
# Since geocoded undercounts by ~30%, when it still exceeds ED, ED linkage is broken
tract_classification <- pop_comparison %>%
  mutate(
    use_geocoded = geocoded_has_more,
    reason = case_when(
      geocoded_has_more ~ "geocoded_has_more_pop",
      TRUE ~ "ed_has_more_pop"
    ),
    data_source = ifelse(use_geocoded, "geocoded", "ed_based")
  )

# Summary
source_summary <- tract_classification %>%
  group_by(data_source, reason) %>%
  summarise(
    n_tracts = n(),
    mean_pop_geocoded = mean(total_pop_geocoded, na.rm = TRUE),
    mean_pop_ed = mean(total_pop_ed, na.rm = TRUE),
    .groups = "drop"
  )

message("\n=== TRACT CLASSIFICATION SUMMARY ===")
print(source_summary, n = 10)

message("\nStep 4: Creating hybrid dataset...")

# Get all variables from both datasets
geocoded_vars <- names(geocoded_data)
ed_vars <- names(ed_data)
common_vars <- intersect(geocoded_vars, ed_vars)

# Create hybrid by selecting appropriate source for each tract
hybrid_data <- tract_classification %>%
  select(GISJOIN_1950, data_source, reason, pop_ratio,
         total_pop_geocoded, total_pop_ed) %>%
  left_join(
    geocoded_data %>%
      select(all_of(common_vars)) %>%
      mutate(source = "geocoded"),
    by = "GISJOIN_1950"
  ) %>%
  left_join(
    ed_data %>%
      select(all_of(common_vars)) %>%
      mutate(source = "ed_based"),
    by = "GISJOIN_1950",
    suffix = c("_geocoded_full", "_ed_full")
  )

# For each tract, select variables from appropriate source
for (var in setdiff(common_vars, "GISJOIN_1950")) {
  geocoded_col <- paste0(var, "_geocoded_full")
  ed_col <- paste0(var, "_ed_full")

  if (geocoded_col %in% names(hybrid_data) & ed_col %in% names(hybrid_data)) {
    hybrid_data[[var]] <- ifelse(
      hybrid_data$data_source == "geocoded",
      hybrid_data[[geocoded_col]],
      hybrid_data[[ed_col]]
    )
  }
}

# Clean up temporary columns
hybrid_data_final <- hybrid_data %>%
  select(GISJOIN_1950, data_source, reason, pop_ratio,
         all_of(setdiff(common_vars, "GISJOIN_1950"))) %>%
  mutate(YEAR = 1930)

message(sprintf("Created hybrid dataset with %d tracts", nrow(hybrid_data_final)))

message("\nStep 5: Comparing hybrid to original methods...")

# Calculate summary stats
comparison_summary <- hybrid_data_final %>%
  summarise(
    n_tracts = n(),
    n_geocoded = sum(data_source == "geocoded"),
    n_ed = sum(data_source == "ed_based"),
    pct_geocoded = 100 * mean(data_source == "geocoded"),
    mean_total_pop = mean(total_pop, na.rm = TRUE),
    mean_black_share = mean(black_share, na.rm = TRUE),
    mean_median_income = mean(median_income, na.rm = TRUE)
  )

message("\n=== HYBRID DATASET SUMMARY ===")
message(sprintf("Total tracts: %d", comparison_summary$n_tracts))
message(sprintf("  Using geocoded: %d (%.1f%%)", comparison_summary$n_geocoded, comparison_summary$pct_geocoded))
message(sprintf("  Using ED-based: %d (%.1f%%)", comparison_summary$n_ed, 100 - comparison_summary$pct_geocoded))
message(sprintf("\nMean population: %.0f people/tract", comparison_summary$mean_total_pop))
message(sprintf("Mean black share: %.3f", comparison_summary$mean_black_share))
message(sprintf("Mean median income: $%.0f", comparison_summary$mean_median_income))

message("\nStep 6: Saving outputs...")

# Save hybrid dataset
write_csv(hybrid_data_final, here(output_dir, "tract_data_1930_hybrid_geocoded_ed.csv"))

# Save classification details
write_csv(tract_classification, here(output_dir, "tract_data_source_classification_1930.csv"))

# Create diagnostic report
diagnostic_file <- here(output_dir, "diagnostics", "hybrid_method_diagnostics_1930.txt")
dir.create(dirname(diagnostic_file), showWarnings = FALSE, recursive = TRUE)

sink(diagnostic_file)
cat("=== HYBRID GEOCODED/ED METHOD DIAGNOSTICS ===\n")
cat("Generated:", as.character(Sys.time()), "\n\n")

cat("SELECTION CRITERIA:\n")
cat("  Use geocoded when it finds more people than ED-based\n")
cat("  Since geocoded systematically undercounts by ~30%, when it still exceeds ED,\n")
cat("  this indicates the ED linkage is broken for that tract\n\n")

cat("CLASSIFICATION SUMMARY:\n")
print(source_summary)

cat("\n\nEXAMPLES OF TRACTS USING GEOCODED DATA (ED linkage broken):\n")
geocoded_examples <- tract_classification %>%
  filter(use_geocoded) %>%
  arrange(desc(pop_ratio)) %>%
  select(GISJOIN_1950, reason, pop_ratio,
         total_pop_ed, total_pop_geocoded, pop_diff) %>%
  head(20)
print(geocoded_examples)

cat("\n\nEXAMPLES OF TRACTS USING ED-BASED DATA (ED linkage good):\n")
ed_examples <- tract_classification %>%
  filter(!use_geocoded) %>%
  arrange(desc(total_pop_ed)) %>%
  select(GISJOIN_1950, reason, pop_ratio,
         total_pop_ed, total_pop_geocoded, pop_diff) %>%
  head(20)
print(ed_examples)

cat("\n\nDISTRIBUTION OF POPULATION RATIOS (geocoded/ED):\n")
print(summary(tract_classification$pop_ratio))

sink()

message(sprintf("\n=== COMPLETE ==="))
message(sprintf("Hybrid dataset saved to: %s",
                here(output_dir, "tract_data_1930_hybrid_geocoded_ed.csv")))
message(sprintf("Classification details: %s",
                here(output_dir, "tract_data_source_classification_1930.csv")))
message(sprintf("Diagnostics report: %s", diagnostic_file))

message("\n=== SUMMARY ===")
message(sprintf("%.1f%% of tracts use geocoded data (problematic ED coverage)",
                comparison_summary$pct_geocoded))
message(sprintf("%.1f%% of tracts use ED-based data (good ED coverage)",
                100 - comparison_summary$pct_geocoded))
