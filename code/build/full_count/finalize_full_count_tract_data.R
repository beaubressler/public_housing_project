####
# Finalize full count tract data for 1930 and 1940
#
# Uses:
#   - 1930: Pure geocoded (ED-based income has r=0.056 correlation - broken)
#   - 1940: Hybrid with threshold 1.5 (7.9% geocoded, 92.1% ED-based)
#
# Output: tract_data_final_1930_1940.gpkg (replaces tract_data_concorded_from_ed_1930_1940.gpkg)
####

library(tidyverse)
library(sf)
library(here)

cat("\n===========================================================\n")
cat("Finalizing Full Count Tract Data (1930 & 1940)\n")
cat("===========================================================\n\n")

# Directories
output_dir <- here("data", "derived", "census", "full_count", "tract")
shapefile_dir <- here("data", "raw", "nhgis", "gis", "nhgis0027_shapefile_tl2000_us_tract_1950")

cat("Step 1: Loading 1930 geocoded data...\n")
data_1930 <- read_csv(
  here(output_dir, "tract_data_1930_geocoded.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    # Rename employment variables to match expected format
    employed_pop = employed,
    unemployed_pop = unemployed,
    not_in_lf_pop = not_in_lf
  ) %>%
  select(-employed, -unemployed, -not_in_lf)

cat("  Loaded", nrow(data_1930), "tracts for 1930\n")
cat("  Variables:", paste(names(data_1930)[1:10], collapse=", "), "...\n\n")

cat("Step 2: Loading 1940 hybrid data...\n")
data_1940 <- read_csv(
  here(output_dir, "tract_data_1940_hybrid_geocoded_ed.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    # Rename employment variables to match expected format
    employed_pop = employed,
    unemployed_pop = unemployed,
    not_in_lf_pop = not_in_lf
  ) %>%
  select(-employed, -unemployed, -not_in_lf)

cat("  Loaded", nrow(data_1940), "tracts for 1940\n")
cat("  Data source breakdown:\n")
cat("    Geocoded:", sum(data_1940$data_source == "geocoded", na.rm=TRUE), "tracts\n")
cat("    ED-based:", sum(data_1940$data_source == "ed_based", na.rm=TRUE), "tracts\n\n")

cat("Step 3: Identifying common variables...\n")

# Get common variables between the two years
common_vars <- intersect(names(data_1930), names(data_1940))

# Remove diagnostic variables from 1940 that aren't in 1930
vars_to_keep <- setdiff(common_vars, c("source"))

# Add back variables that should be in both but might be missing
# Year-specific variables that should be kept
vars_1930_specific <- c("literate_pop", "total_literacy_pop", "literacy_rate")
vars_1940_specific <- c("hs_grad_pop", "some_college_pop", "total_educ_pop",
                        "pct_hs_grad", "pct_some_college",
                        "data_source", "reason", "pop_ratio")

cat("  Common variables:", length(vars_to_keep), "\n")
cat("  1930-specific variables:", length(vars_1930_specific), "\n")
cat("  1940-specific variables:", length(vars_1940_specific), "\n\n")

cat("Step 4: Loading 1950 tract shapefile for geometry...\n")
tract_shapes <- st_read(
  here(shapefile_dir, "US_tract_1950.shp"),
  quiet = TRUE
) %>%
  select(GISJOIN, geometry) %>%
  mutate(GISJOIN_1950 = GISJOIN)

cat("  Loaded geometry for", nrow(tract_shapes), "tracts\n\n")

cat("Step 5: Combining datasets...\n")

# Select and prepare 1930 data
data_1930_final <- data_1930 %>%
  select(all_of(vars_to_keep), all_of(vars_1930_specific)) %>%
  mutate(YEAR = 1930)

# Select and prepare 1940 data
data_1940_final <- data_1940 %>%
  select(all_of(vars_to_keep), all_of(vars_1940_specific)) %>%
  mutate(YEAR = 1940)

# Combine both years
combined_data <- bind_rows(data_1930_final, data_1940_final)

cat("  Combined dataset:", nrow(combined_data), "tract-year observations\n")
cat("    1930:", sum(combined_data$YEAR == 1930), "tracts\n")
cat("    1940:", sum(combined_data$YEAR == 1940), "tracts\n\n")

cat("Step 6: Adding geometry...\n")

# Join with geometry
final_data <- combined_data %>%
  left_join(tract_shapes, by = "GISJOIN_1950") %>%
  st_as_sf()

cat("  Added geometry to", sum(!is.na(st_dimension(final_data))), "tracts\n\n")

cat("Step 7: Saving final dataset...\n")

output_file <- here(output_dir, "tract_data_concorded_final_1930_1940.gpkg")

st_write(
  final_data,
  output_file,
  delete_dsn = TRUE,
  quiet = TRUE
)

cat("  Saved to:", output_file, "\n\n")

cat("Step 8: Summary statistics...\n\n")

summary_stats <- final_data %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(
    n_tracts = n(),
    mean_total_pop = mean(total_pop, na.rm = TRUE),
    mean_black_share = mean(black_share, na.rm = TRUE),
    mean_median_income = mean(median_income, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)

cat("\n===========================================================\n")
cat("COMPLETE\n")
cat("===========================================================\n\n")

cat("Output file: tract_data_concorded_final_1930_1940.gpkg\n")
cat("  1930: Pure geocoded data (ED-based income broken with r=0.056)\n")
cat("  1940: Hybrid (threshold 1.5) - 7.9% geocoded, 92.1% ED-based\n\n")

cat("This file replaces: tract_data_concorded_from_ed_1930_1940.gpkg\n")
cat("  in combine_neighborhood_data.R (line ~88)\n\n")
