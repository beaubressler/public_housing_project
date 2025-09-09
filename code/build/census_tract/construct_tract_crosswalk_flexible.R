# Flexible Tract Crosswalk Construction
#
# This is an adaptation of the area-reweighting crosswalking Python code provided by Fabian Eckert et al 2020
# Creates area re-weighting crosswalks between 1930-2000 Census tracts and any target census tract year
# 
# Unlike the original script which was hardcoded to crosswalk TO 1950 tracts,
# this version uses the tract configuration system to create crosswalks TO any target year

library(tidyverse)
library(sf)
library(here)

# Source tract configuration
source(here("code", "config", "tract_config.R"))

cat("Creating crosswalks TO", TRACT_YEAR, "census tracts\n")

# Define file paths using configuration
reference_path <- get_tract_reference_path()
reference_fname <- get_tract_reference_filename()
reference_geoid <- "GISJOIN.1"

# Define output path
output_path <- here("data", "derived", "geographic_crosswalks")

# Ensure output directory exists
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# Years to create crosswalks for (excluding the target year)
all_years <- c("1930", "1940", "1960", "1970", "1980", "1990", "2000")
source_years <- all_years[all_years != as.character(TRACT_YEAR)]

cat("Creating crosswalks from years:", paste(source_years, collapse = ", "), "\n")

# Loop through the source years
for (y in source_years) {
  cat("Processing year:", y, "\n")
  
  reporting_path <- here("data", "raw", "nhgis", "gis", paste0("nhgis0027_shapefile_tl2000_us_tract_", y))
  reporting_fname <- paste0("US_tract_", y, ".shp")
  reporting_geoid <- "GISJOIN"
  output_fname <- get_crosswalk_filename(y, TRACT_YEAR)
  
  # Check if source shapefile exists
  source_shapefile <- file.path(reporting_path, reporting_fname)
  if (!file.exists(source_shapefile)) {
    cat("WARNING: Source shapefile not found:", source_shapefile, "\n")
    cat("Skipping year", y, "\n")
    next
  }
  
  # Check if output file already exists
  output_file <- file.path(output_path, output_fname)
  if (file.exists(output_file)) {
    cat("Crosswalk already exists:", output_fname, "\n")
    cat("Skipping year", y, "\n")
    next
  }
  
  # Read in the reporting shapefile
  cat("  Reading", y, "shapefile...\n")
  shp_reporting <- st_read(here(reporting_path, reporting_fname), quiet = TRUE)
  shp_reporting <- shp_reporting %>% 
    mutate(area_base = st_area(.))
  
  # Read in the reference shapefile
  cat("  Reading", TRACT_YEAR, "reference shapefile...\n")
  shp_reference <- st_read(file.path(reference_path, reference_fname), quiet = TRUE)
  
  # Compute intersection
  cat("  Computing intersections...\n")
  intersect <- st_intersection(shp_reporting, shp_reference) %>%
    mutate(area = st_area(.))
  
  # Compute weights
  cat("  Computing area weights...\n")
  intersect <- intersect %>%
    mutate(weight = as.numeric(area) / as.numeric(area_base))
  
  # Renormalize weights to ensure that weights sum up to 1 for each unique reporting_geoid
  cat("  Normalizing weights...\n")
  intersect <- intersect %>%
    group_by(across(all_of(reporting_geoid))) %>%
    mutate(weight = weight / sum(weight)) %>%
    ungroup()
  
  # Keep only relevant columns and rename using configuration
  output <- intersect %>%
    select(all_of(c(reporting_geoid, reference_geoid, "weight"))) %>%
    rename_with(
      .fn = ~ case_when(
        . == reporting_geoid ~ paste0("GISJOIN_", y),
        . == reference_geoid ~ get_tract_id_var(),
        TRUE ~ .
      ),
      .cols = everything()
    ) %>% 
    # drop 0 weights...
    filter(weight > 0) %>% 
    # drop geometry
    st_drop_geometry()
  
  # Quality check: weights should sum to 1 for each source tract
  weight_check <- output %>%
    group_by(!!sym(paste0("GISJOIN_", y))) %>%
    summarize(total_weight = sum(weight), .groups = 'drop')
  
  weights_not_one <- sum(abs(weight_check$total_weight - 1) > 0.001)
  if (weights_not_one > 0) {
    cat("  WARNING:", weights_not_one, "source tracts have weights not summing to 1.0\n")
  }
  
  # Save the output
  cat("  Saving crosswalk:", output_fname, "\n")
  write_csv(output, output_file)
  
  cat("  Completed crosswalk for", y, "->", TRACT_YEAR, 
      "(", nrow(output), "tract pairs )\n\n")
}

cat("Crosswalk construction completed!\n")
cat("Target tract year:", TRACT_YEAR, "\n")
cat("Output directory:", output_path, "\n")

# Final check: verify all required crosswalks exist
cat("\nVerifying crosswalk availability...\n")
check_crosswalks_exist()