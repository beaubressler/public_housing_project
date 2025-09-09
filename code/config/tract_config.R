# Tract Configuration System
# 
# This file provides a centralized configuration system for switching between
# different census tract year definitions (1950 vs 2000) without changing
# analysis code throughout the pipeline.
#
# Usage: Change TRACT_YEAR to switch tract definitions
# - TRACT_YEAR = 1950: Use 1950 census tract boundaries (default/current)
# - TRACT_YEAR = 2000: Use 2000 census tract boundaries
#
# All analysis code continues to use GISJOIN_1950 as the tract identifier,
# but this variable will point to whichever tract definition is selected.

library(here)

# ============================================================================
# CONFIGURATION
# ============================================================================

# Set the target tract year for analysis
# Change this to 1950 or 2000 to switch tract definitions
TRACT_YEAR <- 1950

# ============================================================================
# DERIVED VARIABLES AND HELPER FUNCTIONS
# ============================================================================

# The actual tract variable name that corresponds to TRACT_YEAR
get_tract_id_var <- function() {
  paste0("GISJOIN_", TRACT_YEAR)
}

# Standard tract ID variables used throughout the codebase
# Note: GISJOIN_1950 remains the standard variable name regardless of TRACT_YEAR
tract_id_variables <- c("YEAR", "GISJOIN_1950")

# Helper function to get crosswalk filename for any source year
get_crosswalk_filename <- function(from_year, to_year = TRACT_YEAR) {
  paste0("tract_concordance_weights", from_year, "_to_", to_year, ".csv")
}

# Helper function to get crosswalk file path
get_crosswalk_filepath <- function(from_year, to_year = TRACT_YEAR) {
  here("data", "derived", "geographic_crosswalks", get_crosswalk_filename(from_year, to_year))
}

# Helper function to get reference tract shapefile path
get_tract_reference_path <- function(year = TRACT_YEAR) {
  here("data", "raw", "nhgis", "gis", paste0("nhgis0027_shapefile_tl2000_us_tract_", year))
}

# Helper function to get reference tract shapefile name
get_tract_reference_filename <- function(year = TRACT_YEAR) {
  paste0("US_tract_", year, ".shp")
}

# Function to check if required crosswalks exist for current TRACT_YEAR
check_crosswalks_exist <- function(years = c("1930", "1940", "1960", "1970", "1980", "1990", "2000")) {
  # Remove the target year from the list (no need to crosswalk to itself)
  source_years <- years[years != as.character(TRACT_YEAR)]
  
  missing_crosswalks <- c()
  for (year in source_years) {
    crosswalk_path <- get_crosswalk_filepath(year, TRACT_YEAR)
    if (!file.exists(crosswalk_path)) {
      missing_crosswalks <- c(missing_crosswalks, year)
    }
  }
  
  if (length(missing_crosswalks) > 0) {
    cat("WARNING: Missing crosswalks to", TRACT_YEAR, "for years:", paste(missing_crosswalks, collapse = ", "), "\n")
    cat("Run crosswalk construction script to generate missing crosswalks.\n")
    return(FALSE)
  }
  
  return(TRUE)
}

# Function to rename tract variable to standard GISJOIN_1950 name
# This allows all downstream code to continue using GISJOIN_1950 regardless of actual tract year
standardize_tract_variable <- function(data) {
  actual_tract_var <- get_tract_id_var()
  
  if (TRACT_YEAR != 1950 && actual_tract_var %in% names(data)) {
    # Rename the actual tract variable to GISJOIN_1950
    data <- data %>%
      rename(GISJOIN_1950 = !!sym(actual_tract_var))
    
    cat("Renamed", actual_tract_var, "to GISJOIN_1950 for compatibility\n")
  }
  
  return(data)
}

# ============================================================================
# CONFIGURATION VALIDATION AND LOGGING
# ============================================================================

# Validate configuration
validate_tract_config <- function() {
  valid_years <- c(1950, 2000)
  
  if (!TRACT_YEAR %in% valid_years) {
    stop("TRACT_YEAR must be one of: ", paste(valid_years, collapse = ", "))
  }
  
  # Check if reference shapefile exists
  ref_path <- get_tract_reference_path()
  ref_file <- file.path(ref_path, get_tract_reference_filename())
  
  if (!file.exists(ref_file)) {
    stop("Reference tract shapefile not found: ", ref_file)
  }
  
  return(TRUE)
}

# Log current configuration
log_tract_config <- function() {
  cat("\n")
  cat("=======================================================\n")
  cat("TRACT CONFIGURATION\n")
  cat("=======================================================\n")
  cat("Target tract year:", TRACT_YEAR, "\n")
  cat("Tract ID variable:", get_tract_id_var(), "\n")
  cat("Reference shapefile:", get_tract_reference_filename(), "\n")
  
  if (TRACT_YEAR != 1950) {
    cat("NOTE: Analysis code will use GISJOIN_1950 as alias for", get_tract_id_var(), "\n")
  }
  
  cat("=======================================================\n")
  cat("\n")
}

# ============================================================================
# INITIALIZATION
# ============================================================================

# Validate configuration when this file is sourced
validate_tract_config()

# Log configuration
log_tract_config()

# Check crosswalk availability (warning only, don't stop execution)
check_crosswalks_exist()