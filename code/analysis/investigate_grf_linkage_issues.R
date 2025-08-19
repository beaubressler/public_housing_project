####
# Investigation Script: GRF Linkage Issues for NYC ED Data
# This script investigates why only 27.8% of NYC's population is captured
# in the ED data despite full-count census being 100% coverage
####

library(tidyverse)
library(here)

cat("=== INVESTIGATING GRF LINKAGE ISSUES ===\n\n")

# The hypothesis: Full-count records exist for all of NYC, but the Geographic Reference File (GRF)
# isn't successfully linking them to enumeration districts for the full geography

# STEP 1: Check the GRF file structure and NYC coverage
cat("STEP 1: Examining Geographic Reference File\n")
cat("-------------------------------------------\n")

grf_file <- here("data", "derived", "geographic_reference_file", "grf_1940_histid_ed_city.csv")

if(file.exists(grf_file)) {
  grf_data <- read.csv(grf_file)
  cat("✓ GRF file found with", nrow(grf_data), "records\n")
  cat("GRF columns:", paste(names(grf_data), collapse = ", "), "\n\n")
  
  # Look for NYC entries
  if("city" %in% names(grf_data) && "state" %in% names(grf_data)) {
    nyc_grf <- grf_data %>% 
      filter(state == "NY", 
             city %in% c("New York", "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island", "StatenIsland"))
    
    cat("NYC records in GRF:\n")
    if(nrow(nyc_grf) > 0) {
      grf_summary <- nyc_grf %>%
        group_by(city) %>%
        summarise(n_records = n(), .groups = 'drop') %>%
        arrange(desc(n_records))
      print(grf_summary)
      cat("Total NYC GRF records:", sum(grf_summary$n_records), "\n")
    } else {
      cat("No NYC records found in GRF!\n")
      cat("Available cities in NY:", paste(unique(grf_data$city[grf_data$state == "NY"]), collapse = ", "), "\n")
    }
  } else {
    cat("GRF structure doesn't match expected format\n")
    cat("Sample records:\n")
    print(head(grf_data))
  }
} else {
  cat("✗ GRF file not found\n")
}

# STEP 2: Check the full-count data structure
cat("\nSTEP 2: Examining Full-Count Data Structure\n")
cat("------------------------------------------\n")

# Check what's in the raw full-count files
full_count_dir <- here("data", "raw", "ipums", "full_count")
full_count_files <- list.files(full_count_dir, pattern = "*.dat.gz|*.xml", full.names = TRUE)

cat("Full-count files available:\n")
for(file in full_count_files) {
  file_info <- file.info(file)
  cat("  -", basename(file), "- Size:", round(file_info$size / 1e9, 2), "GB\n")
}

# STEP 3: Check the ED processing pipeline for linking issues
cat("\nSTEP 3: Examining ED Processing Pipeline\n")
cat("---------------------------------------\n")

# Look at the collapse_full_count_to_ed.R script to see how the linking works
processing_script <- here("code", "build", "full_count", "collapse_full_count_to_ed.R")
if(file.exists(processing_script)) {
  cat("✓ Found ED processing script\n")
  
  # Read key parts of the script to understand the workflow
  script_lines <- readLines(processing_script)
  
  # Look for GRF-related operations
  grf_lines <- grep("grf|GRF", script_lines, ignore.case = TRUE, value = TRUE)
  if(length(grf_lines) > 0) {
    cat("GRF-related operations in script:\n")
    for(i in 1:min(5, length(grf_lines))) {
      cat("  ", grf_lines[i], "\n")
    }
  }
  
  # Look for NYC-specific processing
  nyc_lines <- grep("New York|NYC|Bronx|Brooklyn|Manhattan|Queens|Staten", script_lines, ignore.case = TRUE, value = TRUE)
  if(length(nyc_lines) > 0) {
    cat("\nNYC-related operations in script:\n")
    for(i in 1:min(3, length(nyc_lines))) {
      cat("  ", nyc_lines[i], "\n")
    }
  }
} else {
  cat("✗ ED processing script not found\n")
}

# STEP 4: Check intermediate processing files
cat("\nSTEP 4: Checking Intermediate Processing Files\n")
cat("---------------------------------------------\n")

# Check if there are intermediate files that show the linking process
full_count_output_dir <- here("data", "derived", "census", "full_count")
intermediate_dirs <- c("raw", "by_city", "ed_by_city")

for(dir_name in intermediate_dirs) {
  dir_path <- file.path(full_count_output_dir, dir_name)
  if(dir.exists(dir_path)) {
    files <- list.files(dir_path, full.names = FALSE)
    cat("✓", dir_name, "directory contains", length(files), "files\n")
    if(length(files) > 0) {
      cat("    Files:", paste(head(files, 3), collapse = ", "), "\n")
    }
  } else {
    cat("✗", dir_name, "directory not found\n")
  }
}

# STEP 5: Test hypothesis about Urban Transitions ED files
cat("\nSTEP 5: Examining Urban Transitions ED Files\n")
cat("-------------------------------------------\n")

# The Urban Transitions files should contain the ED boundaries and identifiers
# If these are incomplete, that could explain the linkage gaps

urban_transitions_dir <- here("data", "raw", "urban_transitions", "enumeration_districts")

# Check if the ED files have been extracted
extracted_dirs <- c("ForEachDecade", "ForEachDecade_old1", "ForEachDecade_old2")
for(dir_name in extracted_dirs) {
  dir_path <- file.path(urban_transitions_dir, dir_name)
  if(dir.exists(dir_path)) {
    files <- list.files(dir_path, pattern = "*NY*", full.names = FALSE)
    cat("✓", dir_name, "contains", length(files), "NY-related files\n")
    
    # Check specifically for NYC boroughs
    nyc_files <- files[grepl("Bronx|Brooklyn|Manhattan|Queens|Staten", files, ignore.case = TRUE)]
    if(length(nyc_files) > 0) {
      cat("    NYC files:", paste(nyc_files, collapse = ", "), "\n")
    }
  }
}

# STEP 6: Summary and recommendations
cat("\nSTEP 6: Summary and Next Steps\n")
cat("------------------------------\n")

cat("HYPOTHESIS: GRF linkage is incomplete for NYC\n")
cat("- Full-count data: 100% US coverage (should include all NYC)\n")
cat("- Current ED output: Only 27.8% of NYC population\n")
cat("- Gap: 72.2% of NYC records not linked to EDs\n\n")

cat("LIKELY CAUSES:\n")
cat("1. GRF missing many NYC enumeration districts\n")
cat("2. Urban Transitions ED files not fully processed\n")
cat("3. Geographic matching failures in processing pipeline\n")
cat("4. City name/boundary definition mismatches\n\n")

cat("NEXT STEPS TO INVESTIGATE:\n")
cat("1. Extract and examine Urban Transitions NYC ED files\n")
cat("2. Compare ED coverage in Urban Transitions vs GRF\n")
cat("3. Check processing logs for NYC-specific errors\n")
cat("4. Test processing pipeline on individual NYC boroughs\n")

cat("\n=== INVESTIGATION COMPLETE ===\n")