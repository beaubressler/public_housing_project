####
# Investigation Script: Finding Missing NYC ED Data
# This script systematically checks why only 27.8% of NYC's 1940 population 
# is covered in the ED data (2.07M vs 7.45M expected)
####

library(tidyverse)
library(here)

cat("=== INVESTIGATING MISSING NYC ED DATA ===\n\n")

# STEP 1: Check what cities are in the processing list
cat("STEP 1: Checking cities list in processing script\n")
cat("----------------------------------------------\n")

# Read the processing script to see what cities are included
script_lines <- readLines(here("code", "build", "full_count", "collapse_full_count_to_ed.R"))
cities_start <- grep("cities <- c\\(", script_lines)
cities_end <- grep("\\)", script_lines[cities_start:length(script_lines)])[1] + cities_start - 1

cities_lines <- script_lines[cities_start:cities_end]
cities_text <- paste(cities_lines, collapse = " ")

# Extract NYC boroughs
nyc_boroughs_in_list <- c()
if(grepl("BronxNY", cities_text)) nyc_boroughs_in_list <- c(nyc_boroughs_in_list, "Bronx")
if(grepl("BrooklynNY", cities_text)) nyc_boroughs_in_list <- c(nyc_boroughs_in_list, "Brooklyn") 
if(grepl("ManhattanNY", cities_text)) nyc_boroughs_in_list <- c(nyc_boroughs_in_list, "Manhattan")
if(grepl("QueensNY", cities_text)) nyc_boroughs_in_list <- c(nyc_boroughs_in_list, "Queens")
if(grepl("StatenIslandNY", cities_text)) nyc_boroughs_in_list <- c(nyc_boroughs_in_list, "StatenIsland")
if(grepl("RichmondNY", cities_text)) nyc_boroughs_in_list <- c(nyc_boroughs_in_list, "Richmond")

cat("NYC boroughs in processing cities list:", paste(nyc_boroughs_in_list, collapse = ", "), "\n")
cat("Missing boroughs:", paste(setdiff(c("Bronx", "Brooklyn", "Manhattan", "Queens", "StatenIsland"), nyc_boroughs_in_list), collapse = ", "), "\n\n")

# STEP 2: Check Urban Transitions data availability
cat("STEP 2: Checking Urban Transitions ED data files\n")
cat("-----------------------------------------------\n")

urban_transitions_dir <- here("data", "raw", "urban_transitions", "enumeration_districts")

# Check ForEachDecade archives
if(file.exists(file.path(urban_transitions_dir, "ForEachDecade", "ForEachCity1940.zip"))) {
  cat("✓ Found ForEachCity1940.zip\n")
  # List contents without extracting
  zip_contents <- system(paste0("unzip -l '", file.path(urban_transitions_dir, "ForEachDecade", "ForEachCity1940.zip"), "'"), intern = TRUE)
  nyc_files <- grep("NewYork|Bronx|Brooklyn|Manhattan|Queens|Staten|Richmond", zip_contents, ignore.case = TRUE, value = TRUE)
  if(length(nyc_files) > 0) {
    cat("NYC-related files in archive:\n")
    cat(paste(nyc_files, collapse = "\n"), "\n")
  } else {
    cat("No obvious NYC files found in ForEachCity1940.zip\n")
  }
} else {
  cat("✗ ForEachCity1940.zip not found\n")
}

# Check individual borough files
cat("\nChecking individual borough files:\n")
borough_files <- c("BronxNY.zip", "BrooklynNY.zip", "ManhattanNY.zip", "QueensNY.zip", "StatenIslandNY.zip", "RichmondNY.zip")
old2_dir <- file.path(urban_transitions_dir, "ForEachDecade_old2")

for(borough_file in borough_files) {
  file_path <- file.path(old2_dir, borough_file)
  if(file.exists(file_path)) {
    cat("✓", borough_file, "found\n")
  } else {
    cat("✗", borough_file, "missing\n")
  }
}

# STEP 3: Check Geographic Reference File
cat("\nSTEP 3: Checking Geographic Reference File\n")
cat("------------------------------------------\n")

grf_file <- here("data", "derived", "geographic_reference_file", "grf_1940_histid_ed_city.csv")
if(file.exists(grf_file)) {
  cat("✓ Found GRF file\n")
  grf_data <- read.csv(grf_file)
  cat("Total GRF records:", nrow(grf_data), "\n")
  
  if("city" %in% names(grf_data)) {
    nyc_cities <- grf_data %>% 
      filter(grepl("new york|bronx|brooklyn|manhattan|queens|staten|richmond", city, ignore.case = TRUE)) %>%
      group_by(city) %>%
      summarise(n_records = n(), .groups = 'drop')
    
    if(nrow(nyc_cities) > 0) {
      cat("NYC cities in GRF:\n")
      print(nyc_cities)
    } else {
      cat("No NYC cities found in GRF\n")
    }
  } else {
    cat("GRF columns:", paste(names(grf_data), collapse = ", "), "\n")
  }
} else {
  cat("✗ GRF file not found\n")
}

# STEP 4: Check existing ED output
cat("\nSTEP 4: Checking existing ED output files\n") 
cat("-----------------------------------------\n")

ed_output_dir <- here("data", "derived", "census", "full_count", "ed_by_city")
ed_files <- list.files(ed_output_dir, pattern = "*.csv", full.names = TRUE)

cat("ED output files found:", length(ed_files), "\n")
for(file in ed_files) {
  cat("  -", basename(file), "\n")
}

# Check the main 1940 file we've been using
main_1940_file <- file.path(ed_output_dir, "city_ed_data_1940.csv")
if(file.exists(main_1940_file)) {
  ed_1940 <- read.csv(main_1940_file)
  
  # Check city coverage
  city_summary <- ed_1940 %>%
    group_by(CITY_NAME) %>%
    summarise(n_eds = n(), total_pop = sum(white_pop + black_pop + other_pop, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(total_pop))
  
  cat("\nTop 10 cities by population in ED data:\n")
  print(head(city_summary, 10))
  
  # NYC breakdown by b_city 
  nyc_eds <- ed_1940 %>% filter(CITY_NAME == "New York, NY")
  if(nrow(nyc_eds) > 0) {
    nyc_breakdown <- nyc_eds %>%
      mutate(borough = str_sub(b_city, 1, -3)) %>%
      group_by(borough) %>%
      summarise(n_eds = n(), total_pop = sum(white_pop + black_pop + other_pop, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_pop))
    
    cat("\nNYC borough breakdown in ED data:\n")
    print(nyc_breakdown)
  }
}

# STEP 5: Summary and recommendations
cat("\nSTEP 5: Summary and Next Steps\n")
cat("------------------------------\n")

cat("Expected NYC population (Health Areas): 7.45M\n")
cat("Current ED data population: 2.07M (27.8% coverage)\n")
cat("Missing population: 5.38M (72.2%)\n\n")

cat("LIKELY CAUSES:\n")
cat("1. Missing boroughs in cities processing list\n")
cat("2. Urban Transitions data not fully processed\n") 
cat("3. GRF mapping issues\n")
cat("4. ED data files not extracted/processed\n\n")

cat("RECOMMENDED NEXT STEPS:\n")
cat("1. Add missing NYC boroughs to cities list in collapse_full_count_to_ed.R\n")
cat("2. Extract and process Urban Transitions ZIP files\n")
cat("3. Re-run ED processing pipeline\n")
cat("4. Check GRF processing for NYC coverage\n")

cat("\n=== INVESTIGATION COMPLETE ===\n")