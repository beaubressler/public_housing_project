####
# Download 2010 ACS Data from NHGIS using API
#
# This script uses the ipumsr package to programmatically download
# 2010 ACS 5-year (2006-2010) tract-level data for income, housing,
# employment, occupation, and education variables.
####

library(ipumsr)
library(tidyverse)
library(here)

# Setup API key (only need to do this once)
# Get your key from: https://account.ipums.org/api_keys
# Then run: set_ipums_api_key("YOUR-KEY-HERE", save = TRUE)

# Check if API key is set
# if (!ipums_api_key_exists()) {
#   stop("IPUMS API key not found. Please run:\n",
#        "  set_ipums_api_key('YOUR-KEY-HERE', save = TRUE)\n",
#        "Get your key from: https://account.ipums.org/api_keys")
# }

# Define output directories
income_dir <- here("data", "raw", "nhgis", "tables", "income", "2010")
housing_dir <- here("data", "raw", "nhgis", "tables", "housing", "2010")
employment_dir <- here("data", "raw", "nhgis", "tables", "employment", "2010")
occupation_dir <- here("data", "raw", "nhgis", "tables", "occupation", "2010")

# Create directories if they don't exist
dir.create(income_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(housing_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(employment_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(occupation_dir, showWarnings = FALSE, recursive = TRUE)

cat("========================================\n")
cat("2010 ACS Data Download from NHGIS\n")
cat("========================================\n\n")

# Define NHGIS extract for 2010 ACS 5-year data
# Dataset: 2006-2010 ACS (ds184 in NHGIS)
cat("Defining data extract request...\n")

extract_def <- define_extract_nhgis(
  description = "2010 ACS 5-year tract data for public housing project",
  datasets = ds_spec(
    "2006_2010_ACS5b",  # 5-Year Data [2006-2010, Tracts & Larger Areas]
    data_tables = c(
      # Income
      "B19019",  # Median Household Income by Household Size

      # Housing
      "B25097",  # Median Value (Dollars) by Mortgage Status
      "B25111",  # Median Gross Rent by Year Structure Built

      # Employment
      "B23001",  # Sex by Age by Employment Status

      # Occupation
      "C24060",  # Occupation by Class of Worker

      # Education
      "B15001"   # Sex by Age by Educational Attainment
    ),
    geog_levels = "tract"
  )
)

cat("Submitting extract request to NHGIS...\n")
cat("This may take a moment...\n\n")

# Submit the extract
submitted_extract <- submit_extract(extract_def)

cat("Extract submitted successfully!\n")
cat("Extract Number:", submitted_extract$number, "\n")
cat("Status:", submitted_extract$status, "\n\n")

# Wait for extract to be ready
cat("Waiting for NHGIS to process your extract...\n")
cat("This typically takes 5-30 minutes depending on NHGIS server load.\n")
cat("You can check status at: https://data2.nhgis.org/extracts\n\n")

# Poll for completion (check every 30 seconds)
extract_complete <- FALSE
while (!extract_complete) {
  Sys.sleep(30)  # Wait 30 seconds between checks

  # Check extract status
  extract_info <- get_extract_info(submitted_extract)

  cat("Status:", extract_info$status, "\n")

  if (extract_info$status == "completed") {
    extract_complete <- TRUE
    cat("\n✓ Extract completed successfully!\n\n")
  } else if (extract_info$status == "failed") {
    stop("Extract failed. Please check NHGIS website for details:\n",
         "https://data2.nhgis.org/extracts\n")
  }
}

# Download the extract
cat("Downloading extract files...\n")
download_path <- download_extract(submitted_extract,
                                   download_dir = here("data", "raw", "nhgis", "tables"))

cat("✓ Download complete!\n")
cat("Files saved to:", download_path, "\n\n")

# Extract the ZIP file
cat("Extracting files...\n")
unzip_dir <- here("data", "raw", "nhgis", "tables", "2010_extract")
dir.create(unzip_dir, showWarnings = FALSE, recursive = TRUE)
unzip(download_path, exdir = unzip_dir)

# Find and organize the CSV files (search recursively)
csv_files <- list.files(unzip_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

if (length(csv_files) == 0) {
  warning("No CSV files found in extract. Check the unzip directory.")
} else {
  cat("Found", length(csv_files), "CSV file(s)\n\n")

  # Copy files to appropriate directories
  # Since all tables are in one file, copy to each directory
  for (csv_file in csv_files) {
    file.copy(csv_file, income_dir, overwrite = TRUE)
    file.copy(csv_file, housing_dir, overwrite = TRUE)
    file.copy(csv_file, employment_dir, overwrite = TRUE)
    file.copy(csv_file, occupation_dir, overwrite = TRUE)
    cat("  Copied:", basename(csv_file), "\n")
  }

  # Also copy codebook
  codebook_files <- list.files(unzip_dir, pattern = "_codebook\\.txt$", full.names = TRUE, recursive = TRUE)
  if (length(codebook_files) > 0) {
    for (codebook in codebook_files) {
      file.copy(codebook, income_dir, overwrite = TRUE)
      file.copy(codebook, housing_dir, overwrite = TRUE)
      file.copy(codebook, employment_dir, overwrite = TRUE)
      file.copy(codebook, occupation_dir, overwrite = TRUE)
    }
    cat("  Copied codebook\n")
  }
}

cat("\n========================================\n")
cat("Download Complete!\n")
cat("========================================\n\n")
cat("Files saved to:\n")
cat("  -", income_dir, "\n")
cat("  -", housing_dir, "\n")
cat("  -", employment_dir, "\n")
cat("  -", occupation_dir, "\n\n")

cat("Next steps:\n")
cat("1. Verify the downloaded data looks correct\n")
cat("2. Run the tract crosswalk script to create 2010 -> 1950 crosswalk\n")
cat("3. Update the census data cleaning scripts to include 2010\n\n")

cat("You can now proceed with data processing!\n")
