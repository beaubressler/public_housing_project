####
# Explore available 2010 ACS tables in NHGIS
#
# This script helps identify the correct table codes for the 2010 ACS data
####

library(ipumsr)
library(tidyverse)
library(here)

# Get metadata for 2010 ACS datasets
cat("Fetching NHGIS metadata for datasets...\n\n")

# Get all available datasets
datasets <- get_metadata_nhgis(type = "datasets")

cat("Total datasets available:", nrow(datasets), "\n\n")

# Filter for ACS datasets
acs_datasets <- datasets %>%
  filter(str_detect(description, "ACS"))

cat("ACS datasets found:\n")
print(acs_datasets %>% select(name, description) %>% head(20))

# Look specifically for 2010 or 2006-2010
cat("\n\nLooking for 2010 ACS datasets:\n")
acs_2010 <- acs_datasets %>%
  filter(str_detect(name, "2010") | str_detect(description, "2010") |
         str_detect(name, "2006") | str_detect(description, "2006"))

print(acs_2010 %>% select(name, description))

# If we found datasets, use the first one, otherwise try the standard name
if (nrow(acs_2010) > 0) {
  dataset_name <- acs_2010$name[1]
  cat("\n\nUsing dataset:", dataset_name, "\n")
} else {
  dataset_name <- "2010_ACS5a"
  cat("\n\nTrying standard dataset name:", dataset_name, "\n")
}

cat("\nGetting data tables for:", dataset_name, "...\n")

# Get data tables for the dataset
tables <- get_metadata_nhgis(dataset = dataset_name)

cat("\n=== Available Data Tables ===\n\n")

# Look for income tables
cat("INCOME tables:\n")
income_tables <- tables %>%
  filter(str_detect(description, regex("income", ignore_case = TRUE))) %>%
  select(name, description)
print(income_tables)

cat("\n\nHOUSING/RENT tables:\n")
housing_tables <- tables %>%
  filter(str_detect(description, regex("rent|value|housing|median gross|vacancy", ignore_case = TRUE))) %>%
  select(name, description)
print(housing_tables)

cat("\n\nEMPLOYMENT tables:\n")
employment_tables <- tables %>%
  filter(str_detect(description, regex("employment|labor force|unemploy", ignore_case = TRUE))) %>%
  select(name, description)
print(employment_tables)

cat("\n\nOCCUPATION tables:\n")
occupation_tables <- tables %>%
  filter(str_detect(description, regex("occupation", ignore_case = TRUE))) %>%
  select(name, description)
print(occupation_tables)

cat("\n\nEDUCATION tables:\n")
education_tables <- tables %>%
  filter(str_detect(description, regex("education|educational attainment", ignore_case = TRUE))) %>%
  select(name, description)
print(education_tables)

# Save results for reference
cat("\n\nSaving table list to file...\n")
output_dir <- here::here("data", "raw", "nhgis", "tables", "2010")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(tables, file.path(output_dir, "available_tables.csv"))
cat("Saved to:", file.path(output_dir, "available_tables.csv"), "\n")

cat("\n\n=== RECOMMENDED TABLES ===\n")
cat("Based on the search, use these table codes in your download script:\n\n")

# Try to find the right ones
if (nrow(income_tables) > 0) {
  cat("Income: ", income_tables$name[1], "\n")
}
if (nrow(housing_tables) > 0) {
  rent_idx <- which(str_detect(housing_tables$description, regex("gross rent", ignore_case = TRUE)))[1]
  value_idx <- which(str_detect(housing_tables$description, regex("median value", ignore_case = TRUE)))[1]
  if (!is.na(rent_idx)) cat("Median Rent: ", housing_tables$name[rent_idx], "\n")
  if (!is.na(value_idx)) cat("Median Home Value: ", housing_tables$name[value_idx], "\n")
}
if (nrow(employment_tables) > 0) {
  cat("Employment: ", employment_tables$name[1], "\n")
}
if (nrow(occupation_tables) > 0) {
  cat("Occupation: ", occupation_tables$name[1], "\n")
}
if (nrow(education_tables) > 0) {
  cat("Education: ", education_tables$name[1], "\n")
}
