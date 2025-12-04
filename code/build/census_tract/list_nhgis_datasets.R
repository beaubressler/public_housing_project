####
# List all available NHGIS datasets to find the correct 2010 ACS dataset name
####

library(ipumsr)
library(tidyverse)

cat("Fetching all NHGIS datasets...\n")
cat("This may take a moment...\n\n")

# Get all datasets
all_datasets <- get_metadata_nhgis(type = "datasets")

cat("Total datasets available:", nrow(all_datasets), "\n\n")

# Show just ACS datasets
cat("=== ALL ACS DATASETS ===\n")
acs_datasets <- all_datasets %>%
  filter(str_detect(description, "ACS")) %>%
  arrange(name) %>%
  select(name, description)

print(acs_datasets, n = 100)

# Filter for anything with 2010
cat("\n\n=== DATASETS WITH '2010' ===\n")
datasets_2010 <- all_datasets %>%
  filter(str_detect(name, "2010") | str_detect(description, "2010")) %>%
  select(name, description)

print(datasets_2010, n = 100)

# Save to file for reference
write_csv(all_datasets, "nhgis_all_datasets.csv")
cat("\n\nFull dataset list saved to: nhgis_all_datasets.csv\n")
cat("\nLook for the correct dataset name in the output above.\n")
cat("It might be something like:\n")
cat("  - 2010_ACS5a\n")
cat("  - 2006_2010_ACS5a\n")
cat("  - 2010_SF1a\n")
cat("  - 2010_SF2a\n")
