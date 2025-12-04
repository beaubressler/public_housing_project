library(tidyverse)
library(haven)
library(here)

cat("\n")
cat("=======================================================\n")
cat("Cleaning and Crosswalking Opportunity Insights Data\n")
cat("=======================================================\n\n")

# Read OI data
cat("Reading Opportunity Insights tract outcomes data...\n")
oi_raw <- read_dta(here("data", "raw", "opportunity_insights", "tract_outcomes_simple.dta"))

cat("Raw OI data:", nrow(oi_raw), "tracts\n")

# Convert to GISJOIN_2010 format
cat("Converting to GISJOIN format...\n")
oi_2010 <- oi_raw %>%
  mutate(
    state_str = sprintf("%02d", state),
    county_str = sprintf("%03d", county),
    tract_str = sprintf("%06d", tract),
    GISJOIN_2010 = paste0("G", state_str, "0", county_str, "0", tract_str)
  ) %>%
  select(-state_str, -county_str, -tract_str)

# Read 2010 -> 1950 crosswalk
cat("Reading 2010 -> 1950 crosswalk...\n")
crosswalk <- read_csv(
  here("data", "derived", "geographic_crosswalks", "tract_concordance_weights2010_to_1950.csv"),
  show_col_types = FALSE
)

cat("Crosswalk contains", nrow(crosswalk), "tract pairs\n")
cat("  Unique 2010 tracts:", n_distinct(crosswalk$GISJOIN_2010), "\n")
cat("  Unique 1950 tracts:", n_distinct(crosswalk$GISJOIN_1950), "\n")

# Join OI data with crosswalk
cat("\nJoining OI data with crosswalk...\n")
oi_with_crosswalk <- oi_2010 %>%
  inner_join(crosswalk, by = "GISJOIN_2010")

cat("After join:", nrow(oi_with_crosswalk), "rows\n")
cat("Unique 2010 tracts matched:", n_distinct(oi_with_crosswalk$GISJOIN_2010), "\n")

# Identify outcome variables to aggregate
outcome_vars <- names(oi_2010) %>%
  str_subset("^(kfr|jail)_") %>%
  str_subset("_p25$")

count_vars <- names(oi_2010) %>%
  str_subset("_count$")

cat("\nOutcome variables identified:", length(outcome_vars), "\n")
cat("Count variables for weighting:", length(count_vars), "\n")

# Function to aggregate OI outcomes to 1950 tracts with population weighting
aggregate_to_1950 <- function(data, outcome_var, count_var) {
  # For each outcome, use corresponding count variable as population weight
  # Combined weight = crosswalk weight × population count

  data %>%
    filter(!is.na(!!sym(outcome_var)), !is.na(!!sym(count_var))) %>%
    mutate(combined_weight = weight * !!sym(count_var)) %>%
    group_by(GISJOIN_1950) %>%
    summarize(
      !!sym(outcome_var) := weighted.mean(!!sym(outcome_var), w = combined_weight, na.rm = TRUE),
      !!sym(paste0(outcome_var, "_n_tracts")) := n(),
      !!sym(paste0(outcome_var, "_total_count")) := sum(!!sym(count_var), na.rm = TRUE),
      .groups = 'drop'
    )
}

# Map outcome variables to their count variables
outcome_count_pairs <- tribble(
  ~outcome, ~count,
  # Pooled outcomes
  "kfr_pooled_pooled_p25", "pooled_pooled_count",
  "jail_pooled_pooled_p25", "pooled_pooled_count",
  # Black outcomes
  "kfr_black_pooled_p25", "black_pooled_count",
  "jail_black_pooled_p25", "black_pooled_count",
  # Hispanic outcomes
  "kfr_hisp_pooled_p25", "hisp_pooled_count",
  "jail_hisp_pooled_p25", "hisp_pooled_count",
  # White outcomes
  "kfr_white_pooled_p25", "white_pooled_count",
  "jail_white_pooled_p25", "white_pooled_count",
  # Gender outcomes - pooled
  "kfr_pooled_male_p25", "pooled_male_count",
  "jail_pooled_male_p25", "pooled_male_count",
  "kfr_pooled_female_p25", "pooled_female_count",
  "jail_pooled_female_p25", "pooled_female_count",
  # Gender outcomes - Black
  "kfr_black_male_p25", "black_male_count",
  "jail_black_male_p25", "black_male_count",
  "kfr_black_female_p25", "black_female_count",
  "jail_black_female_p25", "black_female_count",
  # Gender outcomes - White
  "kfr_white_male_p25", "white_male_count",
  "jail_white_male_p25", "white_male_count",
  "kfr_white_female_p25", "white_female_count",
  "jail_white_female_p25", "white_female_count"
)

cat("\nAggregating outcomes to 1950 tracts...\n")

# Aggregate each outcome separately, then join together
oi_1950_list <- list()

for (i in 1:nrow(outcome_count_pairs)) {
  outcome_var <- outcome_count_pairs$outcome[i]
  count_var <- outcome_count_pairs$count[i]

  cat("  Processing", outcome_var, "...\n")

  oi_1950_list[[i]] <- aggregate_to_1950(oi_with_crosswalk, outcome_var, count_var)
}

# Join all outcomes together
cat("\nCombining all outcomes...\n")
oi_1950 <- oi_1950_list[[1]]

for (i in 2:length(oi_1950_list)) {
  oi_1950 <- oi_1950 %>%
    full_join(oi_1950_list[[i]], by = "GISJOIN_1950")
}

# Save output
output_file <- here("data", "derived", "opportunity_insights", "oi_outcomes_1950_tracts.csv")

# Create directory if it doesn't exist
dir.create(here("data", "derived", "opportunity_insights"), showWarnings = FALSE, recursive = TRUE)

cat("\nSaving aggregated OI outcomes to 1950 tracts...\n")
write_csv(oi_1950, output_file)

cat("\nCompleted!\n")
cat("  Output:", nrow(oi_1950), "1950 tracts with OI outcomes\n")
cat("  File:", output_file, "\n")

# Summary statistics
cat("\nSummary of key outcomes:\n")
cat("  Upward mobility (pooled):\n")
cat("    Mean:", round(mean(oi_1950$kfr_pooled_pooled_p25, na.rm = TRUE), 2), "\n")
cat("    SD:", round(sd(oi_1950$kfr_pooled_pooled_p25, na.rm = TRUE), 2), "\n")
cat("    N:", sum(!is.na(oi_1950$kfr_pooled_pooled_p25)), "\n")

cat("  Incarceration rate (pooled):\n")
cat("    Mean:", round(mean(oi_1950$jail_pooled_pooled_p25, na.rm = TRUE), 3), "\n")
cat("    SD:", round(sd(oi_1950$jail_pooled_pooled_p25, na.rm = TRUE), 3), "\n")
cat("    N:", sum(!is.na(oi_1950$jail_pooled_pooled_p25)), "\n")

cat("\n=======================================================\n")
cat("OI Data Cleaning and Crosswalking Complete!\n")
cat("=======================================================\n\n")
