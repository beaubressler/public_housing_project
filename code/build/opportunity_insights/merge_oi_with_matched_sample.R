library(tidyverse)
library(here)

cat("\n")
cat("=======================================================\n")
cat("Merging OI Outcomes with Matched Sample\n")
cat("=======================================================\n\n")

# Read OI outcomes (1950 tracts)
cat("Reading OI outcomes data...\n")
oi_outcomes <- read_csv(
  here("data", "derived", "opportunity_insights", "oi_outcomes_1950_tracts.csv"),
  show_col_types = FALSE
)

cat("OI outcomes:", nrow(oi_outcomes), "1950 tracts\n\n")

# Loop through matching specifications
matching_specs <- c("baseline", "no_cbsa", "caliper")

for (spec in matching_specs) {

  cat("\n=======================================================\n")
  cat("Processing matching specification:", spec, "\n")
  cat("=======================================================\n\n")

  # Read matched sample data (2-year matched dataset)
  cat("Reading matched sample data (2-year)...\n")
  matched_data <- read_csv(
    here("data", "derived", "merged", "combined", "matched_dataset", spec, "tract_data_matched_2_year.csv"),
    show_col_types = FALSE
  )

  cat("Matched sample:", nrow(matched_data), "tract-year observations\n")
  cat("  Unique tracts:", n_distinct(matched_data$GISJOIN_1950), "\n")
  cat("  Years:", paste(sort(unique(matched_data$year)), collapse = ", "), "\n")

  # Get baseline (1980) characteristics for each tract
  # OI outcomes are measured around 2010s for 1978-1983 cohorts, so 1980 is appropriate baseline
  cat("\nExtracting 1980 baseline characteristics...\n")
  baseline_1980 <- matched_data %>%
    filter(year == 1980) %>%
    select(
      GISJOIN_1950,
      cbsa_title,
      match_group,
      group_type,
      location_type,  # ADDED: needed for correct treatment definition
      treated,
      treatment_year,
      # Key baseline variables
      black_share,
      white_share,
      median_income,
      median_rent_calculated,
      total_pop,
      distance_from_cbd,
      redlined_binary_80pp,
      # Additional characteristics
      unemp_rate,
      pct_hs_grad,
      total_public_housing_units
    )

  # Get 1970 characteristics for additional controls
  cat("\nExtracting 1970 baseline characteristics...\n")
  baseline_1970 <- matched_data %>%
    filter(year == 1970) %>%
    select(
      GISJOIN_1950,
      black_share_1970 = black_share,
      median_income_1970 = median_income,
      total_pop_1970 = total_pop,
      unemp_rate_1970 = unemp_rate,
      median_rent_calculated_1970 = median_rent_calculated
    ) %>%
    distinct(GISJOIN_1950, .keep_all = TRUE)

  # Merge 1970 characteristics onto 1980 baseline
  baseline_1980 <- baseline_1980 %>%
    left_join(baseline_1970, by = "GISJOIN_1950")

  cat("Baseline sample:", nrow(baseline_1980), "tracts\n")
  cat("\nSample structure:\n")
  cat("  By location_type:\n")
  baseline_1980 %>%
    count(location_type) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    print()

  cat("\n  Sanity check - sample sizes by location_type:\n")
  cat("    Treated tracts:", sum(baseline_1980$location_type == "treated"), "\n")
  cat("    Donor pool for treated:",
      sum(baseline_1980$location_type == "donor_pool" & baseline_1980$group_type == "treated"), "\n")
  cat("    Inner ring tracts:", sum(baseline_1980$location_type == "inner"), "\n")
  cat("    Donor pool for inner:",
      sum(baseline_1980$location_type == "donor_pool" & baseline_1980$group_type == "inner"), "\n")

  # Merge OI outcomes with baseline characteristics
  cat("\nMerging OI outcomes with matched sample...\n")
  oi_matched <- baseline_1980 %>%
    left_join(oi_outcomes, by = "GISJOIN_1950")

  # Check merge quality
  n_missing <- sum(is.na(oi_matched$kfr_pooled_pooled_p25))
  pct_missing <- round(100 * n_missing / nrow(oi_matched), 1)

  cat("\nMerge diagnostics:\n")
  cat("  Total tracts:", nrow(oi_matched), "\n")
  cat("  Tracts with OI data:", sum(!is.na(oi_matched$kfr_pooled_pooled_p25)), "\n")
  cat("  Tracts missing OI data:", n_missing, "(", pct_missing, "%)\n")

  # Check by location_type
  cat("\nOI data availability by location_type:\n")
  oi_matched %>%
    group_by(location_type) %>%
    summarize(
      n = n(),
      with_oi = sum(!is.na(kfr_pooled_pooled_p25)),
      pct_with_oi = round(100 * with_oi / n, 1),
      .groups = 'drop'
    ) %>%
    print()

  # Save merged dataset
  output_file <- here("data", "derived", "opportunity_insights", paste0("oi_matched_sample_", spec, ".csv"))

  cat("\nSaving merged dataset...\n")
  write_csv(oi_matched, output_file)

  cat("  Output file:", output_file, "\n")

  # Summary statistics by location_type
  cat("\nSummary of OI outcomes by location_type:\n")

  summary_by_location <- oi_matched %>%
    group_by(location_type) %>%
    summarize(
      n = n(),
      # Upward mobility
      kfr_mean = mean(kfr_pooled_pooled_p25, na.rm = TRUE),
      kfr_sd = sd(kfr_pooled_pooled_p25, na.rm = TRUE),
      kfr_n = sum(!is.na(kfr_pooled_pooled_p25)),
      # Incarceration
      jail_mean = mean(jail_pooled_pooled_p25, na.rm = TRUE),
      jail_sd = sd(jail_pooled_pooled_p25, na.rm = TRUE),
      jail_n = sum(!is.na(jail_pooled_pooled_p25)),
      .groups = 'drop'
    )

  print(summary_by_location)

  # Quick comparisons
  cat("\n========================================\n")
  cat("PRELIMINARY COMPARISONS\n")
  cat("========================================\n")

  # Comparison 1: Treated vs their donors
  cat("\n1. TREATED VS DONOR POOL:\n")

  treated_donors <- oi_matched %>%
    filter(group_type == "treated")

  if (nrow(treated_donors) > 0) {
    kfr_ttest <- t.test(
      kfr_pooled_pooled_p25 ~ I(location_type == "treated"),
      data = treated_donors
    )

    cat("  Upward mobility:\n")
    cat("    Treated:", round(kfr_ttest$estimate[2], 3), "\n")
    cat("    Donors:", round(kfr_ttest$estimate[1], 3), "\n")
    cat("    Difference:", round(kfr_ttest$estimate[2] - kfr_ttest$estimate[1], 3), "\n")
    cat("    p-value:", round(kfr_ttest$p.value, 4), "\n")

    jail_ttest <- t.test(
      jail_pooled_pooled_p25 ~ I(location_type == "treated"),
      data = treated_donors
    )

    cat("  Incarceration:\n")
    cat("    Treated:", round(jail_ttest$estimate[2], 4), "\n")
    cat("    Donors:", round(jail_ttest$estimate[1], 4), "\n")
    cat("    Difference:", round(jail_ttest$estimate[2] - jail_ttest$estimate[1], 4), "\n")
    cat("    p-value:", round(jail_ttest$p.value, 4), "\n")
  }

  # Comparison 2: Inner vs their donors
  cat("\n2. INNER RING VS DONOR POOL:\n")

  inner_donors <- oi_matched %>%
    filter(group_type == "inner")

  if (nrow(inner_donors) > 0) {
    kfr_ttest <- t.test(
      kfr_pooled_pooled_p25 ~ I(location_type == "inner"),
      data = inner_donors
    )

    cat("  Upward mobility:\n")
    cat("    Inner:", round(kfr_ttest$estimate[2], 3), "\n")
    cat("    Donors:", round(kfr_ttest$estimate[1], 3), "\n")
    cat("    Difference:", round(kfr_ttest$estimate[2] - kfr_ttest$estimate[1], 3), "\n")
    cat("    p-value:", round(kfr_ttest$p.value, 4), "\n")

    jail_ttest <- t.test(
      jail_pooled_pooled_p25 ~ I(location_type == "inner"),
      data = inner_donors
    )

    cat("  Incarceration:\n")
    cat("    Inner:", round(jail_ttest$estimate[2], 4), "\n")
    cat("    Donors:", round(jail_ttest$estimate[1], 4), "\n")
    cat("    Difference:", round(jail_ttest$estimate[2] - jail_ttest$estimate[1], 4), "\n")
    cat("    p-value:", round(jail_ttest$p.value, 4), "\n")
  }

} # End matching specification loop

cat("\n=======================================================\n")
cat("All Matching Specifications Complete!\n")
cat("=======================================================\n\n")
