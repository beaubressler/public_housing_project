library(tidyverse)
library(here)
library(fixest)
library(modelsummary)
library(tinytable)

# Suppress automatic stars footnote in modelsummary
options(modelsummary_stars_note = FALSE)

# Helper function to save tables using tabular environment
save_oi_tables <- function(models, base_name, title = NULL, notes = NULL, coef_map = NULL, spanning_header = NULL) {
  tex_file <- file.path(output_dir, paste0(base_name, ".tex"))

  tab <- modelsummary(
    models,
    output = "tinytable",
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = c("nobs", "r.squared"),
    coef_map = coef_map,
    fmt = 3
  )

  # Add spanning header if specified
  if (!is.null(spanning_header)) {
    tab <- group_tt(tab, j = spanning_header)
  }

  # Apply tabular theme for threeparttable compatibility
  tab <- theme_tt(tab, theme = "tabular")

  # Save to file
  save_tt(tab, output = tex_file, overwrite = TRUE)

  # Clean up - remove table wrappers, keep only tabular environment
  tex_content <- readLines(tex_file)

  # Remove table wrappers
  tex_content <- tex_content[!grepl("\\\\begin\\{table\\}", tex_content)]
  tex_content <- tex_content[!grepl("\\\\end\\{table\\}", tex_content)]
  tex_content <- tex_content[!grepl("\\\\centering", tex_content)]

  writeLines(tex_content, tex_file)

  cat("  Saved", base_name, "\n")
}

cat("\n")
cat("=======================================================\n")
cat("Opportunity Insights Outcomes Analysis\n")
cat("=======================================================\n\n")

# Loop through matching specifications
matching_specs <- c("baseline", "no_cbsa", "caliper")

for (spec in matching_specs) {

  cat("\n=======================================================\n")
  cat("Analyzing matching specification:", spec, "\n")
  cat("=======================================================\n\n")

  # Read merged data
  cat("Reading merged OI matched sample data...\n")
  oi_data <- read_csv(
    here("data", "derived", "opportunity_insights", paste0("oi_matched_sample_", spec, ".csv")),
    show_col_types = FALSE
  )

  cat("Sample size:", nrow(oi_data), "tracts\n\n")

  # Create output directory
  output_dir <- here("output", "opportunity_insights", spec)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # ===========================================================================
  # DIAGNOSTIC PLOTS
  # ===========================================================================

  cat("\n\nCREATING DIAGNOSTIC PLOTS\n")
  cat("=======================================================\n\n")

  # Distribution plots
  p1 <- ggplot(oi_data, aes(x = kfr_pooled_pooled_p25, fill = location_type)) +
    geom_density(alpha = 0.5) +
    labs(title = "Distribution of Upward Mobility by Location Type",
         x = "Household Income Rank (p25 parents)", y = "Density") +
    theme_minimal()

  ggsave(file.path(output_dir, "dist_mobility_by_location.pdf"), p1, width = 8, height = 5)

  p2 <- ggplot(oi_data, aes(x = jail_pooled_pooled_p25, fill = location_type)) +
    geom_density(alpha = 0.5) +
    labs(title = "Distribution of Incarceration Rate by Location Type",
         x = "Incarceration Rate", y = "Density") +
    theme_minimal()

  ggsave(file.path(output_dir, "dist_incarceration_by_location.pdf"), p2, width = 8, height = 5)

  cat("  Saved distribution plots\n")

  # Balance check plots
  balance_vars <- c("black_share", "median_income", "total_pop", "distance_from_cbd")

  for (var in balance_vars) {
    p <- ggplot(oi_data, aes(x = location_type, y = .data[[var]])) +
      geom_boxplot() +
      labs(title = paste("Baseline", var, "by Location Type"),
           x = "Location Type", y = var) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave(file.path(output_dir, paste0("balance_", var, ".pdf")), p, width = 7, height = 5)
  }

  cat("  Saved balance check plots\n")

  # Scatter plot of matched pairs - Treated vs Donors
  treated_data <- oi_data %>%
    filter(group_type == "treated") %>%
    select(match_group, location_type, kfr_pooled_pooled_p25, jail_pooled_pooled_p25) %>%
    pivot_wider(names_from = location_type, values_from = c(kfr_pooled_pooled_p25, jail_pooled_pooled_p25))

  p3 <- ggplot(treated_data, aes(x = kfr_pooled_pooled_p25_donor_pool, y = kfr_pooled_pooled_p25_treated)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Upward Mobility: Treated vs Matched Donors",
         x = "Donor Pool", y = "Treated") +
    theme_minimal()

  ggsave(file.path(output_dir, "scatter_treated_donors_mobility.pdf"), p3, width = 6, height = 6)

  cat("  Saved scatter plots\n\n")

  # ===========================================================================
  # ANALYSIS 1: TREATED VS THEIR MATCHED DONORS
  # ===========================================================================

  cat("ANALYSIS 1: TREATED VS THEIR MATCHED DONORS\n")
  cat("=======================================================\n\n")

  treated_sample <- oi_data %>%
    filter(group_type == "treated") %>%
    mutate(treated = location_type == "treated")

  cat("Sample size:", nrow(treated_sample), "\n")
  cat("  Treated:", sum(treated_sample$location_type == "treated"), "\n")
  cat("  Donors:", sum(treated_sample$location_type == "donor_pool"), "\n\n")

  # Upward mobility
  cat("1. UPWARD MOBILITY\n\n")

  kfr_treated_fe <- feols(kfr_pooled_pooled_p25 ~ treated | match_group,
                          data = treated_sample)

  modelsummary(
    list("Mobility" = kfr_treated_fe),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # Incarceration
  cat("\n2. INCARCERATION\n\n")

  jail_treated_fe <- feols(jail_pooled_pooled_p25 ~ treated | match_group,
                           data = treated_sample)

  modelsummary(
    list("Incarceration" = jail_treated_fe),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # Test with 1970 controls
  cat("\n3. WITH 1970 CONTROLS\n\n")

  kfr_treated_fe_1970ctrl <- feols(
    kfr_pooled_pooled_p25 ~ treated + black_share_1970 + median_income_1970 +
      total_pop_1970 + unemp_rate_1970 + median_rent_calculated_1970 | match_group,
    data = treated_sample
  )

  jail_treated_fe_1970ctrl <- feols(
    jail_pooled_pooled_p25 ~ treated + black_share_1970 + median_income_1970 +
      total_pop_1970 + unemp_rate_1970 + median_rent_calculated_1970 | match_group,
    data = treated_sample
  )

  modelsummary(
    list("Mobility (1970 controls)" = kfr_treated_fe_1970ctrl,
         "Incarceration (1970 controls)" = jail_treated_fe_1970ctrl),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # Test with 1980 controls
  cat("\n4. WITH 1980 CONTROLS (ROBUSTNESS)\n\n")

  kfr_treated_fe_1980ctrl <- feols(
    kfr_pooled_pooled_p25 ~ treated + black_share + median_income +
      total_pop + unemp_rate + median_rent_calculated | match_group,
    data = treated_sample
  )

  jail_treated_fe_1980ctrl <- feols(
    jail_pooled_pooled_p25 ~ treated + black_share + median_income +
      total_pop + unemp_rate + median_rent_calculated | match_group,
    data = treated_sample
  )

  modelsummary(
    list("Mobility (1980 controls)" = kfr_treated_fe_1980ctrl,
         "Incarceration (1980 controls)" = jail_treated_fe_1980ctrl),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # ===========================================================================
  # ANALYSIS 2: INNER RING VS THEIR MATCHED DONORS (SPILLOVER)
  # ===========================================================================

  cat("\n\nANALYSIS 2: INNER RING VS THEIR MATCHED DONORS\n")
  cat("=======================================================\n\n")

  inner_sample <- oi_data %>%
    filter(group_type == "inner") %>%
    mutate(treated = location_type == "inner")

  cat("Sample size:", nrow(inner_sample), "\n")
  cat("  Inner ring:", sum(inner_sample$location_type == "inner"), "\n")
  cat("  Donors:", sum(inner_sample$location_type == "donor_pool"), "\n\n")

  # Upward mobility
  cat("1. UPWARD MOBILITY\n\n")

  kfr_inner_fe <- feols(kfr_pooled_pooled_p25 ~ treated | match_group,
                        data = inner_sample)

  modelsummary(
    list("Mobility" = kfr_inner_fe),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # Incarceration
  cat("\n2. INCARCERATION\n\n")

  jail_inner_fe <- feols(jail_pooled_pooled_p25 ~ treated | match_group,
                         data = inner_sample)

  modelsummary(
    list("Incarceration" = jail_inner_fe),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # Test with 1970 controls
  cat("\n3. WITH 1970 CONTROLS\n\n")

  kfr_inner_fe_1970ctrl <- feols(
    kfr_pooled_pooled_p25 ~ treated + black_share_1970 + median_income_1970 +
      total_pop_1970 + unemp_rate_1970 + median_rent_calculated_1970 | match_group,
    data = inner_sample
  )

  jail_inner_fe_1970ctrl <- feols(
    jail_pooled_pooled_p25 ~ treated + black_share_1970 + median_income_1970 +
      total_pop_1970 + unemp_rate_1970 + median_rent_calculated_1970 | match_group,
    data = inner_sample
  )

  modelsummary(
    list("Mobility (1970 controls)" = kfr_inner_fe_1970ctrl,
         "Incarceration (1970 controls)" = jail_inner_fe_1970ctrl),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # Test with 1980 controls
  cat("\n4. WITH 1980 CONTROLS (ROBUSTNESS)\n\n")

  kfr_inner_fe_1980ctrl <- feols(
    kfr_pooled_pooled_p25 ~ treated + black_share + median_income +
      total_pop + unemp_rate + median_rent_calculated | match_group,
    data = inner_sample
  )

  jail_inner_fe_1980ctrl <- feols(
    jail_pooled_pooled_p25 ~ treated + black_share + median_income +
      total_pop + unemp_rate + median_rent_calculated | match_group,
    data = inner_sample
  )

  modelsummary(
    list("Mobility (1980 controls)" = kfr_inner_fe_1980ctrl,
         "Incarceration (1980 controls)" = jail_inner_fe_1980ctrl),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # ===========================================================================
  # HETEROGENEITY: TREATED VS DONORS
  # ===========================================================================

  cat("\n\nHETEROGENEITY ANALYSIS: TREATED VS DONORS\n")
  cat("=======================================================\n\n")

  # By baseline Black share
  cat("1. BY BASELINE BLACK SHARE\n\n")

  treated_sample <- treated_sample %>%
    mutate(high_black_share = black_share > median(black_share, na.rm = TRUE))

  kfr_het_black <- feols(
    kfr_pooled_pooled_p25 ~ I(location_type == "treated") +
      I(location_type == "treated"):high_black_share | match_group,
    data = treated_sample
  )

  jail_het_black <- feols(
    jail_pooled_pooled_p25 ~ I(location_type == "treated") +
      I(location_type == "treated"):high_black_share | match_group,
    data = treated_sample
  )

  modelsummary(
    list("Mobility" = kfr_het_black, "Incarceration" = jail_het_black),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # ===========================================================================
  # RACE-SPECIFIC ANALYSIS
  # ===========================================================================

  cat("\n\nRACE-SPECIFIC ANALYSIS: TREATED VS DONORS\n")
  cat("=======================================================\n\n")

  # Treated vs donors, race-specific outcomes
  treated_sample_race <- oi_data %>%
    filter(group_type == "treated")

  # Black children outcomes
  cat("BLACK CHILDREN:\n\n")

  kfr_black_fe <- feols(
    kfr_black_pooled_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_race
  )

  jail_black_fe <- feols(
    jail_black_pooled_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_race
  )

  modelsummary(
    list("Mobility" = kfr_black_fe, "Incarceration" = jail_black_fe),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # White children outcomes
  cat("\n\nWHITE CHILDREN:\n\n")

  kfr_white_fe <- feols(
    kfr_white_pooled_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_race
  )

  jail_white_fe <- feols(
    jail_white_pooled_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_race
  )

  modelsummary(
    list("Mobility" = kfr_white_fe, "Incarceration" = jail_white_fe),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # ===========================================================================
  # GENDER-SPECIFIC ANALYSIS
  # ===========================================================================

  cat("\n\nGENDER-SPECIFIC ANALYSIS: TREATED VS DONORS\n")
  cat("=======================================================\n\n")

  treated_sample_gender <- oi_data %>%
    filter(group_type == "treated")

  # Male outcomes
  cat("MALE CHILDREN:\n\n")

  kfr_male_fe <- feols(
    kfr_pooled_male_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_gender
  )

  jail_male_fe <- feols(
    jail_pooled_male_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_gender
  )

  modelsummary(
    list("Mobility" = kfr_male_fe, "Incarceration" = jail_male_fe),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # Female outcomes
  cat("\n\nFEMALE CHILDREN:\n\n")

  kfr_female_fe <- feols(
    kfr_pooled_female_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_gender
  )

  jail_female_fe <- feols(
    jail_pooled_female_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_gender
  )

  modelsummary(
    list("Mobility" = kfr_female_fe, "Incarceration" = jail_female_fe),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # ===========================================================================
  # RACE × GENDER ANALYSIS
  # ===========================================================================

  cat("\n\nRACE × GENDER ANALYSIS: TREATED VS DONORS\n")
  cat("=======================================================\n\n")

  # Black male vs Black female
  cat("BLACK CHILDREN BY GENDER:\n\n")

  kfr_black_male_fe <- feols(
    kfr_black_male_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_gender
  )

  jail_black_male_fe <- feols(
    jail_black_male_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_gender
  )

  kfr_black_female_fe <- feols(
    kfr_black_female_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_gender
  )

  jail_black_female_fe <- feols(
    jail_black_female_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_gender
  )

  modelsummary(
    list(
      "Black Male: Mobility" = kfr_black_male_fe,
      "Black Male: Jail" = jail_black_male_fe,
      "Black Female: Mobility" = kfr_black_female_fe,
      "Black Female: Jail" = jail_black_female_fe
    ),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # White male vs White female
  cat("\n\nWHITE CHILDREN BY GENDER:\n\n")

  kfr_white_male_fe <- feols(
    kfr_white_male_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_gender
  )

  jail_white_male_fe <- feols(
    jail_white_male_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_gender
  )

  kfr_white_female_fe <- feols(
    kfr_white_female_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_gender
  )

  jail_white_female_fe <- feols(
    jail_white_female_p25 ~ I(location_type == "treated") | match_group,
    data = treated_sample_gender
  )

  modelsummary(
    list(
      "White Male: Mobility" = kfr_white_male_fe,
      "White Male: Jail" = jail_white_male_fe,
      "White Female: Mobility" = kfr_white_female_fe,
      "White Female: Jail" = jail_white_female_fe
    ),
    stars = TRUE,
    gof_map = c("nobs", "r.squared")
  )

  # ===========================================================================
  # SAVE REGRESSION TABLES
  # ===========================================================================

  # ===========================================================================
  # MAGNITUDE CALCULATIONS
  # ===========================================================================

  cat("\n\nMAGNITUDE INTERPRETATIONS\n")
  cat("=======================================================\n\n")

  # Calculate means and SDs for donor pool
  donors_treated <- treated_sample %>% filter(location_type == "donor_pool")

  # Main outcomes
  mean_mobility <- mean(donors_treated$kfr_pooled_pooled_p25, na.rm = TRUE)
  sd_mobility <- sd(donors_treated$kfr_pooled_pooled_p25, na.rm = TRUE)
  mean_jail <- mean(donors_treated$jail_pooled_pooled_p25, na.rm = TRUE)
  sd_jail <- sd(donors_treated$jail_pooled_pooled_p25, na.rm = TRUE)

  cat("TREATED VS DONORS:\n")
  cat("  Mobility baseline (mean):", round(mean_mobility, 3), "\n")
  cat("  Mobility baseline (SD):", round(sd_mobility, 3), "\n")
  cat("  Effect: -0.017 = ", round(-0.017/mean_mobility * 100, 1), "% of mean, ",
      round(-0.017/sd_mobility, 2), " SD\n\n", sep = "")

  cat("  Incarceration baseline (mean):", round(mean_jail, 3), "\n")
  cat("  Incarceration baseline (SD):", round(sd_jail, 3), "\n")
  cat("  Effect: 0.006 = ", round(0.006/mean_jail * 100, 1), "% of mean, ",
      round(0.006/sd_jail, 2), " SD\n\n", sep = "")

  # Race-specific
  mean_black_mobility <- mean(donors_treated$kfr_black_pooled_p25, na.rm = TRUE)
  sd_black_mobility <- sd(donors_treated$kfr_black_pooled_p25, na.rm = TRUE)
  mean_white_mobility <- mean(donors_treated$kfr_white_pooled_p25, na.rm = TRUE)
  sd_white_mobility <- sd(donors_treated$kfr_white_pooled_p25, na.rm = TRUE)

  cat("RACE-SPECIFIC:\n")
  cat("  Black mobility baseline:", round(mean_black_mobility, 3), "(SD:", round(sd_black_mobility, 3), ")\n")
  cat("  Effect: -0.012 = ", round(-0.012/mean_black_mobility * 100, 1), "% of mean, ",
      round(-0.012/sd_black_mobility, 2), " SD\n\n", sep = "")

  cat("  White mobility baseline:", round(mean_white_mobility, 3), "(SD:", round(sd_white_mobility, 3), ")\n")
  cat("  Effect: -0.011 = ", round(-0.011/mean_white_mobility * 100, 1), "% of mean, ",
      round(-0.011/sd_white_mobility, 2), " SD\n\n", sep = "")

  # Gender-specific
  mean_male_mobility <- mean(donors_treated$kfr_pooled_male_p25, na.rm = TRUE)
  sd_male_mobility <- sd(donors_treated$kfr_pooled_male_p25, na.rm = TRUE)
  mean_female_mobility <- mean(donors_treated$kfr_pooled_female_p25, na.rm = TRUE)
  sd_female_mobility <- sd(donors_treated$kfr_pooled_female_p25, na.rm = TRUE)

  cat("GENDER-SPECIFIC:\n")
  cat("  Male mobility baseline:", round(mean_male_mobility, 3), "(SD:", round(sd_male_mobility, 3), ")\n")
  cat("  Effect: -0.009 = ", round(-0.009/mean_male_mobility * 100, 1), "% of mean, ",
      round(-0.009/sd_male_mobility, 2), " SD\n\n", sep = "")

  cat("  Female mobility baseline:", round(mean_female_mobility, 3), "(SD:", round(sd_female_mobility, 3), ")\n")
  cat("  Effect: -0.017 = ", round(-0.017/mean_female_mobility * 100, 1), "% of mean, ",
      round(-0.017/sd_female_mobility, 2), " SD\n\n", sep = "")

  cat("\n\nSaving regression tables...\n")

  # Original combined table (for slides - no 1970 controls)
  save_oi_tables(
    list(
      "Mobility" = kfr_treated_fe,
      "Incarceration" = jail_treated_fe,
      " Mobility" = kfr_inner_fe,
      " Incarceration" = jail_inner_fe
    ),
    "oi_combined_results_original",
    title = "OI Outcomes: Treated and Nearby vs Matched Donors",
    notes = "All specifications include match group fixed effects.",
    coef_map = c('treatedTRUE' = "Treated"),
    spanning_header = list("Treated Tracts" = 2:3, "Nearby Tracts" = 4:5)
  )

  # Paper tables: Treated vs donors (with and without 1980 controls)
  save_oi_tables(
    list(
      "(1)" = kfr_treated_fe,
      "(2)" = kfr_treated_fe_1980ctrl,
      "(3)" = jail_treated_fe,
      "(4)" = jail_treated_fe_1980ctrl
    ),
    "oi_treated_vs_donors",
    title = "OI Outcomes: Treated vs Matched Donor Pool",
    notes = "All specifications include match group fixed effects. Columns 2 and 4 control for 1980 neighborhood characteristics (Black share, median income, total population, unemployment rate, median rent).",
    coef_map = c('treatedTRUE' = "Treated"),
    spanning_header = list("Mobility" = 2:3, "Incarceration" = 4:5)
  )

  # Paper tables: Inner ring vs donors (with and without 1980 controls)
  save_oi_tables(
    list(
      "(1)" = kfr_inner_fe,
      "(2)" = kfr_inner_fe_1980ctrl,
      "(3)" = jail_inner_fe,
      "(4)" = jail_inner_fe_1980ctrl
    ),
    "oi_inner_vs_donors",
    title = "OI Outcomes: Inner Ring vs Matched Donor Pool",
    notes = "All specifications include match group fixed effects. Columns 2 and 4 control for 1980 neighborhood characteristics (Black share, median income, total population, unemployment rate, median rent).",
    coef_map = c('treatedTRUE' = "Nearby"),
    spanning_header = list("Mobility" = 2:3, "Incarceration" = 4:5)
  )

  # Robustness tables: Treated vs donors with 1970 controls (for appendix)
  save_oi_tables(
    list(
      "(1)" = kfr_treated_fe,
      "(2)" = kfr_treated_fe_1970ctrl,
      "(3)" = jail_treated_fe,
      "(4)" = jail_treated_fe_1970ctrl
    ),
    "oi_treated_vs_donors_1970ctrl",
    title = "OI Outcomes: Treated vs Matched Donor Pool (1970 Controls)",
    notes = "All specifications include match group fixed effects. Columns 2 and 4 control for 1970 neighborhood characteristics (Black share, median income, total population, unemployment rate, median rent).",
    coef_map = c('treatedTRUE' = "Treated"),
    spanning_header = list("Mobility" = 2:3, "Incarceration" = 4:5)
  )

  # Robustness tables: Inner ring vs donors with 1970 controls (for appendix)
  save_oi_tables(
    list(
      "(1)" = kfr_inner_fe,
      "(2)" = kfr_inner_fe_1970ctrl,
      "(3)" = jail_inner_fe,
      "(4)" = jail_inner_fe_1970ctrl
    ),
    "oi_inner_vs_donors_1970ctrl",
    title = "OI Outcomes: Inner Ring vs Matched Donor Pool (1970 Controls)",
    notes = "All specifications include match group fixed effects. Columns 2 and 4 control for 1970 neighborhood characteristics (Black share, median income, total population, unemployment rate, median rent).",
    coef_map = c('treatedTRUE' = "Nearby"),
    spanning_header = list("Mobility" = 2:3, "Incarceration" = 4:5)
  )

  # Race-specific
  save_oi_tables(
    list("Black: Mobility" = kfr_black_fe, "Black: Jail" = jail_black_fe,
         "White: Mobility" = kfr_white_fe, "White: Jail" = jail_white_fe),
    "oi_race_specific_results",
    title = "Race-Specific Opportunity Insights Outcomes",
    notes = "All specifications include match group fixed effects."
  )

  # Gender-specific
  save_oi_tables(
    list("Male: Mobility" = kfr_male_fe, "Male: Jail" = jail_male_fe,
         "Female: Mobility" = kfr_female_fe, "Female: Jail" = jail_female_fe),
    "oi_gender_specific_results",
    title = "Gender-Specific Opportunity Insights Outcomes",
    notes = "All specifications include match group fixed effects."
  )

  # Race × Gender - Black children
  save_oi_tables(
    list("Black Male: Mobility" = kfr_black_male_fe, "Black Male: Jail" = jail_black_male_fe,
         "Black Female: Mobility" = kfr_black_female_fe, "Black Female: Jail" = jail_black_female_fe),
    "oi_race_gender_black_results",
    title = "Black Children: Gender-Specific Outcomes",
    notes = "All specifications include match group fixed effects."
  )

  # Race × Gender - White children
  save_oi_tables(
    list("White Male: Mobility" = kfr_white_male_fe, "White Male: Jail" = jail_white_male_fe,
         "White Female: Mobility" = kfr_white_female_fe, "White Female: Jail" = jail_white_female_fe),
    "oi_race_gender_white_results",
    title = "White Children: Gender-Specific Outcomes",
    notes = "All specifications include match group fixed effects."
  )

} # End matching specification loop

cat("\n=======================================================\n")
cat("All Matching Specifications Complete!\n")
cat("=======================================================\n\n")
