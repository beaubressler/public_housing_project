####
# Coarsened Exact Matching (CEM) for Public Housing Analysis
# Uses automatic binning to create balanced treatment/control groups
####

library(MatchIt)
library(sf)
library(lmtest)
library(fixest)
library(here)
library(RColorBrewer)
library(tidyverse)
library(clue)  # for solve_LSAP

# libraries for covariate testing
library(cobalt)
library(tableone)
library(kableExtra)
library(tinytable)

# 0. Load shared configuration -----
source(here("code", "learn", "matched_did", "matching", "matching_config.R"))

variant <- "cem"

# Load helper functions
source(here("code", "learn", "matched_did", "helpers", "balance_table_creation.R"))
source(here("code", "learn", "matched_did", "helpers", "time_series_plotting.R"))

# define directories
merged_data_dir <- here("data", "derived", "merged", data_type)
output_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset", variant)
dir.create(output_data_dir, recursive = TRUE, showWarnings = FALSE)

figures_dir <- here("output", "figures", "matched_did", data_type, variant)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

balance_table_dir <- here("output", "balance_tables", "matched_did", data_type, variant)
dir.create(balance_table_dir, recursive = TRUE, showWarnings = FALSE)

# define file paths
tract_with_treatment_status_filepath <-
  here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")
treated_tracts_panel_filepath <-
  here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")
event_study_rings_filepath <-
  here(merged_data_dir, "unique_tracts_and_rings.csv")

# read census data with treatment status
census_tract_sample_raw <-
  st_read(tract_with_treatment_status_filepath)

# read tract tracts panel
treated_tracts_panel_raw <-
  st_read(treated_tracts_panel_filepath) %>%
  st_drop_geometry()

# read in unique rings and tracts
tracts_and_rings <-
  read_csv(event_study_rings_filepath) %>%
  filter(location_type != "outer")

# data on total public housing units inner ring is exposed to
inner_ring_units <-
  read_csv(here(merged_data_dir, "inner_ring_tracts_first_treated.csv")) %>%
  select(-treatment_year)

## !! Set match vars for CEM
cem_match_vars <- c("total_pop", "black_pop", 
                "median_rent_calculated", "median_income",
                 "unemp_rate")

cem_match_vars <- c("asinh_pop_total", "asinh_pop_black", 
                    "asinh_median_rent_calculated", "asinh_median_income",
                    "unemp_rate")

# 1. Data Preparation ------
treated_tracts_panel <-
  treated_tracts_panel_raw %>%
  distinct(GISJOIN_1950, total_public_housing_units,
           total_original_project_size, largest_original_project_size,
           has_highrise_project)

census_tract_data <-
  census_tract_sample_raw %>%
  mutate(centroid = st_centroid(geom)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  select(-centroid) %>%
  left_join(treated_tracts_panel) %>%
  left_join(tracts_and_rings) %>%
  mutate(location_type = if_else(is.na(location_type), "donor_pool", location_type)) %>%
  mutate(asinh_pop_total = asinh(total_pop),
         asinh_pop_white = asinh(white_pop),
         asinh_pop_black = asinh(black_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent_calculated = asinh(median_rent_calculated),
         asinh_median_home_value_calculated = asinh(median_home_value_calculated),
         asinh_distance_from_cbd = asinh(distance_from_cbd),
         ln_population_density = log(population_density),
         asinh_private_population_estimate = asinh(private_population_estimate),
         asinh_private_black_population_estimate = asinh(private_black_population_estimate),
         asinh_private_white_population_estimate = asinh(private_white_population_estimate),
         log_private_units_estimate = asinh(total_private_units_estimate)
         ) %>%
  mutate(county_id = paste0(STATEA, COUNTYA)) %>%
  dplyr::rename(year = YEAR) %>%
  st_drop_geometry() %>%
  arrange(GISJOIN_1950, year)  %>%
  mutate(category_most_overlap = ifelse(is.na(category_most_overlap), "missing", category_most_overlap),
         grade_most_overlap = ifelse(is.na(grade_most_overlap), "missing", grade_most_overlap)) %>%
  left_join(inner_ring_units, by = c("GISJOIN_1950" = "GISJOIN_1950"))

# Use exact matching variables for baseline variant
cem_exact_matching_vars <- c("ur_binary_5pp")

# CEM matching function -----
cem_then_nearest_maha_antiexact <- function(
    data,
    treatment_year,
    match_vars,                      # e.g. c("total_pop","black_share","median_rent_calculated","median_income")
    group_type = "inner",
    pre_treatment_decades = 2,
    time_invariant_vars = NULL,
    exact_match_vars = NULL,
    cem_cutpoints = 3,
    cem_grouping = NULL,
    antiexact_vars = c("cbsa_title"),
    keep_cols = c("GISJOIN_1950","city","county_id","cbsa_title","cbd")
) {
  
  ## Testing
  # data <- census_tract_data
  # treatment_year <- 1970
  # match_vars <- c("total_pop","black_share","median_rent_calculated","median_income")
  # group_type = "inner"
  # pre_treatment_decades = 2
  # time_invariant_vars = c("asinh_distance_from_cbd")
  # exact_match_vars = c("redlined_binary_80pp", "ur_binary_5pp")
  # cem_cutpoints = NULL
  # cem_grouping = NULL
  # antiexact_vars = c("cbsa_title")
  # keep_cols = c("GISJOIN_1950","city","county_id","cbsa_title","cbd")
  
  
  
  stopifnot(is.numeric(treatment_year))
  pre_years <- if (pre_treatment_decades == 1) treatment_year - 10 else c(treatment_year - 10, treatment_year - 20)
  tv <- setdiff(match_vars, time_invariant_vars)

  # Build wide pre-treatment covariates
  matching_data <- data %>%
    filter(
      (location_type == group_type & treatment_year == !!treatment_year) |
        (location_type == "donor_pool" & year %in% pre_years)
    ) %>%
    mutate(category_most_overlap = as.character(category_most_overlap))

  wide <- matching_data %>%
    select(all_of(unique(c(
      keep_cols, "redlined_binary_80pp","ur_binary_5pp","category_most_overlap",
      "location_type","year","GISJOIN_1950","cbsa_title","county_id","city","cbd",
      tv, time_invariant_vars
    )))) %>%
    pivot_wider(
      names_from = year,
      values_from = all_of(tv),
      names_glue = "year_{year}_{.value}"
    ) %>%
    mutate(treatment_group = as.integer(location_type == group_type))

  # For 2-decade matching: create trend variables (t-10 minus t-20)
  if (pre_treatment_decades == 2) {
    t_minus_10 <- treatment_year - 10
    t_minus_20 <- treatment_year - 20

    for (v in tv) {
      trend_var <- paste0("trend_", v)
      level_t10 <- paste0("year_", t_minus_10, "_", v)
      level_t20 <- paste0("year_", t_minus_20, "_", v)

      wide[[trend_var]] <- wide[[level_t10]] - wide[[level_t20]]
    }

    # Match on: levels at t-10 + trends + time-invariant + exact
    match_cols <- c(
      paste0("year_", t_minus_10, "_", tv),  # Levels at t-10
      paste0("trend_", tv),                   # Trends from t-20 to t-10
      time_invariant_vars,
      exact_match_vars %||% character()
    )
  } else {
    # For 1-decade matching: just match on t-10 levels (existing behavior)
    match_cols <- c(
      as.vector(outer(pre_years, tv, function(y, v) paste0("year_", y, "_", v))),
      time_invariant_vars,
      exact_match_vars %||% character()
    )
  }

  wide <- wide %>% filter(complete.cases(across(all_of(match_cols))))

  # Print matching specification
  cat("Matching on", length(match_cols), "variables:\n")
  if (pre_treatment_decades == 2) {
    cat("  - Levels at t-10:", paste(paste0("year_", treatment_year - 10, "_", tv), collapse = ", "), "\n")
    cat("  - Trends (t-10 minus t-20):", paste(paste0("trend_", tv), collapse = ", "), "\n")
  } else {
    cat("  - Levels at t-10:", paste(paste0("year_", treatment_year - 10, "_", tv), collapse = ", "), "\n")
  }
  if (!is.null(time_invariant_vars)) {
    cat("  - Time-invariant:", paste(time_invariant_vars, collapse = ", "), "\n")
  }
  if (!is.null(exact_match_vars)) {
    cat("  - Exact matching:", paste(exact_match_vars, collapse = ", "), "\n")
  }
  cat("\n")

  # ---- CEM (k2k = FALSE) ----
  # if (is.null(cem_cutpoints)) {
  #   num_vars <- match_cols[vapply(match_cols, function(v) is.numeric(wide[[v]]), logical(1))]
  #   cem_cutpoints <- setNames(
  #     lapply(num_vars, function(v) stats::quantile(wide[[v]], probs = c(.25,.5,.75), na.rm = TRUE)),
  #     num_vars
  #   )
  # }
  
  cem_form <- as.formula(paste("treatment_group ~", paste(match_cols, collapse = " + ")))
  m_cem <- matchit(
    cem_form,
    data      = wide,
    method    = "cem",
    cutpoints = cem_cutpoints,
    grouping  = cem_grouping,
    k2k       = FALSE
  )
  
  in_support <- m_cem$weights > 0
  matched0 <- wide[in_support, ]
  # rename CEM subclass to avoid collision later
  matched0$cem_subclass <- m_cem$subclass[in_support]
  
  matched0 <- matched0 %>%
    group_by(cem_subclass) %>%
    filter(sum(treatment_group == 1) > 0, sum(treatment_group == 0) > 0) %>%
    ungroup()
  
  if (nrow(matched0) == 0L) {
    warning("No strata with both treated and controls after CEM.")
    return(list(long = matched0[0, ], pairs = tibble()))
  }
  
  # ---- Nearest (Mahalanobis) WITHIN CEM subclass + anti-exact ----
  # Use same variables as CEM matching for distance calculation
  if (pre_treatment_decades == 2) {
    # For 2-decade: use levels at t-10 + trends
    dist_vars <- c(
      paste0("year_", treatment_year - 10, "_", tv),  # Levels at t-10
      paste0("trend_", tv),                            # Trends
      time_invariant_vars
    )
  } else {
    # For 1-decade: use levels at t-10 only
    dist_vars <- c(
      as.vector(outer(pre_years, tv, function(y, v) paste0("year_", y, "_", v))),
      time_invariant_vars
    )
  }
  
  # Make sure NN covariates are complete
  matched0 <-
    matched0 %>%
    filter(complete.cases(across(all_of(dist_vars))))
  
  nn_form <- as.formula(paste("treatment_group ~", paste(dist_vars, collapse = " + ")))
  antiexact_formula <- if (length(antiexact_vars)) as.formula(paste("~", paste(antiexact_vars, collapse = " + "))) else NULL
  
  m_nn <- matchit(
    nn_form,
    data      = matched0,
    method    = "nearest",
    exact     = "cem_subclass",       # confine within CEM bins
    antiexact = "cbsa_title",    # e.g., ~ cbsa_title
    distance  = "mahalanobis",
    replace   = TRUE,
    ratio     = 1
  )
  
  # Print matching summary
  cat("Matched units:", sum(m_nn$weights[m_nn$weights > 0]), "\n")
  cat("Control units used:", sum(m_nn$weights[m_nn$treatment_group == 0] > 0), "\n")
  cat("Controls used multiple times:", 
      sum(m_nn$weights[matched0$treatment_group == 0] > 1), "\n\n")
  
  # Get matched data using get_matches() for replacement matching
  matched_data <- get_matches(m_nn, data = matched0)
  
  # Add treatment year and group type information
  matched_data$matched_treatment_year <- treatment_year
  matched_data$group_type <- group_type
  matched_data$match_type <- "cem"
  
  # Get variable names for reshaping
  year_vars <- grep("^year_", names(matched_data), value = TRUE)
  
  # Extract matching pairs information - just keep tract IDs and match info
  matched_pairs <- matched_data %>%
    select(GISJOIN_1950, subclass, weights,
           matched_treatment_year, treatment_group, group_type, match_type) %>%
    distinct()

  # Merge back to full long panel to get all years of data
  matched_data_long <- data %>%
    inner_join(matched_pairs, by = "GISJOIN_1950", relationship = "many-to-many") %>%
    arrange(GISJOIN_1950, year)

  return(list(matched_data = matched_data_long, pairs = matched_pairs))

}

res <- cem_then_nearest_maha_antiexact(
  data                 = census_tract_data,
  treatment_year       = 1970,
  match_vars           = c("total_pop","black_share","median_rent_calculated","median_income"),
  group_type           = "inner",
  pre_treatment_decades = 1,
  antiexact_vars       = c("cbsa_title")
)


matched_long  <- res$matched_data
matched_pairs <- res$pairs


# Run CEM matching -----
treatment_years <- unique(census_tract_data %>%
                            filter(!is.na(treatment_year)) %>%
                            pull(treatment_year))

# Initialize lists to store results
matched_datasets_cem <- list()
matched_pairs_cem <- list()

# Perform matching for each group and year
for (group in group_types) {
  for (year in treatment_years) {
    for (nyear in c(1, 2)) {
      df_name <- paste0("matched_data_", group, "_", nyear, "pretreatment_", year, "_cem")
      pairs_name <- paste0("matched_pairs_", group, "_", nyear, "pretreatment_", year, "_cem")

      cat("=== CEM Matching ===\n")
      cat("Group:", group, "| Year:", year, "| Pre-treatment decades:", nyear, "\n")

      result <- cem_then_nearest_maha_antiexact(
        data = census_tract_data,
        treatment_year = year,
        match_vars = cem_match_vars,
        group_type = group,
        pre_treatment_decades = nyear,
        time_invariant_vars = c("asinh_distance_from_cbd"),
        exact_match_vars = cem_exact_matching_vars,
        antiexact_vars = c("cbsa_title")
      )

      matched_datasets_cem[[df_name]] <- result[["matched_data"]]
      matched_pairs_cem[[pairs_name]] <- result[["pairs"]]
    }
  }
}

# Create combined matched datasets -----

# Combine 1-year pretreatment matching
all_matched_data_1_year_cem <-
  bind_rows(matched_datasets_cem$matched_data_treated_1pretreatment_1950_cem,
            matched_datasets_cem$matched_data_treated_1pretreatment_1960_cem,
            matched_datasets_cem$matched_data_treated_1pretreatment_1970_cem,
            matched_datasets_cem$matched_data_treated_1pretreatment_1980_cem,
            matched_datasets_cem$matched_data_inner_1pretreatment_1950_cem,
            matched_datasets_cem$matched_data_inner_1pretreatment_1960_cem,
            matched_datasets_cem$matched_data_inner_1pretreatment_1970_cem,
            matched_datasets_cem$matched_data_inner_1pretreatment_1980_cem
            ) %>%
  select(GISJOIN_1950, matched_treatment_year, subclass, group_type, weights) %>%
  distinct()

# Combine 2-year pretreatment matching
all_matched_data_2_year_cem <- bind_rows(
  matched_datasets_cem$matched_data_treated_2pretreatment_1950_cem,
  matched_datasets_cem$matched_data_treated_2pretreatment_1960_cem,
  matched_datasets_cem$matched_data_treated_2pretreatment_1970_cem,
  matched_datasets_cem$matched_data_treated_2pretreatment_1980_cem,
  matched_datasets_cem$matched_data_inner_2pretreatment_1950_cem,
  matched_datasets_cem$matched_data_inner_2pretreatment_1960_cem,
  matched_datasets_cem$matched_data_inner_2pretreatment_1970_cem,
  matched_datasets_cem$matched_data_inner_2pretreatment_1980_cem
) %>%
  select(GISJOIN_1950, matched_treatment_year, subclass, group_type, weights) %>%
  distinct()

# Merge matching information back to original dataset
tract_data_matched_1_year <- census_tract_data %>%
  left_join(all_matched_data_1_year_cem,
            by = "GISJOIN_1950",
            relationship = "many-to-many") %>%
  mutate(
    match_group = ifelse(!is.na(matched_treatment_year),
                    paste(matched_treatment_year, subclass, sep = "_"),
                         NA)
  ) %>%
  filter(!is.na(match_group))

# Merge for 2-year
tract_data_matched_2_year <- census_tract_data %>%
  left_join(all_matched_data_2_year_cem, by = "GISJOIN_1950", relationship = "many-to-many") %>%
  mutate(match_group = ifelse(!is.na(matched_treatment_year),
                              paste(matched_treatment_year, subclass, sep = "_"), NA)) %>%
  filter(!is.na(match_group))

# Summary statistics -----
cat("\n=== CEM MATCHING SUMMARY ===\n")
cat("Original treated tracts (all years):",
    nrow(census_tract_data %>% filter(location_type == "treated") %>% distinct(GISJOIN_1950)), "\n")

matched_summary <- tract_data_matched_1_year %>%
  group_by(group_type, location_type) %>%
  summarise(
    n_obs = n(),
    n_tracts = n_distinct(GISJOIN_1950),
    n_match_groups = n_distinct(match_group),
    .groups = 'drop'
  )

print(matched_summary)

# Additional cleaning -----
## Clean private population ----
tract_data_matched_1_year <- tract_data_matched_1_year %>%
  group_by(match_group, group_type) %>%
  mutate(
    has_missing_private_pop = any(is.na(private_population_estimate)),
    private_population_estimate_y = ifelse(has_missing_private_pop, NA, private_population_estimate),
    private_black_population_estimate = ifelse(has_missing_private_pop, NA, private_black_population_estimate),
    private_white_population_estimate = ifelse(has_missing_private_pop, NA, private_white_population_estimate),
    asinh_private_population_estimate = ifelse(has_missing_private_pop, NA, asinh_private_population_estimate),
    asinh_private_black_population_estimate = ifelse(has_missing_private_pop, NA, asinh_private_black_population_estimate),
    asinh_private_white_population_estimate = ifelse(has_missing_private_pop, NA, asinh_private_white_population_estimate)
  ) %>%
  ungroup()

## Merge spillover project size data -----
inner_spillover_characteristics <- tract_data_matched_1_year %>%
  filter(group_type == "inner", location_type == "inner",
         !is.na(total_public_housing_units_built_nearby)) %>%
  select(match_group, total_public_housing_units_built_nearby) %>%
  distinct() %>%
  mutate(
    spillover_size_bin = ntile(total_public_housing_units_built_nearby, 3),
    spillover_size_group = factor(case_when(
      spillover_size_bin == 1 ~ "Small Nearby",
      spillover_size_bin == 2 ~ "Medium Nearby",
      spillover_size_bin == 3 ~ "Large Nearby"
    ), levels = c("Small Nearby", "Medium Nearby", "Large Nearby")),
    log_spillover_units = log(total_public_housing_units_built_nearby)
  )

tract_data_matched_1_year <-
  tract_data_matched_1_year %>%
  select(-total_public_housing_units_built_nearby) %>%
  left_join(inner_spillover_characteristics,
            by = "match_group") %>%
  mutate(
    total_public_housing_units_built_nearby = ifelse(location_type == "treated", NA, total_public_housing_units_built_nearby),
    spillover_size_group = ifelse(location_type == "treated", NA, spillover_size_group),
    log_spillover_units = ifelse(location_type == "treated", NA, log_spillover_units)
  )

cat("\n=== SPILLOVER SIZE DATA MERGED ===\n")
spillover_check <- tract_data_matched_1_year %>%
  filter(group_type == "inner") %>%
  group_by(location_type) %>%
  summarise(
    n_obs = n(),
    n_with_spillover_data = sum(!is.na(total_public_housing_units_built_nearby)),
    n_with_size_groups = sum(!is.na(spillover_size_group)),
    .groups = 'drop'
  )
print(spillover_check)

# Create balance tables ----
create_balance_tables(tract_data_matched_1_year, tract_data_matched_2_year, balance_table_dir)

# Save datasets -----
cat("\n=== SAVING DATASETS ===\n")

write_csv(tract_data_matched_1_year,
          here(output_data_dir, "tract_data_matched_1_year.csv"))

write_csv(tract_data_matched_2_year,
          here(output_data_dir, "tract_data_matched_2_year.csv"))

cat("Datasets saved to:\n",
    here(output_data_dir, "tract_data_matched_1_year.csv"), "\n",
    here(output_data_dir, "tract_data_matched_2_year.csv"), "\n")

# Analyze matching dropouts -----
cat("\n=== MATCHING DROPOUT ANALYSIS ===\n")

all_treated_tracts <- census_tract_data %>%
  filter(location_type == "treated") %>%
  distinct(GISJOIN_1950, treatment_year, county_id, redlined_binary_80pp, ur_binary_5pp, cbsa_title)

matched_treated_tracts <- tract_data_matched_1_year %>%
  filter(location_type == "treated") %>%
  distinct(GISJOIN_1950, matched_treatment_year, county_id, redlined_binary_80pp, ur_binary_5pp, cbsa_title) %>%
  rename(treatment_year = matched_treatment_year)

unmatched_tracts <- all_treated_tracts %>%
  anti_join(matched_treated_tracts, by = c("GISJOIN_1950", "treatment_year"))

cat("Total treated tracts:", nrow(all_treated_tracts), "\n")
cat("Successfully matched treated tracts:", nrow(matched_treated_tracts), "\n")
cat("Unmatched treated tracts:", nrow(unmatched_tracts), "\n\n")

if (nrow(unmatched_tracts) > 0) {
  cat("REASONS FOR MATCHING FAILURE:\n")

  cat("\n1. County distribution of unmatched tracts:\n")
  county_summary <- unmatched_tracts %>%
    count(county_id, cbsa_title, sort = TRUE)
  print(county_summary)

  cat("\n2. Redlining status of unmatched tracts:\n")
  redlining_summary <- unmatched_tracts %>%
    count(redlined_binary_80pp, sort = TRUE)
  print(redlining_summary)

  cat("\n3. Urban renewal status of unmatched tracts:\n")
  ur_summary <- unmatched_tracts %>%
    count(ur_binary_5pp, sort = TRUE)
  print(ur_summary)

  cat("\n4. Combined exact matching combinations for unmatched tracts:\n")
  exact_combo_summary <- unmatched_tracts %>%
    count(county_id, redlined_binary_80pp, ur_binary_5pp, cbsa_title, sort = TRUE)
  print(head(exact_combo_summary, 10))

  write_csv(unmatched_tracts, here(output_data_dir, "unmatched_treated_tracts.csv"))
  cat("\nUnmatched tracts saved to:", here(output_data_dir, "unmatched_treated_tracts.csv"), "\n")
}

cat("\n=== DROPOUT ANALYSIS COMPLETE ===\n")

# Create time series plots ----
variables_to_plot <- list(
  list(var = "black_share", label = "Black Share", percent = TRUE),
  list(var = "black_pop", label = "Black pop", percent = FALSE),
  list(var = "total_pop", label = "Total Population", percent = FALSE),
  list(var = "median_income", label = "Median Income (1950$)", percent = FALSE)
)

cat("\n=== CREATING TIME SERIES FOR 1-YEAR MATCHED SAMPLE ===\n")
create_multiple_time_series(
  data = tract_data_matched_1_year,
  variables_to_plot = variables_to_plot,
  figures_dir = here("output", "time_series", data_type, variant, "1yr")
)

cat("\n=== CREATING TIME SERIES FOR 2-YEAR MATCHED SAMPLE ===\n")
create_multiple_time_series(
  data = tract_data_matched_2_year,
  variables_to_plot = variables_to_plot,
  figures_dir = here("output", "time_series", data_type, variant, "2yr")
)

cat("\n=== CEM MATCHING COMPLETE ===\n")
