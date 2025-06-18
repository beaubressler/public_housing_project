####
# 
# Here, I run various forms of matching and inverse propensity score weighting

###

library(tidyverse)
library(MatchIt)
library(Matching)
library(sf)
library(lmtest)
library(fixest)
library(here)
library(RColorBrewer)

# libraries for covariate testing
library(cobalt)
library(tableone)
library(kableExtra)
#library for Sant'Anna and Zhou (2020)
library(DRDID)
# library for Callaway and Sant'Anna (2021)
library(did)

library(splines)


rm(list=ls())

# 0. Set seed and parameters -----

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small", or "combined"
data_type <- "combined"

set.seed(123456L)
# set number of nearest neighbors for KNN matching
knn_neighbors <- 1

# Notes: With CDD large, KNN = 2 seems to work well
# with the smaller dataset, KNN = 3 work

# define directories
merged_data_dir <- here("data", "derived", "merged", data_type)
map_output_dir <- here("output", "figures", "matched_did", data_type)
balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)
holc_data_dir <- here("data", "derived", "holc")

map_dir <- here("output", "figures", "matched_did", data_type, "with_rings")
results_dir <- here("output", "regression_results", "matched_did", data_type)
balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)

# directory to 
output_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset")

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
# These are the tracts that are treated, the ring of tracts touching those, and the ring of tracts
# touching the inner ring tracts
tracts_and_rings <-
  read_csv(event_study_rings_filepath) %>% 
  #try excluding outer
  filter(location_type != "outer")

# 1. Data Preparation ------

# get treated tracts, unique, and merge onto census tract sample
treated_tracts_panel <-
  treated_tracts_panel_raw %>% 
  distinct(STATE, COUNTY, TRACTA, total_public_housing_units)

census_tract_data <-
  #left_join(census_tract_sample_raw, treated_tracts_panel) %>% 
  census_tract_sample_raw %>% 
  left_join(treated_tracts_panel) %>%
  # merge on rings
  left_join(tracts_and_rings) %>% 
  # set location_type = "donor_pool" if it's NA
  mutate(location_type = if_else(is.na(location_type), "donor_pool", location_type)) %>%
  # create unique id for each tract
  mutate(tract_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
  # # select relevant columns
  mutate(asinh_pop_total = asinh(total_pop),
         asinh_pop_white = asinh(white_pop),
         asinh_pop_black = asinh(black_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent_calculated = asinh(median_rent_calculated),
         asinh_median_home_value_calculated = asinh(median_home_value_calculated),
         asinh_distance_from_cbd = asinh(distance_from_cbd),
         ln_population_density = log(population_density)) %>% 
  dplyr::rename(year = YEAR) %>% 
  st_drop_geometry() %>% 
  # Ensure data is sorted by tract_id and year
  arrange(tract_id, year)  %>% 
  # for HOLC variables (grade and category) if category is missisng ,set to "missing"
  mutate(category_most_overlap = ifelse(is.na(category_most_overlap), "missing", category_most_overlap),
         grade_most_overlap = ifelse(is.na(grade_most_overlap), "missing", grade_most_overlap))

# check category by city
# census_tract_data %>% 
#   group_by(city) %>% 
#   count(category_most_overlap) %>% 
#   # calculate share of missing by city
#   summarise(share_missing = n / sum(n)) %>%
#   View()

# check if education share is missing
# census_tract_data %>%
#   filter(year != 1930, is.na(pct_some_college)) %>% 
#   dplyr::select(tract_id, year, city, COUNTY, pct_some_college) %>% 
#   View()

# Set up matching algorithm ------
# Because of my data structure, it makes sense to do the propensity score matching 
# separately for each treatment year, then combine
# Define group types and treatment years and matching variables


group_types <- c("treated", "inner")

# baseline
# matching_vars <- c("population_density", "distance_from_cbd",
#                    "asinh_pop_black", "asinh_pop_total",
#                    "black_share", "high_skill_share")

# new matching vars
matching_vars <- c("population_density", "asinh_distance_from_cbd", 
                    "asinh_pop_black", "asinh_pop_total",
                    "black_share", "unemp_rate", "pct_hs_grad",
                    "asinh_median_home_value_calculated")


# function for plotting density plots
plot_matching_density_plots <- function(m.out, vars, type = "density") {
  var_formula <- as.formula(paste("~", paste(vars, collapse = " + ")))
  plot(m.out, type = type, interactive = FALSE, which.xs = var_formula)
}



# Function to perform matching
perform_matching_for_year <- function(data, treatment_year, match_vars, nearest_neighbors, group_type,
                                      match_type = "nearest", match_link = "probit", # logit or probit
                                      pre_treatment_decades = 2, caliper = FALSE,
                                      caliper_cutoff = 0.2, exact_match_vars = c("cbsa_title"),
                                      with_replacement = FALSE) {
  
  # testing
  # data <- census_tract_data
  # treatment_year <- 1970
  # match_vars <- matching_vars
  # nearest_neighbors <- 1
  # group_type <- "inner"
  # match_type <- "nearest"
  # caliper = FALSE
  # pre_treatment_decades <- 1
  # exact_match_vars = c("cbsa_title", "redlined_binary_80pp")

  # Determine how many years of pretreatment data i match on
  if (pre_treatment_decades == 1) {
    pre_treatment_years <- c(treatment_year - 10)
  } else if (pre_treatment_decades == 2) {
    pre_treatment_years <- c(treatment_year - 10, treatment_year - 20)
  }
  
  # Separate time-invariant variables
  time_invariant_vars <- c("asinh_distance_from_cbd")
  time_varying_vars <- setdiff(match_vars, time_invariant_vars)
  
  
  # Filter data based on group_type and include appropriate pre-treatment years
  matching_data <- data %>%
    filter(
      (location_type == group_type & treatment_year == !!treatment_year) | # group of interest, treated in the treatment year, all years of data
        (location_type == "donor_pool" & year %in% pre_treatment_years))   # control group, data only from pre-treatment years
  
  # Ensure category is character type across all years
  matching_data <- matching_data %>%
    mutate(category_most_overlap = as.character(category_most_overlap))
  
  # Reshape the data to wide format with corrected variable names
  matching_data_wide <- matching_data %>%
    dplyr::select(STATE, COUNTY, TRACTA, city, cbd, cbsa_title, redlined_binary_80pp,
                  category_most_overlap, location_type, 
           year, all_of(c(time_varying_vars, time_invariant_vars))) %>%
    pivot_wider(
      names_from = year,
      values_from = all_of(time_varying_vars),
      names_glue = "year_{year}_{.value}"
    )
  
  # Determine relevant variables based on pre-treatment years
  relevant_vars <- c(
    paste0("year_", rep(pre_treatment_years, each = length(time_varying_vars)),
           "_", rep(time_varying_vars, times = length(pre_treatment_years))),
    time_invariant_vars
  )
  
  # Filter for complete cases only for the specific years we're working with
  complete_cases <- matching_data_wide %>%
    dplyr::select(all_of(relevant_vars)) %>%
    complete.cases()
  
  matching_data_wide <- matching_data_wide %>%
    filter(complete_cases)
  
  category_vars <- relevant_vars[grep("category", relevant_vars)]
  non_category_vars <- setdiff(relevant_vars, category_vars)
  
  matching_data_wide <- matching_data_wide %>%
    mutate(across(all_of(category_vars)))

  # Create a binary "treatment" indicator for matching
  matching_data_wide <- matching_data_wide %>%
    mutate(treatment_group = as.factor(ifelse(location_type == group_type, 1, 0)))
  

  formula_str <- paste(paste("treatment_group ~", paste(relevant_vars, collapse = " + ")))
  match_formula <- as.formula(formula_str)
  ## Try full matching 
  # m.out4 <- matchit(match_formula, data = matching_data_wide,
  #                   exact = c("city", "cbd", "category_most_overlap"),
  #                   method = "full",
  #                   distance = "glm", link = "probit"
  # )
  # 
  # summary(m.out4)
  
  if (match_type == "nearest") {
    
    if (caliper == TRUE) {
    # Perform matching
        m.out <- matchit(match_formula,
                   data = matching_data_wide,
                   #exact = c("city", "cbd", "category_most_overlap"),
                   exact = exact_match_vars,
                   method = "nearest",
                   distance = "glm",
                   ratio = nearest_neighbors,
                   link = match_link,
                   caliper = caliper_cutoff,
                   std.caliper = TRUE,
                   replace = with_replacement)
  
    } else {
      m.out <- matchit(match_formula,
                   data = matching_data_wide,
                   #exact = c("city", "cbd", "category_most_overlap"),
                   exact = exact_match_vars,
                   method = "nearest",
                   distance = "glm",
                   ratio = nearest_neighbors,
                   link = match_link,
                   replace = with_replacement)
    }
  
  
  } else if (match_type == "full") {
         m.out <- matchit(match_formula,
                     data = matching_data_wide,
                     exact = exact_match_vars,
                     method = "full",
                     distance = "glm",
                     link = match_link,
                     replace = with_replacement)
  } else if (match_type == "genetic") {
    m.out <- matchit(match_formula,
                     data = matching_data_wide,
                     exact = exact_match_vars,
                     method = "genetic",
                     distance = "glm",
                     link = match_link,
                     replace = with_replacement)
  }
  
    
  
  # Get matched data
  matched_data <- match.data(m.out)
  
  # Add treatment year and group type information
  matched_data$matched_treatment_year <- treatment_year
  matched_data$group_type <- group_type
  matched_data$match_type <- match_type
  
  
  year_vars <- grep("^(normalized_)?year_", names(matched_data), value = TRUE)
  
  # Reshape back to long format
  matched_data_long <- matched_data %>%
    mutate(across(contains("category"), as.character)) %>%
    pivot_longer(
      cols = all_of(year_vars),
      names_to = c("year", ".value"),
      names_pattern = "year_([0-9]+)_(.+)"
    ) %>%
    mutate(year = as.integer(year))
  
  return(list(matched_data = matched_data_long, m.out = m.out))
}


## Run matching -----
treatment_years <- unique(census_tract_data %>%
                            filter(!is.na(treatment_year)) %>%
                            pull(treatment_year))

# Initialize lists to store results
matched_datasets <- list()
m_out_objects <- list()

# Perform matching for each group and year
for (group in group_types) {
  for (year in treatment_years) {
    for (nyear in c(1, 2)) {
      df_name <- paste0("matched_data_", group, "_", nyear, "pretreatment_", year)
      m_out_name <- paste0("m_out_", group, "_", nyear, "pretreatment_", year)
      
      # display which group and year we are working on
      print(paste0("Matching for group: ", group, " and year: ", year))
      
      # baseline 
      result <- perform_matching_for_year(census_tract_data, year, matching_vars,
                                          nearest_neighbors = knn_neighbors, group_type = group,
                                          pre_treatment_decades = nyear, match_type = "nearest", match_link = "logit",
                                          exact_match_vars = c("cbsa_title", "redlined_binary_80pp"),
                                          caliper = FALSE)
                                       #   caliper = TRUE, caliper_cutoff = 0.5)
      
      matched_datasets[[df_name]] <- result[["matched_data"]]
      m_out_objects[[m_out_name]] <- result[["m.out"]]
    }
  }
}

## full matches
# summary(m_out_objects$m_out_treated_1pretreatment_1960) # .17 distance, . 144 black
# summary(m_out_objects$m_out_treated_2pretreatment_1960) # .174 distance, .14 black
# summary(m_out_objects$m_out_treated_1pretreatment_1970) # .2187 distance, .348 black pop
# summary(m_out_objects$m_out_treated_2pretreatment_1970) # .1964 distance, .37 black pop
# 
# ## Full matching, removing low skill share and white pop
# summary(m_out_objects$m_out_treated_1pretreatment_1960) # .08 distance, . .16 black
# summary(m_out_objects$m_out_treated_2pretreatment_1960) # .09 distance, .16 black
# summary(m_out_objects$m_out_treated_1pretreatment_1970) #  .193 distance, .33 black pop
# summary(m_out_objects$m_out_treated_2pretreatment_1970) # . 149 distance, .32 black pop
# 
# # density plot with 
# plot(m_out_objects$m_out_treated_1pretreatment_1970, type = "density", interactive = FALSE,
#      which.xs = ~year_1960_asinh_pop_black + year_1960_asinh_pop_total + year_1960_black_share)
# 
# # KNN matching, 1 neighbor
# summary(m_out_objects$m_out_treated_1pretreatment_1960) #  .06 distance, .086 black
# summary(m_out_objects$m_out_treated_2pretreatment_1960) #  .08 distance,  -.0238 black
# summary(m_out_objects$m_out_treated_1pretreatment_1970) #  .15 distance, .214 black pop
# summary(m_out_objects$m_out_treated_2pretreatment_1970) #  .14 distance, .23 black pop
# summary(m_out_objects$m_out_treated_1pretreatment_1980) #  .03 distance, .02 black pop
# summary(m_out_objects$m_out_treated_2pretreatment_1980) #  .007 distance, .23 black pop
# 
# # KNN matching, 1 neighbor, logit
# summary(m_out_objects$m_out_treated_1pretreatment_1960) #  .06 distance, .086 black
# summary(m_out_objects$m_out_treated_2pretreatment_1960) #  .08 distance,  -.0238 black
# summary(m_out_objects$m_out_treated_1pretreatment_1970) #  .15 distance, .214 black pop
# summary(m_out_objects$m_out_treated_2pretreatment_1970) #  .14 distance, .23 black pop
# summary(m_out_objects$m_out_treated_1pretreatment_1980) #  .03 distance, .02 black pop
# summary(m_out_objects$m_out_treated_2pretreatment_1980) #  .007 distance, .23 black pop
# 
# # KNN matching, 2 neighbors
# summary(m_out_objects$m_out_treated_1pretreatment_1960) #  .11 distance, .12 black
# summary(m_out_objects$m_out_treated_2pretreatment_1960) #  .13 distance,  .11 black
# summary(m_out_objects$m_out_treated_1pretreatment_1970) #  .19 distance, .27 black pop
# summary(m_out_objects$m_out_treated_2pretreatment_1970) #  .17 distance, .25 black pop
# summary(m_out_objects$m_out_treated_1pretreatment_1980) #  .02 distance, .07 black pop
# summary(m_out_objects$m_out_treated_2pretreatment_1980) #  .02 distance, .19 black pop
# 
# # KNN matching, 1 neighbor, caliper = 0.1
# summary(m_out_objects$m_out_treated_1pretreatment_1960) #  .002 distance, .05 black
# summary(m_out_objects$m_out_treated_2pretreatment_1960) #  .0016 distance,  -.03 black
# summary(m_out_objects$m_out_treated_1pretreatment_1970) #  .0017 distance, .068 black pop
# summary(m_out_objects$m_out_treated_2pretreatment_1970) #  .003 distance, .12 black pop
# 
# plot(m_out_objects$m_out_treated_1pretreatment_1970, type = "density", interactive = FALSE,
#      which.xs = ~year_1960_asinh_pop_black + year_1960_asinh_pop_total + year_1960_black_share)
# plot(m_out_objects$m_out_treated_1pretreatment_1980, type = "density", interactive = FALSE,
#      which.xs = ~year_1970_asinh_pop_black + year_1970_asinh_pop_total + year_1970_black_share)
# plot(m_out_objects$m_out_treated_1pretreatment_1960, type = "density", interactive = FALSE,
#      which.xs = ~year_1950_asinh_pop_black + year_1950_asinh_pop_total + year_1950_black_share)
# 
# plot(m_out_objects$m_out_inner_1pretreatment_1970, type = "density", interactive = FALSE,
#      which.xs = ~year_1960_asinh_pop_black + year_1960_asinh_pop_total + year_1960_black_share)
# 
# # inner ring matching, 1 neighbor



# Create matched datasets -----

## Matched dataset: Treated and inner rings ----

## matched data, 1 year of pretreatment 
# Combine all matched datasets
all_matched_data_1_year <- bind_rows(matched_datasets$matched_data_treated_1pretreatment_1960,
                              matched_datasets$matched_data_treated_1pretreatment_1970,
                              matched_datasets$matched_data_treated_1pretreatment_1980,
                              matched_datasets$matched_data_inner_1pretreatment_1960,
                              matched_datasets$matched_data_inner_1pretreatment_1970,
                              matched_datasets$matched_data_inner_1pretreatment_1980)

all_matched_data_2_year <- 
  bind_rows(matched_datasets$matched_data_treated_2pretreatment_1960,
            matched_datasets$matched_data_treated_2pretreatment_1970,
            matched_datasets$matched_data_treated_2pretreatment_1980,
            matched_datasets$matched_data_inner_2pretreatment_1960,
            matched_datasets$matched_data_inner_2pretreatment_1970,
            matched_datasets$matched_data_inner_2pretreatment_1980)

# Merge matching information back to the original dataset


# 1 years of matching
tract_data_matched_1_year <- census_tract_data %>%
  # note: there can be multiple matches, because a control tract can be a control for multiple tracts
  left_join(all_matched_data_1_year %>% 
              dplyr::select(STATE, COUNTY, TRACTA, weights, matched_treatment_year, subclass, group_type) %>% 
              distinct(),
            by = c("STATE", "COUNTY", "TRACTA")) %>% 
  # Replace NA weights with 0 (for unmatched observations)
  mutate(weights = ifelse(is.na(weights), 0, weights)) %>% 
  #  create a match_group, which is a unique identifer for each match
  mutate(
    match_group = ifelse(!is.na(matched_treatment_year),
                         paste(city, matched_treatment_year, subclass, sep = "_"),
                         NA)
  ) %>% 
  # drop unmatched observations
  filter(!is.na(match_group))


# 2 years of matching
tract_data_matched_2_year <- census_tract_data %>%
  # note: there can be multiple matches, because a control tract can be a control for multiple tracts
  left_join(all_matched_data_2_year %>% 
              dplyr::select(STATE, COUNTY, TRACTA, weights, matched_treatment_year, subclass, group_type) %>% 
              distinct(),
            by = c("STATE", "COUNTY", "TRACTA")) %>% 
  # Replace NA weights with 0 (for unmatched observations)
  mutate(weights = ifelse(is.na(weights), 0, weights)) %>% 
  #  create a match_group, which is a unique identifer for each match
  mutate(
    match_group = ifelse(!is.na(matched_treatment_year),
                         paste(city, matched_treatment_year, subclass, sep = "_"),
                         NA)
  ) %>% 
  # drop unmatched observations
  filter(!is.na(match_group))


# 
# check balance of covariates ----


## 1. Compare means of full dataset ----
# Assuming 'matched_data' is your dataset with both treated and matched control units
# dataset of just matched units at pre-treatment period
balanced_data_1_year <- tract_data_matched_1_year %>%
  filter(!is.na(match_group)) %>%
  group_by(match_group) %>%
  mutate(group_treatment_year = min(matched_treatment_year, na.rm = TRUE)) %>%
  filter(year == group_treatment_year - 10) %>%
  ungroup()

balanced_data_2_year <-
  tract_data_matched_2_year %>%
  filter(!is.na(match_group)) %>%
  group_by(match_group) %>%
  mutate(group_treatment_year = min(matched_treatment_year, na.rm = TRUE)) %>%
  filter(year == group_treatment_year - 10) %>%
  ungroup()

# List of covariates to check balance
covariates <- c("black_share", "white_share",  "asinh_median_income", "median_income",
                "median_home_value_calculated", "asinh_median_home_value_calculated", "median_rent_calculated","asinh_median_rent_calculated", 
                "distance_from_cbd", "population_density", "housing_density", "total_pop", "black_pop",
                "white_pop", "pct_hs_grad", "pct_some_college",
                "asinh_pop_black", "asinh_pop_white", "asinh_pop_total",
                "total_units", "vacancy_rate", "high_skill_share", "low_skill_share")  

# Create tables for each group_type
datasets <- list(balanced_data_1_year = balanced_data_1_year, balanced_data_2_year = balanced_data_2_year)


for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]  # Access each dataset by name
  
  for (group in group_types ) {
    cat(paste("Processing group:", group, "in dataset", dataset_name, "\n"))
    
    balance_table <- CreateTableOne(vars = covariates,
                                    strata = "location_type",
                                    data = dataset %>% filter(group_type == group),
                                    test = TRUE)
    
    # Print the table
    print(balance_table, smd = TRUE)
  }
}

## 2. Use m.out objects in each year  -----
summary(m_out_objects$m_out_treated_1pretreatment_1960)
summary(m_out_objects$m_out_treated_1pretreatment_1970)
summary(m_out_objects$m_out_treated_1pretreatment_1980)

summary(m_out_objects$m_out_inner_1pretreatment_1960)
summary(m_out_objects$m_out_inner_1pretreatment_1970)
summary(m_out_objects$m_out_inner_1pretreatment_1980)

# plot(m_out_objects$m_out_treated_1pretreatment_1970, type = "density")

# Loop over the covariates and plot density plots
covariate_balance_plots <- list()

for (covariate in covariates) {
  covariate <- "black_share"
  data <-  balanced_data_1_year %>% filter(group_type == "inner",
                      !is.infinite(!!sym(covariate)))
  
  p <- ggplot(data,
              aes(x = !!sym(covariate), fill = location_type)) +
    geom_density(alpha = 0.5) +  # Add transparency to see both densities
    labs(title = paste("Density plot of", covariate),
         x = covariate,
         y = "Density",) +
    theme_minimal() +
    scale_fill_manual(values = c("treated" = "blue", inner = "green", "donor_pool" = "red"))  # Choose colors as needed
  
  # Print each plot
  print(p)
  
  # add plot to list
  covariate_balance_plots[[covariate]] <- p
}





# Output datasets -----
write_csv(tract_data_matched_1_year, here(output_data_dir, "tract_data_matched_1_year.csv"))
write_csv(tract_data_matched_2_year, here(output_data_dir, "tract_data_matched_2_year.csv"))
          

# Try genetic matching  ------
#TODO: Move this elsewhere
# matched_datasets_genetic <- list()
# m_out_objects_genetic <- list()
# 
# # Perform matching for each group and year
# for (group in group_types) {
#   for (year in treatment_years) {
#     for (nyear in c(1, 2)) {
#       df_name <- paste0("matched_data_", group, "_", nyear, "pretreatment_", year)
#       m_out_name <- paste0("m_out_", group, "_", nyear, "pretreatment_", year)
#       
#       # display which group and year we are working on
#       print(paste0("Matching for group: ", group, " and year: ", year))
#       
#       result <- perform_matching_for_year(census_tract_data, year, matching_vars,
#                                           nearest_neighbors = knn_neighbors, group_type = group,
#                                           pre_treatment_decades = nyear, 
#                                           match_type = "genetic", match_link = "logit",
#                                           exact_match_vars = c("city"))
#       
#       matched_datasets_genetic[[df_name]] <- result[["matched_data"]]
#       m_out_objects_genetic[[m_out_name]] <- result[["m.out"]]
#     }
#   }
# }
# 
# ## genetic matches
# summary(m_out_objects_genetic$m_out_treated_1pretreatment_1960) # .17 distance, . 144 black
# summary(m_out_objects_genetic$m_out_treated_2pretreatment_1960) # .174 distance, .14 black
# summary(m_out_objects_genetic$m_out_treated_1pretreatment_1970) # .2187 distance, .348 black pop
# summary(m_out_objects_genetic$m_out_treated_2pretreatment_1970) # .1964 distance, .37 black pop
# summary(m_out_objects_genetic$m_out_treated_1pretreatment_1980) # .1964 distance, .37 black pop
# summary(m_out_objects_genetic$m_out_treated_2pretreatment_1980) # .1964 distance, .37 black pop
# 
# 
# summary(m_out_objects_genetic$m_out_inner_1pretreatment_1960) # .17 distance, . 144 black)
# summary(m_out_objects_genetic$m_out_inner_2pretreatment_1960) # .174 distance, .14 black
# summary(m_out_objects_genetic$m_out_inner_1pretreatment_1970) # .2187 distance, .348 black pop
# summary(m_out_objects_genetic$m_out_inner_2pretreatment_1970) # .1964 distance, .37 black pop
# summary(m_out_objects_genetic$m_out_inner_1pretreatment_1980) # .1964 distance, .37 black pop
# summary(m_out_objects_genetic$m_out_inner_2pretreatment_1980) # .1964 distance, .37 black pop
# 
# ## Create matched datasets: Genetic -----
# all_matched_data_1_year_genetic <- bind_rows(matched_datasets_genetic$matched_data_treated_1pretreatment_1960,
#                                              matched_datasets_genetic$matched_data_treated_1pretreatment_1970,
#                                              matched_datasets_genetic$matched_data_treated_1pretreatment_1980,
#                                              matched_datasets_genetic$matched_data_inner_1pretreatment_1960,
#                                              matched_datasets_genetic$matched_data_inner_1pretreatment_1970,
#                                              matched_datasets_genetic$matched_data_inner_1pretreatment_1980)
# 
# all_matched_data_2_year_genetic <- 
#   bind_rows(matched_datasets_genetic$matched_data_treated_2pretreatment_1960,
#             matched_datasets_genetic$matched_data_treated_2pretreatment_1970,
#             matched_datasets_genetic$matched_data_treated_2pretreatment_1980,
#             matched_datasets_genetic$matched_data_inner_2pretreatment_1960,
#             matched_datasets_genetic$matched_data_inner_2pretreatment_1970,
#             matched_datasets_genetic$matched_data_inner_2pretreatment_1980)
# 
# # Merge matching information back to the original dataset
# 
# tract_data_matched_1_year_genetic <- 
#   census_tract_data %>%
#   left_join(all_matched_data_1_year_genetic %>% 
#               dplyr::select(STATE, COUNTY, TRACTA, weights, matched_treatment_year, subclass, group_type) %>% 
#               distinct(),
#             by = c("STATE", "COUNTY", "TRACTA")) %>% 
#   # Replace NA weights with 0 (for unmatched observations)
#   mutate(weights = ifelse(is.na(weights), 0, weights)) %>% 
#   #  create a match_group, which is a unique identifer for each match
#   mutate(
#     match_group = ifelse(!is.na(matched_treatment_year),
#                          paste(city, matched_treatment_year, subclass, sep = "_"),
#                          NA)
#   ) %>% 
#   # drop unmatched observations
#   filter(!is.na(match_group))
# 
#   # 2 year of matching
# tract_data_matched_2_year_genetic <-
#   census_tract_data %>%
#   left_join(all_matched_data_2_year_genetic %>% 
#               dplyr::select(STATE, COUNTY, TRACTA, weights, matched_treatment_year, subclass, group_type) %>% 
#               distinct(),
#             by = c("STATE", "COUNTY", "TRACTA")) %>% 
#   # Replace NA weights with 0 (for unmatched observations)
#   mutate(weights = ifelse(is.na(weights), 0, weights)) %>% 
#   #  create a match_group, which is a unique identifer for each match
#   mutate(
#     match_group = ifelse(!is.na(matched_treatment_year),
#                          paste(city, matched_treatment_year, subclass, sep = "_"),
#                          NA)
#   ) %>% 
#   # drop unmatched observations
#   filter(!is.na(match_group))
# 
# ### check balance of covariates  -----
# balanced_data_1_year_genetic <- tract_data_matched_1_year_genetic %>%
#   filter(!is.na(match_group)) %>%
#   group_by(match_group) %>%
#   mutate(group_treatment_year = min(matched_treatment_year, na.rm = TRUE)) %>%
#   filter(year == group_treatment_year - 10) %>%
#   ungroup()
# 
# balanced_data_2_year_genetic <-
#   tract_data_matched_2_year_genetic %>%
#   filter(!is.na(match_group)) %>%
#   group_by(match_group) %>%
#   mutate(group_treatment_year = min(matched_treatment_year, na.rm = TRUE)) %>%
#   filter(year == group_treatment_year - 10) %>%
#   ungroup()
# 
# 
# # List of covariates to check balance
# covariates <- c("black_share", "white_share",  "asinh_median_income", "median_income",
#                 "median_home_value_calculated", "asinh_median_home_value_calculated", "median_rent_calculated","asinh_median_rent_calculated", 
#                 "distance_from_cbd", "population_density", "housing_density", "total_pop", "black_pop",
#                 "white_pop", "pct_hs_grad", "pct_some_college",
#                 "asinh_pop_black", "asinh_pop_white", "asinh_pop_total",
#                 "total_units", "vacancy_rate", "high_skill_share", "low_skill_share")  
# 
# # Create tables for each group_type
# datasets <- list(
#   "1_year_genetic" = balanced_data_1_year_genetic,
#   "2_year_genetic" = balanced_data_2_year_genetic
# )
# 
# 
# for (dataset_name in names(datasets)) {
#   dataset <- datasets[[dataset_name]]  # Access each dataset by name
#   
#   for (group in group_types ) {
#     cat(paste("Processing group:", group, "in dataset", dataset_name, "\n"))
#     
#     balance_table <- CreateTableOne(vars = covariates,
#                                     strata = "location_type",
#                                     data = dataset %>% filter(group_type == group),
#                                     test = TRUE)
#     
#     # Print the table
#     print(balance_table, smd = TRUE)
#   }
# }


## output datasets ----
# Save the matched datasets
# write_csv(tract_data_matched_1_year_genetic, here(output_data_dir, "tract_data_matched_1_year_genetic.csv"))
# write_csv(tract_data_matched_2_year_genetic, here(output_data_dir, "tract_data_matched_2_year_genetic.csv"))



 
# ## Create and output balance tables ----
# TODO: move this elsewhere
# 
# # function for outputting balance tables
# create_balance_tables <- function(data, covariates, output_dir) {
#   # Function to create and save balance table
#   create_and_save_table <- function(data, covariates, strata_var, filename) {
#     balance <- CreateTableOne(vars = covariates,
#                               strata = strata_var,
#                               data = data,
#                               test = TRUE)
#     
#     balance_html <- kable(print(balance, printToggle = FALSE, smd = TRUE), format = "html") %>%
#       kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
#     
#     save_kable(balance_html, file = file.path(output_dir, filename))
#   }
#   
#   # Create inner vs donor_pool balance table
#   inner_donor <- data %>% 
#     filter(location_type %in% c("inner", "donor_pool") & group_type == "inner")
#   create_and_save_table(inner_donor, covariates, "location_type", "inner_vs_donor_balance_table.html")
#   
#   # Create treated vs donor_pool balance table
#   treated_donor <- data %>% 
#     filter(location_type %in% c("treated", "donor_pool") & group_type == "treated")
#   create_and_save_table(treated_donor, covariates, "location_type", "treated_vs_donor_balance_table.html")
#   
#   # Create balance tables for each city
#   cities <- unique(data$city)
#   
#   for (city in cities) {
#     city_data <- data %>% filter(city == !!city)
#     
#     # Inner vs donor_pool for the city
#     city_inner_donor <- city_data %>% 
#       filter(location_type %in% c("inner", "donor_pool") & group_type == "inner")
#     create_and_save_table(city_inner_donor, covariates, "location_type", 
#                           paste0(city, "_inner_vs_donor_balance_table.html"))
#     
#     # Treated vs donor_pool for the city
#     city_treated_donor <- city_data %>% 
#       filter(location_type %in% c("treated", "donor_pool") & group_type == "treated")
#     create_and_save_table(city_treated_donor, covariates, "location_type", 
#                           paste0(city, "_treated_vs_donor_balance_table.html"))
#   }
# }
# 
# create_balance_tables(balanced_data, covariates, balance_table_dir)
# 
# 
# 
# # Create balance tables for each city 
# for (city in unique(tract_data_matched_1_year$city)) {
#   balance_table <- CreateTableOne(vars = covariates,
#                                   strata = "location_type",
#                                   data = balanced_data %>% filter(city == city),
#                                   test = TRUE)
#   
#   # Print the table
#   print(balance_table, smd = TRUE)
# }
# 
# 
# 
# 
# 
# 
# 
# # Create maps of treated and control tracts for each city-----
# # get all tracts in our data with their status
# 
# # Switch from 1990 to just all unique tractsx
# all_tracts_with_status <-
#   census_tract_sample_raw %>%
#   filter(YEAR == 1990) %>% 
#   left_join(tract_data_matched_1_year %>% 
#               filter(year == 1990) %>% 
#               dplyr::select(STATE, COUNTY, TRACTA, location_type, weights, matched_treatment_year, group_type),
#             by = c("STATE", "COUNTY", "TRACTA")) %>% 
#   group_by(STATE, COUNTY, TRACTA) %>%
#   # create combined_group_type equal to a combined string of the unique group types for donor_pool tracts
#   # this tells us which groups the donor_pool tract serves as a control for (which can be multiple)
#   # something about this isn't working
#   mutate(
#     combined_group_type = case_when(
#       any(location_type == "donor_pool") ~ paste(sort(unique(group_type[group_type != ""])), collapse = "-"),
#       TRUE ~ NA_character_
#     ),
#     final_type = case_when(
#       is.na(location_type) | (location_type == "donor_pool" & (is.na(combined_group_type) | combined_group_type == "")) ~ "unmatched",
#       location_type != "donor_pool" ~ location_type,
#       TRUE ~ combined_group_type
#     )
#   ) %>%
#   ungroup() %>%
#   distinct(STATE, COUNTY, TRACTA, .keep_all = TRUE) %>% 
#   # if tract is donor pool, set final type = donor_pool + final_type
#   mutate(final_type = case_when(
#     location_type == "donor_pool" ~ paste("donor_pool", final_type, sep = "_"),
#     TRUE ~ final_type
#   ))
# 
# # view
# # View(all_tracts_with_status %>% dplyr::select(STATE, COUNTY, TRACTA, location_type, final_type, combined_group_type))
# 
# create_city_map <- function(data, city_name) {
#   # Filter data for the specific city
#   city_data <- data %>%
#     filter(city == city_name)
#   
#   # Function to mix colors
#   mix_colors <- function(colors) {
#     col_matrix <- col2rgb(colors)
#     mixed_col <- rgb(
#       red = mean(col_matrix["red", ]),
#       green = mean(col_matrix["green", ]),
#       blue = mean(col_matrix["blue", ]),
#       maxColorValue = 255
#     )
#     return(mixed_col)
#   }
#   
#   # Define color palette with more distinct base colors
#   color_palette <- c(
#     "treated" = "darkred",
#     "inner" = "darkgreen",
#     "outer" = "blue",
#     "donor_pool_treated" = "#FF9999",
#     "donor_pool_inner" = "#99FF99",
#     "donor_pool_outer" = "#9999FF",
#     "donor_pool_inner-treated" = "#FFFF00",
#     "donor_pool_outer-treated" = "#FF00FF",
#     "donor_pool_inner-outer" = "#00FFFF",
#     "donor_pool_inner-outer-treated" = "black",
#     "donor_pool_unmatched" = "#DDDDDD"
#   )
#   
#   color_labels <- c(
#     "treated" = "Treated",
#     "inner" = "Inner Ring",
#     "outer" = "Outer Ring",
#     "donor_pool_treated" = "Control: Treated",
#     "donor_pool_inner" = "Control: Inner",
#     "donor_pool_outer" = "Control: Outer",
#     "donor_pool_inner-treated" = "Control: Treated + Inner",
#     "donor_pool_outer-treated" = "Control: Treated + Outer",
#     "donor_pool_inner-outer" = "Control: Inner + Outer",
#     "donor_pool_inner-outer-treated" = "Control: Treated + Inner + Outer",
#     "donor_pool_unmatched" = "Unmatched"
#   )
#   
#   
#   # Create the map
#   ggplot() +
#     geom_sf(data = city_data, aes(fill = final_type), color = "black", size = 0.1) +
#     scale_fill_manual(values = color_palette,
#                       name = "Tract Type",
#                       labels = color_labels,
#                       drop = FALSE) +
#     theme_minimal() +
#     theme(
#       plot.background = element_rect(fill = "white", color = NA),
#       panel.background = element_rect(fill = "white", color = NA),
#       plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
#       legend.position = "bottom",
#       legend.background = element_rect(fill = "white", color = "black"),
#       legend.key = element_rect(color = "black", size = 0.2),
#       axis.text = element_blank(),
#       axis.ticks = element_blank()
#     ) +
#     guides(fill = guide_legend(ncol = 2)) + 
#     ggtitle(paste("Tract types in", city_name))
# }
# 
# # Create maps for each city
# cities <- unique(all_tracts_with_status$city)
# for (city in cities) {
#   map <- create_city_map(all_tracts_with_status, city)
#   
#   ggsave(filename = paste0(map_dir, gsub(" ", "_", city), "_refined_tract_types_map.png"),
#          plot = map, width = 12, height = 10, dpi = 300, bg = "white")
#   
#   print(paste("Map created for", city))
# }
