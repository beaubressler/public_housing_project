###
# Assess matching
###
library(tidyverse)
library(modelsummary)
library(tableone)
library(here)

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small", or "combined"
data_type <- "combined"

set.seed(123456L)

# define directories
merged_data_dir <- here("data", "derived", "merged", data_type)
match_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset")

balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)

# Read in 2 year 
tract_data_matched_2_year <- read_csv(here(match_data_dir, "tract_data_matched_2_year.csv"))

# Produce matching tables -----

## Balance on pre-treatment covariate
pre_treatment_data <-
  tract_data_matched_2_year %>% 
  group_by(match_group) %>%
  mutate(group_treatment_year = min(matched_treatment_year, na.rm = TRUE)) %>%
  filter(year == group_treatment_year - 10) %>%
  ungroup()

covariates <- c("total_pop", "black_pop",
                 "white_pop","black_share", "redlined_binary_80pp", "median_income",
                "median_home_value_calculated", "median_rent_calculated","median_housing_age", 
                "distance_from_cbd", "population_density", 
                "housing_density","total_units",  "pct_hs_grad")  


# Create tables for each group_type, output 
group_types <- c("treated", "inner")
for (group in group_types ) {
  # cat(paste("Processing group:", group, "in dataset", dataset_name, "\n"))
  
  data <- 
    pre_treatment_data %>% 
    filter(group_type == group) %>% 
    dplyr::select(all_of(covariates), location_type) %>% 
    dplyr::rename(`Total Pop` = total_pop,
                  `Black Pop` = black_pop,
                  `White Pop` = white_pop,
                  `Black Share` = black_share,
                  `Redlined` = redlined_binary_80pp,
                  `Median Income` = median_income,
                  `Median Home Value` = median_home_value_calculated,
                  `Median Rent` = median_rent_calculated,
                  # `Median Housing Age` = median_housing_age,
                  `Distance from CBD` = distance_from_cbd,
                  `Population Density` = population_density,
                  `Housing Density` = housing_density,
                  `Total Units` = total_units,
                  `Pct HS Grad` = pct_hs_grad) %>% 
    mutate(location_type = case_when(location_type == "treated" ~ "Treated",
                                    location_type == "inner" ~ "Inner Ring",
                                    TRUE ~ "Donor Pool"))

  datasummary_balance(~location_type,
                      data = data,
                      stars= TRUE,
                      fmt = fmt_decimal(digit = 2, pdigits = 3),
                      dinm_statistic = "p.value",
                      output = here(balance_table_dir, paste0("balance_table_", group, ".tex")))
}


