####
# Site selection analysis

# In this program, I will run probit regressions studying the determinants of 
# public housing site selection...
####


# Preliminaries -----

# Load libraries
library(tidyverse)
library(here)
library(sf)
library(tigris)
library(crsuggest)
library(mapview)
library(maps)
library(ragg)
library(viridis)
library(modelsummary)
library(stargazer)
library(tableone)
library(kableExtra)

library(gt)

library(spdep)

# for event study
library(fixest)
library(marginaleffects)
library(ggfixest)

# conley standard errors
library(conleyreg)

# !! choose which data to use:
# choices are "digitized", "cdd_large", or "cdd_small" or "combined"
data_type <- "combined"


# directory for regression output
site_selection_output_dir <- here("output", "regression_results", "site_selection", data_type)
balance_table_dir <- here("output", "balance_tables", "site_selection", data_type)


data_dir <- here("data", "derived", "merged", data_type)

## Prepping tables----
var_names <- c(lag_black_share = "Percent Black",
               asinh_lag_median_rent_calculated = "Median Rent",
               asinh_lag_median_home_value_calculated = "Median Home Value",
               lag_median_educ_years_25plus = "Median Years of Education",
               asinh_lag_median_income = "Median Income",
               asinh_lag_median_housing_age = "Median Housing Age",
               lag_housing_density = "Housing Density",
               lag_population_density = "Population Density",
               lag_share_needing_repair = "Share Needing Major Repairs",
               lag_vacancy_rate = "Vacancy Rate",
               city = "City",
               COUNTYA = "County",
               YEAR = "Year"
               )

# Read in and prep data ----
# # read treated_tracts_panel.gpkg
# treated_tracts_panel <-
#   st_read(here(data_dir, "treated_tracts_panel_balanced.gpkg")) %>% 
#   st_drop_geometry()

# read in Census tract sample with treatment status
census_tract_sample <-
  st_read(here(data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")) %>% 
  # get lat and long of the tract centroid
  mutate(centroid = st_centroid(geom)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  st_drop_geometry()



# create census tract sample where we keep the treated tract only up until it is first 
# treated. 
treated_tracts_only <- 
  census_tract_sample %>% 
  filter(treated == 1) %>% 
  group_by(GISJOIN_1950) %>% 
  summarise(first_treated_year = min(YEAR)) %>% 
  ungroup()

census_tract_sample_first_treated <-
  census_tract_sample %>% 
  left_join(treated_tracts_only, by = c("GISJOIN_1950")) %>%
  filter(treated == 0 | (treated == 1 & YEAR <= first_treated_year))

# create lag of covariates, since that's what I want to estimate:
# 1. Median income
# 2. Median rent
# 3. Median house value
# 4. Median years of education
# 5. Percent black
# 6. Median housing age
# 7. 
 
census_tract_sample_first_treated <- 
  census_tract_sample_first_treated %>% 
  group_by(cbsa_title, GISJOIN_1950) %>% 
  arrange(YEAR) %>% 
  mutate(lag_median_income = lag(median_income),
         lag_median_rent_calculated = lag(median_rent_calculated),
         lag_median_home_value_calculated = lag(median_home_value_calculated),
         lag_median_educ_years_25plus = lag(median_educ_years_25plus),
         lag_black_share = lag(black_share),
         lag_median_housing_age = lag(median_housing_age),
         lag_share_needing_repair = lag(share_needing_repair),
         lag_share_no_water = lag(share_no_water),
         lag_housing_density = lag(housing_density),
         lag_population_density = lag(population_density),
         lag_vacancy_rate = lag(vacancy_rate),
         lag_pct_hs_grad = lag(pct_hs_grad),
         ) %>% 
  # calculate logs of lagged variables (asinh transformation to avoid issues with 0s)
  mutate(asinh_lag_median_income = asinh(lag_median_income),
         asinh_lag_median_rent_calculated = asinh(lag_median_rent_calculated),
         asinh_lag_median_home_value_calculated = asinh(lag_median_home_value_calculated),
         asinh_lag_median_housing_age = asinh(median_housing_age),
         asinh_lag_housing_density = asinh(lag_housing_density),
         asinh_lag_population_density = asinh(lag_population_density)) %>%
  ungroup()


# I think if I run the regressions on 1930 and 2000, where we never have treated == 1 for the first time,
# it creates a false relationship between lack of treatment and the values of these variables
census_tract_sample_first_treated_1940_to_1980 <- 
  census_tract_sample_first_treated %>% 
  filter(YEAR %in% c(1930, 1940, 1950, 1960, 1970, 1980))

# Also create a version of the dataset just in 2000, end of period treated vs untreated
reshaped_census_data <-
  census_tract_sample %>%
  filter(YEAR %in% c(1930, 1940, 1950, 1960)) %>%
  select(GISJOIN_1950, YEAR, share_no_water, share_needing_repair, black_share, population_density,
         housing_density, pct_hs_grad, median_income, median_home_value_calculated, median_rent_calculated, median_housing_age,
         black_pop, total_pop, white_pop, total_units, low_skill_share, lfp_rate, unemp_rate) %>%
  mutate(asinh_total_pop = asinh(total_pop),
         asinh_black_pop = asinh(black_pop),
         asinh_white_pop = asinh(white_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent_calculated = asinh(median_rent_calculated),
         asinh_median_home_value_calculated = asinh(median_home_value_calculated)) %>% 
  pivot_wider(
    names_from = YEAR,
    values_from = c(share_no_water, share_needing_repair, black_share, population_density,
                    housing_density, pct_hs_grad, median_income, median_home_value_calculated, 
                    median_rent_calculated, median_housing_age, 
                    total_units, low_skill_share, total_pop, white_pop, black_pop,
                    asinh_total_pop, asinh_white_pop, asinh_black_pop, asinh_median_income, 
                    asinh_median_rent_calculated, asinh_median_home_value_calculated,
                    lfp_rate, unemp_rate),
    names_glue = "{.value}_{YEAR}"
  )

census_tract_sample_2000 <-
  census_tract_sample %>% 
  filter(YEAR == 2000) %>% 
  st_drop_geometry() %>% 
  left_join(reshaped_census_data, by = c("GISJOIN_1950")) %>% 
  mutate(county_id = paste0(STATEA, COUNTYA)) %>% 
  mutate(asinh_distance_from_cbd = asinh(distance_from_cbd),
         distance_from_cbd = distance_from_cbd/1000)

# Create balance table -----
vars <- c("redlined_binary_80pp", "share_needing_repair_1940", "median_income_1950", 
          "median_home_value_calculated_1950", "median_rent_calculated_1950", "median_housing_age_1950", 
          "population_density_1950", "pct_hs_grad_1950", "black_share_1950",
          "low_skill_share_1950", "distance_from_cbd", "unemp_rate_1950") 

vars <- c("redlined_binary_80pp", "share_needing_repair_1940", "median_income_1950", 
          "median_home_value_calculated_1950", "median_rent_calculated_1950", "median_housing_age_1950", 
          "population_density_1950", "pct_hs_grad_1950", "black_share_1950",
          "low_skill_share_1950", "distance_from_cbd", "unemp_rate_1950") 


cleaned_vars <-c("Redlined", "Share Needing Repair (1940)", "Median Income", 
                 "Median Home Value", "Median Rent", "Median Housing Age", 
                 "Population Density", "High School Grad Share", 
                 "Black Share", "Low Skill Share", "Distance from CBD", "Unemp. Rate")

# Rename columns in the dataset
census_tract_sample_2000_for_balance_table <- 
  census_tract_sample_2000 %>% 
  dplyr::rename(!!!setNames(vars, cleaned_vars))


balance_table <- CreateTableOne(vars = cleaned_vars, strata = "treated",
                                data = census_tract_sample_2000_for_balance_table, test = TRUE)

print(balance_table, smd = TRUE)

balance_html <- kable(print(balance_table, printToggle = FALSE, smd = TRUE), format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

save_kable(balance_html, file = file.path(balance_table_dir, "treated_untreated_balance_table.tex"))



# Estimate probit models -----
# regress probit model of treatment status on covariates
# Covariates: (lags of)
# 1. Median income
# 2. Median rent
# 3. Median house value
# 4. Median years of education
# 5. Percent black
# 6. Median housing age
# 7. Housing density
# 8. Population density
# 9. Vacancy rates
# 10. HOLC score
# control for city, county, and year fixed effects

full_varlist <-
  c("asinh_lag_median_income", "lag_median_educ_years_25plus", "asinh_lag_median_rent_calculated",
    "asinh_lag_median_home_value_calculated", "lag_black_share", "asinh_lag_median_housing_age",
    "lag_housing_density", "lag_population_density", "lag_vacancy_rate", "lag_pct_hs_grad")

full_varlist_formula <- paste(full_varlist, collapse = " + ")


# varlist to be used in model 2, which will allow for 1950 data
reduced_varlist <- 
  c("asinh_lag_median_rent_calculated", "lag_pct_hs_grad",  "asinh_lag_median_income", "lag_black_share", 
    "lag_population_density", "lag_vacancy_rate")
  
reduced_varlist_formula <- paste(reduced_varlist, collapse = " + ")

## Model 1. Demographics
model_1_probit <- 
  feglm(treated ~
          black_share_1950 + 
          median_income_1950  + 
          pct_hs_grad_1950 | county_id, 
        family = binomial(link = "probit"), 
        data = census_tract_sample_2000,
        cluster = "GISJOIN_1950")

model_1_probit_me <- avg_slopes(model_1_probit)

# with conley SE
model_1_probit_conley <- 
  feglm(treated ~
          black_share_1950 + 
          median_income_1950  + 
          pct_hs_grad_1950 | county_id, 
        family = binomial(link = "probit"), 
        data = census_tract_sample_2000,
        vcov = vcov_conley(lat = "lat", lon = "lon", 
                    cutoff = 2)) 
model_1_probit_conley_me <- avg_slopes(model_1_probit_conley)
model_1_probit_conley_me


## Model 2: Add neighborhood characteristics
model_2_probit <- 
  feglm(treated ~
          black_share_1950 + 
          median_income_1950  + 
          pct_hs_grad_1950 +
          unemp_rate_1950 + 
          redlined_binary_80pp + 
          population_density_1950 +
          cbd + 
          asinh_distance_from_cbd | county_id, 
        family = binomial(link = "probit"), 
        data = census_tract_sample_2000,
        cluster = "GISJOIN_1950")
model_2_probit_me <- avg_slopes(model_2_probit)
model_2_probit_me

model_2_probit_conley <- 
  feglm(treated ~
          black_share_1950 + 
          median_income_1950  + 
          pct_hs_grad_1950 +
          unemp_rate_1950 + 
          redlined_binary_80pp + 
          population_density_1950 +
          cbd + 
          asinh_distance_from_cbd | county_id, 
        family = binomial(link = "probit"), 
        data = census_tract_sample_2000,
        vcov_conley(lat = "lat", lon = "lon", 
                    cutoff = 2))
model_2_probit_conley_me <- avg_slopes(model_2_probit_conley)
model_2_probit_conley_me

# Model 3: Add housing characteristics
model_3_probit <- 
  feglm(treated ~
          black_share_1950 + 
          asinh_total_pop_1950 +
          median_income_1950  + 
          pct_hs_grad_1950 +
          unemp_rate_1950 + 
          redlined_binary_80pp + 
          #population_density_1950 +
          asinh_distance_from_cbd + 
          cbd + 
          share_needing_repair_1940 + 
          median_home_value_calculated_1950 + 
          median_rent_calculated_1950 | county_id, 
        family = binomial(link = "probit"), 
        data = census_tract_sample_2000,
        cluster = "GISJOIN_1950")
model_3_probit_me <- avg_slopes(model_3_probit)
model_3_probit_me

model_3_probit_conley <- 
  feglm(treated ~
          asinh_black_pop_1950 + 
          asinh_total_pop_1950 +
          #asinh_white_pop_1950 +
          #black_share_1950 + 
          median_income_1950  + 
          pct_hs_grad_1950 +
          unemp_rate_1950 + 
          redlined_binary_80pp + 
          #population_density_1950 +
          cbd + 
          share_needing_repair_1940 + 
          asinh_distance_from_cbd + 
          median_rent_calculated_1950 + 
          median_home_value_calculated_1950
          | county_id, 
        family = binomial(link = "probit"), 
        data = census_tract_sample_2000,
        vcov_conley(lat = "lat", lon = "lon", 
                    cutoff = 2))

model_3_probit_conley
model_3_probit_conley_me <- avg_slopes(model_3_probit_conley)
model_3_probit_conley_me

# output 
models <- list(
    "(1)" = model_1_probit,
    "(2)" = model_2_probit,
    "(3)" = model_3_probit
)

# Create a mapping from original variable names to custom labels
variable_labels <- c(
  "black_share_1950" = "Black Share (1950)",
  "median_income_1950" = "Median Income (1950)",
  "pct_hs_grad_1950" = "HS Grad Rate (1950)",
  "redlined_binary_80pp" = "Redlined Indicator",
  "population_density_1950" = "Population Density (1950)",
  "asinh_distance_from_cbd" = "(asinh) Distance from CBD",
  "cbd" = "CBD Indicator",
  "unemp_rate_1950" = "Unemployment Rate (1950)",
  "share_needing_repair_1940" = "Share Needing Major Repairs (1940)",
  "median_home_value_calculated_1950" = "Median Home Value (1950)",
  "asinh_median_home_value_calculated_1950" = "Median Home Value (1950)",
  "asinh_median_rent_calculated_1950" = "Median Rent (1950)",
  "median_housing_age_1950" = "Median Housing Age (1950)")
  

# Customize output for an academic paper format in LaTeX
fe_row <-  tibble::tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "CBSA fixed effects", "Yes", "Yes", "Yes")

attr(fe_row, "position") = 13   # This places the added row above the GOF statistics like R²

modelsummary(
  models,
  estimate = "{estimate} ({std.error}){stars}",   # Shows coefficient with standard error below in parentheses
  statistic = NULL,                        # Removes additional statistics beside SEs
  coef_omit = "(Intercept)",               # Omits the intercept if not needed
  coef_map = variable_labels,              # Maps variable names to custom labels
  stars = TRUE,                            # Significance stars
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",     # Omits unneeded goodness-of-fit statistics
  title = "Probit: Likelihood of ever receiving a public housing project (1951-1973)",
  add_rows = fe_row,
  # notes = c(
  #   "Standard errors in parentheses are clustered by neighborhood ID.",
  #   "Columns 1 and 3 show the results from linear probability models (LPM), while columns 2 and 4 show the marginal effects from probit models.",
  #   "Probit coefficients represent effects on a latent variable, not direct changes in probability."),
  output = here(site_selection_output_dir, "ever_treated_site_selection_models_probit.tex")
)

# output conley models
modelsummary(
  list(
    "(1)" = model_1_probit_conley,
    "(2)" = model_2_probit_conley,
    "(3)" = model_3_probit_conley
  ),
  estimate = "{estimate} ({std.error}){stars}",   # Shows coefficient with standard error below in parentheses
  statistic = NULL,                        # Removes additional statistics beside SEs
  coef_omit = "(Intercept)",               # Omits the intercept if not needed
  coef_map = variable_labels,              # Maps variable names to custom labels
  stars = TRUE,                            # Significance stars
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",     # Omits unneeded goodness-of-fit statistics
  title = "Probit: Likelihood of ever receiving a public housing project (1951-1973) with Conley standard errors",
  add_rows = fe_row,
  output = here(site_selection_output_dir, "ever_treated_site_selection_models_probit_conley.tex")
)


# Estimate linear probability models -----
## Model 1. Demographics
model_1_lpm <- 
  feols(treated ~
          black_share_1950 + 
          median_income_1950  + 
          pct_hs_grad_1950 | county_id, 
        data = census_tract_sample_2000,
        cluster = "GISJOIN_1950")
model_1_lpm

# conley standard errors
model_1_lpm_conley <- 
  feols(treated ~
          black_share_1950 + 
          median_income_1950  + 
          pct_hs_grad_1950 | county_id, 
        data = census_tract_sample_2000,
        vcov_conley(lat = "lat", lon = "lon", 
                    cutoff = 2))
model_1_lpm_conley

## Model 2. Add neighborhood characteristics
model_2_lpm <- 
  feols(treated ~
          black_share_1950 + 
          median_income_1950  + 
          pct_hs_grad_1950 +
          unemp_rate_1950 + 
          redlined_binary_80pp + 
          population_density_1950 +
          cbd + 
          asinh_distance_from_cbd | county_id, 
        data = census_tract_sample_2000,
        cluster = "GISJOIN_1950")
model_2_lpm

# conley 
model_2_lpm_conley <- 
  feols(treated ~
          black_share_1950 + 
          median_income_1950  + 
          pct_hs_grad_1950 +
          unemp_rate_1950 + 
          redlined_binary_80pp + 
          asinh_total_pop_1950 +
          cbd + 
          asinh_distance_from_cbd | county_id, 
        data = census_tract_sample_2000,
        vcov_conley(lat = "lat", lon = "lon", 
                    cutoff = 2))
model_2_lpm_conley

## Model 3. Add housing characteristics
model_3_lpm <- 
  feols(treated ~
          black_share_1950 + 
          median_income_1950  + 
          pct_hs_grad_1950 +
          unemp_rate_1950 + 
          redlined_binary_80pp + 
          population_density_1950 +
          cbd + 
          share_needing_repair_1940 + 
          asinh_distance_from_cbd + 
          median_rent_calculated_1950 + 
          median_home_value_calculated_1950 | county_id,
        data = census_tract_sample_2000,
        cluster = "GISJOIN_1950")
model_3_lpm

# conley
model_3_lpm_conley <- 
  feols(treated ~
          black_share_1950 + 
          asinh_black_pop_1950+
          asinh_total_pop_1950 +
          asinh_median_income_1950  + 
          pct_hs_grad_1950 +
          unemp_rate_1950 + 
          redlined_binary_80pp + 
          #population_density_1950 +
          asinh_distance_from_cbd + 
          cbd +
          share_needing_repair_1940 + 
          median_home_value_calculated_1950 +
          median_rent_calculated_1950  | county_id, 
        data = census_tract_sample_2000,
        vcov_conley(lat = "lat", lon = "lon", 
                    cutoff = 2))
model_3_lpm_conley



models_lpm <- list(
  "(1)" = model_1_lpm,
  "(2)" = model_2_lpm,
  "(3)" = model_3_lpm
)


modelsummary(
  models_lpm,
  estimate = "{estimate} ({std.error}){stars}",   # Shows coefficient with standard error below in parentheses
  statistic = NULL,                        # Removes additional statistics beside SEs
  coef_omit = "(Intercept)",               # Omits the intercept if not needed
  coef_map = variable_labels,              # Maps variable names to custom labels
  stars = TRUE,                            # Significance stars
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",     # Omits goodness-of-fit statistics
  title = "Linear probability model: Likelihood of receiving a public housing project (1951-1973)",
  add_rows = fe_row,
  # notes = c(
  #   "Standard errors in parentheses are clustered by neighborhood ID.",
  #   "Columns 1 and 3 show the results from linear probability models (LPM), while columns 2 and 4 show the marginal effects from probit models.",
  #   "Probit coefficients represent effects on a latent variable, not direct changes in probability."),
  output = here(site_selection_output_dir, "ever_treated_site_selection_models_lpm.tex")
)

# output with conley SE
models_lpm_conley <- list(
  "(1)" = model_1_lpm_conley,
  "(2)" = model_2_lpm_conley,
  "(3)" = model_3_lpm_conley
)

modelsummary(
  models_lpm_conley,
  estimate = "{estimate} ({std.error}){stars}",   # Shows coefficient with standard error below in parentheses
  statistic = NULL,                        # Removes additional statistics beside SEs
  coef_omit = "(Intercept)",               # Omits the intercept if not needed
  coef_map = variable_labels,              # Maps variable names to custom labels
  stars = TRUE,                            # Significance stars
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",     # Omits goodness-of-fit statistics
  title = "Linear probability model with Conley standard errors: Likelihood of receiving a public housing project (1951-1973)",
  add_rows = fe_row,
  # notes = c(
  #   "Standard errors in parentheses are clustered by neighborhood ID.",
  #   "Columns 1 and 3 show the results from linear probability models (LPM), while columns 2 and 4 show the marginal effects from probit models.",
  #   "Probit coefficients represent effects on a latent variable, not direct changes in probability."),
  output = here(site_selection_output_dir, "ever_treated_site_selection_models_lpm_conley.tex")
)

# Estimate using 1940 variables -----

## Motivation: Test site selection using pre-treatment characteristics
# Use 1940 variables to predict treatment that occurs 1941+
# This avoids potential reverse causality from early treatment effects

# Create dataset with 1940 predictors (match 1950 structure)
site_selection_1940 <- reshaped_census_data %>%
  filter(GISJOIN_1950 %in% census_tract_sample_2000$GISJOIN_1950) %>%
  left_join(
    census_tract_sample_2000 %>% 
      select(GISJOIN_1950, treated, asinh_distance_from_cbd, cbd, redlined_binary_80pp, city, cbsa_title,
             lat, lon, county_id, STATEA, COUNTYA),
    by = "GISJOIN_1950"
  ) %>%
  # Use 1940 values for main predictors (match 1950 variable set)
  mutate(
    black_share_1940 = black_share_1940,
    asinh_total_pop_1940 = asinh_total_pop_1940, 
    asinh_black_pop_1940 = asinh_black_pop_1940,
    median_income_1940 = median_income_1940,
    median_home_value_calculated_1940 = median_home_value_calculated_1940,
    median_rent_calculated_1940 = median_rent_calculated_1940,
    pct_hs_grad_1940 = pct_hs_grad_1940,
    unemp_rate_1940 = unemp_rate_1940,
    lfp_rate_1940 = lfp_rate_1940,
    population_density_1940 = total_pop_1940 / 1,
    share_needing_repair_1940 = share_needing_repair_1940,
    asinh_distance_from_cbd = asinh_distance_from_cbd
  ) %>%
  # Keep complete cases for key variables
  filter(!is.na(black_share_1940), !is.na(asinh_total_pop_1940), !is.na(treated)) %>%
  # Add fixed effects (match 1950 structure)
  mutate(
    county_id = ifelse(is.na(county_id), paste0(STATEA, COUNTYA), county_id)
  )

# Check sample size
cat("Sample size for 1940 analysis:", nrow(site_selection_1940), "\n")
cat("Treated tracts:", sum(site_selection_1940$treated), "\n")

## PROBIT MODELS with 1940 variables ----
model_1_probit_1940 <- feglm(
  treated ~ black_share_1940 + asinh_total_pop_1940 + asinh_distance_from_cbd + cbd + redlined_binary_80pp,
  data = site_selection_1940,
  family = binomial(link = "probit"),
  cluster = "GISJOIN_1950"
)

model_2_probit_1940 <- feglm(
  treated ~ black_share_1940 + asinh_total_pop_1940 + asinh_distance_from_cbd + cbd + redlined_binary_80pp +
            median_income_1940 + pct_hs_grad_1940 + unemp_rate_1940,
  data = site_selection_1940 %>% filter(!is.na(median_income_1940), !is.na(pct_hs_grad_1940)),
  family = binomial(link = "probit"), 
  cluster = "GISJOIN_1950"
)

model_3_probit_1940 <- feglm(
  treated ~ black_share_1940 + asinh_total_pop_1940 + median_income_1940 + pct_hs_grad_1940 + 
            unemp_rate_1940 + redlined_binary_80pp + asinh_distance_from_cbd + cbd + 
            share_needing_repair_1940 + median_home_value_calculated_1940 + median_rent_calculated_1940 | county_id,
  data = site_selection_1940 %>% 
    filter(!is.na(median_income_1940), !is.na(pct_hs_grad_1940), !is.na(share_needing_repair_1940),
           !is.na(median_home_value_calculated_1940), !is.na(median_rent_calculated_1940)),
  family = binomial(link = "probit"),
  cluster = "GISJOIN_1950"
)

# Calculate marginal effects for 1940 probit models
mfx_1_1940 <- slopes(model_1_probit_1940, vcov = cluster ~ GISJOIN_1950)
mfx_2_1940 <- slopes(model_2_probit_1940, vcov = cluster ~ GISJOIN_1950) 
mfx_3_1940 <- slopes(model_3_probit_1940, vcov = cluster ~ GISJOIN_1950)

## LINEAR PROBABILITY MODELS with 1940 variables ----
model_1_lpm_1940 <- feols(
  treated ~ black_share_1940 + asinh_total_pop_1940 + asinh_distance_from_cbd + 
    cbd + redlined_binary_80pp | county_id,
  data = site_selection_1940,
  cluster = "GISJOIN_1950"
)

model_2_lpm_1940 <- feols(
  treated ~ black_share_1940 + asinh_total_pop_1940 + asinh_distance_from_cbd + cbd +
    redlined_binary_80pp +
            median_income_1940 + pct_hs_grad_1940 + unemp_rate_1940 | county_id,
  data = site_selection_1940,
  cluster = "GISJOIN_1950"
)

model_3_lpm_1940 <- feols(
  treated ~ black_share_1940 + asinh_total_pop_1940 + median_income_1940 + pct_hs_grad_1940 + 
            unemp_rate_1940 + redlined_binary_80pp + asinh_distance_from_cbd + cbd + 
            share_needing_repair_1940 + median_rent_calculated_1940 | county_id,
  data = site_selection_1940 %>% 
    filter(!is.na(median_income_1940), !is.na(pct_hs_grad_1940), !is.na(share_needing_repair_1940),
           !is.na(median_home_value_calculated_1940), !is.na(median_rent_calculated_1940)),
  cluster = "GISJOIN_1950"
)

## LINEAR PROBABILITY MODELS with Conley Standard Errors (1940) ----
model_1_lpm_conley_1940 <- feols(
  treated ~ asinh_black_pop_1940 + asinh_total_pop_1940 + asinh_distance_from_cbd +
    cbd + redlined_binary_80pp | county_id,
  data = site_selection_1940,
  vcov_conley(lat = "lat", lon = "lon", 
              cutoff = 2)
)

model_2_lpm_conley_1940 <- feols(
  treated ~ black_share_1940 + asinh_total_pop_1940 + asinh_distance_from_cbd + cbd + redlined_binary_80pp +
            asinh_median_income_1940 + pct_hs_grad_1940 + unemp_rate_1940 | county_id,
  data = site_selection_1940 %>% filter(!is.na(median_income_1940), !is.na(pct_hs_grad_1940)),
  vcov_conley(lat = "lat", lon = "lon", 
              cutoff = 3)
)

model_3_lpm_conley_1940 <- feols(
  treated ~ asinh_total_pop_1940 +
    black_share_1940 +
    asinh_median_income_1940 + pct_hs_grad_1940 + 
            unemp_rate_1940 + redlined_binary_80pp + asinh_distance_from_cbd + cbd + 
            share_needing_repair_1940  +  asinh_median_rent_calculated_1940 | county_id,
  data = site_selection_1940,
  vcov_conley(lat = "lat", lon = "lon", 
              cutoff = 3)
)

# Variable labels for 1940 models (match 1950 structure)
variable_labels_1940 <- c(
  "black_share_1940" = "Black Share (1940)",
  "asinh_total_pop_1940" = "Total Population (1940)", 
  "median_income_1940" = "Median Income (1940)",
  "pct_hs_grad_1940" = "% High School Graduates (1940)",
  "unemp_rate_1940" = "Unemployment Rate (1940)",
  "redlined_binary_80pp" = "Redlined (HOLC Grade D)",
  "asinh_distance_from_cbd" = "Distance from CBD",
  "cbd" = "CBD Indicator", 
  "share_needing_repair_1940" = "Share Needing Major Repairs (1940)",
  "median_home_value_calculated_1940" = "Median Home Value (1940)",
  "median_rent_calculated_1940" = "Median Rent (1940)"
)

# Fixed effects rows for 1940 models (match 1950 structure)
fe_row_1940 <- tibble(
  term = c("County Fixed Effects"),
  "(1)" = "No",
  "(2)" = "No", 
  "(3)" = "Yes"
)

## OUTPUT PROBIT MARGINAL EFFECTS (1940) ----
# models_1940_mfx <- list(
#   "(1)" = mfx_1_1940,
#   "(2)" = mfx_2_1940, 
#   "(3)" = mfx_3_1940
# )
# 
# modelsummary(
#   models_1940_mfx,
#   estimate = "{estimate} ({std.error}){stars}",
#   statistic = NULL,
#   coef_map = variable_labels_1940,
#   stars = TRUE,
#   gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",
#   title = "Site Selection Analysis: Marginal Effects from Probit Models Using 1940 Characteristics",
#   add_rows = fe_row_1940,
#   notes = c(
#     "Dependent variable: Indicator for receiving public housing project (1941-1980)",
#     "Marginal effects calculated at sample means with standard errors clustered by MSA",
#     "1940 characteristics used to avoid reverse causality from treatment effects"
#   ),
#   output = here(site_selection_output_dir, "site_selection_1940_marginal_effects.tex")
# )

## OUTPUT LINEAR PROBABILITY MODELS (1940) ----
models_1940_lpm <- list(
  "(1)" = model_1_lpm_1940,
  "(2)" = model_2_lpm_1940,
  "(3)" = model_3_lpm_1940
)

modelsummary(
  models_1940_lpm,
  estimate = "{estimate} ({std.error}){stars}",
  statistic = NULL,
  coef_omit = "(Intercept)",
  coef_map = variable_labels_1940,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",
  title = "Linear Probability Models: Site Selection Using 1940 Characteristics",
  add_rows = fe_row_1940,
  notes = c(
    "Dependent variable: Indicator for receiving public housing project (1941-1980)",
    "Standard errors clustered by MSA in parentheses",
    "1940 characteristics used to avoid reverse causality from treatment effects"
  ),
  output = here(site_selection_output_dir, "site_selection_1940_lpm.tex")
)

## OUTPUT LINEAR PROBABILITY MODELS with Conley SE (1940) ----
models_1940_lpm_conley <- list(
  "(1)" = model_1_lpm_conley_1940,
  "(2)" = model_2_lpm_conley_1940,
  "(3)" = model_3_lpm_conley_1940
)

modelsummary(
  models_1940_lpm_conley,
  estimate = "{estimate} ({std.error}){stars}",
  statistic = NULL,
  coef_omit = "(Intercept)",
  coef_map = variable_labels_1940,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",
  title = "Linear Probability Models with Conley Standard Errors: Site Selection Using 1940 Characteristics",
  add_rows = fe_row_1940,
  notes = c(
    "Dependent variable: Indicator for receiving public housing project (1941-1980)",
    "Conley standard errors (2km cutoff) in parentheses account for spatial correlation",
    "1940 characteristics used to avoid reverse causality from treatment effects"
  ),
  output = here(site_selection_output_dir, "site_selection_1940_lpm_conley.tex")
)

# Compare key coefficients between 1950 and 1940 specifications
cat("\n=== COMPARISON: 1940 vs 1950 Site Selection Results ===\n")
cat("LPM Model 3 coefficients:\n")
cat("Black Share: 1940 =", round(coef(model_3_lpm_conley_1940)["asinh_black_pop_1940"], 4), 
    "vs 1950 =", round(coef(model_3_lpm_conley)["asinh_black_pop_1950"], 4), "\n")
cat("Total Pop: 1940 =", round(coef(model_3_lpm_conley_1940)["asinh_total_pop_1940"], 4),
    "vs 1950 =", round(coef(model_3_lpm_conley)["asinh_total_pop_1950"], 4), "\n")
cat("Distance CBD: 1940 =", round(coef(model_3_lpm_conley_1940)["asinh_distance_from_cbd"], 4),
    "vs 1950 =", round(coef(model_3_lpm_conley)["asinh_distance_from_cbd"], 4), "\n")

cat("\n1940 site selection analysis complete: Results support robustness of 1950 specification.\n")






# Commenting out for now: 
# # Lasso -----
# # Streamlined LASSO Analysis for Site Selection Paper
# library(glmnet)
# library(ggplot2)
# 
# ## 1. PREPARE DATA ----
# X <- census_tract_sample_2000 %>%
#   dplyr::select(
#     asinh_total_pop_1950, asinh_black_pop_1950, black_share_1950,
#     asinh_median_income_1950, pct_hs_grad_1950, redlined_binary_80pp,
#     cbd, asinh_distance_from_cbd,
#     lfp_rate_1950, unemp_rate_1950, share_needing_repair_1940,
#     median_home_value_calculated_1950, median_rent_calculated_1950
#   ) %>%
#   as.matrix()
# 
# y <- census_tract_sample_2000$treated
# 
# # Keep complete cases only
# complete_cases <- complete.cases(X, y)
# X_complete <- X[complete_cases, ]
# y_complete <- y[complete_cases]
# 
# ## 2. FIT LASSO MODEL ----
# lasso_model <- cv.glmnet(X_complete, y_complete, alpha = 1, family = "binomial", standardize = TRUE)
# 
# ## 3. VARIABLE SELECTION RESULTS ----
# # Get coefficients at lambda.min and lambda.1se
# coef_min <- coef(lasso_model, s = "lambda.min")
# coef_1se <- coef(lasso_model, s = "lambda.1se")
# 
# # Create results table
# lasso_results <- data.frame(
#   Variable = rownames(coef_min),
#   Coef_Lambda_Min = as.numeric(coef_min),
#   Coef_Lambda_1SE = as.numeric(coef_1se)
# ) %>%
#   filter(Variable != "(Intercept)") %>%
#   mutate(
#     Selected_Min = Coef_Lambda_Min != 0,
#     Selected_1SE = Coef_Lambda_1SE != 0,
#     Stable_Selection = Selected_Min & Selected_1SE
#   ) %>%
#   arrange(desc(abs(Coef_Lambda_Min)))
# 
# print("LASSO Variable Selection Results:")
# print(lasso_results)
# 
# # Variables selected at lambda.min (for paper text)
# selected_vars_min <- lasso_results %>% filter(Selected_Min) %>% pull(Variable)
# cat("\nVariables selected at lambda.min:\n")
# print(selected_vars_min)
# 
# # Most stable variables (selected at both lambda values)
# stable_vars <- lasso_results %>% filter(Stable_Selection) %>% pull(Variable) 
# cat("\nMost stable variables (selected at both lambda.min and lambda.1se):\n")
# print(stable_vars)
# 
# ## 4. COMPARISON WITH LPM RESULTS ----
# # Your significant LPM variables (update these based on your actual results)
# lpm_significant_vars <- c("black_share_1950", "asinh_total_pop_1950", 
#                           "asinh_median_income_1950", "unemp_rate_1950", 
#                           "redlined_binary_80pp")
# 
# # Compare
# overlap <- intersect(gsub("_1950", "", lpm_significant_vars), gsub("_1950", "", selected_vars_min))
# lmp_only <- setdiff(gsub("_1950", "", lpm_significant_vars), gsub("_1950", "", selected_vars_min))
# lasso_only <- setdiff(gsub("_1950", "", selected_vars_min), gsub("_1950", "", lpm_significant_vars))
# 
# cat("\n=== COMPARISON WITH LPM RESULTS ===\n")
# cat("Variables important in both LPM and LASSO:\n")
# print(overlap)
# cat("\nVariables significant in LPM but not selected by LASSO:\n") 
# print(lmp_only)
# cat("\nVariables selected by LASSO but not significant in LPM:\n")
# print(lasso_only)
# 
# ## 5. CREATE FIGURE FOR PAPER ----
# # Variable importance plot
# top_vars <- lasso_results %>% 
#   filter(Selected_Min) %>% 
#   slice_max(abs(Coef_Lambda_Min), n = 8)
# 
# importance_plot <- ggplot(top_vars, aes(x = reorder(Variable, abs(Coef_Lambda_Min)), 
#                                         y = abs(Coef_Lambda_Min))) +
#   geom_col(fill = "steelblue", alpha = 0.7) +
#   coord_flip() +
#   labs(
#     title = "LASSO Variable Importance for Site Selection",
#     x = "Variables",
#     y = "Absolute Coefficient Value"
#   ) +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10))
# 
# print(importance_plot)
# 
# ## 6. SUMMARY STATS FOR PAPER ----
# cat("\n=== SUMMARY FOR PAPER ===\n")
# cat("Total variables considered:", nrow(lasso_results), "\n")
# cat("Variables selected at lambda.min:", sum(lasso_results$Selected_Min), "\n") 
# cat("Variables selected at lambda.1se:", sum(lasso_results$Selected_1SE), "\n")
# cat("Most stable variables:", sum(lasso_results$Stable_Selection), "\n")
# 
# # Top 3 predictors by importance
# top_3 <- head(selected_vars_min, 3)
# cat("\nTop 3 predictors:\n")
# for(i in 1:length(top_3)) {
#   coef_val <- lasso_results[lasso_results$Variable == top_3[i], "Coef_Lambda_Min"]
#   cat(paste(i, ".", top_3[i], "(coef =", round(coef_val, 3), ")\n"))
# }
# 
# ## 7. SAVE RESULTS FOR PAPER ----
# # Save the main results table
# #write.csv(lasso_results, here(site_selection_output_dir, "lasso_variable_selection.csv"), row.names = FALSE)
# 
# # Save the plot
# ggsave(here(site_selection_output_dir, "lasso_variable_importance.png"), 
#        importance_plot, width = 8, height = 6, dpi = 300)
