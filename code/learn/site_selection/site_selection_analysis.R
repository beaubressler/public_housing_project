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
# read treated_tracts_panel.gpkg
treated_tracts_panel <-
  st_read(here(data_dir, "treated_tracts_panel_balanced.gpkg")) %>% 
  st_drop_geometry()

# read in Census tract sample with treatment status
census_tract_sample <-
  st_read(here(data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")) %>% 
  st_drop_geometry()

# create census tract sample where we keep the treated tract only up until it is first 
# treated. 
treated_tracts_only <- 
  census_tract_sample %>% 
  filter(treated == 1) %>% 
  group_by(STATEA, COUNTYA, TRACTA) %>% 
  summarise(first_treated_year = min(YEAR)) %>% 
  ungroup() %>% 
  st_drop_geometry()

census_tract_sample_first_treated <-
  census_tract_sample %>% 
  left_join(treated_tracts_only, by = c("TRACTA", "COUNTYA", "STATEA")) %>%
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
  group_by(cbsa_title, STATEA, COUNTYA, TRACTA) %>% 
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


# I think if I run the regressions on 1930 and 1990, where we never have treated == 1 for the first time,
# it creates a false relationship between lack of treatment and the values of these variables
census_tract_sample_first_treated_1940_to_1980 <- 
  census_tract_sample_first_treated %>% 
  filter(YEAR %in% c(1930, 1940, 1950, 1960, 1970, 1980))

# Also create a version of the dataset just in 1990, end of period treated vs untreated
reshaped_census_data <-
  census_tract_sample %>%
  filter(YEAR %in% c(1930, 1940, 1950, 1960)) %>%
  select(STATEA, COUNTYA, TRACTA, YEAR, share_no_water, share_needing_repair, black_share, population_density,
         housing_density, pct_hs_grad, median_income, median_home_value_calculated, median_rent_calculated, median_housing_age,
         black_pop, total_pop, total_units, low_skill_share, lfp_rate, unemp_rate) %>%
  mutate(asinh_total_pop = asinh(total_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent_calculated = asinh(median_rent_calculated),
         asinh_median_home_value_calculated = asinh(median_home_value_calculated)) %>% 
  pivot_wider(
    names_from = YEAR,
    values_from = c(share_no_water, share_needing_repair, black_share, population_density,
                    housing_density, pct_hs_grad, median_income, median_home_value_calculated, 
                    median_rent_calculated, median_housing_age, 
                    black_pop, total_pop, total_units, low_skill_share,
                    asinh_total_pop, asinh_median_income, 
                    asinh_median_rent_calculated, asinh_median_home_value_calculated,
                    lfp_rate, unemp_rate),
    names_glue = "{.value}_{YEAR}"
  )

census_tract_sample_1990 <-
  census_tract_sample %>% 
  filter(YEAR == 1990) %>% 
  st_drop_geometry() %>% 
  left_join(reshaped_census_data, by = c("STATEA", "COUNTYA", "TRACTA")) %>% 
  mutate(neighborhood_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
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
census_tract_sample_1990_for_balance_table <- 
  census_tract_sample_1990 %>% 
  dplyr::rename(!!!setNames(vars, cleaned_vars))


balance_table <- CreateTableOne(vars = cleaned_vars, strata = "treated",
                                data = census_tract_sample_1990_for_balance_table, test = TRUE)

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

## Probability of ever being treated -----
redlined_model <- feols(treated ~ redlined_binary_80pp | cbsa_title, census_tract_sample_1990)
share_needing_repair_model <- feols(treated ~ share_needing_repair_1940 | cbsa_title, census_tract_sample_1990)
median_income_model <- feols(treated ~ asinh(median_income_1950) | cbsa_title, census_tract_sample_1990)
median_home_value_calculated_model <- feols(treated ~ asinh(median_home_value_calculated_1950) | cbsa_title, census_tract_sample_1990)
median_rent_calculated_model <- feols(treated ~ asinh(median_rent_calculated_1950) | cbsa_title, census_tract_sample_1990)
median_housing_age_model <- feols(treated ~ median_housing_age_1950 | cbsa_title, census_tract_sample_1990)
pop_density_model  <- feols(treated ~ population_density_1950 | cbsa_title, census_tract_sample_1990)
hs_grad_model <- feols(treated ~ pct_hs_grad_1950 | cbsa_title, census_tract_sample_1990)
low_skill_share_model <- feols(treated ~ low_skill_share_1950 | cbsa_title, census_tract_sample_1990)
black_share_model <- feols(treated ~ black_share_1950 | cbsa_title, census_tract_sample_1990)



## Model 1. Demographics
model_1_probit <- 
  feglm(treated ~
          black_share_1950 + 
          median_income_1950  + 
          pct_hs_grad_1950 | cbsa_title, 
        family = binomial(link = "probit"), 
        data = census_tract_sample_1990,
        cluster = "neighborhood_id")

model_1_probit_me <- avg_slopes(model_1_probit)
summary(model_1_probit_me)


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
          asinh_distance_from_cbd | cbsa_title, 
        family = binomial(link = "probit"), 
        data = census_tract_sample_1990,
        cluster = "neighborhood_id")
model_2_probit_me <- avg_slopes(model_2_probit)
summary(model_2_probit_me)

# Model 3: Add housing characteristics
model_3_probit <- 
  feglm(treated ~
          black_share_1950 + 
          median_income_1950  + 
          pct_hs_grad_1950 +
          unemp_rate_1950 + 
          redlined_binary_80pp + 
          population_density_1950 +
          asinh_distance_from_cbd + 
          cbd + 
          share_needing_repair_1940 + 
          median_home_value_calculated_1950 + 
          median_rent_calculated_1950 + 
          median_housing_age_1950 | cbsa_title, 
        family = binomial(link = "probit"), 
        data = census_tract_sample_1990,
        cluster = "neighborhood_id")
model_3_probit_me <- avg_slopes(model_3_probit)
summary(model_3_probit_me)


# output 
models <- list(
    "(1)" = model_1_probit_me,
    "(2)" = model_2_probit_me,
    "(3)" = model_3_probit_me
)

# Create a mapping from original variable names to custom labels
variable_labels <- c(
  "black_share_1950" = "Black Share (1950)",
  "median_income_1950" = "Median Income (1950)",
  "pct_hs_grad_1950" = "High School Graduation Rate (1950)",
  "redlined_binary_80pp" = "Redlined Indicator",
  "population_density_1950" = "Population Density (1950)",
  "asinh_distance_from_cbd" = "(asinh) Distance from CBD",
  "cbd" = "CBD Indicator",
  "unemp_rate_1950" = "Unemployment Rate (1950)",
  "share_needing_repair_1940" = "Share Needing Major Repairs (1940)",
  "median_home_value_calculated_1950" = "Median Home Value (1950)",
  "median_rent_calculated_1950" = "Median Rent (1950)",
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
  title = "Predicting likelihood of ever receiving a public housing project (1951-1973)",
  add_rows = fe_row,
  # notes = c(
  #   "Standard errors in parentheses are clustered by neighborhood ID.",
  #   "Columns 1 and 3 show the results from linear probability models (LPM), while columns 2 and 4 show the marginal effects from probit models.",
  #   "Probit coefficients represent effects on a latent variable, not direct changes in probability."),
  output = here(site_selection_output_dir, "ever_treated_site_selection_models_20250107.tex")
)


## Try with logs for all variables

# 
# ## Probability of being treated based on previous decades variables ----
# 
# ## Demographics only 
# feols(treated ~  black_share_1970 | cbsa_title, census_tract_sample_first_treated_1940_to_1980)
# 
# 
# 
# # model 1: all data, logs of lagged variables
# model1_formula <- as.formula(paste0("treated ~ ", full_varlist_formula,
#                             "| city + COUNTYA + YEAR"))
# 
# # Run the regression
# model1 <- feols(model1_formula, data = census_tract_sample_first_treated_1940_to_1980, cluster = "TRACTA")
# 
# summary(model1)
# 
# # figure out what years are in the sample that don't have missing data for any of the full_varlist variables
# years_in_sample_model_1 <-
#   census_tract_sample_first_treated_1940_to_1980 %>% 
#   filter(across(all_of(full_varlist), ~ !is.na(.))) %>%
#   pull(YEAR) %>% 
#   unique()
# 
# years_in_sample_model_1
# 
# # model 1_1: Same as model 1, but exclude variables that seem potentially very related
# model1_1_formula <- as.formula(paste0("treated ~ ", full_varlist_alt_formula, 
#                             "| city + COUNTYA + YEAR"))
# # Run the regression
# model1_1 <- feols(model1_1_formula, data = census_tract_sample_first_treated_1940_to_1980, cluster = "TRACTA")
# 
# summary(model1_1)
# 
# # model 2: Exclude variables that are maybe noisy
# model2_formula <- as.formula(paste0("treated ~", reduced_varlist_formula, 
#                             "| city + COUNTYA + YEAR"))
# 
# # Run the regression
# model2 <- feols(model2_formula,
#                 data = census_tract_sample_first_treated_1940_to_1980, 
#                 cluster = "TRACTA")
# 
# summary(model2)
# 
# years_in_sample_model_2 <-
#   census_tract_sample_first_treated_1940_to_1980 %>% 
#   filter(across(all_of(reduced_varlist), ~ !is.na(.))) %>%
#   pull(YEAR) %>% 
#   unique()
# 
# years_in_sample_model_2
# ## model 3: include share of housing needing repairs
# model3_formula <- as.formula(paste0("treated ~ asinh_lag_median_rent_calculated +",
#                                     "lag_black_share + lag_pct_hs_grad + asinh_lag_median_income + ",
#                                     "lag_population_density +  lag_vacancy_rate +
#                                     lag_share_needing_repair", 
#                             "| city + COUNTYA + YEAR"))
# # Run the regression
# model3 <- feols(model3_formula, data = census_tract_sample_first_treated_1940_to_1980, cluster = "TRACTA")
# 
# summary(model3)
# 
# ## model 4: include share of housing needing repairs, only 1950
# # model4 <- feols(model3_formula,
# #                 data = census_tract_sample_first_treated_1940_to_1980 %>% filter(YEAR == 1950),
# #                 cluster = "TRACTA")
# # 
# # summary(model4)
# 
# # Export models into Latex tables ----
# site_selection_models <- 
#   list(model1, model2, model3)
# 
# 
# # export models to latex
# etable(site_selection_models, tex = TRUE, style.tex = style.tex("aer"),
#        markdown = TRUE,
#        title = "Determinants of Public Housing Site Selection",
#        digits = 2, 
#        dict = var_names, replace = TRUE,
#        fitstat = ~ n + ar2 + awr2 + ivf,
#        file = paste0(site_selection_output_dir, "site_selection_probit_models_", data_type, ".tex"))
# 
# 
