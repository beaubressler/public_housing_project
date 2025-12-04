####
# Site selection analysis
# Using 1940 characteristics to predict public housing placement (1941-1973)
####

# Preliminaries -----
library(tidyverse)
library(here)
library(sf)
library(modelsummary)
library(tinytable)
library(fixest)

# Load table utilities helper
source(here("code", "helpers", "table_utilities.R"))

# Data setup
data_type <- "combined"
site_selection_output_dir <- here("output", "regression_results", "site_selection", data_type)
data_dir <- here("data", "derived", "merged", data_type)

# Read data ----
census_tract_sample <-
  st_read(here(data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")) %>% 
  mutate(centroid = st_centroid(geom)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  st_drop_geometry()

# Create reshaped data for 1940 analysis
reshaped_census_data <-
  census_tract_sample %>%
  filter(YEAR %in% c(1930, 1940, 1950, 1960)) %>%
  select(GISJOIN_1950, YEAR, share_needing_repair, share_no_water, black_share, population_density,
         pct_hs_grad, median_income, median_home_value_calculated, median_rent_calculated,
         lfp_rate,
         black_pop, total_pop, unemp_rate) %>%
  mutate(asinh_total_pop = asinh(total_pop),
         asinh_black_pop = asinh(black_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent_calculated = asinh(median_rent_calculated),
         asinh_median_home_value_calculated = asinh(median_home_value_calculated),
         ) %>%
  pivot_wider(
    names_from = YEAR,
    values_from = c(share_needing_repair, share_no_water, black_share, population_density,
                    pct_hs_grad, median_income, median_home_value_calculated, lfp_rate,
                    median_rent_calculated, total_pop, black_pop,
                    asinh_total_pop, asinh_black_pop, asinh_median_income,
                    asinh_median_rent_calculated, asinh_median_home_value_calculated, unemp_rate),
    names_glue = "{.value}_{YEAR}"
  )

# Create final dataset with treatment status
site_selection_data <- 
  census_tract_sample %>% 
  filter(YEAR == 2000) %>% 
  st_drop_geometry() %>% 
  left_join(reshaped_census_data, by = "GISJOIN_1950") %>% 
  mutate(county_id = paste0(STATEA, COUNTYA),
         asinh_distance_from_cbd = asinh(distance_from_cbd),
         asinh_distance_to_highway_km = asinh(distance_to_highway_km)) %>%
  filter(!is.na(black_share_1940), !is.na(total_pop_1940))

# Variable labels for models
variable_labels_1940 <- c(
  "black_share_1940" = "Black Share",
  "asinh_total_pop_1940" = "Asinh Total Population",
  "total_pop_1940" = "Total Population",
  "asinh_median_income_1940" = "Asinh Median Income",
  "median_income_1940" = "Median Income",
  "asinh_median_rent_calculated_1940" = "Asinh Median Rent",
  "median_rent_calculated_1940" = "Median Rent",
  "pct_hs_grad_1940" = "Pct Graduated HS",
  "unemp_rate_1940" = "Unemployment Rate",
  "lfp_rate_1940" = "LFP Rate",
  "redlined_binary_80pp" = "Redlined (HOLC)",
  "asinh_distance_from_cbd" = "Asinh Dist. from CBD",
  "cbd" = "CBD Indicator",
  "share_needing_repair_1940" = "Share Needing Major Repairs",
  "share_no_water_1940" = "Share Without Water",
  "asinh_distance_to_highway_km" = "Asinh Dist. to Highway",
  "ur_binary_5pp" = "Urban Renewal Area"
)

# Poster version: short, high-contrast labels
variable_labels_1940_poster <- c(
  "black_share_1940"                     = "\\% Black",
  "asinh_total_pop_1940"                 = "ln(Pop)",
  "total_pop_1940"                       = "Pop",
  "asinh_median_income_1940"             = "ln(Income)",
  "median_income_1940"                   = "Income",
  "pct_hs_grad_1940"                     = "HS grad (\\%)",
  "unemp_rate_1940"                      = "Unemp. (\\%)",
  "lfp_rate_1940"                        = "LFP (\\%)",
  "asinh_median_rent_calculated_1940"    = "ln(Rent)",
  "median_rent_calculated_1940"          = "Rent",
  "redlined_binary_80pp"                 = "HOLC redlined",
  "asinh_distance_from_cbd"              = "ln(dist. CBD)",
  "cbd"                                  = "CBD",
  "share_needing_repair_1940"            = "Major repair (\\%)",
  "share_no_water_1940"                  = "No water (\\%)",
  "asinh_distance_to_highway_km"         = "ln(dist. hwy, km)",
  "ur_binary_5pp"                        = "UR area"
)

# Characteristics of treated vs untreated tracts in 1940 ----

# Create descriptive table comparing initial characteristics
descriptive_vars <- c(
  "black_share_1940", "asinh_total_pop_1940", "asinh_median_income_1940",
  "pct_hs_grad_1940", "unemp_rate_1940", "lfp_rate_1940", 
  "asinh_median_rent_calculated_1940", "share_needing_repair_1940",
  "asinh_distance_from_cbd", "redlined_binary_80pp", "ur_binary_5pp"
)

# Calculate means and standard deviations by treatment status
descriptive_stats <- site_selection_data %>%
  filter(city == "New York City") %>% 
  select(treated, all_of(descriptive_vars)) %>%
  pivot_longer(cols = -treated, names_to = "variable", values_to = "value") %>%
  group_by(treated, variable) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    sd_val = sd(value, na.rm = TRUE),
    n_obs = sum(!is.na(value)),
    .groups = 'drop'
  ) %>%
  mutate(
    mean_sd = ifelse(variable %in% c("redlined_binary_80pp", "ur_binary_5pp"),
                     sprintf("%.3f", mean_val),
                     sprintf("%.2f (%.2f)", mean_val, sd_val))
  ) %>%
  select(treated, variable, mean_sd) %>%
  pivot_wider(names_from = treated, values_from = mean_sd, names_prefix = "treated_")

# Calculate differences and t-tests
difference_tests <- site_selection_data %>%
  select(treated, all_of(descriptive_vars)) %>%
  pivot_longer(cols = -treated, names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    diff = mean(value[treated == 1], na.rm = TRUE) - mean(value[treated == 0], na.rm = TRUE),
    t_stat = t.test(value[treated == 1], value[treated == 0])$statistic,
    p_value = t.test(value[treated == 1], value[treated == 0])$p.value,
    .groups = 'drop'
  ) %>%
  mutate(
    diff_formatted = sprintf("%.3f", diff),
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**", 
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    diff_sig = paste0(diff_formatted, significance)
  )

# Combine into final table
descriptive_table_data <- descriptive_stats %>%
  left_join(difference_tests, by = "variable") %>%
  mutate(
    Variable = case_when(
      variable == "black_share_1940" ~ "Black Share",
      variable == "asinh_total_pop_1940" ~ "Log Total Population", 
      variable == "asinh_median_income_1940" ~ "Log Median Income",
      variable == "pct_hs_grad_1940" ~ "Pct Graduated HS",
      variable == "unemp_rate_1940" ~ "Unemployment Rate",
      variable == "lfp_rate_1940" ~ "LFP Rate",
      variable == "asinh_median_rent_calculated_1940" ~ "Log Median Rent",
      variable == "share_needing_repair_1940" ~ "Share Needing Major Repairs",
      variable == "asinh_distance_from_cbd" ~ "Log Dist. from CBD",
      variable == "redlined_binary_80pp" ~ "Redlined (HOLC)",
      variable == "ur_binary_5pp" ~ "Urban Renewal Area",
      TRUE ~ variable
    )
  ) %>%
  select(Variable, treated_0, treated_1, diff_sig) %>%
  rename(
    "Non-Public Housing" = treated_0,
    "Public Housing" = treated_1,
    "Difference" = diff_sig
  )

# Add sample sizes
n_non_public <- sum(site_selection_data$treated == 0, na.rm = TRUE)
n_public <- sum(site_selection_data$treated == 1, na.rm = TRUE)

descriptive_table_data <- descriptive_table_data %>%
  add_row(
    Variable = "N (Tracts)",
    `Non-Public Housing` = as.character(n_non_public),
    `Public Housing` = as.character(n_public),
    Difference = "",
    .before = 1
  )

# Create publication-ready table
descriptive_table <- tt(descriptive_table_data) |>
  format_tt(escape = FALSE) |>
  theme_tt(theme = "tabular")

# Save table
descriptive_table_path <- here(site_selection_output_dir, "descriptive_characteristics_1940.tex")
save_tt(descriptive_table, descriptive_table_path, overwrite = TRUE)
remove_table_wrappers(descriptive_table_path)


# Site selection models using 1940 characteristics -----

## Model 0: Parsimoneous
model_0_lpm <- feols(
  treated ~
    # demographics
    black_share_1940 +
    asinh_total_pop_1940 +
    # SES
    asinh_median_income_1940 +
    # Housing quality
    # share_needing_repair_1940 + 
    asinh_median_rent_calculated_1940
    | county_id,
  data = site_selection_data,
  vcov_conley(lat = "lat", lon = "lon", cutoff = 2)
)

# # in levels
# model_0_lpm_levels <- 
#   feols(
#     treated ~ 
#       black_share_1940 +
#       asinh_total_pop_1940 +
#       median_income_1940 | county_id,
#     data = site_selection_data,
#     vcov_conley(lat = "lat", lon = "lon", cutoff = 2)
#   )

## Model 1: Full model
model_1_lpm <- feols(
  treated ~
    # demographics
    black_share_1940 +
    asinh_total_pop_1940 +
    # SES
    asinh_median_income_1940 +
    pct_hs_grad_1940 +
    unemp_rate_1940 +
    lfp_rate_1940 +
    # housing
    asinh_median_rent_calculated_1940 +
    # share_needing_repair_1940 + 
    #share_needing_repair_1940 +
    # geography
    asinh_distance_from_cbd + 
    cbd + 
    # pre-exsting discrimination 
    redlined_binary_80pp | county_id,
  data = site_selection_data,
  vcov_conley(lat = "lat", lon = "lon", cutoff = 2)
)

# model_1_lpm_levels <-
#   feols(
#     treated ~
#       # demographics
#       black_share_1940 +
#       total_pop_1940 +
#       # SES
#       median_income_1940 +
#       pct_hs_grad_1940 +
#       unemp_rate_1940 +
#       lfp_rate_1940 +
#       # housing
#       median_rent_calculated_1940 +
#       #share_needing_repair_1940 +
#       # geography
#       distance_from_cbd + 
#       cbd + 
#       # pre-existing discrimination 
#       redlined_binary_80pp | county_id,
#     data = site_selection_data,
#     vcov_conley(lat = "lat", lon = "lon", cutoff = 2)
#   )

# 8/21/2025: I don't include share needing major repairs in 1940 because 
# it is missing for some tracts. However, if I do include it, it is statistically insignificant



## Model 2: Robustness with housing quality
# Note: share_needing_repair_1940 and share_no_water_1940 are missing for ~5% of tracts
# Included to test robustness, expected to be insignificant due to correlation with rent
model_2_lpm <- feols(
  treated ~
    # demographics
    black_share_1940 +
    asinh_total_pop_1940 +
    # SES
    asinh_median_income_1940 +
    pct_hs_grad_1940 +
    unemp_rate_1940 +
    lfp_rate_1940 +
    # housing
    asinh_median_rent_calculated_1940 +
    share_needing_repair_1940 +  # Robustness: housing quality
    # share_no_water_1940 +  # Robustness: housing quality (lack of running water) -> TODO this variable is defined wrong i think
    # geography
    asinh_distance_from_cbd +
    cbd +
    # pre-exsting discrimination
    redlined_binary_80pp | county_id,
  data = site_selection_data,
  vcov_conley(lat = "lat", lon = "lon", cutoff = 2)
)

## Model 3: Add mid-century urban policies
model_3_lpm <- feols(
  treated ~
    # demographics
    black_share_1940 +
    asinh_total_pop_1940 +
    # SES
    asinh_median_income_1940 +
    pct_hs_grad_1940 +
    unemp_rate_1940 +
    lfp_rate_1940 +
    # housing
    asinh_median_rent_calculated_1940 +
    # geography
    asinh_distance_from_cbd + 
    cbd +
    # pre-exsting discrimination
    redlined_binary_80pp +
    # Mid-century urban policies
    asinh_distance_to_highway_km + ur_binary_5pp | county_id,
  data = site_selection_data,
  vcov_conley(lat = "lat", lon = "lon", cutoff = 2)
)


# Fixed effects row for output
fe_row_1940 <- tibble::tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
  "County fixed effects", "Yes", "Yes", "Yes", "Yes"
)

# Helper to extract Conley vcov from model
conley_vcov <- function(x) vcov_conley(x, lat = "lat", lon = "lon", cutoff = 2)

# Output LPM results
models_1940_lpm <- list(
  "(1)" = model_0_lpm,
  "(2)" = model_1_lpm,
  "(3)" = model_2_lpm,
  "(4)" = model_3_lpm
)

site_selection_table <- modelsummary(
  models_1940_lpm,
  coef_omit = "(Intercept)",
  coef_map = variable_labels_1940,
  vcov = conley_vcov,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  fmt = 3,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",
  add_rows = fe_row_1940,
  output = "tinytable"
) |>
  theme_tt(theme = "tabular")

# save to .tex
table_path <- here(site_selection_output_dir, "site_selection_1940_lpm.tex")
save_tt(site_selection_table, table_path, overwrite = TRUE)
remove_table_wrappers(table_path)

## Smaller version for poster ----
keep_poster <- c(
  "asinh_total_pop_1940",
  "black_share_1940",
  "asinh_median_income_1940",
  "asinh_median_rent_calculated_1940",
  "unemp_rate_1940",
  "lfp_rate_1940",
  "redlined_binary_80pp",
  "ur_binary_5pp"
)

# short, LaTeX-safe labels
# variable_labels_1940_poster <- c(
#   "black_share_1940"                  = "\\% Black",
#   "asinh_median_income_1940"          = "ln(Income)",
#   "asinh_median_rent_calculated_1940" = "ln(Rent)",
#   "asinh_distance_from_cbd"           = "ln(dist. CBD)",
#   "redlined_binary_80pp"              = "HOLC redlined",
#   "share_needing_repair_1940"         = "Needs repair (percent)"
# )
models_1940_lpm_poster <- list(
  "(1)" = models_1940_lpm[["(1)"]],
  "(2)" = models_1940_lpm[["(3)"]]
)


site_selection_table_poster <- modelsummary(
  models_1940_lpm_poster,
  coef_omit = "(Intercept)",
  coef_map = variable_labels_1940_poster[keep_poster],
  stars = TRUE,
  fmt = 2,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within|Adj",
  output = "tinytable"
) |>
  theme_tt(theme = "tabular")

poster_table_path <- here(site_selection_output_dir, "site_selection_1940_lpm_poster.tex")
save_tt(site_selection_table_poster, poster_table_path, overwrite = TRUE)
remove_table_wrappers(poster_table_path)


# Standardized coefficients ----
baseline_prob <- mean(site_selection_data$treated, na.rm = TRUE)  # 10.58

# --- Redlined ---
redlined_effect <- coef(model_3_lpm)["redlined_binary_80pp"]
redlined_effect_relative <- redlined_effect /baseline_prob
redlined_effect
redlined_effect_relative

# --- Black share ---
black_share_std <- sd(site_selection_data$black_share_1940, na.rm = TRUE)
black_share_effect <- coef(model_3_lpm)["black_share_1940"] * black_share_std
black_share_effect_relative <- black_share_effect / baseline_prob
black_share_effect
black_share_effect_relative

# --- Income ---
income_std <- sd(site_selection_data$asinh_median_income_1940, na.rm = TRUE)
income_effect <- coef(model_3_lpm)["asinh_median_income_1940"] * income_std
income_effect_relative <- income_effect / baseline_prob
income_effect
income_effect_relative

# --- Unemployment rate ---
unemployment_std <- sd(site_selection_data$unemp_rate_1940, na.rm = TRUE)
unemployment_effect <- coef(model_3_lpm)["unemp_rate_1940"] * unemployment_std
unemployment_effect_relative <- unemployment_effect / baseline_prob
unemployment_effect
unemployment_effect_relative

# --- Share needing repair ---
share_needing_repair_std <- sd(site_selection_data$share_needing_repair_1940, na.rm = TRUE)
share_needing_repair_effect <- coef(model_2_lpm)["share_needing_repair_1940"] * share_needing_repair_std
share_needing_repair_effect_relative <- share_needing_repair_effect / baseline_prob
share_needing_repair_effect
share_needing_repair_effect_relative

# --- Urban renewal ---
urban_renewal_effect <- coef(model_3_lpm)["ur_binary_5pp"]
urban_renewal_effect_relative <- urban_renewal_effect / baseline_prob
urban_renewal_effect
urban_renewal_effect_relative

# Print summary -----
cat("Site selection analysis complete using 1940 characteristics\n")
cat("Sample size:", nrow(site_selection_data), "tracts\n")
cat("Treated tracts:", sum(site_selection_data$treated), "\n")



# Logit -----
# -------------------------
# Logit (with county FE + Conley SE)
# -------------------------
library(fixest)            # you already loaded
library(modelsummary)      # you already loaded
library(marginaleffects)   # install.packages("marginaleffects") if needed

## Helper: Conley vcov for fixest objects
conley2 <- function(m) vcov_conley(m, lat = "lat", lon = "lon", cutoff = 2)

## Model 0 (parsimonious)
model_0_logit <- feglm(
  treated ~
    black_share_1940 +
    asinh_total_pop_1940 +
    asinh_median_income_1940 +
    asinh_median_rent_calculated_1940 |
    county_id,
  data = site_selection_data,
  family = binomial(link = "logit")
)

## Model 1 (full)
model_1_logit <- feglm(
  treated ~
    black_share_1940 +
    asinh_total_pop_1940 +
    asinh_median_income_1940 +
    pct_hs_grad_1940 +
    unemp_rate_1940 +
    lfp_rate_1940 +
    asinh_median_rent_calculated_1940 +
    asinh_distance_from_cbd +
    cbd +
    redlined_binary_80pp |
    county_id,
  data = site_selection_data,
  family = binomial(link = "logit")
)

## Model 2 (add housing quality robustness)
model_2_logit <- feglm(
  treated ~
    black_share_1940 +
    asinh_total_pop_1940 +
    asinh_median_income_1940 +
    pct_hs_grad_1940 +
    unemp_rate_1940 +
    lfp_rate_1940 +
    asinh_median_rent_calculated_1940 +
    share_needing_repair_1940 +
    asinh_distance_from_cbd +
    cbd +
    redlined_binary_80pp |
    county_id,
  data = site_selection_data,
  family = binomial(link = "logit")
)

## Model 3 (add mid-century policies)
model_3_logit <- feglm(
  treated ~
    black_share_1940 +
    asinh_total_pop_1940 +
    asinh_median_income_1940 +
    pct_hs_grad_1940 +
    unemp_rate_1940 +
    lfp_rate_1940 +
    asinh_median_rent_calculated_1940 +
    asinh_distance_from_cbd +
    cbd +
    redlined_binary_80pp +
    asinh_distance_to_highway_km + ur_binary_5pp |
    county_id,
  data = site_selection_data,
  family = binomial(link = "logit")
)

# -------------------------
# Table A: Odds Ratios (exp(beta)) with Conley SEs
# -------------------------
models_1940_logit <- list(
  "(1)" = model_0_logit,
  "(2)" = model_1_logit,
  "(3)" = model_2_logit,
  "(4)" = model_3_logit
)

# modelsummary can exponentiate coefficients for logit to show ORs
logit_or_table <- modelsummary(
  models_1940_logit,
  coef_map  = variable_labels_1940,
  coef_omit = "(Intercept)",
  exponentiate = TRUE,              # odds ratios
  vcov = conley2,                   # Conley(2km)
  stars = TRUE,
  fmt = 3,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",
  gof_map  = get_gof_map_regression(),
  add_rows = fe_row_1940,
  output = "tinytable"
) |>
  theme_tt(theme = "tabular")

logit_or_path <- here(site_selection_output_dir, "site_selection_1940_logit_or.tex")
save_tt(logit_or_table, logit_or_path, overwrite = TRUE)
remove_table_wrappers(logit_or_path)

# -------------------------
# Table B: Average Marginal Effects (AME) with Conley SEs
# (Do this for your headline model; repeat for others if desired)
# -------------------------
# marginaleffects works with fixest::feglm; supply vcov func for SEs
ame_m3 <- marginaleffects::avg_slopes(
  model_3_logit,
  variables = c(
    "black_share_1940",
    "asinh_total_pop_1940",
    "asinh_median_income_1940",
    "pct_hs_grad_1940",
    "unemp_rate_1940",
    "lfp_rate_1940",
    "asinh_median_rent_calculated_1940",
    "asinh_distance_from_cbd",
    "cbd",
    "redlined_binary_80pp",
    "asinh_distance_to_highway_km",
    "ur_binary_5pp"
  )
)

# format AME table
ame_tbl <- ame_m3 |>
  dplyr::mutate(term = dplyr::recode(term, !!!variable_labels_1940)) |>
  dplyr::select(term, estimate, std.error, conf.low, conf.high, p.value)

ame_tt <- tt(ame_tbl,
             columns = c("term","estimate","std.error","conf.low","conf.high","p.value"),
             cn = c("Variable","AME","SE","CI low","CI high","p")) |>
  format_tt(digits = 3) |>
  theme_tt(theme = "tabular")

ame_path <- here(site_selection_output_dir, "site_selection_1940_logit_ame.tex")
save_tt(ame_tt, ame_path, overwrite = TRUE)
remove_table_wrappers(ame_path)

# =========================
# AUC + LPM diagnostics + Marginal Effects (clean)
# =========================
library(pROC)
library(dplyr)
library(ggplot2)
library(marginaleffects)

# --- helper for AUC from fixest models ---
auc_from_fixest <- function(model, y_full = NULL, data_full = NULL) {
  p <- as.numeric(fitted(model))
  y <- model$y
  if (is.null(y)) {
    if (is.null(y_full)) stop("Provide y_full if model$y is NULL.")
    y <- y_full
  }
  if (!is.null(data_full)) {
    p_all <- tryCatch(predict(model, newdata = data_full, type = "response"), error = function(e) NULL)
    if (!is.null(p_all)) {
      keep <- !is.na(p_all)
      y <- y_full[keep]
      p <- p_all[keep]
    }
  }
  roc(y, p, quiet = TRUE)$auc
}

# =========================
# 1) AUC
# =========================

# LOGIT
auc_logit <- auc_from_fixest(
  model_3_logit,
  y_full    = site_selection_data$treated,
  data_full = site_selection_data
)

# LPM
p_lpm   <- as.numeric(fitted(model_3_lpm))
keep_lpm <- !is.na(p_lpm)
y_lpm    <- site_selection_data$treated[keep_lpm]
auc_lpm  <- roc(y_lpm, p_lpm[keep_lpm], quiet = TRUE)$auc

cat(sprintf("AUC (Logit): %.3f\n", as.numeric(auc_logit)))
cat(sprintf("AUC (LPM)  : %.3f\n", as.numeric(auc_lpm)))

# Optional overlay ROC plot
roc_logit <- roc(model_3_logit$y, fitted(model_3_logit), quiet = TRUE)
roc_lpm   <- roc(y_lpm, p_lpm[keep_lpm], quiet = TRUE)
plot(roc_lpm,  col = "black", lwd = 2, main = "ROC: LPM vs Logit")
plot(roc_logit, add = TRUE, col = "gray50", lwd = 2, lty = 2)
legend("bottomright",
       c(sprintf("LPM  (AUC=%.3f)", as.numeric(auc_lpm)),
         sprintf("Logit(AUC=%.3f)", as.numeric(auc_logit))),
       col = c("black", "gray50"), lwd = 2, lty = c(1,2), bty = "n")

# =========================
# 2) LPM diagnostics
# =========================
pred_lpm <- p_lpm
cat("Share < 0  :", mean(pred_lpm < 0, na.rm = TRUE), "\n")
cat("Share > 1  :", mean(pred_lpm > 1, na.rm = TRUE), "\n")
cat("Range [min,max]:", paste(range(pred_lpm, na.rm = TRUE), collapse = " , "), "\n")
hist(pred_lpm, breaks = 40, main = "LPM predicted values", xlab = "Predicted P(treated)")

# =========================
# 3) Marginal effects (MEM, +10pp diff, and by-risk-decile)
# =========================

# Conley(2km) vcov for marginaleffects
conley2 <- function(m) vcov_conley(m, lat = "lat", lon = "lon", cutoff = 2)

## 3a) MEM (derivative at the mean) for all regressors
mem_all <- slopes(model_3_logit, newdata = "mean", vcov = conley2)   # <- use slopes(), not marginaleffects()
summary(mem_all)

## 3b) MEM for black_share, then scale to +10pp (linear scaling)
mem_black <- slopes(
  model_3_logit,
  variables = "black_share_1940",
  newdata   = "mean",
  vcov      = conley2
)
summary(mem_black)

mem_black_10pp <- transform(mem_black,
                            estimate  = estimate * 0.10,
                            std.error = std.error * 0.10,
                            conf.low  = conf.low  * 0.10,
                            conf.high = conf.high * 0.10
)
mem_black_10pp[, c("term","estimate","std.error","conf.low","conf.high","p.value")]

# Marginal effects by risk decile
# ==================================================
# Function: marginal effects by risk decile for any variable
# ==================================================
get_me_by_decile <- function(model, var, data, vcov_fun = NULL) {
  # Predicted probabilities
  p_full <- predict(model, newdata = data, type = "response")
  idx <- !is.na(p_full)
  nd <- data[idx, ]
  nd$p_hat <- p_full[idx]
  nd$risk_decile <- dplyr::ntile(nd$p_hat, 10)
  
  # Compute marginal effects by risk decile
  ame <- slopes(
    model,
    variables = var,
    by = "risk_decile",
    newdata = nd,
    vcov = vcov_fun
  )
  
  # Add +10pp scaling for convenience
  ame <- ame |>
    mutate(
      est_10pp = estimate * 0.10,
      se_10pp  = std.error * 0.10,
      lo_10pp  = conf.low  * 0.10,
      hi_10pp  = conf.high * 0.10
    )
  
  # Quick plot
  plt <- ggplot(ame, aes(risk_decile, estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .2) +
    labs(
      x = "Decile of predicted risk (logit p̂)",
      y = paste0("Marginal effect on P(treated)\n(per 1.0 in ", var, ")"),
      title = paste("Marginal effect of", var, "by baseline risk")
    ) +
    theme_minimal()
  
  list(table = ame, plot = plt)
}

# Optionally keep your Conley SEs (or set vcov_fun = NULL)
conley2 <- function(m) vcov_conley(m, lat = "lat", lon = "lon", cutoff = 2)

# Run for several variables
me_black     <- get_me_by_decile(model_3_logit, "black_share_1940", site_selection_data, conley2)
me_income    <- get_me_by_decile(model_3_logit, "asinh_median_income_1940", site_selection_data, conley2)
me_redlined  <- get_me_by_decile(model_3_logit, "redlined_binary_80pp", site_selection_data, conley2)
me_unemp     <- get_me_by_decile(model_3_logit, "unemp_rate_1940", site_selection_data, conley2)

# View tables
head(me_black$table)
head(me_income$table)

# Show plots
me_black$plot
me_income$plot
me_redlined$plot
me_unemp$plot
