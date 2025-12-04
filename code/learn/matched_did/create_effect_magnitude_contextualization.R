## create_effect_magnitude_contextualization.R
## Generates interpretable effect magnitudes for presentation slides
## Focus: Population variables and demographic shares with full context

library(tidyverse)
library(here)

# Configuration - matches your main analysis setup
data_type <- "combined"
VARIANT <- "baseline"
dataset_type <- "2_year"

results_dir <- here("output", "regression_results", "matched_did",
                   data_type, VARIANT, dataset_type)

# Read baseline summary statistics and regression coefficients
summary_stats <- read_csv(here(results_dir, "event_study_coefficients.csv"))

# Define baseline means at t = -10 (baseline period)
# Values from matched_dataset at baseline year
baseline_means <- tribble(
  ~variable, ~treated_mean, ~spillover_mean,
  "black_share", 0.269, 0.205,
  "white_share", 0.716, 0.782,
  "asinh_pop_total", 9.10, 9.02,
  "asinh_pop_black", 5.77, 5.11,
  "asinh_pop_white", 8.38, 8.49,
  "asinh_private_population_estimate", 9.10, 9.02,
  "asinh_median_income", 3.85, 3.90,
  "asinh_median_rent_calculated", 6.40, 6.47,
  "pct_hs_grad", 0.31, 0.33,
  "unemp_rate", 0.11, 0.10
)

# Extract coefficients at t=+30
coefs_t30 <- summary_stats %>%
  filter(term == "30") %>%
  select(outcome_clean, group, estimate, std.error, p.value) %>%
  rename(variable = outcome_clean)

# Helper function to format significance
add_sig <- function(p_val) {
  case_when(
    p_val < 0.01 ~ "***",
    p_val < 0.05 ~ "**",
    p_val < 0.10 ~ "*",
    TRUE ~ " (not significant)"
  )
}

# Helper function to calculate new levels and % changes
contextualize_effect <- function(var_name, group_name, baseline_means, coefs_t30) {

  baseline_row <- baseline_means %>%
    filter(variable == var_name)

  coef_row <- coefs_t30 %>%
    filter(variable == var_name, group == group_name)

  if (nrow(baseline_row) == 0 || nrow(coef_row) == 0) {
    return(NULL)
  }

  baseline <- if (group_name == "treated") baseline_row$treated_mean else baseline_row$spillover_mean
  coef <- coef_row$estimate
  p_val <- coef_row$p.value
  sig <- add_sig(p_val)

  # For share variables (already in levels)
  if (str_detect(var_name, "share|pct_|unemp_rate")) {
    new_value <- baseline + coef
    pct_change <- (coef / baseline) * 100

    # Format differently for shares vs rates
    if (str_detect(var_name, "share")) {
      return(list(
        variable = var_name,
        group = group_name,
        baseline = baseline,
        coefficient = coef,
        new_value = new_value,
        pp_change = coef,
        pct_change = pct_change,
        p_value = p_val,
        text_snippet = sprintf(
          "%s by %.1fpp from %.0f%% to %.1f%% (%.0f%% relative %s)%s",
          ifelse(coef > 0, "Increased", "Declined"),
          abs(coef * 100), baseline * 100, new_value * 100, abs(pct_change),
          ifelse(coef > 0, "increase", "decline"), sig
        )
      ))
    } else {
      return(list(
        variable = var_name,
        group = group_name,
        baseline = baseline,
        coefficient = coef,
        new_value = new_value,
        change = coef,
        pct_change = pct_change,
        p_value = p_val,
        text_snippet = sprintf(
          "Changed by %.1fpp from %.1f%% to %.1f%% (%.0f%% relative change)%s",
          coef * 100, baseline * 100, new_value * 100, pct_change, sig
        )
      ))
    }
  }

  # For asinh-transformed variables (coefficient ≈ % change)
  if (str_detect(var_name, "asinh")) {
    # For asinh variables, coefficient approximates % change
    # More precisely: % change = 100 * (exp(coef) - 1)
    pct_change <- 100 * (exp(coef) - 1)

    return(list(
      variable = var_name,
      group = group_name,
      baseline_asinh = baseline,
      coefficient = coef,
      approx_pct_change = pct_change,
      p_value = p_val,
      text_snippet = sprintf(
        "%s by ~%.0f%%%s",
        ifelse(coef > 0, "Increased", "Declined"),
        abs(pct_change), sig
      )
    ))
  }

  # For raw population (not asinh)
  if (var_name == "total_pop") {
    new_value <- baseline + coef
    pct_change <- (coef / baseline) * 100

    return(list(
      variable = var_name,
      group = group_name,
      baseline = baseline,
      coefficient = coef,
      new_value = new_value,
      pct_change = pct_change,
      p_value = p_val,
      text_snippet = sprintf(
        "%s by %.0f%% (from ~%.0f to ~%.0f residents)%s",
        ifelse(coef > 0, "Increased", "Declined"),
        abs(pct_change), baseline, new_value, sig
      )
    ))
  }

  return(NULL)
}

# Key variables to contextualize
key_vars <- c(
  "black_share",
  "white_share",
  "asinh_pop_total",
  "asinh_pop_black",
  "asinh_pop_white",
  "asinh_private_population_estimate",
  "asinh_median_income",
  "asinh_median_rent_calculated"
)

# Generate contextualizations for treated neighborhoods
cat("\n========================================\n")
cat("TREATED NEIGHBORHOODS (t = +30)\n")
cat("========================================\n\n")

treated_contexts <- map(key_vars, ~contextualize_effect(.x, "treated", baseline_means, coefs_t30))
treated_contexts <- compact(treated_contexts)

for (ctx in treated_contexts) {
  cat(sprintf("%s:\n  %s\n\n", ctx$variable, ctx$text_snippet))
}

# Generate contextualizations for spillover neighborhoods
cat("\n========================================\n")
cat("SPILLOVER NEIGHBORHOODS (t = +30)\n")
cat("========================================\n\n")

spillover_contexts <- map(key_vars, ~contextualize_effect(.x, "inner", baseline_means, coefs_t30))
spillover_contexts <- compact(spillover_contexts)

for (ctx in spillover_contexts) {
  cat(sprintf("%s:\n  %s\n\n", ctx$variable, ctx$text_snippet))
}

# Save to CSV for reference
treated_df <- map_dfr(treated_contexts, as_tibble)
spillover_df <- map_dfr(spillover_contexts, as_tibble)

combined_df <- bind_rows(
  treated_df %>% mutate(group = "treated"),
  spillover_df %>% mutate(group = "spillover")
)

output_file <- here(results_dir, "effect_magnitudes_context.csv")
write_csv(combined_df, output_file)

cat("\n========================================\n")
cat(sprintf("Saved detailed calculations to:\n%s\n", output_file))
cat("========================================\n\n")

# Print some suggested bullet points
cat("\n========================================\n")
cat("SUGGESTED SLIDE BULLETS - TREATED\n")
cat("========================================\n\n")

cat("Population and Demographics:\n")
for (ctx in treated_contexts) {
  if (!is.null(ctx) && str_detect(ctx$variable, "pop|share")) {
    cat(sprintf("  • %s\n", ctx$text_snippet))
  }
}

cat("\nEconomic Outcomes:\n")
for (ctx in treated_contexts) {
  if (!is.null(ctx) && str_detect(ctx$variable, "income|rent")) {
    cat(sprintf("  • %s\n", ctx$text_snippet))
  }
}

cat("\n========================================\n")
cat("SUGGESTED SLIDE BULLETS - SPILLOVER\n")
cat("========================================\n\n")

cat("Population and Demographics:\n")
for (ctx in spillover_contexts) {
  if (!is.null(ctx) && str_detect(ctx$variable, "pop|share")) {
    cat(sprintf("  • %s\n", ctx$text_snippet))
  }
}

cat("\nEconomic Outcomes:\n")
for (ctx in spillover_contexts) {
  if (!is.null(ctx) && str_detect(ctx$variable, "income|rent")) {
    cat(sprintf("  • %s\n", ctx$text_snippet))
  }
}

cat("\n")
