## 01_setup_and_balance.R
## Spatial DiD: Data loading and balance table creation
## Part 1 of the spatial DiD analysis pipeline

PART1_DONE <- FALSE

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(tableone)
  library(kableExtra)
  library(webshot)
  library(ggplot2)
})

set.seed(123)

message("\n=== SPATIAL DID PART 1: SETUP AND BALANCE ===")

# ---- Switches ----
data_type <- get0("data_type", ifnotfound = "combined")

# ---- Directories ----
merged_data_dir <- here("data", "derived", "merged", data_type)
balance_table_dir <- here("output", "balance_tables", "stacked_did", data_type)

# Create output directories
dir.create(file.path(balance_table_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(balance_table_dir, "plots"), recursive = TRUE, showWarnings = FALSE)

# ---- File paths ----
event_study_rings_path <- here(merged_data_dir, "event_study_data_rings.csv")
unique_tracts_and_rings_path <- here(merged_data_dir, "unique_tracts_and_rings.csv")

# ---- Load data ----
message("Loading spatial DiD data...")
event_study_data_rings <- read_csv(event_study_rings_path, show_col_types = FALSE) %>%
  mutate(asinh_distance_from_cbd = asinh(distance_from_cbd))
unique_tracts_and_rings <- read_csv(unique_tracts_and_rings_path, show_col_types = FALSE)

message(sprintf("Loaded %d observations across %d treated projects",
                nrow(event_study_data_rings),
                n_distinct(event_study_data_rings$treated_id)))

# ---- Balance testing covariates (matching matched_did exactly) ----
covariates <- c(
  "asinh_pop_total", "black_share", "asinh_pop_black",
  "asinh_median_income", "asinh_median_rent_calculated",
  "unemp_rate", "lfp_rate", "asinh_distance_from_cbd",
  "distance_to_highway_km", "pct_hs_grad"
)

# ---- Prepare balance test data (at event_time = -10) ----
message("\nPreparing baseline data for balance tests (t = -10)...")
balanced_data <- event_study_data_rings %>%
  filter(event_time == -10) %>%
  filter(white_pop <= total_pop,
         black_pop <= total_pop,
         white_share <= 1,
         black_share <= 1) %>%
  filter(!is.na(black_share))

message(sprintf("Balance sample: %d tracts", nrow(balanced_data)))

# ---- Balance Table: Treated vs Outer Ring ----
message("\n--- Creating Treated vs Outer Ring balance tables ---")

balanced_data_treated <- balanced_data %>%
  filter(location_type %in% c("treated", "outer"))

balance_table_treated <- CreateTableOne(vars = covariates,
                                        strata = "location_type",
                                        data = balanced_data_treated,
                                        test = TRUE)

print(balance_table_treated, smd = TRUE)

# Save table to HTML/PDF
balance_html_treated <- kable(print(balance_table_treated, printToggle = FALSE, smd = TRUE),
                              format = "html", caption = "Balance Table: Treated vs Outer") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

html_file_treated <- file.path(balance_table_dir, "tables", "balance_table_treated_vs_outer.html")
writeLines(balance_html_treated, html_file_treated)

pdf_file_treated <- file.path(balance_table_dir, "tables", "balance_table_treated_vs_outer.pdf")
webshot(html_file_treated, pdf_file_treated, delay = 1, vwidth = 1000, vheight = 800)
if (file.exists(html_file_treated)) file.remove(html_file_treated)

message("Treated vs Outer balance table saved to: ", pdf_file_treated)

# Create balance plot for treated vs outer
source(here("code", "learn", "matched_did", "helpers", "balance_table_creation.R"))
create_balance_plot(balance_table_treated,
                   group_name = "treated",
                   title = "Pre-treatment Balance: Treated vs Outer Ring",
                   save_path = file.path(balance_table_dir, "plots", "balance_plot_treated_vs_outer.pdf"))

message("Treated vs Outer balance plot saved")

# ---- Balance Table: Inner vs Outer Ring (Spillover) ----
message("\n--- Creating Inner vs Outer Ring balance tables ---")

balanced_data_inner <- balanced_data %>%
  filter(location_type %in% c("inner", "outer"))

balance_table_inner <- CreateTableOne(vars = covariates,
                                      strata = "location_type",
                                      data = balanced_data_inner,
                                      test = TRUE)

print(balance_table_inner, smd = TRUE)

# Save table to HTML/PDF
balance_html_inner <- kable(print(balance_table_inner, printToggle = FALSE, smd = TRUE),
                            format = "html", caption = "Balance Table: Inner vs Outer") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

html_file_inner <- file.path(balance_table_dir, "tables", "balance_table_inner_vs_outer.html")
writeLines(balance_html_inner, html_file_inner)

pdf_file_inner <- file.path(balance_table_dir, "tables", "balance_table_inner_vs_outer.pdf")
webshot(html_file_inner, pdf_file_inner, delay = 1, vwidth = 1000, vheight = 800)
if (file.exists(html_file_inner)) file.remove(html_file_inner)

message("Inner vs Outer balance table saved to: ", pdf_file_inner)

# Create balance plot for inner vs outer
create_balance_plot(balance_table_inner,
                   group_name = "inner",
                   title = "Pre-treatment Balance: Inner vs Outer Ring",
                   save_path = file.path(balance_table_dir, "plots", "balance_plot_inner_vs_outer.pdf"))

message("Inner vs Outer balance plot saved")

# ---- Export data objects for next scripts ----
message("\n=== Part 1 complete: Data loaded and balance tables created ===")
message("Balance tables saved to: ", balance_table_dir)

PART1_DONE <- TRUE
