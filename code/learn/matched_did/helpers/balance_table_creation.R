library(tableone)
library(tinytable)
library(dplyr)
library(stringr)
library(tibble)
library(ggplot2)
library(here)

# Load color palettes
source(here("code", "helpers", "color_palettes.R"))

# Create balance plot from CreateTableOne object
create_balance_plot <- function(tableone_obj, group_name, title = "Covariate Balance", save_path = NULL) {

  # Extract SMD from tableone object
  table_matrix <- print(tableone_obj, smd = TRUE, printToggle = FALSE)

  # Convert to data frame
  balance_df <- as.data.frame(table_matrix) %>%
    rownames_to_column("Variable") %>%
    filter(Variable != "n") %>%  # Remove sample size row
    select(Variable, SMD) %>%
    mutate(
      # Handle "<0.001" and similar formatted values
      SMD = gsub("<", "", SMD),  # Remove < symbol
      SMD = abs(as.numeric(SMD))  # Convert to numeric and take absolute value
    )

  # Variable name mapping - short labels for plots
  variable_labels <- c(
    "asinh_pop_total" = "Population",
    "black_share" = "Black Share",
    "asinh_pop_black" = "Black Pop (asinh)",
    "asinh_median_income" = "Median Income (asinh)",
    "asinh_median_rent_calculated" = "Median Rent (asinh)",
    "unemp_rate" = "Unemployment Rate",
    "lfp_rate" = "Labor Force Part. Rate",
    "asinh_distance_from_cbd" = "Distance from CBD",
    "distance_to_highway_km" = "Distance to Highway",
    "pct_hs_grad" = "Share HS grad"
  )

  # Clean variable names
  balance_df <- balance_df %>%
    mutate(
      Variable = str_replace_all(Variable, "\\s*\\(mean \\(SD\\)\\)", ""),
      Variable = if_else(Variable %in% names(variable_labels),
                        variable_labels[Variable],
                        Variable),
      # Add indicator for whether balance threshold is exceeded
      exceeds_threshold = SMD > 0.1
    )

  # Create love plot - compact for slides
  p <- ggplot(balance_df, aes(x = SMD, y = reorder(Variable, SMD))) +
    # Vertical reference line at 0.1 threshold
    geom_vline(xintercept = 0.1, linetype = "dashed", color = "gray40", linewidth = 0.5) +
    # Points colored by threshold
    geom_point(aes(color = exceeds_threshold), size = 3) +
    scale_color_manual(values = c("TRUE" = okabe_ito[["vermillion"]],
                                   "FALSE" = okabe_ito[["sky_blue"]])) +
    scale_x_continuous(limits = c(0, 0.6), expand = expansion(mult = c(0, 0.05)),
                      breaks = seq(0, 0.6, 0.1)) +
    labs(
      title = title,
      x = "Absolute Standardized Mean Difference",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 11),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      legend.position = "none",
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 10, 5, 5)
    )

  # Save if path provided
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = 5, height = 3.5)
    cat("Balance plot saved to:", save_path, "\n")
  }

  return(p)
}

# Helper function to save both main and slides versions of a balance table
save_balance_table_versions <- function(balance_df,
                                        group_label,      # "Treated" or "Spillover"
                                        year_suffix,      # "" or "_2yr"
                                        year_label,       # "" or " (2-year matching)"
                                        notes_main,       # Full notes for main version (not used, kept for compatibility)
                                        tables_dir,
                                        tables_slides_dir) {

  # Load table utilities for removing wrappers
  source(here("code", "helpers", "table_utilities.R"))

  # Create file name components
  file_suffix <- paste0(tolower(gsub(" ", "_", group_label)), "_neighborhoods", year_suffix)

  # Main version - just the table, no caption or notes
  main_table <- tt(balance_df) %>%
    format_tt(escape = FALSE) %>%
    theme_tt(theme = "tabular")

  save_tt(main_table,
          file.path(tables_dir, paste0("balance_table_", file_suffix, ".tex")),
          overwrite = TRUE)

  # Remove table wrapper to get just the tabular environment
  remove_table_wrappers(file.path(tables_dir, paste0("balance_table_", file_suffix, ".tex")))

  cat(paste0(group_label, year_label, " balance table saved to: ",
             file.path(tables_dir, paste0("balance_table_", file_suffix, ".tex")), "\n"))

  # Slides version - also just the table
  slides_table <- tt(balance_df) %>%
    format_tt(escape = FALSE) %>%
    theme_tt(theme = "tabular")

  save_tt(slides_table,
          file.path(tables_slides_dir, paste0("balance_table_", file_suffix, ".tex")),
          overwrite = TRUE)

  # Remove table wrapper for slides version too
  remove_table_wrappers(file.path(tables_slides_dir, paste0("balance_table_", file_suffix, ".tex")))

  cat(paste0("Slides version saved to: ",
             file.path(tables_slides_dir, paste0("balance_table_", file_suffix, ".tex")), "\n"))
}

# Create balance tables for matched datasets
create_balance_tables <- function(matched_data_1_year, matched_data_2_year, balance_table_dir) {

  cat("\n=== BALANCE CHECKS ===\n")

  # Covariates for balance checking
  covariates <- c(
    "asinh_pop_total", "black_share", "asinh_pop_black",
    "asinh_median_income", "asinh_median_rent_calculated",
    "unemp_rate", "lfp_rate", "asinh_distance_from_cbd",
    "distance_to_highway_km", "pct_hs_grad"
  )

  # Ensure balance tables directory exists
  dir.create(balance_table_dir, recursive = TRUE, showWarnings = FALSE)

  # Create tables directories
  tables_dir <- file.path(balance_table_dir, "tables")
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

  tables_slides_dir <- file.path(tables_dir, "slides")
  dir.create(tables_slides_dir, recursive = TRUE, showWarnings = FALSE)

  # Create plots directories
  plots_dir <- file.path(balance_table_dir, "plots")
  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

  slides_plots_dir <- file.path(plots_dir, "slides")
  dir.create(slides_plots_dir, recursive = TRUE, showWarnings = FALSE)

  # Variable name mapping
  variable_labels <- c(
    "asinh_pop_total" = "Total Population (asinh)",
    "black_share" = "Black Share",
    "asinh_pop_black" = "Black Population (asinh)",
    "asinh_median_income" = "Median Income (asinh)",
    "asinh_median_rent_calculated" = "Median Rent (asinh)",
    "unemp_rate" = "Unemployment Rate",
    "lfp_rate" = "Labor Force Participation Rate",
    "asinh_distance_from_cbd" = "Distance from CBD (asinh)",
    "share_needing_repair" = "Share Needing Major Repairs",
    "total_pop" = "Total Population",
    "black_pop" = "Black Population",
    "white_pop" = "White Population",
    "asinh_pop_white" = "White Population (asinh)",
    "white_share" = "White Share",
    "median_income" = "Median Income",
    "median_rent_calculated" = "Median Rent",
    "median_home_value_calculated" = "Median Home Value",
    "distance_from_cbd" = "Distance from CBD (m)",
    "distance_to_highway_km" = "Distance to Highway (km)",
    "pct_hs_grad" = "Share HS Grad"
  )

  # Function to convert CreateTableOne output to publication table
  format_balance_table <- function(tableone_obj, group_name) {

    # Extract the printed output as data frame with p-values
    table_matrix <- print(tableone_obj, smd = TRUE, test = TRUE, printToggle = FALSE)

    # Convert to data frame and clean up
    balance_df <- as.data.frame(table_matrix) %>%
      rownames_to_column("Variable") %>%
      # Remove the first row (sample sizes) - we'll add back manually
      filter(Variable != "n") %>%
      # Clean variable names - remove (mean (SD)) suffixes and map to clean names
      mutate(
        Variable = str_replace_all(Variable, "\\s*\\(mean \\(SD\\)\\)", ""),
        Variable = if_else(Variable %in% names(variable_labels),
                          variable_labels[Variable],
                          Variable)
      ) %>%
      # Select columns including p-value
      select(Variable, donor_pool, !!sym(group_name), SMD, p) %>%
      rename(
        "Control" = donor_pool,
        "Std. Diff." = SMD,
        "p_value" = p
      ) %>%
      # Add significance stars to Std. Diff. based on p-values
      mutate(
        p_numeric = suppressWarnings(as.numeric(p_value)),
        `Std. Diff.` = case_when(
          is.na(p_numeric) ~ `Std. Diff.`,  # Skip if p-value is NA or non-numeric
          p_numeric < 0.01 ~ paste0(`Std. Diff.`, "***"),
          p_numeric < 0.05 ~ paste0(`Std. Diff.`, "**"),
          p_numeric < 0.10 ~ paste0(`Std. Diff.`, "*"),
          TRUE ~ `Std. Diff.`
        )
      ) %>%
      # Remove p-value and temporary columns
      select(-p_value, -p_numeric) %>%
      rename(
        "Std. Diff." = `Std. Diff.`
      )

    # Rename the treatment column based on group type
    if (group_name == "treated") {
      balance_df <- balance_df %>% rename("Treated" = treated)
    } else if (group_name == "inner") {
      balance_df <- balance_df %>% rename("Spillover" = inner)
    }

    # Add sample sizes back
    # Extract n from the original table
    n_control <- table_matrix["n", "donor_pool"]
    n_treated <- table_matrix["n", group_name]

    balance_df <- balance_df %>%
      add_row(
        Variable = "N (Tracts)",
        Control = n_control,
        !!names(balance_df)[3] := n_treated,  # Dynamic column name
        `Std. Diff.` = "",
        .before = 1
      )

    return(balance_df)
  }

  # Function to convert CreateTableOne output to table WITH p-values for appendix
  format_balance_table_with_pvalues <- function(tableone_obj, group_name) {

    # Extract the printed output as data frame with p-values
    table_matrix <- print(tableone_obj, smd = TRUE, test = TRUE, printToggle = FALSE)

    # Convert to data frame and clean up
    balance_df <- as.data.frame(table_matrix) %>%
      rownames_to_column("Variable") %>%
      # Remove the first row (sample sizes) - we'll add back manually
      filter(Variable != "n") %>%
      # Clean variable names - remove (mean (SD)) suffixes and map to clean names
      mutate(
        Variable = str_replace_all(Variable, "\\s*\\(mean \\(SD\\)\\)", ""),
        Variable = if_else(Variable %in% names(variable_labels),
                          variable_labels[Variable],
                          Variable)
      ) %>%
      # Select columns including p-value (keep it this time!)
      select(Variable, donor_pool, !!sym(group_name), SMD, p) %>%
      rename(
        "Control" = donor_pool,
        "Std. Diff." = SMD,
        "p-value" = p
      )

    # Rename the treatment column based on group type
    if (group_name == "treated") {
      balance_df <- balance_df %>% rename("Treated" = treated)
    } else if (group_name == "inner") {
      balance_df <- balance_df %>% rename("Spillover" = inner)
    }

    # Add sample sizes back
    # Extract n from the original table
    n_control <- table_matrix["n", "donor_pool"]
    n_treated <- table_matrix["n", group_name]

    balance_df <- balance_df %>%
      add_row(
        Variable = "N (Tracts)",
        Control = n_control,
        !!names(balance_df)[3] := n_treated,  # Dynamic column name
        `Std. Diff.` = "",
        `p-value` = "",
        .before = 1
      )

    return(balance_df)
  }

  ## ---------- 1-year matched sample ----------
  cat("\n--- 1-year pre-treatment balance ---\n")

  # Balance for treated group - use actual t=-10 for each project
  balance_data_treated <- matched_data_1_year %>%
    filter(group_type == "treated") %>%
    # Calculate t=-10 year for each project and filter
    mutate(pre_treatment_year = matched_treatment_year - 10) %>%
    filter(year == pre_treatment_year)

  if (nrow(balance_data_treated) > 0) {
    balance_table_treated <- CreateTableOne(
      vars = covariates,
      strata = "location_type",
      data = balance_data_treated,
      test = TRUE
    )

    cat("Balance for TREATED group matching:\n")
    print(balance_table_treated, smd = TRUE)

    # Create balance table for treated neighborhoods
    cat("Creating balance table for treated neighborhoods...\n")

    treated_balance_formatted <- format_balance_table(balance_table_treated, "treated")

    # Save both main and slides versions
    save_balance_table_versions(
      treated_balance_formatted,
      "Treated",
      "",
      "",
      "\\footnotesize \\textit{Notes:} Sample includes all treated neighborhoods and their matched controls from the propensity score matching procedure. Stars indicate statistical significance from t-tests: * p<0.10, ** p<0.05, *** p<0.01.",
      tables_dir,
      tables_slides_dir
    )

    # Create balance plots
    # Paper version - no title
    create_balance_plot(balance_table_treated,
                       group_name = "treated",
                       title = NULL,
                       save_path = file.path(plots_dir, "balance_plot_treated_1yr.pdf"))

    # Slides version - no title
    create_balance_plot(balance_table_treated,
                       group_name = "treated",
                       title = NULL,
                       save_path = file.path(slides_plots_dir, "balance_plot_treated_1yr.pdf"))
  }

  # Balance for inner group - use actual t=-10 for each project
  balance_data_inner <- matched_data_1_year %>%
    filter(group_type == "inner") %>%
    # Calculate t=-10 year for each project and filter
    mutate(pre_treatment_year = matched_treatment_year - 10) %>%
    filter(year == pre_treatment_year)

  if (nrow(balance_data_inner) > 0) {
    balance_table_inner <- CreateTableOne(
      vars = covariates,
      strata = "location_type",
      data = balance_data_inner,
      test = TRUE
    )

    cat("Balance for INNER group matching:\n")
    print(balance_table_inner, smd = TRUE)

    # Create balance table for spillover neighborhoods
    cat("Creating balance table for spillover neighborhoods...\n")

    inner_balance_formatted <- format_balance_table(balance_table_inner, "inner")

    # Save both main and slides versions
    save_balance_table_versions(
      inner_balance_formatted,
      "Spillover",
      "",
      "",
      paste0("\\footnotesize \\textit{Notes:} Sample includes all neighborhoods contiguous to public housing neighborhoods (within 1km of public housing projects) and their matched controls ",
             "from the propensity score matching procedure. Stars indicate statistical significance from t-tests: * p<0.10, ** p<0.05, *** p<0.01."),
      tables_dir,
      tables_slides_dir
    )

    # Create balance plots
    # Paper version - no title
    create_balance_plot(balance_table_inner,
                       group_name = "inner",
                       title = NULL,
                       save_path = file.path(plots_dir, "balance_plot_spillover_1yr.pdf"))

    # Slides version - no title
    create_balance_plot(balance_table_inner,
                       group_name = "inner",
                       title = NULL,
                       save_path = file.path(slides_plots_dir, "balance_plot_spillover_1yr.pdf"))
  }

  ## ---------- 2-year matched sample ----------
  cat("\n--- 2-year pre-treatment balance ---\n")

  # Balance for treated group - use actual t=-10 for each project
  balance_data_treated_2yr <- matched_data_2_year %>%
    filter(group_type == "treated") %>%
    # Calculate t=-10 year for each project and filter
    mutate(pre_treatment_year = matched_treatment_year - 10) %>%
    filter(year == pre_treatment_year)

  if (nrow(balance_data_treated_2yr) > 0) {
    balance_table_treated_2yr <- CreateTableOne(
      vars = covariates,
      strata = "location_type",
      data = balance_data_treated_2yr,
      test = TRUE
    )

    cat("Balance for TREATED group matching (2-year):\n")
    print(balance_table_treated_2yr, smd = TRUE)

    # Create balance table for treated neighborhoods (2-year)
    cat("Creating 2-year balance table for treated neighborhoods...\n")

    treated_balance_formatted_2yr <- format_balance_table(balance_table_treated_2yr, "treated")

    # Save both main and slides versions
    save_balance_table_versions(
      treated_balance_formatted_2yr,
      "Treated",
      "_2yr",
      " (2-year matching)",
      "\\footnotesize \\textit{Notes:} Sample includes all treated neighborhoods and their matched controls from the 2-year pre-treatment propensity score matching procedure. Stars indicate statistical significance from t-tests: * p<0.10, ** p<0.05, *** p<0.01.",
      tables_dir,
      tables_slides_dir
    )

    # Also save version with p-values for appendix
    treated_balance_pvalues_2yr <- format_balance_table_with_pvalues(balance_table_treated_2yr, "treated")

    pvalues_table <- tt(treated_balance_pvalues_2yr) %>%
      format_tt(escape = FALSE) %>%
      theme_tt(theme = "tabular")

    save_tt(pvalues_table,
            file.path(tables_slides_dir, "balance_table_treated_neighborhoods_2yr_pvalues.tex"),
            overwrite = TRUE)

    remove_table_wrappers(file.path(tables_slides_dir, "balance_table_treated_neighborhoods_2yr_pvalues.tex"))

    cat("P-value version saved to:",
        file.path(tables_slides_dir, "balance_table_treated_neighborhoods_2yr_pvalues.tex"), "\n")

    # Create balance plots
    # Paper version - no title
    create_balance_plot(balance_table_treated_2yr,
                       group_name = "treated",
                       title = NULL,
                       save_path = file.path(plots_dir, "balance_plot_treated_2yr.pdf"))

    # Slides version - no title
    create_balance_plot(balance_table_treated_2yr,
                       group_name = "treated",
                       title = NULL,
                       save_path = file.path(slides_plots_dir, "balance_plot_treated_2yr.pdf"))
  }

  # Balance for inner group - use actual t=-10 for each project (2-year)
  balance_data_inner_2yr <- matched_data_2_year %>%
    filter(group_type == "inner") %>%
    # Calculate t=-10 year for each project and filter
    mutate(pre_treatment_year = matched_treatment_year - 10) %>%
    filter(year == pre_treatment_year)

  if (nrow(balance_data_inner_2yr) > 0) {
    balance_table_inner_2yr <- CreateTableOne(
      vars = covariates,
      strata = "location_type",
      data = balance_data_inner_2yr,
      test = TRUE
    )

    cat("Balance for INNER group matching (2-year):\n")
    print(balance_table_inner_2yr, smd = TRUE)

    # Create balance table for spillover neighborhoods (2-year)
    cat("Creating 2-year balance table for spillover neighborhoods...\n")

    inner_balance_formatted_2yr <- format_balance_table(balance_table_inner_2yr, "inner")

    # Save both main and slides versions
    save_balance_table_versions(
      inner_balance_formatted_2yr,
      "Spillover",
      "_2yr",
      " (2-year matching)",
      paste0("\\footnotesize \\textit{Notes:} Sample includes all neighborhoods contiguous to public housing neighborhoods (within 1km of public housing projects) and their matched controls ",
             "from the 2-year pre-treatment propensity score matching procedure. Stars indicate statistical significance from t-tests: * p<0.10, ** p<0.05, *** p<0.01."),
      tables_dir,
      tables_slides_dir
    )

    # Also save version with p-values for appendix
    spillover_balance_pvalues_2yr <- format_balance_table_with_pvalues(balance_table_inner_2yr, "inner")

    pvalues_table_spillover <- tt(spillover_balance_pvalues_2yr) %>%
      format_tt(escape = FALSE) %>%
      theme_tt(theme = "tabular")

    save_tt(pvalues_table_spillover,
            file.path(tables_slides_dir, "balance_table_spillover_neighborhoods_2yr_pvalues.tex"),
            overwrite = TRUE)

    remove_table_wrappers(file.path(tables_slides_dir, "balance_table_spillover_neighborhoods_2yr_pvalues.tex"))

    cat("P-value version saved to:",
        file.path(tables_slides_dir, "balance_table_spillover_neighborhoods_2yr_pvalues.tex"), "\n")

    # Create balance plots
    # Paper version - no title
    create_balance_plot(balance_table_inner_2yr,
                       group_name = "inner",
                       title = NULL,
                       save_path = file.path(plots_dir, "balance_plot_spillover_2yr.pdf"))

    # Slides version - no title
    create_balance_plot(balance_table_inner_2yr,
                       group_name = "inner",
                       title = NULL,
                       save_path = file.path(slides_plots_dir, "balance_plot_spillover_2yr.pdf"))
  }

  cat("\n=== BALANCE TABLES COMPLETE ===\n")

  return(TRUE)
}
