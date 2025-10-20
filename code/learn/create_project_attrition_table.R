# Create public housing project attrition table
# Shows sample selection steps for projects

library(tidyverse)
library(here)
library(tinytable)
library(haven)

# Read original CDD data
cdd_original <- read_dta("data/raw/hud_consolidated_directory/CDD_1973.dta")

# Read merged CDD data (CDD + HUD lat/long)
cdd_merged <- read_csv(here("data", "derived", "public_housing", "working",
                            "merged_cdd_projects.csv"))

# Read combined public housing dataset
combined_data <- read_csv(here("data", "derived", "public_housing", "working",
                               "combined_cdd_and_digitized_projects.csv"))

# Step 1: Original CDD sample
step1 <- cdd_original %>%
  summarise(
    step = "Original CDD sample",
    projects = n(),
    units = sum(totunitsplanned, na.rm = TRUE)
  )

# Step 2: Geolocated (combined CDD + digitized)
step2 <- combined_data %>%
  summarise(
    step = "Geolocated projects",
    projects = n(),
    units = sum(total_units, na.rm = TRUE)
  )

# Step 3: Built 1941-1973 only
step3 <- combined_data %>%
  filter(!is.na(year_completed), year_completed >= 1941, year_completed <= 1973) %>%
  summarise(
    step = "Built 1941-1973 only",
    projects = n(),
    units = sum(total_units, na.rm = TRUE)
  )

# Step 4: Minimum 50 units
step4 <- combined_data %>%
  filter(!is.na(year_completed), year_completed >= 1941, year_completed <= 1973) %>%
  filter(total_units >= 50) %>%
  summarise(
    step = "Minimum 50 units",
    projects = n(),
    units = sum(total_units, na.rm = TRUE)
  )

# Step 5: With population/race data
step5 <- combined_data %>%
  filter(!is.na(year_completed), year_completed >= 1941, year_completed <= 1973) %>%
  filter(total_units >= 50) %>%
  filter(!is.na(proj_black_population_estimate), !is.na(proj_white_population_estimate)) %>%
  summarise(
    step = "With population/race data",
    projects = n(),
    units = sum(total_units, na.rm = TRUE)
  )

# Panel A: All projects
panel_a <- bind_rows(step1, step2, step3, step4, step5) %>%
  mutate(
    projects_lost_pct = round((1 - projects / first(projects)) * 100, 1),
    units_lost_pct = round((1 - units / first(units)) * 100, 1)
  ) %>%
  mutate(
    projects = format(projects, big.mark = ","),
    units = format(units, big.mark = ",")
  ) %>%
  select(
    Step = step,
    Projects = projects,
    Units = units,
    `Projects Lost (\\%)` = projects_lost_pct,
    `Units Lost (\\%)` = units_lost_pct
  ) %>%
  mutate(Step = paste0("  ", Step))  # Indent for panel

# Panel B: Projects >= 50 units
# Step 1: Original CDD >= 50 units
panel_b_step1 <- cdd_original %>%
  filter(totunitsplanned >= 50) %>%
  summarise(
    step = "  Original CDD sample ($\\geq$50 units)",
    projects = n(),
    units = sum(totunitsplanned, na.rm = TRUE)
  )

# Step 2: Geolocated >= 50 units
panel_b_step2 <- combined_data %>%
  filter(total_units >= 50) %>%
  summarise(
    step = "  Geolocated projects ($\\geq$50 units)",
    projects = n(),
    units = sum(total_units, na.rm = TRUE)
  )

panel_b <- bind_rows(panel_b_step1, panel_b_step2) %>%
  mutate(
    projects_lost_pct = round((1 - projects / first(projects)) * 100, 1),
    units_lost_pct = round((1 - units / first(units)) * 100, 1)
  ) %>%
  mutate(
    projects = format(projects, big.mark = ","),
    units = format(units, big.mark = ",")
  ) %>%
  select(
    Step = step,
    Projects = projects,
    Units = units,
    `Projects Lost (\\%)` = projects_lost_pct,
    `Units Lost (\\%)` = units_lost_pct
  )

# Combine panels with headers
attrition_table <- bind_rows(
  tibble(Step = "Panel A: All Projects", Projects = "", Units = "", `Projects Lost (\\%)` = NA_real_, `Units Lost (\\%)` = NA_real_),
  panel_a,
  tibble(Step = "", Projects = "", Units = "", `Projects Lost (\\%)` = NA_real_, `Units Lost (\\%)` = NA_real_),
  tibble(Step = "Panel B: Projects $\\geq$50 Units", Projects = "", Units = "", `Projects Lost (\\%)` = NA_real_, `Units Lost (\\%)` = NA_real_),
  panel_b
)

# Create tinytable
project_table <- tt(
  attrition_table,
  caption = "Public Housing Project Sample Selection\\label{tab:project_attrition}",
  notes = "\\footnotesize \\textit{Notes:} Sample selection criteria applied to combined CDD and digitized public housing dataset. Panel A shows all projects; Panel B restricts to projects with 50+ units from the start."
) |>
  style_tt(i = c(0, 7), bold = TRUE) |>
  format_tt(escape = FALSE) |>
  theme_tt(theme = "tabular")

# Save to file
save_tt(
  project_table,
  here("output", "tables", "project_attrition_table.tex"),
  overwrite = TRUE
)

# Create slides version without notes
project_table_slides <- tt(
  attrition_table
) |>
  style_tt(i = c(0, 7), bold = TRUE) |>
  format_tt(escape = FALSE) |>
  theme_tt(theme = "tabular")

# Save slides version
save_tt(
  project_table_slides,
  here("output", "tables", "slides", "project_attrition_table.tex"),
  overwrite = TRUE
)

cat("Project attrition table saved to output/tables/project_attrition_table.tex\n")
cat("Slides version saved to output/tables/slides/project_attrition_table.tex\n")
