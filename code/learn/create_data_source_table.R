# Create table showing public housing dataset construction by data source
# Shows how projects are geolocated from different HUD sources and final dataset composition

library(tidyverse)
library(here)
library(tinytable)

# Read data
merged_cdd <- read_csv(here("data", "derived", "public_housing", "working", "merged_cdd_projects.csv"))
merged_cdd_non_collapsed <- read_csv(here("data", "derived", "public_housing", "working", "merged_cdd_projects_non_collapsed.csv"))
combined_data <- read_csv(here("data", "derived", "public_housing", "working", "combined_cdd_and_digitized_projects.csv"))

# Read original CDD for baseline
cdd_original <- haven::read_dta(here("data", "raw", "hud_consolidated_directory", "CDD_1973.dta"))

# Panel A: CDD Projects by Geolocation Source ----

# Step 1: Original CDD sample
panel_a_step1 <- cdd_original %>%
  summarise(
    step = "Original CDD sample (1973)",
    projects = n(),
    units = sum(totunitsplanned, na.rm = TRUE)
  )

# Step 2: Breakdown by geolocation source (simplified - no full/partial distinction)
panel_a_breakdown <- merged_cdd %>%
  group_by(source) %>%
  summarise(
    projects = n(),
    units = sum(totunits, na.rm = TRUE)
  ) %>%
  mutate(step = case_when(
    source == "psh2000" ~ "  Geolocated via PSH 2000",
    source == "hud951" ~ "  Geolocated via HUD 951",
    source == "psh1997" ~ "  Geolocated via PSH 1997",
    source == "ngda" ~ "  Geolocated via NGDA",
    source == "manual" ~ "  Manual fixes",
    TRUE ~ paste0("  ", source)
  )) %>%
  select(step, projects, units)

# Step 3: Total geolocated
panel_a_total <- merged_cdd %>%
  filter(!is.na(latitude)) %>%
  summarise(
    step = "Total CDD projects geolocated",
    projects = n(),
    units = sum(totunits, na.rm = TRUE)
  )

# Combine Panel A
panel_a <- bind_rows(
  panel_a_step1,
  panel_a_breakdown,
  panel_a_total
) %>%
  mutate(
    pct_projects = round(projects / panel_a_step1$projects * 100, 1),
    pct_units = round(units / panel_a_step1$units * 100, 1)
  ) %>%
  mutate(
    projects = format(projects, big.mark = ","),
    units = format(units, big.mark = ","),
    pct_projects = ifelse(row_number() == 1, "", as.character(pct_projects)),
    pct_units = ifelse(row_number() == 1, "", as.character(pct_units))
  ) %>%
  select(
    Step = step,
    Projects = projects,
    Units = units,
    `Projects (\\%)` = pct_projects,
    `Units (\\%)` = pct_units
  )

# Panel B: Final Dataset Composition ----

# Categorize sources in combined data
combined_categorized <- combined_data %>%
  mutate(dataset_source = case_when(
    source == "CDD" ~ "CDD-HUD only",
    source == "Digitized" ~ "Digitized only",
    source == "CDD + Digitized" ~ "CDD + Digitized supplement",
    TRUE ~ "Other/Manual"
  ))

# Summary by dataset source
panel_b <- combined_categorized %>%
  group_by(dataset_source) %>%
  summarise(
    projects = n(),
    units = sum(total_units, na.rm = TRUE)
  ) %>%
  bind_rows(
    combined_categorized %>%
      summarise(
        dataset_source = "Total final dataset",
        projects = n(),
        units = sum(total_units, na.rm = TRUE)
      )
  ) %>%
  mutate(
    pct_projects = round(projects / last(projects) * 100, 1),
    pct_units = round(units / last(units) * 100, 1)
  ) %>%
  mutate(
    step = paste0("  ", dataset_source),
    projects = format(projects, big.mark = ","),
    units = format(units, big.mark = ","),
    pct_projects = ifelse(row_number() == n(), "", as.character(pct_projects)),
    pct_units = ifelse(row_number() == n(), "", as.character(pct_units))
  ) %>%
  select(
    Step = step,
    Projects = projects,
    Units = units,
    `Projects (\\%)` = pct_projects,
    `Units (\\%)` = pct_units
  )

# Combine panels with headers ----
data_source_table <- bind_rows(
  tibble(Step = "Panel A: CDD Geolocation by Source", Projects = "", Units = "", `Projects (\\%)` = "", `Units (\\%)` = ""),
  panel_a,
  tibble(Step = "", Projects = "", Units = "", `Projects (\\%)` = "", `Units (\\%)` = ""),
  tibble(Step = "Panel B: Final Dataset Composition", Projects = "", Units = "", `Projects (\\%)` = "", `Units (\\%)` = ""),
  panel_b
)

# Create tinytable
source_table <- tt(
  data_source_table,
  caption = "Public Housing Dataset Construction by Data Source\\label{tab:data_source}",
  notes = "\\footnotesize \\textit{Notes:} Panel A shows how CDD projects (with construction dates but no locations) were geolocated by matching to various HUD datasets via project codes. Matching algorithm: first attempts to match on full project codes (including site letter suffixes like A/B/C), then falls back to partial codes for unmatched projects. Priority order for location: PSH 2000 > HUD 951 > PSH 1997 > NGDA. Panel B shows the composition of the final dataset, combining CDD-HUD data with hand-digitized projects from city annual reports and FOIA requests."
) %>%
  style_tt(i = c(2, nrow(panel_a) + 4), bold = TRUE) %>%
  format_tt(escape = FALSE) %>%
  theme_tt("resize")

# Save table
save_tt(
  source_table,
  here("output", "tables", "data_source_table.tex"),
  overwrite = TRUE
)

cat("Data source table saved to output/tables/data_source_table.tex\n")
