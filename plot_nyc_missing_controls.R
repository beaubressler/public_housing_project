library(tidyverse)
library(sf)
library(ggplot2)
library(here)

# Load data
merged_data_dir <- here("data", "derived", "merged", "combined")
event_data <- read_csv(here(merged_data_dir, "event_study_data_rings.csv"))

# Find treated_ids missing controls
treated_ids_with_controls <- event_data %>%
  filter(location_type == "outer") %>%
  distinct(treated_id) %>%
  pull(treated_id)

# Get NYC project data
nyc_projects <- event_data %>%
  filter(city == "New York City", location_type == "treated", year == treatment_year) %>%
  distinct(treated_id, COUNTY, treatment_year, total_public_housing_units) %>%
  mutate(
    has_controls = ifelse(treated_id %in% treated_ids_with_controls, "Has Controls", "Missing Controls"),
    borough = case_when(
      COUNTY == "New York" ~ "Manhattan",
      COUNTY == "Bronx" ~ "Bronx", 
      COUNTY == "Kings" ~ "Brooklyn",
      COUNTY == "Queens" ~ "Queens",
      COUNTY == "Richmond" ~ "Staten Island",
      TRUE ~ COUNTY
    )
  )

# Load census tract data to get tract geometries for spatial context
census_tracts <- st_read(here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")) %>%
  filter(city == "New York City", YEAR == 1990) %>%
  mutate(
    borough = case_when(
      COUNTY == "New York" ~ "Manhattan",
      COUNTY == "Bronx" ~ "Bronx", 
      COUNTY == "Kings" ~ "Brooklyn",
      COUNTY == "Queens" ~ "Queens",
      COUNTY == "Richmond" ~ "Staten Island",
      TRUE ~ COUNTY
    )
  )

# Get treated tract locations for mapping
treated_tracts_raw <- st_read(here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")) %>%
  filter(city == "New York City")

# Check column names and create join
cat("Treated tracts columns:", paste(names(treated_tracts_raw), collapse = ", "), "\n")
cat("NYC projects columns:", paste(names(nyc_projects), collapse = ", "), "\n")

# Create treated tracts with control status
treated_tracts <- treated_tracts_raw %>%
  left_join(nyc_projects %>% select(treated_id, has_controls), 
            by = "treated_id") %>%
  filter(!is.na(has_controls)) %>%
  mutate(
    borough = case_when(
      COUNTY == "New York" ~ "Manhattan",
      COUNTY == "Bronx" ~ "Bronx", 
      COUNTY == "Kings" ~ "Brooklyn",
      COUNTY == "Queens" ~ "Queens",
      COUNTY == "Richmond" ~ "Staten Island",
      TRUE ~ COUNTY
    )
  )

# Create the map
nyc_map <- ggplot() +
  # Background tracts
  geom_sf(data = census_tracts, fill = "lightgray", color = "white", size = 0.1) +
  # Treated tracts colored by control status
  geom_sf(data = treated_tracts, 
          aes(fill = has_controls), 
          color = "white", size = 0.2, alpha = 0.8) +
  scale_fill_manual(
    values = c("Has Controls" = "blue", "Missing Controls" = "red"),
    name = "Control Status"
  ) +
  facet_wrap(~ borough, scales = "free") +
  labs(
    title = "NYC Public Housing Projects: Missing vs Has Outer Ring Controls",
    subtitle = "Contiguity-Based Spatial DiD - Treated Tracts Highlighted",
    caption = "Red = Missing outer ring controls, Blue = Has outer ring controls\nEach colored area represents a census tract with public housing"
  ) +
  theme_void() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

print(nyc_map)

# Summary statistics
cat("\n=== NYC CONTROL COVERAGE SUMMARY ===\n")
summary_stats <- nyc_projects %>%
  group_by(borough) %>%
  summarise(
    total_projects = n(),
    missing_controls = sum(has_controls == "Missing Controls"),
    pct_missing = round(missing_controls / total_projects * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_missing))

print(summary_stats)

# Borough breakdown plot
borough_plot <- ggplot(nyc_projects) +
  geom_bar(aes(x = reorder(borough, -table(borough)[borough]), fill = has_controls), 
           position = "dodge") +
  scale_fill_manual(
    values = c("Has Controls" = "blue", "Missing Controls" = "red"),
    name = "Control Status"
  ) +
  labs(
    title = "NYC Public Housing Projects by Borough and Control Status",
    x = "Borough", y = "Number of Projects"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold")
  )

print(borough_plot)

# Save plots
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)
ggsave(here("output", "figures", "nyc_missing_controls_map.pdf"), 
       nyc_map, width = 14, height = 10)
ggsave(here("output", "figures", "nyc_missing_controls_borough.pdf"), 
       borough_plot, width = 10, height = 6)

cat("\nPlots saved to output/figures/\n")