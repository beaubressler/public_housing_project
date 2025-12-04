# Create publication-quality Chicago spillover map
# Shows all Chicago census tracts with treated and inner rings labeled

# Preliminaries ----
library(here)
library(tidyverse)
library(sf)

# Set data type
data_type <- "combined"
merged_data_dir <- here("data", "derived", "merged", data_type)

# Read data ----
unique_tracts_and_rings <- read_csv(here(merged_data_dir, "unique_tracts_and_rings.csv"))
census_tract_sample_indexed_unique <- read_sf(here(merged_data_dir, "census_tract_sample_with_treatment_status_all.gpkg")) %>%
  group_by(TRACTA, COUNTYA, STATEA) %>%
  filter(row_number() == 1) %>%
  mutate(tract_id = row_number())

# Read Chicago public housing project locations
# Filter to only projects whose tracts are in the analysis (treated_tracts_panel_balanced)
# This ensures map consistency with actual analysis sample

# Get list of treated tracts in the analysis
treated_tracts_panel <- read_sf(here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")) %>%
  filter(city == "Chicago") %>%
  st_drop_geometry() %>%
  distinct(GISJOIN_1950)

# Read all projects in balanced sample
all_chicago_projects <- read_sf(here("data", "derived", "public_housing", "working", data_type, "public_housing_sample_balanced.gpkg")) %>%
  filter(locality == "CHICAGO")

# Spatial join to get tract for each project
chicago_projects_with_tracts <- st_join(
  all_chicago_projects,
  census_tract_sample_indexed_unique %>% select(GISJOIN_1950, geom),
  join = st_within
)

# Filter to only projects in tracts that are actually in the analysis
chicago_projects <- chicago_projects_with_tracts %>%
  filter(GISJOIN_1950 %in% treated_tracts_panel$GISJOIN_1950) %>%
  select(all_of(names(all_chicago_projects)))

# Get Chicago tracts ----
chicago_tracts <- census_tract_sample_indexed_unique %>%
  filter(city == "Chicago")

# Get ring assignments for Chicago ----
chicago_rings <- unique_tracts_and_rings %>%
  inner_join(chicago_tracts %>% st_drop_geometry() %>% select(GISJOIN_1950, city),
             by = "GISJOIN_1950")

# Merge with tract geometries - include all Chicago tracts ----
chicago_rings_with_geom <- chicago_tracts %>%
  left_join(chicago_rings, by = "GISJOIN_1950") %>%
  mutate(location_type = ifelse(is.na(location_type), "other", location_type)) %>%
  # Create display category - only label treated and inner
  mutate(display_category = case_when(
    location_type == "treated" ~ "Treated",
    location_type == "inner" ~ "Nearby",
    TRUE ~ "Other Areas"
  ))

# Create publication-quality map ----
chicago_spillover_map <- ggplot() +
  geom_sf(data = chicago_rings_with_geom,
          aes(fill = display_category),
          color = "white",
          size = 0.15) +
  geom_sf(data = chicago_projects,
          color = "black",
          size = 0.9,
          shape = 16,
          show.legend = FALSE) +
  scale_fill_manual(
    values = c(
      "Treated" = "#d73027",
      "Nearby" = "#4575b4",
      "Other Areas" = "gray"
    ),
    breaks = c("Treated", "Nearby"),
    name = ""
  ) +
  labs(
    title = NULL,
    subtitle = NULL
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40", margin = margin(b = 20)),
    legend.position = "right",
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 14),
    legend.margin = margin(l = 20),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE, override.aes = list(color = NA)))

# Save high-quality versions ----
ggsave(here("output", "figures", "chicago_spillover_all_tracts.pdf"),
       chicago_spillover_map,
       width = 10, height = 8,
       dpi = 300, device = "pdf")

# Also create a version without subtitle for cleaner look ----
chicago_spillover_clean <- chicago_spillover_map +
  labs(subtitle = NULL) +
  theme(plot.title = element_text(margin = margin(b = 30)))

ggsave(here("output", "figures", "chicago_spillover_all_tracts_clean.pdf"),
       chicago_spillover_clean,
       width = 10, height = 8,
       dpi = 300, device = "pdf")

# Save slide versions (in slides subdirectory) - NO TITLE for slides
dir.create(here("output", "figures", "slides"), showWarnings = FALSE, recursive = TRUE)

chicago_spillover_no_title <- chicago_spillover_map +
  labs(title = NULL, subtitle = NULL) +
  theme(plot.margin = margin(10, 20, 20, 20))

ggsave(here("output", "figures", "slides", "chicago_spillover_all_tracts_clean.pdf"),
       chicago_spillover_no_title,
       width = 10, height = 8,
       dpi = 300, device = "pdf")

cat("Publication-quality Chicago spillover maps with all tracts saved:\n")
cat("- chicago_spillover_all_tracts.pdf (with subtitle)\n")
cat("- chicago_spillover_all_tracts_clean.pdf (title only)\n")
cat("- chicago_spillover_all_tracts_no_title.pdf (no title - for slides)\n")
cat("- Slide versions saved to slides/ subdirectory\n")
cat("Shows all Chicago tracts with only treated and spillover areas labeled\n")
