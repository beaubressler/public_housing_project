# Create Chicago map showing spatial DiD ring structure
# Shows all 3 rings: treated, inner, and outer for spatial DiD appendix

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

# Get Chicago tracts ----
chicago_tracts <- census_tract_sample_indexed_unique %>%
  filter(city == "Chicago")

# Get ring assignments for Chicago from spatial DiD data ----
event_study_rings <- read_csv(here(merged_data_dir, "event_study_data_rings.csv"))

chicago_rings <- event_study_rings %>%
  filter(cbsa_title == "Chicago-Naperville-Joliet, IL-IN-WI") %>%
  select(GISJOIN_1950, location_type) %>%
  distinct()

# Merge with tract geometries - include all Chicago tracts ----
chicago_rings_with_geom <- chicago_tracts %>%
  left_join(chicago_rings, by = "GISJOIN_1950") %>%
  mutate(location_type = ifelse(is.na(location_type), "other", location_type)) %>%
  # Create display category - show all 3 rings
  mutate(display_category = case_when(
    location_type == "treated" ~ "Treated",
    location_type == "inner" ~ "Inner Ring",
    location_type == "outer" ~ "Outer Ring",
    TRUE ~ "Other Areas"
  )) %>%
  mutate(display_category = factor(display_category,
                                   levels = c("Treated", "Inner Ring", "Outer Ring", "Other Areas")))

# Create map ----
chicago_spatial_did_map <- ggplot() +
  geom_sf(data = chicago_rings_with_geom,
          aes(fill = display_category),
          color = "white",
          size = 0.15) +
  scale_fill_manual(
    values = c(
      "Treated" = "#d73027",
      "Inner Ring" = "#fc8d59",
      "Outer Ring" = "#91bfdb",
      "Other Areas" = "gray90"
    ),
    breaks = c("Treated", "Inner Ring", "Outer Ring"),
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
    legend.position = "right",
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 14),
    legend.margin = margin(l = 20),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE, override.aes = list(color = NA)))

# Save PDF versions ----
# Create output directories
dir.create(here("output", "figures", "stacked_did", data_type),
           showWarnings = FALSE, recursive = TRUE)
dir.create(here("output", "figures", "slides"),
           showWarnings = FALSE, recursive = TRUE)

# Main version
ggsave(here("output", "figures", "stacked_did", data_type, "chicago_spatial_did_rings.pdf"),
       chicago_spatial_did_map,
       width = 10, height = 8,
       dpi = 300, device = "pdf")

# Slide version (also save to slides directory for easy access)
ggsave(here("output", "figures", "slides", "chicago_spatial_did_rings.pdf"),
       chicago_spatial_did_map,
       width = 10, height = 8,
       dpi = 300, device = "pdf")

cat("Chicago spatial DiD ring map saved:\n")
cat("- stacked_did/", data_type, "/chicago_spatial_did_rings.pdf\n", sep = "")
cat("- slides/chicago_spatial_did_rings.pdf\n")
cat("Shows all 3 rings: Treated (red), Inner Ring (orange), Outer Ring (blue)\n")
