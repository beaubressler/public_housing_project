# Create Chicago HOLC redlining maps for presentation
# Map 1: Raw HOLC grades (A, B, C, D)
# Map 2: Binary 80pp classification

# Preliminaries ----
library(here)
library(tidyverse)
library(sf)

# Set data type
data_type <- "combined"
merged_data_dir <- here("data", "derived", "merged", data_type)

# Read data ----
census_tracts <- st_read(here(merged_data_dir, "census_tract_sample_with_treatment_status_all.gpkg"))

# Read original HOLC boundaries
holc_raw <- st_read(here("data", "raw", "mapping_inequality", "mappinginequality.gpkg"))

# Get all Chicago tracts from 1950 ----
chicago_tracts <- census_tracts %>%
  filter(city == "Chicago", YEAR == 1950) %>%
  select(GISJOIN_1950, redlined_binary_80pp, geom)

# Get original Chicago HOLC boundaries ----
chicago_holc <- holc_raw %>%
  filter(city == "Chicago") %>%
  st_transform(st_crs(chicago_tracts)) %>%
  st_make_valid()

# Create output directory ----
dir.create(here("output", "figures", "slides"), showWarnings = FALSE, recursive = TRUE)

# Map 1: Original HOLC Boundaries overlaid on Census Tracts ----
# Traditional HOLC colors
holc_colors <- c(
  "A" = "#76a865",      # Green (Best)
  "B" = "#7cb5d2",      # Blue (Still Desirable)
  "C" = "#ffff00",      # Yellow (Definitely Declining)
  "D" = "#d9544d",      # Red (Hazardous)
  "Commercial" = "gray40",
  "Industrial" = "gray20"
)

# Create factor with proper ordering for HOLC polygons
chicago_holc <- chicago_holc %>%
  mutate(grade_display = case_when(
    category == "Commercial" ~ "Commercial",
    category == "Industrial" ~ "Industrial",
    TRUE ~ as.character(grade)
  ),
  grade_display = factor(grade_display,
                        levels = c("A", "B", "C", "D", "Commercial", "Industrial")))

# Plot original HOLC boundaries over census tracts
chicago_holc_raw <- ggplot() +
  # First layer: Census tract boundaries (just outlines)
  geom_sf(data = chicago_tracts,
          fill = "gray95",
          color = "gray70",
          size = 0.3) +
  # Second layer: Original HOLC polygons
  geom_sf(data = chicago_holc,
          aes(fill = grade_display),
          color = "white",
          size = 0.2,
          alpha = 0.8) +
  scale_fill_manual(
    values = holc_colors,
    breaks = c("A", "B", "C", "D"),
    labels = c("A (Best)", "B (Still Desirable)",
               "C (Definitely Declining)", "D (Hazardous)"),
    name = "",
    na.value = "gray90"
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
  guides(fill = guide_legend(ncol = 1, byrow = TRUE, override.aes = list(color = NA, alpha = 1)))

# Save Map 1
ggsave(here("output", "figures", "slides", "chicago_holc_raw_grades.pdf"),
       chicago_holc_raw,
       width = 10, height = 8,
       dpi = 300, device = "pdf")

cat("Map 1 saved: chicago_holc_raw_grades.pdf\n")

# Map 2: 80pp Binary Classification ----
chicago_tracts <- chicago_tracts %>%
  mutate(redlined_80pp_display = factor(
    ifelse(redlined_binary_80pp == 1, "Redlined (≥80% Grade D)", "Not Redlined"),
    levels = c("Redlined (≥80% Grade D)", "Not Redlined")
  ))

# Plot binary classification
chicago_holc_80pp <- ggplot() +
  geom_sf(data = chicago_tracts,
          aes(fill = redlined_80pp_display),
          color = "white",
          size = 0.15) +
  scale_fill_manual(
    values = c(
      "Redlined (≥80% Grade D)" = "#d73027",
      "Not Redlined" = "gray85"
    ),
    name = ""
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

# Save Map 2
ggsave(here("output", "figures", "slides", "chicago_holc_80pp_binary.pdf"),
       chicago_holc_80pp,
       width = 10, height = 8,
       dpi = 300, device = "pdf")

cat("Map 2 saved: chicago_holc_80pp_binary.pdf\n")

# Summary ----
cat("\nChicago HOLC maps created successfully:\n")
cat("- chicago_holc_raw_grades.pdf: Shows original HOLC boundaries overlaid on census tracts\n")
cat(sprintf("  - %d original HOLC polygons (A: %d, B: %d, C: %d, D: %d)\n",
            nrow(chicago_holc),
            sum(chicago_holc$grade == "A", na.rm = TRUE),
            sum(chicago_holc$grade == "B", na.rm = TRUE),
            sum(chicago_holc$grade == "C", na.rm = TRUE),
            sum(chicago_holc$grade == "D", na.rm = TRUE)))
cat("- chicago_holc_80pp_binary.pdf: Shows 80pp binary classification at tract level\n")
cat(sprintf("  - %d tracts classified as redlined (≥80%% Grade D)\n",
            sum(chicago_tracts$redlined_binary_80pp == 1, na.rm = TRUE)))
cat(sprintf("  - %d tracts not redlined\n",
            sum(chicago_tracts$redlined_binary_80pp == 0, na.rm = TRUE)))
