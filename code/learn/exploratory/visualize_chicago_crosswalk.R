# Visualize Census Tract Crosswalk Methodology
# Demonstrates area-reweighting harmonization to 1950 tract boundaries

library(tidyverse)
library(sf)
library(here)
library(patchwork)

# Create output directory
output_dir <- here("output", "figures", "crosswalk_visualization")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Configuration: which source year to use (1970 or 1980)
SOURCE_YEAR <- 1980

cat("Creating crosswalk visualizations for", SOURCE_YEAR, "-> 1950\n")

# ============================================================================
# LOAD DATA
# ============================================================================

# Load tract shapefiles
cat("Loading tract shapefiles...\n")

source_shp_path <- here("data", "raw", "nhgis", "gis",
                        paste0("nhgis0027_shapefile_tl2000_us_tract_", SOURCE_YEAR))
source_shp <- st_read(file.path(source_shp_path, paste0("US_tract_", SOURCE_YEAR, ".shp")),
                      quiet = TRUE)

target_shp_path <- here("data", "raw", "nhgis", "gis",
                        "nhgis0027_shapefile_tl2000_us_tract_1950")
target_shp <- st_read(file.path(target_shp_path, "US_tract_1950.shp"),
                      quiet = TRUE)

# Load crosswalk
crosswalk <- read_csv(
  here("data", "derived", "geographic_crosswalks",
       paste0("tract_concordance_weights", SOURCE_YEAR, "_to_1950.csv")),
  show_col_types = FALSE
)

# ============================================================================
# FILTER TO COOK COUNTY (CHICAGO)
# ============================================================================

cat("Filtering to Cook County, Chicago...\n")

# Cook County: Illinois (state 17), Cook County (031)
# Filter shapefiles to Cook County
source_cook <- source_shp %>%
  filter(str_starts(GISJOIN, "G17"), str_detect(GISJOIN, "031")) %>%
  st_make_valid()

target_cook <- target_shp %>%
  filter(str_starts(GISJOIN, "G17"), str_detect(GISJOIN, "031")) %>%
  st_make_valid()

cat("  Source year:", nrow(source_cook), "tracts\n")
cat("  Target year:", nrow(target_cook), "tracts\n")

# Focus on a smaller geographic area (South Side)
# Get bounding box for a subset of tracts
bbox_tracts <- source_cook %>%
  slice(50:75) %>%
  st_bbox()

source_subset <- source_cook %>%
  st_crop(bbox_tracts)

target_subset <- target_cook %>%
  st_crop(bbox_tracts)

cat("  Subset:", nrow(source_subset), "source tracts,",
    nrow(target_subset), "target tracts\n")

# ============================================================================
# FIGURE 1: BOUNDARY COMPARISON
# ============================================================================

cat("\nCreating Figure 1: Boundary comparison...\n")

# Panel A: Source year boundaries
p_source <- ggplot() +
  geom_sf(data = source_subset, fill = "lightblue", alpha = 0.5,
          color = "black", linewidth = 0.4) +
  labs(title = paste0(SOURCE_YEAR, " Census Tracts"),
       subtitle = "Source year boundaries") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Panel B: 1950 boundaries
p_target <- ggplot() +
  geom_sf(data = target_subset, fill = "coral", alpha = 0.5,
          color = "black", linewidth = 0.4) +
  labs(title = "1950 Census Tracts",
       subtitle = "Target year boundaries") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Combine panels
fig1 <- p_source + p_target +
  plot_annotation(
    title = "Census Tract Boundary Changes in Chicago",
    subtitle = paste("Demonstrating need for harmonization:", SOURCE_YEAR, "→ 1950"),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  )

# Save Figure 1 (PDF only)
ggsave(
  filename = file.path(output_dir, "chicago_crosswalk_boundaries.pdf"),
  plot = fig1,
  width = 12,
  height = 6,
  device = "pdf"
)

cat("  Saved boundary comparison figure\n")

# ============================================================================
# FIGURE 2: AREA-REWEIGHTING MECHANICS
# ============================================================================

# Function to create crosswalk methodology figure
make_crosswalk_figure <- function(example_gisjoin, suffix = "simple") {

  cat("  Creating", suffix, "example with tract:", example_gisjoin, "\n")

  # Get the source tract geometry
  example_source <- source_cook %>%
    filter(GISJOIN == example_gisjoin)

  # Get crosswalk entries for this tract
  example_crosswalk <- crosswalk %>%
    filter(get(paste0("GISJOIN_", SOURCE_YEAR)) == example_gisjoin)

  cat("    Splits into", nrow(example_crosswalk), "target tracts\n")
  cat("    Weights:", paste(sprintf("%.3f", example_crosswalk$weight), collapse = ", "), "\n")

  # Get the target tract geometries
  example_targets <- target_cook %>%
    filter(GISJOIN %in% example_crosswalk$GISJOIN_1950) %>%
    left_join(
      example_crosswalk,
      by = c("GISJOIN" = "GISJOIN_1950")
    ) %>%
    mutate(target_id = row_number())

  # Compute intersections
  example_intersections <- st_intersection(example_source, example_targets) %>%
    mutate(intersection_area = st_area(.))

  # Panel A: Source tract alone
  p_a <- ggplot() +
    geom_sf(data = example_source, fill = "lightblue", alpha = 0.7,
            color = "black", linewidth = 0.8) +
    labs(title = paste0("A. Source Tracts (", SOURCE_YEAR, ")")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    )

  # Panel B: Target tracts with overlay
  p_b <- ggplot() +
    geom_sf(data = example_source, fill = NA,
            color = "blue", linewidth = 1.2) +
    geom_sf(data = example_targets,
            aes(fill = factor(target_id)),
            alpha = 0.5, color = "black", linewidth = 0.8) +
    geom_sf_text(data = example_targets,
                 aes(label = paste0("Target ", target_id)),
                 size = 3.5, fontface = "bold") +
    scale_fill_brewer(palette = "Set2", name = "1950 Tracts") +
    labs(title = "B. Target Tracts (1950)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )

  # Panel C: Intersection pieces with weights
  p_c <- ggplot() +
    geom_sf(data = example_intersections,
            aes(fill = weight),
            alpha = 0.7, color = "black", linewidth = 0.5) +
    geom_sf_text(data = example_intersections,
                 aes(label = sprintf("Target %d\n%.2f", target_id, weight)),
                 size = 3.5, fontface = "bold", lineheight = 0.9) +
    scale_fill_viridis_c(option = "plasma", name = "Weight") +
    labs(title = "C. Area Weights") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )

  # Combine panels
  fig <- p_a + p_b + p_c

  # Save figure (PDF only)
  ggsave(
    filename = file.path(output_dir, paste0("chicago_crosswalk_weights_", suffix, ".pdf")),
    plot = fig,
    width = 15,
    height = 5,
    device = "pdf"
  )

  cat("    Saved chicago_crosswalk_weights_", suffix, ".pdf\n", sep = "")

  return(invisible(NULL))
}

cat("\nCreating Figure 2: Area-reweighting mechanics (simple and complex examples)...\n")

# Prepare crosswalk subset
crosswalk_subset <- crosswalk %>%
  filter(get(paste0("GISJOIN_", SOURCE_YEAR)) %in% source_subset$GISJOIN) %>%
  filter(GISJOIN_1950 %in% target_subset$GISJOIN)

# Find SIMPLE example: 2-way balanced split
simple_tracts <- crosswalk_subset %>%
  group_by(across(all_of(paste0("GISJOIN_", SOURCE_YEAR)))) %>%
  summarize(
    n_targets = n(),
    min_weight = min(weight),
    weight_sd = sd(weight),
    .groups = 'drop'
  ) %>%
  filter(n_targets == 2, min_weight > 0.10) %>%
  arrange(weight_sd)

if (nrow(simple_tracts) > 0) {
  simple_gisjoin <- simple_tracts[[paste0("GISJOIN_", SOURCE_YEAR)]][1]
} else {
  cat("  No ideal simple example found, using fallback\n")
  simple_gisjoin <- crosswalk_subset %>%
    group_by(across(all_of(paste0("GISJOIN_", SOURCE_YEAR)))) %>%
    summarize(n_targets = n(), .groups = 'drop') %>%
    filter(n_targets == 2) %>%
    slice(1) %>%
    pull(paste0("GISJOIN_", SOURCE_YEAR))
}

# Find COMPLEX example: 3-way split
complex_tracts <- crosswalk_subset %>%
  group_by(across(all_of(paste0("GISJOIN_", SOURCE_YEAR)))) %>%
  summarize(
    n_targets = n(),
    min_weight = min(weight),
    weight_sd = sd(weight),
    .groups = 'drop'
  ) %>%
  filter(n_targets == 3, min_weight > 0.15) %>%
  arrange(desc(weight_sd))

if (nrow(complex_tracts) > 0) {
  complex_gisjoin <- complex_tracts[[paste0("GISJOIN_", SOURCE_YEAR)]][1]
} else {
  cat("  No ideal complex example found, relaxing criteria\n")
  complex_tracts <- crosswalk_subset %>%
    group_by(across(all_of(paste0("GISJOIN_", SOURCE_YEAR)))) %>%
    summarize(
      n_targets = n(),
      min_weight = min(weight),
      .groups = 'drop'
    ) %>%
    filter(n_targets == 3, min_weight > 0.10) %>%
    slice(1)

  if (nrow(complex_tracts) > 0) {
    complex_gisjoin <- complex_tracts[[paste0("GISJOIN_", SOURCE_YEAR)]][1]
  } else {
    complex_gisjoin <- NULL
  }
}

# Generate simple example
make_crosswalk_figure(simple_gisjoin, "simple")

# Generate complex example if available
if (!is.null(complex_gisjoin)) {
  make_crosswalk_figure(complex_gisjoin, "complex")
} else {
  cat("  Skipping complex example (no suitable 3-way splits found)\n")
}

# ============================================================================
# FIGURE 3: MERGE PATTERN (Multiple sources → one target)
# ============================================================================

# Function to create merge pattern figure (multiple source tracts → one target tract)
make_merge_figure <- function(target_gisjoin, suffix = "merge") {

  cat("  Creating", suffix, "example with target tract:", target_gisjoin, "\n")

  # Get the target tract geometry (1950)
  target_tract <- target_cook %>%
    filter(GISJOIN == target_gisjoin)

  # Get crosswalk entries for this target tract
  target_crosswalk <- crosswalk %>%
    filter(GISJOIN_1950 == target_gisjoin)

  cat("    Receives from", nrow(target_crosswalk), "source tracts\n")
  cat("    Weights:", paste(sprintf("%.3f", target_crosswalk$weight), collapse = ", "), "\n")

  # Get the source tract geometries
  source_tracts <- source_cook %>%
    filter(GISJOIN %in% target_crosswalk[[paste0("GISJOIN_", SOURCE_YEAR)]]) %>%
    left_join(
      target_crosswalk,
      by = c("GISJOIN" = paste0("GISJOIN_", SOURCE_YEAR))
    ) %>%
    mutate(source_id = row_number())

  # Compute intersections
  merge_intersections <- st_intersection(source_tracts, target_tract) %>%
    mutate(intersection_area = st_area(.))

  # Panel A: Multiple source tracts
  p_a <- ggplot() +
    geom_sf(data = source_tracts,
            aes(fill = factor(source_id)),
            alpha = 0.6, color = "black", linewidth = 0.8) +
    geom_sf_text(data = source_tracts,
                 aes(label = paste0("Source ", source_id)),
                 size = 3.5, fontface = "bold") +
    scale_fill_brewer(palette = "Set1", name = paste0(SOURCE_YEAR, " Tracts")) +
    labs(title = paste0("A. Source Tracts (", SOURCE_YEAR, ")")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )

  # Panel B: Target tract with source overlays
  p_b <- ggplot() +
    geom_sf(data = source_tracts,
            aes(fill = factor(source_id)),
            alpha = 0.3, color = "gray50", linewidth = 0.5, linetype = "dashed") +
    geom_sf(data = target_tract, fill = NA,
            color = "red", linewidth = 1.2) +
    geom_sf_text(data = target_tract,
                 label = "1950 Target",
                 size = 4, fontface = "bold", color = "red") +
    scale_fill_brewer(palette = "Set1", name = paste0(SOURCE_YEAR, " Tracts")) +
    labs(title = "B. Target Tract (1950)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )

  # Panel C: Intersection pieces with weights
  p_c <- ggplot() +
    geom_sf(data = merge_intersections,
            aes(fill = weight),
            alpha = 0.7, color = "black", linewidth = 0.5) +
    geom_sf_text(data = merge_intersections,
                 aes(label = sprintf("Source %d\n%.2f", source_id, weight)),
                 size = 3.5, fontface = "bold", lineheight = 0.9) +
    scale_fill_viridis_c(option = "plasma", name = "Weight") +
    labs(title = "C. Area Weights") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )

  # Combine panels
  fig <- p_a + p_b + p_c

  # Save figure (PDF only)
  ggsave(
    filename = file.path(output_dir, paste0("chicago_crosswalk_weights_", suffix, ".pdf")),
    plot = fig,
    width = 15,
    height = 5,
    device = "pdf"
  )

  cat("    Saved chicago_crosswalk_weights_", suffix, ".pdf\n", sep = "")

  return(invisible(NULL))
}

cat("\nCreating Figure 3: Merge pattern (multiple sources → one target)...\n")

# Find MERGE example: one 1950 target that receives from 2-3 source tracts
merge_targets <- crosswalk_subset %>%
  group_by(GISJOIN_1950) %>%
  summarize(
    n_sources = n(),
    min_weight = min(weight),
    weight_sd = sd(weight),
    .groups = 'drop'
  ) %>%
  filter(n_sources >= 2, n_sources <= 3, min_weight > 0.15) %>%
  arrange(desc(n_sources), weight_sd)

if (nrow(merge_targets) > 0) {
  merge_target_gisjoin <- merge_targets$GISJOIN_1950[1]
  make_merge_figure(merge_target_gisjoin, "merge")
} else {
  cat("  No suitable merge example found (relaxing criteria)\n")
  merge_targets_relaxed <- crosswalk_subset %>%
    group_by(GISJOIN_1950) %>%
    summarize(n_sources = n(), min_weight = min(weight), .groups = 'drop') %>%
    filter(n_sources >= 2, min_weight > 0.10) %>%
    arrange(desc(n_sources))

  if (nrow(merge_targets_relaxed) > 0) {
    merge_target_gisjoin <- merge_targets_relaxed$GISJOIN_1950[1]
    make_merge_figure(merge_target_gisjoin, "merge")
  } else {
    cat("  Skipping merge example (no suitable cases found)\n")
    merge_target_gisjoin <- NULL
  }
}

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n=== VISUALIZATION COMPLETE ===\n")
cat("Output directory:", output_dir, "\n")
cat("\nGenerated files:\n")
cat("  - chicago_crosswalk_boundaries.pdf\n")
cat("  - chicago_crosswalk_weights_simple.pdf (1 source → 2 targets)\n")
if (!is.null(complex_gisjoin)) {
  cat("  - chicago_crosswalk_weights_complex.pdf (1 source → 3 targets)\n")
}
if (exists("merge_target_gisjoin") && !is.null(merge_target_gisjoin)) {
  cat("  - chicago_crosswalk_weights_merge.pdf (multiple sources → 1 target)\n")
}
cat("\n")
