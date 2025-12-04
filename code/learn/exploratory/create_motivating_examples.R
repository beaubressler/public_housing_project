# Create motivating examples: Hunter's Point and Brownsville trajectories
# This script identifies the census tracts containing these iconic projects
# and visualizes their demographic and economic trajectories

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# Load color palettes
source(here("code", "helpers", "color_palettes.R"))

# Load public housing project data
ph_projects <- st_read(here("data", "derived", "public_housing", "working",
                            "cleaned_housing_projects.gpkg"))

# Load census tract panel data
tract_data <- st_read(here("data", "derived", "merged", "combined",
                          "census_tract_sample_with_treatment_status_all.gpkg"))

# ============================================================================
# 1. Identify Hunter's Point tracts (San Francisco)
# ============================================================================

hunters_point <- ph_projects %>%
  filter(grepl("Hunters Point|Hunters View", project_name, ignore.case = TRUE),
         grepl("San Francisco", locality, ignore.case = TRUE))

cat("Hunter's Point projects found:\n")
print(hunters_point %>% st_drop_geometry() %>%
        select(project_name, total_public_housing_units, year_completed))

# Find which tracts contain Hunter's Point
# First save project info before join
hunters_info <- hunters_point %>%
  st_drop_geometry() %>%
  select(project_name, total_public_housing_units, year_completed)

hunters_tracts <- st_join(hunters_point, tract_data) %>%
  st_drop_geometry() %>%
  select(GISJOIN_1950) %>%
  distinct(GISJOIN_1950) %>%
  mutate(neighborhood = "Hunter's Point")

cat("\nHunter's Point project info:\n")
print(hunters_info)

cat("\nHunter's Point census tracts:\n")
print(hunters_tracts)

# ============================================================================
# 2. Identify Brownsville tracts (Brooklyn)
# ============================================================================

brownsville <- ph_projects %>%
  filter(grepl("Van Dyke|Samuel J. Tilden|Howard|Langston Hughes|Glenmore Plaza|Pennsylvania Ave",
               project_name, ignore.case = TRUE),
         grepl("New York", locality, ignore.case = TRUE))

cat("\n\nBrownsville projects found:\n")
print(brownsville %>% st_drop_geometry() %>%
        arrange(year_completed) %>%
        select(project_name, total_public_housing_units, year_completed))

# Find which tracts contain Brownsville projects
# First save project info before join
brownsville_info <- brownsville %>%
  st_drop_geometry() %>%
  select(project_name, total_public_housing_units, year_completed)

brownsville_tracts <- st_join(brownsville, tract_data) %>%
  st_drop_geometry() %>%
  select(GISJOIN_1950) %>%
  distinct(GISJOIN_1950) %>%
  mutate(neighborhood = "Brownsville")

cat("\nBrownsville project info:\n")
print(brownsville_info)

cat("\nBrownsville census tracts:\n")
print(brownsville_tracts)

# ============================================================================
# 3. Extract trajectories for these tracts
# ============================================================================

# Get unique tract IDs (remove NAs)
hunters_tract_ids <- unique(hunters_tracts$GISJOIN_1950)
hunters_tract_ids <- hunters_tract_ids[!is.na(hunters_tract_ids)]

brownsville_tract_ids <- unique(brownsville_tracts$GISJOIN_1950)
brownsville_tract_ids <- brownsville_tract_ids[!is.na(brownsville_tract_ids)]

# Extract panel data for these tracts
trajectory_data <- tract_data %>%
  st_drop_geometry() %>%
  filter(GISJOIN_1950 %in% c(hunters_tract_ids, brownsville_tract_ids)) %>%
  mutate(
    neighborhood = case_when(
      GISJOIN_1950 %in% hunters_tract_ids ~ "Hunter's Point, SF",
      GISJOIN_1950 %in% brownsville_tract_ids ~ "Brownsville, Brooklyn",
      TRUE ~ NA_character_
    )
  ) %>%
  select(GISJOIN_1950, neighborhood, year = YEAR,
         black_share, white_share,
         total_pop, median_income, median_rent_calculated) %>%
  # Rename for clarity
  rename(pop_total = total_pop)

# Calculate neighborhood-level averages (across tracts in each neighborhood)
neighborhood_trajectories <- trajectory_data %>%
  group_by(neighborhood, year) %>%
  summarize(
    black_share = mean(black_share, na.rm = TRUE),
    white_share = mean(white_share, na.rm = TRUE),
    median_income = mean(median_income, na.rm = TRUE),
    median_rent = mean(median_rent_calculated, na.rm = TRUE),
    pop_total = sum(pop_total, na.rm = TRUE),
    .groups = "drop"
  )

# Mark treatment years (earliest project in each neighborhood)
hunters_treatment_year <- min(hunters_info$year_completed, na.rm = TRUE)
brownsville_treatment_year <- min(brownsville_info$year_completed, na.rm = TRUE)

cat("\n\nTreatment years:\n")
cat(sprintf("Hunter's Point: %d\n", hunters_treatment_year))
cat(sprintf("Brownsville: %d\n", brownsville_treatment_year))

# ============================================================================
# 4. Create visualizations
# ============================================================================

# Create output directory
dir.create(here("output", "figures", "motivating_examples"),
           recursive = TRUE, showWarnings = FALSE)

# Plot 1: Black share over time
p_black_share <- ggplot(neighborhood_trajectories,
                        aes(x = year, y = black_share, color = neighborhood, group = neighborhood)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  # Add vertical lines for treatment
  geom_vline(xintercept = hunters_treatment_year, linetype = "dashed",
             color = okabe_ito[["orange"]], alpha = 0.6) +
  geom_vline(xintercept = brownsville_treatment_year, linetype = "dashed",
             color = okabe_ito[["sky_blue"]], alpha = 0.6) +
  scale_color_manual(values = c("Hunter's Point, SF" = okabe_ito[["orange"]],
                                "Brownsville, Brooklyn" = okabe_ito[["sky_blue"]])) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Black Population Share: Two Iconic Public Housing Neighborhoods",
    subtitle = "Dashed lines indicate first public housing construction",
    x = "Year",
    y = "Black Share of Population",
    color = "Neighborhood"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(here("output", "figures", "motivating_examples", "black_share_trajectory.pdf"),
       p_black_share, width = 10, height = 6)

# Plot 2: Median income over time (in 2023 dollars)
p_income <- ggplot(neighborhood_trajectories,
                   aes(x = year, y = median_income, color = neighborhood, group = neighborhood)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = hunters_treatment_year, linetype = "dashed",
             color = okabe_ito[["orange"]], alpha = 0.6) +
  geom_vline(xintercept = brownsville_treatment_year, linetype = "dashed",
             color = okabe_ito[["sky_blue"]], alpha = 0.6) +
  scale_color_manual(values = c("Hunter's Point, SF" = okabe_ito[["orange"]],
                                "Brownsville, Brooklyn" = okabe_ito[["sky_blue"]])) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Median Household Income: Two Iconic Public Housing Neighborhoods",
    subtitle = "Dashed lines indicate first public housing construction (2023 dollars)",
    x = "Year",
    y = "Median Household Income",
    color = "Neighborhood"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(here("output", "figures", "motivating_examples", "median_income_trajectory.pdf"),
       p_income, width = 10, height = 6)

# Plot 3: Combined 2x2 panel
p_combined <- neighborhood_trajectories %>%
  pivot_longer(cols = c(black_share, median_income, median_rent, pop_total),
               names_to = "variable", values_to = "value") %>%
  mutate(
    variable_label = case_when(
      variable == "black_share" ~ "Black Share",
      variable == "median_income" ~ "Median Income ($2023)",
      variable == "median_rent" ~ "Median Rent ($2023)",
      variable == "pop_total" ~ "Total Population"
    ),
    variable_label = factor(variable_label,
                           levels = c("Black Share", "Total Population",
                                    "Median Income ($2023)", "Median Rent ($2023)"))
  ) %>%
  ggplot(aes(x = year, y = value, color = neighborhood, group = neighborhood)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~variable_label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Hunter's Point, SF" = okabe_ito[["orange"]],
                                "Brownsville, Brooklyn" = okabe_ito[["sky_blue"]])) +
  labs(
    title = "Demographic and Economic Trajectories: Two Iconic Public Housing Neighborhoods",
    x = "Year",
    y = NULL,
    color = "Neighborhood"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(here("output", "figures", "motivating_examples", "combined_trajectories.pdf"),
       p_combined, width = 12, height = 8)

# ============================================================================
# 5. Create summary statistics table
# ============================================================================

# Calculate changes from 1940 to 1980
changes <- neighborhood_trajectories %>%
  filter(year %in% c(1940, 1980)) %>%
  group_by(neighborhood) %>%
  summarize(
    black_share_1940 = black_share[year == 1940],
    black_share_1980 = black_share[year == 1980],
    black_share_change = black_share_1980 - black_share_1940,

    income_1940 = median_income[year == 1940],
    income_1980 = median_income[year == 1980],
    income_pct_change = (income_1980 - income_1940) / income_1940 * 100,

    .groups = "drop"
  )

cat("\n\nSummary of changes (1940 to 1980):\n")
print(changes)

# Save summary
write.csv(changes,
          here("output", "tables", "motivating_examples_summary.csv"),
          row.names = FALSE)

# Save full trajectories
write.csv(neighborhood_trajectories,
          here("output", "tables", "motivating_examples_trajectories.csv"),
          row.names = FALSE)

# ============================================================================
# 6. Create maps showing change in Black share 1950-1970
# ============================================================================

cat("\n\nCreating maps of change in Black share (1950-1970)...\n")

# Identify nearby neighborhoods (within 2km)
hunters_tract_geoms <- tract_data %>%
  filter(GISJOIN_1950 %in% hunters_tract_ids, YEAR == 1950)

brownsville_tract_geoms <- tract_data %>%
  filter(GISJOIN_1950 %in% brownsville_tract_ids, YEAR == 1950)

# Buffer by 2km and find nearby tracts
hunters_buffer <- st_buffer(hunters_tract_geoms, dist = 2000)
brownsville_buffer <- st_buffer(brownsville_tract_geoms, dist = 2000)

# Get nearby tracts (within buffer but not treated)
# Filter out non-residential areas (pop < 100 in 1950)
sf_nearby <- tract_data %>%
  filter(cbsa_title == "San Francisco-Oakland-Fremont, CA", YEAR == 1950) %>%
  st_filter(hunters_buffer) %>%
  filter(!GISJOIN_1950 %in% hunters_tract_ids,
         total_pop >= 100) %>%
  pull(GISJOIN_1950) %>%
  unique()

nyc_nearby <- tract_data %>%
  filter(cbsa_title == "New York-Northern New Jersey-Long Island, NY-NJ-PA", YEAR == 1950) %>%
  st_filter(brownsville_buffer) %>%
  filter(!GISJOIN_1950 %in% brownsville_tract_ids,
         total_pop >= 100) %>%
  pull(GISJOIN_1950) %>%
  unique()

cat("Hunter's Point nearby tracts:", length(sf_nearby), "\n")
cat("Brownsville nearby tracts:", length(nyc_nearby), "\n")

# Calculate change in Black share and White pop 1950-1970
sf_change <- tract_data %>%
  st_drop_geometry() %>%
  filter(cbsa_title == "San Francisco-Oakland-Fremont, CA",
         YEAR %in% c(1950, 1970),
         GISJOIN_1950 %in% c(hunters_tract_ids, sf_nearby)) %>%
  select(GISJOIN_1950, YEAR, black_share, white_pop) %>%
  pivot_wider(names_from = YEAR, values_from = c(black_share, white_pop)) %>%
  mutate(change_black_share = black_share_1970 - black_share_1950,
         change_white_pop = white_pop_1970 - white_pop_1950,
         tract_type = if_else(GISJOIN_1950 %in% hunters_tract_ids, "Treated", "Nearby"))

nyc_change <- tract_data %>%
  st_drop_geometry() %>%
  filter(cbsa_title == "New York-Northern New Jersey-Long Island, NY-NJ-PA",
         YEAR %in% c(1950, 1970),
         GISJOIN_1950 %in% c(brownsville_tract_ids, nyc_nearby)) %>%
  select(GISJOIN_1950, YEAR, black_share, white_pop) %>%
  pivot_wider(names_from = YEAR, values_from = c(black_share, white_pop)) %>%
  mutate(change_black_share = black_share_1970 - black_share_1950,
         change_white_pop = white_pop_1970 - white_pop_1950,
         tract_type = if_else(GISJOIN_1950 %in% brownsville_tract_ids, "Treated", "Nearby"))

# Merge change data with geometries
sf_map_data <- tract_data %>%
  filter(cbsa_title == "San Francisco-Oakland-Fremont, CA", YEAR == 1950,
         GISJOIN_1950 %in% c(hunters_tract_ids, sf_nearby)) %>%
  left_join(sf_change, by = "GISJOIN_1950")

nyc_map_data <- tract_data %>%
  filter(cbsa_title == "New York-Northern New Jersey-Long Island, NY-NJ-PA", YEAR == 1950,
         GISJOIN_1950 %in% c(brownsville_tract_ids, nyc_nearby)) %>%
  left_join(nyc_change, by = "GISJOIN_1950")

# Map 1: Hunter's Point SF
p_sf_map <- ggplot() +
  geom_sf(data = sf_map_data,
          aes(fill = change_black_share),
          color = "gray30", size = 0.3) +
  geom_sf(data = hunters_point,
          color = "red", size = 3, shape = 21, fill = "red", alpha = 0.8) +
  scale_fill_gradient2(
    low = okabe_ito[["sky_blue"]],
    mid = "white",
    high = okabe_ito[["vermillion"]],
    midpoint = 0,
    labels = scales::percent_format(),
    name = "Change in\nBlack Share\n(1950-1970)"
  ) +
  labs(
    title = "Hunter's Point, San Francisco: Change in Black Population Share",
    subtitle = "Red dots = Public housing projects | 1950 to 1970",
    caption = "Includes treated tracts and residential neighborhoods within 2km (pop >= 100 in 1950)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave(here("output", "figures", "motivating_examples", "hunters_point_change_map.pdf"),
       p_sf_map, width = 10, height = 8)

# Map 2: Brownsville Brooklyn
p_nyc_map <- ggplot() +
  geom_sf(data = nyc_map_data,
          aes(fill = change_black_share),
          color = "gray30", size = 0.3) +
  geom_sf(data = brownsville,
          color = "red", size = 3, shape = 21, fill = "red", alpha = 0.8) +
  scale_fill_gradient2(
    low = okabe_ito[["sky_blue"]],
    mid = "white",
    high = okabe_ito[["vermillion"]],
    midpoint = 0,
    labels = scales::percent_format(),
    name = "Change in\nBlack Share\n(1950-1970)"
  ) +
  labs(
    title = "Brownsville, Brooklyn: Change in Black Population Share",
    subtitle = "Red dots = Public housing projects | 1950 to 1970",
    caption = "Includes treated tracts and residential neighborhoods within 2km (pop >= 100 in 1950)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
p_nyc_map

ggsave(here("output", "figures", "motivating_examples", "brownsville_change_map.pdf"),
       p_nyc_map, width = 10, height = 8)

# ============================================================================
# 7. Create maps showing change in White population 1950-1970
# ============================================================================

cat("\nCreating maps of change in White population (1950-1970)...\n")

# Map 3: Hunter's Point SF - White pop change
p_sf_white_map <- ggplot() +
  geom_sf(data = sf_map_data,
          aes(fill = change_white_pop),
          color = "gray30", size = 0.3) +
  geom_sf(data = hunters_point,
          color = "red", size = 3, shape = 21, fill = "red", alpha = 0.8) +
  scale_fill_gradient2(
    low = okabe_ito[["vermillion"]],
    mid = "white",
    high = okabe_ito[["sky_blue"]],
    midpoint = 0,
    labels = scales::comma_format(),
    name = "Change in\nWhite Pop\n(1950-1970)"
  ) +
  labs(
    title = "Hunter's Point, San Francisco: Change in White Population",
    subtitle = "Red dots = Public housing projects | 1950 to 1970",
    caption = "Includes treated tracts and residential neighborhoods within 2km (pop >= 100 in 1950)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave(here("output", "figures", "motivating_examples", "hunters_point_white_pop_change_map.pdf"),
       p_sf_white_map, width = 10, height = 8)

# Map 4: Brownsville Brooklyn - White pop change
p_nyc_white_map <- ggplot() +
  geom_sf(data = nyc_map_data,
          aes(fill = change_white_pop),
          color = "gray30", size = 0.3) +
  geom_sf(data = brownsville,
          color = "red", size = 3, shape = 21, fill = "red", alpha = 0.8) +
  scale_fill_gradient2(
    low = okabe_ito[["vermillion"]],
    mid = "white",
    high = okabe_ito[["sky_blue"]],
    midpoint = 0,
    labels = scales::comma_format(),
    name = "Change in\nWhite Pop\n(1950-1970)"
  ) +
  labs(
    title = "Brownsville, Brooklyn: Change in White Population",
    subtitle = "Red dots = Public housing projects | 1950 to 1970",
    caption = "Includes treated tracts and residential neighborhoods within 2km (pop >= 100 in 1950)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave(here("output", "figures", "motivating_examples", "brownsville_white_pop_change_map.pdf"),
       p_nyc_white_map, width = 10, height = 8)

cat("\nMaps created successfully!\n")

# ============================================================================
# 8. Create 1940 baseline maps for comparison
# ============================================================================

cat("\nCreating 1940 baseline maps for comparison...\n")

# Calculate change from 1940-1970
sf_change_1940 <- tract_data %>%
  st_drop_geometry() %>%
  filter(cbsa_title == "San Francisco-Oakland-Fremont, CA",
         YEAR %in% c(1940, 1970),
         GISJOIN_1950 %in% c(hunters_tract_ids, sf_nearby)) %>%
  select(GISJOIN_1950, YEAR, black_share, white_pop) %>%
  pivot_wider(names_from = YEAR, values_from = c(black_share, white_pop)) %>%
  mutate(change_black_share = black_share_1970 - black_share_1940,
         change_white_pop = white_pop_1970 - white_pop_1940,
         tract_type = if_else(GISJOIN_1950 %in% hunters_tract_ids, "Treated", "Nearby"))

nyc_change_1940 <- tract_data %>%
  st_drop_geometry() %>%
  filter(cbsa_title == "New York-Northern New Jersey-Long Island, NY-NJ-PA",
         YEAR %in% c(1940, 1970),
         GISJOIN_1950 %in% c(brownsville_tract_ids, nyc_nearby)) %>%
  select(GISJOIN_1950, YEAR, black_share, white_pop) %>%
  pivot_wider(names_from = YEAR, values_from = c(black_share, white_pop)) %>%
  mutate(change_black_share = black_share_1970 - black_share_1940,
         change_white_pop = white_pop_1970 - white_pop_1940,
         tract_type = if_else(GISJOIN_1950 %in% brownsville_tract_ids, "Treated", "Nearby"))

# Merge with geometries
sf_map_data_1940 <- tract_data %>%
  filter(cbsa_title == "San Francisco-Oakland-Fremont, CA", YEAR == 1950,
         GISJOIN_1950 %in% c(hunters_tract_ids, sf_nearby)) %>%
  left_join(sf_change_1940, by = "GISJOIN_1950")

nyc_map_data_1940 <- tract_data %>%
  filter(cbsa_title == "New York-Northern New Jersey-Long Island, NY-NJ-PA", YEAR == 1950,
         GISJOIN_1950 %in% c(brownsville_tract_ids, nyc_nearby)) %>%
  left_join(nyc_change_1940, by = "GISJOIN_1950")

# Map: Hunter's Point - Black share change 1940-1970
p_sf_map_1940 <- ggplot() +
  geom_sf(data = sf_map_data_1940,
          aes(fill = change_black_share),
          color = "gray30", size = 0.3) +
  geom_sf(data = hunters_point,
          color = "red", size = 3, shape = 21, fill = "red", alpha = 0.8) +
  scale_fill_gradient2(
    low = okabe_ito[["sky_blue"]],
    mid = "white",
    high = okabe_ito[["vermillion"]],
    midpoint = 0,
    labels = scales::percent_format(),
    name = "Change in\nBlack Share\n(1940-1970)"
  ) +
  labs(
    title = "Hunter's Point, San Francisco: Change in Black Population Share",
    subtitle = "Red dots = Public housing projects | 1940 to 1970"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave(here("output", "figures", "motivating_examples", "hunters_point_change_map_1940.pdf"),
       p_sf_map_1940, width = 10, height = 8)

# Map: Hunter's Point - White pop change 1940-1970
p_sf_white_map_1940 <- ggplot() +
  geom_sf(data = sf_map_data_1940,
          aes(fill = change_white_pop),
          color = "gray30", size = 0.3) +
  geom_sf(data = hunters_point,
          color = "red", size = 3, shape = 21, fill = "red", alpha = 0.8) +
  scale_fill_gradient2(
    low = okabe_ito[["vermillion"]],
    mid = "white",
    high = okabe_ito[["sky_blue"]],
    midpoint = 0,
    labels = scales::comma_format(),
    name = "Change in\nWhite Pop\n(1940-1970)"
  ) +
  labs(
    title = "Hunter's Point, San Francisco: Change in White Population",
    subtitle = "Red dots = Public housing projects | 1940 to 1970"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave(here("output", "figures", "motivating_examples", "hunters_point_white_pop_change_map_1940.pdf"),
       p_sf_white_map_1940, width = 10, height = 8)

# Map: Brownsville - Black share change 1940-1970
p_nyc_map_1940 <- ggplot() +
  geom_sf(data = nyc_map_data_1940,
          aes(fill = change_black_share),
          color = "gray30", size = 0.3) +
  geom_sf(data = brownsville,
          color = "red", size = 3, shape = 21, fill = "red", alpha = 0.8) +
  scale_fill_gradient2(
    low = okabe_ito[["sky_blue"]],
    mid = "white",
    high = okabe_ito[["vermillion"]],
    midpoint = 0,
    labels = scales::percent_format(),
    name = "Change in\nBlack Share\n(1940-1970)"
  ) +
  labs(
    title = "Brownsville, Brooklyn: Change in Black Population Share",
    subtitle = "Red dots = Public housing projects | 1940 to 1970"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave(here("output", "figures", "motivating_examples", "brownsville_change_map_1940.pdf"),
       p_nyc_map_1940, width = 10, height = 8)

# Map: Brownsville - White pop change 1940-1970
p_nyc_white_map_1940 <- ggplot() +
  geom_sf(data = nyc_map_data_1940,
          aes(fill = change_white_pop),
          color = "gray30", size = 0.3) +
  geom_sf(data = brownsville,
          color = "red", size = 3, shape = 21, fill = "red", alpha = 0.8) +
  scale_fill_gradient2(
    low = okabe_ito[["vermillion"]],
    mid = "white",
    high = okabe_ito[["sky_blue"]],
    midpoint = 0,
    labels = scales::comma_format(),
    name = "Change in\nWhite Pop\n(1940-1970)"
  ) +
  labs(
    title = "Brownsville, Brooklyn: Change in White Population",
    subtitle = "Red dots = Public housing projects | 1940 to 1970"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave(here("output", "figures", "motivating_examples", "brownsville_white_pop_change_map_1940.pdf"),
       p_nyc_white_map_1940, width = 10, height = 8)

cat("1940 baseline maps created!\n")

# ============================================================================
# 9. Publication-ready maps (no titles, 1940 baseline, percent change white pop)
# ============================================================================

cat("\nCreating publication-ready maps...\n")

# Add percent change to the 1940 change data
sf_change_1940 <- sf_change_1940 %>%
  mutate(pct_change_white_pop = (white_pop_1970 - white_pop_1940) / white_pop_1940 * 100)

nyc_change_1940 <- nyc_change_1940 %>%
  mutate(pct_change_white_pop = (white_pop_1970 - white_pop_1940) / white_pop_1940 * 100)

# Re-merge with geometries
sf_map_data_1940 <- tract_data %>%
  filter(cbsa_title == "San Francisco-Oakland-Fremont, CA", YEAR == 1950,
         GISJOIN_1950 %in% c(hunters_tract_ids, sf_nearby)) %>%
  left_join(sf_change_1940, by = "GISJOIN_1950")

nyc_map_data_1940 <- tract_data %>%
  filter(cbsa_title == "New York-Northern New Jersey-Long Island, NY-NJ-PA", YEAR == 1950,
         GISJOIN_1950 %in% c(brownsville_tract_ids, nyc_nearby)) %>%
  left_join(nyc_change_1940, by = "GISJOIN_1950")

# Publication map: Hunter's Point - Black share change 1940-1970
p_sf_black_pub <- ggplot() +
  geom_sf(data = sf_map_data_1940,
          aes(fill = change_black_share),
          color = "gray30", size = 0.3) +
  geom_sf(data = hunters_point,
          color = "red", size = 3, shape = 21, fill = "red", alpha = 0.8) +
  scale_fill_gradient2(
    low = okabe_ito[["sky_blue"]],
    mid = "white",
    high = okabe_ito[["vermillion"]],
    midpoint = 0,
    labels = scales::percent_format(),
    name = "Change in\nBlack Share"
  ) +
  theme_void() +
  theme(legend.position = "right")

ggsave(here("output", "figures", "motivating_examples", "hunters_point_black_share_change_1940_pub.pdf"),
       p_sf_black_pub, width = 8, height = 6)

# Publication map: Hunter's Point - White pop change 1940-1970
p_sf_white_pub <- ggplot() +
  geom_sf(data = sf_map_data_1940,
          aes(fill = change_white_pop),
          color = "gray30", size = 0.3) +
  geom_sf(data = hunters_point,
          color = "red", size = 3, shape = 21, fill = "red", alpha = 0.8) +
  scale_fill_gradient2(
    low = okabe_ito[["vermillion"]],
    mid = "white",
    high = okabe_ito[["sky_blue"]],
    midpoint = 0,
    labels = scales::comma_format(),
    name = "Change in\nWhite Pop"
  ) +
  theme_void() +
  theme(legend.position = "right")

ggsave(here("output", "figures", "motivating_examples", "hunters_point_white_pop_change_1940_pub.pdf"),
       p_sf_white_pub, width = 8, height = 6)

# Publication map: Brownsville - Black share change 1940-1970
p_nyc_black_pub <- ggplot() +
  geom_sf(data = nyc_map_data_1940,
          aes(fill = change_black_share),
          color = "gray30", size = 0.3) +
  geom_sf(data = brownsville,
          color = "red", size = 3, shape = 21, fill = "red", alpha = 0.8) +
  scale_fill_gradient2(
    low = okabe_ito[["sky_blue"]],
    mid = "white",
    high = okabe_ito[["vermillion"]],
    midpoint = 0,
    labels = scales::percent_format(),
    name = "Change in\nBlack Share"
  ) +
  theme_void() +
  theme(legend.position = "right")

ggsave(here("output", "figures", "motivating_examples", "brownsville_black_share_change_1940_pub.pdf"),
       p_nyc_black_pub, width = 8, height = 6)

# Publication map: Brownsville - White pop change 1940-1970
p_nyc_white_pub <- ggplot() +
  geom_sf(data = nyc_map_data_1940,
          aes(fill = change_white_pop),
          color = "gray30", size = 0.3) +
  geom_sf(data = brownsville,
          color = "red", size = 3, shape = 21, fill = "red", alpha = 0.8) +
  scale_fill_gradient2(
    low = okabe_ito[["vermillion"]],
    mid = "white",
    high = okabe_ito[["sky_blue"]],
    midpoint = 0,
    labels = scales::comma_format(),
    name = "Change in\nWhite Pop"
  ) +
  theme_void() +
  theme(legend.position = "right")

ggsave(here("output", "figures", "motivating_examples", "brownsville_white_pop_change_1940_pub.pdf"),
       p_nyc_white_pub, width = 8, height = 6)

cat("Publication-ready maps created!\n")

cat("\n\nScript completed successfully!")
cat("\nOutputs saved to:")
cat("\n  - Figures: output/figures/motivating_examples/")
cat("\n  - Tables: output/tables/motivating_examples_*.csv\n")
