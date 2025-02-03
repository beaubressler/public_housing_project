# --------------------------------------------------------
# Area Reweighting Crosswalk Visualization (1940 → 1990)
# --------------------------------------------------------
# This script visualizes the crosswalk of census tracts and enumeration districts
# from 1940 to 1990 using area-weighting methods.
# --------------------------------------------------------

# Load necessary libraries
library(tidyverse)
library(sf)
library(here)

# Define directories for crosswalks, Census data, and Enumeration Districts (EDs)
raw_ed_dir <- here("data", "raw", "urban_transitions", "enumeration_districts")
crosswalk_dir <- here("data", "derived", "geographic_crosswalks")
census_dir <- here("data", "derived", "census")
ed_crosswalk_dir <- here(crosswalk_dir, "ed_to_tract")
grf_dir <- here("data", "derived", "geographic_reference_file")

#-------
# Load raw Detroit, 1940 ED
# ------
year <- 40
city <- "DetroitMI"
city_zip <- here(raw_ed_dir, "ForEachDecade", paste0("ForEachCity19",year, ".zip"))
temp_decade_dir <- tempdir()
unzip(city_zip, exdir = temp_decade_dir)

# Path to the inner city ZIP
city_inner_zip <- here(temp_decade_dir, paste0("ForEachCity1940"), paste0(city, year, ".zip"))
# Unzip the inner city ZIP file
temp_city_dir <- tempdir()
unzip(city_inner_zip, exdir = temp_city_dir)

# Path to the shapefile
shp_file <- file.path(temp_city_dir, paste0(city, year), paste0(city, "_ed", year, "_aggr.shp"))

if (file.exists(shp_file)) {
  detroit_ed <- st_read(shp_file, quiet = TRUE) %>% 
    st_make_valid()
} else {
  stop("Shapefile not found!")
}

# plot it
detroit_ed %>%
  ggplot() +
  geom_sf() +
  labs(title = "Detroit Enumeration Districts (1940), raw UTP data") #+
  #geom_sf_text(data = detroit_ed, aes(label = ed), size = 2) 
  

# --------------------------
# Load Crosswalk Data
# --------------------------

# Tract-level area-weighting crosswalk (1940 → 1990)
tract_crosswalk_1940_to_1990 <- read_csv(here(crosswalk_dir, "tract_concordance_weights1940_to_1990.csv"))

# Enumeration District (ED) to Tract Crosswalks
ed_crosswalk_1940_to_1990 <- read_csv(here(ed_crosswalk_dir, "ed_1940_to_1990_tracts.csv"))
ed_crosswalk_1930_to_1990 <- read_csv(here(ed_crosswalk_dir, "ed_1930_to_1990_tracts.csv"))

# --------------------------
# Load Census and Geography Data
# --------------------------

# Census tract-level data (non-concorded)
census_tracts_original <- st_read(here(census_dir, "non_concorded", "tract_population_data_original_tracts.gpkg"))

# Enumeration district (ED) shapefile
census_ed_geometries <- st_read(here(ed_crosswalk_dir, "ed_1940_geometries.shp"))

# Geographic Reference File (GRF) - 1940
grf_1940 <- read_csv(here(grf_dir, "grf_1940_histid_ed_city.csv"))

# Extract unique city-ED combinations from GRF
grf_unique_eds <- grf_1940 %>% distinct(b_city, b_ed)

# --------------------------------------------------------
# 1. Visualizing Area Reweighting for 1940 → 1990 Tracts
# --------------------------------------------------------

# Extract Detroit tracts from 1940 and 1990 datasets
census_tracts_1940_detroit <- census_tracts_original %>%
  filter(YEAR == 1940, COUNTY == "Wayne", STATE == "Michigan")

census_tracts_1990_detroit <- census_tracts_original %>%
  filter(YEAR == 1990, COUNTY == "Wayne", STATE == "Michigan") %>%
  filter(GISJOIN %in% tract_crosswalk_1940_to_1990$GISJOIN_1990)  # Keep only crosswalked tracts

# Compute total weight assigned to each 1990 tract
tracts_1990_weighted_detroit <- census_tracts_1990_detroit %>%
  left_join(tract_crosswalk_1940_to_1990, by = c("GISJOIN" = "GISJOIN_1990")) %>%
  group_by(GISJOIN) %>%
  summarize(total_weight = sum(weight, na.rm = TRUE))

# Compute how many 1990 tracts correspond to each 1940 tract
tracts_1940_weighted_detroit <- census_tracts_1940_detroit %>%
  left_join(tract_crosswalk_1940_to_1990, by = c("GISJOIN" = "GISJOIN_1940")) %>%
  group_by(GISJOIN) %>%
  mutate(num_1990_tracts = n())

# --------------------------
# Plot 1940 and 1990 Census Tracts
# --------------------------

# 1940 Census Tracts
ggplot() +
  geom_sf(data = census_tracts_1940_detroit, fill = "blue", alpha = 0.3, color = "black", size = 0.2) +
  labs(title = "1940 Census Tracts in Detroit") +
  theme_minimal()

# 1990 Census Tracts
ggplot() +
  geom_sf(data = census_tracts_1990_detroit, fill = "red", alpha = 0.3, color = "black", size = 0.2) +
  labs(title = "1990 Census Tracts in Detroit") +
  theme_minimal()

# Overlay  the two
ggplot() +
  # 1940 Census Tracts (Filled in blue with transparency)
  geom_sf(data = census_tracts_1940_detroit, aes(fill = "1940 Tracts"), alpha = 0.4, color = "black", size = 0.3) +
  # 1990 Census Tracts (Only outlines in red)
  geom_sf(data = census_tracts_1990_detroit, aes(color = "1990 Tracts"), fill = NA, alpha = 0.3, size = 0.6) +
  # Define colors and label.
  scale_fill_manual(name = "", values = c("1940 Tracts" = "blue")) +
  scale_color_manual(name = "", values = c("1990 Tracts" = "red")) +
  # Titles and theme
  labs(title = "1940 and 1990 Census Tracts in Detroit",
       subtitle = "1940 tracts shaded in blue, 1990 tracts outlined in red",
       caption = "Source: U.S. Census Bureau") +
  theme_minimal() +
  theme(legend.position = "bottom")

# --------------------------
# Plot 1: 1990 Tracts Weighted by 1940 Area Contributions
# Interpretation:
# - Total weight > 1: 1990 tract is an aggregation of multiple 1940 tracts.
# - Total weight < 1: 1940 tract is a subset of a 1990 tract.
# --------------------------

ggplot() +
  geom_sf(data = tracts_1990_weighted_detroit, aes(fill = total_weight), color = NA, alpha = 0.7) +
  geom_sf(data = census_tracts_1990_detroit, fill = NA, color = "black", size = 0.3) +
  scale_fill_viridis_c(name = "Total Assigned 1940 Weights") +
  labs(title = "Mapping 1940 to 1990 Tracts via Area Weights",
       subtitle = "1990 tracts shaded by fraction of area received from 1940 tracts") +
  theme_minimal()

# --------------------------
# Plot 2: Number of 1990 Tracts Corresponding to Each 1940 Tract
# --------------------------

ggplot() +
  geom_sf(data = tracts_1940_weighted_detroit, aes(fill = num_1990_tracts), alpha = 0.7) +
  geom_sf(data = census_tracts_1940_detroit, fill = NA, color = "black", size = 0.3) +
  scale_fill_continuous(name = "Number of 1990 Tracts") +
  labs(title = "Mapping 1940 to 1990 Tracts via Area Weights",
       subtitle = "1940 tracts shaded by number of 1990 tracts they correspond to") +
  theme_minimal()

# --------------------------------------------------------
# 2. Visualizing Enumeration District (ED) Crosswalks
# --------------------------------------------------------

# Extract Detroit's 1940 Enumeration Districts
census_ed_data_1940_detroit <-
  census_ed_geometries %>%
  filter(city == "Detroit")

# --------------------------
# Plot 1940 Enumeration Districts
# --------------------------

ggplot() +
  geom_sf(data = census_ed_data_1940_detroit, fill = "blue", alpha = 0.3, color = "black", size = 0.2) +
  labs(title = "1940 Enumeration Districts in Detroit") +
  theme_minimal()

# --------------------------
# Overlay EDs with 1940 Census Tracts
# --------------------------

ggplot() +
  geom_sf(data = census_ed_data_1940_detroit, fill = "blue", alpha = 0.3, color = "black", size = 0.2) +
  geom_sf(data = census_tracts_1940_detroit, fill = NA, color = "red", size = 0.3) +
  labs(title = "1940 Enumeration Districts in Detroit",
       subtitle = "Overlaid with 1940 Census Tracts") +
  theme_minimal()


### try with Atlanta Georgia
census_ed_data_1940_akron <- 
  census_ed_geometries %>%
  filter(city == "Albany")

census_tracts_1940_akron <-
  census_tracts_original %>%
  filter(YEAR == 1940, COUNTY == "Albany")

ggplot() +
  geom_sf(data = census_ed_data_1940_akron, fill = "blue", alpha = 0.3, color = "black", size = 0.2) +
  geom_sf(data = census_tracts_1940_akron, fill = NA, color = "red", size = 0.3) +
  labs(title = "1940 Enumeration Districts in Albany",
       subtitle = "Overlaid with 1940 Census Tracts") +
  theme_minimal()

