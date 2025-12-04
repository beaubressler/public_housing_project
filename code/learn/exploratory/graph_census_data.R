library(MatchIt)
library(tidyverse)
library(sf)
library(lmtest)
library(fixest)
library(here)
library(RColorBrewer)
library(ggrepel)

# libraries for covariate testing
library(cobalt)
library(tableone)
library(kableExtra)
#library for Sant'Anna and Zhou (2020)
library(DRDID)
# library for Callaway and Sant'Anna (2021)
library(did)

library(naniar)

rm(list=ls())

# 0. Set seed and parameters -----

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small", or "combined"
data_type <- "combined"

# Notes: With CDD large, KNN = 2 seems to work well
# with the smaller dataset, KNN = 3 work

# define directories
merged_data_dir <- here("data", "derived", "merged", data_type)
map_output_dir <- here("output", "figures", "matched_did", data_type)
balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)
holc_data_dir <- here("data", "derived", "holc")

map_dir <- here("output", "figures", "exploratory")
results_dir <- here("output", "regression_results", "matched_did", data_type)
balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)


# define file paths
tract_with_treatment_status_filepath <-
  here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")
treated_tracts_panel_filepath <-
  here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")

event_study_rings_filepath <-
  here(merged_data_dir, "unique_tracts_and_rings.csv")

# read census data with treatment status
census_tract_sample_raw <-
  st_read(tract_with_treatment_status_filepath)

# read tract tracts panel
treated_tracts_panel_raw <-
  st_read(treated_tracts_panel_filepath) %>% 
  st_drop_geometry()

# read in unique rings and tracts
# These are the tracts that are treated, the ring of tracts touching those, and the ring of tracts
# touching the inner ring tracts
tracts_and_rings <-
  read_csv(event_study_rings_filepath) %>% 
  #try excluding outer
  filter(location_type != "outer")

# 1. Data Preparation ------

# read in HOLC data
holc_data <-
  read_csv(here(holc_data_dir, "tract_holc_classifications.csv")) %>% 
  select(-contains("city"))


# get treated tracts, unique, and merge onto census tract sample
treated_tracts_panel <-
  treated_tracts_panel_raw %>% 
  distinct(GISJOIN_1950, total_public_housing_units)

census_tract_data_geo <-
  #left_join(census_tract_sample_raw, treated_tracts_panel) %>% 
  census_tract_sample_raw %>% 
  left_join(treated_tracts_panel) %>%
  # merge on HOLC data
  left_join(holc_data) %>% 
  # merge on rings
  left_join(tracts_and_rings) %>% 
  # set location_type = "donor_pool" if it's NA
  mutate(location_type = if_else(is.na(location_type), "donor_pool", location_type)) %>%
  # create unique id for each tract
  mutate(tract_id = paste0(STATEA, COUNTYA, TRACTA)) %>% 
  # # select relevant columns
  mutate(asinh_pop_total = asinh(total_pop),
         asinh_pop_white = asinh(white_pop),
         asinh_pop_black = asinh(black_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent = asinh(median_rent_calculated),
         asinh_median_home_value = asinh(median_home_value_calculated),
         asinh_distance_from_cbd = asinh(distance_from_cbd)) %>% 
  dplyr::rename(year = YEAR) %>% 
  # for HOLC variables (grade and category) if category is missisng ,set to "missing"
  mutate(category_most_overlap = ifelse(is.na(category_most_overlap), "missing", category_most_overlap),
         grade_most_overlap = ifelse(is.na(grade_most_overlap), "missing", grade_most_overlap))

census_tract_data <-
  census_tract_data_geo %>% 
  st_drop_geometry()

# check category by city
census_tract_data %>% 
  group_by(city) %>% 
  count(category_most_overlap)  %>% 
  # calculate share of missing by city
  summarise(share_missing = n / sum(n)) %>%
  View()


# Analysis -----
id_vars <- c("tract_id", "city", "COUNTY", "STATE", "TRACTA")
outcome_vars <- c("black_share", "white_share", "asinh_pop_total", "asinh_pop_black",
                                       "asinh_pop_white",
                                       "asinh_median_income", "asinh_median_home_value", "asinh_median_rent",
                                       "population_density", "lfp_rate", "unemp_rate", 
                                       "pct_hs_grad", "pct_some_college", "median_educ_years_25plus",
                                       "housing_density")



## 1. Check data structure ----



str(census_tract_data)
summary(census_tract_data %>% select(any_of(id_vars), any_of(outcome_vars)))
head(census_tract_data)


# Checking if the panel is balanced
# NOTE: Some tracts are only available in 1950 onwards...
# Perhaps I need to not just filter on counties, but also TRACTS themselves. I guess only part of a county can have tract data
tract_year_df <- 
  table(census_tract_data$tract_id, census_tract_data$city, census_tract_data$year) %>% 
  as.data.frame()

colnames(tract_year_df) <- c("tract_id", "city", "year", "count")

# Check if there are any tracts that are only available in one year
tract_counts <- census_tract_data %>%
  group_by(city, year, tract_id) %>%              # Group by city, year, and tract_id
  summarise(count = n()) %>%                      # Count occurrences of each tract in each year
  filter(count == 1) %>%                          # Filter only those with count == 1
  group_by(city, year) %>%                        # Group again by city and year
  summarise(num_tracts = n())                     # Count number of tracts in each city-year combination


# ## Comapre to all census tract data, before balancing
# census_tract_data_full <- 
#   st_read(here(merged_data_dir, "census_tract_sample_with_treatment_status_all.gpkg"))
# 
# 
# # Check if there are any tracts that are only available in one year
# tract_counts_full <- census_tract_data_full %>%
#   st_drop_geometry() %>% 
#   group_by(city, YEAR, COUNTY, TRACTA) %>%              # Group by city, year, and tract_id
#   summarise(count = n()) %>%                      # Count occurrences of each tract in each year
#   filter(count == 1) %>%                          # Filter only those with count == 1
#   group_by(city, YEAR) %>%                        # Group again by city and year
#   summarise(num_tracts = n())                     # Count number of tracts in each city-year combination


## 2. Check for missing data, distribution of it -----
# Check for missing data
# Heatmap of missing values across all variables
gg_miss_var(census_tract_data  %>% select(any_of(outcome_vars)),
            show_pct = TRUE) +
  labs(title = "Missing Values by Variable")

# exclude 1930 from heatmap
gg_miss_var(census_tract_data  %>% filter(year != 1930) %>% select(any_of(outcome_vars)),
            show_pct = TRUE) +
  labs(title = "Missing Values by Variable, excl 1930")

# exclude 1930 and 1940 from ehatmap
gg_miss_var(census_tract_data  %>% filter(year != 1930 & year != 1940) %>% select(any_of(outcome_vars)),
            show_pct = TRUE) +
  labs(title = "Missing Values by Variable, excl 1930 and 1940")

# check if missing completely at random (MCAR)
mcar_test(census_tract_data  %>% select(any_of(outcome_vars)))

## 3. Check for spatial autocorrelation ----
# Commenting out because it takes a while to run
library(spdep)

coords <- st_centroid(census_tract_data_geo %>% filter(year == 1990)) %>% 
  st_coordinates() 

neighbors <- poly2nb(census_tract_data_geo %>% filter(year == 1990), queen = FALSE) 

# remove tracts with no neighbors
no_neighbors <- which(card(neighbors) == 0)

census_tract_data_no_isolated <- census_tract_data_geo %>% filter(year == 1990) 
census_tract_data_no_isolated <- census_tract_data_no_isolated[-no_neighbors,]

neighbors <- poly2nb(census_tract_data_no_isolated, queen = FALSE)
weights <- nb2listw(neighbors)
# Moran'S I test
moran.test(census_tract_data_no_isolated %>% pull(black_share), weights)


# Graphs -----

## US map of data coverage -----
unique_cbsas <- 
  census_tract_data_geo %>% 
  st_drop_geometry() %>% 
  select(cbsa_title) %>% 
  distinct()

cbsa_data <- 
  census_tract_data_geo %>% 
  filter(year == 1990) %>% 
  group_by(cbsa_title) %>% 
  summarize(
    population = sum(total_pop, na.rm = TRUE)/1000,  # Aggregate population
    geometry = st_union(geom)  # Merge geometries
  ) %>%
  ungroup()

# mainland us - use full resolution to capture Florida and Texas southern tips
states <-
  tigris::states(cb = FALSE) %>%
  filter(!STUSPS %in% c("HI", "AK", "PR"))  # Exclude Hawaii, Alaska, and Puerto Rico
states <- st_transform(states, st_crs(cbsa_data))



mainland_bbox <- st_bbox(c(
  xmin = -125,  # Western edge
  xmax = -65,   # Eastern edge
  ymin = 22,    # Southern edge (captures Florida Keys and southern Texas)
  ymax = 50     # Northern edge
), crs = st_crs(states))  # Use the CRS of cbsa_data


cbsa_centroids <- cbsa_data %>%
  st_centroid() %>% # Compute centroids for each CBSA
  st_transform(crs = 3857) %>%  # Transform to a projected CRS
  mutate(
    buffer_size = sqrt(population) / max(sqrt(population)) * 125000,  # Adjust scaling as needed
    label = str_extract(cbsa_title, "^[^-,]+")  # Extract text up to the first "-" or ","
  )

cbsa_circles <- st_buffer(cbsa_centroids, dist = cbsa_centroids$buffer_size)

# Crop states and CBSA data to mainland USA
states_cropped <- st_crop(states, mainland_bbox)
cbsa_data_cropped <- st_crop(cbsa_data, mainland_bbox)

ggplot() +
  # Add base map
  geom_sf(data = states_cropped, fill = "gray90", color = "white", size = 0.2) +
  
  # Add CBSA locations, with size by population
  geom_sf(data = cbsa_circles, size = cbsa_circles$buffer_size, color = "dodgerblue", fill = "dodgerblue", alpha = 0.6) +
  
  geom_text_repel(data = cbsa_centroids,
                  aes(label = label, geometry = geometry),
                  stat = "sf_coordinates", size = 2.75) +

  # Scale for population size
  scale_fill_continuous(
    name = "Population",
    labels = scales::comma
  ) +
  
  # Theme and labels
  theme_minimal() +
  theme(
    plot.margin = margin(0, 0, 0, 0),  # top, right, bottom, left
    panel.grid = element_blank(),              # Remove gridlines
    axis.title = element_blank(),              # Remove axis titles
    axis.text = element_blank(),               # Remove axis text (latitude/longitude labels)
    axis.ticks = element_blank()               # Remove axis ticks
  ) 
# +
#   labs(
#     title = "Geographic Coverage of Sample"
#   )

# save map
ggsave(
  here(map_dir, "cbsa_coverage_map.pdf"),
  width = 13.5, height = 9.5, units = "in",
  dpi = 450, bg = "white",
  device = cairo_pdf,
  limitsize = FALSE
)

# save slide version (larger text, cleaner)
dir.create(here(map_dir, "slides"), showWarnings = FALSE, recursive = TRUE)
ggsave(
  here(map_dir, "slides", "cbsa_coverage_map.pdf"),
  width = 13.5, height = 9.5, units = "in",
  dpi = 450, bg = "white",
  device = cairo_pdf,
  limitsize = FALSE
)

## Graphs of each variable by city ----
outcome_vars <- c("black_share", "asinh_pop_total", "total_pop", "black_pop", "asinh_pop_black",
                  "white_pop", "asinh_pop_white", "median_income", "median_home_value", "median_rent",
                  "asinh_median_income", "asinh_median_home_value", "asinh_median_rent",
                  "population_density", "lfp_rate", "unemp_rate", 
                  "high_skill_share", "mid_skill_share",
                  "pct_hs_grad", "pct_some_college", "median_educ_years_25plus",
                  "housing_density", "asinh_distance_from_cbd")

# Calculate global min and max for each variable across all cities and years

# Create an empty list to store min and max values for each variable
global_min_max <- list()

# Loop over each variable in outcome_vars
for (var in outcome_vars) {
  # Calculate the 2nd and 98th percentiles for each variable
  for (var in outcome_vars) {
    global_min_max[[var]] <- census_tract_data_geo %>%
      st_drop_geometry() %>%
      summarise(
        lower_bound = quantile(.data[[var]], probs = 0.01, na.rm = TRUE),
        upper_bound = quantile(.data[[var]], probs = 0.99, na.rm = TRUE)
      )
  }
}

# Convert the list to a data frame or tibble for easier use later
global_min_max <- bind_rows(global_min_max, .id = "variable")


# Loop through each variable in outcome_var
for (var in outcome_vars) {
  
  # Extract the global min and max for the current variable
  min_value <- global_min_max %>% filter(variable == var) %>% pull(lower_bound)
  max_value <- global_min_max %>% filter(variable == var) %>% pull(upper_bound)
  
  
  # Create a new PDF file for each variable
  pdf_filename <- paste0(var, "_maps.pdf")
  pdf(file = here(map_dir, pdf_filename), width = 8, height = 6)
  
  # Loop through each city
  for (city_name in unique(census_tract_data$city)) {
    
    # Filter the dataset for the current city, drop 1930
    data_for_city <- census_tract_data_geo %>%
      filter(city == city_name) %>% 
      filter(year != 1930)
    
    # Loop through each year for the current city
    for (y in unique(data_for_city$year)) {
      
      # Filter data for the current year within the current city
      data_for_year_city <- data_for_city %>% filter(year == y)
      
      # for category most overlap, use a different color scale (discrete)
        # Create a ggplot map for the current variable, city, and year
      map <- ggplot(data_for_year_city) +
        geom_sf(aes_string(fill = var)) +  # aes_string allows dynamic variable selection
        scale_fill_viridis_c(limits = c(min_value, max_value), option = "plasma") +  # Set global limits
        theme_minimal() +
        labs(title = paste("Map of", var, "in", city_name, "-", y),
             fill = var)
      
      # Print the map (this adds it to the current PDF file)
      print(map)
    }
  }
  
  # Close the PDF after adding all maps for the current variable
  dev.off()
}

# Separately graph HOLC data
# Loop through each city

pdf_filename <- paste0("category_most_overlaps", "_maps.pdf")
pdf(file = here(map_dir, pdf_filename), width = 8, height = 6)

for (city_name in unique(census_tract_data$city)) {
  
  # Filter the dataset for the current city
  data_for_city <- census_tract_data_geo %>% filter(city == city_name)
  
  # Loop through each year for the current city
  for (y in unique(data_for_city$year)) {
    
    # Filter data for the current year within the current city
    data_for_year_city <- data_for_city %>% filter(year == y)
  
    # Create a ggplot map for the current variable, city, and year
    map <- ggplot(data_for_year_city) +
      geom_sf(aes_string(fill = var)) +  # aes_string allows dynamic variable selection
      scale_fill_discrete() +
      theme_minimal() +
      labs(title = paste("Map of", var, "in", city_name, "-", y),
           fill = var)
    
    # Print the map (this adds it to the current PDF file)
    print(map)
  }
}

dev.off()

## Graph of original 1940 tract data ----

# I am concerned about why some  seemingly random tracts seem to be missing from this panel...
# Are they missing from the original Census tract data too?
# I guess it could be places where the population data (for example) is messed up in one year, and so I drop the tract for all years
# I suppose I have to do that.
# But still, will compare
# ANSWER:
# 1. At this point (09/26/2024), the reason that there are some tracts missing is that I am excluding tracts that 
# already have public housing entirely from the sample. This way, they cannot be "never treated."
# There is a question about whether this is the right approach to take, but it is my current approach



# Load original 1940 tract data
census_tract_data_original <-
  read_sf(here(merged_data_dir, "census_tract_sample_with_treatment_status_all.gpkg"))

# create map of 1940 black share in new york city
census_tract_data_original %>% 
  filter(city == "Richmond", YEAR == 1940) %>% 
  ggplot() +
  geom_sf(aes(fill = black_share)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "1940 Total Pop in New York City")


# TODO: Why are so many of these tracts getting dropped? I see them in 1940 so they must be here all the time...
# View(census_tract_data_original %>% filter(city == "Chicago", TRACTA == "0315"))


# Get list of tracts that are in the original data but not the balanced data using a semi join
tracts_in_original_not_balanced <- 
  census_tract_data_original %>% 
  anti_join(census_tract_data, by = c("COUNTY", "TRACTA"))
  
  

# View pop data without concordance:
pop_data_nonconcorded <- read_sf(here("data", "derived", "census", "non_concorded", "tract_population_data_original_tracts.gpkg"))

# map  total_pop
pop_data_nonconcorded %>% 
  filter(COUNTY == "New York", YEAR == 1980) %>% 
  ggplot() +
  geom_sf(aes(fill = total_pop)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "1940 Black Share in New York City")
