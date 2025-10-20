###
# This is an adaptation of the area-reweighting crosswalking Python code provided by Fabian Eckert et al 2020
# Creates area re-weighting crosswalks between 1930-2000 Census tracts and 1950 Census tracts

###

library(tidyverse)
library(sf)
library(here)

# Define file paths and variables
reference_path <- here("data", "raw", "nhgis", "gis", "nhgis0027_shapefile_tl2000_us_tract_1950")
reference_fname <- "US_tract_1950.shp"
reference_geoid <- "GISJOIN.1"

# Define output path
output_path <- here("data", "derived", "geographic_crosswalks")

# Loop through the years
for (y in c("1930", "1940", "1960", "1970", "1980", "1990", "2000","2010")) {
  reporting_path <- here("data", "raw", "nhgis", "gis", paste0("nhgis0027_shapefile_tl2000_us_tract_", y))
  reporting_fname <- paste0("US_tract_", y, ".shp")
  reporting_geoid <- "GISJOIN"
  output_fname <- paste0("tract_concordance_weights", y, "_to_1950.csv")
  
  # Read in the reporting shapefile
  shp_reporting <- st_read(here(reporting_path, reporting_fname))
  shp_reporting <- shp_reporting %>% 
    mutate(area_base = st_area(.))
  
  # Read in the reference shapefile
  shp_reference <- st_read(file.path(reference_path, reference_fname))
  
  # Compute intersection
  intersect <- st_intersection(shp_reporting, shp_reference) %>%
    mutate(area = st_area(.))
  
  # Compute weights
  intersect <- intersect %>%
    mutate(weight = as.numeric(area) / as.numeric(area_base))
  
  # Renormalize weights to ensure that weights sum up to 1 for each unique reporting_geoid
  intersect <- intersect %>%
    group_by(across(all_of(reporting_geoid))) %>%
    mutate(weight = weight / sum(weight)) %>%
    ungroup()
  
  
  # Keep only relevant columns and rename
  output <- intersect %>%
    select(all_of(c(reporting_geoid, reference_geoid, "weight"))) %>%
    rename_with(
      .fn = ~ ifelse(. == reporting_geoid, paste0("GISJOIN_", y),
                     ifelse(. == reference_geoid, "GISJOIN_1950", .)),
      .cols = everything()
    )  %>% 
    # drop 0 weights...
    filter(weight > 0) %>% 
    # drop geometry
    st_drop_geometry()
  
  # Save the output
  write_csv(output, file.path(output_path, output_fname))
}
