####
# Construct Enumeration District (ED) to Census Tract Crosswalk Using Area Weights
####
# This script generates crosswalks that map historical enumeration districts (EDs) 
# from 1930 and 1940 to 1950 census tracts using area-based reweighting.
# 
# The approach is adapted from Fabian Eckert et al. (2020) and follows these steps:
# 1. Reads historical ED shapefiles for each city and year.
# 2. Computes intersections between EDs and 1950 census tracts.
# 3. Assigns weights based on the proportion of each ED’s area within a 1950 tract.
# 4. Normalizes weights to ensure they sum to 1 for each ED.
# 5. Saves the resulting crosswalk as a CSV and the ED geometries as a shapefile.
#
# The script has been optimized using `map()` and `map_dfr()` to improve efficiency.
####


# Preliminaries ----
library(tidyverse)
library(sf)
library(here)
library(purrr)

# Define file paths and variables
reference_path <- here("data", "raw", "nhgis", "gis", "nhgis0027_shapefile_tl2000_us_tract_1950")
reference_fname <- "US_tract_1950.shp"
reference_geoid <- "GISJOIN"

# Define output path
data_dir <- here("data", "raw", "urban_transitions", "enumeration_districts")
output_path <- here("data", "derived", "geographic_crosswalks", "ed_to_tract")

# Read in the reference shapefile: The 1950 census tracts
shp_reference <- st_read(file.path(reference_path, reference_fname))

# Loop through the years
#for (y in c("1930", "1940", "1950", "1960", "1970", "1980")) {
# list of cities for which I have data
# cities <- c(
#   "AkronOH", "AlbanyNY", "BaltimoreMD", "BirminghamAL", "BostonMA", "BridgeportCT", 
#   "BuffaloNY", "ChattanoogaTN", "ChicagoIL", "CincinnatiOH", "ClevelandOH", 
#   "ColumbusOH", "DallasTX", "DaytonOH", "DenverCO", "DesMoinesIA", "FlintMI", 
#   "FortWorthTX", "GrandRapidsMI", "HartfordCT", "HoustonTX", "IndianapolisIN", 
#   "JacksonvilleFL", "JerseyCityNJ", "KansasCityKS", "LosAngelesCA", "LouisvilleKY", 
#   "MiamiFL", "MilwaukeeWI", "MinneapolisMN", "NashvilleTN", "NewHavenCT", 
#   "NewOrleansLA", "NewarkNJ", "OaklandCA", "OklahomaCityOK", "OmahaNE", 
#   "PhiladelphiaPA", "PittsburghPA", "SanAntonioTX", "SanFranciscoCA", 
#   "SeattleWA", "SpokaneWA", "SpringfieldMA", "StLouisMO", "StPaulMN", 
#   "SyracuseNY", "TrentonNJ", "TulsaOK", "WashingtonDC", "WorcesterMA", 
#   "YonkersNY"
# )

# dropping Houston and Salt Lake City, which have typos in the names of the directories

cities <- c(
  "AkronOH", "AlbanyNY", "AtlantaGA", "BaltimoreMD", "BirminghamAL", "BostonMA", 
  "BridgeportCT", "BronxNY", "BrooklynNY", "BuffaloNY", "ChattanoogaTN", 
  "ChicagoIL", "CincinnatiOH", "ClevelandOH", "ColumbusOH", "DallasTX", 
  "DaytonOH", "DenverCO", "DesMoinesIA", "DetroitMI", "FlintMI", "FortWorthTX", 
  "GrandRapidsMI", "HartfordCT", "HoustonTX",  "IndianapolisIN", "JacksonvilleFL", 
  "JerseyCityNJ", "KansasCityKS", "KansasCityMO", "LongBeachCA", "LosAngelesCA", 
  "LouisvilleKY", "ManhattanNY", "MemphisTN", "MiamiFL", "MilwaukeeWI", 
  "MinneapolisMN", "NashvilleTN", "NewHavenCT", "NewOrleansLA", "NewarkNJ", 
  "NorfolkVA", "OaklandCA", "OklahomaCityOK", "OmahaNE", "PatersonNJ", 
  "PhiladelphiaPA", "PittsburghPA", "PortlandOR", "ProvidenceRI", "QueensNY", 
  "RichmondVA", "RochesterNY","SaltLakeCityUT",  "SanAntonioTX", "SanDiegoCA", 
  "SanFranciscoCA", "ScrantonPA", "SeattleWA", "SpokaneWA", "SpringfieldMA", 
  "StLouisMO", "StPaulMN", "StatenIslandNY", "SyracuseNY", "ToledoOH", 
  "TrentonNJ", "TulsaOK", "WashingtonDC", "WorcesterMA", "YonkersNY", 
  "YoungstownOH")
  
years <- c(30, 40)


# 1/31/2025: Vectorized version of the loop

process_city_year <- function(city, y) {
  message(paste("Processing city:", city, "for year:", y))
  
  state <- substr(city, nchar(city) - 1, nchar(city))  # Extract state
  city_name <- substr(city, 1, nchar(city) - 2)        # Extract city name
  
  # Path to the outer ZIP file for the decade
  decade_zip <- file.path(data_dir, "ForEachDecade", paste0("ForEachCity19", y, ".zip"))
  if (!file.exists(decade_zip)) {
    message(paste("Decade ZIP file not found:", decade_zip))
    return(NULL)
  }
  temp_decade_dir <- tempdir()
  unzip(decade_zip, exdir = temp_decade_dir)
  
  # Path to the inner city-specific ZIP file
  city_zip <- file.path(temp_decade_dir, paste0("ForEachCity19", y), paste0(city, y, ".zip"))
  if (!file.exists(city_zip)) {
    message(paste("City ZIP file not found:", city_zip))
    return(NULL)
  }
  temp_city_dir <- tempdir()
  unzip(city_zip, exdir = temp_city_dir)
  
  # Path to the ED shapefile
  shp_file <- file.path(temp_city_dir, paste0(city, y), paste0(city, "_ed", y, "_aggr.shp"))
  if (!file.exists(shp_file)) {
    message(paste("Shapefile not found:", shp_file))
    return(NULL)
  }
  
  # Read and process the ED shapefile
  shp_reporting_raw <- st_read(shp_file, quiet = TRUE) %>%
    st_make_valid() %>%
    mutate(area_base = st_area(.)) %>%
    # Drop missing or invalid EDs
    filter(!is.na(ed),
           ed != 0,
           !str_detect(ed, "(?i)x"),
           !str_detect(ed, "\\?")) %>% # drop upper or lower case x
    # convert EDs to upper case
    mutate(ed = toupper(ed))

  
  # Assign CRS if missing
  if (is.na(st_crs(shp_reporting_raw))) {
    st_crs(shp_reporting_raw) <- 4326  # Assign EPSG:4326
    message("Assigned WGS 84 CRS to shp_reporting.")
  }
  
  # Transform CRS to match reference
  shp_reporting <- st_transform(shp_reporting_raw, st_crs(shp_reference)) %>%
    st_make_valid()
  
  # Store ED geometries
  shp_reporting <- shp_reporting %>%
    mutate(city = city_name, state = state, year = paste0("19", y))  # Add metadata
  
  # Compute intersections and weights
  intersect <- st_intersection(shp_reporting, shp_reference) %>%
    mutate(area = st_area(.),
           weight = as.numeric(area) / as.numeric(area_base))
  
  # Normalize weights
  intersect <- intersect %>%
    group_by(ed) %>%
    mutate(total_weight = sum(weight),
           weight = weight / total_weight) %>%
    ungroup()
  
  # Keep only relevant columns
  crosswalk <- intersect %>%
    select(ed, reference_geoid, weight) %>%
    rename_with(~ ifelse(. == "ed", paste0("ed_19", y), ifelse(. == reference_geoid, "GISJOIN_1950", .))) %>%
    filter(weight > 0) %>%
    mutate(city = city_name, state = state) %>%
    st_drop_geometry()
  
  ed_geometry <- shp_reporting %>%
    select(ed, city, state, year, geometry)
  
  return(list(crosswalk = crosswalk, ed_geometries = ed_geometry))
}

# Apply `map_dfr()` to process all cities for a given year
process_year <- function(y) {
  message(paste("Processing all cities for year:", y))
  
  results <- purrr::map(cities, ~process_city_year(.x, y))
  
  # Extract and combine crosswalks
  combined_data <- purrr::map_dfr(results, "crosswalk")

    write_csv(combined_data, file.path(output_path, paste0("ed_19", y, "_to_1950_tracts.csv")))
  
  # Extract and combine ED geometries
  all_eds_sf <- bind_rows(purrr::map(results, "ed_geometries"))
  st_write(all_eds_sf, file.path(output_path, paste0("ed_19", y, "_geometries.shp")), delete_layer = TRUE)
}

# Run for all years using `map()`
purrr::map(years, process_year)


