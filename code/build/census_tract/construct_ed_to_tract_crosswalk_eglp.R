####
# Construct Enumeration District (ED) to Census Tract Crosswalk Using Area Weights
####
# This script generates crosswalks that map historical enumeration districts (EDs) 
# from 1930 and 1940 to 1990 census tracts using area-based reweighting.
# 
# The approach is adapted from Fabian Eckert et al. (2020) and follows these steps:
# 1. Reads historical ED shapefiles for each city and year.
# 2. Computes intersections between EDs and 1990 census tracts.
# 3. Assigns weights based on the proportion of each ED’s area within a 1990 tract.
# 4. Normalizes weights to ensure they sum to 1 for each ED.
# 5. Saves the resulting crosswalk as a CSV and the ED geometries as a shapefile.
#
# The script has been optimized using `map()` and `map_dfr()` to improve efficiency.
####


# Preliminaries ----
library(tidyverse)
library(sf)
library(here)

# Define file paths and variables
reference_path <- here("data", "raw", "nhgis", "gis", "nhgis0027_shapefile_tl2000_us_tract_1990")
reference_fname <- "US_tract_1990.shp"
reference_geoid <- "GISJOIN"

# Define output path
data_dir <- here("data", "raw", "urban_transitions", "enumeration_districts")
output_path <- here("data", "derived", "geographic_crosswalks", "ed_to_tract")

# Read in the reference shapefile: The 1990 census tracts
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
    rename_with(~ ifelse(. == "ed", paste0("ed_19", y), ifelse(. == reference_geoid, "GISJOIN_1990", .))) %>%
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
  
  results <- map(cities, ~process_city_year(.x, y))
  
  # Extract and combine crosswalks
  combined_data <- map_dfr(results, "crosswalk", .id = "city")
  write_csv(combined_data, file.path(output_path, paste0("ed_19", y, "_to_1990_tracts.csv")))
  
  # Extract and combine ED geometries
  all_eds_sf <- bind_rows(map(results, "ed_geometries"))
  st_write(all_eds_sf, file.path(output_path, paste0("ed_19", y, "_geometries.shp")), delete_layer = TRUE)
}

# Run for all years using `map()`
map(years, process_year)



# # Loop through years: 30 and 40
# 
# 
# for (y in years) {
#   combined_data <- list() # Initialize list to combine crosswalk data for this year
#   all_eds_list <- list () # initialize list to store ED geometries
#   
#   for (city in cities) {
#     # Write to terminal which city and year we are working on
#     message(paste("Processing city:", city, "for year:", y))
#     
#     state <- substr(city, nchar(city) - 1, nchar(city))  # Extract state
#     city_name <- substr(city, 1, nchar(city) - 2)        # Extract city name
#     
#     # 1. Path to the outer ZIP file for the decade
#     decade_zip <- file.path(data_dir, "ForEachDecade", paste0("ForEachCity19", y, ".zip"))
#     if (!file.exists(decade_zip)) {
#       message(paste("Decade ZIP file not found:", decade_zip))
#       next
#     }
#     temp_decade_dir <- tempdir()
#     unzip(decade_zip, exdir = temp_decade_dir)
#     
#     # 2. Path to the inner city-specific ZIP file
#     city_zip <- file.path(temp_decade_dir, paste0("ForEachCity19", y), paste0(city, y, ".zip"))
#     if (!file.exists(city_zip)) {
#       message(paste("City ZIP file not found:", city_zip))
#       next
#     }
#     temp_city_dir <- tempdir()
#     unzip(city_zip, exdir = temp_city_dir)
#     
#     # 3. Path to the ED shapefile within the unzipped city directory
#     shp_file <- file.path(temp_city_dir, paste0(city, y), paste0(city, "_ed", y, "_aggr.shp"))
#     if (!file.exists(shp_file)) {
#       message(paste("Shapefile not found:", shp_file))
#       next
#     }
#     
#     # Read and process the ED shapefile
#     shp_reporting_raw <- st_read(shp_file, quiet = TRUE) %>%
#       st_make_valid() %>%
#       mutate(area_base = st_area(.))
#     
#     # data for historical ED data;
#     # 0. Drop if ED is missing (NA or 0)
#     shp_reporting <- shp_reporting_raw %>%
#       filter(!is.na(ed) & ed != 0)
#     
#     # Assign CRS if missing
#     if (is.na(st_crs(shp_reporting))) {
#       st_crs(shp_reporting) <- 4326  # Assign EPSG:4326
#       message("Assigned WGS 84 CRS to shp_reporting.")
#     }
#     # set to same CRS as reference
#     shp_reporting <- st_transform(shp_reporting, st_crs(shp_reference)) %>%
#       st_make_valid()
#     shp_reference <- st_make_valid(shp_reference)
#     
#     # Store ED geometries for this city
#     shp_reporting <- shp_reporting %>%
#       mutate(city = city_name, state = state, year = paste0("19", y))  # Add metadata
#     all_eds_list[[city]] <- shp_reporting
#     
#     # Compute intersections and weights
#     intersect <- st_intersection(shp_reporting, shp_reference) %>%
#       mutate(area = st_area(.)) %>%
#       mutate(weight = as.numeric(area) / as.numeric(area_base))
#     
#     # Renormalize weights to ensure that weights sum up to 1 for each unique reporting_geoid
#     # This is important here, because there are mapping issues in which EDs overlap, and so
#     # we need to normalize the weights to sum to only 1
#     intersect <- intersect %>%
#       group_by(ed) %>%
#       mutate(total_weight = sum(weight)) %>%
#       mutate(weight = weight / total_weight) %>%
#       ungroup()
#     
#     # Keep only relevant columns and rename
#     output <- intersect %>%
#       select(ed, reference_geoid, weight) %>%
#       rename_with(~ ifelse(. == "ed", paste0("ed_19", y),
#                            ifelse(. == reference_geoid, "GISJOIN_1990", .))) %>%
#       # drop 0 weights...
#       filter(weight > 0) %>%
#       # Add city and state columns
#       mutate(city = city_name, state = state) %>%
#       # drop geometry
#       st_drop_geometry()
#     
#     combined_data <- bind_rows(combined_data, output)
#   }
#   
#   # Save combined data for the year
#   write_csv(combined_data, file.path(output_path, paste0("ed_19", y, "_to_1990_tracts.csv")))
#   
#   # Combine all ED geometries for this year
#   all_eds_sf <- bind_rows(all_eds_list)
#   
#   # Save ED geometries as a shapefile
#   st_write(all_eds_sf, output_path, delete_layer = TRUE)
#   
# }



## OLD Directory structure (Pre 12/2024)
# years <- c("30", "40")
# 
# 
# # loop through years: 30 and 40
# for (y in years) {
#   combined_data <- list() # Initialize a list to combine data for this year
#   
#   for (city in cities) { # loop through cities
#   
#     state <- substr(city, nchar(city) - 1, nchar(city))
#     city_name <- substr(city, 1, nchar(city) - 2)
#     
#     # Unzip the main city zip file
#     city_zip_file <- here("data", "raw", "urban_transitions", "enumeration_districts", 
#                           "ForEachDecade", paste0(city, ".zip"))
#     if (!file.exists(city_zip_file)) {
#       message(paste("City zip file not found:", city_zip_file))
#       next
#     }
#     temp_city_dir <- tempdir()
#     unzip(city_zip_file, exdir = temp_city_dir)
#     
#     # Step 2: Unzip the year-specific zip file within the city zip
#     year_zip_file <- file.path(temp_city_dir, paste0(city, y, ".zip"))
#     if (!file.exists(year_zip_file)) {
#       message(paste("Year zip file not found:", year_zip_file))
#       next  # Skip to the next city
#     }
#     temp_year_dir <- tempdir()
#     unzip(year_zip_file, exdir = temp_year_dir)
#     
#     shp_file <- file.path(temp_year_dir, paste0(city, y), paste0(city, "_ed", y, "_aggr.shp"))
# 
#     # read in enumeration district file
#     shp_reporting_raw <- st_read(shp_file, quiet = TRUE)  %>% 
#       st_make_valid() %>% 
#       # calculate area
#       mutate(area_base = st_area(.)) 
#     
#     # map the shp_reporting, add ed number as label s(ed variable)
#     # ggplot() + geom_sf(data = shp_reporting_raw) +
#     #   geom_sf_text(data = shp_reporting_raw, aes(label = ed), size = 1.5)
#     
#     # data for historical ED data;
#     # 0. Drop if ED is missing (NA or 0)
#     # 1. Drop ED if there are very few people in it (actually, not doing this)
#     shp_reporting <- 
#       shp_reporting_raw %>%
#       # drop if enumeration district is missing (NA or 0)
#       filter(!is.na(ed) & ed != 0) %>% 
#       # drop if total pop is less than 10  
#       # Why? We don't want to dilute the weights of other EDs using basically empty EDs 
#       filter(totalpop >= 10)
#     
#     
#     # if missing CRS, assign to the CRS ED should be in (EPSG: 4326)
#     if (is.na(st_crs(shp_reporting))) {
#       st_crs(shp_reporting) <- 4326  # Assign WGS 84 CRS (EPSG:4326)
#       message("Assigned WGS 84 CRS to shp_reporting.")
#     }
#     
#     # Convert to same CRS: Specifically, convert ED to tract CRS
#     if (st_crs(shp_reporting) != st_crs(shp_reference)) {
#       shp_reporting <- st_transform(shp_reporting, st_crs(shp_reference))
#     }
#     
#     
#     # ensure geometries are valid
#     shp_reporting <- st_make_valid(shp_reporting)
#     shp_reference <- st_make_valid(shp_reference)
#     
#     # Check for self-intersections or overlaps in reporting shapefile
#     # overlap_check_reporting <- st_overlaps(shp_reporting, shp_reporting, sparse = FALSE)
#     # sum(overlap_check_reporting[lower.tri(overlap_check_reporting)])
#     # 
#     # overlapping_indices <- which(overlap_check_reporting, arr.ind = TRUE)
#     # 
#     # # Extract the unique IDs or rows of overlapping geometries
#     # overlapping_geometries <- shp_reporting[unique(overlapping_indices[, 1]), ]
#     
#   
#     # Compute intersections and weights
#     intersect <- st_intersection(shp_reporting, shp_reference) %>%
#       mutate(area = st_area(.)) %>% 
#       mutate(weight = as.numeric(area) / as.numeric(area_base))
#     
#     # Renormalize weights to ensure that weights sum up to 1 for each unique reporting_geoid
#     # This is important here, because there are mapping issues in which EDs overlap, and so
#     # we need to normalize the weights to sum to only 1
#     intersect <- intersect %>%
#       group_by(ed) %>%
#       mutate(total_weight = sum(weight)) %>% 
#       mutate(weight = weight / total_weight) %>%
#       ungroup()
#     
#     # Keep only relevant columns and rename
#     output <- intersect %>%
#       select(ed, reference_geoid, weight) %>%
#       rename_with(~ ifelse(. == "ed", paste0("ed_19", y),
#                            ifelse(. == reference_geoid, "GISJOIN_1990", .))) %>%
#       # drop 0 weights...
#       filter(weight > 0) %>% 
#       # Add city and state columns
#       mutate(city = city_name, state = state) %>%  
#       # drop geometry
#       st_drop_geometry()
#     
#     # append each city data to the combined data
#     combined_data <- bind_rows(combined_data, output)
#   }
#   
#   # Combine data for all cities in the year and save as a single CSV
#   yearly_output <- bind_rows(combined_data)
#   
#   write_csv(yearly_output, file.path(output_path, paste0("ed_19", y, "_to_1990_tracts.csv")))
#   
# }
