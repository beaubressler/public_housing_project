### Construct ed to tract crosswalk using area reweighting 


###
# This is an adaptation of the area-reweighting crosswalking Python code provided by Fabian Eckert et al 2020
# Creates area re-weighting crosswalks between 1940 Census tracts and 1990 Census tracts

###


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

# Loop through years: 30 and 40


for (y in years) {
  combined_data <- list()  # Initialize list to combine data for this year
  
  for (city in cities) {
    state <- substr(city, nchar(city) - 1, nchar(city))  # Extract state
    city_name <- substr(city, 1, nchar(city) - 2)        # Extract city name
    
    # 1. Path to the outer ZIP file for the decade
    decade_zip <- file.path(data_dir, "ForEachDecade", paste0("ForEachCity19", y, ".zip"))
    if (!file.exists(decade_zip)) {
      message(paste("Decade ZIP file not found:", decade_zip))
      next
    }
    temp_decade_dir <- tempdir()
    unzip(decade_zip, exdir = temp_decade_dir)
    
    # 2. Path to the inner city-specific ZIP file
    city_zip <- file.path(temp_decade_dir, paste0("ForEachCity19", y), paste0(city, y, ".zip"))
    if (!file.exists(city_zip)) {
      message(paste("City ZIP file not found:", city_zip))
      next
    }
    temp_city_dir <- tempdir()
    unzip(city_zip, exdir = temp_city_dir)
    
    # 3. Path to the ED shapefile within the unzipped city directory
    shp_file <- file.path(temp_city_dir, paste0(city, y), paste0(city, "_ed", y, "_aggr.shp"))
    if (!file.exists(shp_file)) {
      message(paste("Shapefile not found:", shp_file))
      next
    }
    
    # Read and process the ED shapefile
    shp_reporting_raw <- st_read(shp_file, quiet = TRUE) %>%
      st_make_valid() %>%
      mutate(area_base = st_area(.))
    
    # data for historical ED data;
    # 0. Drop if ED is missing (NA or 0)
    # # 1. Drop ED if there are very few people in it (acutally not doing this now)
      shp_reporting <- shp_reporting_raw %>%
      filter(!is.na(ed) & ed != 0)
    
    # Assign CRS if missing
    if (is.na(st_crs(shp_reporting))) {
      st_crs(shp_reporting) <- 4326  # Assign EPSG:4326
      message("Assigned WGS 84 CRS to shp_reporting.")
    }
    # set to same CRS as reference
    shp_reporting <- st_transform(shp_reporting, st_crs(shp_reference)) %>%
      st_make_valid()
    shp_reference <- st_make_valid(shp_reference)
    
    # Compute intersections and weights
    intersect <- st_intersection(shp_reporting, shp_reference) %>%
      mutate(area = st_area(.)) %>%
      mutate(weight = as.numeric(area) / as.numeric(area_base))
    
    # Renormalize weights to ensure that weights sum up to 1 for each unique reporting_geoid
    # This is important here, because there are mapping issues in which EDs overlap, and so
    # we need to normalize the weights to sum to only 1
    intersect <- intersect %>%
      group_by(ed) %>%
      mutate(total_weight = sum(weight)) %>%
      mutate(weight = weight / total_weight) %>%
      ungroup()
    
    # Keep only relevant columns and rename
    output <- intersect %>%
      select(ed, reference_geoid, weight) %>%
      rename_with(~ ifelse(. == "ed", paste0("ed_19", y),
                           ifelse(. == reference_geoid, "GISJOIN_1990", .))) %>%
      # drop 0 weights...
      filter(weight > 0) %>%
      # Add city and state columns
      mutate(city = city_name, state = state) %>%
      # drop geometry
      st_drop_geometry()
    
    combined_data <- bind_rows(combined_data, output)
  }
  
  # Save combined data for the year
  write_csv(combined_data, file.path(output_path, paste0("ed_19", y, "_to_1990_tracts.csv")))
}



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
