####
# Prep GRF for geolocating
# There are two outputs here:
# 1. A list of all of the unique street addresses (which we will geocode)
# 2. A linkage between histids and street addresses (which we will link to after geocoding)
####

# Load necessary packages
library(tidyverse)
library(here)

# Define file paths
grf_dir <- here("data", "raw", "urban_transitions", "geographic_reference_file")
grf_output_dir <- here("data", "derived", "geographic_reference_file")

# Define cities and years to loop through
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

years <- c(1930, 1940)

# Loop through years
for (y in years) {
  
  # Process all cities for this year using `map_dfr()` (efficient looping)
  combined_data <- map_dfr(cities, function(c) {
    
    # Construct file path
    grf_file <- paste0(c, "_", y, ".csv")
    grf_path <- here(grf_dir, y, grf_file)
    
    # Check if file exists before trying to read
    if (!file.exists(grf_path)) {
      message("Skipping missing file: ", grf_path)
      return(NULL)  # Skip if file is missing
    }
    
    # Read the city-year GRF dataset
    grf <- read_csv(grf_path, show_col_types = FALSE) 
    
    # Process: Extract **histid** and full **street address**
    grf %>%
      select(histid, b_city, b_stfull, b_hn) %>% 
      mutate(
        state = str_sub(b_city, -2, -1),  # Extract last 2 characters as state
        city = str_sub(b_city, 1, -3),   # Extract everything before last 2 chars
        street_address = paste0(b_hn, " ", b_stfull, ", ", city, ", ", state),
        missing_street_number = is.na(b_hn),
        missing_address = is.na(b_stfull)
      ) %>% 
      select(histid, street_address, missing_street_number, missing_address) # Keep relevant columns
  })
  
  # Save full **histid-street address** dataset for this year
  write_csv(combined_data, here(grf_output_dir, paste0("grf_", y, "_histid_street.csv")))
  
  # Extract **unique addresses only** for geocoding
  unique_addresses <- combined_data %>%
    filter(!missing_street_number) %>%
    distinct(street_address)
  
  # Save unique street addresses separately
  write_csv(unique_addresses, here(grf_output_dir, paste0("grf_", y, "_unique_addresses.csv")))
  
}
