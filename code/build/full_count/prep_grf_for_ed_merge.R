###
#  Create list of histids (individual ID), ed number, and city 
# Prep the Geographic Reference File 
####


library(tidyverse)
library(here)

# Define file paths and variables
grf_dir <- here("data", "raw", "urban_transitions", "geographic_reference_file")
grf_output_dir <- here("data", "derived", "geographic_reference_file")


# list of cities for which I have ED map data

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

# loop through years: 1930 and 1940
for (y in years) {
  combined_data <- list() # Initialize a list to combine data for this year

  for (c in cities) {
    
    # Construct file paths
    grf_file <- paste0(c, "_", y, ".csv")
    grf_path <- here(grf_dir, y, grf_file)
    
    # Read in the ED shapefile 
    grf <- read_csv(grf_path)
    
    # Create list of histids (individual ID), ed number, and city 
    histid_ed_city <- grf %>% 
      select(histid, b_ed, b_city, b_hh) %>% 
      # convert all columns to character
      mutate_all(as.character) 
    
    # Append
    combined_data <- bind_rows(combined_data, histid_ed_city)
    
  }
  # Save the list of histids (individual ID), ed number, and city 
  write_csv(combined_data, here(grf_output_dir, paste0("grf_", y, "_histid_ed_city.csv")))
}
