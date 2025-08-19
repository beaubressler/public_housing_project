####
# Interactive Census Tract Explorer
# This script creates interactive maps to explore census tract codes
# and identify the correct NHGISST and NHGISCTY codes for NYC
####

library(sf)
library(dplyr)
library(here)
library(mapview)

# Function to create interactive map for exploring tract codes
explore_tract_codes <- function(year) {
  
  # Construct file path
  shapefile_path <- here("data", "raw", "nhgis", "gis", 
                        paste0("nhgis0027_shapefile_tl2000_us_tract_", year),
                        paste0("US_tract_", year, ".shp"))
  
  # Check if file exists
  if (!file.exists(shapefile_path)) {
    stop(paste("Shapefile not found for year", year, "at", shapefile_path))
  }
  
  # Read shapefile
  cat("Reading", year, "census tract shapefile...\n")
  tracts <- st_read(shapefile_path, quiet = TRUE)
  
  # Transform to WGS84 for mapview
  tracts <- st_transform(tracts, crs = 4326)
  
  cat("Total tracts loaded:", nrow(tracts), "\n")
  
  # Show unique state and county codes
  cat("\nUnique NHGISST codes:\n")
  print(sort(unique(tracts$NHGISST)))
  
  cat("\nUnique NHGISCTY codes (first 30):\n")
  print(head(sort(unique(tracts$NHGISCTY)), 30))
  
  # Create subset around NYC area for faster loading
  # Let's focus on the northeast US
  bbox_nyc <- st_bbox(c(xmin = -75, ymin = 40, xmax = -73, ymax = 42), crs = 4326)
  nyc_area <- st_crop(tracts, bbox_nyc)
  
  cat("\nTracts in NYC area bbox:", nrow(nyc_area), "\n")
  
  if (nrow(nyc_area) > 0) {
    cat("NHGISST codes in NYC area:\n")
    print(unique(nyc_area$NHGISST))
    
    cat("NHGISCTY codes in NYC area:\n") 
    print(sort(unique(nyc_area$NHGISCTY)))
  }
  
  # Create interactive map
  cat("\nCreating interactive mapview...\n")
  cat("Click on tracts to see their NHGISST and NHGISCTY codes\n")
  
  # Create the mapview with popup showing codes
  m <- mapview(nyc_area, 
               zcol = "NHGISST",
               legend = TRUE,
               popup = leafpop::popupTable(nyc_area, 
                                          zcol = c("NHGISST", "NHGISCTY", "GISJOIN"),
                                          feature.id = FALSE,
                                          row.numbers = FALSE),
               layer.name = paste("Tracts", year),
               map.types = c("OpenStreetMap", "Esri.WorldImagery"))
  
  return(m)
}

# Function to explore specific state
explore_state_tracts <- function(year, nhgisst_code) {
  
  # Construct file path
  shapefile_path <- here("data", "raw", "nhgis", "gis", 
                        paste0("nhgis0027_shapefile_tl2000_us_tract_", year),
                        paste0("US_tract_", year, ".shp"))
  
  # Read shapefile
  tracts <- st_read(shapefile_path, quiet = TRUE)
  
  # Filter for specific state
  state_tracts <- tracts %>%
    filter(NHGISST == nhgisst_code) %>%
    st_transform(crs = 4326)
  
  cat("Found", nrow(state_tracts), "tracts for NHGISST:", nhgisst_code, "\n")
  
  if (nrow(state_tracts) > 0) {
    cat("NHGISCTY codes in this state:\n")
    county_counts <- table(state_tracts$NHGISCTY)
    print(county_counts)
  }
  
  # Create interactive map
  m <- mapview(state_tracts, 
               zcol = "NHGISCTY",
               legend = TRUE,
               popup = leafpop::popupTable(state_tracts, 
                                          zcol = c("NHGISST", "NHGISCTY", "GISJOIN"),
                                          feature.id = FALSE,
                                          row.numbers = FALSE),
               layer.name = paste("State", nhgisst_code, year),
               map.types = c("OpenStreetMap", "Esri.WorldImagery"))
  
  return(m)
}

# Function to show available codes and create basic info
show_available_codes <- function(year) {
  
  # Construct file path
  shapefile_path <- here("data", "raw", "nhgis", "gis", 
                        paste0("nhgis0027_shapefile_tl2000_us_tract_", year),
                        paste0("US_tract_", year, ".shp"))
  
  # Read shapefile
  tracts <- st_read(shapefile_path, quiet = TRUE)
  
  cat("Available columns:\n")
  print(names(tracts))
  
  cat("\nUnique NHGISST codes:\n")
  print(sort(unique(tracts$NHGISST)))
  
  cat("\nFor each state, number of tracts:\n")
  state_counts <- table(tracts$NHGISST)
  print(state_counts)
  
  # Check for NY state code 360
  if ("360" %in% tracts$NHGISST) {
    ny_tracts <- tracts %>% filter(NHGISST == "360")
    cat("\nFor NY state (NHGISST = 360), NHGISCTY codes:\n")
    county_counts <- table(ny_tracts$NHGISCTY)
    print(county_counts)
    
    # Check specifically for NYC boroughs
    cat("\nNYC Borough NHGISCTY codes (approximate):\n")
    cat("- Bronx: 005\n")
    cat("- Kings (Brooklyn): 047\n") 
    cat("- New York (Manhattan): 061\n")
    cat("- Queens: 081\n")
    cat("- Richmond (Staten Island): 085\n")
  }
  
  return(tracts)
}

# Main execution examples
if (interactive()) {
  cat("Census Tract Code Explorer\n")
  cat("=========================\n\n")
  
  # First, let's see what codes are available
  cat("Checking available codes in 1950 data...\n")
  tracts <- show_available_codes(1940)
  
  # Then explore interactively
  cat("\nCreating interactive map for 1950...\n")
  map_1950 <- explore_tract_codes(1940)
  print(map_1950)
  
  # If we know the state code, explore that state
  # map_ny <- explore_state_tracts(1950, "360")
  # print(map_ny)
}

# Usage:
show_available_codes(1950)         # Show all available codes
explore_tract_codes(1950)          # Interactive map of NYC area
explore_state_tracts(1950, "360")  # Explore specific state
