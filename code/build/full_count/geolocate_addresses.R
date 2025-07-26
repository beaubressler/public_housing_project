####
# OLD:  No longer using this!! Only geolocating using Streetmap Premium
# Script for geocoding the full count census !!!!
#####

# Premilinaries----
library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)
library(parallel)  # For parallel processing

# Directories
grf_output_dir <- here("data", "derived", "geographic_reference_file")

# Read in addresses
# Load the addresses (CSV should have an "address" column)
addresses <- read_csv(here(grf_output_dir, "grf_1940_histid_street.csv"))


addresses_sample <- 
  head(addresses, 10000) %>% 
  mutate(add = paste0(b_hn, " ", b_stfull),
         country = "USA") %>% 
  filter(!is.na(b_hn))

# Pelias Structured API URL
api_url <- "http://localhost:4000/v1/search/structured"


# Function to perform structured geocoding
geocode_pelias <- function(address, locality, region, country) {
  # Print query parameters for debugging
  print(paste0("Querying: ", address, ", ", locality, ", ", region, ", ", country))
  
  # Format query parameters
  query_params <- list(
    address = address,
    locality = locality,
    region = region,
    country = country
  )
  
  # Generate full request URL
  request_url <- modify_url(api_url, query = query_params)
  print(paste0("Full Request URL: ", request_url))  # 🔍 Debug: Print full URL
  
  # Make API request
  response <- tryCatch({
    GET(api_url, query = query_params, timeout(5))
  }, error = function(e) {
    print("❌ Error making request")
    return(NULL)
  })
  
  # If request failed, return NA
  if (is.null(response) || status_code(response) != 200) {
    return(c(NA, NA))
  }
  
  # Extract and parse API response
  raw_json <- content(response, "text", encoding = "UTF-8")
  print(paste0("Raw API Response: ", raw_json))  # 🔍 Debug: Print API response
  
  # Convert JSON response to R object safely
  data <- tryCatch({
    fromJSON(raw_json, simplifyVector = FALSE)
  }, error = function(e) {
    print("❌ Error parsing JSON")
    return(NULL)
  })
  
  # If JSON parsing failed, return NA
  if (is.null(data) || is.null(data$features) || length(data$features) == 0 || is.null(data$features[[1]])) {
    return(c(NA, NA))  # Return NA if no result found
  }
  
  # Extract lat/lon
  coords <- data$features[[1]]$geometry$coordinates
  print(paste0("✅ Extracted lat/lon: ", coords[2], ", ", coords[1]))  # 🔍 Debug: Show extracted coords
  
  return(c(coords[2], coords[1]))  # (lat, lon)
}


# Parallel processing for speed
num_cores <- detectCores() - 1
results <- mcmapply(geocode_pelias,
                    address = addresses_sample$add,
                    locality = addresses_sample$city,
                    region = addresses_sample$state,
                    country = addresses_sample$country,
                    mc.cores = num_cores)

results_t <- as.data.frame(t(results))  
# Rename columns to "lat" and "lon"
colnames(results_t) <- c("lat", "lon")


# Convert to data frame and save
#TODO fix
x <- cbind(addresses_sample, results_t)
colnames(addresses)[5:6] <- c("lat", "lon")
write.csv(addresses, "structured_geocoded_addresses.csv", row.names = FALSE)
