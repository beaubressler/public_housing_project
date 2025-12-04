###
# This is code for cleaning my digitized data 
# In particular, here I will geocode addresses in the data

# For projects where I have a set of street boundaries instead of a particular address,
# I will geocode the first 2 and the last 2 streets

library(tidyverse)
library(readxl)
#library(osrm)
library(tidygeocoder)
library(haven)
# google maps
library(mapsapi)

library(osmdata)
library(sf)
# places api
library(googleway)

library(here)


digitization_dir <- "data/digitization/"
out_dir <- "data/derived/public_housing/working/"

# My (Beau Bressler's) Google Maps project API key 
# Note: Replicator would need their own
google_maps_api_key <- 'AIzaSyAKOGqu6O3qASvrrnjQ0GFMn6ChlcjJ_QA'

# set google api key
Sys.setenv(GOOGLEGEOCODE_API_KEY = google_maps_api_key)

# Read CDD data 
cdd_data <- read_dta("data/raw/hud_consolidated_directory/CDD_1973.dta")

# Prep geographic functions ----

# Function to calculate midpoint between two points
# Note: Could just take average lat and long
calculate_midpoint <- function(lon1, lat1, lon2, lat2) {
  # Create points
  point1 <- st_point(c(lon1, lat1))
  point2 <- st_point(c(lon2, lat2))
  
  # Create a linestring
  line <- st_sfc(st_linestring(rbind(c(lon1, lat1), c(lon2, lat2))), crs = 4326)
  
  # Use st_segmentize to add points along the great circle path
  segmented_line <- st_segmentize(line, dfMaxLength = 0.1)  # 0.1 degrees, adjust as needed
  
  # Extract all points and find the middle one
  points <- st_cast(segmented_line, "POINT")
  midpoint <- points[ceiling(length(points)/2)]
  
  return(st_coordinates(midpoint))
}



# read in public HUD data with addresses -----
psh_1997_raw <- read_xls("data/raw/pic97/HUD3_cleaned.xls")

psh_1997 <-
  psh_1997_raw %>% 
  # lower case column names
  rename_all(tolower) %>%
  # keep only columns we need
  select(-contains("pct")) %>% 
  # rename lat and long
  rename(latitude = lat, longitude = long) %>%
  # remove spaces from project code
  mutate(code = str_remove_all(code, " ")) %>% 
  # remove "_" from project code
  mutate(code = str_remove_all(code, "_")) %>%
  # remove "{" from project code
  mutate(code = str_remove_all(code, "\\{")) %>%
  # remove "-" from project code
  mutate(code = str_remove_all(code, "-")) %>%
  # count number of characters in project code
  mutate(code_length = str_length(code)) %>% 
  # drop any code with less than 7 characters
  filter(code_length >= 7) %>%
  mutate(state_code = str_sub(code, 1, 2),
         #city_code is next 3 digits of  code
         city_code = as.numeric(str_sub(code, 3, 5)),
         # site_code is the remaining digits of code
         site_code = str_sub(code, 6, -1),
         # site_code is the remaining digits of code
         site_code = str_sub(code, 6, -1),
         # if site_code contains a letter, remove the letter
         site_code = if_else(str_detect(site_code, "[A-Z]"), str_remove_all(site_code, "[A-z]+$"), site_code),
         # convert site_code to numeric
         site_code = as.numeric(site_code),
         # site_letter is the last digits of code (no matter the length), only if it's a letter, otherwise NA
         site_letter = if_else(str_detect(str_sub(code, -1, -1), "[A-Z]"), str_extract(code, "[A-Za-z]+$"), NA_character_),
         # project code is state_code - city_code - site_code
         project_code = paste0(state_code, "-", city_code, "-", site_code),
         # project_code_full is state_code - city_code - site_code + site_letter if site_letter is not NA
         project_code_full = if_else(is.na(site_letter), project_code, paste0(project_code, "-", site_letter)))

# psh_2000
psh_2000_raw <- read_xlsx("data/raw/pic2000/all_projects_2000.xlsx")

psh_2000 <- 
psh_2000_raw %>% 
  # keep only columns we need
  select(-contains("pct")) %>% 
  # keep only public housing
  filter(program_label == "PH") %>% 
  # state_code is first 2 characters of  code
  mutate(state_code = str_sub(code, 1, 2),
         #city_code is next 3 digits of  code
         city_code = as.numeric(str_sub(code, 3, 5)),
         # site_code is the remaining digits of code
         site_code = str_sub(code, 6, -1),
         # if site_code contains a letter, remove the letter
         site_code = if_else(str_detect(site_code, "[A-Z]"), str_remove_all(site_code, "[A-z]+$"), site_code),
         # convert site_code to numeric
         site_code = as.numeric(site_code),
         # site_letter is the last digits of code (no matter the length), only if it's a letter, otherwise NA
         site_letter = if_else(str_detect(str_sub(code, -1, -1), "[A-Z]"), str_extract(code, "[A-Za-z]+$"), NA_character_),
         # project code is state_code - city_code - site_code
         project_code = paste0(state_code, "-", city_code, "-", site_code),
         # project_code_full is state_code - city_code - site_code + site_letter if site_letter is not NA
         project_code_full = if_else(is.na(site_letter), project_code, paste0(project_code, "-", site_letter)))



# Geolocate addresses from digitization using Google Maps API ----
## Chicago -----
chicago_raw <- read_xlsx(paste0(digitization_dir, "chicago/digitization_annual_statistical_report_1973.xlsx"))

chicago_data <- 
  chicago_raw %>% 
  # exclude scattered sites
  filter(!str_detect(address_or_streets, "Scattered")) %>%
  # include city and state
  mutate(address_clean = paste0(address_or_streets, ", Chicago, IL")) %>% 
  # clean the year-completed column: Use first year completed (sometimes theres an annex which was built later)
  mutate(year_completed_raw = year_completed,
         year_completed = as.numeric(str_extract(year_completed, "[0-9]{4}"))) %>% 
  mutate(total_acres = as.numeric(total_acres),
         total_units = as.numeric(total_units)) %>% 
  # create project with name
  mutate(project_name_with_city = paste0(project_name, ", Chicago, IL"))
    



# geocode: Note, this gets all of the addresses
chicago_geocoded_google <- 
  chicago_data %>%
  geocode(address_clean, method = "google", lat = "lat_address", long = "long_address") %>% 
  geocode(project_name_with_city, method = "google", lat = "lat_name_with_city", long = "long_name_with_city") %>% 
  # reverse geocode to make sure the address makes sense
  reverse_geocode(lat_address, long_address, address = "reverse_geocoded_address") %>% 
  reverse_geocode(lat_name_with_city, long_name_with_city, address = "reverse_geocoded_address_name_with_city")

# calculate distance between the two geocoded addresses
chicago_geocoded_google <-
  chicago_geocoded_google %>% 
  mutate(
    geom_address = map2(long_address, lat_address, ~ st_point(c(.x, .y))),
    geom_name_with_city = map2(long_name_with_city, lat_name_with_city, ~ st_point(c(.x, .y)))
  ) %>%
  # Convert the list of points to sfc objects with the correct CRS (EPSG:4326)
  mutate(
    geom_address = st_sfc(geom_address, crs = 4326),
    geom_name_with_city = st_sfc(geom_name_with_city, crs = 4326)
  ) %>%
  # Calculate the distance between the two geometries for each row
  mutate(distance_m = st_distance(geom_address, geom_name_with_city, by_element = TRUE),
         distance_km = as.numeric(distance_m) / 1000) %>% 
  mutate(
    lat_long_address = paste(lat_address, long_address, sep = ", "),
    lat_long_name_with_city = paste(lat_name_with_city, long_name_with_city, sep = ", ")
  ) %>% 
  # identify particular places where we should use the "name" based geolocation
  # Prticularly, checked cases where difference between the two geocoded addresses was > 1 km
  mutate(lat = 
           case_when (project_name == "Philip Murray Homes" ~ lat_name_with_city,
                      project_name == "Stateway Gardens" ~ lat_name_with_city,
                      project_name == "Clark & Irving Apts." ~ lat_name_with_city,
                      .default = lat_address
           ),
  long =
           case_when (project_name == "Philip Murray Homes" ~ long_name_with_city,
                      project_name == "Stateway Gardens" ~ long_name_with_city,
                      project_name == "Clark & Irving Apts." ~ long_name_with_city,
                      .default = long_address
           )) %>% 
  # manual fixes: Identified these through looking at google maps
  # Particularly, checked cases where difference between the two geocoded addresses was > 1 km
  mutate(lat_fix = case_when(
    project_name == "Washington Park Homes" & address_or_streets == "57th-Stewart : 58th-Normal" ~ 41.78868,
    project_name == "Raymond M. Hilliard Center" ~ 41.85350,
    project_name == "Armour Sq. Apts. & Annex" ~ 41.83510,
    project_name == "Harold L. Ickes Homes" ~ 41.85108
  ),
  long_fix = case_when(
    project_name == "Washington Park Homes" & address_or_streets == "57th-Stewart : 58th-Normal" ~ -87.63639,
    project_name == "Raymond M. Hilliard Center" ~ -87.62780,
    project_name == "Armour Sq. Apts. & Annex" ~ -87.63164,
    project_name == "Harold L. Ickes Homes" ~ -87.62866
  )) %>%
  # apply manual fixes
  mutate(lat = if_else(is.na(lat_fix), lat, lat_fix),
         long = if_else(is.na(long_fix), long, long_fix)) %>%
  # document source
  mutate(geocode_source = case_when(
    project_name == "Philip Murray Homes" ~ "name_with_city",
    project_name == "Stateway Gardens" ~ "name_with_city",
    project_name == "Clark & Irving Apts." ~ "name_with_city",
    !is.na(lat_fix) ~ "manual",
    TRUE ~ "address"
  ))
  

# output the geocded data
write_csv(chicago_geocoded_google, here(digitization_dir, "chicago", "chicago_geocoded_projects.csv"))

## San Francisco -----
sf_raw_1973 <- read_xlsx(here(digitization_dir, "san_francisco", "1973_project_list.xlsx"))
sf_raw_1966 <- read_xlsx(here(digitization_dir, "san_francisco", "1966_sfha_annual_report.xlsx"))

# Geolocating works best with a single intersection, so I will create an abbreviated addresscolumn
# that includes only the first two street names, and another column with the last two streets

sf_data_1973 <- 
  sf_raw_1973 %>% 
  mutate(
    first_two_streets = sapply(address_or_streets, function(x) {
      address_parts <- str_split(x, ",\\s*")[[1]]
      street_names <- str_trim(address_parts[1:min(length(address_parts), 2)])
      paste(street_names, collapse = " and ")
    }),
    last_two_streets = sapply(address_or_streets, function(x) {
      address_parts <- str_split(x, ",\\s*")[[1]]
      street_names <- str_trim(address_parts[max(1, length(address_parts) - 1):length(address_parts)])
      paste(street_names, collapse = " and ")
    })
  )


# geocode
sf_geocoded_1973_google <- 
  sf_data_1973 %>%
  mutate(first_two_streets_clean = paste0(first_two_streets, ", San Francisco, CA"),
         last_two_streets_clean = paste0(last_two_streets, ", San Francisco, CA"),
         # if last two streets and first two streets are the same, set last two streets as missing
         last_two_streets_clean = if_else(first_two_streets_clean == last_two_streets_clean, NA_character_, last_two_streets_clean),
         project_name_with_city = paste0(project_name, ", San Francisco, CA")
         ) %>%
  geocode(first_two_streets_clean, method = "google", lat = "lat_first_two_streets", long = "long_first_two_streets") %>% 
  geocode(last_two_streets_clean, method = "google", lat = "lat_last_two_streets", long = "long_last_two_streets") %>% 
  geocode(project_name_with_city, method = "google", lat = "lat_name_with_city", long = "long_name_with_city") %>% 
  # reverse geocode
  reverse_geocode(lat_first_two_streets, long_first_two_streets, address = "reverse_geocoded_address_first_two_streets") %>% 
  reverse_geocode(lat_last_two_streets, long_last_two_streets, address = "reverse_geocoded_address_last_two_streets") %>% 
  reverse_geocode(lat_name_with_city, long_name_with_city, address = "reverse_geocoded_address_name_with_city")

# select a single address for each project. For now, take the midpoint of the two addresses
# also calculate distance between the points

# calculate midpoints for addresses with two points
midpoints_sf <-
  sf_geocoded_1973_google %>% 
  filter(!is.na(long_last_two_streets)) %>% 
  rowwise() %>%
  mutate(start_point = list(st_point(c(long_first_two_streets, lat_first_two_streets))),
         end_point = list(st_point(c(long_last_two_streets, lat_last_two_streets))),
         midpoint = list(calculate_midpoint(
           long_first_two_streets, lat_first_two_streets,
           long_last_two_streets, lat_last_two_streets)),
         midpoint_lon = midpoint[1],
         midpoint_lat = midpoint[2]
  ) %>%
  ungroup() %>%
  # calculate distance between points
  mutate(
    start_point = st_sfc(start_point, crs = 4326),
    end_point = st_sfc(end_point, crs = 4326),
    distance_between_first_and_last_street_address_km = as.numeric(st_distance(start_point, end_point, by_element = TRUE)) / 1000
  ) %>%
  # keep rows for merging
  select(project_code, address_or_streets, midpoint_lon, midpoint_lat, distance_between_first_and_last_street_address_km)

# merge midpoints onto full dataset
sf_geocoded_1973_google_with_fixes <- 
  sf_geocoded_1973_google %>% 
  left_join(midpoints_sf) %>% 
  # calculate distance between first street and name with city points
  rowwise() %>%
  mutate(
    start_point = list(st_point(c(long_first_two_streets, lat_first_two_streets))),
    start_point = st_sfc(start_point, crs = 4326),
    end_point = list(st_point(c(long_name_with_city, lat_name_with_city))),
    end_point = st_sfc(end_point, crs = 4326),
    distance_between_street_and_name_address_km = as.numeric(st_distance(start_point, end_point, by_element = TRUE))/1000
  ) %>%
  ungroup() %>%
  select(-start_point, -end_point)

# set the final lat long that we will use
sf_geocoded_1973_google_with_fixes <-
  sf_geocoded_1973_google_with_fixes %>% 
  mutate(lat = case_when(
    !is.na(midpoint_lat) ~ midpoint_lat,
    is.na(midpoint_lat) ~ lat_first_two_streets),
    long = case_when(
      !is.na(midpoint_lon) ~ midpoint_lon,
      is.na(midpoint_lon) ~ long_first_two_streets)) %>% 
  # If the distance between the two points is greater than 1 km, and the distance between name and city is less than 1 km, use the name and city point
  mutate(lat = case_when(
    distance_between_first_and_last_street_address_km > 1 & distance_between_street_and_name_address_km < 1 ~ lat_name_with_city,
    TRUE ~ lat),
    long = case_when(
      distance_between_first_and_last_street_address_km > 1 & distance_between_street_and_name_address_km < 1 ~ long_name_with_city,
      TRUE ~ long)
  ) %>%
  # manual fixes
  mutate(lat = case_when(
    project_name == "Holly Courts" ~ lat_name_with_city,
    project_name == "Yerba Buena Annex" ~ lat_name_with_city,
    project_name == "Sunnydale" ~ lat_last_two_streets,
    project_name == "Hunters Point" ~ lat_last_two_streets,
    # Manual fix for CA-17A geocoding error (Earl St in Mission instead of Hunter's Point)
    project_code == "CA-17A" & grepl("Earl.*Kiska", address_or_streets) ~ 37.729,
    TRUE ~ lat),
    long = case_when(
      project_name == "Holly Courts" ~ long_name_with_city,
      project_name == "Yerba Buena Annex" ~ long_name_with_city,
      project_name == "Sunnydale" ~ long_last_two_streets,
      project_name == "Hunters Point" ~ long_last_two_streets,
      # Manual fix for CA-17A geocoding error (Earl St in Mission instead of Hunter's Point)
      project_code == "CA-17A" & grepl("Earl.*Kiska", address_or_streets) ~ -122.378,
      TRUE ~ long)) %>% 
  # if missing now, use the first two streets
  mutate(lat = case_when(
    is.na(lat) ~ lat_first_two_streets,
    TRUE ~ lat),
    long = case_when(
      is.na(long) ~ long_first_two_streets,
      TRUE ~ long)) %>% 
  # create column that says which address we used
  mutate(geocode_source = case_when(
    !is.na(midpoint_lat) ~ "midpoint",
    distance_between_first_and_last_street_address_km > 1 & distance_between_street_and_name_address_km < 1 ~ "name_with_city",
    project_name == "Holly Courts" ~ "name_with_city",
    project_name == "Yerba Buena Annex" ~ "name_with_city",
    project_name == "Sunnydale" ~ "last_two_streets",
    project_name == "Hunters Point" ~ "last_two_streets",
    project_code == "CA-17A" & grepl("Earl.*Kiska", address_or_streets) ~ "manual_fix",
    is.na(lat) ~ "first_two_streets",
    TRUE ~ "first_two_streets"
  ))


# output 
write_csv(sf_geocoded_1973_google_with_fixes, here(digitization_dir, "san_francisco", "sf_geocoded_projects.csv"))



## Los Angeles -----

la_raw <- read_xlsx(here(digitization_dir, "los_angeles", "la_housing_authority_annual_report.xlsx"))

# geocode
la_geocoded_google <- 
  la_raw %>%
  mutate(address_clean = paste0(address_or_streets, ", Los Angeles, CA")) %>%
  geocode(address_clean, method = "google") %>% 
  reverse_geocode(lat, long, address = "reverse_geocoded_address") %>% 
  mutate(geocode_source = "address")

# output
write_csv(la_geocoded_google, here(digitization_dir, "los_angeles", "la_geocoded_projects.csv"))

## Washington DC ------

dc_raw <- read_xlsx(here(digitization_dir, "washington_dc", "1972_ncha_annual_report.xlsx"))

# clean addresses to include one intersection
dc_data <- 
  dc_raw %>%
  # mutate(address_clean = str_replace_all(address_or_streets, "[Bb]etween", ",")) %>% 
  # eastgate gardens
  mutate(address_clean = ifelse(address_or_streets == "50th St., 51st St., E, F, G Sts., Drake Place, 50th Place SE",
                                "F, G, and 51st Streets SE; Benning Road SE; and Drake Place SE", address_or_streets),
         # east capitol dwellings
         address_clean = ifelse(address_clean == "E. Capitol at DC Line NE and SE", "East Capitol Street and 59th street SE", address_clean),
         # montana terrace (this is my best guess based on the address)
         address_clean = ifelse(address_clean == "Montana Ave., 15th and Bryant Sts. NE", "1625 Montana Ave NE", address_clean)) %>% 
  mutate(address_clean = paste0(address_clean, ", Washington, DC")) %>% 
  # create name with city
  mutate(name_with_city = paste0(project_name, ", Washington, DC"))


# geocode
dc_geocoded_google <- 
  dc_data %>%
  geocode(address_clean, method = "google") %>% 
  # geocode(name_with_city, method = "google") %>%
  reverse_geocode(lat, long, address = "reverse_geocoded_address") %>% 
  mutate(geocode_source = "address")

# output 
write_csv(dc_geocoded_google, here(digitization_dir, "washington_dc", "dc_geocoded_projects.csv"))

## Atlanta -----

atlanta_raw <- read_xlsx(here(digitization_dir, "atlanta", "atlanta_annual_report_1970.xlsx"))

# geocode
atlanta_geocoded_google <- 
  atlanta_raw %>%
  filter(address_or_streets != "N/A", !str_detect(address_or_streets, "Scattered")) %>%
  mutate(address_clean = paste0(address_or_streets, ", Atlanta, GA")) %>%
  geocode(address_clean, method = "google") %>% 
  reverse_geocode(lat, long, address = "reverse_geocoded_address") %>% 
  mutate(geocode_source = "address")

# output
write_csv(atlanta_geocoded_google, here(digitization_dir, "atlanta", "atlanta_geocoded_projects.csv"))

## Baltimore -----
baltimore_raw <- read_xlsx(here(digitization_dir, "baltimore", "baltimore_projects.xlsx"))

# geocode
baltimore_geocoded_google <- 
  baltimore_raw %>%
  mutate(address_clean = paste0(address_or_streets, ", Baltimore, MD")) %>%
  geocode(address_clean, method = "google") %>% 
  reverse_geocode(lat, long, address = "reverse_geocoded_address") %>% 
  # geocode source column
  mutate(geocode_source = "address")

# output
write_csv(baltimore_geocoded_google, here(digitization_dir, "baltimore", "baltimore_geocoded_projects.csv"))

# Download Baltimore City boundaries
# baltimore_boundary <-
#   counties(state = "MD") %>% 
#   filter(NAMELSAD == "Baltimore city") %>%
#   st_transform(crs = 4326)  # Transform to WGS84 to match your points
# 
# 
# # map the proejcts
# baltimore_geocoded_google %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
#   mutate(row_num = row_number()) %>%
#   ggplot() +
#   geom_sf(data = baltimore_boundary, fill = NA, color = "black") +  # City outline
#   geom_sf(color = "red", size = 2) +
#   labs(title = "Baltimore Housing Authority Projects, 1973",
#        x = "Longitude",
#        y = "Latitude") +
#   theme_minimal() +
#   geom_text(aes(label = row_num, geometry = geometry),
#                   stat = "sf_coordinates",
#                   size = 3)  # Labels

## NYC -----
# Note: For NYC, I do two things:
# 0. Geolocate the projects
# 1. Merge in the initial population data from Max-GM

nyc_raw <- read_xlsx(here(digitization_dir, "nycha", "pdbdec1973_digitization.xlsx"))

# nyc_population_data_raw <-
#   read_xlsx(here(digitization_dir, "nycha", "initial_composition_ph.xlsx"), sheet = "Final data")

# geocode projects (up through 1973)
nyc_data <-
  nyc_raw %>%
  filter(!is.na(address_or_streets)) %>%
  mutate(
    first_two_streets = sapply(address_or_streets, function(x) {
      address_parts <- str_split(x, ";\\s*")[[1]]
      street_names <- str_trim(address_parts[1:min(length(address_parts), 2)])
      paste(street_names, collapse = " and ")
    }),
    last_two_streets = sapply(address_or_streets, function(x) {
      address_parts <- str_split(x, ";\\s*")[[1]]
      street_names <- str_trim(address_parts[max(1, length(address_parts) - 1):length(address_parts)])
      paste(street_names, collapse = " and ")
    })
    )%>% 
  # Geolocating works best with a single intersection, so I will create an abbreviated addresscolumn
  # that includes only the first two street names, and another column with the last two streets
  mutate(first_two_streets_clean = paste0(first_two_streets, ", ", other_geo_info, ", New York, NY"),
         last_two_streets_clean = paste0(last_two_streets, ", ", other_geo_info, ", New York, NY"),
         last_two_streets_clean = if_else(first_two_streets_clean == last_two_streets_clean, NA_character_, last_two_streets_clean)) %>%
  # name + city
  mutate(name_with_city = paste0(project_name, ", ", other_geo_info, ", New York, NY"))

nyc_geocoded_google <-
  nyc_data %>%
  geocode(address_or_streets, method = "google", lat = "lat_full_address", long = "long_full_address") %>%
  geocode(first_two_streets_clean, method = "google", lat = "lat_first_two_streets", long = "long_first_two_streets") %>% 
  geocode(last_two_streets_clean, method = "google", lat = "lat_last_two_streets", long = "long_last_two_streets") %>%
  geocode(name_with_city, method = "google", lat = "lat_name_with_city", long = "long_name_with_city")%>%
  # reverse geocoding
  reverse_geocode(lat_first_two_streets, long_first_two_streets, address = "reverse_geocoded_address_first_two_streets") %>% 
  reverse_geocode(lat_last_two_streets, long_last_two_streets, address = "reverse_geocoded_address_last_two_streets") %>% 
  reverse_geocode(lat_full_address, long_full_address, address = "reverse_geocoded_address_full_address") %>%
  reverse_geocode(lat_name_with_city, long_name_with_city, address = "reverse_geocoded_address_name_with_city")


# select a single address for each project. For now, take the midpoint of the two addresses
# also calculate distance between the points
nyc_geocoded_google_with_fixes <-
  nyc_geocoded_google %>%
  rowwise() %>%
  mutate(
    lat_midpoint = (lat_first_two_streets + lat_last_two_streets) / 2,
    long_midpoint = (long_first_two_streets + long_last_two_streets) / 2,
    start_point_address = list(st_point(c(long_full_address, lat_full_address))),
    start_point = list(st_point(c(long_first_two_streets, lat_first_two_streets))),
    end_point = list(st_point(c(long_last_two_streets, lat_last_two_streets))),
    end_point_name = list(st_point(c(long_name_with_city, lat_name_with_city)))) %>% 
  ungroup() %>% 
  mutate(start_point = st_sfc(start_point, crs = 4326),
         start_point_address = st_sfc(start_point_address, crs = 4326),
         end_point = st_sfc(end_point, crs = 4326),
         end_point_name = st_sfc(end_point_name, crs = 4326),
         distance_between_first_and_last_street_address_km = as.numeric(st_distance(start_point, end_point, by_element = TRUE)) / 1000,
         distance_between_address_and_name_km = as.numeric(st_distance(start_point_address, end_point_name, by_element = TRUE)) / 1000,
         distance_between_first_street_and_address_km = as.numeric(st_distance(start_point, start_point_address, by_element = TRUE)) / 1000) %>% 
  # Set address to midpoint
  # If distance between first_and_last_street_address is greater than 2, and distance between name and address is less than 2, use the name lat and long
  # using case_when
  mutate(
    lat = case_when(
      distance_between_first_and_last_street_address_km > 2 & distance_between_address_and_name_km < 2 ~ lat_name_with_city,
      TRUE ~ lat_midpoint
    ),
    long = case_when(
      distance_between_first_and_last_street_address_km > 2 & distance_between_address_and_name_km < 2 ~ long_name_with_city,
      TRUE ~ long_midpoint
    )
  ) %>%
  # If distance between first_and_last_street_address is greater than 2, and distance between name and address is greater than 2, set to missing
  mutate(
    lat = case_when(
      distance_between_first_and_last_street_address_km > 2 & distance_between_address_and_name_km > 2 ~ NA_real_,
      TRUE ~ lat
    ),
    long = case_when(
      distance_between_first_and_last_street_address_km > 2 & distance_between_address_and_name_km > 2 ~ NA_real_,
      TRUE ~ long
    )
  ) %>%
  # If distance between first and last is > 2 and distance between name and address is missing, set to missing
  mutate(
    lat = case_when(
      distance_between_first_and_last_street_address_km > 2 & is.na(distance_between_address_and_name_km) ~ NA_real_,
      TRUE ~ lat
    ),
    long = case_when(
      distance_between_first_and_last_street_address_km > 2 & is.na(distance_between_address_and_name_km) ~ NA_real_,
      TRUE ~ long
    )
  ) %>%
  # create geocode_source column based on rules above
  mutate(
    geocode_source = case_when(
      distance_between_first_and_last_street_address_km > 2 & distance_between_address_and_name_km < 2 ~ "name",
      distance_between_first_and_last_street_address_km > 2 & distance_between_address_and_name_km > 2 ~ "missing",
      distance_between_first_and_last_street_address_km > 2 & is.na(distance_between_address_and_name_km) ~ "missing",
      TRUE ~ "midpoint"
    )
  )

# output 
write_csv(nyc_geocoded_google_with_fixes, here(digitization_dir, "nycha", "nyc_geocoded_projects.csv"))

## Boston ----
boston_raw <- read_xlsx(here(digitization_dir, "boston", "boston_housing_projects_final.xlsx"))

# geocode
boston_geocoded_google <- 
  boston_raw %>%
  filter(!is.na(address_or_streets)) %>%
  geocode(address_or_streets, method = "google") %>% 
  reverse_geocode(lat, long, address = "reverse_geocoded_address") %>% 
  # geocode source column
  mutate(geocode_source = "address")

# output
write_csv(boston_geocoded_google, here(digitization_dir, "boston", "boston_geocoded_projects.csv"))


# Combine digitized projects into a single dataset ------

# If running separate, can uncomment and read in the geocoded files
# This allows you to edit this dataset without necessarily re-geocoding every city
# E.g. If you want to re-run new york, you can run the new york geocoding above, uncomment
# every other city below, and then combine them all together

chicago_geocoded_google <- read_csv(here(digitization_dir, "chicago", "chicago_geocoded_projects.csv"))
sf_geocoded_1973_google_with_fixes <- read_csv(here(digitization_dir, "san_francisco", "sf_geocoded_projects.csv"))
la_geocoded_google <- read_csv(here(digitization_dir, "los_angeles", "la_geocoded_projects.csv"))
dc_geocoded_google <- read_csv(here(digitization_dir, "washington_dc", "dc_geocoded_projects.csv"))
atlanta_geocoded_google <- read_csv(here(digitization_dir, "atlanta", "atlanta_geocoded_projects.csv"))
baltimore_geocoded_google <- read_csv(here(digitization_dir, "baltimore", "baltimore_geocoded_projects.csv"))
nyc_geocoded_google_with_fixes <- read_csv(here(digitization_dir, "nycha", "nyc_geocoded_projects.csv"))
boston_geocoded_google <- read_csv(here(digitization_dir, "boston", "boston_geocoded_projects.csv"))

# Combine all geocoded projects into a single dataset

all_geocoded_projects <-
  bind_rows(
    chicago_geocoded_google %>% mutate(city = "Chicago"),
    sf_geocoded_1973_google_with_fixes %>% mutate(city = "San Francisco"),
    la_geocoded_google %>% mutate(city = "Los Angeles"),
    dc_geocoded_google %>% mutate(city = "Washington DC"),
    atlanta_geocoded_google %>% mutate(city = "Atlanta"),
    baltimore_geocoded_google %>% mutate(city = "Baltimore"),
    nyc_geocoded_google_with_fixes %>% mutate(city = "New York City"),
    boston_geocoded_google %>% mutate(city = "Boston")
  )


# Save ----

# Save all geocoded projects to csv
write_csv(all_geocoded_projects, paste0(out_dir, "geocoded_projects.csv"))
