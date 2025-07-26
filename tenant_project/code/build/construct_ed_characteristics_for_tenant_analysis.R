# I already did a lot of this actually in public_housing/code/build/full_count/collapse_full_count_to_ed.R
# I think I just need to add the geographic information onto those output files

# Preliminaries -----
library(tidyverse)
library(here)
library(sf)

collapsed_ed_dir <- here("data", "derived", "census", "full_count", "ed_by_city")
ed_geometry_dir <- here("data", "derived", "geographic_crosswalks", "ed_to_tract")
output_dir <- here("tenant_project", "data", "derived", "neighborhood_data")

# Full count data collapsed to city-ed level by collapse_full_count_to_ed.R
ed_by_city_1930 <- read_csv(here(collapsed_ed_dir, "city_ed_data_1930.csv"))
ed_by_city_1940 <- read_csv(here(collapsed_ed_dir, "city_ed_data_1940.csv"))

# Construct ED geometries -----
# function for caculating centroid coordinates
calculate_ed_lat_long <- function(df) {
  df <- 
    
    centroids <- st_centroid(df)
  coords <- st_coordinates(centroids)
  
  df %>%
    mutate(
      lat = coords[, 2],
      long = coords[, 1]
    ) 
}


# Reconstructing from raw Urban Transitions Project data
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

# Set up paths
read_raw_ed_geometries <- function(city, year) {
  
  
  message("Reading raw ED geometry for city: ", city, ", year: 19", year)
  
  data_dir <- here("data", "raw", "urban_transitions", "enumeration_districts")
  standard_crs <- 4326  
  
  # Build ZIP paths
  decade_zip <- file.path(data_dir, "ForEachDecade", paste0("ForEachCity19", year, ".zip"))
  temp_decade_dir <- tempdir()
  unzip(decade_zip, exdir = temp_decade_dir)
  
  city_zip <- file.path(temp_decade_dir, paste0("ForEachCity19", year), paste0(city, year, ".zip"))
  temp_city_dir <- tempdir()
  unzip(city_zip, exdir = temp_city_dir)
  
  # Path to shapefile
  shp_dir <- file.path(temp_city_dir, paste0(city, year))
  shp_file <- file.path(shp_dir, paste0(city, "_ed", year, "_aggr.shp"))
  
  # Read shapefile (no filtering)
  shp <- st_read(shp_file, quiet = TRUE) %>%
    filter(!is.na(ed)) %>% 
    select(ed, geometry) %>%
    mutate(city = city, year = paste0("19", year))
  
  # Fix CRS
  if (is.na(st_crs(shp))) {
    message("No CRS found for ", city, " ", year, " — assigning EPSG:4326")
    st_crs(shp) <- standard_crs
  } else if (st_crs(shp) != st_crs(standard_crs)) {
    shp <- st_transform(shp, crs = standard_crs)
  }
  
  
  return(shp)
}

# Create all combinations
city_year_combos <- expand.grid(city = cities, year = years, stringsAsFactors = FALSE)

# Map over the combos
all_eds <- purrr::pmap_dfr(city_year_combos, function(city, year) {
  read_raw_ed_geometries(city, year)
})

all_eds <-
  all_eds %>% 
  st_make_valid() %>% 
  mutate(state = substr(city, nchar(city) - 1, nchar(city)),
         city = substr(city, 1, nchar(city) - 2)) %>% 
  calculate_ed_lat_long() %>% 
  st_drop_geometry() %>% 
  # convert EDs to upper case
  mutate(ed = toupper(ed))

# keep only one of each city-state-ed-year 
all_eds <- all_eds %>%
  distinct(city, state, ed, year, .keep_all = TRUE) %>%
  arrange(city, state, ed)

# change city names for merge with collapsed data
# StLouis -> SaintLouis
# StPauk _> SaintPaul
# Bronx, Brooklyn, Manhattan, Queens, StatenIsland -> NewYorkCity
# 
# TODO: Waiting for the GRF to reload with the cities 
# x <- 
#   all_eds %>% 
#   mutate(city = case_when(
#     city == "StLouis" ~ "SaintLouis",
#     city == "StPaul" ~ "SaintPaul",
#     city %in% c("Bronx", "Brooklyn", "Manhattan", "Queens", "StatenIsland") ~ "NewYorkCity",
#     TRUE ~ city
#   )) %>%


# Clean ED characteristic data -----
# Construct median rent group and median house values (which)
calculate_median_ed_city <- function(data, var_code, var_name) {
  data %>%
    mutate_at(vars(contains(var_code)), as.numeric) %>%
    pivot_longer(cols = starts_with(var_code), names_to = "range", values_to = "num_in_sample") %>%
    mutate(
      range = gsub(paste0(var_code, "_"), "", range),
      range = as.numeric(range)
    ) %>%
    group_by(CITY_NAME, b_ed) %>%
    arrange(range) %>%
    mutate(
      cumulative_freq = cumsum(num_in_sample),
      total = sum(num_in_sample),
      median_position = total / 2
    ) %>%
    filter(cumulative_freq >= median_position) %>%
    slice(1) %>%
    mutate(median = range) %>%
    select(CITY_NAME, b_ed, median) %>%
    rename(!!var_name := median)
}

# 1930: 
# Median rent category
median_rent_ed_1930 <- calculate_median_ed_city(ed_by_city_1930, var_code = "rent_group", var_name = "median_rent_group")

# convert to single number
rent_group_midpoints_1930 <- tibble(
  median_rent_group = 1:15,
  median_rent_dollars = c(
    2.5,     # $0–5
    5.5,     # $5–6
    7.5,     # $6–9
    11.5,    # $9–14
    16.5,    # $14–19
    21.5,    # $19–24
    26.5,    # $24–29
    34,      # $29–39
    44,      # $39–49
    54,      # $49–59
    66.5,    # $59–74
    86.5,    # $74–99
    124,     # $99–149
    174,     # $149–199
    200      # $200+ (conservative lower bound, could raise this)
  )
)

median_rent_ed_1930 <-
  median_rent_ed_1930 %>% 
  left_join(rent_group_midpoints_1930, by = "median_rent_group")


# Median home value category
median_home_value_ed_1930 <- calculate_median_ed_city(ed_by_city_1930, var_code = "home_value", var_name = "median_home_value_group")

home_value_group_midpoints_1930 <- tibble(
  median_home_value_group = 1:15,
  median_home_value_dollars = c(
    250,     # $0–500
    600,     # $501–699
    850,     # $700–999
    1250,    # $1000–1499
    1750,    # $1500–1999
    2250,    # $2000–2499
    2750,    # $2500–2999
    3500,    # $3000–3999
    4500,    # $4000–4999
    5500,    # $5000–5999
    6750,    # $6000–7499
    8750,    # $7500–9999
    12500,   # $10000–14999
    17500,   # $15000–19999
    20000    # $20000+ (could bump higher if desired, but conservative)
  )
)

median_home_value_ed_1930 <-
  median_home_value_ed_1930 %>% 
  left_join(home_value_group_midpoints_1930, by = "median_home_value_group")


# merge onto original
ed_by_city_1930 <- ed_by_city_1930 %>%
  select(-contains('rent_group'), -contains("home_value")) %>% 
  left_join(median_rent_ed_1930, by = c("CITY_NAME", "b_ed")) %>%
  left_join(median_home_value_ed_1930, by = c("CITY_NAME", "b_ed")) 



# 1940
# Median rent category
median_rent_ed_1940 <- calculate_median_ed_city(ed_by_city_1940, var_code = "rent_group", var_name = "median_rent_group")

median_rent_ed_1940 <-
  median_rent_ed_1940 %>% 
  left_join(rent_group_midpoints_1930, by = "median_rent_group")

# Median home value category
median_home_value_ed_1940 <- calculate_median_ed_city(ed_by_city_1940, var_code = "valueh_group", var_name = "median_home_value_group")

median_home_value_ed_1940 <-
  median_home_value_ed_1940 %>% 
  left_join(home_value_group_midpoints_1930, by = "median_home_value_group")

ed_by_city_1940 <- ed_by_city_1940 %>%
  select(-contains('rent_group'), -contains("home_value")) %>% 
  left_join(median_rent_ed_1940, by = c("CITY_NAME", "b_ed")) %>%
  left_join(median_home_value_ed_1940, by = c("CITY_NAME", "b_ed"))



# split city name up into city and state

ed_by_city_1930 <- ed_by_city_1930 %>% 
  mutate(state = substr(b_city, nchar(b_city) - 1, nchar(b_city)),
         city = substr(b_city, 1, nchar(b_city) - 2)) %>% 
  dplyr::rename(ed = b_ed) 


ed_by_city_1940 <- ed_by_city_1940 %>%
  mutate(state = substr(b_city, nchar(b_city) - 1, nchar(b_city)),
         city = substr(b_city, 1, nchar(b_city) - 2)) %>% 
  dplyr::rename(ed = b_ed)


# Note: This merge isn't working well...

ed_by_city_1930_geo <-
  ed_by_city_1930 %>% 
  mutate(ed = as.character(ed)) %>% 
  left_join(all_eds %>% filter(year == 1930))

ed_by_city_1940_geo <- 
  ed_by_city_1940 %>%
  left_join(all_eds %>%  filter(year == 1940))

# check which all_eds didnt merge
anti_join(all_eds %>% filter(year == 1930), ed_by_city_1930_geo, by = c("ed", "city", "state")) %>%
  arrange(ed, city, state)

# Not every ED gets a geometry unfortunately.... however, I think most this is just issues with the 
# Urban Transitions Project data, so there's not that much I can do 
# I am not transforming the EDs, so there shouldn't really be any issues merging to them
# Doesn't seem systematic


# save the results
write_csv(ed_by_city_1930_geo, here(output_dir, "ed_characteristics_1930.csv"))
write_csv(ed_by_city_1940_geo, here(output_dir, "ed_characteristics_1940.csv"))



## Testing ED geometries -----
