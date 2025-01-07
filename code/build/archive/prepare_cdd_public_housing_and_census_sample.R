####
# Combine data and prep sample:
# 1. Read in census tract pop, housing, education, income data, and combine 
# 2. Define sample of cities
# 3. Geocode public housing data
# 3. Merge census tract data with public housing data 
# 4. Define treated status of Census tracts


# Output:
# 1. Census tract data sample
# 2. Public housing data sample
# 3. Treated tract sample 
####

# Preliminaries -----
library(tidyverse)
library(sf)
library(tigris)
library(crsuggest)
library(mapview)
library(maps)
library(ragg)
library(viridis)
library(geosphere)


rm(list =ls())

# !! choose which sample: "cdd_large" or "cdd_small" 
sample_size <- "cdd_small"


# define file paths
tract_with_treatment_status_filepath <- paste0("data/derived/merged/census_tract_sample_with_treatment_status_", sample_size, ".gpkg")
cdd_project_sample_filepath <- paste0("data/derived/public_housing/working/housing_project_sample_", sample_size, ".gpkg")
treated_tracts_panel_filepath <- paste0("data/derived/merged/treated_tracts_panel_", sample_size, ".gpkg")

# Prep Census data ----

## Read in census tract data ----

tract_id_variables <-  c("YEAR", "STATEA", "COUNTYA", "TRACTA")

### Population by race, total pop, population density, CBD designation
census_pop_data <- 
  read_sf("data/derived/census/tract_population_data.gpkg") 

## Housing: 
census_housing_data <-
  read_sf("data/derived/census/tract_housing_data.gpkg") %>% 
  # drop geometry 
  st_drop_geometry() %>% 
  # keep only merging variables and median variables
  select(any_of(tract_id_variables), contains("median"), total_units, total_units, vacancy_rate,
         share_needing_repair, share_no_water, housing_density)
  
## Median income
census_income_data <-
  read_sf("data/derived/census/tract_income_data.gpkg") %>% 
  # drop geometry
  st_drop_geometry() %>%
  # keep only merging variables and median variables
  select(any_of(tract_id_variables), contains("median"))

## Median education
# I have illiteracy in 1920 and 1930, but for now, don't use it
# Still need to clean illiteracy data
census_education_data <-
  read_sf("data/derived/census/tract_education_data.gpkg") %>% 
  filter(YEAR >= 1940) %>% 
  # drop geometry
  st_drop_geometry() %>%
  # keep only merging variables and median variables
  select(any_of(tract_id_variables), contains("median"))

## employment 
census_employment_data <-
  read_sf("data/derived/census/tract_employment_data.gpkg") %>% 
  # drop geometry
  st_drop_geometry() %>%
  # keep only merging variables and median variables
  select(any_of(tract_id_variables), contains("pop"), contains("rate"))

## Combine census tract data ----
census_tract_data_full <-
  census_pop_data %>% 
  left_join(census_housing_data, by = c("YEAR", "STATEA", "COUNTYA", "TRACTA")) %>% 
  left_join(census_income_data, by = c("YEAR", "STATEA", "COUNTYA", "TRACTA")) %>%
  left_join(census_education_data, by = c("YEAR", "STATEA", "COUNTYA", "TRACTA")) %>%
  left_join(census_employment_data, by = c("YEAR", "STATEA", "COUNTYA", "TRACTA")) %>% 
  # create variables 
  mutate(employment_pop_ratio = employed_pop/total_pop)


## Get only cities in our sample ----
# for cdd_large:
# [1] "NEW YORK CITY"                      "CHICAGO"                            "PHILADELPHIA"                      
# [4] "BALTIMORE CITY"                     "NEW ORLEANS"                        "ATLANTA"                           
# [7] "NEWARK"                             "LOS ANGELES"                        "WASHINGTON DC-NATIONAL CAPITOL"    
# [10] "CLEVELAND-CUYAHOGA METROPOLITAN"    "BOSTON"                             "PITTSBURGH"                        
# [13] "DETROIT"                            "DALLAS"                             "SAN FRANCISCO"                     
# [16] "CINCINNATI-CINCINNATI METROPOLITAN" "SAN ANTONIO"                        "MINNEAPOLIS"                       
# [19] "SEATTLE"                            "BIRMINGHAM"                         "MEMPHIS"                           
# [22] "MIAMI-DADE COUNTY HSG & URB DEV"    "ST LOUIS CITY"                      "NASHVILLE-DAVIDSON"                
# [25] "COLUMBUS"                           "LOUISVILLE"                         "EL PASO"                           
# [28] "BUFFALO"                            "TAMPA"                              "ST PAUL"                           
# [31] "MILWAUKEE"                          "MOBILE-MOBILE"                      "NORFOLK"                           
# [34] "DENVER-DENVER CITY AND COUNTY"      "KNOXVILLE"                          "JERSEY CITY"                       
# [37] "RICHMOND"                           "OMAHA"                              "NEW HAVEN TOWN"                    
# [40] "EAST ST LOUIS"                      "PROVIDENCE"                         "HOUSTON"                           
# [43] "CHARLOTTE"                          "HONOLULU-HAWAII"                    "CHATTANOOGA"                       
# [46] "INDIANAPOLIS"                       "FALL RIVER"                         "OAKLAND"                           
# [49] "AKRON-AKRON METROPOLITAN"           "TOLEDO-TOLEDO METROPOLITAN"        

if(sample_size == "cdd_large") {
  census_tract_sample <-
    census_tract_data_full %>% 
    filter((COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York") |
             # 2. chicago
             (COUNTY %in% c("Cook") & STATE == "Illinois") |
             # 5. philadelphia
             (COUNTY %in% ("Philadelphia") & STATE == "Pennsylvania") |
             # 3. baltimore
             (COUNTY %in% c("Baltimore City") & STATE == "Maryland") |
             # 4. new orleans
             (COUNTY %in% c("Orleans") & STATE == "Louisiana") |
             # 5. atlanta
             (COUNTY %in% c("Fulton") & STATE == "Georgia") |
             # 6. newark
             (COUNTY %in% c("Essex") & STATE == "New Jersey") |
             # 7. los angeles
             (COUNTY %in% c("Los Angeles") & STATE == "California") |
             # 8. washington dc
             (STATE == "District Of Columbia") |
             # 9. cleveland
             (COUNTY %in% c("Cuyahoga") & STATE == "Ohio") |
             # 10. boston
             (COUNTY %in% c("Suffolk") & STATE == "Massachusetts") |
             # 11. pittsburgh
             (COUNTY %in% c("Allegheny") & STATE == "Pennsylvania") |
             # 12. detroit
             (COUNTY %in% c("Wayne") & STATE == "Michigan") |
             # 13. dallas
             (COUNTY %in% c("Dallas", "Collin", "Denton", "Rockwall", "Kaufman") & STATE == "Texas") |
             # 14. san francisco
             (COUNTY %in% c("San Francisco") & STATE == "California") |
             # 15. cincinnati
             (COUNTY %in% c("Hamilton") & STATE == "Ohio") |
             # 16. san antonio
             (COUNTY %in% c("Bexar") & STATE == "Texas") |
             # 17. minneapolis
             (COUNTY %in% c("Hennepin") & STATE == "Minnesota") |
             # 18. seattle
             (COUNTY %in% c("King") & STATE == "Washington") |
             # 19. memphis
             (COUNTY %in% c("Shelby") & STATE == "Tennessee") |
             # 20. birmingham
             (COUNTY %in% c("Jefferson") & STATE == "Alabama") |
             # 21. miami
             (COUNTY %in% c("Dade") & STATE == "Florida") |
             # 22. st louis
             (COUNTY %in% c("St Louis City") & STATE == "Missouri") |
             # 23. nashville
             (COUNTY %in% c("Davidson") & STATE == "Tennessee") |
             # Columbus
             (COUNTY %in% c("Franklin") & STATE == "Ohio") |
             # 24. louisville
             (COUNTY %in% c("Jefferson") & STATE == "Kentucky") |
             # 26. el paso
             (COUNTY %in% c("El Paso") & STATE == "Texas") |
             # 27. buffalo
             (COUNTY %in% c("Erie") & STATE == "New York") |
             # 28. tampa
             (COUNTY %in% c("Hillsborough") & STATE == "Florida") |
             # 30. st paul
             (COUNTY %in% c("Ramsey") & STATE == "Minnesota") |
             # 31. milwaukee
             (COUNTY %in% c("Milwaukee") & STATE == "Wisconsin") |
             # 32. mobile
             (COUNTY %in% c("Mobile") & STATE == "Alabama") |
             # 33. norfolk
             (COUNTY %in% c("Norfolk City") & STATE == "Virginia") |
             # 34. denver
             (COUNTY %in% c("Denver") & STATE == "Colorado") |
             # 35. knoxville
             (COUNTY %in% c("Knox") & STATE == "Tennessee") |
             # 38. jersey city
             (COUNTY %in% c("Hudson") & STATE == "New Jersey") |
             # 25. richmond
             (COUNTY %in% c("Richmond City") & STATE == "Virginia") |
             # 39. omaha
             (COUNTY %in% c("Douglas") & STATE == "Nebraska") |
             # 40. new haven
             (COUNTY %in% c("New Haven") & STATE == "Connecticut") |
             # 43. EAST ST LOUIS
             (COUNTY %in% c("St Clair") & STATE == "Illinois") |
             # 42. Providence
             (COUNTY %in% c("Providence") & STATE == "Rhode Island") |
             # 41. houston
             (COUNTY %in% c("Harris") & STATE == "Texas") |
             # 44. CHARLOTTE
             (COUNTY %in% c("Mecklenburg") & STATE == "North Carolina") |
             # 45. Honolulu
             (COUNTY %in% c("Honolulu") & STATE == "Hawaii") |
             # 46. Chattanooga
             (COUNTY %in% c("Hamilton") & STATE == "Tennessee") |
             # 47. Indianapolis
             (COUNTY %in% c("Marion") & STATE == "Indiana") |
             # 48. Fall River
             (COUNTY %in% c("Bristol") & STATE == "Massachusetts") |
             # 49. Oakland
             (COUNTY %in% c("Alameda") & STATE == "California") |
             # 50. Akron
             (COUNTY %in% c("Summit") & STATE == "Ohio") | 
             #  Toledo
             (COUNTY %in% c("Lucas") & STATE == "Ohio")
             ) %>% 
    # create a variable called "city" to identify the city
       mutate(city = case_when(                 
                COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York" ~ "New York City",
                 COUNTY %in% c("Cook") & STATE == "Illinois" ~ "Chicago",
                 COUNTY %in% c("Philadelphia") & STATE == "Pennsylvania" ~ "Philadelphia",
                 COUNTY %in% c("Wayne") & STATE == "Michigan" ~ "Detroit",
                 COUNTY %in% c("Cuyahoga") & STATE == "Ohio" ~ "Cleveland",
                 COUNTY %in% c("Baltimore City") & STATE == "Maryland" ~ "Baltimore",
                 COUNTY %in% c("Allegheny") & STATE == "Pennsylvania" ~ "Pittsburgh",
                 COUNTY %in% c("Hamilton") & STATE == "Ohio" ~ "Cincinnati",
                 STATE == "District Of Columbia" ~ "Washington DC",
                 COUNTY %in% c("Suffolk") & STATE == "Massachusetts" ~ "Boston",
                 COUNTY %in% c("St Louis City") & STATE == "Missouri" ~ "St. Louis",
                 COUNTY %in% c("San Francisco") & STATE == "California" ~ "San Francisco",
                 COUNTY %in% c("Los Angeles") & STATE == "California" ~ "Los Angeles",
                 COUNTY %in% c("Dade") & STATE == "Florida" ~ "Miami",
                 COUNTY %in% c("Fulton") & STATE == "Georgia" ~ "Atlanta",
                 COUNTY %in% c("King") & STATE == "Washington" ~ "Seattle",
                 COUNTY %in% c("Orleans") & STATE == "Louisiana" ~ "New Orleans",
                 COUNTY %in% c("Essex") & STATE == "New Jersey" ~ "Newark",
                 COUNTY %in% c("Dallas", "Collin", "Denton", "Rockwall", "Kaufman") & STATE == "Texas" ~ "Dallas",
                 COUNTY %in% c("Bexar") & STATE == "Texas" ~ "San Antonio",
                 COUNTY %in% c("Hennepin") & STATE == "Minnesota" ~ "Minneapolis",
                 COUNTY %in% c("Shelby") & STATE == "Tennessee" ~ "Memphis",
                 COUNTY %in% c("Jefferson") & STATE == "Alabama" ~ "Birmingham",
                 COUNTY %in% c("Davidson") & STATE == "Tennessee" ~ "Nashville",
                 COUNTY %in% c("Franklin") & STATE == "Ohio" ~ "Columbus",
                 COUNTY %in% c("Jefferson") & STATE == "Kentucky" ~ "Louisville",
                 COUNTY %in% c("El Paso") & STATE == "Texas" ~ "El Paso",
                 COUNTY %in% c("Erie") & STATE == "New York" ~ "Buffalo",
                 COUNTY %in% c("Hillsborough") & STATE == "Florida" ~ "Tampa",
                 COUNTY %in% c("Ramsey") & STATE == "Minnesota" ~ "St. Paul",
                 COUNTY %in% c("Milwaukee") & STATE == "Wisconsin" ~ "Milwaukee",
                 COUNTY %in% c("Mobile") & STATE == "Alabama" ~ "Mobile",
                 COUNTY %in% c("Norfolk City") & STATE == "Virginia" ~ "Norfolk",
                 COUNTY %in% c("Denver") & STATE == "Colorado" ~ "Denver",
                 COUNTY %in% c("Knox") & STATE == "Tennessee" ~ "Knoxville",
                 COUNTY %in% c("Hudson") & STATE == "New Jersey" ~ "Jersey City",
                 COUNTY %in% c("Richmond City") & STATE == "Virginia" ~ "Richmond",
                 COUNTY %in% c("Douglas") & STATE == "Nebraska" ~ "Omaha",
                 COUNTY %in% c("New Haven") & STATE == "Connecticut" ~ "New Haven",
                 COUNTY %in% c("St Clair") & STATE == "Illinois" ~ "East St. Louis",
                 COUNTY %in% c("Providence") & STATE == "Rhode Island" ~ "Providence",
                 COUNTY %in% c("Harris") & STATE == "Texas" ~ "Houston",
                 COUNTY %in% c("Mecklenburg") & STATE == "North Carolina" ~ "Charlotte",
                 COUNTY %in% c("Honolulu") & STATE == "Hawaii" ~ "Honolulu",
                 COUNTY %in% c("Hamilton") & STATE == "Tennessee" ~ "Chattanooga",
                 COUNTY %in% c("Marion") & STATE == "Indiana" ~ "Indianapolis",
                 COUNTY %in% c("Bristol") & STATE == "Massachusetts" ~ "Fall River",
                 COUNTY %in% c("Alameda") & STATE == "California" ~ "Oakland",
                 COUNTY %in% c("Summit") & STATE == "Ohio" ~ "Akron",
                 COUNTY %in% c("Lucas") & STATE == "Ohio" ~ "Toledo",
                 TRUE ~ NA  # This catches any counties not explicitly listed
               ))
#counties_in_data <- unique(census_tract_sample$COUNTY) %>% as.data.frame()
  
} else if (sample_size == "cdd_small") {
  
  # list of cities: 
  # 1.NYC,
  # 2. Chicago
  # 3. Detroit
  # 4. Cleveland
  # 5. Philadelphia
  # 6. Baltimore
  # 7. Pittsburgh
  # 8. Cincinnati
  # 9. Washington DC
  # 10. Boston
  # 11. St. Louis
  # 12. San Francisco
  # 13. Los Angeles
  # 14. Miami
  # 15. Atlanta
  
  
  census_tract_sample <-
    census_tract_data_full %>% 
    # 1. new york city
    filter((COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York") |
             # 2. chicago
             (COUNTY %in% c("Cook") & STATE == "Illinois") | 
             # 3. detroit
             (COUNTY %in% c("Wayne") & STATE == "Michigan") |
             # 4. cleveland
             (COUNTY %in% c("Cuyahoga") & STATE == "Ohio") |
             # 5. philadelphia
             (COUNTY %in% ("Philadelphia") & STATE == "Pennsylvania") |
             # 6. baltimore
             (COUNTY %in% ("Baltimore City") & STATE == "Maryland") | 
             # 7. pittsburgh
             (COUNTY %in% ("Allegheny") & STATE == "Pennsylvania") |
             # 8. cincinnati
             (COUNTY %in% ("Hamilton") & STATE == "Ohio") |
             # 9. washington dc
             (STATE == "District Of Columbia") |
             # 10. boston
             (COUNTY %in% ("Suffolk") & STATE == "Massachusetts") |
             # 11. st. louis
             (COUNTY %in% ("St Louis City") & STATE == "Missouri") |
             # 12. san francisco
             (COUNTY %in% ("San Francisco") & STATE == "California") |
             # 13. los angeles
             (COUNTY %in% ("Los Angeles") & STATE == "California") |
             # 14. miami
             (COUNTY %in% ("Dade") & STATE == "Florida") |
             # 15. atlanta
             (COUNTY %in% ("Fulton") & STATE == "Georgia")) %>%
    # create a variable called "city"
    mutate(city = case_when(
      COUNTY %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") & STATE == "New York" ~ "New York City",
      COUNTY %in% c("Cook") & STATE == "Illinois" ~ "Chicago",
      COUNTY %in% c("Wayne") & STATE == "Michigan" ~ "Detroit",
      COUNTY %in% c("Cuyahoga") & STATE == "Ohio" ~ "Cleveland",
      COUNTY %in% ("Philadelphia") & STATE == "Pennsylvania" ~ "Philadelphia",
      COUNTY %in% ("Baltimore City") & STATE == "Maryland" ~ "Baltimore",
      COUNTY %in% ("Allegheny") & STATE == "Pennsylvania" ~ "Pittsburgh",
      COUNTY %in% ("Hamilton") & STATE == "Ohio" ~ "Cincinnati",
      STATE == "District Of Columbia" ~ "Washington DC",
      COUNTY %in% ("Suffolk") & STATE == "Massachusetts" ~ "Boston",
      COUNTY %in% ("St Louis City") & STATE == "Missouri" ~ "St. Louis",
      COUNTY %in% ("San Francisco") & STATE == "California" ~ "San Francisco",
      COUNTY %in% ("Los Angeles") & STATE == "California" ~ "Los Angeles",
      COUNTY %in% ("Dade") & STATE == "Florida" ~ "Miami",
      COUNTY %in% ("Fulton") & STATE == "Georgia" ~ "Atlanta"
    ))
}

## Convert Census data to geographic coordinate system (from projected data) ----
census_tract_sample <-
  census_tract_sample %>% 
  st_as_sf() %>% 
  st_transform(4326) %>% 
  # fix the 21 broken geometries
  st_make_valid()
  


## Distance from nearest CBD tract -----
# note Aug 2024: The only counties in sample that don't have CBD tracts
# are counties that are part of greater metro areas (eg the counties near Dallas)

# get cbd tracts in sample
cbd_tracts <-
  census_tract_sample %>%
  filter(cbd == 1, YEAR == 1990) %>% 
  #mutate(centroid = st_centroid(geom)) %>% 
  select(any_of(tract_id_variables), geom) 


# get all non-CBD tracts
all_sample_tracts <- 
  census_tract_sample %>%
  filter(cbd == 0, YEAR == 1990) %>% 
  select(any_of(tract_id_variables), -YEAR, geom)

# calculate minimum distance for each tract
# trying a faster way: Find nearest CBD tract for each tract, then calculate distance
# then you don't need to calculate distance explicitly for each tract
nearest_cbd <- st_nearest_feature(all_sample_tracts, cbd_tracts)
cbd_distances <- st_distance(all_sample_tracts, cbd_tracts[nearest_cbd,], by_element = TRUE)


# add distance_from_cbd to all_sample_tracts
all_sample_tracts <- 
  all_sample_tracts %>% 
  mutate(distance_from_cbd = cbd_distances) %>% 
  st_drop_geometry()

# merge back to the full data
census_tract_sample <- 
  census_tract_sample %>% 
  left_join(all_sample_tracts) %>% 
  # set distance to 0 for CBD tracts
  mutate(distance_from_cbd = ifelse(cbd == 1, 0, distance_from_cbd))

# Prep Public housing data ----
# read data
cdd_projects_merged <- 
  read_csv("data/derived/public_housing/working/merged_cdd_projects.csv")

## geolocation ----
# Get longitude and latitude from NGDA and PSH 
cdd_projects_geolocated <-
  cdd_projects_merged %>% 
  # drop if missing latitude or longitude
  filter(!is.na(latitude), !is.na(longitude))

# convert to sf object
cdd_projects_geolocated_sf <- 
  cdd_projects_geolocated %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# note: Here, 4326 is the EPSG code for WGS 84, the standard coordinate reference system 
# for latitude and longitude. 

# make sure the CRS are matching 
cdd_projects_geolocated_sf <- 
  st_transform(cdd_projects_geolocated_sf, st_crs(census_tract_sample))

st_crs(cdd_projects_geolocated_sf)

# look at only the locations
cdd_projects_geolocated_sf_loc_only <- 
  cdd_projects_geolocated_sf %>% 
  select(project_code, geometry)

# top 50  cities with the most public housing in our data
top_50_localities <- 
  cdd_projects_geolocated %>% 
  group_by(locality, state) %>% 
  summarise(total_units = sum(totunitsplanned, na.rm = TRUE)) %>% 
  arrange(desc(total_units)) %>% 
  head(50) %>% 
  pull(locality)
top_50_localities

# get sample of projects in 15 locations 

if(sample_size == "cdd_large") {
  cdd_project_sample <-
    cdd_projects_geolocated_sf %>%
    filter(locality %in% top_50_localities) %>% 
    # only keep if yearfullocc is not misisng
    filter(!is.na(yearfullocc))
  
} else if (sample_size == "cdd_small") {
  cdd_project_sample <-
    cdd_projects_geolocated_sf %>% 
    filter(
      #1. new york city
      locality == "NEW YORK CITY" |
        #2. chicago  
        locality == "CHICAGO" | 
        # 3. detroit
        (locality == "DETROIT" & state == "MICHIGAN") |
        # 4. cleveland
        locality == "CLEVELAND-CUYAHOGA METROPOLITAN" |
        # 5. philadelphia
        locality == "PHILADELPHIA" |
        # 6. baltimore
        locality == "BALTIMORE CITY" |
        # 7. pittsburgh
        locality == "PITTSBURGH" |
        # 8. cincinnati
        locality == "CINCINNATI-CINCINNATI METROPOLITAN" | 
        # 9. washington dc
        locality == "WASHINGTON DC-NATIONAL CAPITOL" | 
        # 10. boston
        (locality == "BOSTON" & state == "MASSACHUSETTS") |
        # 11. st. louis
        locality == "ST LOUIS CITY" |
        # 12. san francisco
        locality == "SAN FRANCISCO" |
        # 13. los angeles
        locality == "LOS ANGELES" |
        # 14. miami
        locality == "MIAMI-DADE COUNTY HSG & URB DEV" |
        # 15. atlanta
        locality == "ATLANTA" ) %>% 
    # only keep if yearfullocc is not misisng
    filter(!is.na(yearfullocc))
}

# count number of projects by city 
projects_by_city <-
  cdd_project_sample %>% 
  st_drop_geometry() %>%
  count(locality)


# Merge Census and Public Housing data ----
# Here, we will merge and also define "treated" status for each tract in each year using the 
# year of full occupancy

# Define concordance between dates in census_tract_sample and cdd_project_sample
# In particular: Going to follow Guennwig-Moehnert (2023) and define any tract that 
# receives public housing in 1941-1950 as treated in 1950 onward.

cdd_project_sample <- 
  cdd_project_sample %>% 
  # create treatment year = 1940 if yearfullocc = 1931-1940, treatment_year = 1950 if yearfullocc = 1941-1950, etc, up through yearfullocc = 1970
  mutate(treatment_year = case_when(
    yearfullocc %in% 1931:1940 ~ 1940,
    yearfullocc %in% 1941:1950 ~ 1950,
    yearfullocc %in% 1951:1960 ~ 1960,
    yearfullocc %in% 1961:1970 ~ 1970,
    yearfullocc %in% 1971:1980 ~ 1980,
    TRUE ~ NA_real_
  ))


# index the census tract sample data
census_tract_sample_indexed <- 
  census_tract_sample %>% 
  mutate(tract_id = row_number())

# join on geometry, then filter only where YEAR >= treatment_year
# That's when we want to consider the tract treated (eg in 1950, treated if project was built 1941-1950)
treated_tracts <-
  st_join(census_tract_sample_indexed,
          cdd_project_sample, 
          join = st_intersects)

# 2/15/2024: Assume "treatment" happens once: So for each treated tract:
# 1. Sum up the total units planned for the tract in each yearfullocc (in case there's multiple projects planned)
# 2. Keep only the first instance of each year (so, we will just count the number of units planned the first time tract is treated)
# sort by yearfullocc and tract,
# group by state, county, tracta, year
# and keep only the first instance for each year

treated_tracts <-
  treated_tracts %>% 
  group_by(STATE, COUNTY, TRACTA, YEAR, yearfullocc) %>% 
  mutate(totunitsplanned = sum(totunitsplanned, na.rm = TRUE)) %>%
  arrange(STATE, COUNTY, TRACTA, yearfullocc) %>% 
  group_by(STATE, COUNTY, TRACTA, YEAR) %>%
  filter(row_number() == 1) %>%
  ungroup()

# get panel of treated tracts
treated_tracts_panel <-
  treated_tracts %>% 
  # keep if census year >= treatment year
  filter(YEAR >= treatment_year) %>% 
  #st_drop_geometry() %>% 
  select(tract_id, TRACTA, COUNTY, STATE, YEAR, treatment_year, totunitsplanned) %>%
  mutate(treated = 1)

# census sample with treated status
census_tract_sample_with_treatment_status <-
  left_join(census_tract_sample_indexed, treated_tracts_panel %>% select(tract_id, treated) %>% st_drop_geometry(), by = "tract_id") %>% 
  mutate(treated = ifelse(is.na(treated), 0, 1))



# Output datasets as GeoPackage files-------
# Output:
# 1. Census tract sample (with treated status)
# 2. Geolocated public housing project sample
# 3. Treated tracts panel only

# 1.
st_write(census_tract_sample_with_treatment_status, tract_with_treatment_status_filepath,
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

# 2.
st_write(cdd_project_sample, cdd_project_sample_filepath,
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)

# 3.
st_write(treated_tracts_panel, treated_tracts_panel_filepath,
         append = FALSE, layer = "points", driver = "GPKG", overwrite = TRUE, delete_dsn = TRUE)


