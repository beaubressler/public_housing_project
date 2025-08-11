# What this program does:
# Read in the full count Census data for 1930 and 1940 (in chunks due to their size)
# Calculate the following at the city-enumeration district level in each year:
# 1. Population (by race)
# 2. Income (incwage: wage and salary income)
# 3. Years of education (educ, 1940 only)
# 4. Labor force status (among who?)
# 5. Rent (rent: monthly contract rent) per household
# 6. Home value (valueh)

# Because of the size of the dataset, need to do this by chunks
# 

# preliminaries -----
library(tidyverse)
library(here)
library(haven)
library(ipumsr)
library(data.table)

# parallel processing setup
# library(parallel)
# library(future)
# library(furrr)  # For parallel processing
# library(future.apply)

#plan(multisession, workers = 8)  # Use 8 cores for parallel tasks, out of 10

full_count_dir <- here("data", "raw", "ipums", "full_count")
output_dir <- here("data", "derived", "census", "full_count", "ed_by_city")
saavedra_twinam_dir <- here("data", "raw", "saavedra_and_twinam")

# directory of the cleaned Geographic Reference File dataset, which contains the enumeration districts
# that I cleaned
grf_dir <- here("data", "derived", "geographic_reference_file")


# Cities for which I have ED map data
cities <- c(
  "AkronOH", "AlbanyNY", "AtlantaGA", "BaltimoreMD", "BirminghamAL", "BostonMA", 
  "BridgeportCT", "BronxNY", "BrooklynNY", "BuffaloNY", "ChattanoogaTN", 
  "ChicagoIL", "CincinnatiOH", "ClevelandOH", "ColumbusOH", "DallasTX", 
  "DaytonOH", "DenverCO", "DesMoinesIA", "DetroitMI", "FlintMI", "FortWorthTX", 
  "GrandRapidsMI", "HartfordCT","HoustonTX",  "IndianapolisIN", "JacksonvilleFL", 
  "JerseyCityNJ", "KansasCityKS", "KansasCityMO", "LongBeachCA", "LosAngelesCA", 
  "LouisvilleKY", "ManhattanNY", "MemphisTN", "MiamiFL", "MilwaukeeWI", 
  "MinneapolisMN", "NashvilleTN", "NewHavenCT", "NewOrleansLA", "NewarkNJ", 
  "NorfolkVA", "OaklandCA", "OklahomaCityOK", "OmahaNE", "PatersonNJ", 
  "PhiladelphiaPA", "PittsburghPA", "PortlandOR", "ProvidenceRI", "QueensNY", 
  "RichmondVA", "RochesterNY", "SaltLakeCityUT",  "SanAntonioTX", "SanDiegoCA", 
  "SanFranciscoCA", "ScrantonPA", "SeattleWA", "SpokaneWA", "SpringfieldMA", 
  "StLouisMO", "StPaulMN", "StatenIslandNY", "SyracuseNY", "ToledoOH", 
  "TrentonNJ", "TulsaOK", "WashingtonDC", "WorcesterMA", "YonkersNY", 
  "YoungstownOH"
)


# Chunk size
chunk_size_param <- 500000

# Prep Saavedra and Twinam occscores ----
st_occscores <- 
  read_dta(here(saavedra_twinam_dir, "lido_score_1950_public_use.dta")) 

# rename to uppercase
st_occscores <- st_occscores %>%
  rename_with(toupper)

setDT(st_occscores)

# 1930 ----
ddi_file_path_1930 <- here(full_count_dir, "usa_00032.xml")

ddi_1930 <- read_ipums_ddi(ddi_file_path_1930)

# read 10000 rows, all variables
sample_1930 <- read_ipums_micro(ddi_1930, n_max = 10000)

## 1930
grf_1930 <- read_csv(here(grf_dir, "grf_1930_histid_ed_city.csv"))

grf_1930 <- grf_1930 %>% 
  select(histid, b_city, b_ed) %>%
  dplyr::rename(HISTID = histid)

# convert to datatable
setDT(grf_1930)
setkey(grf_1930, HISTID)


# testing
z <- 
  left_join(sample_1930,st_occscores)

z %>% filter(!is.na(LIDO)) %>% pull(AGE) %>% table()

## Population by race ----
# Function that says what to do with each chunk:
cb_function_pop_1930 <- function(x, pos) {
  
  # set as data.table
  setDT(x)
  
  # merge Geographic Reference File info (ED number) onto full count
  x <- merge(x, grf_1930, by = "HISTID", all.x = TRUE)
  
  # collapse by City, ED #, Race
  result <- x[, .(population = .N) , by = .(CITY, b_city, b_ed, RACE)]
  
  return(result)
  
}

# 2. Combine results automatically using a "callback object"
# take results from each chunk and stack them on each other
cb_pop_1930 <- IpumsDataFrameCallback$new(cb_function_pop_1930)

# Read data in chunks
chunked_results_pop_1930 <-
  read_ipums_micro_chunked(ddi_file_path_1930, callback = cb_pop_1930, chunk_size = chunk_size_param)

# Combine results
# Convert to data.table for faster processing
setDT(chunked_results_pop_1930)

# Process population by race using data.table - much faster than dplyr
population_by_race_city_ed_1930 <-
  chunked_results_pop_1930[CITY != 0 & !is.na(b_ed), 
    .(population = sum(population)), 
    by = .(CITY_NAME = as.character(as_factor(CITY)),
           b_ed, b_city, 
           race = as.character(as_factor(RACE)))
  ]

# Convert to wide format using data.table's dcast
population_by_race_city_ed_1930 <- dcast(population_by_race_city_ed_1930, 
                                         CITY_NAME + b_ed + b_city ~ race, 
                                         value.var = "population", 
                                         fill = 0)

# Create race columns using direct column references - the .SD logic was causing population loss
population_by_race_city_ed_1930[, `:=`(
  white_pop = White,
  black_pop = `Black/African American`,
  other_pop = Chinese + `American Indian or Alaska Native` + Japanese + `Other Asian or Pacific Islander`
)]

# Select only needed columns  
population_by_race_city_ed_1930 <- population_by_race_city_ed_1930[, .(CITY_NAME, b_ed, b_city, white_pop, black_pop, other_pop)] 

## Literacy -----
# Share of Age 25+ that is literate
cb_function_literacy_1930 <- function(x, pos) {
  setDT(x)  # Ensure x is a data.table
  
  # Merge with grf_1930 to get ED info
  x <- merge(x, grf_1930, by = "HISTID", all.x = TRUE)
  
  # REMOVED: as_tibble(x) conversion - keep as data.table for memory efficiency
  
  # Filter for age 25+ and drop missing literacy values - data.table syntax
  x <- x[AGE >= 25 & !is.na(LIT) & LIT != 0]
  
  # Create literacy indicator: 1 if literate (LIT==4), 0 otherwise
  x[, literate := ifelse(LIT == 4, 1, 0)]
  
  # Aggregate by CITY, borough, and ED - count total people and literate people
  result <- x[, .(total = .N, literate_count = sum(literate)), by = .(CITY, b_city, b_ed)]
  
  # Calculate literacy rate as proportion
  result[, literacy_rate := literate_count / total]
  
  return(result)
}

cb_literacy_1930 <- IpumsDataFrameCallback$new(cb_function_literacy_1930)

# read data in chunks
chunked_results_literacy_1930 <-
  read_ipums_micro_chunked(ddi_file_path_1930, callback = cb_literacy_1930, chunk_size = chunk_size_param)

# combine and clean the results
literacy_by_city_ed_1930 <-
  chunked_results_literacy_1930 %>%
  filter(CITY != 0, !is.na(b_ed)) %>% 
  mutate(CITY_NAME = as.character(as_factor(CITY))) %>% 
  select(CITY_NAME, b_ed, b_city, literacy_rate) 

head(literacy_by_city_ed_1930)

## Housing ------
### Rent ----
cb_function_rent_1930 <- function(x, pos) {
  setDT(x)  # Ensure x is a data.table
  
  # Merge with grf_1930 to get ED info
  x <- merge(x, grf_1930, by = "HISTID", all.x = TRUE)
  
  # REMOVED: as_tibble(x) conversion - keep as data.table for memory efficiency
  
  # Rename RENT30 to rent for easier reference
  setnames(x, "RENT30", "rent")
  
  # Keep only head of household and filter out missing/invalid rent values
  # RELATE==1 is appropriate here since rent is a household-level variable
  x <- x[RELATE == 1 & rent != 0 & rent != 9999 & rent != 9998]
  
  # Create rent groups using cut function - cleaner than nested fifelse
  rent_breaks <- c(0, 5, 6, 9, 14, 19, 24, 29, 39, 49, 59, 74, 99, 149, 199, Inf)
  x[, rent_group := cut(rent, breaks = rent_breaks, labels = 1:15, right = TRUE, include.lowest = TRUE)]
  x[, rent_group := as.integer(rent_group)]
  
  # Aggregate by CITY, borough, ED, and rent group - count households in each group
  result <- x[, .(population = .N), by = .(CITY, b_ed, b_city, rent_group)]
  
  return(result)
}

# 2. Combine results automatically using a "callback object"
# take results from each chunk and stack them on each other
cb_rent_1930 <- IpumsDataFrameCallback$new(cb_function_rent_1930)

# Read data in chunks
chunked_results_rent_1930 <-
  read_ipums_micro_chunked(ddi_file_path_1930, callback = cb_rent_1930, chunk_size = chunk_size_param)

# Combine results
# Convert to data.table and process rent groups - much faster than dplyr
setDT(chunked_results_rent_1930)

rent_group_by_city_ed_1930 <-
  chunked_results_rent_1930[CITY != 0 & !is.na(b_ed), 
    .(population = sum(population)), 
    by = .(CITY_NAME = as.character(as_factor(CITY)), b_ed, b_city, rent_group)
  ][,
    rent_group := paste0("rent_group_", rent_group)
  ]

# Convert to wide format using data.table's dcast
rent_group_by_city_ed_1930 <- dcast(rent_group_by_city_ed_1930, 
                                    CITY_NAME + b_ed + b_city ~ rent_group, 
                                    value.var = "population", 
                                    fill = 0)
  


### Home values (VALUEH) ----
# VALUEH is already a "group" variable

cb_function_home_value_1930 <- function(x, pos) {
  # set as data.table
  setDT(x)
  
  # keep only if RELATE == 1 (head of household)
  x <- x[RELATE == 1]
  
  # merge Geographic Reference File info (ED number) onto full count
  x <- merge(x, grf_1930, by = "HISTID", all.x = TRUE)
  
  # REMOVED: as_tibble(x) conversion - keep as data.table for memory efficiency
  
  # Rename VALUEH to valueh for easier reference
  setnames(x, "VALUEH", "valueh")
  
  # Keep only head of household and filter out missing/invalid home values
  # RELATE==1 is appropriate here since home value is a household-level variable
  x <- x[RELATE == 1 & valueh != 0 & valueh != 9999998 & valueh != 9999999]
  
  # Create home value groups using cut function - cleaner than nested fifelse
  # Home value ranges: 0-500, 501-699, 700-999, 1000-1499, 1500-1999, 2000-2499, 2500-2999, 3000-3999, 4000-4999, 5000-5999, 6000-7499, 7500-9999, 10000-14999, 15000-19999, 20000+
  valueh_breaks <- c(0, 500, 699, 999, 1499, 1999, 2499, 2999, 3999, 4999, 5999, 7499, 9999, 14999, 19999, Inf)
  x[, valueh_group := cut(valueh, breaks = valueh_breaks, labels = 1:15, right = TRUE, include.lowest = TRUE)]
  x[, valueh_group := as.integer(valueh_group)]
  
  # Group by CITY, b_ed, and valueh_group and calculate population
  result <- x[, .(population = .N), by = .(CITY, b_ed, b_city, valueh_group)]
  
  return(result)
}  

# 2. Combine results automatically using a "callback object"
# take results from each chunk and stack them on each other
cb_home_value_1930 <- IpumsDataFrameCallback$new(cb_function_home_value_1930)

# Read data in chunks
chunked_results_home_value_1930 <-
  read_ipums_micro_chunked(ddi_file_path_1930, callback = cb_home_value_1930, chunk_size = chunk_size_param)

# Combine results
home_value_by_city_ed_1930 <-
  chunked_results_home_value_1930 %>%
  # drop missing CITY and b_ed
  filter(CITY != 0, !is.na(b_ed)) %>% 
  mutate(CITY_NAME = as.character(as_factor(CITY))) %>% 
  group_by(CITY_NAME, b_city, b_ed, valueh_group) %>%
  summarize(population= sum(population), .groups = "drop") %>% 
  mutate(valueh_group = paste0("home_value_", valueh_group)) %>% 
  pivot_wider(names_from = valueh_group, 
              values_from = population,
              values_fill = 0)
  

## Labor force and unemployment, age 14 and up -----
cb_function_lf_1930 <- function(x, pos) {
  
  # set as data.table
  setDT(x)
  
  # REMOVED: tibble conversions - keep as data.table for memory efficiency
  # Keep only individuals age 14 and up, following Census definition of labor force
  x <- x[AGE >= 14]

  # merge Geographic Reference File info (ED number) onto full count
  x <- merge(x, grf_1930, by = "HISTID", all.x = TRUE)
  
  # Aggregate by CITY, borough, ED, and employment status - count people in each category
  result <- x[, .(population = .N) , by = .(CITY, b_ed, b_city, EMPSTAT)]
  
  return(result)
  
}

# 2. Combine results automatically using a "callback object"
# take results from each chunk and stack them on each other
cb_lf_1930 <- IpumsDataFrameCallback$new(cb_function_lf_1930)

# Read data in chunks
chunked_results_lf_1930 <-
  read_ipums_micro_chunked(ddi_file_path_1930, callback = cb_lf_1930, chunk_size = chunk_size_param)

# Combine results
lf_status_by_city_ed_1930 <-
  chunked_results_lf_1930 %>%
  # drop missing CITY and b_ed
  filter(CITY != 0, !is.na(b_ed)) %>% 
  # drop if EMPSTAT is 0 or 9 (unknown)
  filter(EMPSTAT != 0, EMPSTAT != 9) %>%
  mutate(CITY_NAME = as.character(as_factor(CITY)),
         employment_status = case_when(
           EMPSTAT == 1 ~ "employed",
           EMPSTAT == 2 ~ "unemployed",
           EMPSTAT == 3 ~ "not_in_lf"
         )) %>% 
  group_by(CITY_NAME, b_ed, b_city, employment_status) %>%
  summarize(population= sum(population), .groups = "drop") %>% 
  pivot_wider(names_from = employment_status, 
              values_from = population,
              values_fill =  0) %>%
  select(CITY_NAME, b_ed, b_city, employed, unemployed, not_in_lf) 

## Occupation-based income (LIDO) ------
# Use Saavedra and Twinam (2020) method to calculate income based on occupation codes

cb_function_lido_income_1930 <- function(x, pos) {
  setDT(x)
  
  # merge with st_occscores to get LIDO
  x <- merge(x, st_occscores, by = c("STATEFIP", "SEX", "AGE", "RACE", "OCC1950", "IND1950"), all.x = TRUE)
  
  # Merge with grf_1930
  x <- merge(x, grf_1930, by = "HISTID", all.x = TRUE)
  
  # REMOVED: as_tibble(x) conversion - keep as data.table for memory efficiency
  
  # Filter for individuals with valid LIDO scores only
  x <- x[!is.na(LIDO)]
  
  # Calculate family LIDO-based income by summing LIDO scores within each family
  family_income <- x[, .(family_wage_raw = sum(LIDO)), by = .(CITY, b_ed, b_city, SERIAL)]
  
  # Convert LIDO scores to actual 1950 dollars and create income groups
  # These are the 1950 tract-based income groups
  family_income[, family_wage := family_wage_raw * 100]  # Convert to actual 1950 dollars
  
  # Create income groups using data.table's cut function - cleaner than nested fifelse
  breaks <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 6000, 7000, 10000, Inf)
  family_income[, income_group := cut(family_wage, breaks = breaks, labels = 1:14, right = FALSE, include.lowest = TRUE)]
  family_income[, income_group := as.integer(income_group)]
  
  # Return family-level data with income groups - each row represents one family
  result <- family_income[, .(population = .N), by = .(CITY, b_ed, b_city, income_group)]
  return(result)
}


# 2. Combine results automatically using a "callback object"
# take results from each chunk and stack them on each other
cb_lido_1930 <- IpumsDataFrameCallback$new(cb_function_lido_income_1930)

# Read data in chunks
chunked_results_lido_1930 <-
  read_ipums_micro_chunked(ddi_file_path_1930, callback = cb_lido_1930, chunk_size = chunk_size_param)

# Combine results
lido_income_by_city_ed_1930 <-
  chunked_results_lido_1930 %>%
  # drop missing CITY and b_ed
  filter(CITY != 0, !is.na(b_ed)) %>% 
  mutate(CITY_NAME = as.character(as_factor(CITY))) %>%
  group_by(CITY_NAME, b_ed, b_city, income_group) %>%
  summarize(population = sum(population), .groups = "drop") %>%
  mutate(income_group = paste0("income_group_", income_group)) %>%
  pivot_wider(names_from = income_group,
              values_from = population,
              values_fill = 0)


## Combine and output -----

# combine datasets
# Combine datasets using data.table merges - much faster than dplyr left_join
# Convert all to data.table first
setDT(population_by_race_city_ed_1930)
setDT(literacy_by_city_ed_1930)
setDT(rent_group_by_city_ed_1930)
setDT(home_value_by_city_ed_1930)
setDT(lf_status_by_city_ed_1930)
setDT(lido_income_by_city_ed_1930)

# Set keys for faster joins
setkeyv(population_by_race_city_ed_1930, c("CITY_NAME", "b_ed", "b_city"))
setkeyv(literacy_by_city_ed_1930, c("CITY_NAME", "b_ed", "b_city"))
setkeyv(rent_group_by_city_ed_1930, c("CITY_NAME", "b_ed", "b_city"))
setkeyv(home_value_by_city_ed_1930, c("CITY_NAME", "b_ed", "b_city"))
setkeyv(lf_status_by_city_ed_1930, c("CITY_NAME", "b_ed", "b_city"))
setkeyv(lido_income_by_city_ed_1930, c("CITY_NAME", "b_ed", "b_city"))

# Perform left joins to preserve all population records
city_ed_data_1930 <- merge(population_by_race_city_ed_1930, literacy_by_city_ed_1930,
                           by = c("CITY_NAME", "b_ed", "b_city"), all.x = TRUE)
city_ed_data_1930 <- merge(city_ed_data_1930, rent_group_by_city_ed_1930,
                           by = c("CITY_NAME", "b_ed", "b_city"), all.x = TRUE)
city_ed_data_1930 <- merge(city_ed_data_1930, home_value_by_city_ed_1930,
                           by = c("CITY_NAME", "b_ed", "b_city"), all.x = TRUE)
city_ed_data_1930 <- merge(city_ed_data_1930, lf_status_by_city_ed_1930,
                           by = c("CITY_NAME", "b_ed", "b_city"), all.x = TRUE)
city_ed_data_1930 <- merge(city_ed_data_1930, lido_income_by_city_ed_1930,
                           by = c("CITY_NAME", "b_ed", "b_city"), all.x = TRUE) 
  
# output csv
write_csv(city_ed_data_1930, here(output_dir, "city_ed_data_1930.csv"))

# Memory cleanup after 1930 processing - explicit object removal for safety
rm(population_by_race_city_ed_1930, literacy_by_city_ed_1930, 
   rent_group_by_city_ed_1930, home_value_by_city_ed_1930,
   lf_status_by_city_ed_1930, lido_income_by_city_ed_1930,
   chunked_results_pop_1930, chunked_results_literacy_1930,
   chunked_results_rent_1930, chunked_results_home_value_1930,
   chunked_results_lf_1930, chunked_results_lido_1930,
   cb_pop_1930, cb_literacy_1930, cb_rent_1930, cb_home_value_1930, cb_lf_1930, cb_lido_1930,
   grf_1930, st_occscores, city_ed_data_1930, ddi_1930, sample_1930)

# Force garbage collection 
gc()


# 1940 -----
## 1940
grf_1940 <- read_csv(here(grf_dir, "grf_1940_histid_ed_city.csv"))

grf_1940 <- grf_1940 %>% 
  select(histid, b_ed, b_city) %>%
  dplyr::rename(HISTID = histid)

# convert to datatable
setDT(grf_1940)
setkey(grf_1940, HISTID)


ddi_file_path_1940 <- here(full_count_dir, "usa_00033.xml")

ddi_1940 <- read_ipums_ddi(ddi_file_path_1940)

# read 10000 rows, all variables
sample_1940 <- read_ipums_micro(ddi_1940, n_max = 10000)

## Population by race -----
cb_function_pop_1940 <- function(x, pos) {
  # set as data.table
  setDT(x)
  
  # merge Geographic Reference File info (ED number) onto full count
  x <- merge(x, grf_1940, by = "HISTID", all.x = TRUE)
  
  # Aggregate by CITY, borough, ED, and race - count people in each category
  result <- x[, .(population = .N) , by = .(CITY, b_ed, b_city, RACE)]
  
  return(result)

}

# 2. Combine results automatically using a "callback object"
# take results from each chunk and stack them on each other
cb_pop_1940 <- IpumsDataFrameCallback$new(cb_function_pop_1940)

# Read data in chunks
chunked_results_pop_1940 <-
  read_ipums_micro_chunked(ddi_file_path_1940, callback = cb_pop_1940, chunk_size = chunk_size_param)

# Combine results
# Convert to data.table for faster processing
setDT(chunked_results_pop_1940)

# Process population by race using data.table - much faster than dplyr
pop_by_race_by_city_ed_1940 <-
  chunked_results_pop_1940[CITY != 0 & !is.na(b_ed), 
    .(population = sum(population)), 
    by = .(CITY_NAME = as.character(as_factor(CITY)),
           b_ed, b_city, 
           race = as.character(as_factor(RACE)))
  ]

# Convert to wide format using data.table's dcast
pop_by_race_by_city_ed_1940 <- dcast(pop_by_race_by_city_ed_1940, 
                                     CITY_NAME + b_ed + b_city ~ race, 
                                     value.var = "population", 
                                     fill = 0)

# Create race columns using direct column references - the .SD logic was causing population loss
pop_by_race_by_city_ed_1940[, `:=`(
  white_pop = White,
  black_pop = `Black/African American`,
  other_pop = Chinese + `American Indian or Alaska Native` + Japanese + `Other Asian or Pacific Islander`
)]

# Select only needed columns
pop_by_race_by_city_ed_1940 <- pop_by_race_by_city_ed_1940[, .(CITY_NAME, b_ed, b_city, white_pop, black_pop, other_pop)]

# Clean up chunked results to free memory
rm(chunked_results_pop_1940)
gc() 

## Education -----
# Following 1940 tract data: Persons 25 years and older by years of school completed

cb_function_educ_1940 <- function(x, pos) {
  setDT(x)  # Ensure x is a data.table
  
  # Merge with grf_1940 to get ED info
  x <- merge(x, grf_1940, by = "HISTID", all.x = TRUE)
  
  # REMOVED: as_tibble(x) conversion - keep as data.table for memory efficiency
  
  # Filter for education analysis: persons 25+ with valid education data
  # Keep only persons 25 years and older, drop missing/invalid education codes
  x <- x[AGE >= 25 & EDUC != 0 & EDUC != 99]
  
  # Aggregate by CITY, borough, ED, and education level - count people by education
  result <- x[, .(population = .N), by = .(CITY, b_city, b_ed, EDUC)]
  
  return(result)
}

cb_education_1940 <- IpumsDataFrameCallback$new(cb_function_educ_1940)

chunked_results_educ_1940 <-
  read_ipums_micro_chunked(ddi_file_path_1940, callback = cb_education_1940, chunk_size = chunk_size_param)


# Convert to data.table and process education data - much faster than dplyr  
setDT(chunked_results_educ_1940)

pop_by_educ_by_city_ed_1940 <-
  chunked_results_educ_1940[CITY != 0 & !is.na(b_ed), 
    .(population = sum(population)), 
    by = .(CITY_NAME = as.character(as_factor(CITY)), b_ed, b_city, EDUC)
  ][,
    `:=`(hs_grad = ifelse(EDUC >= 6, population, 0),
         some_college = ifelse(EDUC >= 7, population, 0))
  ][,
    .(total_educ_pop = sum(population),
      hs_grad_pop = sum(hs_grad),
      some_college_pop = sum(some_college)), 
    by = .(CITY_NAME, b_ed, b_city)
  ]

# Clean up chunked results to free memory
rm(chunked_results_educ_1940)
gc() 


## Income (INCWAGE) ----
# TODO
# Emiliano Harris JMP does something with 1940 wage income to make it more comparable 
# with later years... could be worth doing that here
# basically he just uses the indicator of whether person makes more than $50 in nonwage income
# and excludes people who do, for whom their wage income doesn't reflect their actual income

# Right now, I am just using their wage income variable as is

# Calculate family wage income 

cb_function_income_1940 <- function(x, pos) {

  setDT(x)  # Ensure x is a data.table
  
  # Merge with grf_1940 to get ED info
  x <- merge(x, grf_1940, by = "HISTID", all.x = TRUE)
  
  # REMOVED: as_tibble(x) conversion - keep as data.table for memory efficiency
  
  # Filter out missing/invalid wage income values
  x <- x[INCWAGE != 999998 & INCWAGE != 999999 & INCWAGE != 0]
  
  # Calculate family wage income by summing individual wages within each family (SERIAL)
  family_income <- x[, .(family_wage = sum(INCWAGE)), by = .(CITY, b_ed, b_city, SERIAL)]
  
  # Create income groups using cut function - cleaner than nested fifelse
  # Income ranges: 0-499, 500-999, 1000-1499, 1500-1999, 2000-2499, 2500-2999, 3000-3499, 3500-3999, 4000-4499, 4500-4999, 5000-5999, 6000-6999, 7000-9999, 10000+
  # TODO: Consider converting to 1950 dollars before grouping for consistency
  income_breaks <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 6000, 7000, 10000, Inf)
  family_income[, income_group := cut(family_wage, breaks = income_breaks, labels = 1:14, right = FALSE, include.lowest = TRUE)]
  family_income[, income_group := as.integer(income_group)]
  
  # Return family-level data with income groups
  return(family_income)
  
}

# Combine results automatically using a "callback object"
cb_income_1940 <- IpumsDataFrameCallback$new(cb_function_income_1940)

# Read data in chunks
chunked_results_income_1940 <- 
  read_ipums_micro_chunked(ddi_file_path_1940, callback = cb_income_1940, chunk_size = chunk_size_param)

# Convert to data.table for faster processing
setDT(chunked_results_income_1940)

# Create income groups using data.table operations - much faster than dplyr
income_group_by_city_ed_1940 <-
  chunked_results_income_1940[CITY != 0 & !is.na(b_ed), 
    .(family_wage = sum(family_wage)), 
    by = .(CITY, b_ed, b_city, SERIAL)
  ][, 
    # Create income groups using cut function - cleaner and faster than case_when
    income_group := cut(family_wage, 
                       breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 6000, 7000, 10000, Inf),
                       labels = 1:14, 
                       right = FALSE, 
                       include.lowest = TRUE)
  ][, 
    income_group := as.integer(income_group)
  ][,
    .(population = .N), 
    by = .(CITY, b_ed, b_city, income_group)
  ][,
    CITY_NAME := as.character(as_factor(CITY))
  ][,
    .(population = sum(population)), 
    by = .(CITY_NAME, b_ed, b_city, income_group)
  ][,
    income_group := paste0("income_group_", income_group)
  ]

# Convert to wide format using data.table's dcast - much faster than pivot_wider
income_group_by_city_ed_1940 <- dcast(income_group_by_city_ed_1940, 
                                      CITY_NAME + b_ed + b_city ~ income_group, 
                                      value.var = "population", 
                                      fill = 0)

# Drop chunked reuslts income, as it's quite large
rm(chunked_results_income_1940)
gc()

## Housing ----
### Rent (RENT) ----
cb_function_rent_1940 <- function(x, pos) {
  setDT(x)  # Ensure x is a data.table
  
  # Merge with grf_1930
  x <- merge(x, grf_1940, by = "HISTID", all.x = TRUE)
  
  # Rename RENT to rent for easier reference  
  setnames(x, "RENT", "rent")
  
  # Keep only head of household and filter out missing/invalid rent values
  # RELATE==1 is appropriate here since rent is a household-level variable
  x <- x[RELATE == 1 & rent != 0 & rent != 9999 & rent != 9998]
  
  # Create rent groups using cut function - cleaner than nested fifelse
  rent_breaks <- c(0, 5, 6, 9, 14, 19, 24, 29, 39, 49, 59, 74, 99, 149, 199, Inf)
  x[, rent_group := cut(rent, breaks = rent_breaks, labels = 1:15, right = TRUE, include.lowest = TRUE)]
  x[, rent_group := as.integer(rent_group)]
  
  # Aggregate by CITY, borough, ED, and rent group - count households in each group
  result <- x[, .(population = .N), by = .(CITY, b_ed, b_city, rent_group)]
  
  return(result)
}

# Combine results automatically using a "callback object"
cb_rent_1940 <- IpumsDataFrameCallback$new(cb_function_rent_1940)

# Read data in chunks
chunked_results_rent_1940 <-
  read_ipums_micro_chunked(ddi_file_path_1940, callback = cb_rent_1940, chunk_size = chunk_size_param)

# Convert to data.table and process rent groups - much faster than dplyr
setDT(chunked_results_rent_1940)

rent_group_by_city_ed_1940 <-
  chunked_results_rent_1940[CITY != 0 & !is.na(b_ed), 
    .(population = sum(population)), 
    by = .(CITY_NAME = as.character(as_factor(CITY)), b_ed, b_city, rent_group)
  ][,
    rent_group := paste0("rent_group_", rent_group)
  ]

# Convert to wide format using data.table's dcast
rent_group_by_city_ed_1940 <- dcast(rent_group_by_city_ed_1940, 
                                    CITY_NAME + b_ed + b_city ~ rent_group, 
                                    value.var = "population", 
                                    fill = 0)

# Clean up chunked results to free memory
rm(chunked_results_rent_1940)
gc()

### Value of home (VALUEH) ----
cb_function_valueh_1940 <- function(x, pos) {
  setDT(x)  # Ensure x is a data.table
  
  # Merge with grf_1930
  x <- merge(x, grf_1940, by = "HISTID", all.x = TRUE)
  
  # REMOVED: as_tibble(x) conversion - keep as data.table for memory efficiency
  
  # Rename VALUEH to valueh for easier reference
  setnames(x, "VALUEH", "valueh")
  
  # Keep only head of household and filter out missing/invalid home values
  # RELATE==1 is appropriate here since home value is a household-level variable
  x <- x[RELATE == 1 & valueh != 0 & valueh != 9999998 & valueh != 9999999]
  
  # Create home value groups using cut function - cleaner than nested fifelse  
  # Home value ranges: 0-500, 501-699, 700-999, 1000-1499, 1500-1999, 2000-2499, 2500-2999, 3000-3999, 4000-4999, 5000-5999, 6000-7499, 7500-9999, 10000-14999, 15000-19999, 20000+
  valueh_breaks <- c(0, 500, 699, 999, 1499, 1999, 2499, 2999, 3999, 4999, 5999, 7499, 9999, 14999, 19999, Inf)
  x[, valueh_group := cut(valueh, breaks = valueh_breaks, labels = 1:15, right = TRUE, include.lowest = TRUE)]
  x[, valueh_group := as.integer(valueh_group)]
  
  # Group by CITY, b_ed, and valueh_group and calculate population
  result <- x[, .(population = .N), by = .(CITY, b_ed, b_city, valueh_group)]
  
  return(result)
}

# Combine results automatically using a "callback object"
cb_valueh_1940 <- IpumsDataFrameCallback$new(cb_function_valueh_1940)

# Read data in chunks
chunked_results_valueh_1940 <-
  read_ipums_micro_chunked(ddi_file_path_1940, callback = cb_valueh_1940, chunk_size = chunk_size_param)

# Convert to data.table and process home value groups - much faster than dplyr
setDT(chunked_results_valueh_1940)

valueh_group_by_city_ed_1940 <-
  chunked_results_valueh_1940[CITY != 0 & !is.na(b_ed), 
    .(population = sum(population)), 
    by = .(CITY_NAME = as.character(as_factor(CITY)), b_ed, b_city, valueh_group)
  ][,
    valueh_group := paste0("valueh_group_", valueh_group)
  ]

# Convert to wide format using data.table's dcast
valueh_group_by_city_ed_1940 <- dcast(valueh_group_by_city_ed_1940, 
                                      CITY_NAME + b_ed + b_city ~ valueh_group, 
                                      value.var = "population", 
                                      fill = 0)

# Clean up chunked results to free memory
rm(chunked_results_valueh_1940)
gc()

## Labor force and unemployment -----
cb_function_lf_1940 <- function(x, pos) {
  
  # set as data.table
  setDT(x)
  
  # Keep only individuals age 14 and up, following Census - data.table syntax
  x <- x[AGE >= 14]
  
  # merge Geographic Reference File info (ED number) onto full count
  x <- merge(x, grf_1940, by = "HISTID", all.x = TRUE)
  
  # collapse by City, ED #, Race
  result <- x[, .(population = .N) , by = .(CITY, b_ed, b_city, EMPSTAT)]
  
  return(result)
  
}

# 2. Combine results automatically using a "callback object"
# take results from each chunk and stack them on each other
cb_lf_1940 <- IpumsDataFrameCallback$new(cb_function_lf_1940)

# Read data in chunks
chunked_results_lf_1940 <-
  read_ipums_micro_chunked(ddi_file_path_1940, callback = cb_lf_1940, chunk_size = chunk_size_param)

# Combine results
# Convert to data.table and process labor force data - much faster than dplyr
setDT(chunked_results_lf_1940)

lf_status_by_city_ed_1940 <-
  chunked_results_lf_1940[CITY != 0 & !is.na(b_ed) & EMPSTAT != 0 & EMPSTAT != 9, 
    .(population = sum(population)), 
    by = .(CITY_NAME = as.character(as_factor(CITY)), 
           b_ed, b_city, 
           employment_status = fcase(
             EMPSTAT == 1, "employed",
             EMPSTAT == 2, "unemployed", 
             EMPSTAT == 3, "not_in_lf",
             default = NA_character_
           ))
  ]

# Convert to wide format using data.table's dcast
lf_status_by_city_ed_1940 <- dcast(lf_status_by_city_ed_1940, 
                                   CITY_NAME + b_ed + b_city ~ employment_status, 
                                   value.var = "population", 
                                   fill = 0)

# Select only needed columns
lf_status_by_city_ed_1940 <- lf_status_by_city_ed_1940[, .(CITY_NAME, b_ed, b_city, employed, unemployed, not_in_lf)]

# Clean up chunked results to free memory
rm(chunked_results_lf_1940)
gc() 

## Combine and output -----

# Combine datasets using data.table merges - much faster than dplyr left_join
# Convert all to data.table first
setDT(pop_by_race_by_city_ed_1940)
setDT(rent_group_by_city_ed_1940)
setDT(valueh_group_by_city_ed_1940)
setDT(lf_status_by_city_ed_1940)
setDT(pop_by_educ_by_city_ed_1940)
setDT(income_group_by_city_ed_1940)

# Set keys for faster joins
setkeyv(pop_by_race_by_city_ed_1940, c("CITY_NAME", "b_ed", "b_city"))
setkeyv(rent_group_by_city_ed_1940, c("CITY_NAME", "b_ed", "b_city"))
setkeyv(valueh_group_by_city_ed_1940, c("CITY_NAME", "b_ed", "b_city"))
setkeyv(lf_status_by_city_ed_1940, c("CITY_NAME", "b_ed", "b_city"))
setkeyv(pop_by_educ_by_city_ed_1940, c("CITY_NAME", "b_ed", "b_city"))
setkeyv(income_group_by_city_ed_1940, c("CITY_NAME", "b_ed", "b_city"))

# Perform data.table merges
city_ed_data_1940 <- pop_by_race_by_city_ed_1940[
  rent_group_by_city_ed_1940, on = c("CITY_NAME", "b_ed", "b_city")][
  valueh_group_by_city_ed_1940, on = c("CITY_NAME", "b_ed", "b_city")][
  lf_status_by_city_ed_1940, on = c("CITY_NAME", "b_ed", "b_city")][
  pop_by_educ_by_city_ed_1940, on = c("CITY_NAME", "b_ed", "b_city")][
  income_group_by_city_ed_1940, on = c("CITY_NAME", "b_ed", "b_city")]

# alternatively:
city_ed_data_1940 <- merge(pop_by_race_by_city_ed_1940, rent_group_by_city_ed_1940,
                           by = c("CITY_NAME", "b_ed", "b_city"), all.x = TRUE)
city_ed_data_1940 <- merge(city_ed_data_1940, valueh_group_by_city_ed_1940,
                           by = c("CITY_NAME", "b_ed", "b_city"), all.x = TRUE)
city_ed_data_1940 <- merge(city_ed_data_1940, lf_status_by_city_ed_1940,
                           by = c("CITY_NAME", "b_ed", "b_city"), all.x = TRUE)
city_ed_data_1940 <- merge(city_ed_data_1940, pop_by_educ_by_city_ed_1940,
                           by = c("CITY_NAME", "b_ed", "b_city"), all.x = TRUE)
city_ed_data_1940 <- merge(city_ed_data_1940, income_group_by_city_ed_1940,
                           by = c("CITY_NAME", "b_ed", "b_city"), all.x = TRUE)

# output csv
write_csv(city_ed_data_1940, here(output_dir, "city_ed_data_1940.csv"))

    
  
  