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
library(ipumsr)
library(data.table)

# parallel processing set-upo
# library(parallel)
# library(future)
# library(furrr)  # For parallel processing
# library(future.apply)

#plan(multisession, workers = 8)  # Use 8 cores for parallel tasks, out of 10



full_count_dir <- here("data", "raw", "ipums", "full_count")
output_dir <- here("data", "derived", "census", "full_count", "ed_by_city")

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


chunk_size_param <- 500000

# 1930 ----
ddi_file_path_1930 <- here(full_count_dir, "usa_00032.xml")

ddi_1930 <- read_ipums_ddi(ddi_file_path_1930)

# read 10000 rows, all variables
sample_1930 <- read_ipums_micro(ddi_1930, n_max = 10000)

## 1930
grf_1930 <- read_csv(here(grf_dir, "grf_1930_histid_ed_city.csv"))

grf_1930 <- grf_1930 %>% 
  select(histid, b_ed) %>%
  dplyr::rename(HISTID = histid)

# convert to datatable
setDT(grf_1930)
setkey(grf_1930, HISTID)



## Population by race ----
# Function that says what to do with each chunk:
cb_function_pop_1930 <- function(x, pos) {
  
  # set as data.table
  setDT(x)
  
  # merge Geographic Reference File info (ED number) onto full count
  x <- merge(x, grf_1930, by = "HISTID", all.x = TRUE)
  
  # collapse by City, ED #, Race
  result <- x[, .(population = .N) , by = .(CITY, b_ed, RACE)]
  
  return(result)
  
}

# 2. Combine results automatically using a "callback object"
# take results from each chunk and stack them on each other
cb_pop_1930 <- IpumsDataFrameCallback$new(cb_function_pop_1930)

# Read data in chunks
chunked_results_pop_1930 <-
  read_ipums_micro_chunked(ddi_file_path_1930, callback = cb_pop_1930, chunk_size = chunk_size_param)

# Combine results
population_by_race_city_ed_1930 <-
  chunked_results_pop_1930 %>%
  # drop missing CITY and b_ed
  filter(CITY != 0, !is.na(b_ed)) %>% 
  mutate(CITY_NAME = as.character(as_factor(CITY)),
         race = as.character(as_factor(RACE))) %>% 
  group_by(CITY_NAME, b_ed, race) %>%
  summarize(population= sum(population), .groups = "drop") %>% 
  pivot_wider(names_from = race, 
              values_from = population,
              values_fill =  0) %>% 
  mutate(white_pop = White,
         black_pop = `Black/African American`,
         other_pop = Chinese + `American Indian or Alaska Native` + Japanese +`Other Asian or Pacific Islander`) %>% 
  select(CITY_NAME, b_ed, white_pop, black_pop, other_pop) 

## Housing ------
### Rent ----
cb_function_rent_1930 <- function(x, pos) {
  setDT(x)  # Ensure x is a data.table
  
  # Merge with grf_1930
  x <- merge(x, grf_1930, by = "HISTID", all.x = TRUE)
  
  x <- as_tibble(x)
  
  # Create rent_groups
  # These groups are based on X
  x <- x %>%
    dplyr::rename(rent = RENT30) %>% 
    # Keep only the head of household 
    filter(RELATE == 1) %>% 
    # drop rent = 0 (NA) and rent == 9999 (missing) and rent == 9998 (no cash rent)
    filter(rent != 0, rent != 9999, rent != 9998) %>% 
    mutate(rent_group = case_when(
      rent >= 0 & rent <= 5 ~ 1,
      rent > 5 & rent <= 6 ~ 2,
      rent > 6 & rent <= 9 ~ 3,
      rent > 9 & rent <= 14 ~ 4,
      rent > 14 & rent <= 19 ~ 5,
      rent > 19 & rent <= 24 ~ 6,
      rent > 24 & rent <= 29 ~ 7,
      rent > 29 & rent <= 39 ~ 8,
      rent > 39 & rent <= 49 ~ 9,
      rent > 49 & rent <= 59 ~ 10,
      rent > 59 & rent <= 74 ~ 11,
      rent > 74 & rent <= 99 ~ 12,
      rent > 99 & rent <= 149 ~ 13,
      rent > 149 & rent <= 199 ~ 14,
      rent > 199 ~ 15,
      TRUE ~ NA_integer_
    ))
  
  # Convert back to data.table
  setDT(x)
  
  # Group by CITY, b_ed, and rent_group and calculate population
  result <- x[, .(population = .N), by = .(CITY, b_ed, rent_group)]
  
  return(result)
}

# 2. Combine results automatically using a "callback object"
# take results from each chunk and stack them on each other
cb_rent_1930 <- IpumsDataFrameCallback$new(cb_function_rent_1930)

# Read data in chunks
chunked_results_rent_1930 <-
  read_ipums_micro_chunked(ddi_file_path_1930, callback = cb_rent_1930, chunk_size = chunk_size_param)

# Combine results
rent_group_by_city_ed_1930 <-
  chunked_results_rent_1930 %>%
  # drop missing CITY and b_ed
  filter(CITY != 0, !is.na(b_ed)) %>% 
  mutate(CITY_NAME = as.character(as_factor(CITY))) %>% 
  group_by(CITY_NAME, b_ed, rent_group) %>%
  summarize(population= sum(population), .groups = "drop") %>%
  mutate(rent_group = paste0("rent_group_", rent_group)) %>% 
  pivot_wider(names_from = rent_group, 
              values_from = population,
              values_fill =  0)
  


### Home values (VALUEH) ----
# VALUEH is already a "group" variable

cb_function_home_value_1930 <- function(x, pos) {
  # set as data.table
  setDT(x)
  
  # keep only if RELATE == 1 (head of household)
  x <- x[RELATE == 1]
  
  # merge Geographic Reference File info (ED number) onto full count
  x <- merge(x, grf_1930, by = "HISTID", all.x = TRUE)
  
  x <- as_tibble(x)
  
  # create home value groups: Based on 1940 tract home value groups
  x <- x %>%
    dplyr::rename(valueh = VALUEH) %>%
    # Keep only the head of household
    filter(RELATE == 1) %>%
    # Drop valueh = 0 (NA), valueh == 9999998 (missing), and valueh == 9999999 (N/A)
    filter(valueh != 0, valueh != 9999998, valueh != 9999999) %>%
    mutate(valueh_group = case_when(
      valueh > 0 & valueh <= 500 ~ 1,          # 0 - 500
      valueh > 500 & valueh <= 699 ~ 2,        # 500 - 699
      valueh > 699 & valueh <= 999 ~ 3,        # 700 - 999
      valueh > 999 & valueh <= 1499 ~ 4,       # 1000 - 1499
      valueh > 1499 & valueh <= 1999 ~ 5,      # 1500 - 1999
      valueh > 1999 & valueh <= 2499 ~ 6,      # 2000 - 2499
      valueh > 2499 & valueh <= 2999 ~ 7,      # 2500 - 2999
      valueh > 2999 & valueh <= 3999 ~ 8,      # 3000 - 3999
      valueh > 3999 & valueh <= 4999 ~ 9,      # 4000 - 4999
      valueh > 4999 & valueh <= 5999 ~ 10,     # 5000 - 5999
      valueh > 5999 & valueh <= 7499 ~ 11,     # 6000 - 7499
      valueh > 7499 & valueh <= 9999 ~ 12,     # 7500 - 9999
      valueh > 9999 & valueh <= 14999 ~ 13,    # 10000 - 14999
      valueh > 14999 & valueh <= 19999 ~ 14,   # 15000 - 19999
      valueh > 19999 ~ 15,                     # 20000+
      TRUE ~ NA_integer_
    ))
  
  # Convert back to data.table
  setDT(x)
  
  # Group by CITY, b_ed, and valueh_group and calculate population
  result <- x[, .(population = .N), by = .(CITY, b_ed, valueh_group)]
  
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
  group_by(CITY_NAME, b_ed, valueh_group) %>%
  summarize(population= sum(population), .groups = "drop") %>% 
  mutate(valueh_group = paste0("home_value_", valueh_group)) %>% 
  pivot_wider(names_from = valueh_group, 
              values_from = population,
              values_fill = 0)
  

## Labor force and unemployment, age 14 and up -----
cb_function_lf_1930 <- function(x, pos) {
  
  # set as data.table
  setDT(x)
  
  # keep only individuals age 14 and up, following Census
  x <- as.tibble(x)
  
  x <- x %>%
    filter(AGE >= 14)
  
  x <- as.data.table(x)

  # merge Geographic Reference File info (ED number) onto full count
  x <- merge(x, grf_1930, by = "HISTID", all.x = TRUE)
  
  # collapse by City, ED #, Race
  result <- x[, .(population = .N) , by = .(CITY, b_ed, EMPSTAT)]
  
  return(result)
  
}

# 2. Combine results automatically using a "callback object"
# take results from each chunk and stack them on each other
cb_lf_1930 <- IpumsDataFrameCallback$new(cb_function_lf_1930)

# Read data in chunks
chunked_results_lf_1930 <-
  read_ipums_micro_chunked(ddi_file_path_1930, callback = cb_lf_1930, chunk_size = 500000)

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
  group_by(CITY_NAME, b_ed, employment_status) %>%
  summarize(population= sum(population), .groups = "drop") %>% 
  pivot_wider(names_from = employment_status, 
              values_from = population,
              values_fill =  0) %>%
  select(CITY_NAME, b_ed, employed, unemployed, not_in_lf) 


## Combine and output -----

# combine datasets
city_ed_data_1930 <- 
  left_join(population_by_race_city_ed_1930, rent_group_by_city_ed_1930) %>% 
  left_join(home_value_by_city_ed_1930) %>%
  left_join(lf_status_by_city_ed_1930)
  
# output csv
write_csv(city_ed_data_1930, here(output_dir, "city_ed_data_1930.csv"))

# drop anything with 1930 in it 
rm(list = ls(pattern = "1930"))


# 1940 -----
## 1940
grf_1940 <- read_csv(here(grf_dir, "grf_1940_histid_ed_city.csv"))

grf_1940 <- grf_1940 %>% 
  select(histid, b_ed) %>% dplyr::rename(HISTID = histid)

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
  
  # keep only if RELATE == 1 (head of household)
  x <- x[RELATE == 1]
  
  # merge Geographic Reference File info (ED number) onto full count
  x <- merge(x, grf_1940, by = "HISTID", all.x = TRUE)
  
  # collapse by City,
  result <- x[, .(population = .N) , by = .(CITY, b_ed, RACE)]
  
  return(result)

}

# 2. Combine results automatically using a "callback object"
# take results from each chunk and stack them on each other
cb_pop_1940 <- IpumsDataFrameCallback$new(cb_function_pop_1940)

# Read data in chunks
chunked_results_pop_1940 <-
  read_ipums_micro_chunked(ddi_file_path_1940, callback = cb_pop_1940, chunk_size = chunk_size_param)

# Combine results
pop_by_race_by_city_ed_1940 <-
  chunked_results_pop_1940 %>%
  # drop missing CITY and b_ed
  filter(CITY != 0, !is.na(b_ed)) %>% 
  mutate(CITY_NAME = as.character(as_factor(CITY)),
         race = as.character(as_factor(RACE))) %>% 
  group_by(CITY_NAME, b_ed, race) %>%
  summarize(population= sum(population), .groups = "drop") %>% 
  pivot_wider(names_from = race, 
              values_from = population,
              values_fill =  0) %>% 
  mutate(white_pop = White,
         black_pop = `Black/African American`,
         other_pop = Chinese + `American Indian or Alaska Native` + Japanese +`Other Asian or Pacific Islander`) %>% 
  select(CITY_NAME, b_ed, white_pop, black_pop, other_pop) 

## Education -----
# Following 1940 tract data: Persons 25 years and older by years of school completed

cb_function_educ_1940 <- function(x, pos) {
  setDT(x)  # Ensure x is a data.table
  
  # Merge with grf_1930
  x <- merge(x, grf_1940, by = "HISTID", all.x = TRUE)
  
  x <- as_tibble(x)
  
  # Create educ attainment
  x <- x %>%
    # Keep only persons 25 years and older
    filter(AGE >= 25) %>% 
    # drop NA, missing, or no schooling
    filter(EDUC != 0, EDUC != 99)
  
  # Convert back to data.table
  setDT(x)
  
  # Group by CITY, b_ed, and rent_group and calculate population
  result <- x[, .(population = .N), by = .(CITY, b_ed, EDUC)]
  
  return(result)
}

cb_education_1940 <- IpumsDataFrameCallback$new(cb_function_educ_1940)

chunked_results_educ_1940 <-
  read_ipums_micro_chunked(ddi_file_path_1940, callback = cb_education_1940, chunk_size = chunk_size_param)


pop_by_educ_by_city_ed_1940 <-
  chunked_results_educ_1940 %>%
  # drop missing CITY and b_ed
  filter(CITY != 0, !is.na(b_ed)) %>% 
  mutate(CITY_NAME = as.character(as_factor(CITY))) %>% 
  group_by(CITY_NAME, b_ed, EDUC) %>%
  summarize(population= sum(population), .groups = "drop") %>% 
  # create 
  # create education binary variables
  mutate(hs_grad = ifelse(EDUC >= 6, population, 0),
         some_college = ifelse(EDUC >= 7, population, 0)) %>% 
  group_by(CITY_NAME, b_ed) %>%
  # total pop (of the subgroup)
  summarize(total_educ_pop = sum(population),
            hs_grad_pop = sum(hs_grad),
            some_college_pop = sum(some_college),
            .groups = "drop") %>% 
  select(CITY_NAME, b_ed,
         total_educ_pop, hs_grad_pop, some_college_pop) 


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
  
  # x <- sample_1940
  # Merge with grf_1930
  x <- merge(x, grf_1940, by = "HISTID", all.x = TRUE)
  
  x <- as_tibble(x)

  # Create family (wage) income
  x <- x %>%
    # Drop income == 0 or missing ()
    filter(INCWAGE != 999998, INCWAGE != 999999) %>% 
    group_by(CITY, b_ed, SERIAL) %>% 
    summarise(family_wage = sum(INCWAGE), .groups = "drop") %>%   # %>% 
    # create wage income group
    mutate(income_group = case_when(family_wage < 500 ~ 1, # 0-499
                                    family_wage >= 500 & family_wage < 1000 ~ 2, # 500-999
                                    family_wage >= 1000 & family_wage < 1500 ~ 3, # 1000-1499
                                    family_wage >= 1500 & family_wage < 2000 ~ 4, # 1500-1999
                                    family_wage >= 2000 & family_wage < 2500 ~ 5, # 2000-2499
                                    family_wage >= 2500 & family_wage < 3000 ~ 6, # 2500-2999
                                    family_wage >= 3000 & family_wage < 3500 ~ 7, # 3000-3499
                                    family_wage >= 3500 & family_wage < 4000 ~ 8, # 3500-3999
                                    family_wage >= 4000 & family_wage < 4500 ~ 9, # 4000-4999
                                    family_wage >= 4500 & family_wage < 5000 ~ 10, # 4500-4999
                                    family_wage >= 5000 & family_wage < 6000 ~ 11, # 5000-5999
                                    family_wage >= 6000 & family_wage < 7000 ~ 12, # 6000-6999
                                    family_wage >= 7000 & family_wage < 10000 ~ 13, # 7000-9999
                                    family_wage >= 10000 ~ 14,
                                    TRUE ~ NA_integer_))


  
  # Convert back to data.table
  setDT(x)
  
  # Group by CITY, b_ed, and rent_group and calculate population
  #result <- x[, .(population = .N), by = .(CITY, b_ed, SERIAL)]
  # result <- x[, .(population = .N), by = .(CITY, b_ed, income_group)]
  result <- x
  
  return(result)
  
}

# Combine results automatically using a "callback object"
cb_income_1940 <- IpumsDataFrameCallback$new(cb_function_income_1940)

# Read data in chunks
chunked_results_income_1940 <- 
  read_ipums_micro_chunked(ddi_file_path_1940, callback = cb_income_1940, chunk_size = chunk_size_param)

income_group_by_city_ed_1940 <-
  chunked_results_income_1940 %>% 
  filter(CITY != 0, !is.na(b_ed)) %>% 
  group_by(CITY, b_ed, SERIAL) %>%
  summarise(family_wage = sum(family_wage), .groups = "drop") %>%
  # create wage income group
  mutate(income_group = case_when(family_wage < 500 ~ 1, # 0-499
                                  family_wage >= 500 & family_wage < 1000 ~ 2, # 500-999
                                  family_wage >= 1000 & family_wage < 1500 ~ 3, # 1000-1499
                                  family_wage >= 1500 & family_wage < 2000 ~ 4, # 1500-1999
                                  family_wage >= 2000 & family_wage < 2500 ~ 5, # 2000-2499
                                  family_wage >= 2500 & family_wage < 3000 ~ 6, # 2500-2999
                                  family_wage >= 3000 & family_wage < 3500 ~ 7, # 3000-3499
                                  family_wage >= 3500 & family_wage < 4000 ~ 8, # 3500-3999
                                  family_wage >= 4000 & family_wage < 4500 ~ 9, # 4000-4999
                                  family_wage >= 4500 & family_wage < 5000 ~ 10, # 4500-4999
                                  family_wage >= 5000 & family_wage < 6000 ~ 11, # 5000-5999
                                  family_wage >= 6000 & family_wage < 7000 ~ 12, # 6000-6999
                                  family_wage >= 7000 & family_wage < 10000 ~ 13, # 7000-9999
                                  family_wage >= 10000 ~ 14,
                                  TRUE ~ NA_integer_)) %>% 
  # get population per income group
  group_by(CITY, b_ed, income_group) %>%
  summarise(population = n(), .groups = "drop") %>% 
  mutate(CITY_NAME = as.character(as_factor(CITY))) %>% 
  group_by(CITY_NAME, b_ed, income_group) %>%
  summarize(population= sum(population), .groups = "drop") %>%
  mutate(income_group = paste0("income_group_", income_group)) %>% 
  pivot_wider(names_from = income_group, 
              values_from = population,
              values_fill =  0)




## Housing ----
### Rent (RENT) ----
cb_function_rent_1940 <- function(x, pos) {
  setDT(x)  # Ensure x is a data.table
  
  # Merge with grf_1930
  x <- merge(x, grf_1940, by = "HISTID", all.x = TRUE)
  
  x <- as_tibble(x)
  
  # Create rent_group
  x <- x %>%
    dplyr::rename(rent = RENT) %>% 
    # Keep only the head of household 
    filter(RELATE == 1) %>% 
    # drop rent = 0 (NA) and rent == 9999 (missing) and rent == 9998 (no cash rent)
    filter(rent != 0, rent != 9999, rent != 9998) %>% 
    mutate(rent_group = case_when(
      rent >= 0 & rent <= 5 ~ 1,
      rent > 5 & rent <= 6 ~ 2,
      rent > 6 & rent <= 9 ~ 3,
      rent > 9 & rent <= 14 ~ 4,
      rent > 14 & rent <= 19 ~ 5,
      rent > 19 & rent <= 24 ~ 6,
      rent > 24 & rent <= 29 ~ 7,
      rent > 29 & rent <= 39 ~ 8,
      rent > 39 & rent <= 49 ~ 9,
      rent > 49 & rent <= 59 ~ 10,
      rent > 59 & rent <= 74 ~ 11,
      rent > 74 & rent <= 99 ~ 12,
      rent > 99 & rent <= 149 ~ 13,
      rent > 149 & rent <= 199 ~ 14,
      rent > 199 ~ 15,
      TRUE ~ NA_integer_
    ))
  
  # Convert back to data.table
  setDT(x)
  
  # Group by CITY, b_ed, and rent_group and calculate population
  result <- x[, .(population = .N), by = .(CITY, b_ed, rent_group)]
  
  return(result)
}

# Combine results automatically using a "callback object"
cb_rent_1940 <- IpumsDataFrameCallback$new(cb_function_rent_1940)

# Read data in chunks
chunked_results_rent_1940 <-
  read_ipums_micro_chunked(ddi_file_path_1940, callback = cb_rent_1940, chunk_size = chunk_size_param)

rent_group_by_city_ed_1940 <-
  chunked_results_rent_1940 %>%
  # drop missing CITY and b_ed
  filter(CITY != 0, !is.na(b_ed)) %>% 
  mutate(CITY_NAME = as.character(as_factor(CITY))) %>% 
  group_by(CITY_NAME, b_ed, rent_group) %>%
  summarize(population= sum(population), .groups = "drop") %>%
  mutate(rent_group = paste0("rent_group_", rent_group)) %>% 
  pivot_wider(names_from = rent_group, 
              values_from = population,
              values_fill =  0)

### Value of home (VALUEH) ----
cb_function_valueh_1940 <- function(x, pos) {
  setDT(x)  # Ensure x is a data.table
  
  # Merge with grf_1930
  x <- merge(x, grf_1940, by = "HISTID", all.x = TRUE)
  
  x <- as_tibble(x)
  
  # Create valueh_group
  x <- x %>%
    dplyr::rename(valueh = VALUEH) %>%
    # Keep only the head of household
    filter(RELATE == 1) %>%
    # Drop valueh = 0 (NA), valueh == 9999998 (missing), and valueh == 9999999 (N/A)
    filter(valueh != 0, valueh != 9999998, valueh != 9999999) %>%
    mutate(valueh_group = case_when(
      valueh > 0 & valueh <= 500 ~ 1,          # 0 - 500
      valueh > 500 & valueh <= 699 ~ 2,        # 500 - 699
      valueh > 699 & valueh <= 999 ~ 3,        # 700 - 999
      valueh > 999 & valueh <= 1499 ~ 4,       # 1000 - 1499
      valueh > 1499 & valueh <= 1999 ~ 5,      # 1500 - 1999
      valueh > 1999 & valueh <= 2499 ~ 6,      # 2000 - 2499
      valueh > 2499 & valueh <= 2999 ~ 7,      # 2500 - 2999
      valueh > 2999 & valueh <= 3999 ~ 8,      # 3000 - 3999
      valueh > 3999 & valueh <= 4999 ~ 9,      # 4000 - 4999
      valueh > 4999 & valueh <= 5999 ~ 10,     # 5000 - 5999
      valueh > 5999 & valueh <= 7499 ~ 11,     # 6000 - 7499
      valueh > 7499 & valueh <= 9999 ~ 12,     # 7500 - 9999
      valueh > 9999 & valueh <= 14999 ~ 13,    # 10000 - 14999
      valueh > 14999 & valueh <= 19999 ~ 14,   # 15000 - 19999
      valueh > 19999 ~ 15,                     # 20000+
      TRUE ~ NA_integer_
    ))
  
  # Convert back to data.table
  setDT(x)
  
  # Group by CITY, b_ed, and valueh_group and calculate population
  result <- x[, .(population = .N), by = .(CITY, b_ed, valueh_group)]
  
  return(result)
}

# Combine results automatically using a "callback object"
cb_valueh_1940 <- IpumsDataFrameCallback$new(cb_function_valueh_1940)

# Read data in chunks
chunked_results_valueh_1940 <-
  read_ipums_micro_chunked(ddi_file_path_1940, callback = cb_valueh_1940, chunk_size = chunk_size_param)

valueh_group_by_city_ed_1940 <-
  chunked_results_valueh_1940 %>%
  # drop missing CITY and b_ed
  filter(CITY != 0, !is.na(b_ed)) %>% 
  mutate(CITY_NAME = as.character(as_factor(CITY))) %>% 
  group_by(CITY_NAME, b_ed, valueh_group) %>%
  summarize(population= sum(population), .groups = "drop") %>%
  mutate(valueh_group = paste0("valueh_group_", valueh_group)) %>% 
  pivot_wider(names_from = valueh_group, 
              values_from = population,
              values_fill =  0)

## Labor force and unemployment -----
cb_function_lf_1940 <- function(x, pos) {
  
  # set as data.table
  setDT(x)
  
  # keep only individuals age 14 and up, following Census
  x <- as.tibble(x)
  
  x <- x %>%
    filter(AGE >= 14)
  
  x <- as.data.table(x)
  
  # merge Geographic Reference File info (ED number) onto full count
  x <- merge(x, grf_1940, by = "HISTID", all.x = TRUE)
  
  # collapse by City, ED #, Race
  result <- x[, .(population = .N) , by = .(CITY, b_ed, EMPSTAT)]
  
  return(result)
  
}

# 2. Combine results automatically using a "callback object"
# take results from each chunk and stack them on each other
cb_lf_1940 <- IpumsDataFrameCallback$new(cb_function_lf_1940)

# Read data in chunks
chunked_results_lf_1940 <-
  read_ipums_micro_chunked(ddi_file_path_1940, callback = cb_lf_1940, chunk_size = chunk_size_param)

# Combine results
lf_status_by_city_ed_1940 <-
  chunked_results_lf_1940 %>%
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
  group_by(CITY_NAME, b_ed, employment_status) %>%
  summarize(population= sum(population), .groups = "drop") %>% 
  pivot_wider(names_from = employment_status, 
              values_from = population,
              values_fill =  0) %>%
  select(CITY_NAME, b_ed, employed, unemployed, not_in_lf) 

## Combine and output -----

city_ed_data_1940 <- 
  left_join(pop_by_race_by_city_ed_1940, rent_group_by_city_ed_1940) %>% 
  left_join(valueh_group_by_city_ed_1940) %>%
  left_join(lf_status_by_city_ed_1940) %>% 
  left_join(pop_by_educ_by_city_ed_1940) %>% 
  left_join(income_group_by_city_ed_1940)

# output csv
write_csv(city_ed_data_1940, here(output_dir, "city_ed_data_1940.csv"))

  

    
  
  