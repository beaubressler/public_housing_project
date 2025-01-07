####
# In this file, I clean the HUD data (modern and 2000 PSH) on location and characteristics of public housing
# and merge it with the CDD data 
# Steps I will follow: 
# 0. Clean the datasets 
# 1. Merge PSH data to NGDA datasets by city and name
# 1. From these, get project codes if they differ, as well as maybe 1990 census tract
# 2. If project code doesn't match, use project code from PSH
# 3. Then, merge this to CDD by project code
####

library(tidyverse)
library(readxl)
library(haven)
library(fuzzyjoin)
library(stringdist)

geo_crosswalk_dir <-
  "/Users/beaubressler/Library/CloudStorage/Dropbox/Research/data_resources/crosswalks/geography/"

# modern HUD data
ngda_data_developments_raw <- read_csv("data/raw/hud_public_housing_buildings_ngda/Public_Housing_Developments.csv")
ngda_data_buildings_raw <- read_csv("data/raw/hud_public_housing_buildings_ngda/Public_Housing_Buildings.csv")

# PSH 2000
psh_2000_raw <- read_xlsx("data/raw/pic2000/all_projects_2000.xlsx")

# PSH 1997
psh_1997_raw <- read_xls("data/raw/pic97/HUD3_cleaned.xls")

# CDD
cdd_data_raw <- read_dta("data/raw/hud_consolidated_directory/CDD_1973.dta")

state_fips <-
  read_csv(paste0(geo_crosswalk_dir, "state_fips.csv")) %>% 
  mutate(state_name = toupper(state_name))

####
# 0. Clean datasets -----
####

### 0.1 Clean PSH data ----
# 2000

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
         project_code_full = if_else(is.na(site_letter), project_code, paste0(project_code, site_letter)))
         

# 1997
psh_1997 <-
  psh_1997_raw %>% 
  # lower case column names
  rename_all(tolower) %>%
  # remove spaces from project code
  mutate(code = str_remove_all(code, " ")) %>% 
  # remove "_" from project code
  mutate(code = str_remove_all(code, "_")) %>%
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
         project_code_full = if_else(is.na(site_letter), project_code, paste0(project_code, site_letter)))

  



### 0.2 Clean CDD data ---- 

# 1.merge state abbreviations onto CDD
cdd_data <-
  cdd_data_raw %>% 
  left_join(state_fips, by = c("state" = "state_name"))

# 2. get project code for CDD: State - projectnum1- projectnum2
cdd_data <-
  cdd_data %>% 
  mutate(project_code = paste0(state_usps, "-", projectnum1, "-", projectnum2))

### 0.3 Clean NGDA data ----
ngda_data_developments <- 
  ngda_data_developments_raw %>% 
  # state_code is first 2 characters of development code
  mutate(state_code = str_sub(DEVELOPMENT_CODE, 1, 2),
         #city_code is next 3 digits of development code
         city_code = as.numeric(str_sub(DEVELOPMENT_CODE, 3, 5)),
         # site_code is remaining digits of development code
         site_code = as.numeric(str_sub(DEVELOPMENT_CODE, 6, -1)),
         # project code is state_code - city_code - site_code
         project_code = paste0(state_code, "-", city_code, "-", site_code)) %>% 
  mutate(name = str_to_lower(PROJECT_NAME))
  
ngda_data_buildings <-
  ngda_data_buildings_raw %>% 
  # state_code is first 2 characters of development code
  mutate(state_code = str_sub(DEVELOPMENT_CODE, 1, 2),
         #city_code is next 3 digits of development code
         city_code = as.numeric(str_sub(DEVELOPMENT_CODE, 3, 5)),
         # site_code is remaining digits of development code
         site_code = as.numeric(str_sub(DEVELOPMENT_CODE, 6, -1)),
         # project code is state_code - city_code - site_code
         project_code = paste0(state_code, "-", city_code, "-", site_code))



####
# 1. Merge PSH data to NGDA datasets by city, state and name -----
####
# fuzzy match psh 2000 to NGDA developments
# fuzzy match on name, require same state_code and city_code


psh_2000_for_merge_to_ngda <-
  psh_2000 %>%
  # keep only columns we need
  select(name, contains("code"), site_letter, placefips, msa, latitude, longitude) %>% 
  # rename variables with _psh except for name, state_code, city_code
  rename_at(vars(-name, -state_code, -city_code, -project_code), ~paste0(., "_psh")) %>%
  mutate(name = str_to_lower(name)) %>% 
  #filter missing name, state or city codes
  filter(!is.na(name), !is.na(state_code), !is.na(city_code),
         name != "unnamed", name != "rental", name != "scattered sites",
         name != "project unnnamed") %>% 
  # filter out projects containing "unnamed" or "scattered" or "unselected
  filter(!str_detect(name, "unnamed"), !str_detect(name, "scattered"),
         !str_detect(name, "unselected"))


ngda_data_developments_for_merge <-
  ngda_data_developments %>% 
  #filter missing name, state or city codes
  filter(!is.na(name), !is.na(state_code), !is.na(city_code))

# fuzzy match on name, require same state_code and city_code
psh_2000_developments_fuzzy <-
  ngda_data_developments_for_merge %>%
  stringdist_inner_join(psh_2000_for_merge_to_ngda, by = c("name" = "name",
                                                   "state_code", "city_code"),
                        max_dist = 1,
                        distance_col = "match_distance")

psh_2000_developments_fuzzy_clean <-
  psh_2000_developments_fuzzy %>% 
  filter(state_code.x == state_code.y, city_code.x == city_code.y) %>% 
  # rename all columns with .x and .y as "_ngda" and "_psh"
  rename_all(funs(str_replace(., ".x", "_ngda"))) %>% 
  rename_all(funs(str_replace(., ".y", "_psh"))) %>% 
  select(contains("_psh"), project_code_ngda, name_ngda)

## 1.5 Cleaning fuzzy matches ----
# First, if the NGDA project code appears more than once, drop if the name_psh and name_ngda are different
psh_2000_developments_fuzzy_clean <-
  psh_2000_developments_fuzzy_clean %>%
  group_by(project_code_ngda) %>%
  mutate(n = n()) %>% 
  mutate(n = max(n)) %>% 
  filter(n == 1 | (n > 1 & name_psh == name_ngda))
  
  


# psh_2000_developments_fuzzy_clean where project_code_ngda appears more than once. 
# These are the ones that matched to multiple observations in PSH
psh_2000_developments_fuzzy_clean %>% 
  group_by(project_code_ngda) %>% 
  mutate(n = n()) %>% 
  filter(max(n) > 1) %>% 
  arrange(desc(n)) %>% View()

# Merge these back onto NGDA data by project code (ngda)
ngda_data_developments_merged <- 
  left_join(ngda_data_developments, psh_2000_developments_fuzzy_clean,
            by = c("name" = "name_ngda", 
                   "project_code" = "project_code_ngda")) %>% 
  # replace project code with project code from psh if it's not missing
  mutate(project_code = if_else(!is.na(project_code_psh), project_code_psh,
                                project_code))

# view psh_2000_developments_fuzzy_clean with more than one observation per name and project_code
psh_2000_developments_fuzzy_clean %>% 
  group_by(project_code_ngda, name_ngda) %>% 
  mutate(n = n()) %>% 
  filter(max(n) > 1) %>% 
  arrange(desc(n)) %>% View()

ngda_data_buildings_merged <-
  left_join(ngda_data_buildings, psh_2000_developments_fuzzy_clean,
            by = c("project_code" = "project_code_ngda")) %>% 
  # replace project code with project code from psh if it's not missing
  mutate(project_code = if_else(!is.na(project_code_psh), project_code_psh,
                                project_code))

# TODO: These joins result in the datasets getting slightly bigger... does
# this matter? I think it might for the projects dataset
# TODO: Keep for now but I'll have to figure this out. There are multiple 

### 
# 2. Get PSH projects that are not in the NGDA dataset -----
###

# get PSH project codes that are not in the fuzzy matched dataset 
nonmatched_psh_project_codes <- 
  psh_2000$project_code %>% 
  unique() %>% 
  setdiff(ngda_data_developments_merged$project_code) %>% 
  setdiff(ngda_data_developments_merged$project_code_psh)

psh_2000_for_merge_to_cdd <- 
  psh_2000 %>%
  # keep only project codes in nonmatched_psh_project_codes
  filter(project_code %in% nonmatched_psh_project_codes) %>%
  # keep only variables we need
  select(name, contains("code"), site_letter, placefips, msa, latitude, longitude) %>% 
  # rename every variable with _psh suffix
  rename_all(~paste0(., "_psh")) %>%
  mutate(project_code = project_code_psh)


# 3. Merge NGDA and PSH data to CDD by project code  ------
### Select and rename relevant variables  ----
# Variables I want from NGDA developments
# Project name, FORMAL_PARTICIPANT_NAME, DEVELOPMENT_CODE, PARTICIPANT_CODE, STD_CITY, CITY_CODE, project_code
# LAT, LONG, TRACT_LEVEL 
ngda_data_developments_sample <-
  ngda_data_developments_merged %>% 
  select(PROJECT_NAME, FORMAL_PARTICIPANT_NAME, DEVELOPMENT_CODE,
         PARTICIPANT_CODE, STD_CITY, state_code, LAT, LON, TRACT_LEVEL, X, Y,
         project_code, contains("psh")) %>% 
  # rename LAT and LON with _ngda suffix
  rename(latitude_ngda = LAT,
         longitude_ngda = LON)

# Variables I want from NGDA buildings
# Address, DEVELOPMENT_CODE, FORMAL_PARTICIPANT_NAME, PARTICIPANT_CODE, STD_CITY, STD_ST, STD_ZIP5, STD_ADDR, CITY_CODE, project_code
ngda_data_buildings_sample <-
  ngda_data_buildings_merged %>% 
  select(DEVELOPMENT_CODE, FORMAL_PARTICIPANT_NAME, PARTICIPANT_CODE,
         state_code, STD_CITY, STD_ST, STD_ZIP5, STD_ADDR, city_code,
         project_code, TRACT_LEVEL, CONSTRUCT_DATE, DOFA_ACTUAL_DT, LAT, LON,
         contains("psh")) %>% 
  # rename construct_date and DOFA_ACTUAL_DT with _ngda suffix
  rename(construct_date_ngda = CONSTRUCT_DATE,
         dofa_actual_dt_ngda = DOFA_ACTUAL_DT) %>% 
  # convert to date: y/m/d, first 10 characters
  mutate(construct_date_ngda = as.Date(str_sub(construct_date_ngda, 1, 10), format = "%Y/%m/%d"),
         dofa_actual_dt_ngda = as.Date(str_sub(dofa_actual_dt_ngda, 1, 10), format = "%Y/%m/%d")) %>% 
  mutate(construct_year_ngda = year(construct_date_ngda),
         occ_year_ngda = year(dofa_actual_dt_ngda)) %>% 
  # rename LAT and LON with _ngda suffix
  rename(latitude_ngda = LAT,
         longitude_ngda = LON) 
  


## Merges ----- 


#### 1. CDD projects: Should be a 1-to-1 merge ----
# Caveats: 
  # There are some projects that I have multiple codes for, so it's not exactly 1-1. See lab notebook
  # Not all NGDA data developments should be in here
  # Developments that have been demolished or converted to non-public housing will not be in NGDA

merged_cdd_projects_ngda <- 
  left_join(cdd_data, ngda_data_developments_sample, by = "project_code")

merged_cdd_projects <- 
  left_join(merged_cdd_projects_ngda, psh_2000_for_merge_to_cdd, by = "project_code") %>% 
  # generate variable equal to 1 if  is.na(name_psh.x)
  mutate(name_psh_na = if_else(is.na(name_psh.x), 1, 0)) 

merged_cdd_projects <-
  merged_cdd_projects %>%  
  # if .x variable is missing (variable from ngda_data_developments_sample),
  # replace with .y variable (variable from psh_2000_for_merge_to_cdd)
  mutate(across(ends_with(".x"), ~coalesce(., get(sub(".x$", ".y", cur_column(),
                                                      merged_cdd_projects))))) %>% 
  # drop variables ending in .y
  select(-ends_with(".y")) %>%
  # rename variables ending in .x
  rename_with(~sub(".x$", "", .x), ends_with(".x")) %>%
  # drop name_psh_na
  select(-name_psh_na) %>% 
  mutate(latitude = ifelse(is.na(latitude_psh), latitude_ngda, latitude_psh),
         longitude = ifelse(is.na(longitude_psh), longitude_ngda, longitude_psh)) %>% 
  # if latitude and longitude are -3 (missing code), replace with latitude_ngda and longitude_ngda
  mutate(latitude = ifelse(latitude == -3, latitude_ngda, latitude),
         longitude = ifelse(longitude == -3, longitude_ngda, longitude))

# What share of projects do I have latitude for: 75.7% as of 2023-12-11
nrow(merged_cdd_projects %>% filter(!is.na(latitude) | !is.na(longitude))) /
  nrow(merged_cdd_projects)


#### 2. CDD buildings: Should be a 1-to-many merge ----
# Will check that dates line up 

merged_cdd_buildings <- 
  left_join(cdd_data, ngda_data_buildings_sample, by = "project_code")


### Review merge
# What share of buildings do I have latitude for: 94%????! Seems really high
# But I suppose it makes sense... If I don't merge, I only have 1 row per projects, whereas
# if I do merge (and thus have latitude), I have all of the buildings.
# So 94% doesn't reflect much

nrow(merged_cdd_buildings %>% filter(!is.na(latitude_ngda) | !is.na(latitude_psh))) /
  nrow(merged_cdd_buildings)



### Review merge
View(merged_cdd_buildings %>% 
       filter(!is.na(latitude_ngda) | !is.na(latitude_psh), 
              (yearfullocc==occ_year_ngda)) %>%
       select(state, locality, project_code, contains("year"), contains("occ"),
              contains("date")))

# NOTE: A lot of these merges don't have "yearfullocc" == "occ_year_ngda"...
# Need to look into this more
View(merged_cdd_buildings %>% 
       filter(!is.na(latitude_ngda) | !is.na(latitude_psh),
              (yearfullocc!=occ_year_ngda)) %>%
       select(state, locality, project_code, contains("year"), contains("occ"),
              contains("date")))


### Output ----
write_csv(merged_cdd_projects, "data/derived/public_housing/working/merged_cdd_projects.csv")
write_csv(merged_cdd_buildings, "data/derived/public_housing/working/merged_cdd_buildings.csv")
