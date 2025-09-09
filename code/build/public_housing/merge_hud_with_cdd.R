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
library(here)

# directories 
raw_data_dir <- here("data", "raw")

ngda_data_dir <- here(raw_data_dir, "hud_public_housing_buildings_ngda")
psh_2000_data_dir <- here(raw_data_dir, "pic2000")
psh_1997_data_dir <- here(raw_data_dir, "pic97")
psh_1977_data_dir <- here("data", "derived", "public_housing", "intermediate", "hud")
hud951_dir <- here("data", "derived", "public_housing", "intermediate", "hud")
cdd_data_dir <- here(raw_data_dir, "hud_consolidated_directory")
output_data_dir <- here("data", "derived", "public_housing", "working")

geo_crosswalk_dir <-
  "/Users/beaubressler/Library/CloudStorage/Dropbox/Research/data_resources/crosswalks/geography/"


# modern HUD data
ngda_data_developments_raw <- read_csv(here(ngda_data_dir, "Public_Housing_Developments.csv"))
  
ngda_data_buildings_raw <- read_csv(here(ngda_data_dir, "Public_Housing_Buildings.csv"))

# PSH 2000
psh_2000_raw <- read_xlsx(here(psh_2000_data_dir, "all_projects_2000.xlsx"))

# PSH 1997 
psh_1997_raw <- read_xls(here(psh_1997_data_dir, "HUD3_cleaned.xls"))

# PSH 1977 (cleaned version of data from Yana Kucheva)
psh_1977 <- read_dta(here(psh_1977_data_dir, "pic77_data_for_analysis.dta"))

# HUD 951
hud951 <- read_csv(here(hud951_dir, "hud951_project_file_cleaned.csv"))

# CDD
cdd_data_raw <- read_dta(here(cdd_data_dir, "CDD_1973.dta"))

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
         project_code_full = if_else(is.na(site_letter), project_code, paste0(project_code, "-", site_letter)))


# 1997
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





### 0.2 Clean CDD data ---- 

# 1.merge state abbreviations onto CDD
cdd_data <-
  cdd_data_raw %>% 
  left_join(state_fips, by = c("state" = "state_name"))

# 2. get project code for CDD: State - projectnum1- projectnum2
cdd_data <-
  cdd_data %>% 
  mutate(project_code = paste0(state_usps, "-", projectnum1, "-", projectnum2),
         # if projectnum3 is not blank, add it to project code full
         project_code_full = if_else(projectnum3 == "", project_code,
                                     paste0(state_usps, "-", projectnum1, "-", projectnum2, "-", projectnum3)))
         
         #project_code_full = paste0(state_usps, "-", projectnum1, "-", projectnum2, "-", projectnum3))

# 3. Set totunits = totunitsplanned if totunits is 0 or totunits is NA and yearfullocc is not missing
cdd_data <-
  cdd_data %>% 
  mutate(totunits = if_else(!is.na(yearfullocc) & (is.na(totunits) | totunits == 0), totunitsplanned, totunits))

# 4. Get month of occupancy
cdd_data <-
  cdd_data %>% 
  mutate(
    # parse month abbreviation and 2-digit year
    date_fullocc = parse_date_time(fullocc, orders = "b-y"),
    
    # correct year if it's post-2000
    date_fullocc = if_else(year(date_fullocc) > 2024, 
                           date_fullocc %m-% years(100), 
                           date_fullocc),
    
    monthfullocc = month(date_fullocc)
  ) %>% 
  select(-date_fullocc)

### 0.3 Clean NGDA data ----
ngda_data_developments <- 
  ngda_data_developments_raw %>% 
  # set all column names to lower case
  rename_all(tolower) %>%
  # rename lat and lon
  rename(latitude = lat, longitude = lon) %>%
  # state_code is first 2 characters of development code
  mutate(state_code = str_sub(development_code, 1, 2),
         #city_code is next 3 digits of development code
         city_code = as.numeric(str_sub(development_code, 3, 5)),
         # site_code is remaining digits of development code
         site_code = as.numeric(str_sub(development_code, 6, -1)),
         # project code is state_code - city_code - site_code
         project_code = paste0(state_code, "-", city_code, "-", site_code)) %>% 
  mutate(name = str_to_title(project_name))

ngda_data_buildings <-
  ngda_data_buildings_raw %>%
  # set all column names to lower case
  rename_all(tolower) %>%
  # rename lat and lon
  rename(latitude = lat, longitude = lon) %>%
  # set all column names to lower case
  rename_all(tolower) %>%
  # state_code is first 2 characters of development code
  mutate(state_code = str_sub(development_code, 1, 2),
         #city_code is next 3 digits of development code
         city_code = as.numeric(str_sub(development_code, 3, 5)),
         # site_code is remaining digits of development code
         site_code = as.numeric(str_sub(development_code, 6, -1)),
         # project code is state_code - city_code - site_code
         project_code = paste0(state_code, "-", city_code, "-", site_code))

# New merge algorithm:
# 1. For each HUD dataset, keep the name, project_code, project_code_full, latitude, longitude and 
#.   Rename with _psh1997, _psh2000, and _ngda, respectively 
# 2. Join CDD to PSH1997 and PSH2000 on project code full
# 3. Then, join to all by project code


# 1. Prep datasets for merging -----

# PSH 1997
psh_1997_for_merge_full <-
  psh_1997 %>%
  # 1. keep only columns we need: name, project_code, project_code_full, latitude, longitude
  select(name, code, project_code, project_code_full, latitude, longitude) %>% 
  # 2. rename all variables with _psh1997
  rename_all(~paste0(., "_psh1997")) %>% 
  # 3. If there's an NA in the project code or project_code_full, drop it
  filter(!is.na(project_code_psh1997) & !is.na(project_code_full_psh1997)) %>% 
  # 4. If latitude and longitude are equal to -1 (this means missing), drop it
  filter(!(latitude_psh1997 == -1 & longitude_psh1997 == -1)) %>% 
  # If project_code_full is repeated, keep the first instance 
  distinct(project_code_full_psh1997, .keep_all = TRUE)

# for the merge on project code: keep first instance of 
psh_1997_for_merge_project_code <-
  psh_1997_for_merge_full %>% 
  distinct(project_code_psh1997, .keep_all = TRUE)

# PSH 2000
psh_2000_for_merge_full <-
  psh_2000 %>%
  # 1. keep only columns we need: name, project_code, project_code_full, latitude, longitude
  select(name, project_code, project_code_full, latitude, longitude) %>% 
  # 2. rename all variables with _psh2000
  rename_all(~paste0(., "_psh2000")) %>% 
  # 3. If there's an NA in the project code or project_code_full, drop it
  filter(!is.na(project_code_psh2000) & !is.na(project_code_full_psh2000)) %>% 
  # 4. If latitude and longitude are equal to -3 (this means missing), drop it
  filter(!(latitude_psh2000 == -3 & longitude_psh2000 == -3))

  # for merge on project code: keep first instance 
psh_2000_for_merge_project_code <-
  psh_2000_for_merge_full %>% 
  distinct(project_code_psh2000, .keep_all = TRUE)

# HUD NGDA
ngda_for_merge <-
  ngda_data_developments %>%
  # 1. keep only columns we need: name, project_code, latitude, longitude
  select(name, project_code, latitude, longitude) %>% 
  # 2. rename all variables with _ngda
  rename_all(~paste0(., "_ngda"))

# HUD 951 
hud951_for_merge <- 
  hud951 %>% 
  # 1. keep only columns we need: name, project_code, latitude, longitude
  select(project_name, project_code, proj_med_lat, proj_med_long, street_name, street_num,
         census_tract_1990) %>% 
  # drop if missing lat and long 
  filter(proj_med_lat != 0) %>% 
  # replace tract = 0 with NA
  mutate(census_tract_1990 = ifelse(census_tract_1990 == 0, NA, census_tract_1990)) %>%
  dplyr::rename(name = project_name, latitude = proj_med_lat, longitude = proj_med_long) %>%
  # 2. rename all variables with _hud951
  rename_all(~paste0(., "_hud951"))


# 2. Merge HUD datasets to CDD-----

# First, match to full project codes 
cdd_data_merged_to_psh_by_full_code <-
  cdd_data %>% 
  # 1. join to psh_1997_for_merge
  left_join(psh_1997_for_merge_full, by = c("project_code_full" = "project_code_full_psh1997")) %>%
  # 2. join to psh_2000_for_merge
  left_join(psh_2000_for_merge_full, by = c("project_code_full" = "project_code_full_psh2000")) 


# match to project codes (only projects unmatched to full project codes)
cdd_data_merged_to_psh_by_project_code <-
  cdd_data_merged_to_psh_by_full_code %>% 
  filter(is.na(latitude_psh1997) & is.na(latitude_psh2000)) %>% 
  # drop variables with suffixes "_psh1997" and "_psh2000"
  select(-ends_with("_psh1997"), -ends_with("_psh2000")) %>%
  # 1. join to psh_1997_for_merge
  left_join(psh_1997_for_merge_project_code, by = c("project_code" = "project_code_psh1997")) %>%
  # 2. join to psh_2000_for_merge
  left_join(psh_2000_for_merge_project_code, by = c("project_code" = "project_code_psh2000")) %>% 
  # 3. join to ngda
  left_join(ngda_for_merge, by = c("project_code" = "project_code_ngda")) %>% 
  # 4. join to hud951
  left_join(hud951_for_merge, by = c("project_code" = "project_code_hud951"))


# Combine matches by full code and merge (matched and unmatched)
merged_cdd_project_data <-
  bind_rows(cdd_data_merged_to_psh_by_full_code %>% filter(!(is.na(latitude_psh1997) & is.na(latitude_psh2000))),
          cdd_data_merged_to_psh_by_project_code)

# 3. Reconcile merges ------

# Prioritize PSH2000 over HUD951 over PSH1997 over NGDA for location (based on ground truth accuracy testing)
# Prioritize PSH2000 over HUD951 over PSH1997 over NGDA for name
merged_cdd_project_data <-
  merged_cdd_project_data %>% 
  mutate(name = coalesce(name_psh2000, name_hud951, name_psh1997, name_ngda),
         latitude = coalesce(latitude_psh2000, latitude_hud951, latitude_psh1997, latitude_ngda),
         longitude = coalesce(longitude_psh2000, longitude_hud951, longitude_psh1997, longitude_ngda))

# assign source of location (updated to match new priority order)
merged_cdd_project_data <-
  merged_cdd_project_data %>% 
  mutate(source = case_when(
    !is.na(latitude_psh2000) ~ "psh2000",
    !is.na(latitude_hud951) ~ "hud951",
    !is.na(latitude_psh1997) ~ "psh1997",
    !is.na(latitude_ngda) ~ "ngda",
    TRUE ~ "unknown"
  ))

# View if unmatched
# merged_cdd_project_data %>% 
#   filter(is.na(latitude)) %>% 
#   View()

# manual fixes to project locations-----
merged_cdd_project_data <-
  merged_cdd_project_data %>%
  # pruitt-igoe: 38.642122, -90.208910
  mutate(name = ifelse(project_code == "MO-1-4", "Pruitt-Igoe", name),
         source = ifelse(project_code == "MO-1-4" & is.na(latitude), "manual", source),
         latitude = ifelse(project_code == "MO-1-4" & is.na(latitude), 38.642122, latitude),
         longitude = ifelse(project_code == "MO-1-4" & is.na(longitude), -90.208910, longitude)) %>% 
  # MO-1-5 as well
  mutate(name = ifelse(project_code == "MO-1-5", "Pruitt-Igoe", name),
         source = ifelse(project_code == "MO-1-5" & is.na(latitude), "manual", source),
         latitude = ifelse(project_code == "MO-1-5" & is.na(latitude), 38.642122, latitude),
         longitude = ifelse(project_code == "MO-1-5" & is.na(longitude), -90.208910, longitude)) %>%
  # Brewster (MI-1-13): 701 units: 42.350130, -83.050177
  mutate(name = ifelse(project_code == "MI-1-13", "Brewster", name),
         source = ifelse(project_code == "MI-1-13" & is.na(latitude), "manual", source),
         latitude = ifelse(project_code == "MI-1-13", 42.350130, latitude),
         longitude = ifelse(project_code == "MI-1-13", -83.050177, longitude)
         ) %>% 
  # College Court (Louisville, KY-1-8):  38.240843, -85.764867
  mutate(name = ifelse(project_code == "KY-1-8", "College Court", name),
         source = ifelse(project_code == "KY-1-8" & is.na(latitude), "manual", source),
         latitude = ifelse(project_code == "KY-1-8", 38.240843, latitude),
         longitude = ifelse(project_code == "KY-1-8", -85.764867, longitude)
  ) %>% 
  # La Casita (Denver, CO-1-1): 39.735259116809615, -105.02261160007725
  mutate(name = ifelse(project_code == "CO-1-1", "La Casita", name),
         source = ifelse(project_code == "CO-1-1" & is.na(latitude), "manual", source),
         latitude = ifelse(project_code == "CO-1-1", 39.73525, latitude),
         longitude = ifelse(project_code == "CO-1-1", -105.02261, longitude)) %>% 
  # Tasker Homes Addition (PA-2-8 and PA-2-7): 39.93032934178652, -75.1950514740041
  # Tasker Homes fixes (PA-2-8 and PA-2-7)
  mutate(name = ifelse(project_code %in% c("PA-2-8", "PA-2-7"), "Tasker Homes", name),
       source = ifelse(project_code %in% c("PA-2-8", "PA-2-7") & is.na(latitude), "manual", source),
       latitude = ifelse(project_code == "PA-2-8", 39.93033,
                         ifelse(project_code == "PA-2-7", 39.93033, latitude)),
       longitude = ifelse(project_code == "PA-2-8", -75.19505,
                          ifelse(project_code == "PA-2-7", -75.19505, longitude)))
  

# What share of projects do I have latitude for: 80.8% as of 2024-11-23
nrow(merged_cdd_project_data %>% filter(!is.na(latitude) | !is.na(longitude))) /
  nrow(merged_cdd_project_data)

# what share of units do I have latitude for: 91%! as of 2024-11-23
merged_cdd_project_data %>% 
  filter(!is.na(latitude) | !is.na(longitude), !is.na(totunits)) %>% 
  summarise(total_units = sum(totunits)) %>% 
  pull(total_units)/
  sum(merged_cdd_project_data$totunits, na.rm = TRUE)

# What about among the top 50 localities? 91.9% as well... pretty good
merged_cdd_project_data %>% 
  group_by(locality, state) %>% 
  # total units and total units without missing latitude
  summarise(total_units = sum(totunitsplanned),
            total_units_no_missing_lat = sum(totunitsplanned[!is.na(latitude)])) %>%
  ungroup() %>% 
  # select top 50
  top_n(50, total_units)  %>% 
  # calculate share of units with latitude
  summarise(total_units = sum(total_units),
            total_units_no_missing_lat = sum(total_units_no_missing_lat)) %>% 
  mutate(share = total_units_no_missing_lat/total_units) %>% 
  pull(share)


# Collapse dataset by project code ----

# count number of distinct latitudes by project code
projects_with_multiple_latitudes <- merged_cdd_project_data %>% 
  group_by(project_code) %>% 
  summarise(n_distinct_latitudes = n_distinct(latitude, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(n_distinct_latitudes > 1)  # Keep only projects with more than one distinct latitude

# To view the detailed data for these projects
merged_cdd_project_data_with_multiple_latitudes <- merged_cdd_project_data %>%
  filter(project_code %in% projects_with_multiple_latitudes$project_code)

# count number of distinct yearfullocc by project code
projects_with_multiple_yearfullocc <- merged_cdd_project_data %>% 
  filter(!is.na(yearfullocc)) %>% 
  group_by(project_code) %>% 
  summarise(n_distinct_yearfullocc = n_distinct(yearfullocc, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(n_distinct_yearfullocc > 1)  # Keep only projects with more than one distinct yearfullocc

# To view the detailed data for these projects
merged_cdd_project_data_with_multiple_yearfullocc <- merged_cdd_project_data %>%
  filter(project_code %in% projects_with_multiple_yearfullocc$project_code) %>% 
  filter(!is.na(yearfullocc))

# collapse data by project code and treatment year
collapsed_data <- 
  merged_cdd_project_data %>% 
  filter(!is.na(yearfullocc), !is.na(latitude)) %>% 
  group_by(project_code) %>% 
  mutate(treatment_year = case_when(
      yearfullocc %in% 1931:1940 ~ 1940,
      yearfullocc %in% 1941:1950 ~ 1950,
      yearfullocc %in% 1951:1960 ~ 1960,
      yearfullocc %in% 1961:1970 ~ 1970,
      yearfullocc %in% 1971:1980 ~ 1980)) %>% 
  group_by(project_code, treatment_year) %>%
  summarise(name = first(name, na_rm = TRUE),
            latitude = first(latitude, na_rm = TRUE),
            longitude = first(longitude, na_rm = TRUE),
            source = first(source, na_rm = TRUE),
            locality = first(locality, na_rm = TRUE),
            state = first(state, na_rm = TRUE),
            totunits = sum(totunits, na.rm = TRUE),
            totunitsplanned = sum(totunitsplanned, na.rm = TRUE),
            totelderly = sum(totelderly, na.rm = TRUE),
            roomsperunit = mean(roomsperunit, na.rm = TRUE),
            # I am keeping the LAST yearfullocc and monthfullocc... why?
            yearfullocc = last(yearfullocc, na_rm = TRUE),
            monthfullocc = last(monthfullocc, na_rm = TRUE),
            statefips = first(statefips, na_rm = TRUE),
            countyfips = first(countyfips, na_rm = TRUE),
            state_usps = first(state_usps, na_rm = TRUE),
            slum = first(slum, na_rm = TRUE)) %>% 
  # Calculate the total number of units for each project_code
  group_by(project_code) %>%
  mutate(total_units_by_project = sum(totunits, na.rm = TRUE)) %>%
  # Calculate the share of units for each treatment year
  mutate(share_of_project_units_in_decade = totunits / total_units_by_project) %>%
  ungroup() %>% 
  select(-total_units_by_project)


# count number of distinct observations by project code
projects_with_multiple_observations <- collapsed_data %>% 
  group_by(project_code) %>% 
  summarise(n_distinct_obs = max(row_number())) %>% 
  ungroup() %>% 
  filter(n_distinct_obs > 1)  # Keep only projects with more than one distinct observation

# To view the detailed data for these projects
merged_cdd_project_data_with_multiple_observations <-
  collapsed_data %>%
  filter(!is.na(latitude)) %>% 
  filter(project_code %in% projects_with_multiple_observations$project_code)

# Merge on 1977 population data from 1977 PSH -----
## Going to merge by project code on the collapsed data
collapsed_data_with_populations <-
  collapsed_data %>% 
  left_join(psh_1977 %>% select(-project_code_full)) %>% 
  # allocate populations and households across projects based on unit shares
  mutate(proj_black_population_estimate = proj_black_population_estimate*share_of_project_units_in_decade,
         proj_white_population_estimate = proj_white_population_estimate*share_of_project_units_in_decade,
         proj_black_households = proj_black_households*share_of_project_units_in_decade,
         proj_white_households = proj_white_households*share_of_project_units_in_decade) %>%
  # calculate white and black share 
  mutate(proj_black_share = proj_black_households/proj_total_households,
         proj_white_share = proj_white_households/proj_total_households)
 

# Output dataset ----
write_csv(collapsed_data_with_populations, here(output_data_dir, "merged_cdd_projects.csv"))

# also output the non collapsed version, with missing latitudes, for analysis
write_csv(merged_cdd_project_data, here(output_data_dir, "merged_cdd_projects_non_collapsed.csv"))

