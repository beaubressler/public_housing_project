####
# Combine CDD-HUD data with digitized data to get the most complete dataset that I can 

# Plan for combining datasets:
# 1. Use my digitized data for NYC, SF, Chicago
# 2. For Boston, I think a combination of the stuff I have digitized + a few projects from the CDD data
#   I have the state data for Boston, so I can use that to fill in the gaps
# 3. Append 3 projects for DC
# 3. For the rest, for now I will just use the CDD data

####

# Preliminaries ----
# Load libraries
library(tidyverse)
library(foreign)
library(readxl)
library(here)

working_dir <-  here("data", "derived", "public_housing", "working")

# hud-CDD projects
cdd_projects <- read_csv(here(working_dir, "merged_cdd_projects.csv"))

# digitized projects
digitized_projects_raw <- read_csv(here(working_dir, "geocoded_projects.csv"))

# Cleveland, separately (just to merge additional names on, I suppose)
# cleveland_projects <- read_xlsx("data/digitization/cleveland/1973_cmha_annual_report.xlsx")

# Manual lat-long fixes
# These were checked by undergrad RAs (or Beau)
manual_fixes <- 
  read_xlsx(here(working_dir, "geocoded_projects_handchecked.xlsx")) %>% 
  #filter(city == "New York City" | city == "San Francisco") %>% 
  filter(checked == "YES") %>% 
  select(project_code, address_or_streets, correct_intersection) %>% 
  filter(correct_intersection != "N/A") %>% 
  # split correct intersection into lat and long
  separate(correct_intersection, into = c("lat_fixed", "long_fixed"), sep = ",\\s*")


# Apply manual fixes to digitized data ----
digitized_projects <-
  digitized_projects_raw %>% 
  left_join(manual_fixes) %>% 
  # apply fixes
  mutate(lat = ifelse(!is.na(lat_fixed), lat_fixed, lat),
         long = ifelse(!is.na(long_fixed), long_fixed, long)) %>% 
  select(-lat_fixed, -long_fixed) %>% 
  # convert lat and long to numeric
  mutate(lat = as.numeric(lat),
         long = as.numeric(long))


# Normalize project codes in digitized data -----
split_project_codes <- function(code) {
  parts <- str_split(code, "-")[[1]]
  prefix <- paste(parts[1:(length(parts)-1)], collapse = "-")
  suffixes <- str_split(parts[length(parts)], ",\\s*")[[1]]
  paste(prefix, suffixes, sep = "-")
}



# fix project codes 
digitized_projects <- 
  digitized_projects %>% 
  # capitalize all proejct codes
  mutate(project_code = toupper(project_code)) %>%
  # set federal_or_state to lower case
  mutate(federal_or_state = tolower(federal_or_state)) %>%
  # NYC: if contains Nys, set federal_or_state to federal
  mutate(federal_or_state = ifelse(str_detect(project_code, "NYS"), "federal", federal_or_state),
         project_code = str_replace(project_code, "NY ", "NY-")) %>%
  # SF: if contains CA-, set federal_or_state to federal
  mutate(federal_or_state = ifelse(city == "San Francisco" & str_detect(project_code, "CA-"), "federal", federal_or_state),
         project_code = ifelse(city == "San Francisco", str_replace(project_code, "CA-", "CA-1-"), project_code)) %>%
  # Baltimore: if contains "MD ", set federal_or_state to federal
  mutate(federal_or_state = ifelse(str_detect(project_code, "MD"), "federal", federal_or_state),
         project_code = str_replace(project_code, "MD 2", "MD-2")) %>% 
  # DC: If city == "Washington DC", append "DC-1-" to project code. Also, if federal_or_state is missing, assume federal
  mutate(project_code = ifelse(city == "Washington DC", paste0("DC-1-", project_code), project_code)) %>% 
  mutate(federal_or_state = ifelse(city == "Washington DC" & is.na(federal_or_state), "federal", federal_or_state)) %>%
  # Chicago: If city == "Chicago", append "IL-" to project code
  mutate(project_code = ifelse(city == "Chicago", paste0("IL-", project_code), project_code) ) %>%
  # LA: If city == "Los Angeles", set federal_or_state to federal
  mutate(federal_or_state = ifelse(city == "Los Angeles", "federal", federal_or_state),
         project_code = str_replace(project_code, "CA ", "CA-")) %>%
  # Boston: If city == "Boston" and project_code starts with "2-", append "MA-" to project code
  mutate(project_code = ifelse(city == "Boston" & str_detect(project_code, "^2-"), paste0("MA-", project_code), project_code)) %>% 
  # fix some boston codes with 0 in the front:
  mutate(project_code = case_when(project_code == "MA-2-01" ~ "MA-2-1",
                                  project_code == "MA-2-04" ~ "MA-2-4",
                                  project_code == "MA-2-06" ~ "MA-2-6",
                                  project_code == "MA-2-28" & project_name == "East Boston (Maverick)" ~ "MA-2-8",
                                  .default = project_code)) %>% 
  # replace (#) with -# in project codes
  mutate(project_code = str_replace_all(project_code, " \\(", "-"),
         project_code = str_replace_all(project_code, "\\(", "-"),
         project_code = str_replace_all(project_code, "\\)", "")) 
# %>% 
#   mutate(split_codes = map(project_code, split_project_codes)) %>%
#   unnest(split_codes) %>% 
#   dplyr::rename(project_code_original = project_code,
#                 project_code = split_codes)




# Merge digitized projects with CDD projects: See how much I merge for each city -----

digitized_projects_federal <-
  digitized_projects %>%
  filter(str_detect(federal_or_state, "federal")) %>% 
  filter(!is.na(project_code))

# merge with CDD projects
cdd_projects_sample <- 
  cdd_projects %>%
  filter(locality %in%
           c("NEW YORK CITY", "SAN FRANCISCO", "BALTIMORE CITY",
             "WASHINGTON DC-NATIONAL CAPITOL", "CHICAGO", "LOS ANGELES",
             "BOSTON"))

merged_federal <- 
  cdd_projects_sample %>%
  full_join(digitized_projects_federal, by = c("project_code" = "project_code")) %>%
  mutate(merged_digitized = ifelse(is.na(city), 0, 1),
         merged_cdd = ifelse(is.na(locality), 0, 1)) 


# View(merged_federal %>% filter(merged_cdd == 0) %>%
#        select(city, project_name, project_code, total_units))

#  Create final dataset -----

# CDD projects in cities for which I will use the digitized data
cdd_data_for_combined_data <-
  cdd_projects %>%
  filter(!(locality %in%
           c("NEW YORK CITY", "SAN FRANCISCO", "CHICAGO",
             "BOSTON"))) %>%
  # drop scattered sites
  filter(!str_detect(name, "Scattered"), !str_detect(name, "Scatter")) %>% 
  select(state, locality, project_code, name, totunitsplanned,
         yearfullocc, monthfullocc, latitude, longitude, statefips, countyfips, 
         state_usps, slum, starts_with("proj_")) %>% 
  # define source, also note all CDD projects are federal
  mutate(source = "CDD", federal_or_state = "federal") %>% 
  dplyr::rename(year_completed = yearfullocc,
                month_completed = monthfullocc,
                total_units = totunitsplanned,
                project_name = name) 

# digitized data
digitized_data_for_combined_data <-
  digitized_projects %>%
  filter((city %in%
           c("New York City", "San Francisco", "Washington DC", "Chicago"))) %>%
  # For DC: Keep only city projects missing from CDD 
  filter(city != "Washington DC" |
           (city == "Washington DC" & project_code %in% c("DC-1-W", "DC-1-V", "DC-1-228"))) %>%
  filter(!str_detect(tolower(project_name), "scatter")) %>% 
  filter(!is.na(year_completed), year_completed <= 1973) %>%
  mutate(source = "Digitized") %>% 
  dplyr::rename(latitude = lat, longitude = long) %>% 
  # define locality
  mutate(locality = case_when(
    city == "New York City" ~ "NEW YORK CITY",
    city == "San Francisco" ~ "SAN FRANCISCO",
    city == "Baltimore" ~ "BALTIMORE CITY",
    city == "Washington DC" ~ "WASHINGTON DC-NATIONAL CAPITOL",
    city == "Chicago" ~ "CHICAGO",
    city == "Los Angeles" ~ "LOS ANGELES"
  )) %>%
  # define state
  mutate(state = case_when(
    city == "New York City" ~ "NEW YORK",
    city == "San Francisco" ~ "CALIFORNIA",
    city == "Baltimore" ~ "MARYLAND",
    city == "Washington DC" ~ "DISTRICT OF COLUMBIA",
    city == "Chicago" ~ "ILLINOIS",
    city == "Los Angeles" ~ "CALIFORNIA"
  )) %>% 
  dplyr::rename(proj_total_population_estimate = total_population)
  

# Figure out Boston projects
cdd_projects_boston <-
  cdd_projects %>%
  filter(locality == "BOSTON", state == "MASSACHUSETTS") %>%
  filter(!str_detect(name, "Scattered"), !str_detect(name, "Scatter")) %>% 
  filter(!is.na(yearfullocc)) 


digitized_projects_boston <- digitized_projects %>%
  filter(city == "Boston") %>%
  filter(!str_detect(tolower(project_name), "scattered")) %>% 
  filter(year_completed <= 1973) 


# merge boston datasets together
merged_boston_data <- 
  cdd_projects_boston %>%
  full_join(digitized_projects_boston, by = c("project_code" = "project_code")) %>%
  mutate(merged_digitized = ifelse(is.na(project_name), 0, 1),
         merged_cdd = ifelse(is.na(name), 0, 1)) %>% 
  mutate(source = case_when(merged_cdd == 1 & merged_digitized == 1 ~ "CDD + Digitized",
                            merged_cdd == 1 & merged_digitized == 0 ~ "CDD",
                            merged_cdd == 0 & merged_digitized == 1 ~ "Digitized")) %>% 
  # coalesce: Use digitized data if available, except for lat/long
  mutate(project_name = coalesce(project_name, name),
         total_units = coalesce(total_units, totunitsplanned),
         year_completed = coalesce(year_completed, yearfullocc),
         latitude = coalesce(latitude, lat),
         longitude = coalesce(longitude, long)) %>%
  select(state, locality, project_code, project_name, total_units,
         year_completed, latitude, longitude, statefips, countyfips, 
         state_usps, slum, latitude, longitude, source) %>% 
  mutate(city = "Boston", locality = "BOSTON", statefips = 25, countyfips = 25025, 
         statefip = 25, state_usps = "MA") %>% 
  # drop observations that are duplicates on project_code and units
  distinct(project_code, total_units, .keep_all = TRUE)
  
# Combine all data
combined_data <-
  bind_rows(cdd_data_for_combined_data, digitized_data_for_combined_data) %>% 
  bind_rows(merged_boston_data) %>% 
  select(state, locality, project_code, project_name, total_units, 
         year_completed, month_completed, latitude, longitude, statefips, countyfips, 
         statefip, state_usps, slum, source, latitude, longitude,
         contains("proj_")) 

## output ----
write_csv(combined_data, here(working_dir, "combined_cdd_and_digitized_projects.csv"))



# xx
# #  Checks ------
# ## Summary ----
# # total number of units 
# combined_data %>%
#   summarise(total_units = sum(total_units, na.rm = TRUE),
#             total_projects = n_distinct(project_code, na.rm = TRUE))
# 
# 
# 
# ## Troubleshooting ----
# # Compare my digitized data (number of units and projects) to the CDD data for the cities I am using
# # particularly interested in chicago, LA, and SF
# 
# # CHICAGO: More in CDD than digitized...
# # that's kind of strange because I have the full 1973 chicago data...
# # I will still use digitized data
# 
# #
# chicago_units_and_projects_digitized <- 
#   digitized_projects %>%
#   filter(city %in% c("Chicago"), !is.na(lat)) %>% 
#   select(project_code, total_units, year_completed) %>% 
#   summarise(total_units = sum(total_units, na.rm = TRUE),
#             total_projects = n_distinct(project_code, na.rm = TRUE))
# 
# chicago_units_and_projects_cdd <-
#   cdd_projects %>%
#   filter(locality == "CHICAGO", !is.na(latitude), !is.na(yearfullocc)) %>% 
#   select(name, totunitsplanned, yearfullocc) %>% 
#   summarise(total_units = sum(totunitsplanned, na.rm = TRUE),
#             total_projects = n_distinct(name, na.rm = TRUE))
# 
# 
# 
# 
# # LA: again, more projects and units in CDD than digitized... 
# # This might be because my source was wikipedia? So I am missing some 
# # will use CDD
# la_units_and_projects_digitized <- 
#   digitized_projects %>%
#   filter(city %in% c("Los Angeles"), !is.na(lat)) %>% 
#   select(project_code, total_units, year_completed) %>% 
#   summarise(total_units = sum(total_units, na.rm = TRUE),
#             total_projects = n_distinct(project_code, na.rm = TRUE))
# 
# la_units_and_projects_cdd <-
#   cdd_projects %>%
#   filter(locality == "LOS ANGELES", !is.na(latitude), !is.na(yearfullocc)) %>% 
#   select(name, totunitsplanned, yearfullocc) %>% 
#   summarise(total_units = sum(totunitsplanned, na.rm = TRUE),
#             total_projects = n_distinct(name, na.rm = TRUE))
# 
# 
# # SF: Actually more units and projects in the digitized data, so I will use that 
# sf_units_and_projects_digitized <- 
#   digitized_projects %>%
#   filter(city %in% c("San Francisco"), !is.na(lat), year_completed <= 1973) %>% 
#   select(project_code, total_units, year_completed) %>% 
#   summarise(total_units = sum(total_units, na.rm = TRUE),
#             total_projects = n_distinct(project_code, na.rm = TRUE))
# 
# 
# sf_units_and_projects_cdd <-
#   cdd_projects %>%
#   filter(locality == "SAN FRANCISCO", !is.na(latitude), !is.na(yearfullocc)) %>% 
#   select(name, totunitsplanned, yearfullocc) %>% 
#   summarise(total_units = sum(totunitsplanned, na.rm = TRUE),
#             total_projects = n_distinct(name, na.rm = TRUE))
# 
# View(cdd_projects %>% 
#        filter(locality == "SAN FRANCISCO", !is.na(latitude), !is.na(yearfullocc)) %>%
#        select(name, project_code_full, totunitsplanned, yearfullocc))
#        
# 
# # SF digitized projects who don't have project codes in CDD
# digitized_projects %>%
#   filter(city %in% c("San Francisco"), !is.na(lat)) %>%
#   filter(!project_code %in% c(cdd_projects$project_code)) %>%
#   select(project_name, project_code, city, year_completed, total_units) %>%
#   arrange(year_completed) %>% 
#   View()
#   
# # SF CDD projects who don't have project codes in digitized data
# cdd_projects %>%
#   filter(locality == "SAN FRANCISCO", !is.na(latitude), !is.na(yearfullocc)) %>%
#   filter(!name %in% c(digitized_projects$project_name)) %>%
#   select(name, project_code, totunitsplanned, yearfullocc) %>%
#   arrange(yearfullocc) %>% View()
# 
# 
# # NYC: 130K units vs 81K units in CDD, big upgrade
# nyc_units_and_projects_digitized <- 
#   digitized_projects %>%
#   filter(city %in% c("New York City"), !is.na(lat), year_completed <= 1973) %>% 
#   select(project_code, total_units, year_completed) %>% 
#   summarise(total_units = sum(total_units, na.rm = TRUE),
#             total_projects = n_distinct(project_code, na.rm = TRUE))
# 
# nyc_units_and_projects_cdd <-
#   cdd_projects %>%
#   filter(locality == "NEW YORK CITY", !is.na(latitude), !is.na(yearfullocc), yearfullocc <= 1973) %>% 
#   select(name, totunitsplanned, yearfullocc) %>% 
#   summarise(total_units = sum(totunitsplanned, na.rm = TRUE),
#             total_projects = n_distinct(name, na.rm = TRUE))
# 
# # Baltimore: Actually more in CDD
# # There are some discrepencies between number of units in projects I digitized and CDD data
# # Could be because some of my sources for Baltimore are older, so maybe number of units changed since, say, 1951
# # Will use CDD
# 
# baltimore_units_and_projects_digitized <- 
#   digitized_projects %>%
#   filter(city %in% c("Baltimore"), !is.na(lat), year_completed <= 1973) %>% 
#   select(project_code, total_units, year_completed) %>% 
#   summarise(total_units = sum(total_units, na.rm = TRUE),
#             total_projects = n_distinct(project_code, na.rm = TRUE))
# 
# baltimore_units_and_projects_cdd <-
#   cdd_projects %>%
#   filter(locality == "BALTIMORE CITY", !is.na(latitude), !is.na(yearfullocc)) %>% 
#   select(name, totunitsplanned, yearfullocc) %>% 
#   summarise(total_units = sum(totunitsplanned, na.rm = TRUE),
#             total_projects = n_distinct(name, na.rm = TRUE))
# 
# 
# # DC: More units but fewer projects in CDD...
# # why?
# # There's a project in 1972 with 334 units, about half the difference
# # 
# dc_units_and_projects_digitized <- 
#   digitized_projects %>%
#   filter(city %in% c("Washington DC"), !is.na(lat)) %>% 
#   select(project_code, total_units, year_completed) %>% 
#   summarise(total_units = sum(total_units, na.rm = TRUE),
#             total_projects = n_distinct(project_code, na.rm = TRUE))
# 
# dc_units_and_projects_cdd <-
#   cdd_projects %>%
#   filter(locality == "WASHINGTON DC-NATIONAL CAPITOL", !is.na(latitude), !is.na(yearfullocc)) %>% 
#   select(name, totunitsplanned, yearfullocc) %>% 
#   summarise(total_units = sum(totunitsplanned, na.rm = TRUE),
#             total_projects = n_distinct(name, na.rm = TRUE))
# 
# 
# View(cdd_projects %>%
#        filter(locality == "WASHINGTON DC-NATIONAL CAPITOL", !is.na(latitude),
#               !is.na(yearfullocc)) %>% 
#        select(name, project_code, totunitsplanned, yearfullocc))
# 
# View(digitized_projects %>%
#        filter(city %in% c("Washington DC"), !is.na(lat)) %>% 
#        select(project_name, project_code, total_units, year_completed))
# 
# # DC digitized projects who don't have project codes in CDD
# digitized_projects %>%
#   filter(city %in% c("Washington DC"), !is.na(lat)) %>%
#   filter(!project_code %in% c(cdd_projects$project_code)) %>%
#   select(project_name, project_code, city, year_completed, total_units) %>%
#   arrange(year_completed)
# 
# # can add DC projects that are missing to the CDD:
# # project _codes: DC-1-W, DC-1V, DC-1-228
# city_projects_in_DC <- 
#   digitized_projects %>%
#   filter(city %in% c("Washington DC"), !is.na(lat)) %>%
#   filter(project_code %in% c("DC-1-W", "DC-1-V", "DC-1-228"))
# 
# 
# # Atlanta: more units and projects in CDD, also I actually have project codes
# 
# atlanta_units_and_projects_digitized <- 
#   digitized_projects %>%
#   filter(city %in% c("Atlanta"), !is.na(lat)) %>% 
#   select(project_name, total_units, year_completed) %>% 
#   summarise(total_units = sum(total_units, na.rm = TRUE),
#             total_projects = n_distinct(project_name, na.rm = TRUE))
# 
# atlanta_units_and_projects_cdd <-
#   cdd_projects %>%
#   filter(locality == "ATLANTA", !is.na(latitude), !is.na(yearfullocc)) %>% 
#   select(name, totunitsplanned, yearfullocc) %>% 
#   summarise(total_units = sum(totunitsplanned, na.rm = TRUE),
#             total_projects = n_distinct(name, na.rm = TRUE))
# 
# 
