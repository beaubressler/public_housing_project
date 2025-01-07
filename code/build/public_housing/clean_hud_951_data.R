#### 
# Clean HUD 951 data
# Description of data:
# In 1988-89, project managers reported each address of each HUD-subsidized project on form HUD-951.
# The data were then used at HUD and the Census Bureau for various purposes.
# Because of increasing interest, HUD decided to have a contractor standardize all the address formats in 1995-96 
# and give them geo-codes consistent with the 1990 Census, and the 1995 Zip codes,
# Congressional Districts and OMB-defined Metropolitan Statistical Areas.

# What are the datasets?
# 1. PRJFILE: Project summary data. This will have 1 line per project
# 2. BGFILE: Block group file. This is a summary of the addresses in each project in each Census block group. So i
# 3. ADDFILE: Address file. This is a list of addresses for each project.
####

# Preliminaries -----
library(tidyverse)
library(readxl)
library(haven)
library(here)


# directories 
raw_data_dir <- here("data/raw")
derived_data_dir <- here("data/derived")

cdd_data_dir <- here(raw_data_dir, "hud_consolidated_directory")
hud951_raw_dir <- here(raw_data_dir, "hud951")


hud951_output_dir <- here(derived_data_dir, "public_housing" ,"intermediate", "hud")

geo_crosswalk_dir <-
  "/Users/beaubressler/Library/CloudStorage/Dropbox/Research/data_resources/crosswalks/geography/"

# Read in data -----

cdd_data_raw <- read_dta(here(cdd_data_dir, "CDD_1973.dta"))


# For PRJFILE and BGFILE: I copy pasted the text of the .ACS into Excel, saved as xls
prjfile <- read_xls(here(hud951_raw_dir, "PRJFILE.xls"))
bgfile <- read_xls(here(hud951_raw_dir, "BGFILE.xls"))

# Read ADDFILE directly from the TXT file
addfile_colnames <- c(
  "matching_group", "match_key", "street_name", "street_num", 
  "city_state", "zip_plus_4", "census_state_1990", 
  "census_county_1990", "mcd_ccd", "place_code", 
  "MSA", "census_tract_1990", "census_block_group_1990", 
  "site_id", "ad_code", "source", "add_lat", 
  "add_long", "units", "quality_lat_long", 
  "congressional_district", "dist_median_centroid"
)

addfile <- read.csv(here(hud951_raw_dir, "ADDFILE_text.txt"), header = FALSE,
                      sep = ",", stringsAsFactors = FALSE)

# drop last column: V23, which is an empty column
addfile <- addfile[, -23]

# assing addfile_colnames
colnames(addfile) <- addfile_colnames


state_fips <-
  read_csv(paste0(geo_crosswalk_dir, "state_fips.csv")) %>% 
  mutate(state_name = toupper(state_name))


# Clean data ----

# BGfile Public housing projects
bgfile_ph <- bgfile %>% 
  filter(program == "P") %>% 
  filter(str_detect(matching_group, "^[A-Z]{2}")) %>% 
  mutate(
    prefix = str_sub(matching_group, 1, 2),             # Extract the first 2 letters
    part1 = as.numeric(str_sub(matching_group, 3, 5)),  # Extract next 3 characters and convert to numeric
    part2 = as.numeric(str_sub(matching_group, 6, 8)),  # Extract following 3 characters and convert to numeric
    project_code = paste0(prefix, "-", part1, "-", part2) # Combine into project_code
  ) %>% 
  select(-prefix, -part1, -part2) %>% 
  select(project_code, project_name, contains("census"),
         contains("avg_lat"), contains("avg_long"), 
         contains("med_lat"), contains("med_long"),
         official_project_units)


# PRJfile Public housing projects
prjfile_ph <- prjfile %>% 
  filter(program == "P") %>% 
  filter(str_detect(matching_group, "^[A-Z]{2}")) %>% 
  mutate(
    prefix = str_sub(matching_group, 1, 2),             # Extract the first 2 letters
    part1 = as.numeric(str_sub(matching_group, 3, 5)),  # Extract next 3 characters and convert to numeric
    part2 = as.numeric(str_sub(matching_group, 6, 8)),  # Extract following 3 characters and convert to numeric
    project_code = paste0(prefix, "-", part1, "-", part2) # Combine into project_code
  ) %>% 
  select(-prefix, -part1, -part2) %>% 
  select(project_code, project_name, matching_group, contains("census"),
         contains("avg_lat"), contains("avg_long"), 
         contains("med_lat"), contains("med_long"),
         number_of_addresses, official_units,
         street_name, street_num, -contains("std"), -contains("avg")) %>% 
  mutate(street_name = str_trim(street_name),
         street_num = str_trim(street_num))

## Addfile ph
addfile_ph <- addfile %>% 
  filter(str_detect(matching_group, "^[A-Z]{2}")) %>% 
  mutate(
    prefix = str_sub(matching_group, 1, 2),             # Extract the first 2 letters
    part1 = as.numeric(str_sub(matching_group, 3, 5)),  # Extract next 3 characters and convert to numeric
    part2 = as.numeric(str_sub(matching_group, 6, 8)),  # Extract following 3 characters and convert to numeric
    project_code = paste0(prefix, "-", part1, "-", part2) # Combine into project_code
  ) %>% 
  select(-prefix, -part1, -part2) %>% 
  select(matching_group, project_code, contains("census"),
         contains("lat"), contains("long"), 
         units, street_name, street_num) %>%
  mutate(street_name = str_trim(street_name),
         street_num = str_trim(street_num)) %>% 
  # drop if quality == 9 (not geocoded)
  filter(quality_lat_long != 9) %>% 
  # set tract to NA if it is == 0 
  mutate(census_tract_1990 = ifelse(census_tract_1990 == 0, NA, census_tract_1990)) %>%
  # set tract to NA if it is .99
  mutate(census_tract_1990 = ifelse(census_tract_1990 == .99, NA, census_tract_1990)) %>%
  # set block group to NA if it is an empty string
  mutate(census_block_group_1990 = ifelse(census_block_group_1990 == "", NA, census_block_group_1990))
  


# no project codes in PRJFILE that are not in ADDFILE

# ## Clean CDD data for merge ---- 
# # Note: Can delete this
# ## Clean CDD  data
# cdd_data <-
#   cdd_data_raw %>% 
#   left_join(state_fips, by = c("state" = "state_name"))
# 
# # 2. get project code for CDD: State - projectnum1- projectnum2
# cdd_data <-
#   cdd_data %>% 
#   mutate(project_code = paste0(state_usps, "-", projectnum1, "-", projectnum2),
#          # if projectnum3 is not blank, add it to project code full
#          project_code_full = if_else(projectnum3 == "", project_code,
#                                      paste0(state_usps, "-", projectnum1, "-", projectnum2, "-", projectnum3)))
# 
# #project_code_full = paste0(state_usps, "-", projectnum1, "-", projectnum2, "-", projectnum3))
# 
# # 3. Set totunits = totunitsplanned if totunits is 0 or totunits is NA and yearfullocc is not missing
# cdd_data <-
#   cdd_data %>% 
#   mutate(totunits = if_else(!is.na(yearfullocc) & (is.na(totunits) | totunits == 0), totunitsplanned, totunits))
# 
# # Try project-level merge to CDD -----
# 
# prjfile_ph_for_merge <-
#   prjfile_ph %>% 
#   # drop if first 2 letters of matching_group are not letters
#   filter(str_detect(matching_group, "^[A-Z]{2}")) %>% 
#   mutate(
#     prefix = str_sub(matching_group, 1, 2),             # Extract the first 2 letters
#     part1 = as.numeric(str_sub(matching_group, 3, 5)),  # Extract next 3 characters and convert to numeric
#     part2 = as.numeric(str_sub(matching_group, 6, 8)),  # Extract following 3 characters and convert to numeric
#     project_code = paste0(prefix, "-", part1, "-", part2) # Combine into project_code
#   ) %>% 
#   select(-prefix, -part1, -part2) %>% 
#   mutate(length = nchar(matching_group))
# 
# 
# # merge onto CDD 
# cdd_prjfile_ph <- 
#   cdd_data %>% 
#   left_join(prjfile_ph_for_merge %>% select(project_code, program))
# 
# # check share that merged 
# cdd_prjfile_ph %>% 
#   count(program) %>% 
#   mutate(share = n / sum(n))
# 
# # check share of units that merged
# cdd_prjfile_ph %>% 
#   filter(!is.na(totunitsplanned), !is.na(program)) %>% 
#   summarise(totunitsplanned = sum(totunitsplanned)) %>% 
#   mutate(share = totunitsplanned / sum(cdd_data$totunitsplanned, na.rm = TRUE))
# 
# 

# Collapse ADDFILE to project level ----
# 11/23/2024: Doesn't get me anything
# not clear what the project codes that are in this but not in the prj file are 
# THey are definitely not in CDD

# take median lat and long: This is what they do for the project file 
addfile_ph_by_project <- 
  addfile_ph %>% 
  group_by(project_code) %>% 
  summarise(median_latitude = median(add_lat, na.rm = TRUE),
         median_longitude = median(add_long, na.rm = TRUE)) %>% 
  # drop project codes with -NA- in them
  filter(!str_detect(project_code, "-NA-"))


# SF
# addfile_ph_by_project %>% 
#   filter(str_detect(project_code, "NY-5-")) %>% View()
# 
# x <- prjfile_ph %>% 
#   filter(str_detect(project_code, "NY-5-"))


# View projects in addfile but not in prjfile
# addfile_ph_by_project %>% 
#   anti_join(prjfile_ph, by = "project_code") %>% 
#   View()

# Output ----


write_csv(bgfile_ph, here(hud951_output_dir, "hud951_block_group_file_cleaned.csv"))
write_csv(prjfile_ph, here(hud951_output_dir, "hud951_project_file_cleaned.csv"))
write_csv(addfile_ph, here(hud951_output_dir, "hud951_address_file_cleaned.csv"))
#write_csv(addfile_ph_by_project, here(hud951_output_dir, "hud951_address_file_collapsed_by_project_cleaned.csv"))
