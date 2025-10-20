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

# digitization dir
digitization_dir <- here("data", "digitization")
chi_digitization_dir <- here(digitization_dir, "chicago")
nyc_digitization_dir <- here(digitization_dir, "nycha")

# Read NYC floor data - will merge after project code cleaning
nyc_floors_raw <- read_xlsx(here(nyc_digitization_dir, "pdbdec1973_digitization.xlsx")) %>%
  select(project_code, project_name, floors)

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
  select(-floors) %>%  # Remove floors column before manual fixes
  left_join(manual_fixes) %>% 
  # apply fixes and use hand-checked coordinates when available
  mutate(lat = case_when(
           !is.na(lat_fixed) ~ lat_fixed,  # Manual fixes first
           !is.na(lat) ~ as.character(lat),  # Use existing lat if available
           !is.na(lat_name_with_city) ~ as.character(lat_name_with_city),  # Fall back to name-based coords
           TRUE ~ as.character(lat)),
         long = case_when(
           !is.na(long_fixed) ~ long_fixed,  # Manual fixes first
           !is.na(long) ~ as.character(long),  # Use existing long if available
           !is.na(long_name_with_city) ~ as.character(long_name_with_city),  # Fall back to name-based coords
           TRUE ~ as.character(long))) %>% 
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
  # manual project code fixes
  mutate(project_code = ifelse(project_code == "N- 5-41", "NY-5-41", project_code),
         project_code = ifelse(project_code == "5-149 (G)", "NY-5-149-G", project_code)) %>% 
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

# Merge NYC floor data AFTER project code cleaning
# Clean floor data to match the geocoded data cleaning
nyc_floors_clean <- 
  nyc_floors_raw %>%
  mutate(project_code = toupper(project_code)) %>%
  # Remove ) first, then replace ( with space, then replace spaces with -, then uppercase
  mutate(project_code = str_replace_all(project_code, " \\(", "-"),
         project_code = str_replace_all(project_code, "\\(", "-"),
         project_code = str_replace_all(project_code, "\\)", ""),
         project_code = str_replace_all(project_code, "NY 5", "NY-5"),
         project_code = str_replace_all(project_code, "NY 6", "NY-6"),
         project_code = ifelse(project_code == "NY- 5-41", "NY-5-41", project_code)
         ) %>% 
  select(project_code, project_name, floors)

# Merge floors onto digitized projects for NYC only
digitized_projects <- digitized_projects %>%
  left_join(nyc_floors_clean,
            by = c("project_code", "project_name"))

# Assign race shares to NYC and Chicago digitized projects -----

# For Chicago, convert black and white share based on share of families,
# Then calculate population by race estimates
chicago_data_raw <- read_xlsx(here(chi_digitization_dir, "digitization_annual_statistical_report_1973.xlsx"))
chicago_data_race <-
  chicago_data_raw %>%
  # Fix Excel auto-formatting: Convert "Feb" back to "2"
  mutate(project_code = str_replace(project_code, "Feb", "2")) %>%
  mutate(number_of_families_black = as.numeric(number_of_families_black),
         number_of_families_white = as.numeric(number_of_families_white),
         total_families = as.numeric(total_families),
         total_population = as.numeric(total_population)) %>%
  mutate(black_share = number_of_families_black / total_families,
         white_share = number_of_families_white/ total_families) %>%
  mutate(black_pop = round(black_share * total_population),
         white_pop = round(white_share * total_population)) %>%
  select(project_code, project_name, address_or_streets, black_pop, white_pop) %>%
  rename(black_pop_chicago = black_pop, white_pop_chicago = white_pop) %>%
  mutate(project_code = paste0("IL-", project_code))



# For NYC: I will use data shared by Max Guennewig-Moenert  (initial_composition_ph.xlsx), which comes from NYCHA

nyc_data_race_raw <- 
  read_xlsx(here(nyc_digitization_dir, "initial_composition_ph.xlsx"),
            sheet = "Final data")

nyc_data_race <-
  nyc_data_race_raw %>% 
  # Keep latest date for each project (should be 1971)
  group_by(Project) %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  ungroup() %>%
  # Keep both Project and joint_develop_name for matching, plus race data
  select(Project, joint_develop_name, date, pwhite_date, pblack_date, total_date) %>%
  rename(project_name = Project,
         joint_name = joint_develop_name,
         white_share_nyc = pwhite_date,
         black_share_nyc = pblack_date,
         total_pop_nyc = total_date)

# Test NYC race data merge - first check for exact matches
nyc_exact_matches <- intersect(
  digitized_projects %>% filter(city == "New York City") %>% pull(project_name),
  nyc_data_race$project_name
)

cat("Exact matches found:", length(nyc_exact_matches), "\n")
cat("Exact matches:", paste(nyc_exact_matches, collapse = ", "), "\n\n")

# Show some examples of non-matching names for manual inspection
cat("Sample digitized project names:\n")
cat(paste(head(digitized_projects %>% filter(city == "New York City") %>% pull(project_name), 10), collapse = "\n"))
cat("\n\nSample race data project names:\n") 
cat(paste(head(nyc_data_race$project_name, 10), collapse = "\n"))

# Manual matching table for NYC projects (corrected with actual race data names)
nyc_name_matches <- tribble(
  ~digitized_name, ~race_data_name,
  
  # Street addresses
  "03 Vernon Ave", "303 Vernon Avenue",
  "1010 E. 178th St", "1010 East 178th Street", 
  "131 St. Nicholas Ave.", "131 St. Nicholas Avenue",
  "335 E. 111th St", "335 East 111th Street",
  "344 E. 28th St", "344 East 28th Street",
  "830 Amsterdam Ave.", "830 Amsterdam Avenue",
  "1471 Watson Ave", "1471 Watson Avenue",
  "33-35 Saratoga Ave", "33-35 Saratoga Ave",
  
  # Key direct name matches using actual race data names
  "Queens-Bridge", "Queensbridge",
  "Jacob Riis", "Riis Federal", 
  "Red Hook", "Red Hook I",
  "Red Hook II", "Red Hook II",
  "East River", "East River",
  "Vladeck", "Viadeck Federal",
  "South Jamaica", "South Jamaica I",
  "South Jamaica II", "South Jamaica II",
  "Baruch", "Baruch - Sect. 1",
  "Van Dyke", "Van Dyke - Sect. I",
  "Van Dyke II", "Van Dyke II",
  "Breukelen", "Breukelen",
  "Bronxdale", "Bronxdale",
  "Cooper Park", "Cooper Park",
  "Gravesend", "Gravesend",
  "Hammel", "Hammel",
  "Baisley Park", "Baisley Park",
  "Richmond Terrace", "Richmond Terrace",
  "Williamsburg", "Williamsburg",
  "Harlem River", "Harlem River",
  "Harlem River II", "Harlem River II",
  "Tompkins", "Tompkins",
  "Morrisania", "Morrisania",
  "Abraham Lincoln", "Lincoln",
  "Amsterdam", "Amsterdam",
  "Albany II", "Albany II",
  "Marcy", "Marcy",
  "Gowanus", "Gowanus",
  "Astoria", "Astoria - Sect. I",
  "Melrose", "Melrose",
  "Farragut", "Farragut",
  "Bronx River", "Bronx River",
  "Sound View", "Sound View",
  "Cypress Hills", "Cypress Hills",
  "Howard", "Howard",
  "Douglass Addition", "Douglass Addition",
  "Marlboro", "Marlboro",
  "Bushwick", "Bushwick",
  "Castle Hill", "Castle Hill",
  "Edgemere", "Edgemere",
  "Rutgers", "Rutgers",
  "Stapleton", "Stapleton",
  "Baychester", "Baychester",
  "Audubon", "Audubon",
  "Independence", "Independence",
  "Chelsea", "Chelsea",
  "Chelsea Addition", "Chelsea Addition",
  "Bronx River Addition", "Bronx River Addition",
  "Pelham Parkway", "Pelham Parkway",
  "Gun Hill", "Gun Hill",
  "Nostrand", "Nostrand",
  "Glenwood", "Glenwood",
  "Todt Hill", "Todt Hill",
  "Marble Hill", "Marble Hill",
  "Boulevard", "Boulevard",
  "Parkside", "Parkside",
  "Arverne", "Arverne",
  "Linden", "Linden",
  "Bay View", "Bay View",
  "Coney Island", "Coney Island",
  "Glenmore Plaza", "Glenmore Plaza",
  "Woodside", "Woodside",
  "Wyckoff Gardens", "Wyckoff Gardens",
  
  # Additional comprehensive matches for remaining projects
  "Kings-Borough", "Kingsborough",
  "Claron Point Gardens", "Clason Point",
  "Edwin Markham Gardens", "Markham Gardens",
  "St. Nicholas", "St.Nicholas - Sect.I & II",
  "George Washington", "Washington - Sect. I",
  "Throgg's Neck", "Throggs Neck",
  "Jefferson", "Jefferson Sect. 1",
  "Brevoort", "Brevoort Sect. 1",
  "Edenwald", "Edenwald",
  "Mariner's Harbor", "Mariners Harbor",
  "La Guardia", "LaGuardia - Sect. I",
  "Sen. Robert F. Wagner, Sr.", "Wagner - Sect. I",
  "Highbridge Gardens", "Highbridge",
  "Daniel Webster", "Webster",
  "General Grant", "Grant Sect. 1",
  "William McKinley", "McKinley",
  "Samuel Gompers", "Gompers",
  "Herbert H. Lehman", "Lehman Village",
  "Samuel J. Tilden", "Tilden",
  "Louis Heaton Pink", "Pink",
  "James Monroe", "Monroe",
  "Gouverneur Morris", "Morris - Sect. I",
  "West Brighton I", "West Brighton Plaza I",
  "West Brighton II", "West Brighton Plaza II",
  "Andrew Jackson", "Jackson",
  "Mott Haven (C)", "Mott Haven",
  "De Witt Clinton", "Clinton",
  "Lafayette", "Lafayette Gardens",
  "John Adams", "Adams",
  "John P. Mitchell", "Mitchel",
  "Robert Fulton", "Fulton",
  "Eleanor Roosevelt", "Eleaner Roosevelt I",
  "Site A", "120 West 94th Street",
  "Site B", "74 West 92nd Street",
  "Site C", "589 Amsterdam Avenue",
  "Stanley M. Isaacs", "Isaacs",
  "Boston Sector", "Boston-Secor",
  "La Guardia Addition", "LaGuardia Addition",
  "Polo Grounds Towers", "Polo Grounds",
  "Nathan Straus", "Straus",
  "Sen. Robert A. Taft", "Taft",
  "Eugenio Maria De Hostos Apts.", "Eugenio Maria de Hostos",
  "John Haynes Holmes Towers", "John Haynes Holmes",
  "Mary McLeod Bethune", "Bethune",
  "Ocean Hill Apartments", "Ocean Hill",
  "E.R. Moore", "Moore",
  "Langston Hughes Apartments", "Langston Hughes",
  "Eleanor Roosevelt II", "Eleaner Roosevelt II",
  "Carter G. Woodson", "larter G. Woodson",
  "Max Meltzer Tower", "Max Meltzer",
  "Rafael Hernandez", "Raphael Hernandes",
  "W 32nd St, Mermaid Ave", "W 32nd St.-Mermaid Avenue",
  "Garald J Carey Gardens", "Gerald Carey Gardens",
  "William Reid Apartments", "Williem Reid",
  "Metro North Plaza", "Metro North",
  "Lewis H. Latimer Gardens", "Latimer Gardens",
  "Throggs Neck Addtion", "Throggs Neck Addition",
  "East Chester", "Eastchester",
  "Colonial Park", "Colonial Park",
  "First Houses", "First",
  "J.L. Elliot", "Elliot",
  "Dyckman", "Dyckman",
  "Seogwick", "Sedgwick",
  "Lexington", "Lexington",
  "Ravenswood", "Ravenswood",
  "Cassidy Place- Lafayette Ave.", "Hassidy Pl-Lafayette Ave.",
  "Park Ave-E. 122nd St.-E. 123rd St", "Park Avenue-East 123rd st. 115 East 122nd Street",
  "Fenimore St-Lefferts Ave", "Fenimore St. Lefferts Ave",
  "Teller Ave-E 166th St", "1100-1110 Teller Avenue",
  "Hoe Ave-E.,173rd St", "Hoe Avenue-East 173rd St.",
  "Eagle Ave-E. 163rd St", "905-907 Eagle Avenue",
  "Whitman-Ingersoll", "Whitman",
  "Lillian Wald", "Wald",
  "Lester W. Patterson", "Patterson",
  "J.W. Johnson", "Johnson",
  "Gov. Smith", "Smith - Sect. I",
  "Martin Luther King, Jr.", "King",
  "Albany", "Albany I",
  "James A. Bland", "Bland",
  "Redfern", "Redfern - Sect. I",
  "Carver", "Carver - Sect. I",
  "Forest", "Forest - Sect. I",
  "Sumner", "Sumner - Sect. I",
  "Frederick Douglass", "Douglass",
  "Mill Brook", "Mill Brook - Sect. I",
  "Manhattan-Ville", "Manhattanville",
  "Drew-Hamilton", "Drew",
  "Borgia Butler", "Butler - Sect. I",
  "Woodrow Wilson", "Wilson",
  "Gaylord White", "White",
  "Mill Brook Extension", "Mill Brook - Extension",
  "Jonathan Williams", "Williams",
  "Stephen Wise Towers", "Wise",
  "Arthur H. Murphy", "Murphy",
  "Bernard Haber", "Haber",
  "Gen. Charles W. Berry", "Berry",
  "Pmonok", "Pomonok",
  "St.Mary's Park", "St. Mary's Park",
  "John F. Hylan", "Hylan",
  "William O'Dwyer Gardens", "O'Dwyer",
  
  # Final round - remaining unmatched projects
  "Rehab W. Side Urban Renewal", "W.S.U.R. Rehabs W.S.U.R. Vest Pockets",
  "Seth Low", "Seth Low",
  "Dr. Ramon E. Betances", "Eugenio Maria de Hostos",
  "Rehabilitation", "Rehabilitation Program",
  "Mott Haven (Rehab) (Sites 9, 13, 18)", "Rehab - Mott Haven",
  "Mott Haven (Rehab) (Sites 4, 5, 9)", "Rehab - Mott Haven",
  "Brownsville", "Brownsville",
  "Amsterdam Addition", "Amsterdam",
  "Coney Island I", "Coney Island",
  "Village View (Roosevelt)", "Eleaner Roosevelt I",
  "Cedar Manor (Baisley Gardens)", "Baisley Park",
  "Vladeck", "Vladeck City",
  
  # Additional matches found from unmatched analysis
  "Sheepshead Bay", "Sheepshead Bay",
  "South Beach", "South Beach", 
  "Carleton Manor", "Carleton Manor",
  "Kingsborough Extension", "Kingsborough Extension",
  "Coney Island I (Site 1B)", "Coney Island",
  "Coney Island I (Site 8)", "Coney Island"
)

# Use manual matching table - try both project_name and joint_name
nyc_manual_match <- 
  digitized_projects %>%
  filter(city == "New York City") %>%
  left_join(nyc_name_matches, by = c("project_name" = "digitized_name")) %>%
  left_join(nyc_data_race, by = c("race_data_name" = "project_name")) %>%
  mutate(match_type = "project_name_match")

# Also try direct matching with joint_develop_name for unmatched projects
nyc_joint_match <- 
  digitized_projects %>%
  filter(city == "New York City") %>%
  # Only try projects that didn't match in manual table
  anti_join(nyc_name_matches, by = c("project_name" = "digitized_name")) %>%
  # Clean joint names for matching (remove \r\n characters)
  left_join(
    nyc_data_race %>% 
      mutate(joint_name_clean = str_replace_all(joint_name, "\\r\\n", "") %>% str_trim()),
    by = c("project_name" = "joint_name_clean")
  ) %>%
  mutate(match_type = "joint_name_match", race_data_name = joint_name)

# Combine both matching approaches
nyc_proj_race_merged <-
  bind_rows(
    nyc_manual_match %>% filter(!is.na(white_share_nyc)),
    nyc_joint_match %>% filter(!is.na(white_share_nyc))
  ) %>%
  # Calculate population by race using shares * total population
  mutate(
    white_pop_nyc = round((white_share_nyc/100) * total_population),
    black_pop_nyc = round((black_share_nyc/100) * total_population)
  ) %>%
  # Keep project_code and project_name for unique identification
  select(project_code, project_name, white_pop_nyc, black_pop_nyc) %>%
  # Remove duplicates - keep first match for each project_code + project_name
  distinct(project_code, project_name, .keep_all = TRUE)


cat("Manual matches found:", nrow(nyc_proj_race_merged), "\n")

# Show sample matches
cat("Sample successful matches:\n")
print(head(nyc_proj_race_merged, 10))


# Identify unmatched race data projects
nyc_race_unmatched <- nyc_data_race %>%
  anti_join(nyc_proj_race_merged %>% 
              select(project_name, white_pop_nyc, black_pop_nyc) %>%
              left_join(
                bind_rows(
                  nyc_manual_match %>% filter(!is.na(white_share_nyc)) %>% select(project_name, race_data_name),
                  nyc_joint_match %>% filter(!is.na(white_share_nyc)) %>% select(project_name, race_data_name = joint_name)
                ), 
                by = "project_name"
              ) %>%
              filter(!is.na(race_data_name)),
            by = c("project_name" = "race_data_name")) 

cat("\nUnmatched NYC race data projects:", nrow(nyc_race_unmatched), "\n")
cat("Race data used:", nrow(nyc_data_race) - nrow(nyc_race_unmatched), "out of", nrow(nyc_data_race), "\n")
cat("Usage rate:", round((nrow(nyc_data_race) - nrow(nyc_race_unmatched)) / nrow(nyc_data_race) * 100, 1), "%\n\n")

if(nrow(nyc_race_unmatched) > 0) {
  cat("Sample unmatched race data projects:\n")
  print(head(nyc_race_unmatched, 20))
}


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
  select(state, locality, project_code, project_code_full, name, totunitsplanned,
         yearfullocc, monthfullocc, latitude, longitude, statefips, countyfips,
         state_usps, slum, starts_with("proj_"),
         ends_with("ngda")) %>%
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
  dplyr::rename(proj_total_population_estimate = total_population) %>%
  # Add project_code_full (same as project_code for digitized data)
  mutate(project_code_full = project_code)
  

# Figure out Boston projects
cdd_projects_boston <-
  cdd_projects %>%
  filter(locality == "BOSTON", state == "MASSACHUSETTS") %>%
  filter(!str_detect(name, "Scattered"), !str_detect(name, "Scatter")) %>% 
  filter(!is.na(yearfullocc)) 


digitized_projects_boston <- digitized_projects %>%
  filter(city == "Boston") %>%
  filter(!str_detect(tolower(project_name), "scattered")) %>%
  filter(year_completed <= 1973) %>%
  # Add project_code_full for consistency
  mutate(project_code_full = project_code) 


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
         longitude = coalesce(longitude, long),
         project_code_full = coalesce(project_code_full.x, project_code_full.y, project_code)) %>%
  select(state, locality, project_code, project_code_full, project_name, total_units,
         year_completed, latitude, longitude, statefips, countyfips,
         state_usps, slum, source) %>% 
  mutate(city = "Boston", locality = "BOSTON", statefips = 25, countyfips = 25025, 
         statefip = 25, state_usps = "MA") %>% 
  # drop observations that are duplicates on project_code and units
  distinct(project_code, total_units, .keep_all = TRUE)
  
# Combine all data and merge race data for NYC and Chicago
combined_data <-
  bind_rows(cdd_data_for_combined_data, digitized_data_for_combined_data) %>% 
  bind_rows(merged_boston_data) %>% 
  # Standardize locality names by removing extra spaces
  mutate(locality = str_squish(locality)) %>%
  # Merge NYC race data using project_code and project_name
  left_join(nyc_proj_race_merged, by = c("project_code", "project_name")) %>%
  # Merge Chicago race data using project_code and address
  left_join(chicago_data_race, by = c("project_code", "project_name", "address_or_streets")) %>%
  # Coalesce race data columns for final variables
  mutate(
    proj_white_population_estimate = coalesce(proj_white_population_estimate, white_pop_nyc, white_pop_chicago),
    proj_black_population_estimate = coalesce(proj_black_population_estimate, black_pop_nyc, black_pop_chicago)
  ) %>%
  # Create harmonized building_type variable from Chicago/NYC digitized data
  mutate(
    floors_max = str_extract_all(floors, "[0-9]+") %>%
      map_chr(~ifelse(length(.x) > 0, max(as.numeric(.x)) %>% as.character(), NA_character_)) %>%
      as.numeric(),
    building_type_digitized = case_when(
      # Chicago: use building_style
      !is.na(building_style) & str_detect(tolower(building_style), "elevator") ~ "High-rise (elevator)",
      !is.na(building_style) & str_detect(tolower(building_style), "low-rise") ~ "Mid-rise (walk-up)",
      !is.na(building_style) & str_detect(tolower(building_style), "lr & e") ~ "Mixed",
      # NYC: use max floors (>=7 is high-rise)
      !is.na(floors_max) & floors_max >= 7 ~ "High-rise (elevator)",
      !is.na(floors_max) & floors_max >= 4 ~ "Mid-rise (walk-up)",
      !is.na(floors_max) & floors_max >= 1 ~ "Low-rise",
      TRUE ~ NA_character_
    )
  ) %>%
  # Create concorded building type variable combining all sources
  mutate(
    building_type_concorded = case_when(
      # Digitized data takes priority (Chicago building_style and NYC floors)
      !is.na(building_type_digitized) ~ building_type_digitized,
      # NGDA data as backup
      !is.na(building_type_ngda) ~ building_type_ngda,
      TRUE ~ NA_character_
    )
  ) %>%
  select(state, locality, project_code, project_code_full, project_name, total_units,
         year_completed, month_completed, latitude, longitude, statefips, countyfips,
         statefip, state_usps, slum, source,
         contains("proj_"),
         floors, building_style, number_of_buildings,
         building_type_digitized, building_type_ngda, building_type_concorded) 


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
