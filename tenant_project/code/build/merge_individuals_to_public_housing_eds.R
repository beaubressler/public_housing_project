####
# In this script, I identify individuals in the 1940 Census who were likely living in public housing projects.
# 
# Steps:
# 1. Load geocoded public housing projects (including completion dates and coordinates)
# 2. Filter to projects completed by April 1, 1940 (Census Day), excluding those completed later that year
# 3. Join these project locations to 1940 enumeration district (ED) polygons to identify which ED each project was located in
# 4. Load a pre-processed geographic reference file (GRF) of 1940 individuals, including ED and city
# 5. Join the GRF data to project EDs to flag individuals living in public housing EDs
# 6. Count one record per household to estimate the number of households captured per project
#
# Notes:
# - Project identification is conservative: EDs must fully contain project points
# - In most cases, the number of identified households is less than the total units reported in HUD data,
#   suggesting under-identification, which is expected due to possible mismatches or edge effects
# - These results can be improved in the future by supplementing with full-count Census enumeration pages

# Output is a CSV file with the following columns:
# histid, ed, city, household_id, project_code and project_name
####


library(tidyverse)
library(sf)
library(here)


# Read in geocoded projects
projects_dir <- here("data", "derived", "public_housing", "working")
geocoded_projects <- read_csv(here(projects_dir, "combined_cdd_and_digitized_projects.csv")) 

geocoded_projects_sf <- 
  geocoded_projects %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

geocoded_projects_1940 <-
  geocoded_projects_sf %>% 
  filter(year_completed <= 1940) %>% 
  # drop projects built after April 1, 1940 (Census date), as well as Queensbridge in NYC
  filter(!(year_completed == 1940 & month_completed >= 4) | project_code == "NY-5-2")



# Read in 1940 enumeration district maps
ed_map_dir <- here("data", "derived", "geographic_crosswalks", "ed_to_tract")
ed_map_1940 <- 
  st_read(here(ed_map_dir, "ed_1940_geometries.shp")) %>% 
  st_transform(4326) 


# Match project to EDs
projects_with_eds <- st_join(
  #st_buffer(geocoded_projects_1940, 5),  # buffer around point (adjust if needed)
  geocoded_projects_1940, 
  ed_map_1940 %>% select(ed, city, geometry),
  join = st_within,     # match each project to the ED polygon it falls in
  left = FALSE          # drops projects that don't match any ED
)


# Drop projects in 1940 that did not exist by April 1st
# projects_with_eds <-
#   projects_with_eds %>% 
#   filter(!(project_name %in% c("John Hope",
#                                "Poe Homes",
#                                "Elyton Village",
#                                )


# Identify people in 1940 who lived in these EDs -----
grf_dir_1940 <- here("data", "derived", "geographic_reference_file")

grf_1940 <-
  read_csv(here(grf_dir_1940, "grf_1940_histid_ed_city.csv"))


grf_1940_for_merge <-
  grf_1940 %>% 
  # remove last 2 characters from b_city
  mutate(b_city = str_sub(b_city, 1, -3))


ph_residents_1940 <- 
  grf_1940_for_merge %>%
  inner_join(
    projects_with_eds %>%
      st_drop_geometry() %>%    # drop spatial info for clean join
      select(ed, city, project_code, project_name),               # avoid duplicates if multiple projects in same ED
    by = c("b_ed" = "ed", "b_city" = "city")
  )


# count per household 
one_per_hh <- 
  ph_residents_1940 %>% 
  select(b_hh, b_city, project_name) %>% 
  distinct()


table(one_per_hh$project_name) %>% View()

# do this as dplyr, include city
one_per_hh_count <-
  one_per_hh %>% 
  group_by(project_name, b_city) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% 
  mutate(project_name = str_to_title(project_name)) # make consistent with other project names

# Takeaway: I get some, but not all, of the people in these projects
# Mostly, I am identifying fewer HH than the number of units in the projects, which is promising
# Presumably, I can improve upon this using the Census sheets + information from Census


# Output 
write_csv(
  ph_residents_1940,
  here("tenant_project", "data", "derived", "public_housing_residents", "intermediate", "ph_ed_residents_1940.csv")
)


