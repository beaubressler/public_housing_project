####
# Prepare HOLC map data
# and assign a grade to each Census tract

# Definitions of redlining
#  1. Weiwu (2024): Tracts that are more than 80% covered by HOLC grade D areas
       

####

# preliminaries ----
library(tidyverse)
library(sf)


# Read and Clean data -----
# read HOLC data from mapping inequality
holc_data_raw <- st_read("data/raw/mapping_inequality/mappinginequality.gpkg")

# # read 1990 Census tract data
census_tracts_raw <- st_read("data/derived/census/tract_population_data.gpkg")

# census_tracts_raw <-
#   st_read("data/derived/merged/census_tract_sample_with_treatment_status.gpkg")

# convert HOLC data to same projection as Census data
holc_data <- 
  holc_data_raw %>% 
  # set city == "New York City" for all the 5 boroughs, which are separate in the raw data
  mutate(city = case_when(
    city == "Bronx" ~ "New York City",
    city == "Brooklyn" ~ "New York City",
    city == "Manhattan" ~ "New York City",
    city == "Queens" ~ "New York City",
    city == "Staten Island" ~ "New York City",
    TRUE ~ as.character(city)
  )) %>%
  # # keep only those in the cities in our sample: Note, there's no HOLC data for Cincinatti or DC here
  # filter(city %in% unique(census_tracts_raw$city)) %>% 
  st_transform(st_crs(census_tracts_raw)) %>% 
  # correct any invalid geometries
  st_make_valid() 


# Keep only one of each unique 1990 census tract ----
census_tracts <- 
  census_tracts_raw %>% 
  group_by(STATEA, COUNTYA, TRACTA) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  # correct invalid geometries
  st_make_valid() %>% 
  # keep only the columns we need
  select(STATEA, COUNTYA, TRACTA)

## Intersect holc ratings with Census tracts, and pick rating with largest share of tract -----
# A single tracts can have multiple ratings (since the HOLC maps do not track with 1990 census tracts)
# so, we will assign to each tract the area with the greatest overlap with the HOLC data

# turn off spherical geometry, which seems to muck up the intersection
sf_use_s2(FALSE)

# intersect HOLC and Census tracts
tract_holc_intersections <- 
  # intersect the HOLC data with the census tracts: Results in multiple rows per tract
  st_intersection(census_tracts, holc_data)

## assign HOLC score based on which score intersects the most   -----
tract_holc_max_intersections <-
  tract_holc_intersections %>% 
  # calculate the area of the intersection
  mutate(intersection_area = st_area(geom)) %>% 
  group_by(STATEA, COUNTYA, TRACTA) %>% 
  # keep only the row with the largest intersection area
  filter(intersection_area == max(intersection_area)) %>% 
  ungroup() %>% 
  select(-intersection_area)

tract_holc_max_intersections_no_geom <-
  tract_holc_max_intersections %>% 
  st_drop_geometry()

## Assign continuous score based on share -----
# Calculate areas and proportions
tract_holc_scores <- 
  tract_holc_intersections %>%
  mutate(
    intersection_area = st_area(geom),
    holc_score = case_when(
      grade == "A" ~ 1,
      grade == "B" ~ 2,
      grade == "C" ~ 3,
      grade == "D" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(STATEA, COUNTYA, TRACTA) %>%
  mutate(tract_total_area = sum(intersection_area)) %>%
  ungroup() %>%
  mutate(area_proportion = intersection_area / tract_total_area) %>%
  group_by(STATEA, COUNTYA, TRACTA) %>%
  summarize(
    redlining_score = sum(holc_score * area_proportion, na.rm = TRUE),
    coverage = sum(area_proportion, na.rm = TRUE),
  ) %>%
  ungroup()

# If Redlining score is 0, set it to missing
# In major cities that I will look at, these are mostly industrial and/or commercial areas, which had little population at the time
tract_holc_scores <-
  tract_holc_scores %>% 
  mutate(redlining_score = as.numeric(redlining_score),
         redlining_score = ifelse(redlining_score == 0, NA, redlining_score)) %>% 
  select(-coverage) %>% 
  st_drop_geometry()


## Create redlining binary equal to 1 if >=80% of tract area is in HOLC grade D area ----
# this follows Weiwu (2024)

# NOTE: This df contains all tracts which have some interaction with "Grade D" HOLC area. So 
# need to edit redlined_binary in other data too 
redlined_binary <- 
  tract_holc_intersections %>%
  mutate(intersection_area = st_area(geom)) %>%
  group_by(STATEA, COUNTYA, TRACTA) %>%
  mutate(tract_total_area = sum(intersection_area)) %>%
  ungroup() %>%
  # keep only grade D
  filter(grade == "D") %>%
  mutate(redlined_share = intersection_area / tract_total_area) %>%
  group_by(STATEA, COUNTYA, TRACTA) %>%
  summarize(
    total_grade_d_share = sum(redlined_share, na.rm = TRUE)  # Sum share of Grade D coverage
  ) %>%
  ungroup() %>%
  # Create binary flag: 1 if 80% or more of the tract is Grade D, 0 otherwise
  mutate(redlined_binary_80pp = ifelse(as.numeric(total_grade_d_share) >= 0.8, 1, 0),
         redlined_binary_70pp = ifelse(as.numeric(total_grade_d_share) >= 0.7, 1, 0),
         redlined_binary_50pp = ifelse(as.numeric(total_grade_d_share) >= 0.5, 1, 0)) %>% 
  st_drop_geometry()


# Combine and output final dataset ----
holc_data_final_geom <-
  tract_holc_max_intersections %>% 
  left_join(tract_holc_scores, by = c("STATEA", "COUNTYA", "TRACTA")) %>% 
  left_join(redlined_binary) %>% 
  select(STATEA, COUNTYA, TRACTA, redlining_score, grade, category, fill, contains('redlined_binary'), city) %>% 
  # replace grade = NA if ""
  mutate(grade = ifelse(grade == "", NA, grade)) %>%
  # rename grade and category
  rename(grade_most_overlap = grade, category_most_overlap = category,
         fill_most_overlap = fill) %>% 
  # set redlined binaries equal to 0 if they are missing (these are tracts with no grade_d share)
  mutate_at(vars(contains('redlined_binary')), ~ifelse(is.na(.), 0, .))



holc_data_final_no_geom <-
  tract_holc_max_intersections_no_geom %>% 
  left_join(tract_holc_scores, by = c("STATEA", "COUNTYA", "TRACTA")) %>% 
  left_join(redlined_binary) %>% 
  select(STATEA, COUNTYA, TRACTA, redlining_score, grade, category, fill,
         contains('redlined_binary'), city) %>%
  # replace grade = NA if ""
  mutate(grade = ifelse(grade == "", NA, grade)) %>%
  # rename grade and category
  rename(grade_most_overlap = grade, category_most_overlap = category,
         fill_most_overlap = fill) %>% 
  # set redlined binaries equal to 0 if they are missing (these are tracts with no grade_d share)
  mutate_at(vars(contains('redlined_binary')), ~ifelse(is.na(.), 0, .))


# Save -----
write_csv(holc_data_final_no_geom, "data/derived/holc/tract_holc_classifications.csv")

# 
# # Create map -----
# custom_colors <- c("Best" = "green", 
#                    "Still Desirable" = "blue", 
#                    "Definitely Declining" = "yellow", 
#                    "Hazardous" = "red",
#                    "Industrial" = "black",
#                    "Commercial" = "grey")
# 
# # Plot the map
# ggplot() +
#   geom_sf(data = holc_data_final_geom %>% filter(city == "Chicago"), aes(fill = category_most_overlap)) +
#   geom_sf(data = census_tracts %>% filter(STATEA == "17", COUNTYA == "031"), fill = NA, color = "black")+
#   scale_fill_manual(values = custom_colors) +
#   theme_void() +
#   theme(legend.position = "right")
# 
# 
# 
# # Trial: Mapping Chicago HOLC data -----
# chicago_holc_data <- holc_data %>% filter(city == "Chicago")
# chicago_census_data <- census_tracts_raw %>% filter(COUNTY == "Cook", STATE == "Illinois", YEAR== 1990)
# 
# # map chicago HOLC data
# ggplot(census_tracts) +
#   geom_sf(census_tracts) +
#   geom_sf(data = chicago_holc_data, aes(geometry = geom, fill = category)) + 
#   theme_void()
#   
# 
# 
# intersected_data <- st_intersection(chicago_census_data, chicago_holc_data) 
# 
# x <- intersected_data %>% 
#   mutate(intersection_area = st_area(geom)) %>%
#   group_by(STATEA, COUNTYA, TRACTA) %>%
#   filter(intersection_area == max(intersection_area)) %>%
#   ungroup()
# 
# 
# 
# # Plot the map
# ggplot() +
#   geom_sf(data = chicago_census_data, fill = "lightgray", color = "white", size = 0.1) +
#   geom_sf(data = x, aes(fill = category)) +
#   #scale_fill_gradient(low = "blue", high = "red", name = "Neighborhood Quality") +
#   theme_void() +
#   theme(legend.position = "right")
# 
# 
# # just map chicago HOLC data
# ggplot() +
#   geom_sf(data = chicago_holc_data, aes(fill = category)) +
#   theme_void() +
#   theme(legend.position = "right")
