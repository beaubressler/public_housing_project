library(tidyverse)
library(foreign)
library(readxl)

### read in raw data -----

# address file
hud_951_add_raw <- read_xlsx("data/raw/hud951/addfile.xlsx")

# project file
hud_951_proj_raw <- read_xlsx("data/raw/hud951/prjfile.xlsx")


### Clean HUD 951 data ----

# keep only columns we need
hud_951_proj <-
  hud_951_proj_raw %>% 
  select(matching_group, program, project_name, number_of_addresses, total_units, total_units_geocoded, city_state) %>% 
  dplyr::rename(city_state_proj = city_state)

# merge the hud_951_datasets together by matching_group
hud_951 <- 
  hud_951_add_raw %>% 
  left_join(hud_951_proj, by = "matching_group") %>% 
  # create state column equal to last 2 characters of city_state in hud_951
  mutate(state = str_sub(city_state, -2, -1)) %>% 
  # remove last 2 characters from city_state, save as city
  mutate(city = str_sub(city_state, 1, -3)) %>% 
  # remove extra whitespace from city and street_name
  mutate(city = str_trim(city), 
         street_name = str_trim(street_name)) %>%
  select(-city_state, -city_state_proj)

  
write_csv(hud_951, "data/derived/hud_951.csv")
