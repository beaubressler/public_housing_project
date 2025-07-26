### 
#  Clean 
###

library(tidyverse)
library(here)

data_dir <- here("tenant_project", "data", "derived", "public_housing_addresses", "intermediate")

output_dir <- here("tenant_project", "data", "derived", "public_housing_addresses", "working")

nycha_textract_addresses <- read_csv(here(data_dir, "nycha_addresses_2016_textract.csv"), skip = 1)

# separate file for williamsburg
williamsburg_addresses_raw <- read_csv(here(data_dir, "williamsburg_houses_addresses_handcleaned.csv"))

williamsburg_addresses_clean <- 
  williamsburg_addresses_raw %>% 
  select(ADDRESS) %>% 
  mutate(DEVELOPMENT = "WILLIAMSBURG") %>% 
  dplyr::rename(street_address = ADDRESS) 


# identify for particular developments which were open by april 1940
# ny-5-1	Red Hook	1939 (Doable)
# ny-5-2	Queens-Bridge	1940 (doable, queensbridge), opened 1939
# First houses
# Harlem river 

#
nycha_projects <- c("RED HOOK", "QUEENSBRIDGE", "FIRST HOUSES", "HARLEM RIVER", "WILLIAMSBURG")



# red_hook <- 
#   nycha_textract_addresses %>% 
#   filter(str_detect(DEVELOPMENT, "RED HOOK")) 
# 
# queensbridge <- 
#   nycha_textract_addresses %>% 
#   filter(str_detect(DEVELOPMENT, "QUEENSBRIDGE"))


pre_1940_nyc_ph_addresses <- 
  nycha_textract_addresses %>% 
  filter(str_detect(DEVELOPMENT, "RED HOOK|QUEENSBRIDGE|FIRST HOUSES|HARLEM RIVER|WILLIAMSBURG")) %>%  
  mutate(street_address = paste(`...1`, ADDRESS, sep = " ")) %>% 
  select(street_address, DEVELOPMENT) %>% 
  filter(street_address != "ADDRESS") %>% 
  mutate(street_address = str_to_title(street_address))


# combine
# pre_1940_nyc_ph_addresses <- 
#   bind_rows(
#     red_hook,
#     queensbridge
#   ) %>% 
#   mutate(street_address = paste(`...1`, ADDRESS, sep = " ")) %>% 
#   select(street_address, DEVELOPMENT) %>% 
#   bind_rows(
#     williamsburg_addresses_clean
#   ) %>%
#   filter(street_address != "ADDRESS") %>% 
#   mutate(street_address = str_to_title(street_address))

# Output
write_csv(
  pre_1940_nyc_ph_addresses,
  here(output_dir, "pre_1940_nyc_ph_addresses.csv")
)


