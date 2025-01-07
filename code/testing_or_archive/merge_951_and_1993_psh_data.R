

library(tidyverse)
library(readxl)
library(haven)
library(fuzzyjoin)
library(stringdist)


# read in raw datasets

# PSH 1993 
psh_1993_raw <- read_xlsx("data/raw/pic93/all_projects_1993.xlsx")


# HUD 951 data

hud_951_project_file <-
  read_xlsx("data/raw/hud951/prjfile.xlsx") %>% 
  # keep only public housing 
  filter(program == "P")

hud_951_address_file <- read_xlsx("data/raw/hud951/addfile.xlsx")

hud_951 <- 
  left_join(hud_951_project_file, hud_951_address_file, by = "matching_group")

psh_1993_project_codes <- 
  psh_1993_raw %>% 
  filter(record_type != 3) %>%
  select(code, name)

  temp <- left_join(psh_1993_project_codes, hud_951_project_file, by = c("code" = "clean_section_8_project_num")) 
  