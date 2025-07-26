# 
# TODO: Use SMP + geocoded projects 
# 1. Calculate distance between google maps location and SMP location

# preliminaries-----
library(tidyverse)
library(readxl)
library(here)
out_dir <- here("data", "derived", "public_housing", "working")
  


# read in the google geocoded data
geocoded_projects <- read_xlsx(here(out_dir, "geocoded_projects_handchecked.xlsx"))

# Read in the SMP geocoded 


