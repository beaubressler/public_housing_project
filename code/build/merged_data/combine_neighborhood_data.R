# This script combines the neighborhood data from the Census, Zillow, and HOLC datasets,
# and saves the combined dataset to a GeoPackage file.


# Preliminaries -----
library(tidyverse)
library(sf)
library(tigris)
library(crsuggest)
library(mapview)
library(maps)
library(ragg)
library(viridis)
library(geosphere)
library(here)
library(segregation)
library(readxl)


data_path <- here("data")
census_data_path <- here(data_path, "derived", "census")
census_ed_tract_data_path <- here(census_data_path, "full_count", "tract")
zillow_data_path <- here(data_path, "derived", "zillow")
holc_data_path <- here(data_path, "derived", "holc")
ur_data_path <- here(data_path, "derived", "urban_renewal")
cpi_data_path <- here(data_path, "raw", "measuring_worth")
cbsa_data_path <- here(data_path, "raw", "cbsa_crosswalk")
highway_data_path <- here(data_path, "derived", "highways")

merged_data_path <- here(data_path, "derived", "merged")

# Common functions and variables
tract_id_variables <- c("YEAR", "GISJOIN_1950")

# Function to read and process census data ----
read_census_data <- function() {
  census_pop_data <- read_sf(here(census_data_path, "tract_population_data.gpkg"))
  
  census_housing_data <- read_sf(here(census_data_path, "tract_housing_data.gpkg")) %>%
    st_drop_geometry() %>%
    dplyr::select(any_of(tract_id_variables), contains("median"), total_units, vacancy_rate,
           share_needing_repair, share_no_water, housing_density)

  census_income_data <- read_sf(here(census_data_path, "tract_income_data.gpkg")) %>%
    st_drop_geometry() %>%
    dplyr::select(any_of(tract_id_variables), contains("median"))
  
  census_education_data <- read_sf(here(census_data_path, "tract_education_data.gpkg")) %>%
    filter(YEAR >= 1940) %>%
    st_drop_geometry() %>%
    dplyr::select(any_of(tract_id_variables), contains("median"), contains("pct"))
  
  census_employment_data <- read_sf(here(census_data_path, "tract_employment_data.gpkg")) %>%
    st_drop_geometry() %>%
    dplyr::select(any_of(tract_id_variables), contains("pop"), contains("rate"))
  
  census_occupation_data <- read_sf(here(census_data_path, "tract_occupation_data.gpkg")) %>%
    st_drop_geometry() %>%
    dplyr::select(any_of(tract_id_variables), contains("occupation"), contains("share"))
  
  census_tract_data_full <- census_pop_data %>%
    left_join(census_housing_data, by = c("YEAR", "GISJOIN_1950")) %>%
    left_join(census_income_data, by = c("YEAR", "GISJOIN_1950")) %>%
    left_join(census_education_data, by = c("YEAR", "GISJOIN_1950")) %>%
    left_join(census_employment_data, by = c("YEAR", "GISJOIN_1950")) %>%
    left_join(census_occupation_data, by = c("YEAR", "GISJOIN_1950")) %>%
    mutate(employment_pop_ratio = employed_pop / total_pop)
  
  return(census_tract_data_full)
}


## read and process census tract data  -----
census_tract_data_full <- read_census_data()

## Incorporate in Census tract data constructed from full count ----
# Two uses:
# 1. Use tract-data from full count for any tracts unavailable in the tract-level data (1930 and 1940)
# 2. 1930 and 1940: Income data is only available as constructed from full count data -> use it here
# 3. For any tracts missing tract data that I constructed with EDs, coalesce


# 1/2025: Use tract-data from full count for any tracts unavailable in the tract-level data
# this mostly includes tracts in cities that were not available at the tract level in those years
# but also includes some in cities that were partially included 

# from fc
census_tract_data_from_full_count <-
  read_sf(here(census_ed_tract_data_path, "tract_data_concorded_from_ed_1930_1940.gpkg")) %>% 
  dplyr::rename(employed_pop = employed,
                unemployed_pop = unemployed,
                not_in_lf_pop = not_in_lf) %>% 
  mutate(employment_pop_ratio = employed_pop / total_pop,
         lfp_rate) %>% 
  # drop a couple of erroneous observations
  filter(total_pop != 0 )

census_tract_data_full_1930_and_1940 <-
  census_tract_data_full %>% 
  filter(YEAR %in% c(1930, 1940))

# Now the anti_join will include NYC 1940 ED-derived data since NYC 1940 Health Areas were removed above
additional_tracts_from_full_count_1930_1940 <- 
  census_tract_data_from_full_count %>% 
  anti_join(census_tract_data_full_1930_and_1940 %>% st_drop_geometry(),
            by = c("GISJOIN_1950", "YEAR"))

# Checking NYC population data in full count and ED 
census_tract_data_from_full_count %>% filter((YEAR == 1940 & STATE == "New York" &
           COUNTY %in% c("Bronx", "Kings", "New York", "Queens", "Richmond"))) %>% 
  pull(total_pop) %>% 
  sum(na.rm= TRUE)

census_tract_data_full %>%
  filter(YEAR == 1940 & STATE == "New York" & 
           COUNTY %in% c("Bronx", "Kings", "New York", "Queens", "Richmond")) %>% 
  pull(total_pop) %>% 
  sum(na.rm= TRUE)

# Census tract data from full count - all variables that may need filling
census_tract_data_from_full_count_supplement <- 
  census_tract_data_from_full_count %>%
  select(GISJOIN_1950, YEAR, 
         median_income, 
         median_rent_calculated, 
         median_home_value_calculated,
         lfp_rate, 
         unemp_rate) %>% 
  filter(YEAR %in% c(1930, 1940)) %>% 
  dplyr::rename(
    median_income_fc = median_income,
    median_rent_calculated_fc = median_rent_calculated,
    median_home_value_calculated_fc = median_home_value_calculated,
    lfp_rate_fc = lfp_rate,
    unemp_rate_fc = unemp_rate
  ) %>% 
  st_drop_geometry()

# Single merge with all supplemental variables
census_tract_data_full <- 
  bind_rows(census_tract_data_full, additional_tracts_from_full_count_1930_1940) %>% 
  left_join(census_tract_data_from_full_count_supplement, by = c("GISJOIN_1950", "YEAR")) %>% 
  # Replace all variables with full count versions if missing 
  mutate(
    median_income = coalesce(median_income, median_income_fc),
    median_rent_calculated = coalesce(median_rent_calculated, median_rent_calculated_fc),
    median_home_value_calculated = coalesce(median_home_value_calculated, median_home_value_calculated_fc),
    lfp_rate = coalesce(lfp_rate, lfp_rate_fc),
    unemp_rate = coalesce(unemp_rate, unemp_rate_fc)
  ) %>% 
  select(-ends_with("_fc")) %>% 
  # recalculate area and population density
  mutate(area_m2 = as.numeric(st_area(geom)),
         population_density = total_pop / area_m2)


# variables that are in the full count data but not in the tract-level data

## compare with original -----
x <- names(census_tract_data_full_1930_and_1940)
y <- names(additional_tracts_from_full_count_1930_1940)
setdiff(x, y)

merged_fc_and_fc_tracts <- 
  census_tract_data_from_full_count %>% st_drop_geometry() %>% 
  left_join(census_tract_data_full_1930_and_1940 %>% st_drop_geometry(),
            by = c("GISJOIN_1950", "YEAR"),
            suffix = c("_fc", "_orig")) %>% 
  mutate(diff = total_pop_fc - total_pop_orig,
         total_pop_fc = round(total_pop_fc,2))

# Note: These are not equivalent, though they are extremely highly correlated
# 7/2025: Now, high correlation between overlapping populations
summary(lm(total_pop_fc ~ total_pop_orig, data = merged_fc_and_fc_tracts))
summary(lm(black_pop_fc ~ black_pop_orig, data = merged_fc_and_fc_tracts))
summary(lm(median_rent_calculated_fc ~ median_rent_calculated_orig, data = merged_fc_and_fc_tracts))
summary(lm(median_home_value_calculated_fc ~ median_home_value_calculated_orig, data = merged_fc_and_fc_tracts))
summary(lm(black_share_fc ~ black_share_orig, data = merged_fc_and_fc_tracts))
summary(lm(pct_hs_grad_fc ~ pct_hs_grad_orig, data = merged_fc_and_fc_tracts))
summary(lm(lfp_rate_fc ~ lfp_rate_orig, data = merged_fc_and_fc_tracts))
        


# check cities where orig is missing in 1930
# merged_fc_and_fc_tracts %>% 
#   filter(is.na(total_pop_orig), total_pop_fc != 0, YEAR == 1930) %>% 
#   select(STATE, COUNTY, YEAR, total_pop_fc, total_pop_orig) %>% 
#   View()
#   


### calculate deflator for monetary values -----
# read in cpi data
cpi_data_raw <- read_csv(here(cpi_data_path, "USCPI_1930-2010.csv"), skip = 3, 
                     col_names = c("year", "cpi"))

# keep only relevant years, calculate deflator to 2000
cpi_data <- cpi_data_raw %>% 
  filter(year %in% seq(1930, 2000, 10)) %>% 
  mutate(deflator = cpi[year == 2000] / cpi)

# Calculate the 1950 to 2000 deflator for 1930 income data
deflator_1950_to_2000 <- 
  cpi_data$cpi[cpi_data$year == 2000] / cpi_data$cpi[cpi_data$year == 1950]

# join to census data, apply deflator to monetary values of median home value and median rent
census_tract_data_full <- census_tract_data_full %>% 
  left_join(cpi_data, by = c("YEAR" = "year")) %>% 
  mutate_at(vars("median_home_value_calculated", "median_home_value_reported",
                 "median_home_value_reported_alt", 
                 "median_rent_calculated", "median_rent_reported",
                 "median_rent_reported_alt"),
            ~ . * deflator) %>%
  # Special handling for median_income: 1930 data is already in 1950 dollars
  mutate(median_income = case_when(
    YEAR == 1930 ~ median_income * deflator_1950_to_2000,
    TRUE ~ median_income * deflator
  )) %>%
  dplyr::select(-deflator) %>% 
  # convert income and home values to 1000s of $ for easier interpretation
  mutate(median_home_value_calculated = median_home_value_calculated / 1000,
         median_home_value_reported = median_home_value_reported / 1000,
         median_home_value_reported_alt = median_home_value_reported_alt / 1000,
         median_income = median_income / 1000)


## add additional tract-level data -----
###  zillow neighborhoods  -----
zillow_neighborhoods <-
  read_csv(here(zillow_data_path, "tracts_with_zillow_neighborhoods.csv")) %>% 
  dplyr::rename(zillow_neighborhood = neighborhood)

### HOLC classifications ----
holc_classifications <- 
  read_csv(here(holc_data_path, "tract_holc_classifications.csv")) %>% 
  dplyr::select(-city)

### Urban renewal classifications  -----
urban_renewal_classifications <- 
  read_csv(here(ur_data_path, "tract_urban_renewal_classifications.csv"))

### CBSA classifications -----
# use 2003 cbsa crosswalk to avoid Census changes since then, especially wrt Connecticut

cbsa_data <- 
  read_xlsx(here(cbsa_data_path, "cbsa_crosswalk_2003.xlsx"), skip = 2) %>%
  dplyr::select(`CBSA Title`, FIPS) %>% 
  dplyr::rename(cbsa_title = `CBSA Title`) %>%
  mutate(COUNTYA = substr(FIPS, 3, 5),
         STATEA = substr(FIPS, 1, 2)) %>% 
  dplyr::select(-FIPS)

### Highway proximity data -----
highway_proximity <- 
  read_csv(here(highway_data_path, "tract_data_with_highway_proximity.csv"))


# Combine datasets -----
combined_data <- 
  census_tract_data_full %>%
  left_join(zillow_neighborhoods, by = c("GISJOIN_1950")) %>% 
  left_join(holc_classifications) %>% 
  left_join(urban_renewal_classifications, by = c("GISJOIN_1950")) %>% 
  # if tract is not in HOLC data, assume it was not redlined
  mutate_at(vars(contains('redlined_binary')), ~ifelse(is.na(.), 0, .)) %>% 
  left_join(cbsa_data, by = c("STATEA", "COUNTYA")) %>%
  left_join(highway_proximity, by = c("GISJOIN_1950")) %>% 
  # manually fix Dade county CBSA
  mutate(cbsa_title = ifelse(COUNTYA == "025" & STATEA == "12",
                             "Miami-Fort Lauderdale-Miami Beach, FL",
                             cbsa_title),
         cbsa_title = ifelse(COUNTYA %in% c("129","151") & STATEA == "51",
                              "Virginia Beach-Norfolk-Newport News, VA-NC",
                              cbsa_title)
         )

# Calculate segregation indices --------
# See https://www.huduser.gov/portal/periodicals/cityscpe/vol17num1/ch8.pdf
# for possible local segregation indices

# Calculate both a standard dissimilarity index (how different is tract black share from CBSA black share)
# and a local aspatial dissimilarity index (following Oka and Wong 2015)

local_aspatial_di <- combined_data %>%
  mutate(bw_pop = black_pop + white_pop) %>%
  filter(bw_pop > 0) %>%  # avoid division by zero
  group_by(cbsa_title, YEAR) %>%
  mutate(
    cbsa_black_total = sum(black_pop, na.rm = TRUE),
    cbsa_white_total = sum(white_pop, na.rm = TRUE),
    cbsa_bw_total = cbsa_black_total + cbsa_white_total,
    cbsa_black_share = cbsa_black_total / cbsa_bw_total
  ) %>%
  ungroup() %>%
  mutate(
    tract_black_share = black_pop / bw_pop,
    # this is how much the tract's black share deviates from the CBSA's black share
    local_dissimilarity_index = abs(tract_black_share - cbsa_black_share),
    # abs(w/W - b/B): This the Oka and Wong (2015) measure: 
    local_dissimilarity_index_std = abs((white_pop/cbsa_white_total) - 
                                        (black_pop/cbsa_black_total))
  ) %>% 
  select(GISJOIN_1950, YEAR, local_dissimilarity_index, local_dissimilarity_index_std) %>% 
  st_drop_geometry()

# Join local dissimilarity index to combined data
combined_data <- combined_data %>%
  left_join(local_aspatial_di, by = c("GISJOIN_1950", "YEAR"))

# Output ----
# output 
write_sf(combined_data, here(merged_data_path, "census_tract_data_full.gpkg"))


