####
# Collapse geocoded 1940 Full Count addresses to 1950 tract level
#
# This script implements a hybrid approach:
# - Uses geocoded point-in-polygon assignment for EDs with poor area-weight coverage
# - Falls back to existing area-weighted ED-to-tract method for well-covered EDs
# - Produces tract-level aggregates comparable to collapse_ed_to_tract_data.R
####

library(tidyverse)
library(sf)
library(here)
library(ipumsr)
library(data.table)

# Directories
census_data_dir <- here("data", "derived", "census")
grf_dir <- here("data", "derived", "geographic_reference_file")
geocoded_dir <- here(grf_dir, "geolocated", "esri")
crosswalk_dir <- here("data", "derived", "geographic_crosswalks", "ed_to_tract")
full_count_dir <- here("data", "raw", "ipums", "full_count")
output_dir <- here(census_data_dir, "full_count", "tract")

# Quality threshold for ED coverage (use geocoding if weight sum outside this range)
WEIGHT_SUM_MIN <- 0.90
WEIGHT_SUM_MAX <- 1.10
MIN_MATCH_SCORE <- 85

message("Step 1: Loading and assessing ED-to-tract crosswalk quality...")

# Load ED-to-tract crosswalk
ed_tract_crosswalk_1940 <- read_csv(here(crosswalk_dir, "ed_1940_to_1950_tracts.csv"))

# Assess quality of ED coverage by checking weight sums per tract
ed_quality_assessment <- ed_tract_crosswalk_1940 %>%
  mutate(ed_1940 = tolower(ed_1940)) %>%
  group_by(city, state, ed_1940) %>%
  summarise(
    n_tracts_linked = n(),
    total_weight = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    quality_flag = case_when(
      total_weight < WEIGHT_SUM_MIN ~ "poor_coverage",
      total_weight > WEIGHT_SUM_MAX ~ "overcoverage",
      TRUE ~ "good_coverage"
    ),
    use_geocoding = quality_flag != "good_coverage"
  )

# Summary stats
message("ED Coverage Quality Summary:")
message(sprintf("  Good coverage (%.2f-%.2f): %d EDs (%.1f%%)",
                WEIGHT_SUM_MIN, WEIGHT_SUM_MAX,
                sum(ed_quality_assessment$quality_flag == "good_coverage"),
                100 * mean(ed_quality_assessment$quality_flag == "good_coverage")))
message(sprintf("  Poor coverage (<%.2f): %d EDs (%.1f%%)",
                WEIGHT_SUM_MIN,
                sum(ed_quality_assessment$quality_flag == "poor_coverage"),
                100 * mean(ed_quality_assessment$quality_flag == "poor_coverage")))
message(sprintf("  Overcoverage (>%.2f): %d EDs (%.1f%%)",
                WEIGHT_SUM_MAX,
                sum(ed_quality_assessment$quality_flag == "overcoverage"),
                100 * mean(ed_quality_assessment$quality_flag == "overcoverage")))

# Save diagnostic
write_csv(ed_quality_assessment,
          here(output_dir, "diagnostics", "ed_coverage_quality_1940.csv"))

message("\nStep 2: Loading geocoded address data (this may take a moment)...")

# Read geocoded addresses with quality filters
geocoded_1940 <- fread(here(geocoded_dir, "grf_unique_addresses_1940_Geocoded_ExportTable.csv")) %>%
  as_tibble() %>%
  filter(
    Status != "T",  # Remove ties
    Score >= MIN_MATCH_SCORE  # Keep only high-quality matches
  ) %>%
  select(USER_full_street_address, X, Y, Score, City, RegionAbbr, Status)

message(sprintf("Loaded %d geocoded addresses (%.1f%% of total after quality filtering)",
                nrow(geocoded_1940),
                100 * nrow(geocoded_1940) / 5436226))  # Total rows in file

# Convert to spatial points
geocoded_sf <- geocoded_1940 %>%
  filter(!is.na(X), !is.na(Y)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%  # WGS84
  st_transform(crs = st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))  # Albers Equal Area

rm(geocoded_1940)  # Free memory
gc()

message(sprintf("Created spatial points for %d addresses", nrow(geocoded_sf)))

message("\nStep 3: Loading GRF linkage to HISTID and ED...")

# Load GRF with HISTID-ED linkage (using fread for speed)
grf_1940 <- fread(here(grf_dir, "grf_1940_histid_ed_city.csv")) %>%
  as_tibble() %>%
  mutate(
    HISTID = histid,
    b_ed = tolower(b_ed),
    city = str_sub(b_city, 1, -3),
    state = str_sub(b_city, -2, -1)
  ) %>%
  select(HISTID, b_ed, city, state, b_hh)

setDT(grf_1940)
setkey(grf_1940, HISTID)

message(sprintf("Loaded GRF with %d records", nrow(grf_1940)))

# Merge geocoded addresses with GRF (match on address string)
# Note: The geocoded file uses "USER_full_street_address" which should match the address from GRF prep script
message("\nStep 4: Merging geocoded locations with person-level data...")

# Load full count IPUMS microdata (in chunks to manage memory)
ddi_1940 <- read_ipums_ddi(here(full_count_dir, "usa_00033.xml"))

# Read full count data
full_count_1940 <- read_ipums_micro(ddi_1940, verbose = TRUE)

# Add ED and city information
full_count_1940 <- full_count_1940 %>%
  left_join(grf_1940, by = "HISTID")

# Check merge rates
merge_rate <- mean(!is.na(full_count_1940$b_ed))
message(sprintf("Full count to GRF merge rate: %.1f%%", 100 * merge_rate))

# Filter to cities with ED maps
cities_with_eds <- c(
  "Akron", "Albany", "Atlanta", "Baltimore", "Birmingham", "Boston",
  "Bridgeport", "Bronx", "Brooklyn", "Buffalo", "Chattanooga",
  "Chicago", "Cincinnati", "Cleveland", "Columbus", "Dallas",
  "Dayton", "Denver", "Des Moines", "Detroit", "Flint", "Fort Worth",
  "Grand Rapids", "Hartford","Houston",  "Indianapolis", "Jacksonville",
  "Jersey City", "Kansas City", "Long Beach", "Los Angeles",
  "Louisville", "Manhattan", "Memphis", "Miami", "Milwaukee",
  "Minneapolis", "Nashville", "New Haven", "New Orleans", "Newark",
  "Norfolk", "Oakland", "Oklahoma City", "Omaha", "Paterson",
  "Philadelphia", "Pittsburgh", "Portland", "Providence", "Queens",
  "Richmond", "Rochester", "Salt Lake City",  "San Antonio", "San Diego",
  "San Francisco", "Scranton", "Seattle", "Spokane", "Springfield",
  "St Louis", "St Paul", "Staten Island", "Syracuse", "Toledo",
  "Trenton", "Tulsa", "Washington", "Worcester", "Yonkers",
  "Youngstown"
)

full_count_1940_filtered <- full_count_1940 %>%
  filter(city %in% cities_with_eds)

message(sprintf("Kept %d records in cities with ED maps (%.1f%% of full count)",
                nrow(full_count_1940_filtered),
                100 * nrow(full_count_1940_filtered) / nrow(full_count_1940)))

message("\nStep 5: Loading 1950 tract boundaries...")

# Load 1950 tract boundaries
tract_1950_sf <- read_ipums_sf(
  here("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1950/US_tract_1950.shp")
) %>%
  st_transform(crs = st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))  # Match geocoded CRS

message(sprintf("Loaded %d 1950 tracts", nrow(tract_1950_sf)))

message("\nStep 6: Spatial join - assigning geocoded addresses to tracts...")

# Perform spatial join (point-in-polygon)
geocoded_with_tract <- st_join(geocoded_sf, tract_1950_sf, join = st_within)

# Merge back with HISTID via address match
# Load the address-HISTID linkage from prep_grf_for_geolocating.R
grf_addresses_1940 <- fread(here(grf_dir, "grf_addresses_1940.csv")) %>%
  as_tibble() %>%
  select(histid, full_street_address)

# Join geocoded tract assignments to person records
full_count_with_geocoded_tract <- full_count_1940_filtered %>%
  left_join(grf_addresses_1940, by = c("HISTID" = "histid")) %>%
  left_join(
    geocoded_with_tract %>%
      st_drop_geometry() %>%
      select(USER_full_street_address, GISJOIN_geocoded = GISJOIN),
    by = c("full_street_address" = "USER_full_street_address")
  )

geocoding_match_rate <- mean(!is.na(full_count_with_geocoded_tract$GISJOIN_geocoded))
message(sprintf("Geocoding match rate: %.1f%%", 100 * geocoding_match_rate))

message("\nStep 7: Implementing hybrid approach...")

# Merge ED quality assessment
full_count_with_method <- full_count_with_geocoded_tract %>%
  left_join(
    ed_quality_assessment %>% select(city, state, b_ed = ed_1940, use_geocoding, quality_flag),
    by = c("city", "state", "b_ed")
  ) %>%
  mutate(
    assignment_method = case_when(
      use_geocoding & !is.na(GISJOIN_geocoded) ~ "geocoded",
      !use_geocoding ~ "ed_area_weight",
      TRUE ~ "unassigned"
    )
  )

# Summary of assignment methods
method_summary <- full_count_with_method %>%
  group_by(assignment_method) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pct = 100 * n / sum(n))

message("Assignment method summary:")
print(method_summary)

# For geocoded assignments: use direct point-in-polygon tract
# For ED area weight assignments: will aggregate to ED then use existing crosswalk

message("\nStep 8: Creating person-level variables for aggregation...")

# Prepare variables following collapse_full_count_to_ed.R patterns
person_data <- full_count_with_method %>%
  mutate(
    # Race categories
    white = ifelse(RACE == 1, 1, 0),
    black = ifelse(RACE == 2, 1, 0),
    other = ifelse(!RACE %in% c(1, 2), 1, 0),

    # Labor force status (for those 14+)
    in_labor_force = AGE >= 14,
    employed = ifelse(in_labor_force & EMPSTAT == 1, 1, 0),
    unemployed = ifelse(in_labor_force & EMPSTAT == 2, 1, 0),
    not_in_lf = ifelse(in_labor_force & EMPSTAT == 3, 1, 0),

    # Education (for those 25+)
    age_25_plus = AGE >= 25,
    hs_grad = ifelse(age_25_plus & EDUC >= 6, 1, 0),  # High school or more
    some_college = ifelse(age_25_plus & EDUC >= 7, 1, 0),  # Some college or more

    # Housing (for household heads)
    is_hh_head = RELATE == 1,

    # Wage income
    has_wage_income = !is.na(INCWAGE) & INCWAGE > 0 & INCWAGE < 999998
  )

# For rent and home value: need to aggregate count-level data by group
# Create binned variables for rent
person_data <- person_data %>%
  mutate(
    rent_group = case_when(
      !is_hh_head | is.na(RENT) | RENT == 0 | RENT >= 9998 ~ NA_character_,
      RENT <= 5 ~ "rent_group_1",
      RENT <= 6 ~ "rent_group_2",
      RENT <= 9 ~ "rent_group_3",
      RENT <= 14 ~ "rent_group_4",
      RENT <= 19 ~ "rent_group_5",
      RENT <= 24 ~ "rent_group_6",
      RENT <= 29 ~ "rent_group_7",
      RENT <= 39 ~ "rent_group_8",
      RENT <= 49 ~ "rent_group_9",
      RENT <= 59 ~ "rent_group_10",
      RENT <= 74 ~ "rent_group_11",
      RENT <= 99 ~ "rent_group_12",
      RENT <= 149 ~ "rent_group_13",
      RENT <= 199 ~ "rent_group_14",
      TRUE ~ "rent_group_15"
    ),
    valueh_group = case_when(
      !is_hh_head | is.na(VALUEH) | VALUEH == 0 | VALUEH >= 9999998 ~ NA_character_,
      VALUEH <= 500 ~ "valueh_group_1",
      VALUEH <= 699 ~ "valueh_group_2",
      VALUEH <= 999 ~ "valueh_group_3",
      VALUEH <= 1499 ~ "valueh_group_4",
      VALUEH <= 1999 ~ "valueh_group_5",
      VALUEH <= 2499 ~ "valueh_group_6",
      VALUEH <= 2999 ~ "valueh_group_7",
      VALUEH <= 3999 ~ "valueh_group_8",
      VALUEH <= 4999 ~ "valueh_group_9",
      VALUEH <= 5999 ~ "valueh_group_10",
      VALUEH <= 7499 ~ "valueh_group_11",
      VALUEH <= 9999 ~ "valueh_group_12",
      VALUEH <= 14999 ~ "valueh_group_13",
      VALUEH <= 19999 ~ "valueh_group_14",
      TRUE ~ "valueh_group_15"
    ),
    income_group = case_when(
      !has_wage_income ~ NA_character_,
      INCWAGE < 500 ~ "income_group_1",
      INCWAGE < 1000 ~ "income_group_2",
      INCWAGE < 1500 ~ "income_group_3",
      INCWAGE < 2000 ~ "income_group_4",
      INCWAGE < 2500 ~ "income_group_5",
      INCWAGE < 3000 ~ "income_group_6",
      INCWAGE < 3500 ~ "income_group_7",
      INCWAGE < 4000 ~ "income_group_8",
      INCWAGE < 4500 ~ "income_group_9",
      INCWAGE < 5000 ~ "income_group_10",
      INCWAGE < 6000 ~ "income_group_11",
      INCWAGE < 7000 ~ "income_group_12",
      INCWAGE < 10000 ~ "income_group_13",
      TRUE ~ "income_group_14"
    )
  )

message("\nStep 9a: Aggregating geocoded assignments directly to tracts...")

# For geocoded method: aggregate directly to tract
tract_data_geocoded <- person_data %>%
  filter(assignment_method == "geocoded") %>%
  group_by(GISJOIN_1950 = GISJOIN_geocoded) %>%
  summarise(
    white_pop = sum(white, na.rm = TRUE),
    black_pop = sum(black, na.rm = TRUE),
    other_pop = sum(other, na.rm = TRUE),
    employed = sum(employed, na.rm = TRUE),
    unemployed = sum(unemployed, na.rm = TRUE),
    not_in_lf = sum(not_in_lf, na.rm = TRUE),
    hs_grad_pop = sum(hs_grad, na.rm = TRUE),
    some_college_pop = sum(some_college, na.rm = TRUE),
    total_educ_pop = sum(age_25_plus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(method = "geocoded")

# Aggregate binned housing/income variables
rent_groups_geocoded <- person_data %>%
  filter(assignment_method == "geocoded", !is.na(rent_group)) %>%
  count(GISJOIN_1950 = GISJOIN_geocoded, rent_group) %>%
  pivot_wider(names_from = rent_group, values_from = n, values_fill = 0)

valueh_groups_geocoded <- person_data %>%
  filter(assignment_method == "geocoded", !is.na(valueh_group)) %>%
  count(GISJOIN_1950 = GISJOIN_geocoded, valueh_group) %>%
  pivot_wider(names_from = valueh_group, values_from = n, values_fill = 0)

income_groups_geocoded <- person_data %>%
  filter(assignment_method == "geocoded", !is.na(income_group)) %>%
  count(GISJOIN_1950 = GISJOIN_geocoded, income_group) %>%
  pivot_wider(names_from = income_group, values_from = n, values_fill = 0)

# Combine
tract_data_geocoded <- tract_data_geocoded %>%
  left_join(rent_groups_geocoded, by = "GISJOIN_1950") %>%
  left_join(valueh_groups_geocoded, by = "GISJOIN_1950") %>%
  left_join(income_groups_geocoded, by = "GISJOIN_1950")

message(sprintf("Geocoded method: %d tracts with data", nrow(tract_data_geocoded)))

message("\nStep 9b: Aggregating ED area-weight assignments via crosswalk...")

# For ED area weight method: aggregate to ED first, then apply crosswalk
ed_data_area_weight <- person_data %>%
  filter(assignment_method == "ed_area_weight") %>%
  group_by(city, state, b_ed) %>%
  summarise(
    white_pop = sum(white, na.rm = TRUE),
    black_pop = sum(black, na.rm = TRUE),
    other_pop = sum(other, na.rm = TRUE),
    employed = sum(employed, na.rm = TRUE),
    unemployed = sum(unemployed, na.rm = TRUE),
    not_in_lf = sum(not_in_lf, na.rm = TRUE),
    hs_grad_pop = sum(hs_grad, na.rm = TRUE),
    some_college_pop = sum(some_college, na.rm = TRUE),
    total_educ_pop = sum(age_25_plus, na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate binned variables by ED
rent_groups_ed <- person_data %>%
  filter(assignment_method == "ed_area_weight", !is.na(rent_group)) %>%
  count(city, state, b_ed, rent_group) %>%
  pivot_wider(names_from = rent_group, values_from = n, values_fill = 0)

valueh_groups_ed <- person_data %>%
  filter(assignment_method == "ed_area_weight", !is.na(valueh_group)) %>%
  count(city, state, b_ed, valueh_group) %>%
  pivot_wider(names_from = valueh_group, values_from = n, values_fill = 0)

income_groups_ed <- person_data %>%
  filter(assignment_method == "ed_area_weight", !is.na(income_group)) %>%
  count(city, state, b_ed, income_group) %>%
  pivot_wider(names_from = income_group, values_from = n, values_fill = 0)

# Combine ED aggregates
ed_data_area_weight <- ed_data_area_weight %>%
  left_join(rent_groups_ed, by = c("city", "state", "b_ed")) %>%
  left_join(valueh_groups_ed, by = c("city", "state", "b_ed")) %>%
  left_join(income_groups_ed, by = c("city", "state", "b_ed"))

# Apply area-weighted crosswalk
ed_crosswalk_clean <- ed_tract_crosswalk_1940 %>%
  mutate(ed_1940 = tolower(ed_1940))

tract_data_area_weight <- ed_data_area_weight %>%
  left_join(
    ed_crosswalk_clean %>% select(city, state, b_ed = ed_1940, GISJOIN_1950, weight),
    by = c("city", "state", "b_ed")
  ) %>%
  # Apply weights to all numeric columns
  mutate(across(where(is.numeric) & !matches("weight"), ~ . * weight)) %>%
  group_by(GISJOIN_1950) %>%
  summarise(across(where(is.numeric) & !matches("weight"), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(method = "ed_area_weight")

message(sprintf("ED area-weight method: %d tracts with data", nrow(tract_data_area_weight)))

message("\nStep 10: Combining hybrid results...")

# Combine both methods
tract_data_hybrid <- bind_rows(
  tract_data_geocoded,
  tract_data_area_weight
) %>%
  group_by(GISJOIN_1950) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    methods_used = paste(unique(method), collapse = "+"),
    .groups = "drop"
  )

message(sprintf("Combined hybrid dataset: %d tracts total", nrow(tract_data_hybrid)))

message("\nStep 11: Calculating medians from binned data...")

# Calculate medians using same logic as collapse_ed_to_tract_data.R
calculate_median_from_census <- function(data, var_code, var_name) {
  data %>%
    pivot_longer(cols = starts_with(var_code), names_to = "range", values_to = "num_in_sample") %>%
    mutate(
      range = gsub(var_code, "", range),
      range = as.numeric(range)
    ) %>%
    group_by(GISJOIN_1950) %>%
    mutate(
      cumulative_freq = cumsum(num_in_sample),
      total = sum(num_in_sample),
      median_position = total / 2
    ) %>%
    filter(cumulative_freq >= median_position) %>%
    slice(1) %>%
    mutate(median = paste0(var_code, range)) %>%
    select(GISJOIN_1950, median) %>%
    dplyr::rename(!!var_name := median)
}

median_rent_1940 <- calculate_median_from_census(
  tract_data_hybrid,
  var_code = "rent_group_",
  var_name = "median_rent_group"
)

median_home_value_1940 <- calculate_median_from_census(
  tract_data_hybrid,
  var_code = "valueh_group_",
  var_name = "median_home_value_group"
)

median_income_1940 <- calculate_median_from_census(
  tract_data_hybrid,
  var_code = "income_group_",
  var_name = "median_income_group"
)

# Join medians and convert to dollar values
tract_data_final <- tract_data_hybrid %>%
  select(-starts_with("rent_group_"), -starts_with("valueh_group_"), -starts_with("income_group_")) %>%
  left_join(median_rent_1940, by = "GISJOIN_1950") %>%
  left_join(median_home_value_1940, by = "GISJOIN_1950") %>%
  left_join(median_income_1940, by = "GISJOIN_1950") %>%
  mutate(
    median_home_value_calculated = case_when(
      median_home_value_group == "valueh_group_1" ~ 250,
      median_home_value_group == "valueh_group_2" ~ 600,
      median_home_value_group == "valueh_group_3" ~ 850,
      median_home_value_group == "valueh_group_4" ~ 1250,
      median_home_value_group == "valueh_group_5" ~ 1750,
      median_home_value_group == "valueh_group_6" ~ 2250,
      median_home_value_group == "valueh_group_7" ~ 2750,
      median_home_value_group == "valueh_group_8" ~ 3500,
      median_home_value_group == "valueh_group_9" ~ 4500,
      median_home_value_group == "valueh_group_10" ~ 5500,
      median_home_value_group == "valueh_group_11" ~ 6750,
      median_home_value_group == "valueh_group_12" ~ 8750,
      median_home_value_group == "valueh_group_13" ~ 12500,
      median_home_value_group == "valueh_group_14" ~ 17500,
      median_home_value_group == "valueh_group_15" ~ 20000
    ),
    median_rent_calculated = case_when(
      median_rent_group == "rent_group_1" ~ 2.5,
      median_rent_group == "rent_group_2" ~ 5.5,
      median_rent_group == "rent_group_3" ~ 7.5,
      median_rent_group == "rent_group_4" ~ 11.5,
      median_rent_group == "rent_group_5" ~ 16.5,
      median_rent_group == "rent_group_6" ~ 21.5,
      median_rent_group == "rent_group_7" ~ 26.5,
      median_rent_group == "rent_group_8" ~ 34,
      median_rent_group == "rent_group_9" ~ 44,
      median_rent_group == "rent_group_10" ~ 54,
      median_rent_group == "rent_group_11" ~ 66.5,
      median_rent_group == "rent_group_12" ~ 87.5,
      median_rent_group == "rent_group_13" ~ 124,
      median_rent_group == "rent_group_14" ~ 174,
      median_rent_group == "rent_group_15" ~ 200
    ),
    median_income = case_when(
      median_income_group == "income_group_1" ~ 250,
      median_income_group == "income_group_2" ~ 750,
      median_income_group == "income_group_3" ~ 1250,
      median_income_group == "income_group_4" ~ 1750,
      median_income_group == "income_group_5" ~ 2250,
      median_income_group == "income_group_6" ~ 2750,
      median_income_group == "income_group_7" ~ 3250,
      median_income_group == "income_group_8" ~ 3750,
      median_income_group == "income_group_9" ~ 4250,
      median_income_group == "income_group_10" ~ 4750,
      median_income_group == "income_group_11" ~ 5500,
      median_income_group == "income_group_12" ~ 6500,
      median_income_group == "income_group_13" ~ 8500,
      median_income_group == "income_group_14" ~ 15000
    )
  ) %>%
  mutate(YEAR = 1940)

message("\nStep 12: Adding tract metadata and calculating shares...")

# Load tract boundaries with metadata
tract_info_1950 <- ipums_shape_full_join(
  read_nhgis(here("data/raw/nhgis/tables/1950/nhgis0027_ds82_1950_tract.csv")),
  tract_1950_sf,
  by = "GISJOIN"
) %>%
  select(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, TRACTA, geometry)

# Load CBD indicator
cbd_tracts_1950 <- read_csv(here(census_data_dir, "cbd_tracts_1950_concorded.csv"))

# Merge and calculate final variables
tract_data_final <- tract_data_final %>%
  left_join(tract_info_1950, by = c("GISJOIN_1950" = "GISJOIN")) %>%
  left_join(cbd_tracts_1950 %>% select(GISJOIN_1950, cbd), by = "GISJOIN_1950") %>%
  mutate(
    cbd = ifelse(is.na(cbd), 0, cbd),
    area_m2 = st_area(geometry),
    total_pop = white_pop + black_pop + other_pop,
    white_share = white_pop / total_pop,
    black_share = black_pop / total_pop,
    other_share = other_pop / total_pop,
    pct_hs_grad = hs_grad_pop / total_educ_pop,
    pct_some_college = some_college_pop / total_educ_pop,
    population_density = total_pop / as.numeric(area_m2),
    unemp_rate = unemployed / (employed + unemployed),
    lfp_rate = (employed + unemployed) / (employed + unemployed + not_in_lf)
  ) %>%
  st_as_sf()

message("\nStep 13: Saving outputs...")

# Save main dataset
output_file <- here(output_dir, "tract_data_1940_geocoded_hybrid.gpkg")
st_write(tract_data_final, output_file, append = FALSE, driver = "GPKG")
message(sprintf("Saved main dataset: %s", output_file))

# Save CSV version without geometry
tract_data_final %>%
  st_drop_geometry() %>%
  write_csv(here(output_dir, "tract_data_1940_geocoded_hybrid.csv"))

message("\nStep 14: Creating comparison with ED-based method...")

# Load existing ED-based results for comparison
tract_data_ed_based <- read_sf(here(output_dir, "tract_data_concorded_from_ed_1930_1940.gpkg")) %>%
  filter(YEAR == 1940)

# Compare key variables
comparison_data <- tract_data_final %>%
  st_drop_geometry() %>%
  select(GISJOIN_1950,
         black_share_hybrid = black_share,
         median_income_hybrid = median_income,
         median_rent_hybrid = median_rent_calculated,
         total_pop_hybrid = total_pop,
         methods_used) %>%
  inner_join(
    tract_data_ed_based %>%
      st_drop_geometry() %>%
      select(GISJOIN_1950,
             black_share_ed = black_share,
             median_income_ed = median_income,
             median_rent_ed = median_rent_calculated,
             total_pop_ed = total_pop),
    by = "GISJOIN_1950"
  ) %>%
  mutate(
    black_share_diff = black_share_hybrid - black_share_ed,
    median_income_diff = median_income_hybrid - median_income_ed,
    median_rent_diff = median_rent_hybrid - median_rent_ed,
    total_pop_diff = total_pop_hybrid - total_pop_ed,
    black_share_pct_diff = 100 * black_share_diff / (black_share_ed + 0.001),
    median_income_pct_diff = 100 * median_income_diff / (median_income_ed + 1),
    median_rent_pct_diff = 100 * median_rent_diff / (median_rent_ed + 1)
  )

# Summary statistics
comparison_summary <- comparison_data %>%
  summarise(
    n_tracts = n(),
    mean_black_share_diff = mean(black_share_diff, na.rm = TRUE),
    median_black_share_diff = median(black_share_diff, na.rm = TRUE),
    mean_income_pct_diff = mean(median_income_pct_diff, na.rm = TRUE),
    median_income_pct_diff = median(median_income_pct_diff, na.rm = TRUE),
    mean_rent_pct_diff = mean(median_rent_pct_diff, na.rm = TRUE),
    median_rent_pct_diff = median(median_rent_pct_diff, na.rm = TRUE),
    cor_black_share = cor(black_share_hybrid, black_share_ed, use = "complete.obs"),
    cor_median_income = cor(median_income_hybrid, median_income_ed, use = "complete.obs"),
    cor_median_rent = cor(median_rent_hybrid, median_rent_ed, use = "complete.obs")
  )

message("\n=== COMPARISON SUMMARY: Hybrid vs ED-based ===")
message(sprintf("Number of tracts compared: %d", comparison_summary$n_tracts))
message(sprintf("\nBlack share:"))
message(sprintf("  Mean difference: %.4f", comparison_summary$mean_black_share_diff))
message(sprintf("  Median difference: %.4f", comparison_summary$median_black_share_diff))
message(sprintf("  Correlation: %.4f", comparison_summary$cor_black_share))
message(sprintf("\nMedian income:"))
message(sprintf("  Mean %% difference: %.2f%%", comparison_summary$mean_income_pct_diff))
message(sprintf("  Median %% difference: %.2f%%", comparison_summary$median_income_pct_diff))
message(sprintf("  Correlation: %.4f", comparison_summary$cor_median_income))
message(sprintf("\nMedian rent:"))
message(sprintf("  Mean %% difference: %.2f%%", comparison_summary$mean_rent_pct_diff))
message(sprintf("  Median %% difference: %.2f%%", comparison_summary$median_rent_pct_diff))
message(sprintf("  Correlation: %.4f", comparison_summary$cor_median_rent))

# Save comparison data
write_csv(comparison_data, here(output_dir, "diagnostics", "hybrid_vs_ed_comparison_1940.csv"))
write_csv(comparison_summary, here(output_dir, "diagnostics", "hybrid_vs_ed_summary_1940.csv"))

message("\n=== SCRIPT COMPLETE ===")
message(sprintf("Main output: %s", output_file))
message(sprintf("Diagnostics saved to: %s", here(output_dir, "diagnostics")))
