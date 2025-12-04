####
# Collapse geocoded 1940 Full Count addresses directly to 1950 tract level
#
# This script bypasses ED-to-tract area weighting and normalization by using
# geocoded address points to directly assign individuals to census tracts.
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
full_count_dir <- here("data", "raw", "ipums", "full_count")
output_dir <- here(census_data_dir, "full_count", "tract")

# Quality threshold for geocoding
MIN_MATCH_SCORE <- 85

message("=== Geocoded 1940 Census to Tract Aggregation ===\n")

message("Step 1: Loading geocoded address data...")

# Read geocoded addresses with quality filters
geocoded_1940 <- fread(here(geocoded_dir, "grf_unique_addresses_1940_Geocoded_ExportTable.csv")) %>%
  as_tibble() %>%
  filter(
    Status != "T",  # Remove ties
    Score >= MIN_MATCH_SCORE,  # Keep only high-quality matches
    !is.na(X), !is.na(Y)  # Valid coordinates
  ) %>%
  select(full_street_address = USER_full_street_address, X, Y, Score)

message(sprintf("Loaded %d geocoded addresses (after quality filtering)", nrow(geocoded_1940)))

# Convert to spatial points
geocoded_sf <- geocoded_1940 %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%  # WGS84
  st_transform(crs = st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))  # Albers

rm(geocoded_1940)
gc()

message(sprintf("Created spatial points for %d addresses", nrow(geocoded_sf)))

message("\nStep 2: Loading 1950 tract boundaries...")

tract_1950_sf <- read_ipums_sf(
  here("data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1950/US_tract_1950.shp")
) %>%
  st_transform(crs = st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))

message(sprintf("Loaded %d 1950 tracts", nrow(tract_1950_sf)))

message("\nStep 3: Spatial join - assigning addresses to tracts...")

# Use st_intersects for faster point-in-polygon (returns sparse matrix)
intersects_list <- st_intersects(geocoded_sf, tract_1950_sf)

# Convert to data frame (each point gets matched tract GISJOIN)
geocoded_with_tract <- tibble(
  full_street_address = geocoded_sf$full_street_address,
  tract_idx = as.integer(intersects_list)
) %>%
  left_join(
    tibble(tract_idx = 1:nrow(tract_1950_sf),
           GISJOIN = tract_1950_sf$GISJOIN),
    by = "tract_idx"
  ) %>%
  select(full_street_address, GISJOIN)

message(sprintf("Assigned %d addresses to tracts", sum(!is.na(geocoded_with_tract$GISJOIN))))

message("\nStep 4: Loading GRF address-HISTID linkage...")

grf_addresses_1940 <- fread(here(grf_dir, "grf_addresses_1940.csv")) %>%
  as_tibble() %>%
  select(histid, full_street_address)

message(sprintf("Loaded %d HISTID-address links", nrow(grf_addresses_1940)))

message("\nStep 5: Loading full count microdata in chunks...")

ddi_file_path_1940 <- here(full_count_dir, "usa_00033.xml")
ddi_1940 <- read_ipums_ddi(ddi_file_path_1940)
chunk_size_param <- 100000  # Reduced for better progress tracking

# Convert grf_addresses and geocoded_with_tract to data.table for fast merging
setDT(grf_addresses_1940)
setkey(grf_addresses_1940, histid)
setDT(geocoded_with_tract)
setkey(geocoded_with_tract, full_street_address)

# Callback function to process each chunk
cb_function_geocoded_tract <- function(x, pos) {
  message(sprintf("  Processing chunk at position %d (%d rows)...", pos, nrow(x)))
  setDT(x)

  # Merge GRF addresses and geocoded tract assignments
  x <- merge(x, grf_addresses_1940, by.x = "HISTID", by.y = "histid", all.x = TRUE)
  x <- merge(x, geocoded_with_tract, by = "full_street_address", all.x = TRUE)

  # Keep only geocoded records
  x <- x[!is.na(GISJOIN)]

  if (nrow(x) == 0) {
    # Return empty data.table with correct structure
    return(data.table(GISJOIN = character(0)))
  }

  # Create person-level variables
  x[, `:=`(
    white = ifelse(RACE == 1, 1, 0),
    black = ifelse(RACE == 2, 1, 0),
    other = ifelse(!RACE %in% c(1, 2), 1, 0),
    in_labor_force = AGE >= 14,
    age_25_plus = AGE >= 25,
    is_hh_head = RELATE == 1
  )]

  x[in_labor_force == TRUE, `:=`(
    employed = ifelse(EMPSTAT == 1, 1, 0),
    unemployed = ifelse(EMPSTAT == 2, 1, 0),
    not_in_lf = ifelse(EMPSTAT == 3, 1, 0)
  )]

  x[age_25_plus == TRUE, `:=`(
    hs_grad = ifelse(EDUC >= 6, 1, 0),
    some_college = ifelse(EDUC >= 7, 1, 0)
  )]

  # Aggregate population counts by tract
  pop_counts <- x[, .(
    white_pop = sum(white, na.rm = TRUE),
    black_pop = sum(black, na.rm = TRUE),
    other_pop = sum(other, na.rm = TRUE),
    employed = sum(employed, na.rm = TRUE),
    unemployed = sum(unemployed, na.rm = TRUE),
    not_in_lf = sum(not_in_lf, na.rm = TRUE),
    hs_grad_pop = sum(hs_grad, na.rm = TRUE),
    some_college_pop = sum(some_college, na.rm = TRUE),
    total_educ_pop = sum(age_25_plus, na.rm = TRUE)
  ), by = GISJOIN]

  # Rent groups (household heads only)
  rent_data <- x[is_hh_head == TRUE & !is.na(RENT) & RENT > 0 & RENT < 9998]
  if (nrow(rent_data) > 0) {
    rent_data[, rent_group := fcase(
      RENT <= 5, "rent_group_1",
      RENT <= 6, "rent_group_2",
      RENT <= 9, "rent_group_3",
      RENT <= 14, "rent_group_4",
      RENT <= 19, "rent_group_5",
      RENT <= 24, "rent_group_6",
      RENT <= 29, "rent_group_7",
      RENT <= 39, "rent_group_8",
      RENT <= 49, "rent_group_9",
      RENT <= 59, "rent_group_10",
      RENT <= 74, "rent_group_11",
      RENT <= 99, "rent_group_12",
      RENT <= 149, "rent_group_13",
      RENT <= 199, "rent_group_14",
      default = "rent_group_15"
    )]
    rent_counts <- dcast(rent_data[, .N, by = .(GISJOIN, rent_group)],
                         GISJOIN ~ rent_group, value.var = "N", fill = 0)
  } else {
    rent_counts <- data.table(GISJOIN = character())
  }

  # Home value groups
  valueh_data <- x[is_hh_head == TRUE & !is.na(VALUEH) & VALUEH > 0 & VALUEH < 9999998]
  if (nrow(valueh_data) > 0) {
    valueh_data[, valueh_group := fcase(
      VALUEH <= 500, "valueh_group_1",
      VALUEH <= 699, "valueh_group_2",
      VALUEH <= 999, "valueh_group_3",
      VALUEH <= 1499, "valueh_group_4",
      VALUEH <= 1999, "valueh_group_5",
      VALUEH <= 2499, "valueh_group_6",
      VALUEH <= 2999, "valueh_group_7",
      VALUEH <= 3999, "valueh_group_8",
      VALUEH <= 4999, "valueh_group_9",
      VALUEH <= 5999, "valueh_group_10",
      VALUEH <= 7499, "valueh_group_11",
      VALUEH <= 9999, "valueh_group_12",
      VALUEH <= 14999, "valueh_group_13",
      VALUEH <= 19999, "valueh_group_14",
      default = "valueh_group_15"
    )]
    valueh_counts <- dcast(valueh_data[, .N, by = .(GISJOIN, valueh_group)],
                           GISJOIN ~ valueh_group, value.var = "N", fill = 0)
  } else {
    valueh_counts <- data.table(GISJOIN = character())
  }

  # Income groups (aggregate to family level like ED script)
  income_data <- x[INCWAGE != 999998 & INCWAGE != 999999 & INCWAGE != 0]
  if (nrow(income_data) > 0) {
    # Calculate family wage income by summing individual wages within each family
    family_income <- income_data[, .(family_wage = sum(INCWAGE)), by = .(GISJOIN, SERIAL)]

    # Create income groups using cut() - same as ED script
    income_breaks <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 6000, 7000, 10000, Inf)
    family_income[, income_group := cut(family_wage, breaks = income_breaks, labels = 1:14, right = FALSE, include.lowest = TRUE)]
    family_income[, income_group := as.integer(income_group)]
    family_income[, income_group := paste0("income_group_", income_group)]

    income_counts <- dcast(family_income[, .N, by = .(GISJOIN, income_group)],
                           GISJOIN ~ income_group, value.var = "N", fill = 0)
  } else {
    income_counts <- data.table(GISJOIN = character())
  }

  # Merge all together
  result <- pop_counts
  if (nrow(rent_counts) > 0) result <- merge(result, rent_counts, by = "GISJOIN", all = TRUE)
  if (nrow(valueh_counts) > 0) result <- merge(result, valueh_counts, by = "GISJOIN", all = TRUE)
  if (nrow(income_counts) > 0) result <- merge(result, income_counts, by = "GISJOIN", all = TRUE)

  return(result)
}

# Create callback object
cb_geocoded_tract <- IpumsDataFrameCallback$new(cb_function_geocoded_tract)

# Read and process in chunks
chunked_results <- read_ipums_micro_chunked(
  ddi_file_path_1940,
  callback = cb_geocoded_tract,
  chunk_size = chunk_size_param,
  verbose = TRUE
)

message("\nStep 6: Combining chunk results...")

# Combine all chunks and sum across tracts
setDT(chunked_results)
tract_data <- chunked_results[, lapply(.SD, sum, na.rm = TRUE), by = GISJOIN]
setnames(tract_data, "GISJOIN", "GISJOIN_1950")

message(sprintf("Combined to %d tracts total", nrow(tract_data)))

message("\nStep 9: Calculating medians from binned data...")

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

median_rent <- calculate_median_from_census(tract_data, "rent_group_", "median_rent_group")
median_home_value <- calculate_median_from_census(tract_data, "valueh_group_", "median_home_value_group")
median_income <- calculate_median_from_census(tract_data, "income_group_", "median_income_group")

# Join medians and convert to dollar values
tract_data_final <- tract_data %>%
  select(-starts_with("rent_group_"), -starts_with("valueh_group_"), -starts_with("income_group_")) %>%
  left_join(median_rent, by = "GISJOIN_1950") %>%
  left_join(median_home_value, by = "GISJOIN_1950") %>%
  left_join(median_income, by = "GISJOIN_1950") %>%
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
    ),
    YEAR = 1940
  )

message("\nStep 10: Adding tract metadata and calculating shares...")

# Load tract info
tract_info_1950 <- ipums_shape_full_join(
  read_nhgis(here("data/raw/nhgis/tables/1950/nhgis0027_ds82_1950_tract.csv")),
  tract_1950_sf,
  by = "GISJOIN"
) %>%
  select(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, TRACTA, geometry)

# Load CBD indicator
cbd_tracts_1950 <- read_csv(here(census_data_dir, "cbd_tracts_1950_concorded.csv"), show_col_types = FALSE)

# Final dataset
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

message("\nStep 11: Saving outputs...")

# Save spatial version
st_write(tract_data_final,
         here(output_dir, "tract_data_1940_geocoded.gpkg"),
         append = FALSE, driver = "GPKG")

# Save CSV version
tract_data_final %>%
  st_drop_geometry() %>%
  write_csv(here(output_dir, "tract_data_1940_geocoded.csv"))

message(sprintf("\n=== COMPLETE ==="))
message(sprintf("Geocoded tract data saved to:"))
message(sprintf("  - %s", here(output_dir, "tract_data_1940_geocoded.gpkg")))
message(sprintf("  - %s", here(output_dir, "tract_data_1940_geocoded.csv")))
message(sprintf("\nCoverage: %d tracts with geocoded data", nrow(tract_data_final)))
message(sprintf("To compare with ED-based method, load both datasets and regress geocoded on ED-based values"))
