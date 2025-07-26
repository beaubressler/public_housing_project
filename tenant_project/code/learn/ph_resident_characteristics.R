### 
#
###

library(tidyverse)
library(here)
library(data.table)
library(modelsummary)

ph_residents_dir <- here("tenant_project", "data", "derived", "public_housing_residents")

# Load your dataset
ph_tenants_1940 <- read_csv(here(ph_residents_dir, "nyc_and_chicago_ph_residents_1940_with_characteristics.csv"))

# Ensure correct types
ph_tenants_1940 <- ph_tenants_1940 %>%
  mutate(
    project = as.factor(project),
    employed = if_else(EMPSTAT == 1, 1, 0),
    unemployed = if_else(EMPSTAT == 2, 1, 0),
    in_labor_force = if_else(EMPSTAT %in% c(1, 2), 1, 0),
    hs_or_more = if_else(EDUC >= 6, 1, 0),   # IPUMS: 6 = high school
    some_college_plus = if_else(EDUC >= 7, 1, 0),
    race_black = if_else(RACE == 2, 1, 0),
    race_white = if_else(RACE == 1, 1, 0),
    age_group = cut(AGE, breaks = c(0, 17, 24, 44, 64, Inf),
                    labels = c("0-17", "18-24", "25-44", "45-64", "65+"), right = FALSE)
  )

# Basic unweighted summary — overall
datasummary(
  AGE + employed + unemployed + in_labor_force +
    hs_or_more + some_college_plus +
    race_white + race_black ~ mean + sd,
  data = ph_tenants_1940,
  title = "Summary Statistics: 1940 Public Housing Residents (Overall)",
 # output = "markdown",
  fmt = 2,
  statistics = c("Mean = %.2f", "SD = (%.2f)")
)


# Summary by project
datasummary(
  AGE + employed + unemployed + in_labor_force +
    hs_or_more + some_college_plus +
    race_white + race_black ~ project * (mean + sd),
  data = ph_tenants_1940,
  title = "Summary Statistics: 1940 Public Housing Residents (Overall)",
  #output = "markdown",
  fmt = 2,
  statistics = c("Mean = %.2f", "SD = (%.2f)")
)

# Optional: Summary of age groups by project
age_dist <- ph_tenants_1940 %>%
  group_by(project, age_group) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(project) %>%
  mutate(share = count / sum(count))

print(age_dist) %>% View()


## Link to censoc data
censoc_dir <- here("tenant_project", "data", "raw", "censoc")

censoc_data_numident <- read_csv(here(censoc_dir, "censoc_numident_v3.csv"))
censoc_data_numident_geo <- read_csv(here(censoc_dir, "censoc_numident_geography_supplement_v1.csv"))

censoc_data_dmf <- read_csv(here(censoc_dir, "censoc_dmf_v4.csv"))

## Merge onto my sample
ph_tenants_1940_merged_numident <- 
  ph_tenants_1940 %>%
  left_join(censoc_data_numident, by = c("histid" = "HISTID")) %>% 
  filter(!is.na(bmonth))

ph_tenants_1940_merged_numident_geo <- 
  ph_tenants_1940 %>%
  left_join(censoc_data_numident_geo, by = c("histid" = "HISTID")) %>% 
  filter(!is.na(death_zip))


ph_tenants_1940_merged_dmf <-
  ph_tenants_1940 %>%
  left_join(censoc_data_dmf, by = c("histid" = "HISTID")) %>% 
  filter(!is.na(bmonth))


# all hist ids
all_linked_histids <- 
  bind_rows(ph_tenants_1940_merged_numident %>% select(histid), 
          ph_tenants_1940_merged_dmf %>% select(histid)) %>%
  distinct()


hist(ph_tenants_1940_merged$death_age)
hist(ph_tenants_1940_merged_dmf$death_age)

# match to links -----


## Match to 1940-1950 -----
ipums_dir <- here("tenant_project", "data", "raw", "ipums")

df_preview <- fread(here(ipums_dir, "mlp_1940_1950_v1.2.csv"), nrows = 100000)

## Read in in chunks
ph_histids_1940 <- ph_tenants_1940$histid

# Create an empty list or file to collect results
filtered_chunks <- list()

# Define the filtering function
process_chunk_ipums <- function(df, pos) {
  # Filter: keep only rows where column "state" is "CA"
  filtered <- df %>% filter(histid_1940 %in% ph_histids_1940)
  
  # Append to list (or write to a new CSV inside this function)
  filtered_chunks[[length(filtered_chunks) + 1]] <<- filtered
}

# Read the CSV in chunks
read_csv_chunked(
  file =here(ipums_dir, "mlp_1940_1950_v1.2.csv"),
  callback = DataFrameCallback$new(process_chunk_ipums),
  chunk_size = 100000
)

# Combine all filtered chunks into one data frame
final_filtered_data <- bind_rows(filtered_chunks)

#mlp_1940_1950 <- read_csv(here(ipums_dir, "mlp_1940_1950_v1.2.csv"))

## Match to census tree: 1930-1940 Census tree----
census_tree_dir <- here("tenant_project", "data", "raw", "census_tree")


# read in chunks
census_tree_data_sample <- fread(here(census_tree_dir, "1930_1940.csv"), nrows = 10000)


filtered_chunks_census_tree  <- list()

process_chunk_census_tree <- function(df, pos) {
  # Filter: keep only rows where column "state" is "CA"
  filtered <- df %>% filter(histid1940 %in% ph_histids_1940)
  
  # Append to list (or write to a new CSV inside this function)
  filtered_chunks_census_tree[[length(filtered_chunks_census_tree) + 1]] <<- filtered
}

# read the csv in chunks
read_csv_chunked(
  file =here(census_tree_dir, "1930_1940.csv"),
  callback = DataFrameCallback$new(process_chunk_census_tree),
  chunk_size = 100000
)

# Combine all filtered chunks into one data frame
final_filtered_data_census_tree <- bind_rows(filtered_chunks_census_tree)

## Match to 

