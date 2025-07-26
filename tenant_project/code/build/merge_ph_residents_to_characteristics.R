###
#
####

library(tidyverse)
library(data.table)
library(ipumsr)
library(here)


# directories 
ph_residents_dir <- here("tenant_project", "data", "derived", "public_housing_residents")
ddi_file_path_1940 <- here("data", "raw", "ipums", "full_count", "usa_00033.xml")


# Read in 1940 tenants -----
ph_tenants_1940 <- read_csv(here(ph_residents_dir, "nyc_and_chicago_ph_residents_1940.csv"))

ph_tenants_histids <- 
  ph_tenants_1940 %>% 
  pull(histid)

# Convert to fast lookup table
histid_lookup <- data.table(HISTID = ph_tenants_histids)
setkey(histid_lookup, HISTID)


# Define callback function for filtering HISTIDs
cb_function_filter_histids <- function(x, pos) {
  setDT(x)
  x <- x[HISTID %in% histid_lookup$HISTID]  # filter to just the HISTIDs you want
  return(x)
}

# Set up the callback object
cb_histid_filtered <- IpumsDataFrameCallback$new(cb_function_filter_histids)


# Read in 1940 DDI file
ddi_1940 <- read_ipums_ddi(here(ddi_file_path_1940))

# Read ion microdata in chunks and filter
filtered_1940_data <- read_ipums_micro_chunked(
  ddi_1940,
  callback = cb_histid_filtered,
  chunk_size = 500000  # adjust as needed
)

# merge on PH characteristics
ph_tenants_1940 <- ph_tenants_1940 %>%
  left_join(filtered_1940_data, by = c("histid" =  "HISTID"))

# Output 
write_csv(ph_tenants_1940, here(ph_residents_dir, "nyc_and_chicago_ph_residents_1940_with_characteristics.csv"))


