## Exploratory
library(sf)

# Combined sample
ph_sample_combined <- read_sf("data/derived/public_housing/working/public_housing_sample_combined.gpkg")
ph_sample_cdd_large <- read_sf("data/derived/public_housing/working/public_housing_sample_cdd_large.gpkg")


sum(ph_sample_combined$total_public_housing_units)
sum(ph_sample_cdd_large$total_public_housing_units)
