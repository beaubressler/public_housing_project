## matching_config.R
## Shared configuration for all matching specifications
## This ensures consistency between baseline and no_cbsa variants

# Define group types and matching variables -----
group_types <- c("treated", "inner")

# Matching variables based on site selection analysis:
# ORIGINAL: 9/30/2025
# matching_vars <- c(
#   #"asinh_distance_from_cbd",     # Geographic/transportation access
#   #"asinh_pop_total",             # Neighborhood size
#   "total_pop",
#   "black_share",                 # Racial composition
#   "lfp_rate",
#   "unemp_rate",                  # Economic conditions
#   # "asinh_median_income",         # Socioeconomic status
#   "median_income",
#   "median_rent_calculated"
#   #"asinh_median_rent_calculated"      # Housing market conditions
# )


# Testing
matching_vars <- c(
 # "asinh_distance_from_cbd",     # Geographic/transportation access
 #"distance_to_highway_km",
   "asinh_pop_total",             # Neighborhood size
 #  "asinh_pop_black",
  # "asinh_pop_white",
  #"total_pop",
  #"black_pop",
  "black_share",                 # Racial composition
  "lfp_rate",
  "unemp_rate",                  # Economic conditions
  "asinh_median_income",     # Socioeconomic status
  #"median_income",
  #"median_rent_calculated"
  "asinh_median_rent_calculated"      # Housing market conditions
)

# Exact matching variables (differ by variant)
# Note: Redlining removed from exact matching and should be added to matching_vars
# if you want it included in the propensity score
exact_matching_vars_baseline <- c(
  "county_id",
  "ur_binary_5pp"
)

exact_matching_vars_no_cbsa <- c(
  "ur_binary_5pp"
)

# Matching parameters
knn_neighbors <- 1

# Common data settings
data_type <- "combined"
set.seed(123456L)

cat("Matching configuration loaded:\n")
cat("- Matching variables:", paste(matching_vars, collapse = ", "), "\n")
cat("- Group types:", paste(group_types, collapse = ", "), "\n")
cat("- Data type:", data_type, "\n")
cat("- KNN neighbors:", knn_neighbors, "\n")