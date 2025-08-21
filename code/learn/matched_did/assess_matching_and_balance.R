###
# Assess matching
###
library(tidyverse)
library(modelsummary)
library(tableone)
library(here)

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small", or "combined"
data_type <- "combined"

set.seed(123456L)

# define directories
merged_data_dir <- here("data", "derived", "merged", data_type)
match_data_dir <- here("data", "derived", "merged", data_type, "matched_dataset")

balance_table_dir <- here("output", "balance_tables", "matched_did", data_type)

map_dir <- here("output", "figures", "matched_did", data_type, "with_rings")

# Read in 2 year matched data
tract_data_matched_2_year <- read_csv(here(match_data_dir, "tract_data_matched_2_year.csv"))

# read in census sample, for maps
tract_with_treatment_status_filepath <-
  here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")

census_tract_sample_all <-
  st_read(tract_with_treatment_status_filepath)

census_tract_data_raw <-
  st_read(here(merged_data_dir, "census_tract_data_raw.gpkg"))
  


# Produce matching tables -----

## Balance on pre-treatment covariate
pre_treatment_data <-
  tract_data_matched_2_year %>% 
  group_by(match_group) %>%
  mutate(group_treatment_year = min(matched_treatment_year, na.rm = TRUE)) %>%
  filter(year == group_treatment_year - 10) %>%
  ungroup()

covariates <- c("total_pop", "black_pop",
                 "white_pop","black_share", "redlined_binary_80pp", "median_income",
                "median_home_value_calculated", "median_rent_calculated","median_housing_age", 
                "distance_from_cbd", "population_density", 
                "housing_density","total_units",  "pct_hs_grad")  


# Create tables for each group_type, output 
group_types <- c("treated", "inner")
for (group in group_types ) {
  # cat(paste("Processing group:", group, "in dataset", dataset_name, "\n"))
  
  data <- 
    pre_treatment_data %>% 
    filter(group_type == group) %>% 
    dplyr::select(all_of(covariates), location_type) %>% 
    dplyr::rename(`Total Pop` = total_pop,
                  `Black Pop` = black_pop,
                  `White Pop` = white_pop,
                  `Black Share` = black_share,
                  `Redlined` = redlined_binary_80pp,
                  `Median Income` = median_income,
                  `Median Home Value` = median_home_value_calculated,
                  `Median Rent` = median_rent_calculated,
                  # `Median Housing Age` = median_housing_age,
                  `Distance from CBD` = distance_from_cbd,
                  `Population Density` = population_density,
                  `Housing Density` = housing_density,
                  `Total Units` = total_units,
                  `Pct HS Grad` = pct_hs_grad) %>% 
    mutate(location_type = case_when(location_type == "treated" ~ "Treated",
                                    location_type == "inner" ~ "Inner Ring",
                                    TRUE ~ "Donor Pool"))

  datasummary_balance(~location_type,
                      data = data,
                      stars= TRUE,
                      fmt = fmt_decimal(digit = 2, pdigits = 3),
                      dinm_statistic = "p.value",
                      output = here(balance_table_dir, paste0("balance_table_", group, ".tex")))
}

# Create map of chicago for visualization ---- 

# # # Create maps of treated and control tracts for each city-----
# # # get all tracts in our data with their status
# 
# # Switch from 1990 to just all unique tracts
all_tracts_with_status <-
  census_tract_sample_all %>%
  filter(YEAR == 2000) %>%
  left_join(tract_data_matched_2_year %>%
              filter(year == 2000) %>%
              dplyr::select(STATE, COUNTY, TRACTA, location_type, weights, matched_treatment_year, group_type),
            by = c("STATE", "COUNTY", "TRACTA")) %>%
  group_by(STATE, COUNTY, TRACTA) %>%
  # create combined_group_type equal to a combined string of the unique group types for donor_pool tracts
  # this tells us which groups the donor_pool tract serves as a control for (which can be multiple)
  # something about this isn't working
  mutate(
    combined_group_type = case_when(
      any(location_type == "donor_pool") ~ paste(sort(unique(group_type[group_type != ""])), collapse = "-"),
      TRUE ~ NA_character_
    ),
    final_type = case_when(
      is.na(location_type) | (location_type == "donor_pool" & (is.na(combined_group_type) | combined_group_type == "")) ~ "unmatched",
      location_type != "donor_pool" ~ location_type,
      TRUE ~ combined_group_type
    )
  ) %>%
  ungroup() %>%
  distinct(STATE, COUNTY, TRACTA, .keep_all = TRUE) %>%
  # if tract is donor pool, set final type = donor_pool + final_type
  mutate(final_type = case_when(
    location_type == "donor_pool" ~ paste("donor_pool", final_type, sep = "_"),
    TRUE ~ final_type
  ))

cities <- unique(all_tracts_with_status$cbsa_title)

create_city_map <- function(data, cbsa_name) {
  # Filter data for the specific city
  city_data <- data %>%
    filter(cbsa_title == cbsa_name)

  # Function to mix colors
  mix_colors <- function(colors) {
    col_matrix <- col2rgb(colors)
    mixed_col <- rgb(
      red = mean(col_matrix["red", ]),
      green = mean(col_matrix["green", ]),
      blue = mean(col_matrix["blue", ]),
      maxColorValue = 255
    )
    return(mixed_col)
  }

  # Define color palette with more distinct base colors
  color_palette <- c(
    "treated" = "darkblue",
    "inner" = "darkgreen",
    "donor_pool_treated" = "#1E90FF",
    "donor_pool_inner" = "lightgreen",
    "donor_pool_inner-treated" = "turquoise",
    "unmatched" = "lightgray"
  )

  color_labels <- c(
    "treated" = "Treated",
    "inner" = "Nearby",
    "donor_pool_treated" = "Control: Treated",
    "donor_pool_inner" = "Control: Nearby",
    "donor_pool_inner-treated" = "Control: Treated + Nearby",
    "unmatched" = "Not included"
  )


  # Create the map
  ggplot() +
    geom_sf(data = city_data, aes(fill = final_type), color = "black", size = 0.1) +
    scale_fill_manual(values = color_palette,
                      name = "Tract Type",
                      labels = color_labels,
                      drop = FALSE) +
  labs(title = paste("Matching results in", cbsa_name)) + 
  theme_void() +  # Removes gridlines, background for a cleaner look
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),  # Light gray background
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 12, hjust = 0.5, color = "gray50"),
      legend.position = "right",  # Move legend to the right
      legend.background = element_rect(fill = "white", color = "black"),
      legend.key = element_rect(color = "black", size = 0.2),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold"),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    
    guides(fill = guide_legend(ncol = 1))  # Keep the    ggtitle(paste("Tract types in", cbsa_name))
}

# Create maps for each city
for (city in cities) {
  map <- create_city_map(all_tracts_with_status, city)

  ggsave(filename = here(map_dir, paste0(city,"_ring_type_map.pdf")),
         plot = map, width = 8, height = 6, dpi = 400, bg = "white")

  print(paste("Map created for", city))
}

