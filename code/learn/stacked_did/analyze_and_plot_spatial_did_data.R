# Analyze spatial Did data
# TODO: Decide whether to do this pre or post balancing

# Preliminaries ----
library(tidyverse)
library(sf)
library(webshot)

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small" or "combined"
data_type <- "combined"


# define directories
merged_data_dir <- here("data", "derived", "merged", data_type)
map_output_dir <- here("output", "figures", "stacked_did", data_type)
balance_table_dir <- here("output", "balance_tables", "stacked_did", data_type)


event_study_rings_output_path <- here(merged_data_dir, "event_study_data_rings.csv")
unique_tracts_and_rings_output_path <- here(merged_data_dir, "unique_tracts_and_rings.csv")
census_tract_sample_path <- here(merged_data_dir, "census_tract_sample_with_treatment_status_all.gpkg")


# Read in data 
event_study_rings <- read_csv(event_study_rings_output_path)
unique_tracts_and_rings <- read_csv(unique_tracts_and_rings_output_path)

census_tract_sample_indexed_unique <- 
  read_sf(census_tract_sample_path) %>% 
  group_by(TRACTA, COUNTYA, STATEA) %>%
  filter(row_number() == 1) %>% 
  mutate(tract_id = row_number())

  

# Create maps of tracts and rings for each city ----

# merge unique tracts and rings onto 1990 census data
tracts_and_rings_for_maps <-
  left_join(census_tract_sample_indexed_unique, unique_tracts_and_rings) %>%
  select(STATE, COUNTY, TRACTA, location_type, city) %>% 
  # if location type is missing, set to "excluded"
  mutate(location_type = ifelse(is.na(location_type), "excluded", location_type)) %>% 
  st_as_sf()



### Maps ----
# For each city, create a map of tracts and rings

map_list <- list()
cities <- "Chicago"

#for (c in unique(tracts_and_rings_for_maps$city)) {

for (c in cities) {
  
  tracts_and_rings_for_maps_city <- 
    tracts_and_rings_for_maps %>% 
    filter(city == c)
  
  # create a map of tracts and rings
  tracts_and_rings_map <-
    ggplot() +
    geom_sf(data = tracts_and_rings_for_maps_city, aes(fill = location_type), color = "black", size = 0.1) +
    scale_fill_manual(values = c("outer" = "darkblue", "treated" = "white", "inner" = "lightblue", "excluded" = "gray"),
                      labels = c("outer" = "Outer Ring","treated" ="Treated", "inner" = "Inner Ring",
                                 "excluded"=  "Excluded"),
                      name = "Location Type") +
    labs(title = paste0("Tracts and Rings for ", c)) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(0.5, "cm"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10)
    ) +
    coord_sf() +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
  
  # save the map
  # ggsave(paste0(map_output_dir, c, ".png"), tracts_and_rings_map, width = 8, height = 8)
  map_list[[c]] <- tracts_and_rings_map
  
}

# Combine all plots into a single multi-page PDF
pdf(file = here(map_output_dir, "all_cities_maps.pdf"), width = 11, height = 8.5)

# Loop through the map list and print each plot on a new page
for (plot in map_list) {
  print(plot)
}

# Close the PDF device
dev.off()



# Create balance tables for treated, inner, and outer rings ----

# function for creating balance tables
create_balance_tables <- function(data, covariates, output_dir) {
  
  all_tables <- list()
  
  # Create overall balance table
  overall_balance <- CreateTableOne(vars = covariates,
                                    strata = "location_type",
                                    data = data,
                                    test = TRUE)
  

  # Add overall balance table to the list
  all_tables[["Overall"]] <- kable(print(overall_balance, printToggle = FALSE, smd = TRUE), 
                                   format = "html", caption = "Overall Balance") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  
  
  #save_kable(overall_balance_html, file = file.path(output_dir, "overall_balance_table.html"))
  
  # Create balance tables for each city
  cities <- unique(data$city)
  
  for (c in cities) {
    city_data <- data %>% filter(city == c)
    
    city_balance <- CreateTableOne(vars = covariates,
                                   strata = "location_type",
                                   data = city_data,
                                   test = TRUE)
    
    # Convert to kable and save as HTML
    city_balance_html <- kable(print(city_balance, printToggle = FALSE, smd = TRUE), format = "html") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
    
    all_tables[[c]] <- kable(print(city_balance, printToggle = FALSE, smd = TRUE), 
                                format = "html", caption = paste("Balance Table for", c)) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
    # save_kable(city_balance_html, file = file.path(output_dir, paste0(city, "_balance_table.html")))
  }
  # Combine all tables into a single HTML file
  combined_html <- paste(unlist(all_tables), collapse = "<p style='page-break-before: always'>")
  combined_html <- paste0("<html><body>", combined_html, "</body></html>")
  
  # Save the combined HTML
  html_file <- file.path(output_dir, "combined_balance_tables.html")
  writeLines(combined_html, html_file)
  
  # Convert HTML to PDF
  pdf_file <- file.path(output_dir, "combined_balance_tables.pdf")
  webshot(html_file, pdf_file, delay = 1, vwidth = 1000, vheight = 800)
  
  # Remove the temporary HTML file
  file.remove(html_file)
  
  cat("Combined balance tables PDF has been saved to:", pdf_file, "\n")
  
}


# covariates to check
covariates <- c("black_share", "white_share",  "asinh_median_income", "median_income",
                "median_home_value", "asinh_median_home_value", "median_rent","asinh_median_rent", 
                "distance_from_cbd", "population_density", "housing_density", "total_pop", "black_pop",
                "asinh_pop_black", "asinh_pop_white", "asinh_pop_total")  # Add or modify as needed


## get data for balance test: Data in event time = -10
balanced_data <-
  event_study_data_rings %>%
  filter(event_time == -10) %>% 
  filter(white_pop <= total_pop, 
         black_pop <= total_pop,
         white_share <= 1,
         black_share <= 1) %>%
  filter(!is.na(black_share))


balance_table <- CreateTableOne(vars = covariates,
                                strata = "location_type",
                                data = balanced_data,
                                test = TRUE)



# Print the table
print(balance_table, smd = TRUE)

create_balance_tables(balanced_data, covariates, balance_table_dir)

## Interactive mapview for city rings -----
library(mapview)

# Function to create interactive map of rings for a specific city
mapview_city_rings <- function(city_name) {
  
  # Get tracts for the specified city
  city_tracts <- census_tract_sample_indexed_unique %>%
    filter(city == city_name)
  
  # Get ring assignments for this city
  city_rings <- unique_tracts_and_rings %>%
    inner_join(city_tracts %>% st_drop_geometry() %>% select(GISJOIN_1950, city), 
               by = "GISJOIN_1950")
  
  # Merge with tract geometries
  city_rings_with_geom <- city_tracts %>%
    left_join(city_rings, by = "GISJOIN_1950") %>%
    mutate(location_type = ifelse(is.na(location_type), "excluded", location_type))
  
  # Color scheme
  colors <- c("treated" = "#FF0000",     # Red
              "inner" = "#FFA500",       # Orange  
              "outer" = "#87CEEB",       # Sky blue
              "excluded" = "#D3D3D3")    # Light gray
  
  # Create mapview
  map <- mapview(city_rings_with_geom, 
                 zcol = "location_type",
                 col.regions = colors,
                 layer.name = paste("Rings for", city_name))
  
  return(map)
}

# Function to see available cities
list_available_cities <- function() {
  cities <- unique(census_tract_sample_indexed_unique$city)
  cities <- cities[!is.na(cities)]
  return(sort(cities))
}

# Example usage (uncomment to run):
# list_available_cities()
# mapview_city_rings("Chicago")
mapview_city_rings("New York City")
# mapview_city_rings("San Francisco")
