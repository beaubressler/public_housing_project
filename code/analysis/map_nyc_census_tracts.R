####
# Map NYC Census Tracts by Year
# This script creates maps of New York City census tracts for different years
# to visualize how tract boundaries changed over time
####

library(sf)
library(ggplot2)
library(dplyr)
library(here)
library(patchwork)

# Function to create NYC tract map for a given year
map_nyc_tracts <- function(year, save_plot = FALSE) {
  
  # Construct file path
  shapefile_path <- here("data", "raw", "nhgis", "gis", 
                        paste0("nhgis0027_shapefile_tl2000_us_tract_", year),
                        paste0("US_tract_", year, ".shp"))
  
  # Check if file exists
  if (!file.exists(shapefile_path)) {
    stop(paste("Shapefile not found for year", year, "at", shapefile_path))
  }
  
  # Read shapefile
  cat("Reading", year, "census tract shapefile...\n")
  tracts <- st_read(shapefile_path, quiet = TRUE)
  
  # Filter for NYC (New York County codes: 36005, 36047, 36061, 36081, 36085)
  # These correspond to: Bronx, Brooklyn, Manhattan, Queens, Staten Island
  nyc_tracts <- tracts %>%
    filter(NHGISST == "360") %>%  # New York State
    filter(NHGISCTY %in% c("0050", "0470", "0610", "0810", "0850")) %>%  # NYC counties
    st_transform(crs = 3857)  # Web Mercator for better visualization
  
  cat("Found", nrow(nyc_tracts), "tracts in NYC for", year, "\n")
  
  # Create the map
  p <- ggplot(nyc_tracts) +
    geom_sf(fill = "lightblue", color = "white", size = 0.2) +
    theme_void() +
    labs(title = paste("NYC Census Tracts -", year),
         subtitle = paste("Total tracts:", nrow(nyc_tracts))) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
  
  # Calculate tract areas to identify potentially problematic large tracts
  nyc_tracts_with_area <- nyc_tracts %>%
    mutate(area_sq_km = as.numeric(st_area(.)) / 1000000) %>%
    arrange(desc(area_sq_km))
  
  # Print summary statistics
  cat("\nTract area summary for", year, ":\n")
  cat("Mean area:", round(mean(nyc_tracts_with_area$area_sq_km), 2), "sq km\n")
  cat("Median area:", round(median(nyc_tracts_with_area$area_sq_km), 2), "sq km\n")
  cat("Max area:", round(max(nyc_tracts_with_area$area_sq_km), 2), "sq km\n")
  cat("Number of tracts > 5 sq km:", sum(nyc_tracts_with_area$area_sq_km > 5), "\n")
  
  # Show largest tracts (potential problem areas)
  if (any(nyc_tracts_with_area$area_sq_km > 5)) {
    cat("\nLargest tracts (> 5 sq km):\n")
    large_tracts <- nyc_tracts_with_area %>% 
      filter(area_sq_km > 5) %>%
      st_drop_geometry() %>%
      select(GISJOIN, area_sq_km) %>%
      head(10)
    print(large_tracts)
  }
  
  # Save plot if requested
  if (save_plot) {
    output_path <- here("output", "figures", paste0("nyc_tracts_", year, ".png"))
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    ggsave(output_path, p, width = 10, height = 8, dpi = 300)
    cat("Map saved to:", output_path, "\n")
  }
  
  return(list(plot = p, data = nyc_tracts_with_area))
}

# Function to create comparison maps
compare_nyc_tracts <- function(years = c(1940, 1950, 1960, 1970)) {
  
  cat("Creating comparison maps for years:", paste(years, collapse = ", "), "\n\n")
  
  plots <- list()
  summaries <- list()
  
  for (year in years) {
    result <- map_nyc_tracts(year, save_plot = FALSE)
    plots[[as.character(year)]] <- result$plot
    summaries[[as.character(year)]] <- result$data %>%
      st_drop_geometry() %>%
      summarise(
        year = year,
        n_tracts = n(),
        mean_area = round(mean(area_sq_km), 2),
        median_area = round(median(area_sq_km), 2),
        max_area = round(max(area_sq_km), 2),
        n_large_tracts = sum(area_sq_km > 5)
      )
  }
  
  # Combine plots
  if (length(plots) == 2) {
    combined_plot <- plots[[1]] + plots[[2]]
  } else if (length(plots) == 4) {
    combined_plot <- (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]])
  } else {
    combined_plot <- wrap_plots(plots, ncol = 2)
  }
  
  # Save combined plot
  output_path <- here("output", "figures", "nyc_tracts_comparison.png")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(output_path, combined_plot, width = 16, height = 12, dpi = 300)
  cat("Comparison map saved to:", output_path, "\n")
  
  # Create summary table
  summary_df <- bind_rows(summaries)
  print(summary_df)
  
  return(list(plots = plots, summary = summary_df))
}

# Function to highlight potentially problematic tracts
highlight_problem_tracts <- function(year, area_threshold = 5) {
  
  result <- map_nyc_tracts(year, save_plot = FALSE)
  nyc_tracts <- result$data
  
  # Create map highlighting large tracts
  p <- ggplot(nyc_tracts) +
    geom_sf(aes(fill = area_sq_km > area_threshold), color = "white", size = 0.2) +
    scale_fill_manual(values = c("lightblue", "red"), 
                     labels = c(paste("≤", area_threshold, "sq km"), 
                               paste(">", area_threshold, "sq km")),
                     name = "Tract Size") +
    theme_void() +
    labs(title = paste("NYC Census Tracts -", year),
         subtitle = paste("Highlighting tracts >", area_threshold, "sq km (potentially problematic)")) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      legend.position = "bottom"
    )
  
  # Save plot
  output_path <- here("output", "figures", paste0("nyc_tracts_problems_", year, ".png"))
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(output_path, p, width = 10, height = 8, dpi = 300)
  cat("Problem tract map saved to:", output_path, "\n")
  
  return(p)
}

# Main execution
if (interactive()) {
  cat("NYC Census Tract Mapping Tool\n")
  cat("=============================\n\n")
  
  # Example usage:
  # Single year map
  cat("Creating map for 1940...\n")
  map_1940 <- map_nyc_tracts(1940)
  
  cat("\nCreating map for 1950...\n")
  map_1950 <- map_nyc_tracts(1950)
  
  # Comparison maps
  cat("\nCreating comparison maps...\n")
  comparison <- compare_nyc_tracts(c(1940, 1950, 1960, 1970))
  
  # Highlight problematic tracts for 1940
  cat("\nHighlighting potentially problematic tracts for 1940...\n")
  problem_map_1940 <- highlight_problem_tracts(1940, area_threshold = 5)
  
  cat("\nAll maps completed! Check the output/figures/ directory.\n")
}

# Usage examples:
# map_nyc_tracts(1940)  # Map NYC tracts for 1940
# compare_nyc_tracts(c(1940, 1950))  # Compare two years
# highlight_problem_tracts(1940, area_threshold = 5)  # Highlight large tracts