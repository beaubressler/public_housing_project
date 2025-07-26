##
# Exploratory analysis of public housing data
##

# Preliminaries ----
library(tidyverse)
library(haven)
library(sf)
library(maps)
library(mapdata)
library(tigris)
library(ggrepel)


# dataset choice
# !! Set this yourself
dataset_choice <- "combined"

# directories
merged_data_dir <- here("data", "derived", "merged")
ph_data_dir <- here("data", "derived", "public_housing", "working")
census_data_dir <- here("data", "derived", "census")

## filepaths
balanced_sample_filepath <- 
  here(merged_data_dir, dataset_choice, "census_tract_sample_with_treatment_status_balanced.gpkg")
balanced_treated_tracts_panel_filepath <- 
  here(merged_data_dir, dataset_choice, "treated_tracts_panel_balanced.gpkg")
public_housing_sample_filepath <-
  here(ph_data_dir, dataset_choice, "public_housing_sample_balanced.gpkg")
treated_and_rings_dir <- 
  here(merged_data_dir, dataset_choice, "unique_tracts_and_rings.csv")


# Datasets before balancing
tract_with_treatment_status_filepath <- here(merged_data_dir, dataset_choice,
                                             "census_tract_sample_with_treatment_status_all.gpkg")
treated_tracts_panel_filepath <- here(merged_data_dir, dataset_choice,
                                      "treated_tracts_panel_all.gpkg")
cleaned_projects_filepath <- here(ph_data_dir, "cleaned_housing_projects.gpkg")

# figure output
fig_dir <- here("output", "figures", "exploratory", "public_housing")

# combined public housing data
original_ph_data <- read_csv(here(ph_data_dir, "merged_cdd_projects_non_collapsed.csv"))
original_ph_data_collapsed <- read_csv(here(ph_data_dir, "merged_cdd_projects.csv"))

# read in datasets

full_census_data <- read_sf(tract_with_treatment_status_filepath)
balanced_sample <- read_sf(balanced_sample_filepath)  # 
treated_tracts <- read_sf(balanced_treated_tracts_panel_filepath)
public_housing_sample <- read_sf(public_housing_sample_filepath)
tracts_and_rings <- read_csv(treated_and_rings_dir)


# full census dataset for map
census_tract_data <- read_sf(here(census_data_dir, "tract_population_data.gpkg"))

## Before balancing: Figure out where the ones that are not in my sample are

all_public_housing_projects <- 
  read_sf(cleaned_projects_filepath) %>% 
  st_drop_geometry()

original_treated_tracts <- read_sf(treated_tracts_panel_filepath)

# Question: Why are there 3 SF projects with no units

# number of projects in sample 
nrow(public_housing_sample)

# number of units in sample 
sum(public_housing_sample$total_public_housing_units, na.rm = TRUE)

# get number of unique treated tracts
first_treated_tract <- 
  treated_tracts %>% 
  distinct(GISJOIN_1950, city, cbsa_title,
           total_public_housing_units, treatment_year)

nrow(first_treated_tract)

# Number of units per city 
units_summary <- 
  first_treated_tract %>% 
  group_by(cbsa_title) %>% 
  summarise(total_units = sum(total_public_housing_units, na.rm = TRUE)) %>% 
  arrange(desc(total_units))

units_per_cbsa_plot <-
  ggplot(units_summary, aes(x = reorder(cbsa_title, total_units), y = total_units)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Flip coordinates to make the city names easier to read
  labs(
    title = "Total Public Housing Units per CBSA in sample",
    x = "City",
    y = "Total Public Housing Units"
  ) +
  theme_minimal()


# number of treated tracts per city
tracts_summary <- 
  first_treated_tract %>% 
  group_by(cbsa_title) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Where did the rest of the projects go? ----
# original treated tracts: 1022
first_treated_tracts_original <- 
  original_treated_tracts %>% 
  distinct(GISJOIN_1950, city, total_public_housing_units)


# public housing projects not in my sample
public_housing_projects_not_in_sample <- 
  all_public_housing_projects %>% 
  anti_join(public_housing_sample, by = "project_code")

# how many before 1951
# only 584 projects but 202K units
public_housing_projects_not_in_sample_before_1951 <-
  public_housing_projects_not_in_sample %>% 
  filter(year_completed < 1951)

nrow(public_housing_projects_not_in_sample_before_1951)
sum(public_housing_projects_not_in_sample_before_1951$total_public_housing_units)


## How many from other localities? ----
# get list of cities in my sample
localities_in_my_sample <- 
  public_housing_sample %>% 
  distinct(locality)

# 77 localities in my sample
localities_in_my_sample

# how many in cities that are not in my list of ciites
# 6,084 total localities...
public_housing_projects_not_in_sample_other_localities <- 
  public_housing_projects_not_in_sample %>% 
  anti_join(localities_in_my_sample, by = c("locality" = "locality"))

public_housing_projects_not_in_sample_other_localities %>% 
  distinct(locality)

# how many units in these projects: 542K
sum(public_housing_projects_not_in_sample_other_localities$total_public_housing_units)

# by locality
# Largest: Newawk (why is it missing?), San Antonio, Miami, Tampa
public_housing_projects_not_in_sample_other_localities %>% 
  group_by(locality) %>% 
  summarise(total_units = sum(total_public_housing_units, na.rm = TRUE)) %>% 
  arrange(desc(total_units))

# Produce graphs ----

## Number of units per city ----
units_per_cbsa_plot <- 
  ggplot(units_summary, aes(x = reorder(cbsa_title, total_units), y = total_units)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Flip coordinates to make the city names easier to read
  labs(
    title = "Total Public Housing Units per CBSA in sample",
    x = "City",
    y = "Total Public Housing Units"
  ) +
  theme_minimal()
units_per_cbsa_plot

ggsave(here(fig_dir, "units_per_cbsa_plot.pdf"), units_per_cbsa_plot,
        width = 8, height = 6, dpi = 900,
       bg = "white")

## number of treated tracts per city ----
tracts_per_cbsa_plot <- 
  ggplot(tracts_summary, aes(x = reorder(cbsa_title, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Number of Treated Tracts per CBSA in sample",
    x = "City",
    y = "Number of Treated Tracts"
  ) +
  theme_minimal()
tracts_per_cbsa_plot

ggsave(here(fig_dir, "tracts_per_cbsa_plot.pdf"), 
       tracts_per_cbsa_plot, width = 8, height = 6, dpi = 900,
       bg = "white")

## Share of tracts that are treated per city by 1990 ----
share_treated <- 
  balanced_sample %>%
  st_drop_geometry() %>% 
  filter(YEAR == 1990) %>%
  group_by(cbsa_title) %>%
  summarise(total_tracts = n(), 
            treated_tracts = sum(treated)) %>%
  mutate(share_treated = treated_tracts / total_tracts) 

# Plot share of treated tracts per city
share_treated_per_cbsa_plot <- 
  # Plot the share of treated tracts per city
  ggplot(share_treated, aes(x = reorder(cbsa_title, share_treated), y = share_treated)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  coord_flip() +  # Flip coordinates to make city names easier to read
  scale_y_continuous(labels = scales::percent_format()) +  # Display y-axis as percentages
  labs(
    title = "Share of Tracts in Sample with Public Housing (1990)",
    x = "City",
    y = "Share of Treated Tracts"
  ) +
  theme_minimal()
share_treated_per_cbsa_plot

ggsave(here(fig_dir, "share_treated_per_cbsa_plot.pdf"),
       share_treated_per_cbsa_plot, width = 8, height = 6, 
       dpi = 900,
       bg = "white")


## Units and number of projects per year ---- 

ph_per_year <- 
  public_housing_sample %>% 
  st_drop_geometry() %>% 
  group_by(year_completed) %>% 
  summarise(total_units = sum(total_public_housing_units, na.rm = TRUE),
            number_of_projects = n()) %>% 
  mutate(average_size_of_projects = total_units / number_of_projects)

# units per year plot
units_per_year_plot <- 
  ggplot(ph_per_year, aes(x = year_completed, y = total_units)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Total Public Housing Units by Year Completed",
    x = "Year Completed",
    y = "Total Public Housing Units"
  ) +
  theme_minimal()
units_per_year_plot

ggsave(here(fig_dir, "units_per_year_plot.pdf"), units_per_year_plot,
       width = 8, height = 6, dpi = 900,
       bg = "white")

# number of projects per year plot
projects_per_year_plot <- 
  ggplot(ph_per_year, aes(x = year_completed, y = number_of_projects)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Number of Public Housing Projects by Year Completed",
    x = "Year Completed",
    y = "Number of Public Housing Projects"
  ) +
  theme_minimal()
projects_per_year_plot
ggsave(here(fig_dir, "projects_per_year_plot.pdf"), projects_per_year_plot,
       width = 8, height = 6, dpi = 900,
       bg = "white")

# average size of projects per year plot
average_size_of_projects_per_year_plot <- 
  ggplot(ph_per_year, aes(x = year_completed, y = average_size_of_projects)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Size of Public Housing Projects by Year Completed",
    x = "Year Completed",
    y = "Average Number of Housing Units"
  ) +
  theme_minimal()
average_size_of_projects_per_year_plot

ggsave(here(fig_dir, "average_size_of_projects_per_year_plot.pdf"), 
       average_size_of_projects_per_year_plot, width = 8, height = 6, dpi = 900,
       bg = "white")

## Share of CDD missing lat and long, per locality ----
## original ph data in the localities in my sample

original_ph_data_sample_localities <-
  original_ph_data %>% 
  filter(locality %in% localities_in_my_sample$locality)

# can I join on CBSAs?
original_ph_data_sample_localities %>% 
  filter(is.na(latitude), !is.na(yearfullocc)) %>% 
  View()

# calculate TOTAL share of missing CDD data
# 9.9% of projects in localities in the sample are missing latitude
# 7.1% of units 
# But some will get it from aggregation... so how do I figure that out? 
# But I can 

share_missing_cdd_total <- 
  original_ph_data_sample_localities %>% 
  filter(!is.na(yearfullocc), totunitsplanned > 30) %>% 
  filter(locality != "CHICAGO", locality != "SAN FRANCISCO", locality != "NEW YORK CITY", locality != "BOSTON") %>% 
  # drop places for which 
  summarise(n = n(),
            missing_cdd = sum(is.na(latitude))) %>% 
  mutate(share_missing_cdd = missing_cdd / n)
share_missing_cdd_total

# merge to actual cleaned data
share_missing_cdd_projects <- 
  original_ph_data_sample_localities %>% 
  filter(totunitsplanned > 30, !is.na(yearfullocc)) %>%
  filter(locality != "CHICAGO", locality != "SAN FRANCISCO", locality != "NEW YORK CITY", locality != "BOSTON") %>% 
  group_by(locality) %>% 
  summarise(n = n(),
            missing_cdd = sum(is.na(latitude))) %>% 
  mutate(share_missing_cdd = missing_cdd / n)

# plot
share_missing_cdd_projects_plot <- 
  ggplot(share_missing_cdd_projects, aes(x = reorder(locality, share_missing_cdd), y = share_missing_cdd)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Share of Projects Missing CDD Data per Locality",
    x = "Locality",
    y = "Share of Projects Missing CDD Data"
  ) +
  theme_minimal()
share_missing_cdd_projects_plot


share_units_with_missing_latitude <- 
  original_ph_data_sample_localities %>% 
  filter(!is.na(yearfullocc), totunitsplanned > 30) %>%
  filter(locality != "CHICAGO", locality != "SAN FRANCISCO", locality != "NEW YORK CITY", locality != "BOSTON") %>% 
  group_by(locality) %>% 
  summarise(
    total_units = sum(totunitsplanned, na.rm = TRUE),
    units_missing_latitude = sum(totunitsplanned[is.na(latitude)], na.rm = TRUE)
  ) %>% 
  mutate(share_units_missing_latitude = units_missing_latitude / total_units)

# Plot
share_missing_units_with_missing_latitude_plot <- 
  ggplot(share_units_with_missing_latitude,
         aes(x = reorder(locality, share_units_missing_latitude), y = share_units_missing_latitude)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Share of Units with Missing Latitude per Locality",
    x = "Locality",
    y = "Share of Units with Missing Latitude"
  ) +
  theme_minimal()
share_missing_units_with_missing_latitude_plot

# share of units with missing latitude: 7%
sum(share_units_with_missing_latitude$units_missing_latitude)/sum(share_units_with_missing_latitude$total_units)

# Number of missing CDD projects per locality
number_of_projects_with_missing_latitude <- 
  original_ph_data_sample_localities %>% 
  filter(is.na(latitude), totunitsplanned > 30) %>% 
  group_by(locality) %>% 
  summarise(n = n())

# Plot
number_of_projects_with_missing_latitude_plot <- 
  ggplot(number_of_projects_with_missing_latitude, aes(x = reorder(locality, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(
    title = "Number of Projects with Missing Latitude per Locality",
    x = "Locality",
    y = "Number of Projects with Missing Latitude"
  ) +
  theme_minimal()
number_of_projects_with_missing_latitude_plot

# Maps of Chicago projects and tracts ----

## Map of Chicago projects ----
highlighted_projects <- public_housing_sample %>%
  filter(project_name %in% c("Cabrini Extension", 
                             "Robert R. Taylor Homes"))


chicago_ph_sample <- 
  public_housing_sample %>% 
  filter(locality == "CHICAGO")

chicago_coords <- chicago_ph_sample %>% 
  st_coordinates()

# find row wit hthe minimum longitude
westmost_index <- which.min(chicago_coords[,1])
westmost_point <- chicago_ph_sample[westmost_index,]

# plot map
ggplot() +
  # Cook County Census Tracts (background layer)
  geom_sf(data = census_tract_data %>%
            filter(COUNTY == "Cook", STATE == "Illinois", YEAR == 1940
                   ), 
          fill = "white", color = "gray80", size = 0.2) +  
  # Public Housing Projects
  geom_sf(data = public_housing_sample %>% filter(locality == "CHICAGO"),
          aes(size = total_public_housing_units),
          color = "#0072B2", alpha = 0.8) +  
  # Labels for key projects (with repel to avoid overlap)
  geom_text_repel(data = highlighted_projects, 
                  aes(label = project_name, geometry = geom),
                  stat = "sf_coordinates", size = 3, fontface = "bold",
                  box.padding = 0.8, point.padding = 0.5, 
                  segment.color = "black", segment.size = 0.3,
                  max.overlaps = Inf, min.segment.length = 0) +
  # Title and Legend
  labs(title = "Chicago Public Housing Projects in Sample",
       size = "# of Units") +
# Custom Themes
  theme_void() +  # Removes background elements for a cleaner look
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5, color = "gray50"),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_size(range = c(1, 6))  # Adjust point scaling


ggsave(here(fig_dir, "public_housing_projects_chicago.pdf"), width = 8, height = 6, dpi = 900,
       bg = "white")

## Map of Chicago treated tracts, nearby neighborhoods, urban renewal -----
chicago_all_tracts <- 
  census_tract_data %>%
  filter(COUNTY == "Cook", STATE == "Illinois", YEAR == 1940)

# Get treated tracts in Chicago
chicago_treated_tracts <- 
  treated_tracts %>%
  filter(city == "Chicago") %>%
  # If you need to filter by a specific year, uncomment and modify the next line
  # filter(YEAR == 1970) %>% 
  select(TRACTA, COUNTY, STATE, treated, treatment_year, total_public_housing_units) %>%
  distinct()

# Get all Chicago tracts from balanced sample for context
chicago_balanced_sample <-
  balanced_sample %>%
  filter(city == "Chicago") %>%
  filter(YEAR == 1990) %>%  # Using 1990 to show final treatment status
  select(TRACTA, COUNTY, STATE, treated) %>%
  st_drop_geometry() %>% 
  mutate(treated = factor(treated, levels = c(0, 1), labels = c("Never treated", "Ever Treated")))


chicago_rings <- 
  tracts_and_rings %>%
  filter(COUNTY == "Cook") %>% 
  select(-treatment_year) %>% 
  filter(location_type != "outer")


chicago_all_tracts_w_status <- 
  chicago_all_tracts %>% 
  left_join(chicago_rings) %>% 
  left_join(chicago_balanced_sample) %>% 
  mutate(status = ifelse(location_type == "treated", "Treated", "Never Treated")) %>% 
  mutate(status = ifelse(location_type == "inner", "Neighbor", status)) %>% 
  mutate(status = ifelse(is.na(location_type), "Never Treated",status)) %>% 
  mutate(status = ifelse(is.na(treated), "Excluded", status))
  

# Create a dataset by joining onto all  

# Create a map of Chicago treated tracts
ggplot() +
  # Cook County Census Tracts (background layer)
  geom_sf(data = chicago_all_tracts_w_status,
          aes(fill = status),
           color = "black", size = 0.3, alpha = 0.9) +  
    # Add projects as points for reference
  geom_sf(data = chicago_ph_sample, size = 1.5, 
          color = "lightgreen", alpha = 0.9) +   
  # Customize appearance
    # scale_fill_manual(values = c("white", "lightblue", "gray", "darkblue"),
    # name = "Treatment Status") +
  scale_fill_manual(values = c("Never Treated" = "#D3D3D3",  
                               "Excluded" = "#F0F0F0",   
                               "Neighbor" = "#E66100",  # Slightly darker orange
                               "Treated" = "#00008B"),  # Dark Blue
                    name = "Treatment Status") +

    scale_size(range = c(0.5, 4), name = "PH Units") +
    # Labels and theme
    labs(title = "Chicago: Treatment assignment") +
    theme_void() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.box = "vertical"
    )
  
ggsave(here(fig_dir, "chicago_treatment_assignment.pdf"), width = 8, height = 6, dpi = 900,
       bg = "white")

