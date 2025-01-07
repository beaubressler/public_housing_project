####
# Exploratory analysis of CDD merged data
#####

# Preliminaries ----
library(tidyverse)
library(haven)
library(sf)
library(maps)
library(mapdata)
library(tigris)

# dataset choice
# !! Set this yourself
dataset_choice <- "combined"

# directories
merged_data_dir <- here("data", "derived", "merged")
ph_data_dir <- here("data", "derived", "public_housing", "working")

## filepaths
balanced_sample_filepath <- 
  here(merged_data_dir, dataset_choice, "census_tract_sample_with_treatment_status_balanced.gpkg")
balanced_treated_tracts_panel_filepath <- 
  here(merged_data_dir, dataset_choice, "treated_tracts_panel_balanced.gpkg")
public_housing_sample_filepath <-
  here(ph_data_dir, dataset_choice, "public_housing_sample_balanced.gpkg")


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
balanced_sample <- read_sf(balanced_sample_filepath)
treated_tracts <- read_sf(balanced_treated_tracts_panel_filepath)
public_housing_sample <- read_sf(public_housing_sample_filepath)


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
  distinct(COUNTY, TRACTA, STATE, city, cbsa_title,
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
  distinct(COUNTY, TRACTA, STATE, city, total_public_housing_units)


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

# how many units in these projects
sum(public_housing_projects_not_in_sample_other_localities$total_public_housing_units)


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

`## number of treated tracts per city ----
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
    y = "Average Size of Public Housing Projects"
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
  ggplot(share_units_with_missing_latitude, aes(x = reorder(locality, share_units_missing_latitude), y = share_units_missing_latitude)) +
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
