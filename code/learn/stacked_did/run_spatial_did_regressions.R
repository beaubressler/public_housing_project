####
# Spatial "inner and outer ring" DiD/event studies

# 1. Read in spatial DiD data created by "create_spatial_did_data.R"
# 2. Run event studies 
# 3. Output event study plots

####


### Right now, this is my main analysis file

# Preliminaries -----
# Load libraries
library(tidyverse)
library(here)
library(sf)
library(tigris)
library(crsuggest)
library(mapview)
library(maps)
library(ragg)
library(viridis)

# for covariate balance testing
library(tableone)

library(spdep)

# for event study
library(fixest)
library(ggfixest)
library(did2s)
 
rm(list=ls())

set.seed(123)

# !! choose which data to use:
# choices are "digitized", "cdd_large", "cdd_small", or "combined"
data_type <- "combined"


## define filepaths  -----

# directoriees 
did_output_dir <- here("output", "regression_results", "stacked_did", data_type)
merged_data_dir <- here("data", "derived", "merged", data_type)

# event study data paths
event_study_rings_output_path <- here(merged_data_dir, "event_study_data_rings.csv")
unique_tracts_and_rings_output_path <- here(merged_data_dir, "unique_tracts_and_rings.csv")



## variables for event studies and names -----
es_variables <- c("black_share", "white_share", "asinh_pop_total", "asinh_pop_black", "asinh_pop_white", 
                  "median_home_value_calculated", "asinh_median_home_value_calculated",
                  "median_rent_calculated", "asinh_median_rent_calculated",
                  "median_income", "asinh_median_income", 
                  "pct_hs_grad", "median_educ_years_25plus",
                  "unemp_rate", "lfp_rate", "employment_pop_ratio")
es_titles <- c("Effect of Public Housing on Black Share of Population",
               "Effect of Public Housing on White Share of Population",
               "Effect of Public Housing on (asinh) Total Population",
               "Effect of Public Housing on (asinh) Black Population",
               "Effect of Public Housing on (asinh) White Population",
               "Effect of Public Housing on Median Home Value",
               "Effect of Public Housing on (asinh) Median Home Value", 
               "Effect of Public Housing on Median Rent",
               "Effect of Public Housing on (asinh) Median Rent",
               "Effect of Public Housing on Median Income",
               "Effect of Public Housing on (asinh) Median Income",
               "Effect of Public Housing on Percent Graduating High School",
               "Effect of Public Housing on Median Years of Education",
               "Effect of Public Housing on Unemployment Rate",
               "Effect of Public Housing on Labor Force Participation Rate",
               "Effect of Public Housing on Employment-Population Ratio")



# Read in and prep data ----
event_study_data_rings <-
  read_csv(event_study_rings_output_path) 


# TODO: Run stacked diff-in-diff with inner and outer rings ----

# function for running event study regressions
run_event_study_regression <- function(data, dep_var) {
  
  # testing
  data <- event_study_data_rings
  dep_var <- "black_share"
  
  data <-
    data %>% 
     mutate(ring = case_when(location_type == "treated" ~ 1,
                            location_type == "inner"~  2,
                            location_type == "outer"~ 3)) %>% 
    mutate(ring = factor(ring, levels = c(3,2,1), labels = c("outer", "inner", "treated"))) %>% 
    filter(event_time > -40, event_time != 50)
  
    
  
  formula <- as.formula(paste(dep_var, "~ i(event_time, ring, ref = -10, ref2 = 'outer')",
                              "| treated_id^event_time + treated_id^ring^cluster + city^cluster^year + tract_id"))
  
  # Project (treated_id)-census year
  # Project (treated_id)-ring (location_type_factor)-neighborhood
  
  #TODO  I think this is right now 
  # model 
  model <- 
    feols(formula, data = data, cluster = c("treated_id", "tract_id"))
  
  model_treated <-
    feols(formula, data = data %>% filter(location_type != "inner"), cluster = c("treated_id", "tract_id"))
  model_inner <-
    feols(formula, data %>% filter(location_type != "treated"), cluster = c("treated_id", "tract_id"))
  
  
  
  # Tidy the model output and process it
  tidy_output <- 
    broom::tidy(model_treated) %>%
    mutate(location_type = "treated") %>%
    bind_rows(broom::tidy(model_inner) %>% mutate(location_type = "inner")) %>%
    mutate_if(is.numeric, round, 4) %>% 
    # remove event_time:: and :treated_group from term
    mutate(term = str_remove(term, "event_time::"),
           term = str_remove(term, ":treated_group")) %>% 
    #rename term event_time, convert to numeric
    dplyr::rename(event_time = term) %>%
    mutate(event_time = as.numeric(event_time))
  
  # Extract model summary using broom::glance()
  model_summary <- 
    broom::glance(model_treated) %>%
    bind_rows(broom::glance(model_inner)) %>%
    mutate_if(is.numeric, round, 4)
  
  # Combine tidy_output and model_summary
  output <- list(
    coefficients = tidy_output,
    summary = model_summary
  )
  
  return(output)
  
}


# function for plotting the event study graphs
create_event_study_plot <- function(reg_results_df, data, dep_var, title) {
  
  coefficients <- reg_results_df$coefficients
  
  # add a row for each location type with event_time = -10, estimate = 0, std.error = 0
  coefficients <- 
    bind_rows(coefficients,
              tibble(location_type = c("treated", "inner"), event_time = -10, estimate = 0, std.error = 0))
  
  
  # number of observations
  n_obs <- reg_results_df$summary %>%
    pull(nobs)
  
  # create the plot
  ggplot(coefficients, aes(x = event_time, y = estimate, color = location_type)) +
    geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error),
                  width = 0.2, alpha = 0.4, position = position_dodge(width = 1)) +
    geom_point(position = position_dodge(width = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title,
         subtitle = "",
         x = "Years Relative to Treatment",
         y = "Difference in Difference Estimate") +
    scale_x_continuous(breaks = seq(-40, 60, 10)) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.background = element_rect(fill = "white"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    guides(color = guide_legend(title = "Location Type")) +
    scale_color_brewer(palette = "Set1", labels = c("Inner Ring", "Treated"))
  # # add text at bottom of plot that says "t = 10 coefficient is t10_coefficient of the mean at t = 0"
  # labs(caption = paste("Treated coefficient at t = 10 is", round(t10_coefficient, 2), "of the treated tract mean at t = -10")) +
  # # add text at top of plot that says "n = n_obs"
  # annotate("text", x = 50, y = max(reg_results_df$coefficients$estimate), label = paste("n =", n_obs), hjust = 1)
}

## population regressions ---- 
event_study_rings_blk_sh_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "black_share")

event_study_rings_wht_sh_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "white_share")

event_study_rings_tot_pop_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_pop_total")

event_study_rings_blk_pop_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_pop_black")

event_study_rings_wht_pop_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_pop_white")

## median regressions ---- 

# median house value
event_study_rings_asinh_median_home_value_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_median_home_value_calculated")

event_study_rings_median_home_value_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "median_home_value_calculated")

# median rent
event_study_rings_asinh_median_rent_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_median_rent_calculated")

event_study_rings_median_rent_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "median_rent_calculated")

# median income
event_study_rings_asinh_median_income_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "asinh_median_income")

event_study_rings_median_income_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "median_income")

# median years of education
event_study_rings_median_education_tidy <-
  run_event_study_regression(
    data = event_study_data_rings %>% filter(year != 1930), dep_var = "median_educ_years_25plus")

# share with some college
event_study_rings_some_college_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "pct_some_college")

# share with high school or more
event_study_rings_hs_grad_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "pct_hs_grad")


# unemployment rate
event_study_rings_unemployment_rate_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "unemp_rate")

# LFP rate
event_study_rings_lfp_rate_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "lfp_rate")

# employment pop ratio
event_study_rings_emp_pop_ratio_tidy <-
  run_event_study_regression(
    data = event_study_data_rings, dep_var = "employment_pop_ratio")


# Create plots and output to pdf in the output directory ----

## produce graphs ----

#### 1. Full sample
pdf(here(did_output_dir, "ring_event_study_plots.pdf"), width = 10, height = 5)

# black share did plot
create_event_study_plot(reg_results_df = event_study_rings_blk_sh_tidy, 
                        data = event_study_data_rings,
                        dep_var = "black_share", 
                        "Effect of Public Housing on Black Share of Population")
# white share did plot
create_event_study_plot(event_study_rings_wht_sh_tidy,
                        data = event_study_data_rings,
                        dep_var = "white_share",
                        "Effect of Public Housing on White Share of Population")
# total population did plot
create_event_study_plot(event_study_rings_tot_pop_tidy, 
                        data = event_study_data_rings,
                        dep_var = "asinh_pop_total",
                        "Effect of Public Housing on (asinh) Total Population")
# black population did plot
create_event_study_plot(event_study_rings_blk_pop_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_pop_black",
                        "Effect of Public Housing on (asinh) Black Population")
# white population did plot
create_event_study_plot(event_study_rings_wht_pop_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_pop_white",
                        "Effect of Public Housing on (asinh) White Population") 

# median home value did plot
create_event_study_plot(event_study_rings_asinh_median_home_value_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_median_home_value",
                        "Effect of Public Housing on (asinh) Median Home Value")

create_event_study_plot(event_study_rings_median_home_value_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_median_home_value",
                        "Effect of Public Housing on Median Home Value")


# median rent did plot
create_event_study_plot(event_study_rings_asinh_median_rent_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_median_rent",
                        "Effect of Public Housing on (asinh) Median Rent")

create_event_study_plot(event_study_rings_median_rent_tidy,
                        data = event_study_data_rings,
                        dep_var = "median_rent",
                        "Effect of Public Housing on Median Rent")

# median income did plot
create_event_study_plot(event_study_rings_asinh_median_income_tidy,
                        data = event_study_data_rings,
                        dep_var = "asinh_median_income",
                        "Effect of Public Housing on (asinh) Median Income")

create_event_study_plot(event_study_rings_median_income_tidy,
                        data = event_study_data_rings,
                        dep_var = "median_income",
                        "Effect of Public Housing on Median Income")

# median years of education did plot
create_event_study_plot(event_study_rings_median_education_tidy,
                        data = event_study_data_rings,
                        dep_var = "median_educ_years_25plus",
                        "Effect of Public Housing on Median Years of Education")

# perent some college did plot
create_event_study_plot(event_study_rings_some_college_tidy,
                        data = event_study_data_rings,
                        dep_var = "pct_some_college",
                        "Effect of Public Housing on Percent with Some College")

# share with high school or more
create_event_study_plot(event_study_rings_hs_grad_tidy,
                        data = event_study_data_rings,
                        dep_var = "pct_hs_grad",
                        "Effect of Public Housing on Percent with High School or More")

# unemployment rate did plot
create_event_study_plot(event_study_rings_unemployment_rate_tidy,
                        data = event_study_data_rings,
                        dep_var = "unemp_rate",
                        "Effect of Public Housing on Unemployment Rate")

# LFP rate did plot
create_event_study_plot(event_study_rings_lfp_rate_tidy,
                        data = event_study_data_rings,
                        dep_var = "lfp_rate",
                        "Effect of Public Housing on Labor Force Participation Rate")

# employment pop ratio did plot
create_event_study_plot(event_study_rings_emp_pop_ratio_tidy,
                        data = event_study_data_rings,
                        dep_var = "employment_pop_ratio",
                        "Effect of Public Housing on Employment-Population Ratio")

dev.off()

### 2. Heterogeneity by size of public housing development ----
# Using "total units planned" 
treated_first_row <- 
  event_study_data_rings %>%
  filter(location_type == "treated") %>%
  group_by(treated_id) %>% filter(row_number() == 1)

# get 25th, 50th and 75th percentiles of total_public_housing_units
units_25p <- quantile(treated_first_row$total_public_housing_units, probs = 0.25)
units_50p <- quantile(treated_first_row$total_public_housing_units, probs = 0.5)
units_75p <- quantile(treated_first_row$total_public_housing_units, probs = 0.75)

# create a variable called "size" equal to "25th" if total_public_housing_units is less than 25th percentile,
# "50th" if total_public_housing_units is between 25th and 50th percentile, "75th" if total_public_housing_units is between
# 50th and 75th percentile and "large" otherwise
event_study_data_rings <- event_study_data_rings %>%
  mutate(size = case_when(total_public_housing_units < units_25p ~ "25th",
                          total_public_housing_units >= units_25p & total_public_housing_units < units_50p ~ "50th",
                          total_public_housing_units >= units_50p & total_public_housing_units < units_75p ~ "75th",
                          total_public_housing_units >= units_75p ~ "75th_to_max"),
         size_bunch = case_when(total_public_housing_units < units_50p ~ "small",
                                total_public_housing_units >= units_50p ~ "large"))

# re-run event studies separately for each size

for(size_group in unique(event_study_data_rings$size)) {
  # Filter the dataset for the current size
  filtered_data <- event_study_data_rings %>% filter(size == size_group)
  
  pdf_file_name <- paste0(did_output_dir, "by_size/ring_event_study_plots_size_", size_group, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  
  for(i in seq_along(es_variables)) {
    # Assuming you have a modified version of your function that returns the tidy data for the given variable
    tidy_output <- run_event_study_regression(filtered_data, es_variables[i])
    
    # Create and print the plot
    plot <- create_event_study_plot(tidy_output,
                                    data = filtered_data,
                                    dep_var = es_variables[i],
                                    es_titles[i])
    
    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}

# repeat but combine into 2
for(size_group in unique(event_study_data_rings$size_bunch)) {
  # Filter the dataset for the current size
  filtered_data <- event_study_data_rings %>% filter(size_bunch == size_group)
  
  pdf_file_name <- paste0(did_output_dir, "by_size/ring_event_study_plots_size_", size_group, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  
  for(i in seq_along(es_variables)) {
    # Assuming you have a modified version of your function that returns the tidy data for the given variable
    tidy_output <- run_event_study_regression(filtered_data, es_variables[i])
    
    # Create and print the plot
    plot <- create_event_study_plot(tidy_output,
                                    data = filtered_data,
                                    dep_var = es_variables[i],
                                    es_titles[i])
    
    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}


### 3. Heterogeneity by pre-treatment racial composition of the treated tract -----

# get pre-treatment black shares: 
pre_treatment_data <- 
  event_study_data_rings %>%
  filter(location_type == "treated") %>%
  filter(event_time == -10)

# get quantiles of pre-treatment black shares
blk_sh_25p <- quantile(pre_treatment_data$black_share, probs = 0.25)
blk_sh_50p <- quantile(pre_treatment_data$black_share,  probs = 0.5)
blk_sh_75p <- quantile(pre_treatment_data$black_share, probs = 0.75)

# create a variable called "pre_treatment_blk_sh" equal to "25th" if pre-treatment
# black share of the treated tract is less than 25th percentile, "50th" if pre-treatment black share is between
# 25th and 50th percentile, "75th" if pre-treatment black share is between 50th and 75th
# percentile and "high" otherwise
pre_treatment_blk_sh <-
  event_study_data_rings %>% 
  filter(event_time == -10, location_type == "treated") %>% 
  select(black_share, treated_id) %>%
  dplyr::rename(pre_treatment_blk_sh_treated = black_share)

blk_sh_25p <- quantile(pre_treatment_blk_sh$pre_treatment_blk_sh_treated, probs = 0.25, na.rm = TRUE)
blk_sh_50p <- quantile(pre_treatment_blk_sh$pre_treatment_blk_sh_treated, probs = 0.5, na.rm = TRUE)
blk_sh_75p <- quantile(pre_treatment_blk_sh$pre_treatment_blk_sh_treated, probs = 0.75, na.rm = TRUE)

event_study_data_rings <- 
  left_join(event_study_data_rings, pre_treatment_blk_sh)  %>%
  mutate(pre_treatment_blk_sh = case_when(pre_treatment_blk_sh_treated < blk_sh_25p  ~ "low",
                                          pre_treatment_blk_sh_treated > blk_sh_25p & pre_treatment_blk_sh_treated < blk_sh_50p ~ "medium",
                                          pre_treatment_blk_sh_treated >= blk_sh_50p ~ "high"))


# re-run event studies separately for each pre-treatment black share group
for(race_group in unique(event_study_data_rings %>% filter(!is.na(pre_treatment_blk_sh)) %>% pull(pre_treatment_blk_sh))) {
  # Filter the dataset for the current size
  filtered_data <- event_study_data_rings %>% filter(pre_treatment_blk_sh == race_group)
  
  pdf_file_name <- paste0(did_output_dir, "by_initial_share/ring_event_study_plots_by_blk_share_", race_group, ".pdf")
  
  # Open PDF device for the current size
  pdf(pdf_file_name, width = 10, height = 5)
  
  for(i in seq_along(es_variables)) {
    # Assuming you have a modified version of your function that returns the tidy data for the given variable
    tidy_output <- run_event_study_regression(filtered_data, es_variables[i])
    
    # Create and print the plot
    plot <- create_event_study_plot(tidy_output,
                                    data = filtered_data,
                                    dep_var = es_variables[i],
                                    es_titles[i])
    
    # if the variable is a share, set the y-axis limits to be between -.25 and .25
    # if the variable is a population, set the y-axis limits to be between -2 and 2
    
    # if(i %in% c(1,2)) {
    #   plot <- plot + scale_y_continuous(limits = c(-.25,.25), breaks = seq(-.25, .25, .05))
    # } else {
    #   plot <- plot + scale_y_continuous(limits = c(-2.5,2.5), breaks = seq(-2, 2, .5))
    # }
    
    print(plot)
  }
  
  # Close the PDF device for the current size
  dev.off()
}


### 4. Heterogeneity by city -----
# # loop through each of the cities in the dataset, run regressions separately
# for(c in unique(event_study_data_rings$city)) {
#   # Filter the dataset for the current size
#   filtered_data <- event_study_data_rings %>% filter(city == c)
#   
#   pdf_file_name <- paste0(did_output_dir, "by_city/ring_event_study_plots_", c, ".pdf")
#   
#   # Open PDF device for the current size
#   pdf(pdf_file_name, width = 10, height = 5)
#   
#   for(i in seq_along(es_variables)) {
#     # Assuming you have a modified version of your function that returns the tidy data for the given variable
#     tidy_output <- run_event_study_regression(filtered_data, es_variables[i])
#     
#     # Create and print the plot
#     plot <- create_event_study_plot(tidy_output,
#                                     data = filtered_data,
#                                     dep_var = es_variables[i],
#                                     es_titles[i])
#     print(plot)
#   }
#   
#   # Close the PDF device for the current size
#   dev.off()
# }
