# Project Demographics and Neighborhood Targeting Analysis
# This script analyzes how public housing project racial composition
# varies systematically with neighborhood baseline characteristics

library(tidyverse)
library(fixest)
library(here)
library(modelsummary)
library(tinytable) # for formatting output
library(sf)
library(scales)

rm(list = ls())

# Parameters -----
data_type <- "combined"
merged_data_dir <- here("data", "derived", "merged", data_type)

results_dir <- here("output", "regression_results", "site_selection", data_type)
tables_dir <- here("output", "tables", "site_selection", data_type)

# Create output directories
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# Load balanced dataset -----
cat("Loading balanced dataset with all public housing projects...\n")
balanced_data <- st_read(here(merged_data_dir, "census_tract_sample_with_treatment_status_balanced.gpkg")) %>% 
  st_drop_geometry()

treated_tracts_info <-
  st_read(here(merged_data_dir, "treated_tracts_panel_balanced.gpkg")) %>% 
  st_drop_geometry() %>% 
  distinct(GISJOIN_1950, total_public_housing_units, treatment_year)



# Create analysis dataset -----
cat("Creating project demographics analysis dataset...\n")

# Extract treated tract characteristics at baseline and treatment year
project_demographics <- balanced_data %>%
  select(-total_public_housing_units) %>% 
  left_join(treated_tracts_info, by = "GISJOIN_1950") %>%
  mutate(
    event_time = YEAR - treatment_year
  ) %>%
  # create transformed variables
  mutate(asinh_pop_total = asinh(total_pop),
         asinh_pop_white = asinh(white_pop),
         asinh_pop_black = asinh(black_pop),
         asinh_median_income = asinh(median_income),
         asinh_median_rent_calculated = asinh(median_rent_calculated),
         asinh_median_home_value_calculated = asinh(median_home_value_calculated),
         asinh_distance_from_cbd = asinh(distance_from_cbd),
         ln_population_density = log(population_density),
         asinh_private_population_estimate = asinh(private_population_estimate),
         asinh_private_black_population_estimate = asinh(private_black_population_estimate),
         asinh_private_white_population_estimate = asinh(private_white_population_estimate),
         log_private_units_estimate = asinh(total_private_units_estimate)
  ) %>% 
  # Get baseline characteristics
  group_by(GISJOIN_1950) %>%
  mutate(
    baseline_asinh_pop_total = asinh_pop_total[event_time == -10],
    baseline_black_share = black_share[event_time == -10],
    baseline_white_share = white_share[event_time == -10],
    baseline_log_income = asinh_median_income[event_time == -10],
    baseline_log_rent = asinh_median_rent_calculated[event_time == -10],
    baseline_pop_density = population_density[event_time == -10],
    baseline_unemp_rate = unemp_rate[event_time == -10],
    baseline_dissimilarity = local_dissimilarity_index[event_time == -10],
    baseline_distance_cbd = asinh_distance_from_cbd[event_time == -10],
    baseline_distance_highway = asinh(distance_to_highway_km)[event_time == -10],
    redlined = redlined_binary_80pp[event_time == -10]
  ) %>%
  ungroup() %>%
  # Get project characteristics at treatment year (t=0)
  filter(event_time == 0) %>%
  mutate(
    # Calculate project racial composition
    project_black_share = ifelse(total_public_housing_pop_estimate > 0,
                                black_public_housing_pop_estimate / total_public_housing_pop_estimate,
                                NA),
    project_white_share = ifelse(total_public_housing_pop_estimate > 0,
                                white_public_housing_pop_estimate / total_public_housing_pop_estimate,
                                NA),
    # If share is greater than 1, set to 1
    project_black_share = ifelse(project_black_share > 1, 1, project_black_share),
    project_white_share = ifelse(project_white_share > 1, 1, project_white_share),
    project_non_black_share = 1- project_black_share,
    # Project size 
    project_size_log = log(total_public_housing_units)
    # black share, income, and unemployment terciles
  ) %>%
  #filter(!is.na(project_black_share), !is.na(baseline_black_share))  %>%
  select(GISJOIN_1950, COUNTY, treatment_year,
         # Baseline neighborhood characteristics
         baseline_asinh_pop_total,
         baseline_black_share, baseline_white_share, baseline_log_income, baseline_log_rent,
         baseline_pop_density, baseline_unemp_rate, baseline_dissimilarity,
         baseline_distance_cbd, baseline_distance_highway, redlined, ur_binary_10pp, ur_binary_5pp,
         # Project characteristics
         total_public_housing_units, project_size_log,
         project_black_share, project_white_share) 
#%>% 
 # filter(!is.infinite(project_black_share), !is.infinite(project_white_share)) 


cat("Analysis dataset created with", nrow(project_demographics), "projects\n")

# Descriptive Analysis -----
cat("\n=== DESCRIPTIVE ANALYSIS ===\n")

# Create terciles for analysis
project_demographics <- project_demographics %>%
  mutate(
    baseline_black_tercile = ntile(baseline_black_share, 3),
    baseline_income_tercile = ntile(exp(baseline_log_income), 3)
  )

# Summary by baseline Black share terciles
baseline_black_summary <- project_demographics %>%
  group_by(baseline_black_tercile) %>%
  summarise(
    n_projects = n(),
    mean_baseline_black = mean(baseline_black_share, na.rm = TRUE),
    mean_project_black = mean(project_black_share, na.rm = TRUE),
    mean_project_white = mean(project_white_share, na.rm = TRUE),
    mean_project_size = mean(total_public_housing_units, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    baseline_black_tercile_label = case_when(
      baseline_black_tercile == 1 ~ "Low (Bottom Tercile)",
      baseline_black_tercile == 2 ~ "Medium (Middle Tercile)", 
      baseline_black_tercile == 3 ~ "High (Top Tercile)"
    )
  )
# 
# cat("\nProject Demographics by Baseline Neighborhood Black Share:\n")
# print(baseline_black_summary)
# 
# Summary by baseline income terciles
baseline_income_summary <- project_demographics %>%
  group_by(baseline_income_tercile) %>%
  summarise(
    n_projects = n(),
    mean_baseline_income = mean(exp(baseline_log_income), na.rm = TRUE),
    mean_project_black = mean(project_black_share, na.rm = TRUE),
    mean_project_white = mean(project_white_share, na.rm = TRUE),
    mean_project_size = mean(total_public_housing_units, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    baseline_income_tercile_label = case_when(
      baseline_income_tercile == 1 ~ "Low (Bottom Tercile)",
      baseline_income_tercile == 2 ~ "Medium (Middle Tercile)",
      baseline_income_tercile == 3 ~ "High (Top Tercile)"
    )
  )

cat("\nProject Demographics by Baseline Neighborhood Black Share:\n")
print(baseline_black_summary)

cat("\nProject Demographics by Baseline Neighborhood Income:\n")
print(baseline_income_summary)

# Regression Analysis -----
cat("\n=== REGRESSION ANALYSIS ===\n")

## Project Racial shares ----

### 1. No FE, 
model_black_share_no_fe <- feols(
  project_black_share ~ baseline_black_share + baseline_log_income + 
                        baseline_asinh_pop_total + baseline_unemp_rate + 
                       baseline_distance_cbd + redlined + ur_binary_5pp,
  data = project_demographics,
  cluster = ~COUNTY
)
model_black_share_no_fe

model_white_share_no_fe <- 
feols(
  project_white_share ~ baseline_black_share + baseline_log_income +
                        baseline_asinh_pop_total + baseline_unemp_rate +
                       baseline_distance_cbd + redlined + ur_binary_5pp,
  data = project_demographics,
  cluster = ~COUNTY
)
model_white_share_no_fe


### 2. Full specification with all controls
model_black_share_full <- feols(
  project_black_share ~ baseline_black_share + baseline_log_income + baseline_log_rent +
                        baseline_asinh_pop_total + baseline_unemp_rate +
                       baseline_distance_cbd + baseline_distance_highway + redlined + ur_binary_5pp| COUNTY,
  data = project_demographics,
  cluster = ~COUNTY
)
model_black_share_full


model_white_share_full <- feols(
  project_white_share ~ baseline_black_share + baseline_log_income + baseline_log_rent +
                        baseline_asinh_pop_total + baseline_unemp_rate +
                        baseline_distance_cbd + baseline_distance_highway + redlined  + ur_binary_5pp | COUNTY,
  data = project_demographics,
  cluster = ~COUNTY
)

model_white_share_full

### 3. Simple specifications (parsimonious models)
model_black_share_simple <- feols(
  project_black_share ~ baseline_black_share + baseline_log_income | COUNTY,
  data = project_demographics,
  cluster = ~COUNTY
)

model_white_share_simple <- feols(
  project_white_share ~ baseline_black_share + baseline_log_income | COUNTY,
  data = project_demographics,
  cluster = ~COUNTY
)

cat("\nSimple models:\n")
model_black_share_simple
model_white_share_simple

# ## Project size models ----
# # 8/22/2025: The problem with these project size models is that I split the projects up among multiple tracts
# # if they intersect multiple tracts. This means that I am not really running a "project" level regression. This could explain
# # the surprising results where the project size is negatively associated with baseline Black share.
# # For now, I will not use these. If i wanted to run this, I could aggregate at project level by collapsing, for each project,
# # all the associated tracts. The other thing is that this is not real project size, but rather all of the first projects.
# # I don't think this is as much of an issue with the racial shares models, because those are just shares, not actual size
# 
# # If I wanted, I could create another project-level dataset at some other points to do some of this stuff (eg each project with it's baseline tracts).
# 
# # Project size
# model_size_no_fe <- feols(
#   project_size_log ~ baseline_black_share + baseline_log_income +
#     baseline_pop_density + baseline_unemp_rate +
#     baseline_distance_cbd + redlined + ur_binary_5pp,
#   data = project_demographics,
#   cluster = ~GISJOIN_1950
# )
# 
# model_size_no_fe
# 
# model_size_fe <- 
# feols(
#   project_size_log ~ baseline_black_share + baseline_log_income +
#     baseline_pop_density + baseline_unemp_rate +
#     baseline_distance_cbd + redlined + ur_binary_5pp | COUNTY,
#   data = project_demographics,
#   cluster = ~GISJOIN_1950
# )
# 
# model_size_fe
# 
# feols(project_size_log ~ baseline_black_share + baseline_log_income +
#         baseline_asinh_pop_total + baseline_unemp_rate | COUNTY, data = project_demographics)
# 
# 
# # Model 4: Non-linear specification with terciles
# # model_black_terciles <- feols(
# #   project_black_share ~ i(baseline_black_tercile, ref = 1) + 
# #                        i(baseline_income_tercile, ref = 1) +
# #                        baseline_pop_density + baseline_unemp_rate +
# #                        baseline_distance_cbd + redlined + project_size_log |
# #                        treatment_year,
# #   data = project_demographics,
# #   cluster = ~GISJOIN_1950
# # )
# 
# 
# Save regression tables -----
cat("\n=== SAVING RESULTS ===\n")

# Project racial composition table - focus on Black share only
racial_composition_models <- list(
  "(1)" = model_black_share_simple,
  "(2)" = model_black_share_full
)


racial_comp_table <- modelsummary(
  racial_composition_models,
  stars = TRUE,
  fmt = 3,
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors|Within",
  coef_rename = c(
    "baseline_black_share" = "Black Share",
    "baseline_log_income" = "Log Median Income",
    "baseline_log_rent" = "Log Median Rent",
    "baseline_asinh_pop_total" = "Log Population",
    "baseline_unemp_rate" = "Unemployment Rate",
    "baseline_distance_cbd" = "Log Distance to CBD",
    "baseline_distance_highway" = "Log Distance to Highway",
    "redlined" = "Redlined (HOLC)",
    "ur_binary_5pp" = "Urban Renewal Area"
  ),
  add_rows = data.frame(term = "County FE", "(1)" = "Yes", "(2)" = "Yes",
                        check.names = FALSE),
  title = "Public housing project demographic composition by neighborhood characteristics\\label{tab:project_targeting}",
  # split long footnotes into separate lines
  notes = paste(
    "Regression of project Black share on baseline neighborhood characteristics.",
    "All specifications include county fixed. Standard errors clustered by county.", sep = " " 
  ),
  output = "tinytable",         # get a tinytable object for post-processing
  width  = 1,                   # pass-through to tinytable::tt(): use full \linewidth
  theme  = "resize",            # scale to fit if needed (LaTeX)
  align  = "lcc"                # wider first column for variable names
)

# Optional polish: slightly smaller font & a bit more row height for readability
racial_comp_table <- style_tt(racial_comp_table, font_size = 0.95, height = 1.2)

# save to .tex
tinytable::save_tt(racial_comp_table, file.path(tables_dir, "project_demographics_targeting.tex"), overwrite = TRUE)

# Save descriptive tables
write_csv(baseline_black_summary, file.path(results_dir, "descriptive_by_baseline_black.csv"))
write_csv(baseline_income_summary, file.path(results_dir, "descriptive_by_baseline_income.csv"))

# Create visualization -----
cat("\n=== CREATING VISUALIZATIONS ===\n")

# Scatter plot: Baseline Black share vs Project Black share
plot_targeting <- ggplot(project_demographics, 
                        aes(x = baseline_black_share, y = project_black_share)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Public Housing Project Demographics by Neighborhood Context",
    subtitle = "Project racial composition follows neighborhood baseline patterns",
    x = "Baseline Neighborhood Black Share (t = -10)",
    y = "Public Housing Project Black Share (t = 0)",
    caption = "Each point represents one public housing project. Line shows linear fit."
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank()
  )

# Create figures directory first
figures_dir <- here("output", "figures", "site_selection", data_type)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(figures_dir, "project_demographics_targeting.pdf"),
  plot = plot_targeting,
  width = 10, height = 7
)

# Box plot by terciles (terciles already created above)

plot_terciles <- project_demographics %>%
  mutate(
    baseline_black_tercile_label = case_when(
      baseline_black_tercile == 1 ~ "Low\n(Bottom Tercile)",
      baseline_black_tercile == 2 ~ "Medium\n(Middle Tercile)", 
      baseline_black_tercile == 3 ~ "High\n(Top Tercile)"
    )
  ) %>%
  ggplot(aes(x = baseline_black_tercile_label, y = project_black_share, 
             fill = factor(baseline_black_tercile))) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  labs(
    title = "Project Demographics by Neighborhood Baseline Black Share",
    subtitle = "Projects in whiter neighborhoods served whiter populations",
    x = "Baseline Neighborhood Black Share Tercile",
    y = "Public Housing Project Black Share"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(type = "qual", palette = "Dark2", guide = "none") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Figures directory already created above

ggsave(
  filename = file.path(figures_dir, "project_demographics_by_terciles.pdf"),
  plot = plot_terciles,
  width = 10, height = 7
)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to:", results_dir, "\n")
cat("Tables saved to:", tables_dir, "\n")
cat("Figures saved to:", figures_dir, "\n")
