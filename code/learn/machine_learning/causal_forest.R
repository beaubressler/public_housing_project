# Testing Causal Forest 


library(glmnet)
library(grf)

test_data_ml <- 
  test_data %>% 
  select(year, id_var, STATE, COUNTY, TRACTA, city, treatment_year, black_share, 
         lag_asinh_pop_total,
         lag_asinh_pop_white, lag_asinh_pop_black,
         lag_population_density, lag_black_share, distance_from_cbd, lag_total_units,
         lag_low_skill_share, location_type) %>%
  mutate(treatment = if_else(location_type == "treated", 1, 0)) 

complete_cases <- test_data_ml %>%
  complete.cases()
  
test_data_ml <- test_data_ml[complete_cases, ]


X <- model.matrix(~ lag_asinh_pop_total + lag_asinh_pop_white + lag_asinh_pop_black +
                    lag_population_density + lag_black_share + city + distance_from_cbd + 
                    lag_low_skill_share, 
                  data = test_data_ml)
Y <- test_data_ml$black_share  # Outcome variable
W <- test_data_ml$treatment  # Treatment indicator

cf_model <- causal_forest(X, Y, W)
# Get the variable importance
variable_importance <- variable_importance(cf_model)
# Map variable importance back to the covariate names (columns of X)
covariate_names <- colnames(X)  # Get covariate names from the model matrix
# Combine importance scores with covariate names
importance_with_names <- data.frame(covariate = covariate_names, importance = variable_importance)
# Print the importance with corresponding covariates
print(importance_with_names)

### Individual treatment effects ----
# Estimate individual treatment effects (CATEs)
individual_treatment_effects <- predict(cf_model)$predictions

# Add these estimates to your dataset for further analysis
test_data_ml$predicted_effects <- individual_treatment_effects

# Estimate the overall average treatment effect (ATE)
ate_estimate <- average_treatment_effect(cf_model, target.sample="treated")

# Print ATE results
print(ate_estimate)

### Propensity scores----
propensity_scores <- get_forest_weights(cf_model)

# Add the propensity scores to the dataset
test_data_ml$propensity_scores <- propensity_scores


### Conditional average treatmnet effects -----
# TODO

###. 
# Plot the relationship between population density and predicted treatment effects
ggplot(test_data_ml %>% filter(propensity_scores > 0.01 | treatment == 1),
       aes(x = lag_population_density, y = predicted_effects)) +
  geom_point() +
  labs(title = "Heterogeneous Treatment Effects by Population Density",
       x = "Lagged Population Density", y = "Predicted Treatment Effects")

ggplot(test_data_ml %>% filter(propensity_scores > 0.01 | treatment == 1), aes(x = lag_black_share, y = predicted_effects)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Heterogeneous Treatment Effects by Initial Black Share",
       x = "Lagged Black Share", y = "Predicted Treatment Effects")

ggplot(test_data_ml %>% filter(propensity_scores > 0.0001| treatment == 1), 
       aes(x = lag_asinh_pop_white, y = predicted_effects)) +
  geom_point() +
  labs(title = "Heterogeneous Treatment Effects by Black Population",
       x = "Lagged Black Pop", y = "Predicted Treatment Effects")

### Policy targeting ----
# Sort the dataset by predicted treatment effects
test_data_ml_sorted <- test_data_ml %>%
  arrange(desc(predicted_effects))

# Identify the top 10% of units with the highest predicted treatment effects
top_10_percent_units <- test_data_ml_sorted %>%
  filter(rank(desc(predicted_effects)) <= nrow(test_data_ml_sorted) * 0.1)

# View the top 10% units for targeted intervention
head(top_10_percent_units[, c("STATE", "COUNTY", "city", "lag_black_share", "predicted_effects")])
