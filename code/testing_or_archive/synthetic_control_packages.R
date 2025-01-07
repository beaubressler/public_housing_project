### Here is where I will practice using synthetic control packages/methods


library(tidysynth)

library(augsynth)
data("smoking")

### Tidy synth

smoking_out <-
  smoking %>% 
  synthetic_control(outcome = cigsale,
                    unit = state,
                    time = year,
                    i_unit = "California",
                    i_time = 1988,
                    generate_placebos = TRUE) %>% 
  # Generate the aggregate predictors used to fit the weights
  
  # average log income, retail price of cigarettes, and proportion of the
  # population between 15 and 24 years of age from 1980 - 1988
  generate_predictor(time_window = 1980:1988,
                     ln_income = mean(lnincome, na.rm = T),
                     ret_price = mean(retprice, na.rm = T),
                     youth = mean(age15to24, na.rm = T)) %>%
  
  # average beer consumption in the donor pool from 1984 - 1988
  generate_predictor(time_window = 1984:1988,
                     beer_sales = mean(beer, na.rm = T)) %>%
  
  # Lagged cigarette sales 
  generate_predictor(time_window = 1975,
                     cigsale_1975 = cigsale) %>%
  generate_predictor(time_window = 1980,
                     cigsale_1980 = cigsale) %>%
  generate_predictor(time_window = 1988,
                     cigsale_1988 = cigsale) %>%
  
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1970:1988, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()

# trends
smoking_out %>% plot_trends()

# difference between observed and counterfactual
smoking_out %>% plot_differences()

# balance table
smoking_out %>% grab_balance_table()


#### Gsynth package -----
library(gsynth)




