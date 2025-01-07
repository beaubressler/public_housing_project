# Set seed for reproducibility
set.seed(123)

# Number of neighborhoods and household types
J <- 100  # Number of neighborhoods
K <- 2    # Number of household types

# Total number of households in each type
N_k <- c(1000, 800)  # Number of households in each group

# Preference parameters for each group k
alpha_p <- c(-0.5, -0.4)     # Sensitivity to price
alpha_b <- c(0.3, 0.2)       # Preference for Black share
alpha_w <- c(0.4, 0.3)       # Preference for White share
alpha_inc <- c(0.2, 0.1)     # Sensitivity to income
alpha_ph <- c(-0.1, -0.2)    # Sensitivity to public housing
theta_pref <- c(0.05, 0.03)  # Preference for other characteristics

# Supply elasticity
psi <- 1.0  # Example value

# Initialize neighborhood characteristics
p <- runif(J, 500, 1500)         # Initial housing prices
b <- runif(J, 0, 1)              # Share of Black residents
w <- runif(J, 0, 1 - b)          # Share of White residents
inc <- runif(J, 20000, 60000)    # Median income
ph <- runif(J, 0, 0.2)           # Public housing share
x <- runif(J)                    # Other characteristics
theta_j <- runif(J, 10, 50)      # Supply shifter

# Ensure that shares sum to 1 or less
total_share <- b + w
over_one <- total_share > 1
if (any(over_one)) {
  b[over_one] <- b[over_one] / total_share[over_one]
  w[over_one] <- w[over_one] / total_share[over_one]
}

# Store initial ph for comparison
ph_baseline <- ph

# Initialize delta matrix
delta <- matrix(0, nrow=J, ncol=K)  # J x K matrix

# Iterative process to reach equilibrium
max_iter <- 1000
tolerance <- 1e-6
adjustment_factor <- 0.1

# Store initial prices
p_baseline <- p

# Initialize variables to store baseline results
p_eq_baseline <- p
D_j_baseline <- NULL
P_jk_baseline <- NULL

for (iter in 1:max_iter) {
  # Compute delta
  for (k in 1:K) {
    delta[, k] <- alpha_p[k] * log(p_eq_baseline) +
      alpha_b[k] * b +
      alpha_w[k] * w +
      alpha_inc[k] * log(inc) +
      alpha_ph[k] * ph +
      theta_pref[k] * x
  }
  
  # Compute choice probabilities
  exp_delta <- exp(delta)
  denominator <- colSums(exp_delta)
  P_jk <- exp_delta / matrix(denominator, nrow=J, ncol=K, byrow=TRUE)
  
  # Compute demand
  D_j <- P_jk %*% N_k
  
  # Compute supply
  S_j <- theta_j * p_eq_baseline^psi
  
  # Check for convergence
  excess_demand <- D_j - S_j
  max_excess <- max(abs(excess_demand))
  
  if (max_excess < tolerance) {
    cat("Baseline convergence achieved after", iter, "iterations.\n")
    break
  }
  
  # Adjust prices
  p_eq_baseline <- p_eq_baseline + adjustment_factor * (excess_demand / S_j) * p_eq_baseline
  p_eq_baseline <- pmax(p_eq_baseline, 0.1)  # Ensure prices stay positive
}

# Store baseline results
D_j_baseline <- D_j
S_j_baseline <- S_j
P_jk_baseline <- P_jk

# Display results
cat("Equilibrium Housing Prices:\n")
print(p_eq_baseline)

cat("\nTotal Demand in each neighborhood (D_j):\n")
print(D_j_baseline)

cat("\nTotal Supply in each neighborhood (S_j):\n")
print(S_j_baseline)

# Check maximum difference between demand and supply
max_diff <- max(abs(D_j_baseline - S_j_baseline))
cat("\nMaximum difference between demand and supply:", max_diff, "\n")


# eq housing prices
plot(1:J, p_eq_baseline, type='b', col='blue', pch=19, xlab='Neighborhood', ylab='Price',
     main='Equilibrium Housing Prices Across Neighborhoods')

# demand vs supply
plot(1:J, D_j, type='b', col='red', pch=19, xlab='Neighborhood', ylab='Units',
     main='Demand and Supply in Each Neighborhood')
lines(1:J, S_j, type='b', col='green', pch=19)
legend("topright", legend=c("Demand", "Supply"), col=c("red", "green"), pch=19)



# Policy counterfactual ----
# Policy intervention: Increase public housing in first 10 neighborhoods
ph_policy <- ph  # Copy baseline ph
increase_fraction <- 0.5  # 50% increase

# Apply the increase
ph_policy[1:10] <- ph_policy[1:10] * (1 + increase_fraction)

# Ensure ph does not exceed 1
ph_policy <- pmin(ph_policy, 1.0)


# Initialize variables for policy scenario
p_eq_policy <- p_baseline  # Start from baseline prices

for (iter in 1:max_iter) {
  # Compute delta
  for (k in 1:K) {
    delta[, k] <- alpha_p[k] * log(p_eq_policy) +
      alpha_b[k] * b +
      alpha_w[k] * w +
      alpha_inc[k] * log(inc) +
      alpha_ph[k] * ph_policy +
      theta_pref[k] * x
  }
  
  # Compute choice probabilities
  exp_delta <- exp(delta)
  denominator <- colSums(exp_delta)
  P_jk_policy <- exp_delta / matrix(denominator, nrow=J, ncol=K, byrow=TRUE)
  
  # Compute demand
  D_j_policy <- P_jk_policy %*% N_k
  
  # Compute supply
  S_j_policy <- theta_j * p_eq_policy^psi
  
  # Check for convergence
  excess_demand <- D_j_policy - S_j_policy
  max_excess <- max(abs(excess_demand))
  
  if (max_excess < tolerance) {
    cat("Policy scenario convergence achieved after", iter, "iterations.\n")
    break
  }
  
  # Adjust prices
  p_eq_policy <- p_eq_policy + adjustment_factor * (excess_demand / S_j_policy) * p_eq_policy
  p_eq_policy <- pmax(p_eq_policy, 0.1)  # Ensure prices stay positive
}

# Store policy scenario results
D_j_policy <- D_j_policy
S_j_policy <- S_j_policy
P_jk_policy <- P_jk_policy

# Change in housing prices
price_change <- p_eq_policy - p_eq_baseline

# Plot price changes
plot(1:J, price_change, type='b', col='purple', pch=19,
     xlab='Neighborhood', ylab='Price Change',
     main='Change in Equilibrium Housing Prices Due to Policy')

# Highlight neighborhoods with increased public housing
points(1:10, price_change[1:10], col='red', pch=19)
legend("bottomright", legend=c("Other Neighborhoods", "Public Housing Increase"),
       col=c("purple", "red"), pch=19)


# Demand change
demand_change <- D_j_policy - D_j_baseline

# Supply change (should be minimal unless supply shifter changes)
supply_change <- S_j_policy - S_j_baseline

# Plot demand changes
plot(1:J, demand_change, type='b', col='blue', pch=19,
     xlab='Neighborhood', ylab='Demand Change',
     main='Change in Housing Demand Due to Policy')

# Highlight neighborhoods with increased public housing
points(1:10, demand_change[1:10], col='red', pch=19)
legend("bottomright", legend=c("Other Neighborhoods", "Public Housing Increase"),
       col=c("blue", "red"), pch=19)


## neighborhood racial composition ----
# Baseline expected number of households of each type in each neighborhood
E_n_baseline <- P_jk_baseline * matrix(N_k, nrow=J, ncol=K, byrow=TRUE)

# Policy scenario expected number of households
E_n_policy <- P_jk_policy * matrix(N_k, nrow=J, ncol=K, byrow=TRUE)

# Compute changes in racial composition
racial_comp_change <- E_n_policy - E_n_baseline

# For simplicity, we'll focus on the first group (e.g., Group 1)
plot(1:J, racial_comp_change[,1], type='b', col='green', pch=19,
     xlab='Neighborhood', ylab='Change in Expected Households (Group 1)',
     main='Change in Neighborhood Composition Due to Policy')

# Highlight neighborhoods with increased public housing
points(1:10, racial_comp_change[1:10,1], col='red', pch=19)
legend("bottomright", legend=c("Other Neighborhoods", "Public Housing Increase"),
       col=c("green", "red"), pch=19)



# More complex model ----
# Set seed for reproducibility
# Set seed for reproducibility
set.seed(123)

# Number of neighborhoods and racial groups
J <- 100  # Number of neighborhoods
K <- 3    # Number of racial groups (White, Black, Other)

# Total number of households in each group
N_k <- c(1000, 800, 600)  # Number of households in each racial group

# Preference parameters for each group (White, Black, Other)
alpha_p <- c(-0.5, -0.4, -0.3)      # Sensitivity to price for each group
alpha_b <- c(0.2, 0.5, 0.2)        # Preference for Black share (homophily for Black group)
alpha_w <- c(0.5, 0.2, 0.2)        # Preference for White share (homophily for White group)
alpha_o <- c(0.2, 0.2, 0.5)        # Preference for Other share (homophily for Other group)
alpha_inc <- c(0.2, 0.1, 0.15)     # Sensitivity to income
alpha_ph <- c(-0.1, -0.2, -0.15)   # Sensitivity to public housing
alpha_H <- c(0.1, 0.1, 0.1)        # Sensitivity to local housing units
theta_pref <- c(0.05, 0.03, 0.04)  # Sensitivity to other characteristics
alpha_hs <- c(0.1, 0.1, 0.1)  # preference for high-school educated share

# Supply elasticity
psi <- 1.0  # Example value

# Initialize neighborhood characteristics
p <- runif(J, 500, 1500)         # Initial housing prices
b <- runif(J, 0, 1)              # Share of Black residents
w <- runif(J, 0, 1 - b)          # Share of White residents
o <- 1 - b - w                   # Share of Other residents
inc <- runif(J, 20000, 60000)    # Median income
ph <- runif(J, 0, 0.2)           # Public housing share
H <- runif(J, 100, 1000)         # Number of local housing units
x <- runif(J)                    # Other characteristics
hs_share <- runif(J, 0, 1)       # Share of high-school educated residents 
theta_j <- runif(J, 10, 50)      # Supply shifter

# Initialize delta matrix (systematic utility component)
delta <- matrix(0, nrow=J, ncol=K)

# Iterative process to reach equilibrium
max_iter <- 1000
tolerance <- 1e-6
adjustment_factor <- 0.1

# Store initial prices
p_eq <- p

for (iter in 1:max_iter) {
  # Compute delta for each group incorporating homophily
  for (k in 1:K) {
      # Black households: stronger preference for Black share
      delta[, k] <- alpha_p[k] * log(p_eq) +
        alpha_b[k] * b +
        alpha_w[k] * w +
        alpha_o[k] * o +
        alpha_inc[k] * log(inc) +
        alpha_ph[k] * ph +
        alpha_H[k] * log(H) +
        alpha_hs[k] * hs_share +
        theta_pref[k] * x
  }
  
  # Compute choice probabilities using multinomial logit formula
  exp_delta <- exp(delta)
  denominator <- colSums(exp_delta)
  P_jk <- exp_delta / matrix(denominator, nrow=J, ncol=K, byrow=TRUE)
  
  # Compute total demand in each neighborhood
  D_j <- P_jk %*% N_k
  
  # Compute supply in each neighborhood using isoelastic supply function
  S_j <- theta_j * p_eq^psi
  
  # Check for convergence (demand close to supply)
  excess_demand <- D_j - S_j
  max_excess <- max(abs(excess_demand))
  
  if (max_excess < tolerance) {
    cat("Convergence achieved after", iter, "iterations.\n")
    break
  }
  
  # Adjust prices based on excess demand
  p_eq <- p_eq + adjustment_factor * (excess_demand / S_j) * p_eq
  p_eq <- pmax(p_eq, 0.1)  # Ensure prices stay positive
}

# Results
cat("Equilibrium Housing Prices:\n")
print(p_eq)

cat("\nTotal Demand in each neighborhood (D_j):\n")
print(D_j)

cat("\nTotal Supply in each neighborhood (S_j):\n")
print(S_j)

# Check maximum difference between demand and supply
max_diff <- max(abs(D_j - S_j))
cat("\nMaximum difference between demand and supply:", max_diff, "\n")

# Plot equilibrium prices
plot(1:J, p_eq, type='l', col='blue', lwd=2,
     xlab='Neighborhood', ylab='Equilibrium Price',
     main='Equilibrium Housing Prices by Neighborhood')




## counterfactuals -----

