library(haven)
library(tidyverse)

# Data on 1978-1983 cohorts
opp_insights_data_raw <- read_dta("data/raw/opportunity_insights/tract_outcomes_simple.dta")




# 
# outcomes: kfr_[race]_[gender]_p25
#Mean household income rank for children whose
# parents were at the 25th percentile of the national
# income distribution
# this is their measure of upward mobility... I could just use this
# Can I do weighted averages? Seems like what they do in their 2010 -> 2020 crosswalk 

# outcome: jail_[race]_[gender]_p25


# [race]_[gender]_count  
# Number of children under 18 living in the given tract
#with parents whose household income was below
# the national median. 
# They use this as a weighting variable...


# I need to 