*****
* Testing FE model 
******


*import delimited "/Users/beaubressler/Library/CloudStorage/Dropbox/Research/public_housing/data/derived/merged/event_study_data_rings_geocoded.csv", clear


import delimited "/Users/beaubressler/Library/CloudStorage/Dropbox/Research/public_housing/data/derived/merged/event_study_data_rings.csv", clear

* prep 

encode location_type_factor, gen(loc_type_fact)
gen event_time_plus_50 = event_time + 50

gen post_treat = event_time >= 0

*** Try with 0s and 1s
gen treated_group = (location_type != "outer")


*drop if location_type == "inner"


*keep if city == "New York City"



*** Run regressions ****
/*
reghdfe black_share ib2.loc_type_fact##ib40.event_time_plus_50, absorb(treated_id##loc_type_fact treated_id##year tract_id) cluster(tract_id treated_id)


coefplot,  baselevels cirecast(rcap) keep(*.event_time*)
*/


* Treated vs outer ring
reghdfe black_share treated_group##ib40.event_time_plus_50 if location_type != "inner", absorb(treated_id##loc_type_fact treated_id##year tract_id) cluster(treated_id tract_id)

coefplot, omitted baselevels cirecast(rcap) keep(1.treated_group#*) ///
rename(1.treated_group#([0-9]+).event_time_plus_50 = \1, regex) at(_coef)


* Inner vs outer ring
reghdfe black_share treated_group##ib40.event_time_plus_50 if location_type != "treated", absorb(treated_id##loc_type_fact treated_id##year tract_id) cluster(treated_id tract_id)

coefplot, omitted baselevels cirecast(rcap) keep(1.treated_group#*) ///
rename(1.treated_group#([0-9]+).event_time_plus_50 = \1, regex) at(_coef)


*** New York Only
reghdfe black_share treated_group##ib40.event_time_plus_50 if location_type != "inner" & city == "New York City", absorb(treated_id##treated_group treated_id##year tract_id) cluster(treated_id tract_id)

coefplot, omitted baselevels cirecast(rcap) keep(1.treated_group#*) ///
rename(1.treated_group#([0-9]+).event_time_plus_50 = \1, regex) at(_coef)

reghdfe black_share treated_group##ib40.event_time_plus_50 if location_type != "treated" & city == "New York City", absorb(treated_id##loc_type_fact treated_id##year tract_id) cluster(treated_id tract_id)

coefplot, omitted baselevels cirecast(rcap) keep(1.treated_group#*) ///
rename(1.treated_group#([0-9]+).event_time_plus_50 = \1, regex) at(_coef)





**** Total population*****

* Treated vs outer ring
reghdfe asinh_pop_total treated_group##ib40.event_time_plus_50 if location_type != "inner", absorb(treated_id##loc_type_fact treated_id##year tract_id) cluster(treated_id tract_id)

coefplot, omitted baselevels cirecast(rcap) keep(1.treated_group#*) ///
rename(1.treated_group#([0-9]+).event_time_plus_50 = \1, regex) at(_coef)


* Inner vs outer ring
reghdfe asinh_pop_total treated_group##ib40.event_time_plus_50 if location_type != "treated", absorb(treated_id##loc_type_fact treated_id##year tract_id) cluster(treated_id tract_id)

coefplot, omitted baselevels cirecast(rcap) keep(1.treated_group#*) ///
rename(1.treated_group#([0-9]+).event_time_plus_50 = \1, regex) at(_coef)


*** New York Only
reghdfe asinh_pop_total treated_group##ib40.event_time_plus_50 if location_type != "inner" & city == "New York City", absorb(treated_id##treated_group treated_id##year tract_id) cluster(treated_id tract_id)

coefplot, omitted baselevels cirecast(rcap) keep(1.treated_group#*) ///
rename(1.treated_group#([0-9]+).event_time_plus_50 = \1, regex) at(_coef)

reghdfe asinh_pop_total treated_group##ib40.event_time_plus_50 if location_type != "treated" & city == "New York City", absorb(treated_id##loc_type_fact treated_id##year tract_id) cluster(treated_id)

rename(1.treated_group#([0-9]+).event_time_plus_50 = \1, regex) 

coefplot, omitted baselevels cirecast(rcap) keep(1.treated_group#*) ///
rename(1.treated_group#([0-9]+).event_time_plus_50 = "`:di `=\1-50''", regex) at(_coef) 

xtitle("Event Time") ytitle("DiD Coefficient") ///
xline(0, lpattern(dash) lcolor(red)) yline(0, lpattern(dash)) ///
xlabel(-50(10)50) ///
title("Effect on Population") ///
note("Event time 0 represents the treatment year")

