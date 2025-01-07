*** Sdid demo  from Clarke et al (2023)***
webuse set www.damianclarke.net/stata/
webuse quota_example.dta, clear


** standard run of sdid
*sdid womparl country year quota, vce(bootstrap) seed(1213)

******
*  How to create Event study plot for a single treatment year
******

egen m=min(year) if quota == 1, by(country) // Indicator for year of adoption
egen mm = mean(m), by(country)
keep if mm == 2002 |mm==. // Keep only one time of adoption
drop if lngdp ==. //keep if covariates observed


* Run standard SDID procedure
qui sdid womparl country year quota, vce(noinference) graph g2_opt(ylab(-5(5)20) ///
ytitle("Women in Parliament") scheme(sj)) graph_export(groups, .pdf) ///
covariates(lngdp, projected)

matrix lambda = e(lambda)[1..12,1] //save lambda weight
matrix yco = e(series)[1..12,2] //control baseline
matrix ytr = e(series)[1..12,3] //treated baseline
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference)[1..26,1..2] // Store Ytr-Yco
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)


* block boostrapping
local b = 1
local B = 100
while `b' <=`B' {
	preserve
	bsample, cluster(country) idcluster(c2)
	qui count if quota == 0
	local r1 = r(N)
	qui count if quota != 0
	local r2 = r(N)
	if (`r1' !=0 & `r2' !=0) {
		qui sdid womparl c2 year quota, vce(noinference) graph covariates(lngdp, projected)
		matrix lambda_b = e(lambda)[1..12,1] //save lambda weight
		matrix yco_b = e(series)[1..12,2] //control baseline
		matrix ytr_b = e(series)[1..12,3] //treated baseline
		matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
		matrix meanpre_b = J(26,1,aux_b[1,1])
		matrix d`b' = e(difference)[1..26,2] - meanpre_b
		local ++b
	}
	restore
}

* generate confidence intervals
preserve
keep time d
keep if time!=.
forval b=1/`B' {
svmat d`b' // import each bootstrap replicate of difference between trends
}
egen rsd = rowsd(d11 - d`B'1) //calculate standard deviation of this difference
gen LCI = d + invnormal(0.025)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.975)*rsd //upper bounds on bootstrap CIs
*generate plot

tw rarea UCI LCI time, color(gray%40) || scatter d time, color(blue) m(d) ///
xtitle("") ytitle("Women in Parliament") xlab(1990(1)2015, angle(45)) ///
legend(order(2 "Point Estimate" 1 "95% CI") pos(12) col(2)) ///
xline(2002, lc(black) lp(solid)) yline(0, lc(red) lp(shortdash)) ///
scheme(sj)
graph export "event_sdid.pdf", replace
restore


***** Testing SDID ********


use "/Users/beaubressler/Library/CloudStorage/Dropbox/Research/public_housing/data/derived/nyc_tracts_synthdid_data.dta", clear

* for some reason, black_share missing in 1960 for this 
drop if unit_id == "360610031"


/* commenting out for now: this shows some basics
* run sdid
sdid black_share unit_id YEAR treated, vce(placebo) reps(50) seed(123) ///
     graph g1on g1_opt(xtitle(""))  
	 
	 
* run standard synthetic control
sdid black_share unit_id YEAR treated, method(sc) vce(placebo) reps(50) seed(123) ///
     graph g1on g1_opt(xtitle(""))  
	 
* run diff-in-diff
sdid black_share unit_id YEAR treated, method(did) vce(placebo) reps(50) seed(123) ///
     graph g1on g1_opt(xtitle(""))  
*/
	 
***** Create event-study plot *****
sdid black_share unit_id YEAR treated, vce(placebo) reps(5) seed(123) ///
     graph g1on g1_opt(xtitle(""))  
	 


matrix lambda = e(lambda)[1..4,1] //save lambda weight */
matlist lambda 
matrix yco = e(series)[1..4,2] //control baseline
matrix ytr = e(series)[1..4,3] //treated baseline
matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
scalar meanpre_o = aux[1,1]
matrix difference = e(difference) // Store Ytr-Yco
svmat difference
ren (difference1 difference2) (time d)
replace d = d - meanpre_o // Calculate vector in (8)



* block boostrapping
local b = 1
local B = 2000
while `b' <=`B' {
	preserve
	bsample, cluster(unit_id) idcluster(c2)
	qui count if treated == 0
	local r1 = r(N)
	qui count if treated != 0
	local r2 = r(N)
	if (`r1' !=0 & `r2' !=0) {
		qui sdid black_share c2 YEAR treated, vce(noinference) graph 
		matrix lambda_b = e(lambda)[1..4,1] //save lambda weight
		matrix yco_b = e(series)[1..4,2] //control baseline
		matrix ytr_b = e(series)[1..4,3] //treated baseline
		matrix aux_b = lambda_b'*(ytr_b - yco_b) //calculate the pre-treatment mean
		matrix meanpre_b = J(7,1,aux_b[1,1])
		matrix d`b' = e(difference)[1..7,2] - meanpre_b
		local ++b
	}
	restore
}

* generate confidence intervals
preserve
keep time d
keep if time!=.
forval b=1/`B' {
svmat d`b' // import each bootstrap replicate of difference between trends
}
egen rsd = rowsd(d11 - d`B'1) //calculate standard deviation of this difference
gen LCI = d + invnormal(0.025)*rsd //lower bounds on bootstrap CIs
gen UCI = d + invnormal(0.975)*rsd //upper bounds on bootstrap CIs
*generate plot

tw rarea UCI LCI time, color(gray%40) || scatter d time, color(blue) m(d) ///
xtitle("") ytitle("Effect of public housing on black pop share")  xlab(1940(10)1990, angle(45)) ///
legend(order(2 "Point Estimate" 1 "95% CI") pos(12) col(2)) ///
xline(1970, lc(black) lp(solid)) yline(0, lc(red) lp(shortdash)) ///
scheme(sj)

graph export "event_sdid.pdf", replace
restore

****
* Ru
****
