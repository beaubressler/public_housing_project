*********
* Synthetic Differnence-in-Differences: Black pop share, no controls
*********

global data_dir "/Users/beaubressler/Library/CloudStorage/Dropbox/Research/public_housing/data/derived/data_for_sdid/"
global graph_dir "/Users/beaubressler/Library/CloudStorage/Dropbox/Research/public_housing/output/figures/sdid/"

global event_study_dir "${graph_dir}event_study/"

* 1940-1980 separately
forvalues i = 1940(10)1980 {
	
	* define number of preperiods
	* 1940: 1 (lambda), 1-7 (series)
	* 1960: 1-3 (lambda), 1-7 (series)
	
	* 1970: 1-4... (lambda), 1-7 (series)
		
	local num_pre_periods = (`i' - 1930)/10

	foreach x in treated inner outer {
		
		local i 1950
		local x inner
		local num_pre_periods = (`i' - 1930)/10

		
		use "${data_dir}`x'_synth_did_data_`i'.dta", clear

		* for some reason, black_share missing in 1960 for this 
		drop if unit_id == "360610031"
		
		* use city fixed effects as covariate 
		encode city, gen(city_num)

		sdid black_share unit_id YEAR treated, vce(placebo) ///
			covariates(city_num total_pop population_density distance_from_cbd) reps(20) seed(123) ///
			 graph g1_opt(xtitle(""))  
			 
		graph export "${graph_dir}`x'_black_share_sdid_no_controls_`i'.pdf", replace

		
		* Create event study plot
			
		matrix lambda = e(lambda)[1..`num_pre_periods',1] //save lambda weight */
		matlist lambda 
		matrix yco = e(series)[1..`num_pre_periods',2] //control baseline
		matrix ytr = e(series)[1..`num_pre_periods',3] //treated baseline
		matrix aux = lambda'*(ytr - yco) //calculate the pre-treatment mean
		scalar meanpre_o = aux[1,1]
		matrix difference = e(difference) // Store Ytr-Yco
		svmat difference
		ren (difference1 difference2) (time d)
		replace d = d - meanpre_o // Calculate vector in (8)




	* block boostrapping (set B = number of boostraps)
		local b = 1
		local B = 50
		while `b' <=`B' {
			preserve
			bsample, cluster(unit_id) idcluster(c2)
			qui count if treated == 0
			local r1 = r(N)
			qui count if treated != 0
			local r2 = r(N)
			if (`r1' !=0 & `r2' !=0) {
				qui sdid black_share c2 YEAR treated, vce(noinference) covariates(city_num total_pop population_density) graph 
				matrix lambda_b = e(lambda)[1..`num_pre_periods',1] //save lambda weight
				matrix yco_b = e(series)[1..`num_pre_periods',2] //control baseline
				matrix ytr_b = e(series)[1..`num_pre_periods',3] //treated baseline
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
		xline(`i', lc(black) lp(solid)) yline(0, lc(red) lp(shortdash)) ///
		scheme(sj)

		graph export "${event_study_dir}`x'_black_share_sdid_no_controls_event_study_`i'.pdf", replace
		restore
	}
}
	

*** 1940-1980, pooled: ****

foreach x in treated inner outer {

	use "${data_dir}`x'_synth_did_data_1940_1980.dta", clear

	drop if unit_id == "360610031"

	* use city fixed effects as covariate 
	encode city, gen(city_num)

	sdid black_share unit_id YEAR treated, vce(placebo) covariates(city_num) reps(5) seed(123) ///
		 graph g1_opt(xtitle(""))  

	* try other variables
	sdid population_density unit_id YEAR treated, vce(placebo) covariates(city_num) reps(5) seed(123) ///
		 graph g1_opt(xtitle(""))  

	sdid total_pop unit_id YEAR treated, vce(placebo) covariates(city_num) reps(5) seed(123) ///
		 graph g1_opt(xtitle(""))  


	* median rent: create balanced panel
	* TODO: Create different balanced panel datasets in R to use in Stata 
	drop if missing(median_rent)

	encode unit_id, gen(unit_id_num)
	xtset unit_id_num YEAR

	xtdescribe
	xtsum
	keep if `r(T)' == `r(Tbar)'

	sdid median_rent unit_id YEAR treated, vce(placebo) covariates(city_num) reps(5) seed(123) ///
		 graph g1_opt(xtitle(""))  



}

* test placebo from donor pool...
* not ideal that I find a positive effect
	use "${data_dir}placebo_1950.dta", clear

	drop if unit_id == "360610031"

		encode city, gen(city_num)

		sdid black_share unit_id YEAR treated, method(did) vce(placebo) covariates(total_pop population_density distance_from_cbd) reps(10) seed(123) ///
			 graph g1_opt(xtitle(""))  
