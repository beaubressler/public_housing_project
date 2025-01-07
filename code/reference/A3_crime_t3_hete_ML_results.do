














global ind "A"

cap log close
global name "${ind}3_crime_t3_hete_ML_results"
log using "${pfile}/Log/${name}", replace smcl

* import predicted CATE after running causal trees in R

clear
use A3_crime_t3_hete_ML_output.dta, clear
replace pibpccont = pibpccont/1000

global vars "agecont tencont schcont inccont homcont informcont grtcnaecont grtocupcont pibpccont popcont ginicont unempcont"

global nboot = 1000

replace preds = preds / (y_pretreat + delta_control)
replace sqrt = sqrt / (y_pretreat + delta_control)
g loci = preds - 1.965*sqrt
g hici = preds + 1.965*sqrt

g t = preds/sqrt
su t, d
g sig5 = abs(t)>=1.96
g sig10 = abs(t)>=1.65
* differences below and above the median predicted effect
xtile q2 =preds, nq(2)
g t2 = q2==2

// *** // Table 2 // *** //

stddiff ${vars} , by(t2)
gstats tab ${vars} , by(t2) stat(mean) columns(stat)

timer clear
timer on 1
mhtexp ${vars} , treatment(t2) bootstrap(${nboot}) // bootstrap(1000) 1000 is best
timer off 1
timer list


label var agecont "Age"
label var tencont "Tenure months"
label var schcont "Years of schooling"
label var inccont "Av. Earnings (min wage)"
label var homcont "Homicide Rate"
label var popcont "Municipality Pop."
label var informcont "Informality Rate"
label var grtocupcont "Occupation Growth Rate"
label var grtcnaecont "Sector Growth Rate"
label var unempcont "Unemployment Rate"
label var pibpccont "Income p.c. (R$1000)"
label var ginicont "Gini Index"

* graphs
global nbin = 10
foreach x in $vars {
	binscatter preds `x', line(none) title(`: var label `x'', size(medlarge))  m(t) n($nbin) xtitle("") ytitle("")  ylabels(0 .2 .4 .6) name(`x', replace) graphregion(fcolor(white) color(white))
}

graph combine agecont tencont schcont inccont homcont informcont grtocupcont pibpccont ginicont, ///
title("") name(out, replace) rows(3) xsize(10) ysize(10) plotregion(margin(-1 0 -1 -1 )) imargin(vsmall) graphregion(fcolor(white) color(white)) // xsize(7) ysize(10)
graph export "${pgraph}/A3/A3_ML_main.png", replace name(out)
graph save out "${pgraph}/A3/A3_ML_main", replace


* CATE over rank
graph close
sort preds
gquantiles predsxt = preds , xtile n(100)
gegen ate = mean(preds)
gegen avpreds = mean(preds), by(predsxt)
gegen p50preds = pctile(preds), p(50) by(predsxt)
gegen p5preds = pctile(preds), p(5) by(predsxt)
gegen p25preds = pctile(preds), p(25) by(predsxt)
gegen p75preds = pctile(preds), p(75) by(predsxt)
gegen p95preds = pctile(preds), p(95) by(predsxt)
gegen avloci = mean(loci), by(predsxt)
gegen avhici = mean(hici), by(predsxt)
gegen avsig5 = mean(sig5), by(predsxt)
gegen avsig10 = mean(sig10), by(predsxt)
gegen tagpreds = tag(predsxt)

su sig5 sig10

twoway (scatter avpreds predsxt if tagpreds==1 & predsxt>=1&predsxt<=100, msize(tiny) mcolor(blue)) ///
(line ate predsxt if tagpreds==1 & predsxt>=1&predsxt<=100, lp(dash) lcolor(gs12) ) ///
, ytitle("", axis(1) size(small)) ///
legend(order(1 "Av. Treatment Effect" 2 "Sample Mean Effect" ) r(1)) ///
xtitle(Predicted Treatment Effect - Percentile) graphregion(fcolor(white) color(white)) ylabels(0 .3 .6 .9 1.2, gmin gmax grid) //yline(0, lcolor(black)) 
graph export "${pgraph}/A3/A3_ML_caterank_c.png", replace
graph save "${pgraph}/A3/A3_ML_caterank_c", replace


* heatmap
graph close
global nq = 10
cap drop q_*
gquantiles q_age =agecont, xtile nq($nq)
gquantiles q_ten= tencont, xtile nq($nq)
gquantiles q_remmedr= inccont, xtile nq($nq)
gquantiles q_hom = homcont, xtile nq($nq)
gquantiles q_inf = informcont, xtile nq($nq)
gquantiles q_unemp = unempcont, xtile nq($nq)
gquantiles q_pibpc = pibpccont, xtile nq($nq)

g q_sch = 1 if schcont<=5
replace q_sch = 2 if schcont>5 & schcont<=9
replace q_sch = 3 if schcont>9 & schcont<=12
replace q_sch = 4 if schcont>12
label var q_sch "Education Level"


label var preds "Predicted CATE"

heatplot preds q_age q_ten, discrete(1) statistic(mean)  ylabel(1(1)10, labsize(small)) xlabel(1(1)10, labsize(small))  cut(.12(.02).58)  /// 
ytitle("Age Decile", size(med))  xtitle("Tenure Decile", size(med)) colors(magma, reverse gs) graphregion(fcolor(white) color(white)) leg(subtitle("")) ///
name(heat_ageten, replace)
graph export "${pgraph}/A3/A3_ML_heatmap_ageten.png", replace name(heat_ageten)
//lev(15)

heatplot preds q_hom q_inf, discrete(1) statistic(mean) ylabel(1(1)10, labsize(small)) xlabel(1(1)10, labsize(small)) cut(.12(.02).58)  ///
ytitle("Homicide Rate", size(med))  xtitle("Informality Rate", size(med)) colors(magma, reverse gs) graphregion(fcolor(white) color(white)) leg(subtitle("")) ///
name(heat_hominf, replace)
graph export "${pgraph}/A3/A3_ML_heatmap_hominf.png", replace name(heat_hominf)

heatplot preds q_remmedr q_ten , discrete(1) statistic(mean) ylabel(1(1)10, labsize(small)) xlabel(1(1)10, labsize(small)) cut(.12(.02).58)  ///
xtitle("Tenure Decile", size(med))  ytitle("Income Decile", size(med)) colors(magma, reverse gs) graphregion(fcolor(white) color(white)) leg(subtitle("")) ///
name(heat_incten, replace)

heatplot preds q_sch q_ten, discrete(1) statistic(mean)  xlabel(1(1)10, labsize(small)) ylabel(1 "Elem." 2 "Middle" 3 "High" 4 "College", labsize(small)) cut(.12(.02).58)  ///
xtitle("Tenure Decile", size(med)) ytitle("Education Level", size(med)) colors(magma, reverse gs) graphregion(fcolor(white) color(white)) leg(subtitle("")) ///
name(heat_schoolten, replace)

heatplot preds q_remmedr q_age, discrete(1) statistic(mean) ylabel(1(1)10, labsize(small)) xlabel(1(1)10, labsize(small)) cut(.12(.02).58)  ///
xtitle("Age Decile", size(med))  ytitle("Income Decile", size(med)) colors(magma, reverse gs) graphregion(fcolor(white) color(white)) leg(subtitle("")) ///
name(heat_incage, replace)

heatplot preds q_sch q_age, discrete(1) statistic(mean)  xlabel(1(1)10, labsize(small)) ylabel(1 "Elem." 2 "Middle" 3 "High" 4 "College", labsize(small)) cut(.12(.02).58)  ///
xtitle("Age Decile", size(med)) ytitle("Education Level", size(med)) colors(magma, reverse gs) graphregion(fcolor(white) color(white)) leg(subtitle("")) ///
name(heat_schoolage, replace)


graph combine heat_ageten heat_hominf , ///
title("") name(out, replace) rows(1)  plotregion(margin(-1 -1 -1 -1 )) imargin(med) graphregion(fcolor(white) color(white)) xsize(12) ysize(6)
graph export "${pgraph}/A3/A3_ML_heat_main_a.png", replace name(out)
graph save out "${pgraph}/A3/A3_ML_heat_main_a.gph", replace

grc1leg heat_incage heat_schoolage heat_incten heat_schoolten, ///
pos(3) ///
title("") name(out, replace) rows(2)  plotregion(margin(-1 -1 -1 -1 )) imargin(med) graphregion(fcolor(white) color(white)) // xsize(7) ysize(10)
graph export "${pgraph}/A3/A3_ML_heat_main_b.png", replace name(out)
graph save out "${pgraph}/A3/A3_ML_heat_main_b.gph", replace


graph close


cap log close
