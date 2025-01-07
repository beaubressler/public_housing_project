














global ind "A"

cap log close
global name "${ind}3_crime_t3_hete_ML_preparedata"
log using "${pfile}/Log/${name}", replace smcl


// *** // Preparing and exporting data for machina learning algorithm // *** //

use ${ind}1_crime, clear

keep if mass==1

keep if year >= 2012 & year <= 2014
replace t = t + 100
keep if t >= 97 & t <= 103
keep if gend==0

foreach var in suit { 
g p`var' = `var' >= 1
global outcomes "${outcomes} p`var'"
}

global outcomes "psuit"

keep year id cpf mun sbc mass mass_sh emp inc mon $outcomes t treat identificad dtb school dt_hir dt_dism remmedr ocup firmsize mass_sh

g post = t>=100
g posttreat = t>=100 & treat==1

// By age //
g agegr = 1 if year-year(dtb)>=18 & year-year(dtb)<30
replace agegr = 2 if year-year(dtb)>=30

g agecont = year-year(dtb)

// By education //
g auxschgr = school<12 if treat==1
replace auxschgr = 2 if school>=12 & treat==1
gegen schgr = max(auxschgr), by(year id)
drop auxschgr

g auxschcont = school if treat==1
gegen schcont = max(auxschcont), by(year id)
drop auxschcont

// By income //
replace remmedr = remmedr/622 if year==2012
replace remmedr = remmedr/678 if year==2013
replace remmedr = remmedr/724 if year==2014
gquantiles auxincgr = remmedr if treat==1, xtile n(2) // remmedr
gegen incgr = max(auxincgr), by(year id)
drop auxincgr

g auxinccont = remmedr if treat==1
gegen inccont = max(auxinccont), by(year id)
drop auxinccont

// By tenure //
g auxtencont = (year-year(dt_hir))*12 + month(dt_dism)-month(dt_hir) + (day(dt_dism)-day(dt_hir))/30 if treat==1
*replace auxtencont = auxtencont-1 if day(dt_dism+1)<day(dt_hir) & treat==1
gegen tencont = max(auxtencont), by(year id)
drop auxtencont

g tengr = tencont<=5
replace tengr = 2 if tencont>5 & tencont<=24
replace tengr = 3 if tencont>24 & tencont<=60
replace tengr = 4 if tencont>60

// By areas //
g reggr = int(mun/10^5)
g uf = int(mun/10^4)

gegen tagmun = tag(mun)
gegen taguf = tag(uf)

// By homicide rate //
merge m:1 mun using hom_ipea, keep(1 3) keepus(hom0916) nogen
rename hom0916 hom

// By per capita income - census 2010 //
merge m:1 mun using 0_ibge10_income, keep(1 3) keepus(pibpc10) nogen
rename pibpc10 pibpc

// By municipality level informality rate based 2010 Census //
merge m:1 mun using 0_informality_mun_2010, keep(1 3) keepus(inform) nogen

// By sector state-year employment growth dismissal year //
g cnae2 = int(sbc/10^5)
merge m:1 uf cnae2 year using 0_cnae2emp_ufyear, keep(1 3) keepus(totempcnae Ltotempcnae) nogen
g grtcnae = totempcnae / Ltotempcnae -1
drop totempcnae Ltotempcnae

g cnae5 = int(sbc/10^2)
merge m:1 uf cnae5 year using 0_cnae5emp_ufyear, keep(1 3) keepus(totempcnae Ltotempcnae) nogen
g grtcnaeb = totempcnae / Ltotempcnae -1
drop totempcnae Ltotempcnae

// By occupation state-year employment growth dismissal year //
g oc3 = int(ocup/10^3)
merge m:1 uf oc3 year using 0_oc3emp_ufyear, keep(1 3) keepus(totempocup Ltotempocup) nogen
g grtocup = totempocup / Ltotempocup -1
drop totempocup Ltotempocup

g oc4 = int(ocup/10^4)
merge m:1 uf oc4 year using 0_oc4emp_ufyear, keep(1 3) keepus(totempocup Ltotempocup) nogen
g grtocupb = totempocup / Ltotempocup -1
drop totempocup Ltotempocup

// population - municipality - census 2010 //
merge m:1 mun using 0_ibge10_income, keep(1 3) keepus(pop10) nogen
rename pop10 pop

// gini - municipality - census 2010 //
merge m:1 mun using 0_informality_mun_2010, keep(1 3) keepus(gini) nogen


* below/above the median over individuals
foreach x in hom pibpc inform grtcnae grtcnaeb grtocup grtocupb pop gini {
gquantiles aux`x'gr = `x' if treat==1, xtile n(2) // remmedr
gegen `x'gr = max(aux`x'gr), by(year id)
drop aux`x'gr
}

* below/above the median over municipalities
foreach x in hom pibpc inform grtcnae grtcnaeb grtocup grtocupb pop gini {
gquantiles aux`x'grb = `x' if tagmun==1, xtile n(2) // remmedr
gegen aux`x'grb2 = max(aux`x'grb), by(mun)
replace aux`x'grb2 = . if treat==0
gegen `x'grb = max(aux`x'grb2), by(year id)
drop aux`x'grb aux`x'grb2
}

* expand continuous var to control units
foreach x in hom pibpc inform grtcnae grtcnaeb grtocup grtocupb pop gini {
g aux`x'cont = `x' if treat==1
gegen `x'cont = max(aux`x'cont), by(year id)
drop aux`x'cont
}


// By state level unemployment rate at the time of the dismissal //
merge m:1 uf year using 0_informality_ufyear, keep(1 3) keepus(unemp) nogen
gquantiles auxunempgr = unemp if treat==1, xtile n(2) // remmedr
gegen unempgr = max(auxunempgr), by(year id)
drop auxunempgr

gquantiles auxunempgrb = unemp if taguf==1, xtile n(2) // remmedr
gegen auxunempgrb2 = max(auxunempgrb), by(uf)
replace auxunempgrb2 = . if treat==0
gegen unempgrb = max(auxunempgrb2), by(year id)
drop auxunempgrb auxunempgrb2

g auxunempcont = unemp if treat==1
gegen unempcont = max(auxunempcont), by(year id)
drop auxunempcont


g psuitpost = psuit if post==1
g psuitpre = psuit if post==0
g psuitpre1 = psuit if post==0 & t==99

gcollapse (mean) psuitpost psuitpre psuitpre1 (min) agecont tencont schcont inccont homcont informcont grtcnaecont grtocupcont unempcont pibpccont popcont ginicont, by(year id cpf treat mun)

g psuit = psuitpost - psuitpre 

g uf = int(mun/10^4)

// dropping few missings value
drop if homcont==.
drop if grtocupcont==.
drop if grtcnaecont==.
su

export delimited using "${ind}3_crime_t3_hete_ML.csv", replace nolabel


cap log close
