/*******
* This do file cleans the PSH 1977 file provided to me by Yana Kucheva

In particular, I will label the file, and select only the variables I need
*******/

global raw_data_dir "/Users/beaubressler/Library/CloudStorage/Dropbox/Research/public_housing/data/raw/pic77/pic77_yana/"
global int_data_dir "/Users/beaubressler/Library/CloudStorage/Dropbox/Research/public_housing/data/derived/public_housing/intermediate/"

use ${raw_data_dir}pic77.dta, clear

* Select variables of interest
keep LSTATE LPROJ1 LPROJ2 LPROJ3 LOCPAT LSTAGE LDESIN LSTAGE TSTATE LUNTTO LWHTTO LBLCTO LINDTO LHSPTO LASITO LMINTO

label variable LWHTTO "# of subsidized households, White non-minority"
label variable LBLCTO "# of subsidized households, Black"
label variable LINDTO "# of subsidized households, American Indian"
label variable LHSPTO "# of subsidized households, Hispanic"
label variable LASITO "# of subsidized households, Oriental"
label variable LMINTO "# of subsidized households, Other Minority"

label variable LSTAGE "Project Stage"
label variable LUNTTO	"Total subsidized tenants"


*** label "occupancy pattern code"
label variable LOCPAT "Occupancy Pattern Code"

* First, create a new numeric variable, e.g., LOCPAT_code
gen LOCPAT_code = .

* Assign numeric codes based on the string values of LOCPAT
replace LOCPAT_code = 1 if LOCPAT == "A1" // All White non-minority
replace LOCPAT_code = 2 if LOCPAT == "A2" // All Black
replace LOCPAT_code = 3 if LOCPAT == "A3" // All American Indians
replace LOCPAT_code = 4 if LOCPAT == "A4" // All Hispanics
replace LOCPAT_code = 5 if LOCPAT == "A5" // All Oriental
replace LOCPAT_code = 6 if LOCPAT == "A6" // All Other Minorities
replace LOCPAT_code = 7 if LOCPAT == "B0" // Multi-Site
replace LOCPAT_code = 8 if LOCPAT == "B1" // Other Limitations
replace LOCPAT_code = 9 if LOCPAT == "C1" // White non-minority & Black
replace LOCPAT_code = 10 if LOCPAT == "C2" // White non-minority & American Indian
replace LOCPAT_code = 11 if LOCPAT == "C3" // White & Hispanic
replace LOCPAT_code = 12 if LOCPAT == "C4" // White non-minority & Oriental
replace LOCPAT_code = 13 if LOCPAT == "C5" // White non-minority & Other Minorities
replace LOCPAT_code = 14 if LOCPAT == "C6" // White non-minority, Black & Hispanic only
replace LOCPAT_code = 15 if LOCPAT == "C7" // White non-minority, & any other combination not indicated above
replace LOCPAT_code = 16 if LOCPAT == "D1" // No White non-minority & Some Black
replace LOCPAT_code = 17 if LOCPAT == "D2" // No White non-minority & No Black
replace LOCPAT_code = 18 if LOCPAT == "99" // N.A


* Optionally, you can label these numeric codes for clarity
label define LOCPAT_code_lbl 1 "All White non-minority" 2 "All Black" 3 "All American Indians" ///
  4 "All Hispanics" 5 "All Oriental" 6 "All Other Minorities" 7 "Multi-Site" 8 "Other Limitations" ///
  9 "White non-minority & Black" 10 "White non-minority & American Indian" 11 "White & Hispanic" ///
  12 "White non-minority & Oriental" 13 "White non-minority & Other Minorities" ///
  14 "White non-minority, Black & Hispanic only" 15 "White non-minority, & any other combination not indicated above" ///
  16 "No White non-minority & Some Black" 17 "No White non-minority & No Black" 18 "N/A"
label values LOCPAT_code LOCPAT_code_lbl

label variable LOCPAT_code "Occupancy Pattern Code"



*** label "Elderly "
label variable LDESIN "Initial Project Designation (Elderly)"

label define LDESIN_lbl 0 "All Elderly" 1 "Some Elderly" 2 "No Elderly" 9 "N/A"

label values LDESIN LDESIN_lbl


* output 

