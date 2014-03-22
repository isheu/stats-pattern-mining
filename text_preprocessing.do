// On omnius5
global filpath_provider "G:\ProjectsOnG\Safe_Rx\CPI\Common\Provider_Info\PECOS\201211\Full_2\AI"
global filpath_save "G:\ProjectsOnG\Safe_Rx\CPI\Development\Text_Field_Classification"

// Data preprocessing
use "${filpath_provider}/pec_enrlmt.dta", clear
keep *txt
drop if org_type_othr_txt == "" & othr_rsn_txt == "" & stus_cmt_txt == "" & cmt_txt == ""
gen all_txt = org_type_othr_txt + " " + othr_rsn_txt + " " + stus_cmt_txt + " " + cmt_txt
save "${filpath_save}/pec_enrlmt_txt.dta", replace

keep othr_rsn_txt
sort othr_rsn_txt
by othr_rsn_txt: keep if _n == 1
drop if othr_rsn_txt == ""
drop if _n == 1     // drop ",,,,,,,,"
replace othr_rsn_txt = itrim(othr_rsn_txt)
replace othr_rsn_txt = ltrim(othr_rsn_txt)
replace othr_rsn_txt = rtrim(othr_rsn_txt)
split othr_rsn_txt
save "${filpath_save}/othr_rsn_txt_split.dta", replace

use "${filpath_save}/pec_enrlmt_text.dta", clear
keep all_txt
sort all_txt
by all_txt: keep if _n == 1
replace all_txt = itrim(all_txt)
replace all_txt = ltrim(all_txt)
replace all_txt = rtrim(all_txt)
split all_txt
save "${filpath_save}/all_txt_split.dta", replace


use "${filpath_save}/othr_rsn_txt_split.dta", clear
keep othr_rsn_txt
sort othr_rsn_txt
by othr_rsn_txt: keep if _n == 1

gen training_txt = subinstr(othr_rsn_txt, "#", " ", .)
replace training_txt = subinstr(training_txt, "'", " ", .)
replace training_txt = subinstr(training_txt, "+", " ", .)
replace training_txt = regexr(training_txt, "[0-9]*-[0-9]*-[0-9]*", " ")
replace training_txt = regexr(training_txt, "[0-9]*/[0-9]*/[0-9]*", " ")

// L&T
replace training_txt = regexr(training_txt, "L&T", "LandT")
replace training_txt = regexr(training_txt, "L and T", "LandT")
replace training_txt = regexr(training_txt, "L & T", "LandT")
replace training_txt = regexr(training_txt, "l& t", "LandT")
replace training_txt = regexr(training_txt, "l&t", "LandT")
replace training_txt = regexr(training_txt, "l &t", "LandT")
replace training_txt = regexr(training_txt, "l & t", "LandT")
replace training_txt = regexr(training_txt, "l & T", "LandT")

replace training_txt = subinstr(training_txt, " - ", " ", .)
replace training_txt = subinstr(training_txt, " / ", " ", .)
replace training_txt = subinstr(training_txt, ".", " ", .)
replace training_txt = subinstr(training_txt, ",", " ", .)
replace training_txt = subinstr(training_txt, ";", " ", .)
replace training_txt = subinstr(training_txt, ":", " ", .)
replace training_txt = subinstr(training_txt, "*", " ", .)
replace training_txt = subinstr(training_txt, "¿", " ", .)

replace training_txt = trim(training_txt)
replace training_txt = itrim(training_txt)
replace training_txt = ltrim(training_txt)
replace training_txt = rtrim(training_txt)
replace training_txt = lower(training_txt)

// within
replace training_txt = regexr(training_txt, "w/in", "within")

replace training_txt = subinstr(training_txt, "-", " ", .)
replace training_txt = subinstr(training_txt, "/", " ", .)
replace training_txt = subinstr(training_txt, "&", " ", .)

// tie-in
replace training_txt = regexr(training_txt, "tie in", "tiein")

// req-no
replace training_txt = regexr(training_txt, "req no", "reqno")

// clean up
replace training_txt = regexr(training_txt, "cleanup", "clean up")

// infrequent
replace training_txt = regexr(training_txt, "infreq ", "infrequent ")

sort training_txt
drop if training_txt == ""
by training_txt: keep if _n == 1

outsheet othr_rsn_txt using othr_rsn_txt.txt, noq

// stus_cd == "11"
use "${filpath_provider}/pec_enrlmt.dta", clear
keep if stus_cd == "11"
keep othr_rsn_txt
sort othr_rsn_txt
drop if othr_rsn_txt == ""
* by othr_rsn_txt: keep if _n == 1

replace othr_rsn_txt = itrim(othr_rsn_txt)
replace othr_rsn_txt = ltrim(othr_rsn_txt)
replace othr_rsn_txt = rtrim(othr_rsn_txt)
replace othr_rsn_txt = lower(othr_rsn_txt)

gen training_txt = subinstr(othr_rsn_txt, "#", " ", .)
replace training_txt = subinstr(training_txt, "'", "", .)
* replace training_txt = subinstr(training_txt, "+", " ", .)
replace training_txt = regexr(training_txt, "[0-9]*-[0-9]*-[0-9]*", "dateindicator")
replace training_txt = regexr(training_txt, "[0-9]*/[0-9]*/[0-9]*", "dateindicator")

// L&T
replace training_txt = regexr(training_txt, "L&T", "LandT")
replace training_txt = regexr(training_txt, "l&t", "LandT")

replace training_txt = subinstr(training_txt, " - ", " ", .)
replace training_txt = subinstr(training_txt, " / ", " ", .)
replace training_txt = subinstr(training_txt, ".", " ", .)
replace training_txt = subinstr(training_txt, ",", " ", .)
replace training_txt = subinstr(training_txt, ";", " ", .)
replace training_txt = subinstr(training_txt, ":", " ", .)
replace training_txt = subinstr(training_txt, "*", " ", .)
replace training_txt = subinstr(training_txt, "¿", " ", .)

replace training_txt = regexr(training_txt, "yr", "year")
replace training_txt = regexr(training_txt, "re-", "re")

// within
replace training_txt = regexr(training_txt, "w/in", "within")

replace training_txt = subinstr(training_txt, "-", " ", .)
replace training_txt = subinstr(training_txt, "/", " ", .)
replace training_txt = subinstr(training_txt, "&", " ", .)

replace training_txt = regexr(training_txt, " req ", " request ")

// tie-in
replace training_txt = regexr(training_txt, "tie in", "tiein")

replace training_txt = regexr(training_txt, "cleanup", "clean up")
replace training_txt = regexr(training_txt, "blling", "billing")
replace training_txt = regexr(training_txt, "respons$", "response ")
replace training_txt = regexr(training_txt, " susp ", " suspension ")
replace training_txt = regexr(training_txt, " deactivation ", " deactivations ")

*save "`temp1'"

split training_txt

forvalues i = 1/8 {
    replace training_txt`i' = "license" if training_txt`i' == "lic"
    replace training_txt`i' = "license" if training_txt`i' == "lice"
    replace training_txt`i' = "license" if training_txt`i' == "licen"
    replace training_txt`i' = "license" if training_txt`i' == "licens"
    replace training_txt`i' = "license" if training_txt`i' == "licensed"
    replace training_txt`i' = "license" if training_txt`i' == "licensing"
    replace training_txt`i' = "license" if training_txt`i' == "licnese"
    replace training_txt`i' = "license" if training_txt`i' == "liscensed"
}

gen new_training_txt = training_txt1 + " " + training_txt2 + " " + training_txt3 + " " + training_txt4 + " " + training_txt5 + " " + training_txt6 + " " + training_txt7 + " " + training_txt8

replace new_training_txt = trim(new_training_txt)
replace new_training_txt = itrim(new_training_txt)
replace new_training_txt = ltrim(new_training_txt)
replace new_training_txt = rtrim(new_training_txt)

sort new_training_txt

save "${filpath_save}/new_training_txt_stus_11.dta", replace
outsheet new_training_txt using new_othr_rsn_txt_stus11.txt, noq


use "${filpath_save}/new_training_txt_stus_11.dta", clear

keep if training_txt1 == "license" | training_txt2 == "license" | training_txt3 == "license" | training_txt4 == "license" | training_txt5 == "license" | training_txt6 == "license" | training_txt7 == "license" | training_txt8 == "license"

save "${filpath_save}/training_txt_stus_11_license.dta", replace
outsheet new_training_txt using othr_rsn_txt_stus11_license.txt, noq



