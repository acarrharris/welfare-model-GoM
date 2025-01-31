

cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\"
import delimited using "projection catch per trip.csv", clear 


keep if state==25 & mode=="fh" & area=="offshore"

tostring decade, gen(decade1)
tostring month, gen(month1)

gen decade_month=decade1+"_"+month1 

tabstat cod_ind_clayton had_ind_clayton, stat(mean) by(decade_month)


import delimited  "correlated_sample_Clayton_MA_Area2.csv", clear
keep if mode1==1

tostring decade, gen(decade1)
tostring month, gen(month1)
gen decade_month=decade1+"_"+month1 
tabstat cod_ind had_ind, stat(mean) by(decade_month)

cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\output_data\"
import excel using  "decadal_proj__clayton_ind.xlsx", clear first


split period2, parse(_)
keep if period21=="fh" & period23=="offshore" & period24=="25" 

gen cod_cpt=cod_catch_sum/ ntrips_alt_sum
gen had_cpt=hadd_catch_sum/ ntrips_alt_sum
replace ntrips_alt_sum=round(ntrips_alt_sum)


gen month=1 if inlist(period22, "1", "2")
replace month=1 if inlist(period22, "3", "4")
replace month=3 if inlist(period22, "5", "6")
replace month=4 if inlist(period22, "7", "8")
replace month=5 if inlist(period22, "9", "10")
replace month=6 if inlist(period22, "11", "12")
replace month=7 if inlist(period22, "13", "14")
replace month=8 if inlist(period22, "15", "16")
replace month=9 if inlist(period22, "16", "18")
replace month=10 if inlist(period22, "19", "20")
replace month=11 if inlist(period22, "21", "22")
replace month=12 if inlist(period22, "23", "24")


tostring decade, gen(decade1)
tostring month, gen(month1)
gen decade_month=decade1+"_"+month1 

asgen w_cod_cpt =  cod_cpt, w(ntrips_alt_sum) by(decade_month)
asgen w_hadd_cpt =  had_cpt, w(ntrips_alt_sum) by(decade_month)
keep decade_month  w_cod_cpt w_hadd_cpt
duplicates drop 
sort decade_month








cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\output_data\"
import excel using  "decadal_proj__clayton_ind.xlsx", clear first
split period2, parse(_)

rename period21 mode 
rename period23 area 
rename period24 state
collapse (sum) cv_sum cv_sum_prob cod_keep_sum hadd_keep_sum cod_rel_sum hadd_rel_sum cod_catch_sum hadd_catch_sum ntrips_alt_sum , by(mode area state draw)
collapse (mean) cv_sum cv_sum_prob cod_keep_sum hadd_keep_sum cod_rel_sum hadd_rel_sum cod_catch_sum hadd_catch_sum ntrips_alt_sum , by(mode area state )

local vars cv_sum cv_sum_prob cod_keep_sum hadd_keep_sum cod_rel_sum hadd_rel_sum cod_catch_sum hadd_catch_sum ntrips_alt_sum 
foreach v of local vars{
	egen sum_`v'=sum(`v')
	gen perc_`v'=`v'/sum_`v'
	drop sum_`v'
}



}


