

cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\"
*make a time series of historical catch per trip 
cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\"
u "cod_hadd_catch_data_1_15.dta", clear 





*2021 catch data from MRIP 
u "cod_hadd_catch_data_1_15.dta", clear 
keep if year==2021
su tot_cat_cod //mean= 1.615, SD=2.69
su tot_cat_hadd //mean=3.61, SD=5.83

*calibration 2021 catch - from .rds: calibration_data_2021drawX" 
//mean(combined_calibration$codcattrip) - 1.626903
//sd(combined_calibration$codcattrip) -0.006598431
//mean(combined_calibration$haddcattrip)- 3.614725
//sd(combined_calibration$haddcattrip) - 0.01382403

*from Jorge's files
tempfile one two three four
cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\correlated_data\4-17-2025"

import delimited  "correlated_sample_Clayton_Area1_newmode2.csv", clear
gen source="correlated_sample_Clayton_Area1_newmode2"
save `one', replace 

import delimited  "correlated_sample_Clayton_Area1_newmode3.csv", clear
gen source="correlated_sample_Clayton_Area1_newmode3"
save `two', replace 

import delimited  "correlated_sample_Clayton_Area2_newmode2.csv", clear
gen source="correlated_sample_Clayton_Area2_newmode2"
save `three', replace 

import delimited  "correlated_sample_Clayton_Area2_newmode3.csv", clear
gen source="correlated_sample_Clayton_Area2_newmode3"
append using `one'
append using `two'
append using `three'

sort decade month 
gen state=25 if ma==1
replace state=23 if me==1
replace state=33 if nh==1
split source, parse(_)

replace cod_corr=156 if cod_corr>156
replace cod_ind=156 if cod_ind>156
replace had_corr=77 if had_corr>77
replace had_ind=77 if had_ind>77

gen area="inshore" if  source4 =="Area1"
replace  area="offshore"  if  source4 =="Area2"
gen mode="fh" if  source5=="newmode2"
replace mode="pr" if  source5=="newmode3"
drop season*
drop average_temp cod_ssb had_ssb ma me nh newmode2 newmode3 ffdays12 prob_cod observed_cod prob_had observed_had predicted_cod predicted_had
drop source2 source4 source5
collapse (mean) cod_corr cod_ind had_corr had_ind, by(decade month mode area state)
tempfile proj
save `proj', replace 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2021
rename st state
collapse (sum) dtrip, by(month mode area state)
merge 1:m month mode area state using `proj'
drop if _merge==1
drop _merge

collapse (mean) cod_corr cod_ind had_corr had_ind [aweight=dtrip], by(decade)
/*
decade		cod_corr	cod_ind		had_corr		had_ind
1				3.40648	3.429302	2.574332		2.920153
2				2.962109	2.985154	2.69962		3.011688
3				2.608941	2.597245	2.799984		3.099701
4				2.61735	2.626098	2.810998		3.098629
5				2.019003	2.004134	3.037913		3.374606
6				1.738697	1.726143	4.188794		4.592289
7				1.790609	1.782555	4.270209		4.714401
8				1.768788	1.764612	4.183298		4.593813
*/

*from Lou's input files which are reformatted version of Jorge's files
import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_draw100.csv", clear  
keep month mode area state *clayton*
collapse (mean) *clayton*, by(month mode area state)
reshape long cod_corr_clayton had_corr_clayton cod_ind_clayton had_ind_clayton, i(month mode area state) j(decade) 
tempfile proj
save `proj', replace 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2021
rename st state
collapse (sum) dtrip, by(month mode area state)
merge 1:m month mode area state using `proj'
drop if _merge==1
drop _merge
collapse (mean) cod_corr cod_ind had_corr had_ind [aweight=dtrip], by(decade)
/*
decade		cod_corr_clayton	cod_ind_clayton		had_corr_clayton	had_ind_clayton
1				3.39595				3.40771				2.56903				2.90669
2				2.97307				2.988					2.72994				3.03005
3				2.62318				2.58512				2.80536				3.10712
4				2.62345				2.62731				2.80828				3.10511
5				2.01702				2.00065				3.03698				3.36075
6				1.74232				1.71358				4.19963				4.57071
7				1.78965				1.77689				4.2665					4.70256
8				1.75771				1.78232				4.18961				4.59097
*/


*from Lou's input files which are reformatted version of Jorge's files (2-14 data)
u "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_base_wide.dta", clear 
rename month1 month
rename mode1 mode
gen state1=25 if state=="MA"
replace state1=23 if state=="ME"
replace state1=33 if state=="NH"
drop state
rename state1 state
collapse (mean) had* cod*, by(month mode area state)
reshape long cod_corr_clayton had_corr_clayton cod_ind_clayton had_ind_clayton ///
					cod_corr_plackett had_corr_plackett cod_ind_plackett had_ind_plackett ///
					cod_corr_gaussian had_corr_gaussian cod_ind_gaussian had_ind_gaussian ///
					cod_corr_frank had_corr_frank cod_ind_frank had_ind_frank ///
					cod_corr_gumbel had_corr_gumbel cod_ind_gumbel had_ind_gumbel , i(month mode area state) j(decade) 
tempfile proj
save `proj', replace 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2021
rename st state
collapse (sum) dtrip, by(month mode area state)
merge 1:m month mode area state using `proj'
drop if _merge==1
drop _merge
collapse (mean) cod_corr* cod_ind* had_corr* had_ind* [aweight=dtrip], by(decade)
drop *ind*

*from Lou's input files which are reformatted version of Jorge's files (4-17 data)
u "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_base_wide_2_17.dta", clear 
rename month1 month
rename mode1 mode
renvarlab, lower
gen state1=25 if state=="MA"
replace state1=23 if state=="ME"
replace state1=33 if state=="NH"
drop state
rename state1 state
collapse (mean) had* cod*, by(month mode area state)
reshape long cod_corr_clayton had_corr_clayton cod_ind_clayton had_ind_clayton ///
					cod_corr_plackett had_corr_plackett cod_ind_plackett had_ind_plackett ///
					cod_corr_gaussian had_corr_gaussian cod_ind_gaussian had_ind_gaussian ///
					cod_corr_frank had_corr_frank cod_ind_frank had_ind_frank ///
					cod_corr_gumbel had_corr_gumbel cod_ind_gumbel had_ind_gumbel , i(month mode area state) j(decade) 
tempfile proj
save `proj', replace 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2021
rename st state
collapse (sum) dtrip, by(month mode area state)
merge 1:m month mode area state using `proj'
drop if _merge==1
drop _merge
collapse (mean) cod_corr* cod_ind* had_corr* had_ind* [aweight=dtrip], by(decade)




***From projection output
//mean(base_output_summarized$codcattrip) - 1.699446
//mean(base_output_summarized$haddcattrip) - 4.041861

***From calibration catch-per-trip 
import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\calib_catch_yr2021_draw50.csv", clear 
collapse (mean) tot_cat_cod tot_cat_hadd, by(month mode area state)
tempfile proj
save `proj', replace 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2021
rename st state
collapse (sum) dtrip, by(month mode area state)
merge 1:m month mode area state using `proj'
drop if _merge==1
drop _merge
collapse (mean) tot_cat_cod tot_cat_hadd [aweight=dtrip]

//tot_cat_cod		tot_cat_hadd
//1.6211			3.60545






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


