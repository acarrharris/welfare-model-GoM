
**Create a dataset of catch draws in 2030, 2040, ..., 2080  containing 10,000 catch draws per period

* First identify the range of observed catch-per-trip in order to truncate projected catcg-per-trip to this range
u "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\cod_hadd_catch_data_1_15.dta", clear 
su tot_cat_cod //max 156
su tot_cat_hadd //max 77

*The following code is for projected catch data broken out by area with the new mode classification (recived from J. Holzer on 4/17/2025).
cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\correlated_data\4-17-2025"

local list "correlated_sample_Clayton_Area1_newmode2 correlated_sample_Clayton_Area1_newmode3  correlated_sample_Clayton_Area2_newmode2 correlated_sample_Clayton_Area2_newmode3  correlated_sample_Frank_Area1_newmode2 correlated_sample_Frank_Area1_newmode3   correlated_sample_Frank_Area2_newmode2 correlated_sample_Frank_Area2_newmode3 correlated_sample_Gaussian_Area1_newmode2 correlated_sample_Gaussian_Area1_newmode3 	  correlated_sample_Gaussian_Area2_newmode2 correlated_sample_Gaussian_Area2_newmode3   correlated_sample_Gumbel_Area1_newmode2 correlated_sample_Gumbel_Area1_newmode3  correlated_sample_Gumbel_Area2_newmode2 correlated_sample_Gumbel_Area2_newmode3 		  correlated_sample_Plackett_Area1_newmode2 correlated_sample_Plackett_Area1_newmode3 correlated_sample_Plackett_Area2_newmode2 correlated_sample_Plackett_Area2_newmode3" 

global drawz
foreach s of local list{
*local s "correlated_sample_Gaussian_Area1_newmode3"
import delimited  "`s'.csv", clear
sort decade month 
gen state=25 if ma==1
replace state=23 if me==1
replace state=33 if nh==1
gen source="`s'"
split source, parse(_)

replace cod_corr=156 if cod_corr>156
replace cod_ind=156 if cod_ind>156
replace had_corr=77 if had_corr>77
replace had_ind=77 if had_ind>77

levelsof source4, clean

if "`r(levels)'"=="Area1"{
	gen area="inshore"
}

if "`r(levels)'"=="Area2"{
	gen area="offshore"
}

levelsof source5, clean

if "`r(levels)'"=="newmode2"{
	gen mode="fh"
}

if "`r(levels)'"=="newmode3"{
	gen mode="pr"
}

gen area_tab= substr(source4, strlen(source4), 1)
gen mode_tab= substr(source5, strlen(source5), 1)
gen domain=source3+"_"+area_tab+"_"+mode_tab
levelsof domain, clean
local file="`r(levels)'"
di "`file'"
keep  month decade cod_corr had_corr cod_ind had_ind mode area state source3
sort decade month mode state area
bysort decade month mode state area: gen n=_n

levelsof source3, clean 
local cop="`r(levels)'"

ds  month decade n state area mode source3, not 
renvarlab `r(varlist)', postfix("_`cop'")
drop source
renvarlab, lower
compress

tempfile `file'
save `file', replace

}

u Clayton_1_2.dta, clear 
append using Clayton_2_2.dta
append using Clayton_1_3.dta
append using Clayton_2_3.dta
tempfile clayton
save `clayton', replace 

u Frank_1_2.dta, clear 
append using Frank_2_2.dta
append using Frank_1_3.dta
append using Frank_2_3.dta
tempfile Frank
save `Frank', replace 

u Gaussian_1_2.dta, clear 
append using Gaussian_2_2.dta
append using Gaussian_1_3.dta
append using Gaussian_2_3.dta
tempfile Gaussian
save `Gaussian', replace 

u Plackett_1_2.dta, clear 
append using Plackett_2_2.dta
append using Plackett_1_3.dta
append using Plackett_2_3.dta
tempfile Plackett
save `Plackett', replace 

u Gumbel_1_2.dta, clear 
append using Gumbel_2_2.dta
append using Gumbel_1_3.dta
append using Gumbel_2_3.dta

merge 1:1 month decade n  mode state area using `Plackett', nogen 
merge 1:1 month decade n  mode state area using `clayton', nogen 
merge 1:1 month decade n  mode state area using `Gaussian', nogen 
merge 1:1 month decade n  mode state area using `Frank', nogen 
order decade month mode state area n

save "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_base.dta", replace

u "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_base.dta", clear

reshape wide cod* had*, i(month mode n state area) j(decade)
gen st="MA" if state==25
replace st="NH" if state==33 
replace st="ME" if state==23
drop state
rename st state 
rename mode mode1
rename month month1

save "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_base_wide.dta", replace
u "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_base_wide.dta", clear


global drawz
forv i=1/100{

set seed `i'


import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2021

tostring month, gen(month1)
tostring period, gen(period1)
tostring st, replace

gen domain=month1+"_"+period1+"_"+mode+"_"+area+"_"+st

*keep if domain=="4_8_pr_inshore_25"

gen state="MA" if st=="25"
replace state="NH" if st=="33" 
replace state="ME" if st=="23"

keep if dtrip>0

keep month period mode period2 area st  domain state  dtrip

levelsof domain, local(doms)

tempfile base 
save `base', replace 

global domz
foreach d of local doms{
	
	u `base', clear 
	*local d "7_14_pr_inshore_25"
	keep if domain=="`d'"

	*su dtrip
	*local trips=`r(sum)'
	*local sims=round(`trips'*3)
	*di `sims'
	
	su month
	local mon=`r(mean)'
	
	levelsof month, local(mon) clean
	levelsof mode, local(md) clean
	levelsof period2, local(pdz) clean
	levelsof area, local(areaz) clean
	levelsof state, local(stz) clean

	use  "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_base_wide.dta", clear  
	

	keep if month1==`mon'
	keep if mode1=="`md'"
	keep if area=="`areaz'"
	keep if state=="`stz'"
	
	count
	local n=`r(N)'
	
	if `n'>=3000 {
	
	keep had* cod* month1 
	sample 3000, count

	gen domain="`d'"
	gen mode1="`md'"
	gen period2="`pdz'"
	gen area= "`areaz'"
	gen state= "`stz'"
	gen tripid = ceil(_n / 30)
	bysort tripid: gen catch_draw = _n
	sort tripid catch_draw
	
	*tempfile domz`d'
	*save `domz`d'', replace
	*global domz "$domz "`domz`d''" " 
	
	}
	
	else{
		
		local expand = ceil(3000/`n')+2
		expand `expand'
		sample 3000, count
		
		keep had* cod* month1 
		gen domain="`d'"
		gen mode1="`md'"
		gen period2="`pdz'"
		gen area= "`areaz'"
		gen state= "`stz'"
		
		gen tripid = ceil(_n / 30)
		bysort tripid: gen catch_draw = _n
		sort tripid catch_draw
	}
	
		tempfile domz`d'
		save `domz`d'', replace
		global domz "$domz "`domz`d''" " 
		
		
}

dsconcat $domz
ds month1 domain mode1 period2 area state, not
local vars `r(varlist)'
foreach v of local vars{
	replace `v'=round(`v')
}

gen draw=`i'
split domain, parse("_")
drop mode1 period2 area state 

rename domain1 month
rename domain2 period
rename domain3 mode
rename domain4 area
rename domain5 state
destring month period state, replace
drop month1

order domain month period mode area state  
tostring period, gen(period1)
tostring state, gen(st2)
gen period2=mode+"_"+period1+"_"+area+"_"+st2
drop st2 period1
order domain month period mode area state tripid catch_draw draw period2
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_draw`i'.csv", replace 
}


* Code 8/27/25 to add projected catches for periods in 2019 and 2020 that had no catch/trips in 2021 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2021

tostring month, gen(month1)
tostring period, gen(period1)
tostring st, replace

gen domain=month1+"_"+period1+"_"+mode+"_"+area+"_"+st
gen state="MA" if st=="25"
replace state="NH" if st=="33" 
replace state="ME" if st=="23"
keep if dtrip>0

keep domain 
tempfile y2021
save `y2021', replace 


import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2019

tostring month, gen(month1)
tostring period, gen(period1)
tostring st, replace

gen domain=month1+"_"+period1+"_"+mode+"_"+area+"_"+st
gen state="MA" if st=="25"
replace state="NH" if st=="33" 
replace state="ME" if st=="23"
keep if dtrip>0

keep month period mode period2 area st  domain state  dtrip 
merge 1:1 domain using `y2021'
keep if _merge==1
drop _merge

tempfile add_2019
save `add_2019', replace 

import delimited using "E:\Lou's projects\welfare-model-GoM\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2020

tostring month, gen(month1)
tostring period, gen(period1)
tostring st, replace

gen domain=month1+"_"+period1+"_"+mode+"_"+area+"_"+st
gen state="MA" if st=="25"
replace state="NH" if st=="33" 
replace state="ME" if st=="23"
keep if dtrip>0

keep month period mode period2 area st  domain state  dtrip 
merge 1:1 domain using `y2021'
keep if _merge==1
drop _merge
append using `add_2019'
duplicates drop  

levelsof domain, local(doms)

tempfile base 
save `base', replace 

global drawz
forv i=1/100{

set seed `i'

global domz
foreach d of local doms{
	
	u `base', clear 
	*local d "7_14_pr_inshore_25"
	keep if domain=="`d'"

	*su dtrip
	*local trips=`r(sum)'
	*local sims=round(`trips'*3)
	*di `sims'
	
	su month
	local mon=`r(mean)'
	
	levelsof month, local(mon) clean
	levelsof mode, local(md) clean
	levelsof period2, local(pdz) clean
	levelsof area, local(areaz) clean
	levelsof state, local(stz) clean

	use  "E:\Lou's projects\welfare-model-GoM\input_data\projection_catch_base_wide.dta", clear  
	*tabstat cod_corr_gumbel1, stat(mean) by(month)

	keep if month1==`mon'
	keep if mode1=="`md'"
	keep if area=="`areaz'"
	keep if state=="`stz'"
	
	count
	local n=`r(N)'
	
	if `n'>=3000 {
	
	keep had* cod* month1 
	sample 3000, count

	gen domain="`d'"
	gen mode1="`md'"
	gen period2="`pdz'"
	gen area= "`areaz'"
	gen state= "`stz'"
	gen tripid = ceil(_n / 30)
	bysort tripid: gen catch_draw = _n
	sort tripid catch_draw
	
	*tempfile domz`d'
	*save `domz`d'', replace
	*global domz "$domz "`domz`d''" " 
	
	}
	
	else{
		
		local expand = ceil(3000/`n')+2
		expand `expand'
		sample 3000, count
		
		keep had* cod* month1 
		gen domain="`d'"
		gen mode1="`md'"
		gen period2="`pdz'"
		gen area= "`areaz'"
		gen state= "`stz'"
		
		gen tripid = ceil(_n / 30)
		bysort tripid: gen catch_draw = _n
		sort tripid catch_draw
	}
	
		tempfile domz`d'
		save `domz`d'', replace
		global domz "$domz "`domz`d''" " 
		
		
}

dsconcat $domz
ds month1 domain mode1 period2 area state, not
local vars `r(varlist)'
foreach v of local vars{
	replace `v'=round(`v')
}

gen draw=`i'
split domain, parse("_")
drop mode1 period2 area state 

rename domain1 month
rename domain2 period
rename domain3 mode
rename domain4 area
rename domain5 state
destring month period state, replace
drop month1

order domain month period mode area state  
tostring period, gen(period1)
tostring state, gen(st2)
gen period2=mode+"_"+period1+"_"+area+"_"+st2
drop st2 period1
order domain month period mode area state tripid catch_draw draw period2

preserve
import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_draw`i'.csv", clear
tempfile base`i'
save `base`i'', replace
restore 

append using `base`i'' 

compress

export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_draw`i'.csv", replace 

}



import delimited using "E:\Lou's projects\welfare-model-GoM\input_data\projection_catch_draw1.csv", clear 
collapse (mean) cod_corr_gumbel1 had_corr_gumbel1 cod_ind_gumbel1 had_ind_gumbel1 cod_corr_gumbel2 had_corr_gumbel2 cod_ind_gumbel2 had_ind_gumbel2 cod_corr_gumbel3 had_corr_gumbel3 cod_ind_gumbel3 had_ind_gumbel3 cod_corr_gumbel4 had_corr_gumbel4 cod_ind_gumbel4 had_ind_gumbel4 cod_corr_gumbel5 had_corr_gumbel5 cod_ind_gumbel5 had_ind_gumbel5 cod_corr_gumbel6 had_corr_gumbel6 cod_ind_gumbel6 had_ind_gumbel6 cod_corr_gumbel7 had_corr_gumbel7 cod_ind_gumbel7 had_ind_gumbel7 cod_corr_gumbel8 had_corr_gumbel8 cod_ind_gumbel8 had_ind_gumbel8, by(month)

reshape long cod_corr_gumbel had_corr_gumbel cod_ind_gumbel had_ind_gumbel, i(month) j(decade) string
destring decade, replace
xtset decade month

tsline had_corr_gumbel, by(decade) xlab(#12)
tsline had_ind_gumbel, by(decade) xlab(#12)
tsline cod_corr_gumbel, by(decade)
tsline cod_ind_gumbel, by(decade)

*The following code is for projected catch data broken out by state and area.
cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\correlated_data\2-14-2024"

import delimited  "correlated_sample_Clayton_MA_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=25
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_clayton)
tempfile claytonMA_1
save `claytonMA_1', replace 

import delimited  "correlated_sample_Clayton_MA_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=25
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_clayton)
tempfile claytonMA_2
save `claytonMA_2', replace 

import delimited  "correlated_sample_Clayton_NH_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=33
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_clayton)
tempfile claytonNH_1
save `claytonNH_1', replace 

import delimited  "correlated_sample_Clayton_NH_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=33
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_clayton)
tempfile claytonNH_2
save `claytonNH_2', replace 

import delimited  "correlated_sample_Clayton_ME_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=23
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_clayton)
tempfile claytonME_1
save `claytonME_1', replace 

import delimited  "correlated_sample_Clayton_ME_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=23
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_clayton)
tempfile claytonME_2
save `claytonME_2', replace 

append using `claytonME_1'
append using `claytonNH_2'
append using  `claytonNH_1'
append using  `claytonMA_2'
append using  `claytonMA_1'

preserve
keep cod_corr_clayton had_corr_clayton state area mode month decade
keep if decade==1
collapse (mean) cod_corr_clayton had_corr_clayton, by(state area mode month)
export delimited using "raw_data_check.csv", replace
restore

tempfile clayton
save `clayton', replace


import delimited  "correlated_sample_Frank_MA_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=25
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_frank)
tempfile FrankMA_1
save `FrankMA_1', replace 

import delimited  "correlated_sample_Frank_MA_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=25
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_frank)
tempfile FrankMA_2
save `FrankMA_2', replace 

import delimited  "correlated_sample_Frank_NH_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=33
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_frank)
tempfile FrankNH_1
save `FrankNH_1', replace 

import delimited  "correlated_sample_Frank_NH_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=33
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_frank)
tempfile FrankNH_2
save `FrankNH_2', replace 

import delimited  "correlated_sample_Frank_ME_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=23
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_frank)
tempfile FrankME_1
save `FrankME_1', replace 

import delimited  "correlated_sample_Frank_ME_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=23
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_frank)
tempfile FrankME_2
save `FrankME_2', replace 

append using `FrankME_1'
append using `FrankNH_2'
append using  `FrankNH_1'
append using  `FrankMA_2'
append using  `FrankMA_1'

tempfile Frank
save `Frank', replace






import delimited  "correlated_sample_Gaussian_MA_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=25
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_Gaussian)
tempfile GaussianMA_1
save `GaussianMA_1', replace 

import delimited  "correlated_sample_Gaussian_MA_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=25
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_Gaussian)
tempfile GaussianMA_2
save `GaussianMA_2', replace 

import delimited  "correlated_sample_Gaussian_NH_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=33
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_Gaussian)
tempfile GaussianNH_1
save `GaussianNH_1', replace 

import delimited  "correlated_sample_Gaussian_NH_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=33
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_Gaussian)
tempfile GaussianNH_2
save `GaussianNH_2', replace 

import delimited  "correlated_sample_Gaussian_ME_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=23
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_Gaussian)
tempfile GaussianME_1
save `GaussianME_1', replace 

import delimited  "correlated_sample_Gaussian_ME_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=23
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_Gaussian)
tempfile GaussianME_2
save `GaussianME_2', replace 

append using `GaussianME_1'
append using `GaussianNH_2'
append using  `GaussianNH_1'
append using  `GaussianMA_2'
append using  `GaussianMA_1'

tempfile Gaussian
save `Gaussian', replace






import delimited  "correlated_sample_Gumbel_MA_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=25
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_gumbel)
tempfile GumbelMA_1
save `GumbelMA_1', replace 

import delimited  "correlated_sample_Gumbel_MA_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=25
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_gumbel)
tempfile GumbelMA_2
save `GumbelMA_2', replace 

import delimited  "correlated_sample_Gumbel_NH_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=33
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_gumbel)
tempfile GumbelNH_1
save `GumbelNH_1', replace 

import delimited  "correlated_sample_Gumbel_NH_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=33
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_gumbel)
tempfile GumbelNH_2
save `GumbelNH_2', replace 

import delimited  "correlated_sample_Gumbel_ME_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=23
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_gumbel)
tempfile GumbelME_1
save `GumbelME_1', replace 

import delimited  "correlated_sample_Gumbel_ME_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=23
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_gumbel)
tempfile GumbelME_2
save `GumbelME_2', replace 

append using `GumbelME_1'
append using `GumbelNH_2'
append using  `GumbelNH_1'
append using  `GumbelMA_2'
append using  `GumbelMA_1'

tempfile Gumbel
save `Gumbel', replace





import delimited  "correlated_sample_Plackett_MA_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=25
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_plackett)
tempfile PlackettMA_1
save `PlackettMA_1', replace 

import delimited  "correlated_sample_Plackett_MA_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=25
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_plackett)
tempfile PlackettMA_2
save `PlackettMA_2', replace 

import delimited  "correlated_sample_Plackett_NH_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=33
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_plackett)
tempfile PlackettNH_1
save `PlackettNH_1', replace 

import delimited  "correlated_sample_Plackett_NH_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=33
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_plackett)
tempfile PlackettNH_2
save `PlackettNH_2', replace 

import delimited  "correlated_sample_Plackett_ME_Area1.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=23
gen area = "inshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_plackett)
tempfile PlackettME_1
save `PlackettME_1', replace 

import delimited  "correlated_sample_Plackett_ME_Area2.csv", clear
sort decade month 
keep  month decade cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort decade month mode: gen n=_n
gen state=23
gen area = "offshore"
ds  month decade n state area mode, not 
renvarlab `r(varlist)', postfix(_plackett)
tempfile PlackettME_2
save `PlackettME_2', replace 

append using `PlackettME_1'
append using `PlackettNH_2'
append using  `PlackettNH_1'
append using  `PlackettMA_2'
append using  `PlackettMA_1'

merge 1:1 month decade n  mode state area using `clayton', nogen 
merge 1:1 month decade n  mode state area using `Gumbel', nogen 
merge 1:1 month decade n  mode state area using `Gaussian', nogen 
merge 1:1 month decade n  mode state area using `Frank', nogen 

save "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_base_2_17.dta", replace

u "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_base_2_17.dta", clear

ds month decade mode n state area cod*, not
foreach v in `r(varlist)'{
replace `v'=77 if `v'>77
} 

ds month decade mode n state area had*, not
foreach v in `r(varlist)'{
replace `v'=156 if `v'>156
} 

reshape wide cod* had*, i(month mode n state area) j(decade)
gen st="MA" if state==25
replace st="NH" if state==33 
replace st="ME" if state==23
drop state
rename st state 
rename mode mode1
rename month month1

save "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_base_wide_2_17.dta", replace




global drawz
forv i=1/100{

set seed `i'


import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2021

tostring month, gen(month1)
tostring period, gen(period1)
tostring st, replace

gen domain=month1+"_"+period1+"_"+mode+"_"+area+"_"+st

*keep if domain=="4_8_pr_inshore_25"

gen state="MA" if st=="25"
replace state="NH" if st=="33" 
replace state="ME" if st=="23"

keep if dtrip>0

keep month period mode period2 area st  domain state  dtrip

levelsof domain, local(doms)

tempfile base 
save `base', replace 

global domz
foreach d of local doms{
	
	u `base', clear 
	*local d "7_14_pr_inshore_25"
	keep if domain=="`d'"

	*su dtrip
	*local trips=`r(sum)'
	*local sims=round(`trips'*3)
	*di `sims'
	
	su month
	local mon=`r(mean)'
	
	levelsof month, local(mon) clean
	levelsof mode, local(md) clean
	levelsof period2, local(pdz) clean
	levelsof area, local(areaz) clean
	levelsof state, local(stz) clean

	use  "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_base_wide_2_17.dta", clear  
	

	keep if month1==`mon'
	keep if mode1=="`md'"
	keep if area=="`areaz'"
	keep if state=="`stz'"
	
	count
	local n=`r(N)'
	
	if `n'>=3000 {
	
	keep had* cod* month1 
	sample 3000, count

	gen domain="`d'"
	gen mode1="`md'"
	gen period2="`pdz'"
	gen area= "`areaz'"
	gen state= "`stz'"
	gen tripid = ceil(_n / 30)
	bysort tripid: gen catch_draw = _n
	sort tripid catch_draw
	
	*tempfile domz`d'
	*save `domz`d'', replace
	*global domz "$domz "`domz`d''" " 
	
	}
	
	else{
		
		local expand = ceil(3000/`n')+2
		expand `expand'
		sample 3000, count
		
		keep had* cod* month1 
		gen domain="`d'"
		gen mode1="`md'"
		gen period2="`pdz'"
		gen area= "`areaz'"
		gen state= "`stz'"
		
		gen tripid = ceil(_n / 30)
		bysort tripid: gen catch_draw = _n
		sort tripid catch_draw
	}
	
		tempfile domz`d'
		save `domz`d'', replace
		global domz "$domz "`domz`d''" " 
		
		
}

dsconcat $domz
ds month1 domain mode1 period2 area state, not
local vars `r(varlist)'
foreach v of local vars{
	replace `v'=round(`v')
}

gen draw=`i'
split domain, parse("_")
drop mode1 period2 area state 

rename domain1 month
rename domain2 period
rename domain3 mode
rename domain4 area
rename domain5 state
destring month period state, replace
drop month1

order domain month period mode area state  
tostring period, gen(period1)
tostring state, gen(st2)
gen period2=mode+"_"+period1+"_"+area+"_"+st2
drop st2 period1
order domain month period mode area state tripid catch_draw draw period2
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\projection_catch_draw_2_17_`i'.csv", replace 
}











**Historical data 

**Create a dataset of catch draws in 2030, 2040, ..., 2080  containing 10,000 catch draws per period
cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data"


import delimited  "historical_correlated_sample_MA_Area1_Clayton.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=25
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_clayton)
tempfile claytonMA_1
save `claytonMA_1', replace 

import delimited  "historical_correlated_sample_MA_Area2_Clayton.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=25
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_clayton)
tempfile claytonMA_2
save `claytonMA_2', replace 

import delimited  "historical_correlated_sample_NH_Area1_Clayton.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=33
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_clayton)
tempfile claytonNH_1
save `claytonNH_1', replace 

import delimited  "historical_correlated_sample_NH_Area2_Clayton.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=33
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_clayton)
tempfile claytonNH_2
save `claytonNH_2', replace 

import delimited  "historical_correlated_sample_ME_Area1_Clayton.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=23
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_clayton)
tempfile claytonME_1
save `claytonME_1', replace 

import delimited  "historical_correlated_sample_ME_Area2_Clayton.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=23
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_clayton)
tempfile claytonME_2
save `claytonME_2', replace 

append using `claytonME_1'
append using `claytonNH_2'
append using  `claytonNH_1'
append using  `claytonMA_2'
append using  `claytonMA_1'

tempfile clayton
save `clayton', replace






import delimited  "historical_correlated_sample_MA_Area1_Frank.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=25
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_frank)
tempfile FrankMA_1
save `FrankMA_1', replace 

import delimited  "historical_correlated_sample_MA_Area2_Frank.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=25
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_frank)
tempfile FrankMA_2
save `FrankMA_2', replace 

import delimited  "historical_correlated_sample_NH_Area1_Frank.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=33
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_frank)
tempfile FrankNH_1
save `FrankNH_1', replace 

import delimited  "historical_correlated_sample_NH_Area2_Frank.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=33
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_frank)
tempfile FrankNH_2
save `FrankNH_2', replace 

import delimited  "historical_correlated_sample_ME_Area1_Frank.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=23
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_frank)
tempfile FrankME_1
save `FrankME_1', replace 

import delimited  "historical_correlated_sample_ME_Area2_Frank.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=23
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_frank)
tempfile FrankME_2
save `FrankME_2', replace 

append using `FrankME_1'
append using `FrankNH_2'
append using  `FrankNH_1'
append using  `FrankMA_2'
append using  `FrankMA_1'

tempfile Frank
save `Frank', replace

merge 1:1 year month mode n state area using `clayton'

*keep if year==2021




import delimited  "historical_correlated_sample_MA_Area1_Gaussian.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=25
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_Gaussian)
tempfile GaussianMA_1
save `GaussianMA_1', replace 

import delimited  "historical_correlated_sample_MA_Area2_Gaussian.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=25
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_Gaussian)
tempfile GaussianMA_2
save `GaussianMA_2', replace 

import delimited  "historical_correlated_sample_NH_Area1_Gaussian.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=33
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_Gaussian)
tempfile GaussianNH_1
save `GaussianNH_1', replace 

import delimited  "historical_correlated_sample_NH_Area2_Gaussian.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=33
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_Gaussian)
tempfile GaussianNH_2
save `GaussianNH_2', replace 

import delimited  "historical_correlated_sample_ME_Area1_Gaussian.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=23
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_Gaussian)
tempfile GaussianME_1
save `GaussianME_1', replace 

import delimited  "historical_correlated_sample_ME_Area2_Gaussian.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=23
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_Gaussian)
tempfile GaussianME_2
save `GaussianME_2', replace 

append using `GaussianME_1'
append using `GaussianNH_2'
append using  `GaussianNH_1'
append using  `GaussianMA_2'
append using  `GaussianMA_1'

tempfile Gaussian
save `Gaussian', replace






import delimited  "historical_correlated_sample_MA_Area1_Gumbel.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=25
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_gumbel)
tempfile GumbelMA_1
save `GumbelMA_1', replace 

import delimited  "historical_correlated_sample_MA_Area2_Gumbel.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=25
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_gumbel)
tempfile GumbelMA_2
save `GumbelMA_2', replace 

import delimited  "historical_correlated_sample_NH_Area1_Gumbel.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=33
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_gumbel)
tempfile GumbelNH_1
save `GumbelNH_1', replace 

import delimited  "historical_correlated_sample_NH_Area2_Gumbel.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=33
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_gumbel)
tempfile GumbelNH_2
save `GumbelNH_2', replace 

import delimited  "historical_correlated_sample_ME_Area1_Gumbel.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=23
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_gumbel)
tempfile GumbelME_1
save `GumbelME_1', replace 

import delimited  "historical_correlated_sample_ME_Area2_Gumbel.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=23
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_gumbel)
tempfile GumbelME_2
save `GumbelME_2', replace 

append using `GumbelME_1'
append using `GumbelNH_2'
append using  `GumbelNH_1'
append using  `GumbelMA_2'
append using  `GumbelMA_1'

tempfile Gumbel
save `Gumbel', replace





import delimited  "historical_correlated_sample_MA_Area1_Plackett.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=25
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_plackett)
tempfile PlackettMA_1
save `PlackettMA_1', replace 

import delimited  "historical_correlated_sample_MA_Area2_Plackett.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=25
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_plackett)
tempfile PlackettMA_2
save `PlackettMA_2', replace 

import delimited  "historical_correlated_sample_NH_Area1_Plackett.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=33
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_plackett)
tempfile PlackettNH_1
save `PlackettNH_1', replace 

import delimited  "historical_correlated_sample_NH_Area2_Plackett.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=33
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_plackett)
tempfile PlackettNH_2
save `PlackettNH_2', replace 

import delimited  "historical_correlated_sample_ME_Area1_Plackett.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=23
gen area = "inshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_plackett)
tempfile PlackettME_1
save `PlackettME_1', replace 

import delimited  "historical_correlated_sample_ME_Area2_Plackett.csv", clear
sort year month 
keep  month year cod_corr had_corr cod_ind had_ind mode1 mode2
gen mode3="pr" if mode2==1
replace mode3="fh" if mode1==1
drop mode1 mode2
rename mode3 mode
bysort year month mode: gen n=_n
gen state=23
gen area = "offshore"
ds  month year n state area mode, not 
renvarlab `r(varlist)', postfix(_plackett)
tempfile PlackettME_2
save `PlackettME_2', replace 

append using `PlackettME_1'
append using `PlackettNH_2'
append using  `PlackettNH_1'
append using  `PlackettMA_2'
append using  `PlackettMA_1'

merge 1:1 month year n  mode state area using `clayton', keep(3) nogen 
merge 1:1 month year n  mode state area using `Gumbel', keep(3)  nogen 
merge 1:1 month year n  mode state area using `Gaussian', keep(3)  nogen 
merge 1:1 month year n  mode state area using `Frank', keep(3)  nogen 



save "historical_catch_base.dta", replace

u "historical_catch_base.dta", clear
order year month mode state area n cod* had*


***Check historical 2021 data with calibration data 
*From Jorge 12/6: As discussed on the phone today, it may be a good idea to start with a comparison between the model calibrated year 2021 and the predicted year 2021 that uses the data uploaded here for that year. Thank you. 

*keep if year==2021



tempfile catch
save `catch', replace 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
*keep if year==2021
drop if dtrip==0


keep month period mode period2 area st
tostring month, gen(month1)
tostring period, gen(period1)
tostring st, gen(st1)

*gen domain=month1+"_"+period1+"_"+mode1
tempfile base 
save `base', replace 

levelsof period2, local(doms)

global domz
foreach d of local doms{
	
	u `base', clear 
	keep if period2=="`d'"

	su month
	local mon=`r(mean)'
	di `mon'
	
	levelsof mode, local(md)
	levelsof period2, local(pdz)
	levelsof st, local(stz)
	levelsof area, local(areaz)

	use  "historical_catch_base.dta", clear  
	
	levelsof year, local(yrz)
	foreach y of local yrz{
		use  "historical_catch_base.dta", clear  
		keep if year==`y'
		keep if month==`mon'
		keep if st==`stz'
		keep if area==`areaz'
		keep if mode==`md'
		
		gen period2=`pdz'

		tempfile domz`d'`y'
		save `domz`d'`y'', replace
		global domz "$domz "`domz`d'`y''" " 
	
	}
	

	
}

dsconcat $domz
order period2 year month mode area state n


ds period2 year month mode area state n, not
local vars  `r(varlist)'
foreach v of local vars{
replace `v'=round(`v') 
 }
 
export delimited using "projected historical catch per trip.csv", replace  



import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
*keep if year==2021
keep year month period2 dtrip
drop if dtrip==0
tempfile regs
save `regs', replace


import delimited using "projected historical catch per trip.csv", clear  
*keep if year==2021
collapse (mean) cod_corr_clayton had_corr_clayton cod_ind_clayton had_ind_clayton, by(year month period2)
merge 1:1 year month period2 using `regs', keep(3) nogen

tempfile project 
save `project', replace 

import delimited using "calibration catch per trip 2010_2020.csv" , clear 
*keep if year==2021
collapse (mean) tot_cat*, by(year month period2)
merge 1:1 year month period2 using `regs', keep(3) nogen


merge 1:1 year month period2 using `project'
replace dtrip=round(dtrip)
expand dtrip
collapse (mean) tot_cat_cod cod_corr_clayton cod_ind_clayton tot_cat_hadd had_corr_clayton had_ind_clayton, by(year)

gen ym_date = ym(year, month)
format ym_date %tm
tsset ym_date

tsline tot_cat_hadd had_corr_clayton had_ind_clayton if year==2021


keep if _merge==3 
drop _merge
order period2 tot_cat_cod cod* tot_cat_had had*
tempfile project 
save `project', replace 



merge 1:1 period2 using `project'
replace dtrip=round(dtrip)
expand dtrip

collapse (mean) tot_cat_cod cod_corr_clayton cod_ind_clayton tot_cat_hadd had_corr_clayton had_ind_clayton




