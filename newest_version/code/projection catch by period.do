
**Create a dataset of catch draws in 2030, 2040, ..., 2080  containing 10,000 catch draws per period
cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data"


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






import delimited  "correlated_sample_Guassian_MA_Area1.csv", clear
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
renvarlab `r(varlist)', postfix(_guassian)
tempfile GuassianMA_1
save `GuassianMA_1', replace 

import delimited  "correlated_sample_Guassian_MA_Area2.csv", clear
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
renvarlab `r(varlist)', postfix(_guassian)
tempfile GuassianMA_2
save `GuassianMA_2', replace 

import delimited  "correlated_sample_Guassian_NH_Area1.csv", clear
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
renvarlab `r(varlist)', postfix(_guassian)
tempfile GuassianNH_1
save `GuassianNH_1', replace 

import delimited  "correlated_sample_Guassian_NH_Area2.csv", clear
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
renvarlab `r(varlist)', postfix(_guassian)
tempfile GuassianNH_2
save `GuassianNH_2', replace 

import delimited  "correlated_sample_Guassian_ME_Area1.csv", clear
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
renvarlab `r(varlist)', postfix(_guassian)
tempfile GuassianME_1
save `GuassianME_1', replace 

import delimited  "correlated_sample_Guassian_ME_Area2.csv", clear
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
renvarlab `r(varlist)', postfix(_guassian)
tempfile GuassianME_2
save `GuassianME_2', replace 

append using `GuassianME_1'
append using `GuassianNH_2'
append using  `GuassianNH_1'
append using  `GuassianMA_2'
append using  `GuassianMA_1'

tempfile Guassian
save `Guassian', replace






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
merge 1:1 month decade n  mode state area using `Guassian', nogen 
merge 1:1 month decade n  mode state area using `Frank', nogen 

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




import delimited  "historical_correlated_sample_MA_Area1_Guassian.csv", clear
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
renvarlab `r(varlist)', postfix(_guassian)
tempfile GuassianMA_1
save `GuassianMA_1', replace 

import delimited  "historical_correlated_sample_MA_Area2_Guassian.csv", clear
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
renvarlab `r(varlist)', postfix(_guassian)
tempfile GuassianMA_2
save `GuassianMA_2', replace 

import delimited  "historical_correlated_sample_NH_Area1_Guassian.csv", clear
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
renvarlab `r(varlist)', postfix(_guassian)
tempfile GuassianNH_1
save `GuassianNH_1', replace 

import delimited  "historical_correlated_sample_NH_Area2_Guassian.csv", clear
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
renvarlab `r(varlist)', postfix(_guassian)
tempfile GuassianNH_2
save `GuassianNH_2', replace 

import delimited  "historical_correlated_sample_ME_Area1_Guassian.csv", clear
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
renvarlab `r(varlist)', postfix(_guassian)
tempfile GuassianME_1
save `GuassianME_1', replace 

import delimited  "historical_correlated_sample_ME_Area2_Guassian.csv", clear
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
renvarlab `r(varlist)', postfix(_guassian)
tempfile GuassianME_2
save `GuassianME_2', replace 

append using `GuassianME_1'
append using `GuassianNH_2'
append using  `GuassianNH_1'
append using  `GuassianMA_2'
append using  `GuassianMA_1'

tempfile Guassian
save `Guassian', replace






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
merge 1:1 month year n  mode state area using `Guassian', keep(3)  nogen 
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




