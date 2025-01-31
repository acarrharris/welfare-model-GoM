
**Create a dataset of catch draws containing 10000 catch draws per period for years 2010-2020
**This data set will be used to find p_star values 

local yrs 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
foreach y of local yrs{

	use  "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\cod_hadd_catch_data_1_15.dta", clear  
	keep if year==`y'
	drop if tot_cat_cod==. | tot_cat_hadd==.
	save "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\cod_hadd_catch_data_1_15_yr`y'.dta", replace

}


global drawz
forv i=1/100{

set seed `i'

local yrs 2021
global yrz 
foreach y of local yrs{

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\directed trips and regulations 2010_2020_disaggregated.csv", clear  
*local y=2010
keep if year==`y'

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

	use  "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\cod_hadd_catch_data_1_15_yr`y'.dta", clear  
	
	gen area = "inshore" if inlist(area_x, "1", "5")
	replace area="offshore" if inlist(area_x, "2")
	
	destring month, gen(month1)
	replace mode1="fh" if inlist(mode1, "hd", "ch")

	keep if month1==`mon'
	keep if mode1=="`md'"
	keep if area=="`areaz'"
	keep if state=="`stz'"
	
	count
	local n=`r(N)'
	
	if `n'>=3000 {
	
	keep tot_cat_cod tot_cat_hadd month1 
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
		
		keep tot_cat_cod tot_cat_hadd month1
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
gen year=`y'
replace tot_cat_cod=round(tot_cat_cod) 
replace tot_cat_hadd=round(tot_cat_hadd) 

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

order domain year month period mode area state 
tostring period, gen(period1)
tostring state, gen(st2)
gen period2=mode+"_"+period1+"_"+area+"_"+st2
drop st2 period1
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\calib_catch_yr`y'_draw`i'.csv", replace 
}
}



