
**Create a dataset of catch draws in 2020 containing 10,000 catch draws per period
/*
use  "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data\cod_hadd_trips_adjusted_all_years.dta", clear  
keep if year==2020

destring month, gen(month1)

duplicates report

tempfile catch
save `catch', replace 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2020.csv", clear  

keep month period mode period2
tostring month, gen(month1)
tostring period, gen(period1)

gen domain=month1+"_"+period1+"_"+mode1
tempfile base 
save `base', replace 


levelsof domain, local(doms)

global domz
foreach d of local doms{
	
	u `base', clear 
	keep if domain=="`d'"

	su month
	local mon=`r(mean)'
	di `mon'
	
	levelsof mode1, local(md)
	levelsof period2, local(pdz)

	use  `catch', clear  

	keep if month1==`mon'
	
	count
	local n=`r(N)'
	
	if `n'>10000 {
	
	keep tot_cat_cod tot_cat_hadd month1 
	*sample 10000, count

	gen domain="`d'"
	gen mode1=`md'
	gen period2=`pdz'

	tempfile domz`d'
	save `domz`d'', replace
	global domz "$domz "`domz`d''" " 
	
	}
	
	else{
		
		local expand = ceil(10000/`n')+2
		expand `expand'
		sample 10000, count
		
		keep tot_cat_cod tot_cat_hadd month1
		gen domain="`d'"
		gen mode1=`md'
		gen period2=`pdz'

		tempfile domz`d'
		save `domz`d'', replace
		global domz "$domz "`domz`d''" " 
		
		
	}


	
}

dsconcat $domz
replace tot_cat_cod=round(tot_cat_cod) 
replace tot_cat_hadd=round(tot_cat_hadd) 

keep tot_cat_cod tot_cat_hadd period2
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\calibration catch per trip.csv", replace  


*/


**Create a dataset of catch draws containing 10000 catch draws per period for years 2010-2020
**This data set will be used to find p_star values 

local yrs 2010  2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
global yrz 
foreach y of local yrs{

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2010_2020_disaggregated.csv", clear  
keep if year==2020

tostring month, gen(month1)
tostring period, gen(period1)
tostring st, replace

gen domain=month1+"_"+period1+"_"+mode+"_"+area+"_"+st

keep if domain=="10_19_fh_inshore_25"

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44

keep month period mode period2 area st  domain state 
 
/*
levelsof domain, local(doms)

global domz
foreach d of local doms{
*/	
	su month
	local mon=`r(mean)'

	
	levelsof mode, local(md)
	levelsof period2, local(pdz)
	levelsof area, local(areaz)
	levelsof state, local(stz)

	tempfile base 
	save `base', replace 

	use  "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data\cod_hadd_trips_adjusted_all_years_mode_7_26.dta", clear  
	gen area = "inshore" if inlist(area_x, "1", "5")
	replace area="offshore" if inlist(area_x, "2")
	
	keep if year==2020

	destring month, gen(month1)

*duplicates report



	keep if month1==`mon'
	keep if mode1==`md'
	keep if area==`areaz'
	keep if st==
	count
	local n=`r(N)'
	
	if `n'>3000 {
	
	keep tot_cat_cod tot_cat_hadd month1 
	sample 3000, count

	gen domain="`d'"
	gen mode1=`md'
	gen period2=`pdz'

	tempfile domz`d'
	save `domz`d'', replace
	global domz "$domz "`domz`d''" " 
	
	}
	
	else{
		
		local expand = ceil(10000/`n')+2
		expand `expand'
		sample 3000, count
		
		keep tot_cat_cod tot_cat_hadd month1
		gen domain="`d'"
		gen mode1=`md'
		gen period2=`pdz'

		tempfile domz`d'
		save `domz`d'', replace
		global domz "$domz "`domz`d''" " 
		
		
	}


	
}

dsconcat $domz
replace tot_cat_cod=round(tot_cat_cod) 
replace tot_cat_hadd=round(tot_cat_hadd) 

gen year=`y'

tempfile yrz`y'
save `yrz`y'', replace
global yrz "$yrz "`yrz`y''" " 
		

}

dsconcat $yrz

keep tot_cat_cod tot_cat_hadd period2 year 
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\calibration catch per trip 2010_2020.csv", replace  





**Create a dataset of catch draws containing 10000 catch draws per period for years 2010-2020.
**This data set will be used for the calibrations. In each model iteration, we will draw 3000 of these 

local yrs 2010  2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
global yrz 
foreach y of local yrs{

use  "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data\cod_hadd_trips_adjusted_all_years.dta", clear  
keep if year==`y'

destring month, gen(month1)

duplicates report

tempfile catch
save `catch', replace 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2010_2020.csv", clear  
keep if year==`y'


keep month period mode period2
tostring month, gen(month1)
tostring period, gen(period1)

gen domain=month1+"_"+period1+"_"+mode1
tempfile base 
save `base', replace 


levelsof domain, local(doms)

global domz
foreach d of local doms{
	
	u `base', clear 
	keep if domain=="`d'"

	su month
	local mon=`r(mean)'
	di `mon'
	
	
	
	levelsof mode1, local(md)
	levelsof period2, local(pdz)

	use  `catch', clear  

	keep if month1==`mon'
	
	count
	local n=`r(N)'
	
	if `n'>10000 {
	
	keep tot_cat_cod tot_cat_hadd month1 
	sample 10000, count

	gen domain="`d'"
	gen mode1=`md'
	gen period2=`pdz'

	tempfile domz`d'
	save `domz`d'', replace
	global domz "$domz "`domz`d''" " 
	
	}
	
	else{
		
		local expand = ceil(10000/`n')+2
		expand `expand'
		sample 10000, count
		
		keep tot_cat_cod tot_cat_hadd month1
		gen domain="`d'"
		gen mode1=`md'
		gen period2=`pdz'

		tempfile domz`d'
		save `domz`d'', replace
		global domz "$domz "`domz`d''" " 
		
		
	}


	
}

dsconcat $domz
replace tot_cat_cod=round(tot_cat_cod) 
replace tot_cat_hadd=round(tot_cat_hadd) 

gen year=`y'

tempfile yrz`y'
save `yrz`y'', replace
global yrz "$yrz "`yrz`y''" " 
		

}

dsconcat $yrz

keep tot_cat_cod tot_cat_hadd period2 year 
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\calibration catch per trip 2010_2020_10k_draws.csv", replace  




