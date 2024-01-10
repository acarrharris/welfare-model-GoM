
**Create a dataset of catch draws in 2020 containing 10,000 catch draws per period
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





