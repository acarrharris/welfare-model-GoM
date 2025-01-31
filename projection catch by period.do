
**Create a dataset of catch draws in 2030, 2040, ..., 2080  containing 10,000 catch draws per period
cd "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data"


//import delimited  "correlated_sample_Clayton.csv", clear
import delimited  "correlated_sample_Clayton_v2.csv", clear
sort decade month obs
bysort decade month obs: gen n=_n
keep obs month decade cod_corr had_corr cod_ind had_ind n
ds obs month decade n, not 
renvarlab `r(varlist)', postfix(_clayton)

tempfile clayton
save `clayton', replace 



//import delimited  "correlated_sample_Frank.csv", clear
import delimited  "correlated_sample_Frank_v2.csv", clear
sort decade month obs
bysort decade month obs: gen n=_n
keep obs month decade cod_corr had_corr cod_ind had_ind n
ds obs month decade n, not 
renvarlab `r(varlist)', postfix(_frank)

tempfile frank
save `frank', replace 


//import delimited  "correlated_sample_Guassian.csv", clear
import delimited  "correlated_sample_Guassian_v2.csv", clear
sort decade month obs
bysort decade month obs: gen n=_n
keep obs month decade cod_corr had_corr cod_ind had_ind n
ds obs month decade n, not 
renvarlab `r(varlist)', postfix(_gaussian)

tempfile gaussian
save `gaussian', replace 


//import delimited  "correlated_sample_Plackett.csv", clear
import delimited  "correlated_sample_Plackett_v2.csv", clear
sort decade month obs
bysort decade month obs: gen n=_n
keep obs month decade cod_corr had_corr cod_ind had_ind n
ds obs month decade n, not 
renvarlab `r(varlist)', postfix(_plackett)

tempfile plackett
save `plackett', replace 


//import delimited  "correlated_sample_Gumbel.csv", clear
import delimited  "correlated_sample_Gumbel_v2.csv", clear
sort decade month obs
bysort decade month obs: gen n=_n
keep obs month decade cod_corr had_corr cod_ind had_ind n
ds obs month decade n, not 
renvarlab `r(varlist)', postfix(_gumbel)

merge 1:1 obs month decade n using `frank', nogen
merge 1:1 obs month decade n using `clayton', nogen
merge 1:1 obs month decade n using `plackett', nogen
merge 1:1 obs month decade n using `gaussian', nogen

drop n


tempfile catch
save `catch', replace 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2010_2020.csv", clear  
keep if year==2020
drop if dtrip==0


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
	
	levelsof decade, local(decs)
	foreach t of local decs{
		u `catch', clear 
		keep if decade==`t'
		keep if month==`mon'
	
	
		gen domain="`d'"
		gen mode1=`md'
		gen period2=`pdz'

		tempfile domz`d'`t'
		save `domz`d'`t'', replace
		global domz "$domz "`domz`d'`t''" " 
	
	}
	

	
}

dsconcat $domz
order domain decade month period2 mode1 obs
drop domain obs

ds decade month period2 mode1, not
local vars  `r(varlist)'
foreach v of local vars{
replace `v'=round(`v') 
 }
 
//export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\projection catch per trip 5_6.csv", replace  
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\projection catch per trip 5_28.csv", replace  


/*
import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\projection catch per trip 4_30.csv", clear  
ds decade month period2 mode1, not
collapse (mean) `r(varlist)', by(decade month )

tostring decade, gen(decade2)
tostring month, gen(month2) format("%02.0f")


gen decade_month=decade2+"_"+month2
encode decade_month, gen(decade_month2)
tsset decade_month2

global graphoptions graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)

tsline cod_corr_clayton  cod_ind_clayton, $graphoptions title("Mean catch-per-trip cod") xla(, valuelabel) name(gr1, replace) 
gr play adjust1
tsline had_corr_clayton  had_ind_clayton, $graphoptions title("Mean catch-per-trip haddock") xla(, valuelabel) name(gr2, replace)
gr play adjust1

grc1leg gr1 gr2, col(2) xcommon ycommon
*/




***NOW do the same for the historical data
**Create a dataset of catch draws in 2010-2019, containing 10,000 catch draws per period
**For each year t, we need to have catch draws for year t+1 and regulations for year t+1 for each of the periods in year t. 
cd "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data"


//import delimited  "correlated_sample_Clayton.csv", clear
import delimited  "historical_correlated_sample_Clayton.csv", clear
sort year month 
bysort year month: gen n=_n
keep year month cod_corr had_corr cod_ind had_ind n
ds year month n, not 
renvarlab `r(varlist)', postfix(_clayton)

tempfile clayton
save `clayton', replace 



//import delimited  "correlated_sample_Frank.csv", clear
import delimited  "historical_correlated_sample_Frank.csv", clear
sort year month 
bysort year month: gen n=_n
keep year month cod_corr had_corr cod_ind had_ind n
ds year month n, not 
renvarlab `r(varlist)', postfix(_frank)

tempfile frank
save `frank', replace 


//import delimited  "correlated_sample_Guassian.csv", clear
import delimited  "historical_correlated_sample_Guassian.csv", clear
sort year month 
bysort year month: gen n=_n
keep year month cod_corr had_corr cod_ind had_ind n
ds year month n, not 
renvarlab `r(varlist)', postfix(_gaussian)

tempfile gaussian
save `gaussian', replace 


//import delimited  "correlated_sample_Plackett.csv", clear
import delimited  "historical_correlated_sample_Plackett.csv", clear
sort year month 
bysort year month: gen n=_n
keep year month cod_corr had_corr cod_ind had_ind n
ds year month n, not 
renvarlab `r(varlist)', postfix(_plackett)

tempfile plackett
save `plackett', replace 


//import delimited  "correlated_sample_Gumbel.csv", clear
import delimited  "historical_correlated_sample_Gumbel.csv", clear
sort year month 
bysort year month: gen n=_n
keep year month cod_corr had_corr cod_ind had_ind n
ds year month n, not 
renvarlab `r(varlist)', postfix(_gumbel)

merge 1:1 year month n using `frank', nogen
merge 1:1 year month n using `clayton', nogen
merge 1:1 year month n using `plackett', nogen
merge 1:1 year month n using `gaussian', nogen

drop n


tempfile catch
save `catch', replace 

global yrz

levelsof year if year!=2020, local(yrs)
foreach y of local yrs{

local y_plus1=`y'+1

*Get the periods from year t
import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2010_2020.csv", clear  

keep if year==`y'
drop if dtrip==0

keep month period mode period2
tostring month, gen(month1)
tostring period, gen(period1)

gen domain=month1+"_"+period1+"_"+mode1
tempfile base`y' 
save `base`y'', replace 


levelsof domain, local(doms)

global domz
foreach d of local doms{
	
	u `base`y'', clear 
	keep if domain=="`d'"

	su month
	local mon=`r(mean)'
	di `mon'
	
	levelsof mode1, local(md)
	levelsof period2, local(pdz)

	use  `catch', clear  
 
		keep if year==`y_plus1'
		keep if month==`mon'
	
	
		gen domain="`d'"
		gen mode1=`md'
		gen period2=`pdz'
		tempfile domz`d'
		save `domz`d'', replace
		global domz "$domz "`domz`d''" " 
	
}
dsconcat $domz

tempfile yrz`y'
save `yrz`y'', replace
global yrz "$yrz "`yrz`y''" " 

}
dsconcat $yrz


tostring year, gen(year2)
gen domain2=period2+"_"+year2
order domain domain2 year year2 month period2 mode1 
drop year2 

ds domain domain2 year month period2 mode1, not
local vars  `r(varlist)'
foreach v of local vars{
replace `v'=round(`v') 
 }
 
drop domain2
rename year decade
gen year=decade
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\historical projected catch per trip 6_1.csv", replace  

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\historical projected catch per trip 5_29.csv", clear 
import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\historical projected catch per trip 6_1.csv", clear 







***Create a test dataset where the cod catch is set at 2020 levels and haddock is set at the projected decadal levels 
import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2010_2020.csv", clear   
keep if year ==2020
drop if dtrip==0
keep period2
tempfile regs
save `regs', replace 


import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\calibration catch per trip 2010_2020.csv", clear   
keep if year ==2020
merge m:1 period2 using `regs', keep(3) nogen 


tempfile new
save `new', replace 
global pdz

levelsof period2,  local(pds)
foreach p of local pds{
u `new', clear 
keep if period=="`p'"

expand 4
sample 10000, count 

gen tab=_n
tempfile pdz`p'
save `pdz`p'', replace
global pdz "$pdz "`pdz`p''" " 

}
dsconcat $pdz
expand 8, gen(dup)
bysort period tab: gen decade=_n

drop dup
tempfile base
save `base', replace 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\projection catch per trip 5_28.csv", clear  
*keep if decade ==1
bysort decade period: gen tab=_n
merge 1:1 period decade tab using `base'

drop year tab _merge

local vars cod_corr_gumbel cod_ind_gumbel cod_corr_frank cod_ind_frank cod_corr_clayton cod_ind_clayton cod_corr_plackett cod_ind_plackett cod_corr_gaussian cod_ind_gaussian
foreach v of local vars{
	replace `v'= tot_cat_cod
	
	
}
drop tot_cat_cod tot_cat_hadd

export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\projection catch per trip  6_24 TEST.csv", replace  

