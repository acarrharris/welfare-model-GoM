
**Create a dataset of catch draws in 2030, 2040, ..., 2080  containing 10,000 catch draws per period
import delimited  "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data\correlated_sample_Clayton.csv", clear
sort decade month obs
bysort decade month obs: gen n=_n
keep obs month decade cod_corr had_corr cod_ind had_ind n
ds obs month decade n, not 
renvarlab `r(varlist)', postfix(_clayton)

tempfile clayton
save `clayton', replace 



import delimited  "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data\correlated_sample_Frank.csv", clear
sort decade month obs
bysort decade month obs: gen n=_n
keep obs month decade cod_corr had_corr cod_ind had_ind n
ds obs month decade n, not 
renvarlab `r(varlist)', postfix(_frank)

tempfile frank
save `frank', replace 



import delimited  "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data\correlated_sample_Gumbel.csv", clear
sort decade month obs
bysort decade month obs: gen n=_n
keep obs month decade cod_corr had_corr cod_ind had_ind n
ds obs month decade n, not 
renvarlab `r(varlist)', postfix(_gumbel)

merge 1:1 obs month decade n using `frank', nogen
merge 1:1 obs month decade n using `clayton', nogen
drop n



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
order domain decade month period2 mode1    obs
 drop domain obs

ds decade month period2 mode1, not
local vars  `r(varlist)'
foreach v of local vars{
replace `v'=round(`v') 
 }
 
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\projection catch per trip.csv", replace  





