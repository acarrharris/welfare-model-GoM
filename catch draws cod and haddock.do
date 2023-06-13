/* This is a file that produces a dataset that contains #of fish encountered per trip.
This is a port of Scott's "domain_catch_frequencies_gom_cod_wave_2013.sas"



This is a template program for estimating catch frequecies
using the MRIP public-use datasets.

The program is setup to use information in the trip_yyyyw
dataset to define custom domains.  The catch frequencies are
estimated within the domains by merging the trip information
onto the catch_yyyyw datasets.

Required input datasets:
 trip_yyyyw
 catch_yyyyw

yyyy = year
w    = wave


*/
clear
mata: mata clear
clear matrix 
set matsize 10000
set maxvar  120000
version 12.1

/* General strategy 
COMPUTE totals and std deviations  catch

 */
clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"


**************************************
*FINAL SIMULATION MODEL DATA 
**************************************

***Repeat again for fluke catch target trips, but only draw 10000 trips per state for the boat mode as requested by Jorge on 3/22. 
***This code keeps only boat catch. Will re-run below to get shore catch 

/* General strategy 
COMPUTE totals and std deviations  catch

 */

mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
replace id_code=ID_CODE if id_code=="" & ID_CODE!=""
drop if strmatch(id_code, "*xx*")==1
drop if strat_id==""
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate
 /* THIS IS THE END OF THE DATA MERGING CODE */

replace mode_fx=MODE_FX if mode_fx=="" & MODE_FX!=""
replace area_x=AREA_X if area_x=="" & AREA_X!=""
 
 
 

 
tempfile base 
save `base', replace 
 
 global catch_drawz
*levels year if inlist(year, 2019, 2020), local(yrs)
*Start the loop for each year in the time series
*levelsof year if inlist(year, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998), local(yrs)
*levelsof year if year<1990, local(yrs)

*foreach y of local yrs{

u `base', clear 

*keep if inlist(year,`y') & mode_fx!="3"
*keep if inlist(year,2020) 
keep if mode_fx!="3"



 /* ensure only relevant states */
keep if inlist(st,23, 33, 25)

/*This is the "full" mrip data */
tempfile tc1
save `tc1'
 

gen st2 = string(st,"%02.0f")
tostring year, gen(yr2)

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="ME" if st==23
replace state="NH" if st==33


 /* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" */
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

gen common_dom="ZZ"
replace common_dom="ATLCO" if inlist(common, "atlanticcod") 
replace common_dom="ATLCO" if inlist(common, "haddock") 

replace common_dom="ATLCO"  if inlist(prim1_common, "atlanticcod") 
replace common_dom="ATLCO"  if inlist(prim1_common, "haddock") 


/* we need to retain 1 observation for each strat_id, psu_id, and id_code.  */
/* A.  Trip (Targeted or Caught) (Cod or Haddock) then it should be marked in the domain "_ATLCO"
	1. Caught my_common.  We retain tot_cat
	2. Did not catch my_common.  We set tot_cat=0
   B.  Trip did not (Target or Caught) (Cod or Haddock) then it is marked in the the domain "ZZZZZ"
	1. Caught my_common.  This is impossible.
	2. Did not catch my_common.  We set tot_cat=0
	
To do this:
1.  We set tot_cat, landing, claim, harvest, and release to zero for all instances of common~="my_common"
2.  We set a variable "no_dup"=0 if the record is "my_common" catch and no_dup=1 otherwise.
3.  We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string".
  For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "ATLCO."  If there is no my_common catch, but the 
  trip targeted (cod or haddock) or caught cod, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified as an (A2 from above).
4. After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/


/*
4.  We set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise.*/
 gen no_dup=0
 replace no_dup=1 if strmatch(common, "atlanticcod")==0
 replace no_dup=1 if strmatch(common, "haddock")==0

 *catch frequency adjustments for grouped catch (multiple angler catches reported on a single record);
 
  foreach var of varlist tot_cat landing claim harvest release {
	*replace `var'=`var'/cntrbtrs if cntrbtrs>0 
	 replace `var'=round(`var')
 }
 
gen cod_tot_cat=tot_cat if common=="atlanticcod"
egen sum_cod_tot_cat=sum(cod_tot_cat), by(strat_id psu_id id_code)
 
gen hadd_tot_cat=tot_cat if common=="haddock"
egen sum_hadd_tot_cat=sum(hadd_tot_cat), by(strat_id psu_id id_code)

mvencode sum*, mv(0) over

gen my_dom_id_string=state+"_"+common_dom

/*5.  We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string".
6. After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1
order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common

*keep if common_dom=="SF"
rename intsite SITE_ID
merge m:1 SITE_ID using "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-species-shift/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc

drop _merge

/*classify into GOM or GBS */
gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & strmatch(stock_region_calc,"NORTH")
replace area_s="GBS" if st2=="25" & strmatch(stock_region_calc,"SOUTH")



keep strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common prim2_common prim1_common  mode_fx area_x /// 
		year st state wp_int wp_catch claim release harvest landing tot_cat st2 common_dom  sum_cod_tot_cat sum_hadd_tot_cat  month area_s

rename sum_cod_tot_cat cod_tot_cat	
rename sum_hadd_tot_cat hadd_tot_cat		

keep if common_dom=="ATLCO"
keep if area_s=="GOM"

encode my_dom_id, gen(my_dom_id)
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

*svy: total cod_tot_cat, over(my_dom_id) /*check to see estmates line up with MRIP online query*/
*svy: total hadd_tot_cat, over(my_dom_id) /*check to see estmates line up with MRIP online query*/


gen wp_int_round=round(wp_int)

keep id_code state year month cod_tot_cat hadd_tot_cat  wp_int_round  mode_fx  area_s
expand wp_int_round
destring month, replace

drop wp_int
save "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\cod_hadd_trips_all_years.dta", replace 



clear 
u "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\cod_hadd_trips_all_years.dta", clear 
keep if inlist(year, 2020)
gen month1 = string(month,"%02.0f")

tempfile base
save `base', replace

global catch_drawz
levelsof month1, local(mnths)
foreach m of local mnths{
	u `base', clear 
	keep if month1=="`m'"
	
	su month
	local ntrip=`r(N)'
	if `ntrip'<=10000{
		local expand=round((10000/`r(N)'))+2
		expand `expand'
		sample 10000, count
		}
	
	else{
		sample 10000, count 
	}
	keep month month1 cod hadd 

	*export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\simulated_catch_`m'.csv", replace 
	
tempfile catch_drawz`m'
save `catch_drawz`m'', replace
global catch_drawz "$catch_drawz "`catch_drawz`m''" " 
}


clear
dsconcat $catch_drawz


 * now adjust the data so that instead of 10000 draws per  month, and mode, 
* we will have 10000 draws per BI-MONTHLY period, and mode. This will go directly in the simulaiton model
/*
clear
tempfile base
save `base', emptyok

local myfilelist : dir . files"simulated_ind_catch_*.csv"
foreach file of local myfilelist {
drop _all
insheet using `"`file'"'
local outfile = subinstr("`file'",".csv","",.)
append using "`base'"
save `base', replace
}

*/

tempfile all_catch
save `all_catch', replace 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2020.csv", clear  
keep  month period mode1 period2

levelsof period2, local(pds)

tempfile new
save `new', replace

global catch_draws
foreach p of local pds{
		u `new', clear 
		keep if period2=="`p'"
		levelsof period2, local(pd)
		levelsof month, local(mnth)

		u `all_catch', clear
		keep if month==`mnth'
		
		gen period2="`p'"

*export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\simulated_ind_catch_`s'_`p'.csv",  replace
	
tempfile catch_draws`p'
save `catch_draws`p'', replace
global catch_draws "$catch_draws "`catch_draws`p''" " 
}

clear
dsconcat $catch_draws

export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\simulated_ind_catch.csv",  replace

