***Catch totals all years  (2010-2020)

*Newest method:
*Compute catch totals from the catch-per-trip data that enters into the model-GoM\directed


*This file creates total catch of summer flounder and other species on directed summer flounder trips
	*It also creates catch-per-trip distributions for non-SF/BSB
	*It also exports a sample of trips that contain tip catch levels of fluke and black sea bass for the copula model

/*
set matsize 10000

version 12.1

/* General strategy 
COMPUTE totals and std deviations  catch

 */
clear
*cd "C:\Users\Lou\Desktop\MRIP_data"

clear

mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1

save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate
 

*keep if inlist(year,  2020)

 
/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
keep if inlist(st,23, 33, 25)

/*This is the "full" mrip data */
tempfile tc1
save `tc1'
 
gen st2 = string(st,"%02.0f")

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="NH" if st==33
replace state="ME" if st==23


 /* classify trips that I care about into the things I care about (caught or targeted cod/haddock) and things I don't care about "ZZZZZZZZ" */
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

gen common_dom="ZZ"
replace common_dom="ATLCO" if inlist(common, "atlanticcod") 
replace common_dom="ATLCO" if inlist(common, "haddock") 

replace common_dom="ATLCO"  if inlist(prim1_common, "atlanticcod") 
replace common_dom="ATLCO"  if inlist(prim1_common, "haddock") 

tostring wave, gen(w2)
gen yr2 = string(year)

*gen my_dom_id_string=common_dom+"_"+yr2


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
*turn this off when calculating totals, turn it in when calculating catch per trip distributions. 

/*
 replace claim=claim/cntrbtrs if cntrbtrs>0 
  foreach var of varlist tot_cat landing claim harvest release {
	replace `var'=`var'/cntrbtrs if cntrbtrs>0 
	*replace `var'=round(`var')
	replace `var'= ceil(`var')
 }
 */

 
*Calculate the number of cod/hadd caught per trip; all this info must be contained on one row

gen cod_tot_cat=tot_cat if common=="atlanticcod"
egen sum_cod_tot_cat=sum(cod_tot_cat), by(strat_id psu_id id_code)

gen cod_harvest=landing if common=="atlanticcod"
egen sum_cod_harvest=sum(cod_harvest), by(strat_id psu_id id_code)
 
gen cod_releases=release if common=="atlanticcod"
egen sum_cod_releases=sum(cod_releases), by(strat_id psu_id id_code)
 
 
gen hadd_tot_cat=tot_cat if common=="haddock"
egen sum_hadd_tot_cat=sum(hadd_tot_cat), by(strat_id psu_id id_code)

gen hadd_harvest=landing if common=="haddock"
egen sum_hadd_harvest=sum(hadd_harvest), by(strat_id psu_id id_code)
 
gen hadd_releases=release if common=="haddock"
egen sum_hadd_releases=sum(hadd_releases), by(strat_id psu_id id_code)

mvencode sum*, mv(0) over



preserve 
import excel using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\ma_site_list_updated_SS.xlsx", clear first
keep SITE_EXTERNAL_ID NMFS_STAT_AREA
renvarlab, lower
rename site_external_id intsite
tempfile mrip_sites
save `mrip_sites', replace 
restore

merge m:1 intsite using `mrip_sites',  keep(1 3)


/*
rename intsite SITE_ID
merge m:1 SITE_ID using "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-species-shift/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc
*/
*drop _merge

/*classify into GOM or GBS */
gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
/*
replace area_s="GOM" if st2=="25" & strmatch(stock_region_calc,"NORTH")
replace area_s="GBS" if st2=="25" & strmatch(stock_region_calc,"SOUTH")
*/

replace area_s="GOM" if st2=="25" & inlist(nmfs_stat_area,11, 512, 513,  514)
replace area_s="GBS" if st2=="25" & inlist(nmfs_stat_area,521, 526, 537,  538)
replace area_s="GOM" if st2=="25" & intsite==224

keep if common_dom=="ATLCO"
keep if area_s=="GOM"

gen my_dom_id_string=common_dom+"_"+yr2


/*5.  We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string".
6. After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.*/
bysort year strat_id psu_id id_code (no_dup my_dom_id_string ): gen count_obs1=_n
keep if count_obs1==1
order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common

drop if mode_fx=="3"
*keep if common_dom=="SF"

keep strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common prim2_common prim1_common /// 
		year yr2 st state wp_int wp_catch claim release harvest landing tot_cat st2 common_dom  area_s ///
		sum_cod_tot_cat sum_hadd_tot_cat     ///
		sum_cod_releases sum_hadd_releases     ///
		sum_cod_harvest sum_hadd_harvest   


rename sum_cod_tot_cat cod_tot_cat	
rename sum_hadd_tot_cat hadd_tot_cat			

rename sum_cod_harvest cod_harvest	
rename sum_hadd_harvest hadd_harvest			
	
rename sum_cod_releases cod_releases	
rename sum_hadd_releases hadd_releases			


encode my_dom_id, gen(my_dom_id)
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
/*
replace sf_tot_cat=sf_tot_cat/100000
svy: mean sf_tot_cat ,  cformat(%2.0gc), over(my_dom_id)  
svy: total sf_releases , over(my_dom_id)  
svy: total sf_harvest , over(my_dom_id)  
*/
tempfile new
save `new', replace 

global yrz
levelsof year, local(yrs)
foreach y of local yrs{
	
	u `new', clear 
	keep if year==`y'


local i=1
local myvariables cod_tot_cat cod_releases cod_harvest hadd_tot_cat hadd_releases hadd_harvest 
foreach var of local myvariables{
	svy: total `var' if year==`y', over(my_dom_id)
	
	mat b`i'=e(b)'
	mat colnames b`i'=`var'
	mat V=e(V)

	local ++i 
}
local --i
sort year my_dom_id
dups my_dom_id, drop terse
keep my_dom_id  area_s  common_dom  year

foreach j of numlist 1/`i'{
	svmat b`j', names(col)
}

drop if common_dom=="ZZ"
drop if area_s=="GBS"


tempfile yrz`y'
save `yrz`y'', replace
global yrz "$yrz "`yrz`y''" " 
		

}

dsconcat $yrz
duplicates drop 
drop if area_s=="AAA"
gen prop_cod_harv= 1-cod_harvest/cod_tot_cat
gen prop_hadd_harv= 1-hadd_harvest/hadd_tot_cat
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\total AB1B2 2010_2020 GoM.csv", replace 
*/



**Cod was closed 2015, 2017, and 2018, but there was still harvest in these years. See when this harvest occurred. 
/*
set matsize 10000

version 12.1

/* General strategy 
COMPUTE totals and std deviations  catch

 */
clear
*cd "C:\Users\Lou\Desktop\MRIP_data"

clear

mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1

save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate
 

*keep if inlist(year,  2020)

 
/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
keep if inlist(st,23, 33, 25)

/*This is the "full" mrip data */
tempfile tc1
save `tc1'
 
gen st2 = string(st,"%02.0f")

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



 /* classify trips that I care about into the things I care about (caught or targeted cod/haddock) and things I don't care about "ZZZZZZZZ" */
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

gen common_dom="ZZ"
replace common_dom="ATLCO" if inlist(common, "atlanticcod") 
replace common_dom="ATLCO" if inlist(common, "haddock") 

replace common_dom="ATLCO"  if inlist(prim1_common, "atlanticcod") 
replace common_dom="ATLCO"  if inlist(prim1_common, "haddock") 

tostring wave, gen(w2)
gen yr2 = string(year)

*gen my_dom_id_string=common_dom+"_"+yr2


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
*turn this off when calculating totals, turn it in when calculating catch per trip distributions. 

/*
 replace claim=claim/cntrbtrs if cntrbtrs>0 
  foreach var of varlist tot_cat landing claim harvest release {
	replace `var'=`var'/cntrbtrs if cntrbtrs>0 
	*replace `var'=round(`var')
	replace `var'= ceil(`var')
 }
 */

 
*Calculate the number of cod/hadd caught per trip; all this info must be contained on one row

gen cod_tot_cat=tot_cat if common=="atlanticcod"
egen sum_cod_tot_cat=sum(cod_tot_cat), by(strat_id psu_id id_code)

gen cod_harvest=landing if common=="atlanticcod"
egen sum_cod_harvest=sum(cod_harvest), by(strat_id psu_id id_code)
 
gen cod_releases=release if common=="atlanticcod"
egen sum_cod_releases=sum(cod_releases), by(strat_id psu_id id_code)
 
 
gen hadd_tot_cat=tot_cat if common=="haddock"
egen sum_hadd_tot_cat=sum(hadd_tot_cat), by(strat_id psu_id id_code)

gen hadd_harvest=landing if common=="haddock"
egen sum_hadd_harvest=sum(hadd_harvest), by(strat_id psu_id id_code)
 
gen hadd_releases=release if common=="haddock"
egen sum_hadd_releases=sum(hadd_releases), by(strat_id psu_id id_code)

mvencode sum*, mv(0) over




*merge to GoM site id's
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

*keep if inlist(year, 2015, 2017, 2018)

gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring month1, replace
destring day1, replace

gen period ="1" if day1<=15 & month1==1
replace period ="2" if day1>15 & month1==1
replace period ="3" if day1<=15 & month1==2
replace period ="4" if day1>15 & month1==2
replace period ="5" if day1<=15 & month1==3
replace period ="6" if day1>15 & month1==3
replace period ="7" if day1<=15 & month1==4
replace period ="8" if day1>15 & month1==4
replace period ="9" if day1<=15 & month1==5
replace period ="10" if day1>15 & month1==5
replace period ="11" if day1<=15 & month1==6
replace period ="12" if day1>15 & month1==6
replace period ="13" if day1<=15 & month1==7
replace period ="14" if day1>15 & month1==7
replace period ="15" if day1<=15 & month1==8
replace period ="16" if day1>15 & month1==8
replace period ="17" if day1<=15 & month1==9
replace period ="18" if day1>15 & month1==9
replace period ="19" if day1<=15 & month1==10
replace period ="20" if day1>15 & month1==10
replace period ="21" if day1<=15 & month1==11
replace period ="22" if day1>15 & month1==11
replace period ="23" if day1<=15 & month1==12
replace period ="24" if day1>15 & month1==12



gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")


*gen my_dom_id_string=state+"_"+mode1+"_"+common_dom+"_"+area_s+"_"+yr2+"_"+period
gen my_dom_id_string=mode1+"_"+common_dom+"_"+area_s+"_"+yr2+"_"+period


/*5.  We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string".
6. After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.*/
bysort year strat_id psu_id id_code (no_dup my_dom_id_string ): gen count_obs1=_n
keep if count_obs1==1
order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common

drop if mode_fx=="3"
*keep if common_dom=="SF"

keep strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common prim2_common prim1_common  mode1 /// 
		year yr2 st state wp_int wp_catch claim release harvest landing tot_cat st2 common_dom  area_s period state ///
		sum_cod_tot_cat sum_hadd_tot_cat     ///
		sum_cod_releases sum_hadd_releases     ///
		sum_cod_harvest sum_hadd_harvest   


rename sum_cod_tot_cat cod_tot_cat	
rename sum_hadd_tot_cat hadd_tot_cat			

rename sum_cod_harvest cod_harvest	
rename sum_hadd_harvest hadd_harvest			
	
rename sum_cod_releases cod_releases	
rename sum_hadd_releases hadd_releases			


encode my_dom_id, gen(my_dom_id)
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
/*
replace sf_tot_cat=sf_tot_cat/100000
svy: mean sf_tot_cat ,  cformat(%2.0gc), over(my_dom_id)  
svy: total sf_releases , over(my_dom_id)  
svy: total sf_harvest , over(my_dom_id)  
*/
tempfile new
save `new', replace 

global yrz
levelsof year, local(yrs)
foreach y of local yrs{
	
	u `new', clear 
	keep if year==`y'


local i=1
local myvariables cod_tot_cat cod_releases cod_harvest hadd_tot_cat hadd_releases hadd_harvest 
foreach var of local myvariables{
	svy: total `var' if year==`y', over(my_dom_id)
	
	mat b`i'=e(b)'
	mat colnames b`i'=`var'
	mat V=e(V)

	local ++i 
}
local --i
sort year my_dom_id
dups my_dom_id, drop terse
keep my_dom_id  area_s  common_dom  year period mode1 //state 

foreach j of numlist 1/`i'{
	svmat b`j', names(col)
}

drop if common_dom=="ZZ"
drop if area_s=="GBS"


tempfile yrz`y'
save `yrz`y'', replace
global yrz "$yrz "`yrz`y''" " 
		

}

dsconcat $yrz
duplicates drop 
drop if area_s=="AAA"
destring period, replace
sort year period //state 

/*
egen sum_cod_harvest=sum(cod_harvest), by(year)
gen proportion_cod_harvest_by_period=cod_harvest/sum_cod_harvest
egen sum_prop=sum(proportion_cod_harvest_by_period), by(state year)
*/
tostring period, replace
gen period2=period+"_"+mode1
drop period

tempfile base
save `base', replace 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2010_2020.csv", clear 
merge 1:1 period2 year using `base'
*/