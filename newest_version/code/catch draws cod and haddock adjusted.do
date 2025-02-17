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
clear mata
clear matrix
set maxvar 100000  
version 12.1

/* General strategy 
COMPUTE totals and std deviations  catch

 */
clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

clear

mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate

replace var_id=strat_id if strmatch(var_id,"")

 /* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
keep if inlist(st,23, 33, 25)


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


gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
*replace mode1="bt" if inlist(mode_fx, "4", "5", "6", "7")
*keep if mode1=="bt"
*replace mode1="fh" if inlist(mode_fx, "4", "5")
*replace mode1="pr" if inlist(mode_fx, "7")
*keep if inlist(mode1, "fh", "pr")

replace mode1="hd" if inlist(mode_fx, "4")
replace mode1="ch" if inlist(mode_fx, "5")
replace mode1="pr" if inlist(mode_fx, "7")
keep if inlist(mode1, "hd", "ch", "pr")

 /* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" */
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

gen common_dom="ZZ"
replace common_dom="ATLCO" if inlist(common, "atlanticcod") 
replace common_dom="ATLCO" if inlist(common, "haddock") 

replace common_dom="ATLCO"  if inlist(prim1_common, "atlanticcod") 
replace common_dom="ATLCO"  if inlist(prim1_common, "haddock") 


tostring year, gen(yr2)
tostring wave, gen(wave2)
*gen my_dom_id_string=state+"_"+common_dom+"_"+yr2
*gen my_dom_id_string=state+"_"+common_dom
*gen my_dom_id_string=state+"_"+month1+"_"+mode1+"_"+common_dom
*gen my_dom_id_string=mode1+"_"+yr2+"_"+common_dom
*gen my_dom_id_string=yr2+"_"+wave2+"_"+mode1+"_"+common_dom
gen my_dom_id_string=yr2+"_"+wave2+"_"+mode1+"_"+area_x+"_"+state+"_"+common_dom


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
  For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "ATLCO."  If there is no my_common catch, but the  trip targeted (cod or haddock) or caught cod, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified as an (A2 from above).
4. After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/


local vars claim harvest release landing tot_cat
foreach v of local vars{
	gen cod_`v'=`v' if common=="atlanticcod"
	egen sum_cod_`v'=sum(cod_`v'), by(strat_id psu_id id_code )
	drop cod_`v'
	rename sum_cod_`v' cod_`v'
	
	gen hadd_`v'=`v' if common=="haddock"
	egen sum_hadd_`v'=sum(hadd_`v'), by(strat_id psu_id id_code )
	drop hadd_`v'
	rename sum_hadd_`v' hadd_`v'
	
}


/*
1  Set tot_cat, landing, claim, harvest, and release to zero for all instances of common~="my_common"
2.  We set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise.*/


 gen no_dup=0
 replace no_dup=1 if !inlist(common, "atlanticcod", "haddock")
 /*
 replace no_dup=1 if  strmatch(common, "summerflounder")==0
 replace no_dup=1 if strmatch(common, "blackseabass")==0
 replace no_dup=1 if strmatch(common, "scup")==0
*/
 bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row
order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common

/*classify into GOM or GBS */

preserve 
import excel using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\ma_site_list_updated_SS.xlsx", clear first
keep SITE_EXTERNAL_ID NMFS_STAT_AREA
renvarlab, lower
rename site_external_id intsite
tempfile mrip_sites
save `mrip_sites', replace 
restore

merge m:1 intsite using `mrip_sites',  keep(1 3)

gen str3 area_s="AAA"

replace area_s="GOM" if st2=="23" | st2=="33"
replace area_s="GOM" if st2=="25" & inlist(nmfs_stat_area,511, 512, 513,  514)
replace area_s="GBS" if st2=="25" & inlist(nmfs_stat_area,521, 526, 537,  538)
replace area_s="GOM" if st2=="25" & intsite==224

/*check 
replace my_dom_id_string=yr2+"_"+state+"_"+common_dom
encode my_dom_id_string, gen(my_dom_id)
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
svy:total hadd_tot_cat if st2=="25" & common_dom=="ATLCO", over(my_dom_id)
*/
keep if common_dom=="ATLCO"
keep if area_s=="GOM"
/*
keep if state=="MA"
replace my_dom_id_string=yr2+common_dom
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
encode my_dom_id_string, gen(my_dom_id)
svy:total cod_tot_cat, over(my_dom_id)
svy:total hadd_tot_cat, over(my_dom_id)
*/

keep var_id strat_id psu_id id_code common sp_code claim harvest release landing ///
	tot_cat wp_catch  leader cntrbtrs wp_int state mode1  my_dom_id_string /// 
	cod_claim hadd_claim  cod_harvest hadd_harvest  cod_release hadd_release  cod_landing hadd_landing  ///
	hadd_tot_cat cod_tot_cat  claim_unadj harvest_unadj release_unadj year month year wave area_x  hrsf ffdays2 ffdays12

*original unadjusted catch stats
/*
local vars  cod 
foreach v of local vars{
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

svy:total `v'_claim if my_dom_id_string=="bt_1982_ATLCO"
mat list r(table), format(%12.0gc)
	
svy:total `v'_claim if my_dom_id_string=="bt_2019_ATLCO"
mat list r(table), format(%12.0gc)	
	
svy:total `v'_harvest if my_dom_id_string=="bt_1982_ATLCO"
mat list r(table), format(%12.0gc)

svy:total `v'_harvest if my_dom_id_string=="bt_2019_ATLCO"
mat list r(table), format(%12.0gc)

svy:total `v'_landing if my_dom_id_string=="bt_1982_ATLCO"
mat list r(table), format(%12.0gc)

svy:total `v'_landing if my_dom_id_string=="bt_2019_ATLCO"
mat list r(table), format(%12.0gc)

svy:total `v'_release if my_dom_id_string=="bt_1982_ATLCO"
mat list r(table), format(%12.0gc)

svy:total `v'_release if my_dom_id_string=="bt_2019_ATLCO"
mat list r(table), format(%12.0gc)

svy:total `v'_tot_cat if my_dom_id_string=="bt_1982_ATLCO"
mat list r(table), format(%12.0gc)

svy:total `v'_tot_cat if my_dom_id_string=="bt_2019_ATLCO"
mat list r(table), format(%12.0gc)
}
*/
 



/***********************************************************************
Following code defines new catch count fields to handle grouped catches
************************************************************************/

*1)
*proc sort data=mycatch;
*	by strat_id psu_id leader id_code;
*run;

sort strat_id psu_id leader id_code

*2) data mycatch;
*	set mycatch;
*	by strat_id psu_id leader id_code;
*	group_order + 1;
*	if first.leader then group_order=1;
*run;

bysort strat_id psu_id leader id_code: gen group_order= _n 

*3) data mycatch;
*	set mycatch;
*	if claim=. then claim=0;
*	if harvest=. then harvest=0;
*	if landing=. then landing=0;
*	new_claim1 = claim/cntrbtrs;
	*new_release = release;
*	new_harvest = harvest;
*run;

local vars claim harvest release
foreach v of local vars{
	
	replace cod_`v'=0 if cod_`v'==.
	replace hadd_`v'=0 if hadd_`v'==.

}

*As per John Foster's recommendation, we need to divide claim by total contributers 2) harvest and release by counts of id_code within leader 
*First, sum the catch within leader:
local vars claim harvest release
foreach v of local vars{
	
	egen sum_cod_`v'=sum(cod_`v'), by(strat_id psu_id leader)
	egen sum_hadd_`v'=sum(hadd_`v'), by(strat_id psu_id leader)

}

*Now get counts of id_codes within leader: 
gen tab=1
egen count_id_codes=sum(tab), by(strat_id psu_id leader)
drop tab 

egen max_cntrbtrs=max(cntrbtrs), by(strat_id psu_id leader)

	
*Now divide claim by cntrbtrs and the other catch variables by count_id_codes:
local vars cod hadd
foreach v of local vars{
	
gen new_claim1_`v'=sum_`v'_claim/cntrbtrs
gen new_harvest1_`v'=sum_`v'_harvest/count_id_codes
gen new_release1_`v'=sum_`v'_release/count_id_codes

}

*Now multiply wp_int by count of id_code to get new wp_catch:
gen new_wp_int=wp_int*count_id_codes
	
*we ultimately keep only the first observation within leader, so mark these rows:
bysort strat_id psu_id leader: gen first=1 if _n==1

mvencode new_claim1_* new_harvest1_* new_release1_*, mv(0) override

*generate total catch for the species of interest:
local vars cod hadd
foreach v of local vars{
	gen tot_cat_`v'=new_claim1_`v'+new_harvest1_`v'+new_release1_`v'
	gen landing_`v'=new_claim1_`v'+new_harvest1_`v'

}

su wp_int
local sum_original_wt=`r(sum)'

su new_wp_int if first==1
local sum_new_wt=`r(sum)'

di `sum_original_wt'
di `sum_new_wt'


*test to see how adjusted catch stats match up
/*
local vars  cod 
foreach v of local vars{
svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
*keep if first==1
svy:total new_claim1_`v' if first==1 & my_dom_id_string=="bt_1982_ATLCO"
mat list r(table), format(%12.0gc)
	
svy:total new_claim1_`v' if first==1 & my_dom_id_string=="bt_2019_ATLCO"
mat list r(table), format(%12.0gc)


svy:total new_harvest1_`v' if first==1 & my_dom_id_string=="bt_1982_ATLCO"
mat list r(table), format(%12.0gc) 

svy:total new_harvest1_`v' if first==1 & my_dom_id_string=="bt_2019_ATLCO"
mat list r(table), format(%12.0gc) 


svy:total landing_`v' if first==1 & my_dom_id_string=="bt_1982_ATLCO"
mat list r(table), format(%12.0gc)

svy:total landing_`v' if first==1 & my_dom_id_string=="bt_2019_ATLCO"
mat list r(table), format(%12.0gc)


svy:total new_release1_`v' if first==1 & my_dom_id_string=="bt_1982_ATLCO"
mat list r(table), format(%12.0gc)

svy:total new_release1_`v' if first==1 & my_dom_id_string=="bt_2019_ATLCO"
mat list r(table), format(%12.0gc)

svy:total tot_cat_`v' if first==1 & my_dom_id_string=="bt_1982_ATLCO"
mat list r(table), format(%12.0gc)

svy:total tot_cat_`v' if first==1 & my_dom_id_string=="bt_2019_ATLCO"
mat list r(table), format(%12.0gc)
}
*/
 
keep if first==1
keep year month wave area_x state mode1 tot_cat_cod tot_cat_hadd new_wp_int psu_id strat_id  hrsf ffdays2 ffdays12


* replace the hours fished and number days fished past 2/12 month variables as missing if needed
replace hrsf=. if hrsf==99.8
replace ffdays2=. if inlist(ffdays2, 99, 98)
replace ffdays12=. if inlist(ffdays12, 999, 998)


*For catch per trip data that maintains the independence in catch 
svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
*svy:mean tot_cat_cod if year==2020 & mode=="fh" & month=="07"
*save "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\cod_hadd_trips_adjusted_all_years_wts_6_25.dta", replace 


*For catch per trip data that maintains the dependce in catch 
replace new_wp_int=round(new_wp_int) 
expand new_wp_int
drop new_wp_int


preserve
keep if year>=2010 & year<=2021
drop if tot_cat_cod==. | tot_cat_hadd==.
collapse (sum) new_release1_cod new_release1_hadd tot_cat_cod landing_cod tot_cat_hadd landing_hadd, by(year)

rename new_release1_cod cod_releases
rename tot_cat_cod cod_tot_cat
rename landing_cod cod_harvest

rename new_release1_hadd hadd_releases
rename tot_cat_hadd hadd_tot_cat
rename landing_hadd hadd_harvest
export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\total AB1B2 2010_2020 GoM.csv", replace 
restore

*save "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\cod_hadd_trips_adjusted_all_years_mode_7_26.dta", replace 
save "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\cod_hadd_catch_data_1_15.dta", replace 

