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
*replace id_code=ID_CODE if id_code=="" & ID_CODE!=""
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
*replace mode_fx=MODE_FX if mode_fx=="" & MODE_FX!=""
*replace area_x=AREA_X if area_x=="" & AREA_X!=""


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


gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="bt" if inlist(mode_fx, "4", "5", "6", "7")
keep if mode1=="bt"


 /* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" */
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

gen common_dom="ZZ"
replace common_dom="ATLCO" if inlist(common, "atlanticcod") 
replace common_dom="ATLCO" if inlist(common, "haddock") 

replace common_dom="ATLCO"  if inlist(prim1_common, "atlanticcod") 
replace common_dom="ATLCO"  if inlist(prim1_common, "haddock") 


tostring year, gen(yr2)
*gen my_dom_id_string=state+"_"+common_dom+"_"+yr2
*gen my_dom_id_string=state+"_"+common_dom
*gen my_dom_id_string=state+"_"+month1+"_"+mode1+"_"+common_dom
gen my_dom_id_string=mode1+"_"+yr2+"_"+common_dom


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

keep if common_dom=="ATLCO"
keep if area_s=="GOM"


keep var_id strat_id psu_id id_code common sp_code claim harvest release landing ///
	tot_cat wp_catch  leader cntrbtrs wp_int state mode1 /// 
	cod_claim hadd_claim  cod_harvest hadd_harvest  cod_release hadd_release  cod_landing hadd_landing  ///
	hadd_tot_cat cod_tot_cat  claim_unadj harvest_unadj release_unadj year month year
/*
*original unadjusted catch datasets
local vars sf bsb scup
foreach v of local vars{
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)
svy:total `v'_claim
mat list r(table), format(%12.0gc)
	
svy:total `v'_harvest
mat list r(table), format(%12.0gc)

svy:total `v'_landing
mat list r(table), format(%12.0gc)

svy:total `v'_release
mat list r(table), format(%12.0gc)

svy:total `v'_tot_cat
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
preserve 
keep if first==1
su new_wp_int 
local sum_new_wt=`r(sum)'
di `sum_new_wt'

local vars sf bsb scup
foreach v of local vars{
	
svyset psu_id [pweight= new_wp_int], strata(strat_id) singleunit(certainty)
svy:total new_claim1_`v'
mat list r(table), format(%12.0gc)
	
svy:total new_harvest1_`v'
mat list r(table), format(%12.0gc)

svy:total landing_`v'
mat list r(table), format(%12.0gc)

svy:total new_release1_`v'
mat list r(table), format(%12.0gc)

svy:total tot_cat_`v'
mat list r(table), format(%12.0gc)

svy:total tot_cat_all_species_new 
mat list r(table), format(%12.0gc) // 219,658,442.4

}

restore
*/

keep if first==1

keep year month state mode1 tot_cat_cod tot_cat_hadd new_wp_int
replace new_wp_int=round(new_wp_int) 

mvencode tot_cat_cod tot_cat_hadd, mv(0) override

expand new_wp_int

drop new_wp_int

save "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-species-shift\cod_hadd_trips_adjusted_21_22.dta", replace 
