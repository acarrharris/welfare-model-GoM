

cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

clear

global fluke_effort

*local yrs 2019
*foreach y of local yrs{

tempfile tl1 cl1
dsconcat $triplist

/* *dtrip will be used to estimate total directed trips, do not change it*/

gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
*drop strat_interval
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
*keep if strmatch(common, "summerflounder") | strmatch(common,"summerflounder")
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3)
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

drop _merge
 
*keep if inlist(year, 2021, 2020, 2019, 2018) 
keep if inlist(year, 2020) 




/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
keep if inlist(st,23, 33, 25)


/*This is the "full" mrip data */
*tempfile tc1
*save `tc1'



 /* classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN). */
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "atlanticcod") 
replace dom_id="1" if strmatch(prim1_common, "atlanticcod") 

replace dom_id="1" if strmatch(common, "haddock") 
replace dom_id="1" if strmatch(prim1_common, "haddock") 



tostring wave, gen(w2)
tostring year, gen(year2)
gen st2 = string(st,"%02.0f")

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")


*gen mode1="shor" if inlist(mode_fx, "1", "2", "3") 
*replace mode1="priv" if inlist(mode_fx, "7") 
*replace mode1="chhd" if inlist(mode_fx, "4", "5") 

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

gen date2=mdy(month1,day1,year)
format date2 %td


*keep only fishin year 2020
*keep if date2>=td(01may2020) & date2<=td(30apr2021)

keep if inlist(year, 2020)


/*
destring day1, replace
sort year month1 day1
gen date2=mdy(month1,day1,year)
format date2 %td
*/
*gen season="open" if date2>=td(24may2019) & date2<=td(21sep2019)
*replace season="closed" if season==""


/*Deal with Group Catch -- this bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
Then it generates a flag for claim equal to the largest claim.  
Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1  */

replace claim=0 if claim==.

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (claim): gen claim_flag=claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")




/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n

keep if count_obs1==1

/*
*keep if common_dom=="SF"
rename intsite SITE_ID
merge m:1 SITE_ID using "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-species-shift/ma site allocation.dta",  keep(1 3)
rename  SITE_ID intsite
rename  STOCK_REGION_CALC stock_region_calc

drop _merge
*/

preserve 
import excel using "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/ma_site_list_updated_SS.xlsx", clear first
keep SITE_EXTERNAL_ID NMFS_STAT_AREA
renvarlab, lower
rename site_external_id intsite
tempfile mrip_sites
save `mrip_sites', replace 
restore

merge m:1 intsite using `mrip_sites',  keep(1 3)


/*classify into GOM or GBS */
*gen str3 area_s="AAA"

*replace area_s="GOM" if st2=="23" | st2=="33"
*replace area_s="GOM" if st2=="25" & strmatch(stock_region_calc,"NORTH")
*replace area_s="GBS" if st2=="25" & strmatch(stock_region_calc,"SOUTH")

*gen str3 area_s="AAA"

*replace area_s="GOM" if st2=="23" | st2=="33"
/*
replace area_s="GOM" if st2=="25" & strmatch(stock_region_calc,"NORTH")
replace area_s="GBS" if st2=="25" & strmatch(stock_region_calc,"SOUTH")
*/

gen area_s="GOM" if st2=="25" & inlist(nmfs_stat_area,11, 512, 513,  514)
replace area_s="GBS" if st2=="25" & inlist(nmfs_stat_area,521, 526, 537,  538)
replace area_s="GOM" if st2=="25" & intsite==224


*gen my_dom_id_string=area_s+"_"+period +"_"+mode1+"_"+dom_id

*gen my_dom_id_string=area_s+"_"+month +"_"+dom_id
gen my_dom_id_string=yr2+"_"+wave2+"_"+mode1+"_"+area_x+"_"+state+"_"+common_dom


replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))
/*convert this string to a number */

/* total with over(<overvar>) requires a numeric variable */
encode my_dom_id_string, gen(my_dom_id)


replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)


svy: total dtrip , over(my_dom_id)  

svy: mean boat_hrs if area_s=="GOM" & dom_id=="1"


sort  my_dom_id  year strat_id psu_id id_code
local myvariables dtrip
local i=1
	* labelbook my_dom_id
	 *svy: total dtrip , over(my_dom_id)  
	 *xsvmat, from(r(table)') rownames(rname) names(col) norestore
	 *format b se ll ul %12.0gc
		
	
foreach var of local myvariables{
	 svy: total dtrip , over(my_dom_id)  

	mat b`i'=e(b)'
	mat colnames b`i'=`var'
	mat V=e(V)

	local ++i 
}

local --i
sort my_dom_id year 
dups my_dom_id, drop terse
*keep my_dom_id   period dom_id mode1  month  area_s
keep my_dom_id   dom_id mode1  month  area_s

foreach j of numlist 1/`i'{
	svmat b`j', names(col)
}
keep if strmatch(dom_id,"1")==1
format dtrip %10.0fc
*keep period dtrip  mode1   dom_id month my_dom_id area_s
keep dtrip   dom_id month my_dom_id area_s


destring period month , replace
sort mode period month 
*sort dtrip

*Drop shore mode (only 258/412,514 trip in FY 2020) 
drop if mode1=="sh"

keep if area_s=="GOM"


*Now input the 2020 regulations 
local vars cod_bag cod_min hadd_bag hadd_min
foreach v of local vars{
	gen `v'=.
}

*Cod regs
replace cod_bag= 1 if inlist(period, 18) & mode1=="pr"
replace cod_bag= 1 if inlist(period, 17, 18) & mode1=="fh"
replace cod_min= 21 if cod_bag!=.

*hadd regs
replace hadd_bag= 15 if !inlist(month, 3) 
replace hadd_min= 17 if hadd_bag!=.


drop dom_id my_dom area
tostring period, replace 
gen period2=period+"_"+mode1
order mode1 month period  period2

mvencode cod_bag hadd_bag, mv(0) overr
mvencode cod_min hadd_min, mv(100) overr


export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2020.csv", replace 


***************

***Directed trips and regulations for 2010-2019

cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"
local yearz 2010  2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
local statez 23 33 25

global yrz
foreach y of local yearz{
clear

global fluke_effort

*local yrs 2019
*foreach y of local yrs{

tempfile tl1 cl1
dsconcat $triplist

/* *dtrip will be used to estimate total directed trips, do not change it*/

gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
*drop strat_interval
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
*keep if strmatch(common, "summerflounder") | strmatch(common,"summerflounder")
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3)
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

drop _merge
 
*keep if inlist(year, 2021, 2020, 2019, 2018) 
keep if inlist(year, `y') 




/* THIS IS THE END OF THE DATA MERGING CODE */



tempfile new 
save `new', replace 

foreach s of local statez{
	u `new', clear 
	
 /* ensure only relevant states */
*keep if inlist(st,23, 33, 25)
keep if inlist(st,`s')


/*This is the "full" mrip data */
*tempfile tc1
*save `tc1'



 /* classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN). */
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "atlanticcod") 
replace dom_id="1" if strmatch(prim1_common, "atlanticcod") 

replace dom_id="1" if strmatch(common, "haddock") 
replace dom_id="1" if strmatch(prim1_common, "haddock") 



tostring wave, gen(w2)
tostring year, gen(year2)
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
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="fh" if inlist(mode_fx, "4", "5")


*gen mode1="shor" if inlist(mode_fx, "1", "2", "3") 
*replace mode1="priv" if inlist(mode_fx, "7") 
*replace mode1="chhd" if inlist(mode_fx, "4", "5") 

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

gen date2=mdy(month1,day1,year)
format date2 %td


*keep only fishin year 2020
*keep if date2>=td(01may2020) & date2<=td(30apr2021)



/*
destring day1, replace
sort year month1 day1
gen date2=mdy(month1,day1,year)
format date2 %td
*/
*gen season="open" if date2>=td(24may2019) & date2<=td(21sep2019)
*replace season="closed" if season==""


/*Deal with Group Catch -- this bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
Then it generates a flag for claim equal to the largest claim.  
Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1  */

replace claim=0 if claim==.

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (claim): gen claim_flag=claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")




/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n

keep if count_obs1==1


*keep if common_dom=="SF"
preserve 
import excel using "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-species-shift/ma site list updated SS.xlsx", clear first
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

keep if dom_id=="1"
keep if area_s=="GOM"

*gen my_dom_id_string=year2+"_"+area_s+"_"+period +"_"+mode1+"_"+dom_id
gen my_dom_id_string=mode1+"_"+period+"_"+area_x

*gen my_dom_id_string=area_s+"_"+month +"_"+dom_id


replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))
/*convert this string to a number */

/* total with over(<overvar>) requires a numeric variable */
encode my_dom_id_string, gen(my_dom_id)


replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)


*svy: total dtrip , over(my_dom_id)  
*svy: mean boat_hrs if area_s=="GOM" & dom_id=="1"

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

sort  my_dom_id  year strat_id psu_id id_code
svy: total dtrip , over(my_dom_id)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

/*
local myvariables dtrip
local i=1
	* labelbook my_dom_id
	 *svy: total dtrip , over(my_dom_id)  
	 *xsvmat, from(r(table)') rownames(rname) names(col) norestore
	 *format b se ll ul %12.0gc
		
	
foreach var of local myvariables{
	 svy: total `var' , over(my_dom_id)  

	mat b`i'=e(b)'
	mat colnames b`i'=`var'
	mat V=e(V)

	local ++i 
}

local --i
sort my_dom_id year 
dups my_dom_id, drop terse
*keep my_dom_id   period dom_id mode1  month  area_s
keep my_dom_id   dtrip

foreach j of numlist 1/`i'{
	svmat b`j', names(col)
}
*keep if strmatch(dom_id,"1")==1
format dtrip %10.0fc
*keep period dtrip  mode1   dom_id month my_dom_id area_s
keep my_dom_id   dtrip
*/

*destring period month area_x, replace
*sort  period month  mode area_x 
gen year=`y'
gen st=`s'

tempfile yrz`y'`s'
save `yrz`y'`s'', replace
global yrz "$yrz "`yrz`y'`s''" " 
}
}

dsconcat $yrz
keep my b year st
split my, parse(_)
rename my_dom_id_string1 mode
rename my_dom_id_string2 period
rename my_dom_id_string3 area_x
drop my
rename b dtrip
gen area = "inshore" if inlist(area_x, "1", "5")
replace area="offshore" if inlist(area_x, "2")

collapse (sum) dtrip, by(year st mode period area)
destring period, replace

gen month=1 if inlist(period, 1, 2) 
replace month=2 if inlist(period, 3, 4) & month==.
replace month=3 if inlist(period, 5, 6) & month==.
replace month=4 if inlist(period, 7, 8) & month==.
replace month=5 if inlist(period, 9, 10) & month==.
replace month=6 if inlist(period, 11, 12) & month==.
replace month=7 if inlist(period, 13, 14) & month==.
replace month=8 if inlist(period, 15, 16) & month==.
replace month=9 if inlist(period, 17, 18) & month==.
replace month=10 if inlist(period, 19, 20) & month==.
replace month=11 if inlist(period, 21, 22) & month==.
replace month=12 if inlist(period, 23, 24) & month==.
*sort dtrip


*Drop shore mode (only 258/412,514 trip in FY 2020) 
drop if mode=="sh"
sort year mode period area
rename mode mode1


save "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data\regs_base.dta", replace

u "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data\regs_base.dta", clear 

clear 
set obs 18
gen period=_n
replace period = period+5
gen mode1="fh"
expand 2, gen(dup)
replace mode1="pr" if dup==1
drop dup
gen area="inshore"
expand 2, gen(dup)
replace area="offshore" if dup==1
drop dup
gen st=23
expand 2, gen(dup)
replace st=33 if dup==1
drop dup
expand 2 if st==33, gen(dup)
replace st=25 if dup==1
drop dup

expand 11
bysort st period mode area: gen year=_n
replace year=2009+year
gen month=1 if inlist(period, 1, 2) 
replace month=2 if inlist(period, 3, 4) & month==.
replace month=3 if inlist(period, 5, 6) & month==.
replace month=4 if inlist(period, 7, 8) & month==.
replace month=5 if inlist(period, 9, 10) & month==.
replace month=6 if inlist(period, 11, 12) & month==.
replace month=7 if inlist(period, 13, 14) & month==.
replace month=8 if inlist(period, 15, 16) & month==.
replace month=9 if inlist(period, 17, 18) & month==.
replace month=10 if inlist(period, 19, 20) & month==.
replace month=11 if inlist(period, 21, 22) & month==.
replace month=12 if inlist(period, 23, 24) & month==.

merge 1:1 period mode1 area year st month using "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data\regs_base.dta"
drop _merge

mvencode dtrip, mv(0) override
*Now input the regulations 
local vars cod_bag cod_min hadd_bag hadd_min
foreach v of local vars{
	gen `v'=.
}

***Cod regulations
*2010
replace cod_bag= 10 if year==2010 & period>=8 & period<=20
replace cod_min= 24 if year==2010 & period>=8 & period<=20

*2011
replace cod_bag= 10 if year==2011 & period>=8 & period<=20
replace cod_min= 24 if year==2011 & period>=8 & period<=20

*2012
replace cod_bag= 10 if year==2012 & period==8 
replace cod_min= 24 if year==2012 & period==8 

replace cod_bag= 9 if year==2012 & period>=9 & period<=20
replace cod_min= 19 if year==2012 & period>=9 & period<=20

*2013
replace cod_bag= 9 if year==2013 & period==8 
replace cod_min= 19 if year==2013 & period==8 

replace cod_bag= 9 if year==2013 & period>=9 & period<=20
replace cod_min= 19 if year==2013 & period>=9 & period<=20

*2014
replace cod_bag= 9 if year==2014 & period==8 
replace cod_min= 19 if year==2014 & period==8 

replace cod_bag= 9 if year==2014 & period>=9 & period<=16
replace cod_min= 21 if year==2014 & period>=9 & period<=16

*2015 cod closed, but I adjust to allow harvest due to state regs as per Scott: "2015 was the first closure year and it took time for the states to get on board. The fishery was closed in federal waters, but the states of ME, NH, and MA allowed harvest as long as it occurred in state waters. MA had a bag=1, min size = 19" that year. Thus, MA anglers generally just followed the state regs no matter where they caught the fish because enforcement generally occurs on shore. ME did close their state fishery as well in 2015, but not until Aug. 8 that year. NH was the big culprit where they kept the 9 fish bag limit that year."

*original based on aggregated data 		
*replace cod_bag= 1 if year==2015 & period>=8 & period<=16
*replace cod_min= 19 if year==2015 & period>=8  & period<=16

*new based on disaggregated data 
replace cod_bag= 1 if year==2015 & period>=8 & period<=16 & inlist(st, 25)
replace cod_min= 19 if year==2015 & period>=8  & period<=16 & inlist(st, 25)

replace cod_bag= 9 if year==2015 & period>=9 & period<=16 & st==33
replace cod_min= 21 if year==2015 & period>=9 & period<=16 & st==33

replace cod_bag= 1 if year==2015 & period>=8 & period<=15 & inlist(st, 23)
replace cod_min= 19 if year==2015 & period>=8  & period<=15 & inlist(st, 23)


*2016
replace cod_bag= 1 if year==2016 & period>=15 & period<=18
replace cod_min= 24 if year==2016 & period>=15 & period<=18

*2017 cod closed, 
*adjust for MA state regualtions. Federal closure was not implemtned until August 8th so 2016 regs stayed inplace until then. 2016 regs for MA were 1 fish 19" year round for private and 1  fish 24" 8/1/16 to 9/30/16 for for hire. So set bag limit 1 fish 19" until August 8th in 2017. 

replace cod_bag= 1 if year==2017 & period<=15 & mode1=="pr"
replace cod_min= 19 if year==2017 & period<=15 & mode1=="pr"

*2018 cod closed 

*2019 
replace cod_bag= 1 if year==2019 & inlist(period, 18)
replace cod_min= 21 if year==2019 & inlist(period, 18)


*2020 
replace cod_bag= 1 if year==2020 & inlist(period, 18) & mode1=="pr"
replace cod_min= 21  if year==2020 & inlist(period, 18) & mode1=="pr"

replace cod_bag= 1 if year==2020 & inlist(period, 17, 18) & mode1=="fh"
replace cod_min= 21  if year==2020 &  inlist(period, 17, 18) & mode1=="fh"

***Haddock regulations
*2010
replace hadd_bag= 200 if year==2010 
replace hadd_min= 18 if year==2010 
 
*2011
replace hadd_bag= 200 if year==2011 
replace hadd_min= 18 if year==2011 & period>=9 

*2012 
replace hadd_bag= 9 if year==2012   & period<=7
replace hadd_min= 19 if year==2012   & period<=7

replace hadd_bag= 200 if year==2012   & period>7
replace hadd_min= 18 if year==2012   & period>7

*2013 
replace hadd_bag= 200 if year==2013   
replace hadd_min= 21 if year==2013 

*2014 
replace hadd_bag= 3 if year==2014 & period>=9 & period<=16  
replace hadd_min= 21 if year==2014 & period>=9 & period<=16  

replace hadd_bag= 3 if year==2014 & period >=21
replace hadd_min= 21 if year==2014 & period>=21

*2015 
replace hadd_bag= 3 if year==2015 & period <=4
replace hadd_min= 21 if year==2015 & period<=4

replace hadd_bag= 3 if year==2015 & period>=9 & period<=16  
replace hadd_min= 17 if year==2015 & period>=9 & period<=16  

replace hadd_bag= 3 if year==2015 & period >=21
replace hadd_min= 21 if year==2015 & period>=21

*2016 
replace hadd_bag= 3 if year==2016 & period <=4
replace hadd_min= 21 if year==2016 & period <=4

replace hadd_bag= 15 if year==2016 & period>=9 
replace hadd_min= 17 if year==2016 & period>=9 

*2017 
replace hadd_bag= 15 if year==2017 & period>=4 
replace hadd_min= 17 if year==2017 & period>=4 

replace hadd_bag= 15 if year==2017 & period==8 
replace hadd_min= 17 if year==2017 & period==8

replace hadd_bag= 15 if year==2017 & period>=9 & period<=14
replace hadd_min= 17 if year==2017 & period>=9 & period<=14

replace hadd_bag= 12 if year==2017 & period>14 & period<=17
replace hadd_min= 17 if year==2017 & period>14 & period<=17 

replace hadd_bag= 12 if year==2017 & period>=19
replace hadd_min= 17 if year==2017 & period>=19

*2018 
replace hadd_bag= 12 if year==2018 & period<=4
replace hadd_min= 17 if year==2018 & period<=4

replace hadd_bag= 12 if year==2018 & period==8
replace hadd_min= 17 if year==2018 & period==8

replace hadd_bag= 12 if year==2018 & period>=9 & period<=17
replace hadd_min= 17 if year==2018 & period>=9 & period<=17

replace hadd_bag= 12 if year==2018 & period>=21
replace hadd_min= 17 if year==2018 & period>=21

*2019 
replace hadd_bag= 12 if year==2019 & period<=4
replace hadd_min= 17 if year==2019 & period<=4

replace hadd_bag= 12 if year==2019 & period==8
replace hadd_min= 17 if year==2019 & period==8

replace hadd_bag= 15 if year==2019 & period>=9
replace hadd_min= 17 if year==2019 & period>=9

*2020 
replace hadd_bag= 15 if year==2020 & period<=4
replace hadd_min= 17 if year==2020 & period<=4

replace hadd_bag= 15 if year==2020 & period==8
replace hadd_min= 17 if year==2020 & period==8

replace hadd_bag= 15 if year==2020 & period>=9
replace hadd_min= 17 if year==2020 & period>=9


replace cod_bag=0 if cod_bag==.
replace cod_min=100 if cod_min==.

replace hadd_bag=0 if hadd_bag==.
replace hadd_min=100 if hadd_min==.


***********
mvencode cod_bag hadd_bag, mv(0) overr
mvencode cod_min hadd_min, mv(100) overr


tostring period, gen(pd)
tostring st, gen(st2)
gen domain=pd+mode1+area+st2
drop pd st2 domain
save "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data\regs_base2.dta", replace


u "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data\regs_base2.dta", clear 

tempfile new
save `new', replace 

global yrz

levelsof year if year!=2020, local(yearz)
foreach y of local yearz{
local y_plus1=`y'+1

u `new', clear 
preserve 
keep if year==`y'
tempfile baseyear
save `baseyear', replace
restore

keep if year==`y_plus1'
keep period mode1 area st  month cod_bag cod_min hadd_bag hadd_min
ds period mode1 area st  month, not
renvarlab `r(varlist)', postfix(_t_plus1)
merge 1:1 period mode1 area st  month using `baseyear'

tempfile yrz`y'
save `yrz`y'', replace
global yrz "$yrz "`yrz`y''" " 

}
dsconcat $yrz

tempfile new1
save `new1', replace 

*Now add the regs for 2020 
u "C:\Users\andrew.carr-harris\Desktop\GoM correlated catch data\regs_base2.dta", clear 
keep if year==2020
append using `new1'

sort year mode area st period
order year month period mode1 area st    mode1 dtrip cod_bag cod_min hadd_bag hadd_min cod_bag_t_plus1 cod_min_t_plus1 hadd_bag_t_plus1 hadd_min_t_plus1
drop _merge


rename mode1 mode
gen mode1=1 if mode=="fh"
gen mode2=1 if mode=="pr" 
mvencode mode1 mode2, mv(0)
tostring period, gen(period3)
gen period2 =mode+"_"+period3+"_"+area
drop period3

export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2010_2020_disaggregated.csv", replace 


//import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2010_2020.csv", clear 

import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2010_2020_disaggregated.csv", clear 




export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2010_2020_disaggregated.csv", replace 





import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2010_2020.csv", clear 
keep if year==2010
drop if dtrip==0
egen sum_trip=sum(dtrip)
gen n_sample=round((dtrip/sum_trip)*5000)
replace n_sample=1 if n_sample==0

tostring n_sample, replace
gen string1=`"""'+period2+`"""'+"="+n_sample+","
keep string1


import delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips and regulations 2010_2020.csv", clear 
keep if year==2020
drop if dtrip==0
egen sum_trip=sum(dtrip), by(month)
gen n_sample=round((dtrip/sum_trip)*5000)
replace n_sample=1 if n_sample==0

tostring n_sample, replace
gen string1=`"""'+period2+`"""'+"="+n_sample+","
keep string1




