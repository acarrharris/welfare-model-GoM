

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

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37

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


gen my_dom_id_string=area_s+"_"+period +"_"+mode1+"_"+dom_id


replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))
/*convert this string to a number */

/* total with over(<overvar>) requires a numeric variable */
encode my_dom_id_string, gen(my_dom_id)


replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

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
keep my_dom_id   period dom_id mode1  month  area_s

foreach j of numlist 1/`i'{
	svmat b`j', names(col)
}
keep if strmatch(dom_id,"1")==1
format dtrip %10.0fc
keep period dtrip  mode1   dom_id month my_dom_id area_s


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






