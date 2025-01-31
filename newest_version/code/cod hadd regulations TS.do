
***************************************************************

* Create time series of haddock fishing regulations

* Clear existing data
clear

* Set the date range to cover all periods in the document
local start_date = date("2010-01-01", "YMD")
local end_date = date("2025-04-30", "YMD")

* Create a new dataset with one row per day
local obs = `end_date' - `start_date' + 1
set obs `obs'
generate date = `start_date' + _n - 1
format date %td

* Create variables for regulations
generate season_open = 0
generate for_hire_boat_size_limit = .
generate private_boat_size_limit = .
generate for_hire_boat_bag_limit = .
generate private_boat_bag_limit = .

* Macro to replace values for a specific date range
capture program drop replace_regs
program replace_regs
    syntax, start(string) end(string) ///
            for_hire_size(real) private_size(real) ///
            [for_hire_bag(string) private_bag(string)]
    
    * Convert dates
    local start_date = date("`start'", "YMD")
    local end_date = date("`end'", "YMD")
    
    * Replace season open
    replace season_open = 1 if inrange(date, `start_date', `end_date')
    
    * Replace size limits
    replace for_hire_boat_size_limit = `for_hire_size' if inrange(date, `start_date', `end_date')
    replace private_boat_size_limit = `private_size' if inrange(date, `start_date', `end_date')
    
    * Replace bag limits (handling 'no limit' case)
    if "`for_hire_bag'" != "" & "`for_hire_bag'" != "no limit" {
        replace for_hire_boat_bag_limit = `for_hire_bag' if inrange(date, `start_date', `end_date')
    }
    else if "`for_hire_bag'" == "no limit" {
        replace for_hire_boat_bag_limit = . if inrange(date, `start_date', `end_date')
    }
    
    if "`private_bag'" != "" & "`private_bag'" != "no limit" {
        replace private_boat_bag_limit = `private_bag' if inrange(date, `start_date', `end_date')
    }
    else if "`private_bag'" == "no limit" {
        replace private_boat_bag_limit = . if inrange(date, `start_date', `end_date')
    }
end

* Apply regulations for each period
* 2010-2011
replace_regs, start(2010-01-01) end(2010-04-30) for_hire_size(18) private_size(18) ///
    for_hire_bag("no limit") private_bag("no limit")
	
replace_regs, start(2010-05-01) end(2011-04-30) for_hire_size(18) private_size(18) ///
    for_hire_bag("no limit") private_bag("no limit")

* 2011 period 1
replace_regs, start(2011-05-01) end(2012-01-05) for_hire_size(18) private_size(18) ///
    for_hire_bag("no limit") private_bag("no limit")

* 2011 period 2
replace_regs, start(2012-01-06) end(2012-04-19) for_hire_size(19) private_size(19) ///
    for_hire_bag(9) private_bag(9)

* 2011 period 3
replace_regs, start(2012-04-20) end(2012-04-30) for_hire_size(18) private_size(18) ///
    for_hire_bag("no limit") private_bag("no limit")

* Continue with similar detailed logic for subsequent years
* 2012
replace_regs, start(2012-05-01) end(2013-04-30) for_hire_size(18) private_size(18) ///
    for_hire_bag("no limit") private_bag("no limit")

* 2013
replace_regs, start(2013-05-01) end(2014-04-30) for_hire_size(21) private_size(21) ///
    for_hire_bag("no limit") private_bag("no limit")

* 2014
replace_regs, start(2014-05-01) end(2014-08-31) for_hire_size(21) private_size(21) ///
    for_hire_bag(3) private_bag(3)
replace_regs, start(2014-11-01) end(2015-02-28) for_hire_size(21) private_size(21) ///
    for_hire_bag(3) private_bag(3)

* 2015
replace_regs, start(2015-05-01) end(2015-08-31) for_hire_size(17) private_size(17) ///
    for_hire_bag(3) private_bag(3)
replace_regs, start(2015-11-01) end(2016-02-29) for_hire_size(17) private_size(17) ///
    for_hire_bag(3) private_bag(3)

* 2016
replace_regs, start(2016-05-01) end(2017-02-28) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)
replace_regs, start(2017-04-15) end(2017-04-30) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)

* 2017 period 1
replace_regs, start(2017-05-01) end(2017-07-26) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)

* 2017 period 2
replace_regs, start(2017-07-27) end(2017-09-16) for_hire_size(17) private_size(17) ///
    for_hire_bag(12) private_bag(12)
replace_regs, start(2017-11-01) end(2018-02-28) for_hire_size(17) private_size(17) ///
    for_hire_bag(12) private_bag(12)
replace_regs, start(2018-04-15) end(2018-04-30) for_hire_size(17) private_size(17) ///
    for_hire_bag(12) private_bag(12)

* 2018
replace_regs, start(2018-05-01) end(2018-09-16) for_hire_size(17) private_size(17) ///
    for_hire_bag(12) private_bag(12)
replace_regs, start(2018-11-01) end(2019-02-28) for_hire_size(17) private_size(17) ///
    for_hire_bag(12) private_bag(12)
replace_regs, start(2019-04-15) end(2019-07-04) for_hire_size(17) private_size(17) ///
    for_hire_bag(12) private_bag(12)

* 2019
replace_regs, start(2019-07-05) end(2020-02-29) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)
replace_regs, start(2020-04-15) end(2020-04-30) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)

* 2020-2021
replace_regs, start(2020-05-01) end(2021-02-28) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)
replace_regs, start(2021-04-01) end(2021-04-30) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)
replace_regs, start(2021-05-01) end(2022-02-28) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)
replace_regs, start(2022-04-01) end(2022-08-29) for_hire_size(17) private_size(17) ///
    for_hire_bag(15) private_bag(15)

* 2022
replace_regs, start(2022-08-30) end(2023-02-28) for_hire_size(17) private_size(17) ///
    for_hire_bag(20) private_bag(20)
replace_regs, start(2023-04-01) end(2023-08-13) for_hire_size(17) private_size(17) ///
    for_hire_bag(20) private_bag(20)

* 2023
replace_regs, start(2023-08-14) end(2024-02-28) for_hire_size(18) private_size(17) ///
    for_hire_bag(15) private_bag(10)
replace_regs, start(2024-04-01) end(2024-04-30) for_hire_size(18) private_size(17) ///
    for_hire_bag(15) private_bag(10)

* 2024
replace_regs, start(2024-05-01) end(2025-02-28) for_hire_size(18) private_size(18) ///
    for_hire_bag(15) private_bag(15)
replace_regs, start(2025-04-01) end(2025-04-30) for_hire_size(18) private_size(18) ///
    for_hire_bag(15) private_bag(15)

* Label variables for clarity
label variable date "Calendar Date"
label variable season_open "Fishing Season Open (1=Yes, 0=No)"
label variable for_hire_boat_size_limit "For-hire Boat Size Limit (inches)"
label variable private_boat_size_limit "Private Boat Size Limit (inches)"
label variable for_hire_boat_bag_limit "For-hire Boat Bag Limit"
label variable private_boat_bag_limit "Private Boat Bag Limit"

*keep if season_open==1

* Sort by date
sort date

* Compute how many days were open in each period
gen month=month(date)
gen day=day(date)
gen year=year(date)

gen period ="1" if day<=15 & month==1
replace period ="2" if day>15 & month==1
replace period ="3" if day<=15 & month==2
replace period ="4" if day>15 & month==2
replace period ="5" if day<=15 & month==3
replace period ="6" if day>15 & month==3
replace period ="7" if day<=15 & month==4
replace period ="8" if day>15 & month==4
replace period ="9" if day<=15 & month==5
replace period ="10" if day>15 & month==5
replace period ="11" if day<=15 & month==6
replace period ="12" if day>15 & month==6
replace period ="13" if day<=15 & month==7
replace period ="14" if day>15 & month==7
replace period ="15" if day<=15 & month==8
replace period ="16" if day>15 & month==8
replace period ="17" if day<=15 & month==9
replace period ="18" if day>15 & month==9
replace period ="19" if day<=15 & month==10
replace period ="20" if day>15 & month==10
replace period ="21" if day<=15 & month==11
replace period ="22" if day>15 & month==11
replace period ="23" if day<=15 & month==12
replace period ="24" if day>15 & month==12

replace for_hire_boat_size_limit=100 if season_open!=1
replace private_boat_size_limit=100 if season_open!=1
replace for_hire_boat_bag_limit=0 if season_open!=1
replace private_boat_bag_limit=0 if season_open!=1

replace for_hire_boat_bag_limit=100 if season_open==1 & private_boat_bag_limit==.
replace private_boat_bag_limit=100 if season_open==1 & private_boat_bag_limit==.
 
preserve 
drop for_hire_boat_size_limit for_hire_boat_bag_limit
rename private_boat_size_limit min_size
rename private_boat_bag_limit bag_limit
gen mode="pr"
tempfile pr
save `pr', replace
restore

drop private_boat_size_limit private_boat_bag_limit
rename for_hire_boat_size_limit min_size
rename for_hire_boat_bag_limit bag_limit
gen mode="fh"
append using `pr'

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

preserve 
gen ndays=1
tostring min_size, gen(min2)
tostring bag_limit, gen(bag2)
gen min_bag=min2+"_"+bag2
egen sum_ndays=sum(ndays), by(year period mode area st )
collapse (sum) season_open (mean) sum_ndays=sum_ndays, by(year period mode area st min_size bag_limit)
gen perc_open=season_open/sum_ndays
keep if perc_open>=.5
keep year period mode area st  perc_open min_size bag_limit
tempfile open
save `open', replace 
restore

merge m:1 year period mode area st min_size bag_limit using `open'
destring period, replace 

keep year period mode area st perc_open min_size bag_limit _merge
duplicates drop 
destring period, replace 
sort year period mode area st
drop if inlist(period, 1, 2, 3, 4)
drop if perc_open<.5
drop if _merge==1
duplicates report year period mode area st, gen(dup)
drop _merge
drop perc

rename min hadd_min
rename bag hadd_bag


* Temp save data
tempfile dates_hadd
save `dates_hadd', replace 



***************************************************************


* Create time series of cod fishing regulations

* Clear existing data
clear 

* Set the date range to cover all periods in the document
local start_date = date("2010-01-01", "YMD")
local end_date = date("2025-04-30", "YMD")

* Create a new dataset with one row per day
local obs = `end_date' - `start_date' + 1
set obs `obs'
generate date = `start_date' + _n - 1
format date %td

* Create variables for regulations
generate season_open = 0
generate for_hire_boat_size_limit = .
generate private_boat_size_limit = .
generate for_hire_boat_bag_limit = .
generate private_boat_bag_limit = .

* Macro to replace values for a specific date range
capture program drop replace_regs
program replace_regs
    syntax, start(string) end(string) ///
            for_hire_size(real) private_size(real) ///
            [for_hire_bag(string) private_bag(string)]
    
    * Convert dates
    local start_date = date("`start'", "YMD")
    local end_date = date("`end'", "YMD")
    
    * Replace season open
    replace season_open = 1 if inrange(date, `start_date', `end_date')
    
    * Replace size limits
    replace for_hire_boat_size_limit = `for_hire_size' if inrange(date, `start_date', `end_date')
    replace private_boat_size_limit = `private_size' if inrange(date, `start_date', `end_date')
    
    * Replace bag limits (handling 'no limit' case)
    if "`for_hire_bag'" != "" & "`for_hire_bag'" != "no limit" {
        replace for_hire_boat_bag_limit = `for_hire_bag' if inrange(date, `start_date', `end_date')
    }
    else if "`for_hire_bag'" == "no limit" {
        replace for_hire_boat_bag_limit = . if inrange(date, `start_date', `end_date')
    }
    
    if "`private_bag'" != "" & "`private_bag'" != "no limit" {
        replace private_boat_bag_limit = `private_bag' if inrange(date, `start_date', `end_date')
    }
    else if "`private_bag'" == "no limit" {
        replace private_boat_bag_limit = . if inrange(date, `start_date', `end_date')
    }
end

* Apply regulations for each period
* 2010
replace_regs, start(2010-05-01) end(2010-10-31) for_hire_size(24) private_size(24) ///
    for_hire_bag(10) private_bag(10)
	
* 2011 period 1
replace_regs, start(2011-04-16) end(2011-04-30) for_hire_size(24) private_size(24) ///
    for_hire_bag(10) private_bag(10)

* 2011 period 2
replace_regs, start(2011-05-01) end(2011-10-31) for_hire_size(24) private_size(24) ///
    for_hire_bag(10) private_bag(10)

* 2012 period 1
replace_regs, start(2012-04-16) end(2012-04-30) for_hire_size(24) private_size(24) ///
    for_hire_bag(10) private_bag(10)

* Continue with similar detailed logic for subsequent years
* 2012 period 2
replace_regs, start(2012-05-01) end(2012-10-31) for_hire_size(19) private_size(19) ///
    for_hire_bag(9) private_bag(9)

* 2013 period 1
replace_regs, start(2013-04-16) end(2013-04-30) for_hire_size(19) private_size(19) ///
    for_hire_bag(9) private_bag(9)

* 2013 period 2
replace_regs, start(2013-05-01) end(2013-10-31) for_hire_size(19) private_size(19) ///
    for_hire_bag(9) private_bag(9)

* 2014 period 1
replace_regs, start(2014-04-16) end(2014-04-30) for_hire_size(19) private_size(19) ///
    for_hire_bag(9) private_bag(9)	
	
* 2014 period 2
replace_regs, start(2014-05-01) end(2014-08-31) for_hire_size(21) private_size(21) ///
    for_hire_bag(9) private_bag(9)
	
/* 2015 period 1
replace_regs, start(2014-04-16) end(2014-04-30) for_hire_size(19) private_size(19) ///
    for_hire_bag(9) private_bag(9)	
		**need to add different regs by state below
*/
	
* 2016 
replace_regs, start(2016-08-01) end(2016-09-30) for_hire_size(24) private_size(24) ///
    for_hire_bag(1) private_bag(1)	

* 2017 
replace_regs, start(2017-08-01) end(2017-09-30) for_hire_size(24) private_size(24) ///
    for_hire_bag(1) private_bag(1)		

*2018 closed 

* 2019	
replace_regs, start(2019-09-15) end(2019-09-30) for_hire_size(21) private_size(21) ///
    for_hire_bag(1) private_bag(1)		

* 2020 
**need to add different regs by mode below
	
* 2021
**need to add different regs by mode below

*2022 
replace_regs, start(2022-04-01) end(2022-04-14) for_hire_size(21) private_size(21) ///
    for_hire_bag(1) private_bag(1)


* Label variables for clarity
label variable date "Calendar Date"
label variable season_open "Fishing Season Open (1=Yes, 0=No)"
label variable for_hire_boat_size_limit "For-hire Boat Size Limit (inches)"
label variable private_boat_size_limit "Private Boat Size Limit (inches)"
label variable for_hire_boat_bag_limit "For-hire Boat Bag Limit"
label variable private_boat_bag_limit "Private Boat Bag Limit"

*keep if season_open==1

* Sort by date
sort date


* Compute how many days were open in each period
gen month=month(date)
gen day=day(date)
gen year=year(date)

gen period ="1" if day<=15 & month==1
replace period ="2" if day>15 & month==1
replace period ="3" if day<=15 & month==2
replace period ="4" if day>15 & month==2
replace period ="5" if day<=15 & month==3
replace period ="6" if day>15 & month==3
replace period ="7" if day<=15 & month==4
replace period ="8" if day>15 & month==4
replace period ="9" if day<=15 & month==5
replace period ="10" if day>15 & month==5
replace period ="11" if day<=15 & month==6
replace period ="12" if day>15 & month==6
replace period ="13" if day<=15 & month==7
replace period ="14" if day>15 & month==7
replace period ="15" if day<=15 & month==8
replace period ="16" if day>15 & month==8
replace period ="17" if day<=15 & month==9
replace period ="18" if day>15 & month==9
replace period ="19" if day<=15 & month==10
replace period ="20" if day>15 & month==10
replace period ="21" if day<=15 & month==11
replace period ="22" if day>15 & month==11
replace period ="23" if day<=15 & month==12
replace period ="24" if day>15 & month==12

replace for_hire_boat_size_limit=100 if season_open!=1
replace private_boat_size_limit=100 if season_open!=1
replace for_hire_boat_bag_limit=0 if season_open!=1
replace private_boat_bag_limit=0 if season_open!=1

replace for_hire_boat_bag_limit=100 if season_open==1 & private_boat_bag_limit==.
replace private_boat_bag_limit=100 if season_open==1 & private_boat_bag_limit==.
 
preserve 
drop for_hire_boat_size_limit for_hire_boat_bag_limit
rename private_boat_size_limit min_size
rename private_boat_bag_limit bag_limit
gen mode="pr"
tempfile pr
save `pr', replace
restore

drop private_boat_size_limit private_boat_bag_limit
rename for_hire_boat_size_limit min_size
rename for_hire_boat_bag_limit bag_limit
gen mode="fh"
append using `pr'

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
duplicates drop

** Add additional regulations 
destring period, replace 

preserve 
gen ndays=1
tostring min_size, gen(min2)
tostring bag_limit, gen(bag2)
gen min_bag=min2+"_"+bag2
egen sum_ndays=sum(ndays), by(year period mode area st )
collapse (sum) season_open (mean) sum_ndays=sum_ndays, by(year period mode area st min_size bag_limit)
gen perc_open=season_open/sum_ndays
keep if perc_open>=.5
keep year period mode area st  perc_open min_size bag_limit
tempfile open
save `open', replace 
restore

merge m:1 year period mode area st min_size bag_limit using `open'

keep year period mode area st perc_open min_size bag_limit _merge
duplicates drop 
destring period, replace 
sort year period mode area st
drop if inlist(period, 1, 2, 3, 4)
drop if perc_open<.5
drop if _merge==1
duplicates report year period mode area st, gen(dup)
drop _merge



rename min cod_min
rename bag cod_bag

drop perc

* Temp save data
merge 1:1 year period mode area st using `dates_hadd'
sort year period mode area st
drop if year>2021

drop _merge 


*additional cod regulations 
* *2015 cod closed, but I adjust to allow harvest due to state regs as per Scott: "2015 was the first closure year and it took time for the states to get on board. The fishery was closed in federal waters, but the states of ME, NH, and MA allowed harvest as long as it occurred in state waters. MA had a bag=1, min size = 19" that year. Thus, MA anglers generally just followed the state regs no matter where they caught the fish because enforcement generally occurs on shore. ME did close their state fishery as well in 2015, but not until Aug. 8 that year. NH was the big culprit where they kept the 9 fish bag limit that year."

*new based on disaggregated data 
replace cod_bag= 1 if year==2015 & period>=8 & period<=16 & inlist(st, 25)
replace cod_min= 19 if year==2015 & period>=8  & period<=16 & inlist(st, 25)

replace cod_bag= 9 if year==2015 & period>=9 & period<=16 & st==33
replace cod_min= 21 if year==2015 & period>=9 & period<=16 & st==33

replace cod_bag= 1 if year==2015 & period>=8 & period<=15 & inlist(st, 23)
replace cod_min= 19 if year==2015 & period>=8  & period<=15 & inlist(st, 23)
*2020 
replace cod_bag= 1 if year==2020 & inlist(period, 18) & mode=="pr"
replace cod_min= 21  if year==2020 & inlist(period, 18) & mode=="pr"

replace cod_bag= 1 if year==2020 & inlist(period, 17, 18) & mode=="fh"
replace cod_min= 21  if year==2020 &  inlist(period, 17, 18) & mode=="fh"



*2021 
replace cod_bag= 1 if year==2021 & inlist(period, 7) & mode=="pr"
replace cod_min= 21  if year==2021 & inlist(period, 7) & mode=="pr"

replace cod_bag= 1 if year==2021 & inlist(period, 7) & mode=="fh"
replace cod_min= 21  if year==2021 &  inlist(period, 7) & mode=="fh"

replace cod_bag= 1 if year==2021 & inlist(period, 18) & mode=="pr"
replace cod_min= 21  if year==2021 & inlist(period, 18) & mode=="pr"

replace cod_bag= 1 if year==2021 & inlist(period, 17, 18) & mode=="fh"
replace cod_min= 21  if year==2021 &  inlist(period, 17, 18) & mode=="fh"


order year period mode area st 
sort  year period mode area st 
rename mode mode1

tostring st, gen(st2)



merge 1:1  year period mode1 area st using  "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\input_data\regs_base.dta", keep(2 3)
browse if _merge==2

mvencode cod_bag hadd_bag, mv(0) 
mvencode hadd_min cod_min, mv(100) 

sort year period mode1 area st
drop _merge