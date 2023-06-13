/* This is a file that produces a dataset that contains #of fish encountered per trip.
This is a port of Scott's "cod_domain_length_freqs_by_wave_gom_2013.sas"

This is a template program for estimating length frequencies
using the MRIP public-use datasets.

The program is setup to use information in the trip_yyyyw
dataset to define custom domains.  The length frequencies are
estimated within the domains by merging the trip information onto
the size_yyyyw datasets.

Required input datasets:
 trip_yyyyw
 size_yyyyw


It looks like we also need to port cod_domain_length_freqs_b2_by_wave_gom_2013 as well 

There will be one output per variable and year in working directory:
"$my_common`myv'_a1b1$myyear.dta"



*/


clear
mata: mata clear
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

global fluke_sizeab1


clear

tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1'
clear

 
dsconcat $sizelist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if inlist(year, 2019, 2020)
/* THIS IS THE END OF THE DATA MERGING CODE */


 /* ensure only relevant states */
keep if inlist(st,23, 33, 25)
gen st2 = string(st,"%02.0f")


/*This is the "full" mrip data */
tempfile tc1
save `tc1'

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37


tempfile base
save `base', replace 

global ab1

local myspecies "atlanticcod haddock"
foreach s of local myspecies{
	
u `base', clear 

 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" */
 gen common_dom="zzzzzz"
 replace common_dom="SF" if strmatch(common, "`s'") 
 
tostring wave, gen(w2)
tostring year, gen(year2)

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

gen my_dom_id_string=common_dom+"_"+area_s


replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)

/* l_in_bin already defined
gen l_in_bin=floor(lngth*0.03937) */

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace l_in_bin=0 if strmatch(common, "`s'")==0


sort year2  w2 strat_id psu_id id_code common_dom
svyset psu_id [pweight= wp_size], strata(strat_id) singleunit(certainty)
keep if my_dom_id_string=="SF_GOM"

	svy: tab l_in_bin my_dom_id_string, count se ci format(%12.0gc)
	/*save some stuff  
	matrix of proportions, row names, column names, estimate of total population size*/
	mat eP=e(Prop)
	mat eR=e(Row)'
	mat eC=e(Col)
	local PopN=e(N_pop)

	local mycolnames: colnames(eC)
	mat colnames eP=`mycolnames'
	
	clear
	/*read the eP into a dataset and convert proportion of population into numbers*/
	svmat eP
	foreach var of varlist *{
		replace `var'=`var'*`PopN'
	}

/*read in the "row" */
svmat eR
order eR
rename eR l_in_bin

keep eP1 l_in	
rename eP1 ab1
egen sum=sum(ab1)
gen prop_harvest=ab1/sum
drop sum

drop ab1
gen species="`s'"

tempfile ab1`s'
save `ab1`s'', replace
global ab1 "$ab1 "`ab1`s''" " 

}	

clear
dsconcat $ab1

tempfile ab1
save `ab1', replace




*Now repeat process for b2 - here we keep the raw numbers at length, which we will then combine with VAS data
cd "C:\Users\andrew.carr-harris\Desktop\MRIP_data"

clear

mata: mata clear


tempfile tl1 sl1
dsconcat $triplist

sort year strat_id psu_id id_code
save `tl1', replace
clear

 

dsconcat $b2list
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `sl1', replace

use `tl1'
merge 1:m year strat_id psu_id id_code using `sl1', keep(1 3) nogen

keep if inlist(year, 2019, 2020)
/* THIS IS THE END OF THE DATA MERGING CODE */


 

 /* ensure only relevant states */
keep if inlist(st,23, 33, 25)
gen st2 = string(st,"%02.0f")


/*This is the "full" mrip data */
tempfile tc1
save `tc1'

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37



tempfile base
save `base', replace 

global b2

local myspecies "atlanticcod haddock"
foreach s of local myspecies{
	
u `base', clear 

 /* classify catch into the things I care about (common==$mycommon) and things I don't care about "ZZZZZZZZ" */
 gen common_dom="zzzzzz"
 replace common_dom="SF" if strmatch(common, "`s'") 
 
tostring wave, gen(w2)
tostring year, gen(year2)

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

gen my_dom_id_string=common_dom+"_"+area_s


replace my_dom_id_string=subinstr(ltrim(rtrim(my_dom_id_string))," ","",.)

/* l_in_bin already defined
gen l_in_bin=floor(lngth*0.03937) */

/* this might speed things up if I re-classify all length=0 for the species I don't care about */
replace l_in_bin=0 if strmatch(common, "`s'")==0


sort year2  w2 strat_id psu_id id_code common_dom
svyset psu_id [pweight= wp_size], strata(strat_id) singleunit(certainty)
keep if my_dom_id_string=="SF_GOM"

	svy: tab l_in_bin my_dom_id_string, count se ci format(%12.0gc)
	/*save some stuff  
	matrix of proportions, row names, column names, estimate of total population size*/
	mat eP=e(Prop)
	mat eR=e(Row)'
	mat eC=e(Col)
	local PopN=e(N_pop)

	local mycolnames: colnames(eC)
	mat colnames eP=`mycolnames'
	
	clear
	/*read the eP into a dataset and convert proportion of population into numbers*/
	svmat eP
	foreach var of varlist *{
		replace `var'=`var'*`PopN'
	}

/*read in the "row" */
svmat eR
order eR
rename eR l_in_bin

keep eP1 l_in	
rename eP1 b2
egen sum=sum(b2)
gen prop_released=b2/sum
drop sum

drop b2
gen species="`s'"

tempfile b2`s'
save `b2`s'', replace
global b2 "$b2 "`b2`s''" " 

}	

clear
dsconcat $b2

merge 1:1 species l_in using `ab1'


*Now need total numbers of cod and haddock harvested and released in 2019/20
/*
common_dom	area_s	my_dom_id	cod_tot_cat	cod_releases	cod_harvest	hadd_tot_cat	hadd_releases	hadd_harvest
ATLCO	GOM	ATLCO_GOM			1044331			1022739		21592.43		3409146		1509471			1899676
*/
mvencode prop*, mv(0) override

drop _merge 
gen cod_harvest=21592.43 if species=="atlanticcod"
gen cod_releases=1022739 if species=="atlanticcod"

gen hadd_harvest=1899676 if species=="haddock"
gen hadd_releases=1509471 if species=="haddock"

mvencode cod_harvest cod_releases hadd_harvest hadd_releases, mv(0) override
 

gen cod_tot_ab1=cod_harvest*prop_harvest
gen cod_tot_b2=cod_releases*prop_released

gen hadd_tot_ab1=hadd_harvest*prop_harvest
gen hadd_tot_b2=hadd_releases*prop_released

egen cod_tot_cat=rowtotal(cod_tot_ab1 cod_tot_b2)
egen hadd_tot_cat=rowtotal(hadd_tot_ab1 hadd_tot_b2)

collapse (sum) cod_tot_cat hadd_tot_cat, by(species l_in)

gen tot_cat= cod+hadd
keep l s t


egen sum_tot=sum(tot_cat),  by(species)
format tot_cat sum_tot %12.0gc
gen observed_prob=tot_cat/sum_tot


preserve 
rename l_in fitted_length
keep fitted_length observed_prob species
duplicates drop
tempfile observed_prob
save `observed_prob', replace
restore


*Now smooth the data by fitting to gamma
****estimate gamma parameters for each distirbution

tempfile new
save `new', replace

global fitted_sizes

levelsof species, local(sps)
foreach s of local sps{
u `new', clear

keep if species=="`s'"
keep l_in tot
replace tot=round(tot)
expand tot
drop if tot==0
gammafit l_in
local alpha=e(alpha)
local beta=e(beta)

gen gammafit=rgamma(`alpha', `beta')
replace gammafit=round(gammafit, .5)
gen nfish=1
collapse (sum) nfish, by(gammafit)
egen sumnfish=sum(nfish)
gen fitted_prob=nfish/sumnfish
gen species="`s'"

tempfile fitted_sizes`s'
save `fitted_sizes`s'', replace
global fitted_sizes "$fitted_sizes "`fitted_sizes`s''" " 
}
clear
dsconcat $fitted_sizes

rename gammafit fitted_length		   

merge 1:1 fitted_length species using `observed_prob'
sort species fitted_length 
mvencode fitted_prob observed_prob, mv(0) override
drop if fitted_length==0



*truncate the fitted_probability distribtuion to range of the observed distirbtuion
gen tab=.
levelsof species, local(sts)
foreach s of local sts{

su fitted_length if observed_prob!=0 & species=="`s'"
replace tab=1 if species=="`s'" & (fitted_length>`r(max)' | fitted_length < `r(min)')

*su nfish if state=="`s'"
*replace fitted_prob=nfish/`r(sum)'

}
drop if tab==1
drop tab
drop if fitted_prob==0

egen sum_fitted_prob=sum(fitted_prob), by(species)
replace fitted_prob=fitted_prob/sum_fitted_prob
drop sum_fitted_prob

/* 
twoway(scatter observed fitted_length if species=="atlanticcod" , connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
		   (scatter fitted_prob fitted_length if species=="atlanticcod"  , connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)  ///
		   xtitle("Length in inches") ytitle("Prob") xlabel(0(2)34)    ///
		   legend(lab(1 "CT raw") lab(2 "CT pstar")   cols()))	   
*/
*Now, using the p-star values, create an adjust probability-at-length distribution 
*Input the p-star values at the length category below the actual 2020 minimum size 

gen p_star=0.915 if species=="atlanticcod" & fitted_length== 20.5
replace p_star=0.45 if species=="haddock" & fitted_length== 16.5




*The code below adjust the fitted probabilities to account for the p_star values
gen below=.
gen above=.
levelsof species, local(sts)
foreach s of local sts{
	su fitted_length if p_star!=. & species=="`s'"
	replace below=1 if fitted_length<= `r(max)' & species=="`s'"
	replace above=1 if fitted_length> `r(max)' & species=="`s'"
}

/*
egen sum_belowo=sum(observed_prob), by(species below)
replace sum_belowo=. if below==.

egen sum_belowp=sum(p_star), by(species below)
replace sum_belowp=. if below==.
*/
egen sum_below=sum(fitted_prob), by(species below)
replace sum_below=. if below==.

egen sum_above=sum(fitted_prob), by(species above)
replace sum_above=. if above==.

bysort species (fitted_length): gen first=1 if _n==1
gen cdf_star=fitted_prob if first==1
egen pstar_all= sum(p_star), by(species)

gen prob_below_adj=fitted_prob/sum_below*pstar_all if below==1 & first !=1
replace prob_below_adj=fitted_prob if first==1

gen prob_above_adj=fitted_prob/sum_above*(1-pstar_all) if above==1 
mvencode prob_below_adj prob_above_adj, mv(0) overr
egen prob_star=rowtotal(prob_above_adj prob_below_adj )

format prob_star %8.0g
*keep state fitted_length  observed_prob prob_star 

egen sum_prob_o=sum(observed_prob), by(species)
egen sum_prob_f=sum(fitted_prob), by(species)
egen sum_prob_n=sum(prob_star), by(species)

replace prob_star=prob_star/sum_prob_n
drop sum_prob_n
egen sum_prob_n=sum(prob_star), by(species)
drop sum_prob_o sum_prob_n 
drop sum_prob_f
/*

levelsof species,  local(sts)
foreach s of local sts{
twoway	(scatter fitted_prob fitted_length if species=="`s'" , connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
		   (scatter prob_star fitted_length if species=="`s'"  , connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i))   ///
		   	(scatter observed_prob fitted_length if species=="`s'"  , connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i)   ///
		   xtitle("") ytitle("")  xlab(4(1)30, labsize(vsmall))  ///
		   legend(lab(1 "fitted_prob") lab(2 "prob_star")   lab(2 "observed_prob")  cols()) title("`s'") $graphoptions name("gr`s'", replace))
			
}
grc1leg  gratlanticcod grhaddock


drop tot_smooth1 tot_smooth2 smoothed
drop tot_smooth1 
lowess prob_star fitted_length if species=="atlanticcod" /*&   fitted_length<= 20.5*/, bwidth(.05) gen(tot_smooth1) mean
*lowess prob_star fitted_length if species=="atlanticcod" /* &   fitted_length> 20.5*/, bwidth(.3) gen(tot_smooth2) mean
*mvencode tot_smooth1 tot_smooth2, mv(0)
mvencode tot_smooth1 , mv(0)

su tot_smooth1 if species=="atlanticcod" &  fitted_length<= 20.5
return list

levelsof species,  local(sts)
foreach s of local sts{
twoway	(scatter fitted_prob fitted_length if species=="`s'" , connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
		   (scatter prob_star fitted_length if species=="`s'"  , connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i))   ///
		   	(scatter tot_smooth1 fitted_length if species=="`s'"  , connect(direct) lcol(blue)   lwidth(medthick)  lpat(solid) msymbol(i)   ///
		   xtitle("") ytitle("")  xlab(4(1)30, labsize(vsmall))  ///
		   legend(lab(1 "fitted_prob") lab(2 "prob_star")   lab(2 "observed_prob")  cols()) title("`s'") $graphoptions name("gr`s'", replace))
			
}

grc1leg  gratlanticcod grhaddock


levelsof species,  local(sts)
foreach s of local sts{
twoway	(scatter fitted_prob fitted_length if species=="`s'" , connect(direct) lcol(black)   lwidth(medthick)  lpat(solid) msymbol(i) $graphoptions ) ///
		   (scatter observed fitted_length if species=="`s'"  , connect(direct) lcol(red)   lwidth(medthick)  lpat(solid) msymbol(i)   ///
		   xtitle("") ytitle("")  xlab(4(1)30, labsize(vsmall))  ///
		   legend(lab(1 "fitted_prob") lab(2 "prob_star")  lab(3 "observed") cols()) title("`s'") $graphoptions name("gr`s'", replace))
			
}

grc1leg  gratlanticcod grhaddock

grc1leg grMA grRI grCT grNY grNJ grDE grMD grVA grNC

scatter prob_catch length if reg=="NJ" , connect(direct) lcol(black)   lwidth(medthick) 
scatter prob_catch length if reg=="NO" , connect(direct) lcol(black)   lwidth(medthick) 
scatter prob_catch length if reg=="SO" , connect(direct) lcol(black)   lwidth(medthick) 
*/

drop prob_above_adj prob_below_adj pstar_all cdf_star first sum_above sum_below above below p_star _merge
drop nfish sumn
order species fitted_length obs fitted_prob prob_star

export delimited using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\length_distns_2020.csv", replace


