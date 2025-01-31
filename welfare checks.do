
cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM"
import excel using "predictions_data5.xlsx", clear first

keep if inlist(copula, "_clayton", "baseline")
drop tot_cod_catch tot_keep_cod tot_hadd_catch tot_keep_hadd cv_sum cod_keep_trip hadd_keep_trip cod_catch_trip hadd_catch_trip cv_trip cv_trip_diff cv_sum_diff

replace estimated_trips_ind=estimated_trips if estimated_trips_ind==.

gen cod_keep_trip=(n_choice_occasions*cod_keep_i)/estimated_trips
gen hadd_keep_trip=(n_choice_occasions*hadd_keep_i)/estimated_trips
gen cod_rel_trip=tot_rel_cod/estimated_trips
gen hadd_rel_trip=tot_rel_hadd/estimated_trips

replace cod_catch_trip_ind=(cod_keep_trip+cod_rel_trip) if cod_catch_trip_ind==.
replace hadd_catch_trip_ind=(hadd_keep_trip+hadd_rel_trip) if hadd_catch_trip_ind==.

replace cod_keep_trip_ind=cod_keep_trip if cod_keep_trip_ind==.
replace hadd_keep_trip_ind=hadd_keep_trip if hadd_keep_trip_ind==.


gen cod_rel_trip_ind=cod_catch_trip_ind-cod_keep_trip_ind
gen hadd_rel_trip_ind=hadd_catch_trip_ind-hadd_keep_trip_ind


collapse (mean) cod_keep_trip_ind hadd_keep_trip_ind cod_rel_trip_ind hadd_rel_trip_ind, by(decade)

local vars cod_keep_trip_ind hadd_keep_trip_ind cod_rel_trip_ind hadd_rel_trip_ind
foreach v of local vars{
	replace `v'=`v'*100
	
}

gen cost=55.50573
gen age=55
gen likely=1
gen prefer=1


di 3.9174137* 0.162 + .02455048* 1.594 //cod release + cod keep 
di  -.26712465* 0.094+.07024813*1.156 //haddock release + haddock keep 

gen beta_sqrt_cod_keep =  1.594
gen beta_sqrt_cod_release = 0.162 
gen beta_sqrt_hadd_keep = 1.156
gen beta_sqrt_hadd_release = 0.094 
gen beta_sqrt_cod_hadd_keep  =-0.314  
gen beta_cost = -0.015 
gen beta_opt_out =-1.871 
gen beta_opt_out_age  =0.047 
gen beta_opt_out_likely  =-1.272 
gen beta_opt_out_prefer  =-1.079 

gen vA=        beta_sqrt_cod_keep*sqrt(cod_keep_trip_ind) + ///
                    beta_sqrt_cod_release*sqrt(cod_rel_trip_ind) +  ///
                    beta_sqrt_hadd_keep*sqrt(hadd_keep_trip_ind) + ///
                    beta_sqrt_hadd_release*sqrt(hadd_rel_trip_ind) + ///
                    beta_sqrt_cod_hadd_keep*(sqrt(cod_keep_trip_ind)*sqrt(hadd_keep_trip_ind)) + ///
                    beta_cost*cost
/*
gen vA=        beta_sqrt_cod_keep*cod_keep_trip_ind + ///
                    beta_sqrt_cod_release*cod_rel_trip_ind +  ///
                    beta_sqrt_hadd_keep*hadd_keep_trip_ind + ///
                    beta_sqrt_hadd_release*hadd_rel_trip_ind + ///
                    beta_sqrt_cod_hadd_keep*(cod_keep_trip_ind*hadd_keep_trip_ind) + ///
                    beta_cost*cost
*/
expand 2, gen(dup)	
gen opt_out=1 if dup==1		
mvencode opt, mv(0)		

gen vA_optout = beta_opt_out*opt_out+ ///
        beta_opt_out_age*age + ///
        beta_opt_out_likely*likely + ///
        beta_opt_out_prefer*prefer 
					
gen alt=1 if opt==0
replace alt=2 if opt==1

gen exp_vA=exp(vA) if alt==1
replace exp_vA =exp(vA_optout) if alt==2
sort decade alt

egen vA_col_sum = sum(exp_vA), by(decade)
gen prob0 =exp_vA/vA_col_sum


preserve
keep if decade==0
tempfile base0
save `base0', replace
restore 

gen tab=1
keep  decade vA exp_vA vA_col_sum prob0 tab alt  beta_cost cod_keep_trip_ind hadd_keep_trip_ind cod_rel_trip_ind hadd_rel_trip_ind
preserve
keep if decade==0
drop decade
ds tab alt beta_cost cod_keep_trip_ind hadd_keep_trip_ind cod_rel_trip_ind hadd_rel_trip_ind, not
renvarlab `r(varlist)', postfix(_0)
tempfile base
save `base', replace
restore 

drop if decade==0 
merge m:1 tab alt using `base'
append using `base0'


gen change_CS = (1/beta_cost)*(log(vA_col_sum)-log(vA_col_sum_0))

keep if alt==1
sort decade

tsset decade 

tsline cod_keep_trip_ind hadd_keep_trip_ind cod_rel_trip_ind hadd_rel_trip_ind  change_CS, xtick(0(1)8)

*normalized values 
local vars cod_keep_trip_ind hadd_keep_trip_ind cod_rel_trip_ind hadd_rel_trip_ind  
foreach v of local vars {
    qui summ `v'
    gen `v'2 = (`v' - r(min)) / (r(max) - r(min))
}

* percent chaneg from baselin 
local vars cod_keep_trip_ind hadd_keep_trip_ind cod_rel_trip_ind hadd_rel_trip_ind  
foreach v of local vars {
    qui summ `v' if decade==0
	local base = `r(mean)'
	
    gen `v'3 = ((`v' - `base') / `base')*100
}

foreach v in change_CS {
     su `v', meanonly
     gen double `v'_minmax = 2 * (`v' - r(min)) / (r(max) - r(min)) - 1
}

twoway (tsline cod_keep_trip_ind2 hadd_keep_trip_ind2 cod_rel_trip_ind2 hadd_rel_trip_ind2, yaxis(1))  ///
			(tsline change_CS, yaxis(2)) 

twoway (tsline cod_keep_trip_ind3, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline hadd_keep_trip_ind3 , recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline cod_rel_trip_ind3 , recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline hadd_rel_trip_ind3, recast(connected) msymbol(O) msize(vsmall)  yline(0) yaxis(1))  ///
			(tsline change_CS, recast(connected) msymbol(O) msize(vsmall) lcol(black) yaxis(2) yline(0, axis(2) lcol(black))) 			

			
***Simulations based on actual data 			
cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM"
import excel using "trip_level_output_test__clayton_ind_0.xlsx", clear first
collapse (sum) cod_keep_sum_base hadd_keep_sum_base cod_rel_sum_base hadd_rel_sum_base cod_catch_sum_base hadd_catch_sum_base ntrips_base, by(decade)
ds decade, not
renvarlab `r(varlist)', postdrop(5)
ds decade, not
replace decade=0
rename ntrips ntrips_alt
tempfile f0
save `f0', replace 

forv i=1/8{
	
import excel using "trip_level_output_test__clayton_ind_`i'.xlsx", clear first
tempfile f`i'
save `f`i'', replace 
}

u `f1', clear
append using `f2'		
append using `f3'	
append using `f4'		
append using `f5'		
append using `f6'		
append using `f7'		
append using `f8'		


collapse (sum) cod_keep_sum hadd_keep_sum cod_rel_sum hadd_rel_sum cod_catch_sum hadd_catch_sum cv_sum ntrips_alt, by(decade)
append using `f0'		

local vars cod_keep_sum hadd_keep_sum cod_rel_sum hadd_rel_sum cod_catch_sum hadd_catch_sum 
foreach v of local vars  {
     gen `v'_trip=`v'/ntrips_alt
	 qui summ `v'_trip if decade==0
	 local base = `r(mean)'
	 gen diff_2020_`v'=`v'_trip-`base'
}


local vars  cv_sum
foreach v of local vars  {
     gen `v'_trip=`v'/ntrips_alt
	
}

tsset decade 
twoway (tsline cod_keep_sum_trip, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline cod_rel_sum_trip , recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline hadd_keep_sum_trip , recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline hadd_rel_sum_trip, recast(connected) msymbol(O) msize(vsmall)  yline(0) yaxis(1))  ///
			//(tsline cod_catch_sum_trip , recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			//(tsline hadd_catch_sum_trip, recast(connected) msymbol(O) msize(vsmall)  yline(0) yaxis(1))  ///
			(tsline cv_sum_trip, recast(connected) msymbol(O) msize(vsmall) lcol(black) yaxis(2) yline(0, axis(2) lcol(black))) 	

			
twoway (tsline diff_2020_cod_keep_sum if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_hadd_keep_sum if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_cod_rel_sum if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_hadd_rel_sum if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yline(0) yaxis(1))  ///
			(tsline cv_sum_trip if decade!=0,  xlab(1(1)8) recast(connected) msymbol(O) msize(vsmall) lcol(black) yaxis(2) yline(0, axis(2) lcol(black))) 				
			//(tsline diff_2020_cod_catch_sum , recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			//(tsline diff_2020_hadd_catch_sum, recast(connected) msymbol(O) msize(vsmall)  yline(0) yaxis(1))  ///

			
***Simulations based on data where I held cod catch at 2020 levels 
cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM"
import excel using "trip_level_output_test2__clayton_ind_0.xlsx", clear first
collapse (sum) cod_keep_sum_base hadd_keep_sum_base cod_rel_sum_base hadd_rel_sum_base cod_catch_sum_base hadd_catch_sum_base ntrips_base, by(decade)
ds decade, not
renvarlab `r(varlist)', postdrop(5)
ds decade, not
replace decade=0
rename ntrips ntrips_alt
tempfile f0
save `f0', replace 

forv i=1/8{
	
import excel using "trip_level_output_test2__clayton_ind_`i'.xlsx", clear first
tempfile f`i'
save `f`i'', replace 
}

u `f1', clear
append using `f2'		
append using `f3'	
append using `f4'		
append using `f5'		
append using `f6'		
append using `f7'		
append using `f8'		


collapse (sum) cod_keep_sum hadd_keep_sum cod_rel_sum hadd_rel_sum cod_catch_sum hadd_catch_sum cv_sum ntrips_alt ///
					   cod_keep_sum_base hadd_keep_sum_base cod_rel_sum_base hadd_rel_sum_base cod_catch_sum_base hadd_catch_sum_base ntrips_base ///
					   cod_keep_sum_unw hadd_keep_sum_unw cod_rel_sum_unw hadd_rel_sum_unw cod_catch_sum_unw hadd_catch_sum_unw   ///
					   cod_keep_sum_unw_base hadd_keep_sum_unw_base cod_rel_sum_unw_base hadd_rel_sum_unw_base cod_catch_sum_unw_base hadd_catch_sum_unw_base , ///
					   by(decade )
*append using `f0'		

local vars cod_keep_sum hadd_keep_sum cod_rel_sum hadd_rel_sum   ///
			   cod_keep_sum_unw hadd_keep_sum_unw cod_rel_sum_unw hadd_rel_sum_unw  
foreach v of local vars  {
     gen `v'_trip=`v'/ntrips_alt
	 *qui summ `v'_trip if decade==0
	 *local base = `r(mean)'
	 *gen diff_2020_`v'=`v'_trip-`base'
}

local vars   cod_keep_sum_base hadd_keep_sum_base cod_rel_sum_base hadd_rel_sum_base   ///
				cod_keep_sum_unw_base hadd_keep_sum_unw_base cod_rel_sum_unw_base hadd_rel_sum_unw_base  
foreach v of local vars  {
     gen `v'_trip=`v'/ntrips_base
	 *qui summ `v'_trip if decade==0
	 *local base = `r(mean)'
	 *gen diff_2020_`v'=`v'_trip-`base'
}

local vars  cv_sum
foreach v of local vars  {
     gen `v'_trip=`v'/ntrips_alt
	
}
*keep decade month  *trip
*order decade month cod* hadd*
order decade  cod* hadd*


local vars cod_keep_sum hadd_keep_sum cod_rel_sum hadd_rel_sum   ///
			   cod_keep_sum_unw hadd_keep_sum_unw cod_rel_sum_unw hadd_rel_sum_unw  
foreach v of local vars  {
	 *qui summ `v'_trip if decade==0
	 *local base = `r(mean)'
	 gen diff_2020_`v'_trip=`v'_trip-`v'_base_trip
	 gen diff_2020_`v'=`v'-`v'_base
}

/*
local vars cod_keep_sum hadd_keep_sum cod_rel_sum hadd_rel_sum cod_catch_sum hadd_catch_sum ///
			   cod_keep_sum_unw hadd_keep_sum_unw cod_rel_sum_unw hadd_rel_sum_unw cod_catch_sum_unw hadd_catch_sum_unw
foreach v of local vars  {
	 *qui summ `v'_trip if decade==0
	 *local base = `r(mean)'
	 gen diff_2020_`v'_sum=`v'-`v'_base
}

*/
global graphoptions graphregion(fcolor(white) lcolor(white) margin(large)) plotregion(fcolor(white) lcolor(white)) bgcolor(white)

gen decade2=decade +2030
gen yr_mon=ym(decade2,month)
format yr_mon %tm

tsset yr_mon 

tsset decade
*decade 8 by month
twoway (tsline diff_2020_cod_keep_sum_trip if decade2==2038, recast(connected)  lwidth(thin)  msymbol(O) msize(vsmall)  yaxis(1) )  ///
			(tsline diff_2020_hadd_keep_sum_trip  if decade2==2038, recast(connected)  lwidth(thin)  msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_cod_rel_sum_trip  if decade2==2038, recast(connected)  lwidth(thin)  msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_hadd_rel_sum_trip  if decade2==2038, ytitle("# fish per trip", axis(1)) recast(connected)  lwidth(thin)  msymbol(O) msize(vsmall)  yline(0, axis(1)) yaxis(1)  ylab(, labsize(vsmall) angle(horizontal)  axis(1)) xlab(#8, labsize(vsmall) angle(45)) xtitle(Decade 8-month))  ///
			(tsline cv_sum_trip if decade2==2038, recast(connected)  lwidth(thin)  msymbol(O) msize(vsmall) lcol(black) yaxis(2) ytitle("CV per trip", axis(2)) ylab(, labsize(vsmall) angle(horizontal) axis(2)) yline(0, axis(2) lcol(black))) 							


twoway tsline diff_2020_cod_keep_sum if decade2==2038, recast(connected) lwidth(thin) msymbol(O) msize(vsmall)  yaxis(1)   ||  ///
			tsline diff_2020_hadd_keep_sum  if decade2==2038, recast(connected)  lwidth(thin) msymbol(O) msize(vsmall)  yaxis(1)  ||  ///
			tsline diff_2020_cod_rel_sum  if decade2==2038, recast(connected) lwidth(thin) msymbol(O) msize(vsmall)  yaxis(1)  || ///
			tsline diff_2020_hadd_rel_sum  if decade2==2038, recast(connected) lwidth(thin) msymbol(O) msize(vsmall)  yaxis(1)  ylab(, labsize(vsmall) angle(horizontal)  axis(1)) xlab(#8, labsize(vsmall) angle(45)) xtitle(Decade 8-month) yline(0, axis(1) lcol(red))  ytitle("total # fish", axis(1)) || 			///
			tsline cv_sum if decade2==2038,  recast(connected) msymbol(O) msize(vsmall) lwidth(thin) yaxis(2) lcol(black)  ytitle("Total CV", axis(2)) ylab(, labsize(vsmall) angle(horizontal) axis(2)) yline(0, axis(2) lcol(black))  
			
*all decades by decade 
twoway (tsline diff_2020_cod_keep_sum_trip, recast(connected)  lwidth(thin)  msymbol(O) msize(vsmall)  yaxis(1) )  ///
			(tsline diff_2020_hadd_keep_sum_trip , recast(connected)  lwidth(thin)  msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_cod_rel_sum_trip  , recast(connected)  lwidth(thin)  msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_hadd_rel_sum_trip , ytitle("# fish per trip", axis(1)) recast(connected)  lwidth(thin)  msymbol(O) msize(vsmall)  yline(0, axis(1)) yaxis(1)  ylab(, labsize(vsmall) angle(horizontal)  axis(1)) xlab(#8, labsize(vsmall) angle(45)) xtitle(Decade))  ///
			(tsline cv_sum_trip , recast(connected)  lwidth(thin)  msymbol(O) msize(vsmall) lcol(black) yaxis(2) ytitle("CV per trip", axis(2)) ylab(, labsize(vsmall) angle(horizontal) axis(2)) yline(0, axis(2) lcol(black))) 							


twoway tsline diff_2020_cod_keep_sum, recast(connected) lwidth(thin) msymbol(O) msize(vsmall)  yaxis(1)   ||  ///
			tsline diff_2020_hadd_keep_sum , recast(connected)  lwidth(thin) msymbol(O) msize(vsmall)  yaxis(1)  ||  ///
			tsline diff_2020_cod_rel_sum  , recast(connected) lwidth(thin) msymbol(O) msize(vsmall)  yaxis(1)  || ///
			tsline diff_2020_hadd_rel_sum , recast(connected) lwidth(thin) msymbol(O) msize(vsmall)  yaxis(1)  ylab(, labsize(vsmall) angle(horizontal)  axis(1)) xlab(#8, labsize(vsmall) angle(45)) xtitle(Decade ) yline(0, axis(1) lcol(red))  ytitle("total # fish", axis(1)) || 			///
			tsline cv_sum ,  recast(connected) msymbol(O) msize(vsmall) lwidth(thin) yaxis(2) lcol(black)  ytitle("Total CV", axis(2)) ylab(, labsize(vsmall) angle(horizontal) axis(2)) yline(0, axis(2) lcol(black))  
			
			

			
twoway (tsline cod_keep_sum_unw_trip, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline cod_rel_sum_unw_trip , recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline hadd_keep_sum_unw_trip , recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline hadd_rel_sum_unw_trip, recast(connected) msymbol(O) msize(vsmall)  yline(0) yaxis(1))  ///
			(tsline cv_sum_trip, recast(connected) msymbol(O) msize(vsmall) lcol(black) yaxis(2) yline(0, axis(2) lcol(black))) 	

			
twoway (tsline diff_2020_cod_keep_sum if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_hadd_keep_sum if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_cod_rel_sum if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_hadd_rel_sum if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yline(0) yaxis(1))  ///
			(tsline cv_sum_trip if decade!=0,  xlab(1(1)8) recast(connected) msymbol(O) msize(vsmall) lcol(black) yaxis(2) yline(0, axis(2) lcol(black))) 				

twoway (tsline diff_2020_cod_keep_sum_unw if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_hadd_keep_sum_unw if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_cod_rel_sum_unw if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_hadd_rel_sum_unw if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yline(0) yaxis(1))  ///
			(tsline cv_sum_trip if decade!=0,  xlab(1(1)8) recast(connected) msymbol(O) msize(vsmall) lcol(black) yaxis(2) yline(0, axis(2) lcol(black))) 				
			
twoway (tsline diff_2020_cod_keep_sum_sum if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_hadd_keep_sum_sum if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_cod_rel_sum_sum if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_2020_hadd_rel_sum_sum if decade!=0, recast(connected) msymbol(O) msize(vsmall)  yline(0) yaxis(1))  ///
			(tsline cv_sum if decade!=0,  xlab(1(1)8) recast(connected) msymbol(O) msize(vsmall) lcol(black) yaxis(2) yline(0, axis(2) lcol(black))) 	
			
	
import excel using	"trip_data_check_clayton_ind_8.xlsx", clear first 
expand 2, gen(dup)
gen alt=1 if dup==0
replace alt=2 if dup==1
gen opt_out=1 if alt==2
mvencode opt_out, mv(0)


gen vA_optout= beta_opt_out*opt_out+ ///
          beta_opt_out_age*age + ///
          beta_opt_out_likely*likely_to_fish + ///
          beta_opt_out_prefer*fish_pref_more

gen v0_optout = beta_opt_out*opt_out+ ///
          beta_opt_out_age*age +  ///
          beta_opt_out_likely*likely_to_fish + ///
          beta_opt_out_prefer*fish_pref_more 

gen expon_vA=exp(vA) if alt==1
replace expon_vA=exp(vA_optout) if alt==2

gen expon_v0=exp(v0) if alt==1
replace expon_v0=exp(v0_optout) if alt==2

egen vA_col_sum = sum(expon_vA), by(period2 catch_draw tripid decade)
egen v0_col_sum = sum(expon_v0), by(period2 catch_draw tripid decade)

gen change_CS = (1/beta_cost)*(log(vA_col_sum)-log(v0_col_sum))
gen probA = expon_vA/vA_col_sum
gen prob0 = expon_v0/v0_col_sum

sort period2 catch_draw tripid alt

su change_CS
keep if alt==1

gen tot_cod_cat=tot_keep_cod_base+tot_rel_cod_base
gen tot_cod_cat_base=tot_keep_cod_base+tot_rel_cod_base 
gen tot_hadd_cat=tot_keep_hadd+tot_rel_hadd 
gen tot_hadd_cat_base=tot_keep_hadd_base+tot_rel_hadd_base 
replace tot_keep_cod=tot_keep_cod_base
replace tot_rel_cod=tot_rel_cod_base		  

ds period2 decade tripid, not
collapse (mean) `r(varlist)', by(period2 decade tripid)


drop age  alt beta_cost beta* catch_draw  cost_base draw  expon_v0  expon_vA  fish_pref_more  likely_to_fish  opt_out v0  v0_col_sum   v0_optout vA  vA_col_sum  vA_optout			

gen tot_keep_cod_unw=tot_keep_cod
gen                    tot_keep_hadd_unw=tot_keep_hadd 
gen                    tot_rel_cod_unw=tot_rel_cod 
gen                    tot_rel_hadd_unw=tot_rel_hadd 
gen                   tot_cod_cat_unw=tot_cod_cat 
gen                    tot_hadd_cat_unw=tot_hadd_cat 
gen                    tot_keep_cod_base_unw=tot_keep_cod_base 
gen                   tot_keep_hadd_base_unw=tot_keep_hadd_base 
gen                    tot_rel_cod_base_unw=tot_rel_cod_base 
gen                    tot_rel_hadd_base_unw=tot_rel_hadd_base 
gen                    tot_cod_cat_base_unw=tot_cod_cat_base 
 gen                   tot_hadd_cat_base_unw=tot_hadd_cat_base


local vars tot_keep_cod tot_keep_hadd tot_rel_cod tot_rel_hadd tot_cod_cat tot_hadd_cat
foreach v of local vars{
	replace `v'=`v'*probA
	
}

local vars tot_keep_cod_base tot_keep_hadd_base tot_rel_cod_base tot_rel_hadd_base tot_cod_cat_base tot_hadd_cat_base
foreach v of local vars{
	replace `v'=`v'*prob0
	
}

preserve 
import excel using "calibration_data_all_test2_check_clayton_ind_8.xlsx", clear first 
keep period n_choice_occasions
rename period period2
tempfile calib
save `calib', replace 
restore 

merge m:1 period2 using `calib'
drop _merge 
gen expand = n_choice_occasions/1000


local vars tot_keep_cod tot_rel_cod tot_cod_cat tot_keep_hadd tot_rel_hadd tot_hadd_cat tot_keep_cod_base tot_rel_cod_base tot_cod_cat_base tot_keep_hadd_base tot_rel_hadd_base tot_hadd_cat_base tot_keep_cod_unw tot_keep_hadd_unw tot_rel_cod_unw tot_rel_hadd_unw tot_cod_cat_unw tot_hadd_cat_unw tot_keep_cod_base_unw tot_keep_hadd_base_unw tot_rel_cod_base_unw tot_rel_hadd_base_unw tot_cod_cat_base_unw tot_hadd_cat_base_unw change_CS

foreach v of local vars{
	replace `v'=`v'*expand 
}

gen ntrips_alt=expand*probA
gen ntrips_base=expand*prob0

collapse (sum) tot_keep_cod tot_rel_cod tot_cod_cat tot_keep_hadd tot_rel_hadd tot_hadd_cat tot_keep_cod_base tot_rel_cod_base tot_cod_cat_base tot_keep_hadd_base tot_rel_hadd_base tot_hadd_cat_base tot_keep_cod_unw tot_keep_hadd_unw tot_rel_cod_unw tot_rel_hadd_unw tot_cod_cat_unw tot_hadd_cat_unw tot_keep_cod_base_unw tot_keep_hadd_base_unw  tot_rel_cod_base_unw tot_rel_hadd_base_unw tot_cod_cat_base_unw tot_hadd_cat_base_unw change_CS ntrips_alt ntrips_base, by(month)



local vars tot_keep_cod tot_rel_cod tot_cod_cat tot_keep_hadd tot_rel_hadd tot_hadd_cat    tot_keep_cod_unw tot_keep_hadd_unw tot_rel_cod_unw tot_rel_hadd_unw tot_cod_cat_unw tot_hadd_cat_unw       
foreach v of local vars  {
     gen `v'_trip=`v'/ntrips_alt
	 *qui summ `v'_trip if decade==0
	 *local base = `r(mean)'
	 *gen diff_2020_`v'=`v'_trip-`base'
}

local vars        tot_keep_cod_base tot_rel_cod_base tot_cod_cat_base tot_keep_hadd_base tot_rel_hadd_base tot_hadd_cat_base    tot_keep_cod_base_unw tot_keep_hadd_base_unw  tot_rel_cod_base_unw tot_rel_hadd_base_unw tot_cod_cat_base_unw tot_hadd_cat_base_unw
foreach v of local vars  {
     gen `v'_trip=`v'/ntrips_base
	 *qui summ `v'_trip if decade==0
	 *local base = `r(mean)'
	 *gen diff_2020_`v'=`v'_trip-`base'
}

local vars  change_CS
foreach v of local vars  {
     gen `v'_trip=`v'/ntrips_alt
	
}
order change_CS ntrips_alt


asgen wmchange_CS = change_CS_trip, w(ntrips_alt)
order wmchange_CS
local vars tot_keep_cod tot_rel_cod tot_keep_hadd tot_rel_hadd
foreach v of local vars{
asgen wm_`v'_trip = `v'_trip, w(ntrips_alt)
order wm_`v'_trip
}

local vars tot_keep_cod tot_rel_cod tot_keep_hadd tot_rel_hadd
foreach v of local vars{
asgen wm_`v'_base_trip = `v'_base_trip, w(ntrips_base)
order wm_`v'_base_trip
}

local vars tot_keep_cod tot_rel_cod tot_keep_hadd tot_rel_hadd
foreach v of local vars{
gen diff_`v'_trip = wm_`v'_trip-wm_`v'_base_trip
	
}
order wmchange_CS diff* wm* tot_keep_cod_base_trip tot_rel_cod_base_trip tot_cod_cat_base_trip tot_keep_hadd_base_trip tot_rel_hadd_base_trip tot_hadd_cat_base_trip
order month change tot_keep_cod* tot_rel_cod* tot_keep_hadd* tot_rel_hadd*

local vars tot_keep_cod tot_rel_cod tot_keep_hadd tot_rel_hadd
foreach v of local vars{
	gen diff_`v'= `v'-`v'_base
	gen diff_`v'_unw=`v'_unw-`v'_base_unw
	
}


tsset month  
			
		twoway (tsline diff_tot_keep_cod , recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_tot_rel_cod , recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_tot_keep_hadd, recast(connected) msymbol(O) msize(vsmall)  yaxis(1))  ///
			(tsline diff_tot_rel_hadd , recast(connected) msymbol(O) msize(vsmall)  yline(0) yaxis(1))  ///
			(tsline change_CS ,  xlab(4(1)12) recast(connected) msymbol(O) msize(vsmall) lcol(black) yaxis(2) yline(0, axis(2) lcol(black))) 	