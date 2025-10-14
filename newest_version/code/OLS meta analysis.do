
cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM"


local vs _gaussian_corr _gaussian_ind _plackett_corr _plackett_ind _clayton_corr _clayton_ind _frank_corr _frank_ind _gumbel_corr _gumbel_ind

foreach v of local vs{
	import excel  "predictions_v2_`v'.xlsx", clear first
	tempfile `v'
	save ``v'', replace 
}

u `_gaussian_corr'
append using  `_gaussian_ind'
append using  `_plackett_corr'
append using  `_plackett_ind'
append using  `_clayton_corr'
append using  `_clayton_ind'
append using  `_frank_corr'
append using  `_frank_ind'
append using  `_gumbel_corr'
append using  `_gumbel_ind'

replace copula="ind" if corr_type=="ind"
encode corr, gen(corr_type2)
encode copula, gen(copula2)

gen inv_cv=-cv_sum
gen open=1 if month==9
eststo clear	
gen correlation3=1 if corr_type=="corr"
mvencode correlation3, mv(0)

gen hadd_keep_trip= hadd_keep_sum/ntrips_alt_sum
gen hadd_rel_trip= hadd_rel_sum/ntrips_alt_sum
gen cod_keep_trip= cod_keep_sum/ntrips_alt_sum
gen cod_rel_trip= cod_rel_sum/ntrips_alt_sum

gen cod_hadd_harvest= cod_keep_sum*hadd_keep_sum
gen cod_hadd_keep_trip= cod_keep_trip*hadd_keep_trip




*drop if corr_type=="ind" & !inlist(copula, "_gaussian")
eststo m1: reg inv_cv i.month i.decade cod_keep_sum hadd_keep_sum cod_rel_sum hadd_rel_sum correlation3 i.copula2 k_tau_keep_est_mnth k_tau_catch_est_mnth
eststo m1: reg inv_cv i.month i.decade cod_keep_sum hadd_keep_sum cod_hadd_harvest cod_rel_sum hadd_rel_sum correlation3 i.copula2 k_tau_keep_est_mnth k_tau_catch_est_mnth
eststo m1: reg inv_cv i.month i.decade cod_keep_trip hadd_keep_trip cod_rel_trip hadd_rel_trip correlation3 i.copula2 k_tau_keep_est_mnth k_tau_catch_est_mnth
eststo m1: reg inv_cv i.month i.decade cod_keep_trip hadd_keep_trip cod_rel_trip hadd_rel_trip correlation3 i.copula2 k_tau_keep_est_mnth k_tau_catch_est_mnth
eststo m1: reg inv_cv i.month i.decade cod_keep_sum hadd_keep_sum cod_rel_sum hadd_rel_sum correlation3 i.copula2 k_tau_keep_est_mnth k_tau_catch_est_mnth
eststo m1: reg inv_cv i.month i.decade cod_keep_trip hadd_keep_trip  cod_rel_trip hadd_rel_trip correlation3 i.copula2 k_tau_keep_est_mnth k_tau_catch_est_mnth

eststo m1: reg inv_cv i.month i.decade cod_keep_trip hadd_keep_trip  cod_rel_trip hadd_rel_trip  ib(6).copula2 


esttab m1  using "OLS_meta_results.csv", replace nobase noomitted  /// 
				stats(N r2 , fmt(0 3)) b(2) se(2)  label nogaps    starlevels( * 0.10 ** 0.05 *** 0.010)  
				
				
				
				
cd "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\output_data"
import excel  "model_output_5-14-25_reformat_coast.xlsx", clear first

preserve
keep if year==2021
gen codreltrip=codrel/ntrips
gen haddreltrip=haddrel/ntrips
drop draw copula correlation decade cv cvtrip cv_choice year tot_cat_cod tot_cat_hadd dtrip period
renvarlab `r(varlist)', postfix(_base)
expand 100 
gen draw=_n
tempfile base1
save `base1', replace 
restore 

drop if year!=.
gen inv_cv=-cv
gen inv_cvtrip=-cvtrip

encode corr, gen(corr_type2)
encode copula, gen(copula2)

gen codreltrip=codrel/ntrips
gen haddreltrip=haddrel/ntrips

merge m:1 draw using `base1', keep(3) nogen

tempfile base
save `base', replace 



import excel  "ktau_output_5-14-25.xlsx", clear first
keep if month==0
* Drop unwanted columns
drop k_tau_catch_p k_tau_keep_p

* Split domain into two parts based on "_"
gen part1 = substr(domain, 1, strpos(domain, "_") - 1)
gen part2 = substr(domain, strpos(domain, "_") + 1, .)

* Extract components
gen correlation = part1
gen copula = regexs(1) if regexm(part2, "^([A-Za-z]+)")
gen decade = regexs(1) if regexm(part2, "([0-9]+)")

* Optional: keep only desired columns
keep draw correlation copula decade k_tau_keep_est k_tau_catch_est
destring decade, replace

merge 1:1 draw correlation copula decade using `base', keep(3) nogen 

local vars  codkeeptrip haddkeeptrip codreltrip haddreltrip
foreach v of local vars{
	gen delta_`v'=`v'-`v'_base
}

gen diff_cod_keep_release=codreltrip - codkeeptrip
gen diff_hadd_keep_release=haddreltrip - haddkeeptrip

eststo clear	
eststo m1: reg cvtrip i.decade codkeeptrip haddkeeptrip codreltrip haddreltrip i.corr_type2 i.copula2  k_tau_keep_est k_tau_catch_est
eststo m1: reg cvtrip i.decade diff_cod_keep_release diff_hadd_keep_release i.corr_type2 i.copula2  k_tau_keep_est k_tau_catch_est

eststo m1: reg cvtrip  delta_codkeeptrip delta_haddkeeptrip delta_codreltrip delta_haddreltrip i.corr_type2 i.copula2  k_tau_keep_est k_tau_catch_est

eststo m1: reg cvtrip codreltrip 