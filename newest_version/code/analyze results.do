
import excel using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\output_data\model_output_test1.xlsx", clear first
import excel using "C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\newest_version\output_data\model_output_orig.xlsx", clear first

ds
local vars `r(varlist)'
// Loop through all variable names
foreach var of varlist * {
    // Check if the variable name contains the substring "old"
    if strpos("`var'", "catch") > 0 {
        // Rename the variable, replacing "old" with "new"
        local newname = subinstr("`var'", "catch", "cat", .)
        rename `var' `newname'
    }
}

rename AF  hadd_cat_base_sum_corr_clayton2
rename AU  hadd_cat_base_sum_corr_clayton3
rename BJ  hadd_cat_base_sum_corr_clayton4
rename BY  hadd_cat_base_sum_corr_clayton5
rename CN  hadd_cat_base_sum_corr_clayton6
rename DC  hadd_cat_base_sum_corr_clayton7
rename DR  hadd_cat_base_sum_corr_clayton8

keep  month-ntrips_base_corr_clayton8
ds month mode area state draw, not
local vars `r(varlist)'

collapse (sum) `vars', by(draw)
/*
reshape long cv_sum_corr_clayton cod_keep_sum_corr_clayton cod_rel_sum_corr_clayton hadd_keep_sum_corr_clayton hadd_rel_sum_corr_clayton hadd_cat_sum_corr_clayton cod_cat_sum_corr_clayton cod_keep_base_sum_corr_clayton cod_rel_base_sum_corr_clayton hadd_keep_base_sum_corr_clayton hadd_rel_base_sum_corr_clayton hadd_cat_base_sum_corr_clayton cod_cat_base_sum_corr_clayton ntrips_alt_corr_clayton ntrips_base_corr_clayton, i(draw) j(decade) string
*/
rename hadd_cat_base_sum_corr_clayton hadd_cat_base_sum_corr_clayton1
gen cod_cat_trip=.
gen hadd_cat_trip=.

forv i=1/8{
	gen cod_cat_trip`i'=cod_cat_sum_corr_clayton`i'/ntrips_alt_corr_clayton`i'
	gen hadd_cat_trip`i'=hadd_cat_sum_corr_clayton`i'/ntrips_alt_corr_clayton`i'

	gen cod_cat_trip_base`i'=cod_cat_base_sum_corr_clayton`i'/ntrips_base_corr_clayton`i'
	gen hadd_cat_trip_base`i'=hadd_cat_base_sum_corr_clayton`i'/ntrips_base_corr_clayton`i'
}

order  cod_cat_trip* hadd_cat_trip*