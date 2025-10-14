
args = commandArgs(trailingOnly=TRUE)
options(future.globals.maxSize= 1000000000)

install.packages("plyr")
install.packages("dplyr")
install.packages("readxl")
install.packages("data.table")
install.packages("conflicted")

library("plyr")
library("dplyr")
library("readxl")
library("data.table")
library("conflicted")

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)

options(scipen = 10000, digits = 10)

#Lou's data cd. Change to where "test_data.xlsx" and "calibration_data_2021draw1.rds" exist
data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

test_data <- read_excel(file.path(data_cd, "test_data.xlsx"))

# test_data contains:
    # baseline harvest/discards ("_base"),
    # projected harvest/discards (no subscript)
    # utility parameters
    # trip cost (cost_base) that is constant for baseline/projection
    # angler demographics: age, likelihood of fishing next 12 months (likley_to_fish), 
                         # prefers fishing more than other rec. activities (fish_pref_more)
    # period2: mode, bi-monthly period (1-24), area (inshore/offshore), state
    # 100 simulated trips (tripid) for each period2 in the month of September
    # 30 draws of catch for each tripid (catch_draw)
    # One iteration of the model (i=1)
    # Prediction-year catch is for decade=1, clayton copula


i=1
ndraws<-100 # number of choice occasions simulated for each period2

test_data <- test_data %>%
  dplyr::mutate(period = as.numeric(as.factor(period2)))

# period_names<-subset(test_data, select=c("period", "period2")) #save numeric period2 for subsequent merging
# period_names <- period_names[!duplicated(period_names), ]

# calculate utility in the baseline and prediction years
trip_data <-test_data %>%
  
                #  utility (prediction year)
  dplyr::mutate(vA = beta_sqrt_cod_keep*sqrt(tot_keep_cod) +
                  beta_sqrt_cod_release*sqrt(tot_rel_cod) +  
                  beta_sqrt_hadd_keep*sqrt(tot_keep_hadd) +
                  beta_sqrt_hadd_release*sqrt(tot_rel_hadd) + 
                  beta_sqrt_cod_hadd_keep*(sqrt(tot_keep_cod)*sqrt(tot_keep_hadd)) +
                  beta_cost*cost_base,
                
                #  utility (base year)
                v0 = beta_sqrt_cod_keep*sqrt(tot_keep_cod_base) +
                  beta_sqrt_cod_release*sqrt(tot_rel_cod_base) +
                  beta_sqrt_hadd_keep*sqrt(tot_keep_hadd_base) +
                  beta_sqrt_hadd_release*sqrt(tot_rel_hadd_base) +
                  beta_sqrt_cod_hadd_keep*(sqrt(tot_keep_cod_base)*sqrt(tot_keep_hadd_base)) +
                  beta_cost*cost_base)


trip_data<-trip_data %>% 
  dplyr::arrange(period2, tripid, catch_draw )

# create group index
mean_trip_data <- trip_data %>%
  data.table::data.table() %>% 
  .[, group_index := .GRP, by = .(period2, catch_draw, tripid)]


# expand the data to create two alternatives, representing the alternatives available in choice survey
mean_trip_data <- mean_trip_data %>%
  dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
  tidyr::uncount(n_alt) %>%
  dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                opt_out = ifelse(alt == 2, 1, 0))

setDT(mean_trip_data)

# Filter only alt == 2 once, and calculate vA and v0
mean_trip_data[alt == 2, c("vA", "v0") := .(
  beta_opt_out * opt_out +
    beta_opt_out_age * (age * opt_out) +
    beta_opt_out_likely * (likely_to_fish * opt_out) +
    beta_opt_out_prefer * (fish_pref_more * opt_out)
)]

# Pre-compute exponential terms
mean_trip_data[, `:=`(exp_vA = exp(vA), exp_v0 = exp(v0))]

# Group by group_index and calculate probabilities and log-sums
mean_trip_data[, `:=`(
  probA = exp_vA / sum(exp_vA),
  prob0 = exp_v0 / sum(exp_v0),
  log_sum_base = log(sum(exp_vA)),
  log_sum_alt = log(sum(exp_v0))
), by = group_index]

# Calculate consumer surplus in baseline and prediction years
mean_trip_data[, `:=`(
  CS_base = log_sum_base / -beta_cost,
  CS_alt = log_sum_alt / -beta_cost
)]

# Calculate change consumer surplus 
mean_trip_data[, `:=`(
  change_CS = CS_alt - CS_base
)]

#mean(mean_trip_data$change_CS)

# Get rid of things we don't need.
mean_trip_data <- mean_trip_data %>% 
  dplyr::filter(alt==1) %>% 
  dplyr::select(-c(alt, beta_cost,beta_opt_out, beta_opt_out_age, 
                   beta_opt_out_likely, beta_opt_out_prefer, beta_sqrt_cod_hadd_keep, 
                   beta_sqrt_cod_keep, beta_sqrt_cod_release, beta_sqrt_hadd_keep, 
                   beta_sqrt_hadd_release, likely_to_fish, fish_pref_more,  v0, vA, cost_base, age, 
                   exp_vA, exp_v0, log_sum_base, log_sum_alt, group_index, opt_out))

#rename predicted catch variables ("_new") 
mean_trip_data <- mean_trip_data %>% 
  dplyr::rename(tot_keep_cod_new=tot_keep_cod, 
                tot_rel_cod_new=tot_rel_cod, 
                tot_keep_hadd_new=tot_keep_hadd, 
                tot_rel_hadd_new=tot_rel_hadd)

#compute total predicted total catch of cod and haddock
mean_trip_data <- mean_trip_data %>% 
  dplyr::mutate(tot_cat_cod_new=tot_keep_cod_new+tot_rel_cod_new, 
                tot_cat_hadd_new=tot_keep_hadd_new+tot_rel_hadd_new, 
                tot_cat_cod_base=tot_keep_cod_base+tot_rel_cod_base, 
                tot_cat_hadd_base=tot_keep_hadd_base+tot_rel_hadd_base)                


# compute average catch, probabilities, CS, across catch draws
all_vars<-c()
all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period2","tripid")]
all_vars

mean_trip_data<-mean_trip_data  %>% as.data.table() %>%
  .[,lapply(.SD, mean), by = c("period2","tripid"), .SDcols = all_vars]


# Multiply each of the prediction-year catch variables ("_new) by prediction-year trip probability (probA) to get probability-weighted catch
# FOR DISCUSSION WITH JORGE: 
  #Update on 9/17/24 - multiply change_CS by probA to get probability-weighted change in consumer surplus. 
  #The alternative is to NOT probability-weight change_CS

list_names <- c("tot_keep_cod_new","tot_rel_cod_new", "tot_cat_cod_new",
                "tot_keep_hadd_new", "tot_rel_hadd_new" , "tot_cat_hadd_new" , "change_CS" )

mean_trip_data<-mean_trip_data %>%
  .[,as.vector(list_names) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = list_names] %>%
  .[]


# Multiply each of the baseline-year catch variables ("_base) by baseline-year trip probability (prob0) to get probability-weighted catch
list_names <- c("tot_keep_cod_base","tot_rel_cod_base", "tot_cat_cod_base",
                "tot_keep_hadd_base", "tot_rel_hadd_base" , "tot_cat_hadd_base"  )

mean_trip_data <- mean_trip_data %>%
  data.table::as.data.table() %>%
  .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
  .[]


# mean_trip_data <- mean_trip_data %>%
#   dplyr::mutate(n_choice_occasions_alt = rep(1,nrow(.))) %>%
#   dplyr::left_join(period_names, by = c("period2"))


# pull in calibration data - this gives us:
    # the number of choice occasions in the baseline-year, which we hold constant in the projection

calibration_data_table<-readRDS(paste0(data_cd, "calibration_data_2021draw", i,".rds")) %>%
  dplyr::select(c(n_choice_occasions, period2))

# merge calibration data to mean_trip_data and compute expansion factor, i.e., how many choice occasions does each of our simulated choice occasion represent
sims <- calibration_data_table %>%
  dplyr::right_join(mean_trip_data, by = c("period2")) %>%
  dplyr::mutate(ndraws = ndraws) %>%
  tidyr::separate(period2, into = c("mode", "period", "area", "state")) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  dplyr::mutate(expand = n_choice_occasions/ndraws) 

# keep relevent information
sims <- sims %>%
  dplyr::select(c(mode, period, month, area, state, n_choice_occasions, tripid, expand, change_CS, 
                  CS_base, CS_alt,  probA, prob0,expand,  probA, prob0,
                  tot_keep_cod_new, tot_rel_cod_new, tot_keep_hadd_new, tot_rel_hadd_new,
                  tot_keep_cod_base, tot_rel_cod_base, tot_keep_hadd_base,tot_rel_hadd_base, 
                  tot_cat_cod_base, tot_cat_cod_new, tot_cat_hadd_base, tot_cat_hadd_new)) %>% 
  as.data.frame()


# metrics at the choice occasion level
# here we multiply catch, welfare, and fishing effort (ntrips) in the baseline and prediction year by the expansion factor above
sims <- sims %>%
  
  data.table::as.data.table() %>%
  .[, cv_sum := expand*change_CS] %>%
  
  .[, cod_keep_sum := expand*tot_keep_cod_new] %>%
  .[, cod_rel_sum := expand*tot_rel_cod_new] %>%
  
  .[, hadd_keep_sum := expand*tot_keep_hadd_new] %>%
  .[, hadd_rel_sum := expand*tot_rel_hadd_new] %>%
  
  .[, hadd_catch_sum := expand*tot_cat_hadd_new] %>%
  .[, cod_catch_sum := expand*tot_cat_cod_new] %>%
  
  .[, cod_keep_base_sum := expand*tot_keep_cod_base] %>%
  .[, cod_rel_base_sum := expand*tot_rel_cod_base] %>%
  
  .[, hadd_keep_base_sum := expand*tot_keep_hadd_base] %>%
  .[, hadd_rel_base_sum := expand*tot_rel_hadd_base] %>%
  
  .[, hadd_catch_base_sum := expand*tot_cat_hadd_base] %>%
  .[, cod_catch_base_sum := expand*tot_cat_cod_base] %>%
  
  .[, ntrips_alt := expand*probA] %>%
  .[, ntrips_base := expand*prob0] 

# save predicted directed trips for use in kendall' tau analyses
# dtrip_wts<- sims %>% 
#   dplyr::mutate(period2 = paste0(mode, "_", period, "_", area, "_", state)) %>%
#   dplyr::group_by(period2) %>%
#   dplyr::summarise(ntrips_alt = sum(ntrips_alt),
#                    .groups="drop") %>%  
#   dplyr::ungroup()


# aggregate outcomes across mode, month, area, state
sims<- sims %>% 
  dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
  dplyr::group_by(mode, month, area, state) %>%
  dplyr::summarise(cv= sum(cv_sum),
                   cod_keep = sum(cod_keep_sum),
                   cod_rel = sum(cod_rel_sum),
                   hadd_keep = sum(hadd_keep_sum),
                   hadd_rel = sum(hadd_rel_sum),
                   hadd_cat = sum(hadd_catch_sum),
                   cod_cat = sum(cod_catch_sum),
                   cod_keep_base = sum(cod_keep_base_sum),
                   cod_rel_base = sum(cod_rel_base_sum),
                   hadd_keep_base = sum(hadd_keep_base_sum),
                   hadd_rel_base = sum(hadd_rel_base_sum),
                   hadd_cat_base = sum(hadd_catch_base_sum),
                   cod_cat_base = sum(cod_catch_base_sum),
                   ntrips_alt = sum(ntrips_alt),
                   ntrips_base = sum(ntrips_base), 
                   .groups="drop") %>%  
  dplyr::ungroup() %>% 
  dplyr::mutate(draw=i)


