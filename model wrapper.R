
args = commandArgs(trailingOnly=TRUE)

options(future.globals.maxSize= 1000000000)

# This is the modeling wrapper 

# Steps in the process

#2) Simulate the fishery under alternative regulations and a new catch-at-length distribution for summer flounder
# a) Create new catch-at-length/catch-per-trip distributions for summer flounder based on population numbers at length. 
# a) Calcualte angler welfare/fishing effort changes and changes in catch
# Modeling wrapper test
#profvis::profvis({
#load needed packages and install if not currently installed.
pkgs_to_use <- c("tidyr",  "magrittr", "tidyverse", "reshape2","splitstackshape",
                 "doBy","WriteXLS","Rcpp", "ggplot2","dplyr", "rlist","fitdistrplus",
                 "MASS",  "psych", "rgl","copula", "VineCopula","scales","univariateML",
                 "logspline","readr","data.table","conflicted", "readxl", "writexl", 
                 "plyr" , "furrr", "profvis", "future", "Hmisc", "SDMTools", "tictoc", "rpudplus")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("count", "dplyr")
options(scipen = 100, digits = 5)


###Input data### 
#For the calibration model:
#1)Directed trips for summer flounder and black sea bass by state, mode, and bi-monthly period. 
#This file also contains the baseline year regulations by species
#2)Observed catch (10,000) draws of catch per state and month
#3)Utility model parameters are hard-coded
#4)Trip costs by state and mode, inflation adjusted to the baseline year



n_drawz<-1000
n_draws<-1000
ndraws<-1000

n_catch_draws<-30

# Run the calibration files


#Directed trips and regulations by period
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0)


# Catch
# Here input  catch-per-trip in 2020 by period- so 10,000 draws of catch of cod and hadd per period
catch_data_all = data.frame(read_csv("calibration catch per trip.csv", show_col_types = FALSE))


# Here input  catch-per-trip projections that are based on 2020
#catch_data_all <-data.frame(read_csv("simulated_ind_catch.csv ", show_col_types = FALSE))


# For the baseline (no correlation) scenario, use catch-per-trip distributions from the baseline year (observed_catch_2020_SS_MM_md.csv)
# For all scenarios, use the adjusted catch-at-length distribution and no stock adjustments


# Below is a code that finds p* values which lead to projected harvest approximating actual harvest
# Don't need to run this every time

#source("find pstar values.R")

#once the p_star values are found, run the calibrations and save the output files 


p_star_cod_variable<- 0.915
p_star_hadd_variable<- 0.45



calibration_data_table_base<-list()
cost_files_all_base<-list()
keep_rel_pairz<-list()
keep_rel_pairz_month<-list()

# # Start the clock!
# ptm <- proc.time()
# 
# source("calibration.R")
# 
# # Stop the clock
# proc.time() - ptm




###Run the calibration
for (x in 1:100){
source("calibration.R")

pds_new_all$draw<-x
calibration_data_table_base[[x]] <- pds_new_all

costs_new_all$draw<-x
cost_files_all_base[[x]] <- costs_new_all

keep_rel_pairz[[x]] <- keep_rel_pairs

keep_rel_pairz_month[[x]] <- keep_rel_pairs_month_all

}

period_to_month<-period_vec %>% 
  dplyr::distinct(period2, month, .keep_all = TRUE)

###
#save calibration output
calibration_data_all= list.stack(calibration_data_table_base, fill=TRUE)
calibration_data_all[is.na(calibration_data_all)] = 0
calibration_data_all<-calibration_data_all %>% 
  dplyr::mutate(period2=period) %>% 
  dplyr::left_join(period_to_month, by="period2")
saveRDS(calibration_data_all, file = "calibration_data_all.rds") 

for (x in 1:100){
cost_filez_all= cost_files_all_base[[x]]
cost_filez_all[is.na(cost_filez_all)] = 0
saveRDS(cost_filez_all, file = paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/cost_files/cost_files_all_draw_",x,".rds")) 
}

keep_rel_pairz_all= list.stack(keep_rel_pairz, fill=TRUE)
saveRDS(keep_rel_pairz_all, file = "k_tau_values_calibration.rds") 

keep_rel_pairz_month_all= list.stack(keep_rel_pairz_month, fill=TRUE)
saveRDS(keep_rel_pairz_month_all, file = "k_tau_values_month_calibration.rds") 


###

# ########################################
# #Projection evaluating baseline welfare. Do this by setting the bag limits = 0 
# catch_data_all = data.frame(read_csv("calibration catch per trip.csv", show_col_types = FALSE))
# 
# # Start the clock!
# ptm <- proc.time()
# 
# 
# 
# 
# predictions<-list()
# 
# for (x in 1:100){
#   
# source("projection - baseline welfare.R")
# 
# #Metrics at the choice occasion level
# cv_i<- weighted.mean(trip_level_output$change_CS, trip_level_output$expand)
# cod_keep_i<- weighted.mean(trip_level_output$tot_keep_cod, trip_level_output$expand)
# hadd_keep_i<- weighted.mean(trip_level_output$tot_keep_hadd, trip_level_output$expand)
# cod_rel_i<- weighted.mean(trip_level_output$tot_rel_cod, trip_level_output$expand)
# hadd_rel_i<- weighted.mean(trip_level_output$tot_rel_hadd, trip_level_output$expand)
# 
# 
# trip_level_output <- trip_level_output %>%
#   as.data.table() %>%
#   .[, cv_sum := expand*change_CS] %>%
#   .[, cod_keep_sum := expand*tot_keep_cod] %>%
#   .[, hadd_keep_sum := expand*tot_keep_hadd] %>%
#   .[, cod_rel_sum := expand*tot_rel_cod] %>%
#   .[, hadd_rel_sum := expand*tot_rel_hadd] %>%
#   .[, ntrips_alt := expand*probA] 
# 
# trip_level_output <- trip_level_output %>%
#   mutate_if(is.numeric, replace_na, replace = 0)    
# 
# 
# #Metrics a coast level 
# cv_sum<- sum(trip_level_output$cv_sum)
# cod_keep_sum<- sum(trip_level_output$cod_keep_sum)
# hadd_keep_sum<- sum(trip_level_output$hadd_keep_sum)
# cod_rel_sum<- sum(trip_level_output$cod_rel_sum)
# hadd_rel_sum<- sum(trip_level_output$hadd_rel_sum)
# ntrips_sum<-sum(trip_level_output$ntrips_alt)
# 
# n_choice_occasions_sum<-sum(calibration_data_table_base[[x]]$n_choice_occasions)
# ntrips_sum_baseline<-sum(calibration_data_table_base[[x]]$estimated_trips)
# cod_keep_sum_baseline<- sum(calibration_data_table_base[[x]]$tot_keep_cod)
# hadd_keep_sum_baseline<- sum(calibration_data_table_base[[x]]$tot_keep_hadd)
# cod_rel_sum_baseline<- sum(calibration_data_table_base[[x]]$tot_rel_cod)
# hadd_rel_sum_baseline<- sum(calibration_data_table_base[[x]]$tot_rel_hadd)
# 
# cod_keep_i_baseline<- weighted.mean(calibration_data_table_base[[x]]$tot_keep_cod, calibration_data_table_base[[x]]$n_choice_occasions)
# hadd_keep_i_baseline<- weighted.mean(calibration_data_table_base[[x]]$tot_keep_hadd, calibration_data_table_base[[x]]$n_choice_occasions)
# cod_rel_i_baseline<- weighted.mean(calibration_data_table_base[[x]]$tot_rel_cod, calibration_data_table_base[[x]]$n_choice_occasions)
# hadd_rel_i_baseline<- weighted.mean(calibration_data_table_base[[x]]$tot_rel_hadd, calibration_data_table_base[[x]]$n_choice_occasions)
# 
# 
# predictions[[x]] <- as.data.frame(
#   cbind(
#     cv_i,
#     cod_keep_i,
#     hadd_keep_i,
#     cod_rel_i,
#     hadd_rel_i,
#     
#     cv_sum,
#     cod_keep_sum,
#     hadd_keep_sum,
#     cod_rel_sum,
#     hadd_rel_sum,
#     ntrips_sum,
#     
#     n_choice_occasions_sum, 
#     
#     cod_keep_i_baseline, 
#     hadd_keep_i_baseline, 
#     cod_rel_i_baseline, 
#     hadd_rel_i_baseline, 
#     
#     cod_keep_sum_baseline, 
#     hadd_keep_sum_baseline, 
#     cod_rel_sum_baseline, 
#     hadd_rel_sum_baseline, 
#     ntrips_sum_baseline 
#     )
#   
#   
# )
# 
# predictions[[x]]$draw<-x
# 
# }
# 
# predictions2= list.stack(predictions, fill=TRUE)
# predictions2[is.na(predictions2)] = 0
# 
# write_xlsx(predictions2,"baseline_welfare.xlsx") 
# 
# 
# # Stop the clock
# proc.time() - ptm
# 
# predictions2$group<-1
# format(predictions2$cv_sum, big.mark=",", scientific=FALSE)
# 
# ggplot(predictions2, aes(x=group, y=cv_sum)) + 
#   geom_violin(trim=FALSE) + geom_boxplot( width=0.1) +geom_hline(yintercept=median(predictions2$cv_sum), color="red") 
# 
# 
# trips_base<- predictions2 %>% 
#   dplyr::select(draw, ntrips_sum_baseline) %>% 
#   dplyr::rename(trips=ntrips_sum_baseline) %>% 
#   dplyr::mutate(scenario="baseline")
# 
# trips_alt<- predictions2 %>% 
#   dplyr::select(draw, ntrips_sum) %>% 
#   dplyr::rename(trips=ntrips_sum) %>% 
#   dplyr::mutate(scenario="no harvest")
# 
# trips<- rbind(trips_base, trips_alt)
# 
# ggplot(predictions2, aes(x=group, y=ntrips_sum)) + 
#   geom_violin(trim=FALSE) + geom_boxplot( width=0.1) +geom_hline(yintercept=median(predictions2$ntrips_sum), color="red") 
# 
# ggplot(trips, aes(x=scenario, y=trips)) + 
#   geom_violin(trim=FALSE) + geom_boxplot( width=0.1) +geom_hline(yintercept=median(predictions2$ntrips_sum_baseline), color="red") 
# #################################


########################################
#Projection evaluating baseline welfare from closing cod only. Do this by setting the bag limits for cod = 0 
catch_data_all = data.frame(read_csv("calibration catch per trip.csv", show_col_types = FALSE))

# Start the clock!
ptm <- proc.time()




predictions<-list()

for (x in 1:100){
  
  source("projection - baseline welfare - close cod.R")
  
  #Metrics at the choice occasion level
  cv_i<- weighted.mean(trip_level_output$change_CS, trip_level_output$expand)
  cod_keep_i<- weighted.mean(trip_level_output$tot_keep_cod, trip_level_output$expand)
  hadd_keep_i<- weighted.mean(trip_level_output$tot_keep_hadd, trip_level_output$expand)
  cod_rel_i<- weighted.mean(trip_level_output$tot_rel_cod, trip_level_output$expand)
  hadd_rel_i<- weighted.mean(trip_level_output$tot_rel_hadd, trip_level_output$expand)
  
  
  trip_level_output <- trip_level_output %>%
    as.data.table() %>%
    .[, cv_sum := expand*change_CS] %>%
    .[, cod_keep_sum := expand*tot_keep_cod] %>%
    .[, hadd_keep_sum := expand*tot_keep_hadd] %>%
    .[, cod_rel_sum := expand*tot_rel_cod] %>%
    .[, hadd_rel_sum := expand*tot_rel_hadd] %>%
    .[, ntrips_alt := expand*probA] 
  
  trip_level_output <- trip_level_output %>%
    mutate_if(is.numeric, replace_na, replace = 0)    
  
  
  #Metrics a coast level 
  cv_sum<- sum(trip_level_output$cv_sum)
  cod_keep_sum<- sum(trip_level_output$cod_keep_sum)
  hadd_keep_sum<- sum(trip_level_output$hadd_keep_sum)
  cod_rel_sum<- sum(trip_level_output$cod_rel_sum)
  hadd_rel_sum<- sum(trip_level_output$hadd_rel_sum)
  ntrips_sum<-sum(trip_level_output$ntrips_alt)
  
  n_choice_occasions_sum<-sum(calibration_data_table_base[[x]]$n_choice_occasions)
  ntrips_sum_baseline<-sum(calibration_data_table_base[[x]]$estimated_trips)
  cod_keep_sum_baseline<- sum(calibration_data_table_base[[x]]$tot_keep_cod)
  hadd_keep_sum_baseline<- sum(calibration_data_table_base[[x]]$tot_keep_hadd)
  cod_rel_sum_baseline<- sum(calibration_data_table_base[[x]]$tot_rel_cod)
  hadd_rel_sum_baseline<- sum(calibration_data_table_base[[x]]$tot_rel_hadd)
  
  cod_keep_i_baseline<- weighted.mean(calibration_data_table_base[[x]]$tot_keep_cod, calibration_data_table_base[[x]]$n_choice_occasions)
  hadd_keep_i_baseline<- weighted.mean(calibration_data_table_base[[x]]$tot_keep_hadd, calibration_data_table_base[[x]]$n_choice_occasions)
  cod_rel_i_baseline<- weighted.mean(calibration_data_table_base[[x]]$tot_rel_cod, calibration_data_table_base[[x]]$n_choice_occasions)
  hadd_rel_i_baseline<- weighted.mean(calibration_data_table_base[[x]]$tot_rel_hadd, calibration_data_table_base[[x]]$n_choice_occasions)
  
  
  predictions[[x]] <- as.data.frame(
    cbind(
      cv_i,
      cod_keep_i,
      hadd_keep_i,
      cod_rel_i,
      hadd_rel_i,
      
      cv_sum,
      cod_keep_sum,
      hadd_keep_sum,
      cod_rel_sum,
      hadd_rel_sum,
      ntrips_sum,
      
      n_choice_occasions_sum, 
      
      cod_keep_i_baseline, 
      hadd_keep_i_baseline, 
      cod_rel_i_baseline, 
      hadd_rel_i_baseline, 
      
      cod_keep_sum_baseline, 
      hadd_keep_sum_baseline, 
      cod_rel_sum_baseline, 
      hadd_rel_sum_baseline, 
      ntrips_sum_baseline 
    )
    
    
  )
  
  predictions[[x]]$draw<-x
  
}

predictions2= list.stack(predictions, fill=TRUE)
predictions2[is.na(predictions2)] = 0

write_xlsx(predictions2,"baseline_welfare_close_cod.xlsx") 


# Stop the clock
proc.time() - ptm

predictions2$group<-1
format(predictions2$cv_sum, big.mark=",", scientific=FALSE)

ggplot(predictions2, aes(x=group, y=cv_sum)) + 
  geom_violin(trim=FALSE) + geom_boxplot( width=0.1) +geom_hline(yintercept=median(predictions2$cv_sum), color="red") 


trips_base<- predictions2 %>% 
  dplyr::select(draw, ntrips_sum_baseline) %>% 
  dplyr::rename(trips=ntrips_sum_baseline) %>% 
  dplyr::mutate(scenario="baseline")

trips_alt<- predictions2 %>% 
  dplyr::select(draw, ntrips_sum) %>% 
  dplyr::rename(trips=ntrips_sum) %>% 
  dplyr::mutate(scenario="no harvest")

trips<- rbind(trips_base, trips_alt)

ggplot(predictions2, aes(x=group, y=ntrips_sum)) + 
  geom_violin(trim=FALSE) + geom_boxplot( width=0.1) +geom_hline(yintercept=median(predictions2$ntrips_sum), color="red") 

ggplot(trips, aes(x=scenario, y=trips)) + 
  geom_violin(trim=FALSE) + geom_boxplot( width=0.1) +geom_hline(yintercept=median(predictions2$ntrips_sum_baseline), color="red") 
#################################



####Decadal projections

##################
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0)


#choose which copula data to use

#cop_name = _gumbel, _frank, clayton
cop_name<-"_clayton"

#independat or correlations = corr or ind
ind_or_corr<-"ind"

# Start the clock!
ptm <- proc.time()

source("projection function2.R")

#save the data 
write_xlsx(predictions_all,paste0("predictions_1_3", cop_name, "_", ind_or_corr, ".xlsx")) 


# Stop the clock
proc.time() - ptm
##################

##################
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0)


#choose which copula data to use

#cop_name = _gumbel, _frank, clayton
cop_name<-"_clayton"

#independat or correlations = corr or ind
ind_or_corr<-"corr"

# Start the clock!
ptm <- proc.time()

source("projection function2.R")

#save the data 
write_xlsx(predictions_all,paste0("predictions_1_3", cop_name, "_", ind_or_corr, ".xlsx")) 


# Stop the clock
proc.time() - ptm
##################

##################
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0)


#choose which copula data to use

#cop_name = _gumbel, _frank, clayton
cop_name<-"_gumbel"

#independat or correlations = corr or ind
ind_or_corr<-"ind"

# Start the clock!
ptm <- proc.time()

source("projection function2.R")

#save the data 
write_xlsx(predictions_all,paste0("predictions_1_3", cop_name, "_", ind_or_corr, ".xlsx")) 


# Stop the clock
proc.time() - ptm
##################

##################
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0)


#choose which copula data to use

#cop_name = _gumbel, _frank, clayton
cop_name<-"_gumbel"

#independat or correlations = corr or ind
ind_or_corr<-"corr"

# Start the clock!
ptm <- proc.time()

source("projection function2.R")

#save the data 
write_xlsx(predictions_all,paste0("predictions_1_3", cop_name, "_", ind_or_corr, ".xlsx")) 


# Stop the clock
proc.time() - ptm
##################


##################
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0)


#choose which copula data to use

#cop_name = _gumbel, _frank, clayton
cop_name<-"_frank"

#independat or correlations = corr or ind
ind_or_corr<-"ind"

# Start the clock!
ptm <- proc.time()

source("projection function2.R")

#save the data 
write_xlsx(predictions_all,paste0("predictions_1_3", cop_name, "_", ind_or_corr, ".xlsx")) 


# Stop the clock
proc.time() - ptm
##################


##################
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0)


#choose which copula data to use

#cop_name = _gumbel, _frank, clayton
cop_name<-"_frank"

#independat or correlations = corr or ind
ind_or_corr<-"corr"

# Start the clock!
ptm <- proc.time()

source("projection function2.R")

#save the data 
write_xlsx(predictions_all,paste0("predictions_1_3", cop_name, "_", ind_or_corr, ".xlsx")) 


# Stop the clock
proc.time() - ptm
##################




##Run the projection function 
# Start the clock!
ptm <- proc.time()

directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0)


catch_data_all <-readr::read_csv(file.path(here::here("projection catch per trip.csv")),  show_col_types = FALSE) 

predictions<-list()
predictions3<-list()
predictions5<-list()
ktau_values<-list()

p_star_cod <- p_star_cod_variable
p_star_hadd<-p_star_hadd_variable

for (x in 1:100){

  
 source("projection function.R")

 # prediction_output_by_period1<- trip_level_output
  # trip_level_output4 <- trip_level_output4 %>%  separate(period2, c("period", "mode"), "_")
  
  
  for (d in 1:8){
  
    d<-1
    trip_level_output5<-trip_level_output4 %>% 
      dplyr::filter(decade==d) %>% 
      dplyr::mutate(data_type=paste0(as.character(corr_type),"", as.character(copula)))
    
    data_types<-unique(trip_level_output5$data_type)
    data_types<-as.data.table(data_types)
    data_types$data_type_num <- 1:nrow(data_types)
    data_types<- data_types %>% dplyr::rename(data_type=data_types)
    
    
    trip_level_output5<- trip_level_output5 %>% 
      dplyr::left_join(data_types, by="data_type")
    
    
    for (k in unique(trip_level_output5$data_type_num)){
      
      k<-1
      trip_level_output6<-trip_level_output5 %>% 
        dplyr::filter(data_type_num==k) 
      
      
      #Metrics at the choice occasion level
      cv_i<- weighted.mean(trip_level_output6$change_CS, trip_level_output6$expand)
      cod_keep_i<- weighted.mean(trip_level_output6$tot_keep_cod, trip_level_output6$expand)
      hadd_keep_i<- weighted.mean(trip_level_output6$tot_keep_hadd, trip_level_output6$expand)
      cod_rel_i<- weighted.mean(trip_level_output6$tot_rel_cod, trip_level_output6$expand)
      hadd_rel_i<- weighted.mean(trip_level_output6$tot_rel_hadd, trip_level_output6$expand)
      
      
      trip_level_output6 <- trip_level_output6 %>%
        as.data.table() %>%
        .[, cv_sum := expand*change_CS] %>%
        .[, cod_keep_sum := expand*tot_keep_cod] %>%
        .[, hadd_keep_sum := expand*tot_keep_hadd] %>%
        .[, cod_rel_sum := expand*tot_rel_cod] %>%
        .[, hadd_rel_sum := expand*tot_rel_hadd] %>%
        .[, ntrips_alt := expand*probA] 
      
      trip_level_output6 <- trip_level_output6 %>%
        mutate_if(is.numeric, replace_na, replace = 0)    
      
      
      #Metrics a coast level 
      cv_sum<- sum(trip_level_output6$cv_sum)
      cod_keep_sum<- sum(trip_level_output6$cod_keep_sum)
      hadd_keep_sum<- sum(trip_level_output6$hadd_keep_sum)
      cod_rel_sum<- sum(trip_level_output6$cod_rel_sum)
      hadd_rel_sum<- sum(trip_level_output6$hadd_rel_sum)
      ntrips_sum<-sum(trip_level_output6$ntrips_alt)
      n_choice_occasions_sum<-sum(calibration_data$n_choice_occasions)
      
      ##add k_tau values
      k_tauz_check<-  list.stack(ktau_values, fill=TRUE)

      k_tauz<-k_tau_values_all %>%
        dplyr::filter(decade==d) %>% 
        dplyr::mutate(data_type=paste0(as.character(corr_type),"", as.character(copula))) %>% 
        dplyr::left_join(data_types, by="data_type") %>% 
        dplyr::filter(data_type_num==k) %>% 
        dplyr::select(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p, data_type_num)
      
      
      
      predictions0<- as.data.frame(cbind(
        cv_i,
        cod_keep_i,
        hadd_keep_i,
        cod_rel_i,
        hadd_rel_i,
        
        cv_sum,
        cod_keep_sum,
        hadd_keep_sum,
        cod_rel_sum,
        hadd_rel_sum,
        ntrips_sum,
        n_choice_occasions_sum))
      
      predictions[[k]]<- predictions0 %>% 
        dplyr::mutate(data_type_num=k) %>% 
        dplyr::left_join(data_types, by="data_type_num") %>% 
        dplyr::left_join(k_tauz, by="data_type_num")
      
      
    }
    predictions2= list.stack(predictions, fill=TRUE)
    predictions2[is.na(predictions2)] = 0
    
    predictions3[[d]]<-predictions2 %>% 
      dplyr::mutate(decade=d)
    
  }
  predictions4= list.stack(predictions3, fill=TRUE)
  predictions4[is.na(predictions4)] = 0
  
  predictions4<-predictions4 %>% 
    dplyr::mutate(draw=x)
  
  predictions5[[x]]<-predictions4 
  

  #predictions[[x]]$decade<-d
  
  # predictions[[x]]<-list.append(predictions[[x]],draw=x)
  # predictions[[x]]<-list.append(predictions[[x]],decade=d)
  
  #predictions[[x]]$draw<-x
  #predictions[[x]]$decade<-d
  
}

predictions6= list.stack(predictions5, fill=TRUE)


#k_tau_ouput_full<-list.stack(ktau_values, fill=TRUE)


#write_xlsx(predictions6,"predictions_11_14.xlsx") 
write_xlsx(predictions6,"predictions_12_30.xlsx") 


# Stop the clock
proc.time() - ptm

calib_data <- readRDS("calibration_data_all.rds")
calib_data<-calib_data %>% 
  dplyr::group_by(draw) %>% 
dplyr::summarise(estimated_trips=sum(estimated_trips),
                 tot_cod_catch= sum(tot_cod_catch), 
                 tot_keep_cod = sum(tot_keep_cod), 
                 tot_rel_cod = sum(tot_rel_cod), 
                 
                 tot_hadd_catch = sum(tot_hadd_catch), 
                 tot_keep_hadd = sum(tot_keep_hadd),
                 tot_rel_hadd = sum(tot_rel_hadd),
                 
                 n_choice_occasions = sum(n_choice_occasions)) %>% 
  dplyr::mutate(cod_keep_i=tot_keep_cod/n_choice_occasions, 
                hadd_keep_i=tot_keep_hadd/n_choice_occasions,
                cod_keep_trip=tot_keep_cod/estimated_trips, 
                hadd_keep_trip=tot_keep_hadd/estimated_trips)



predictions_data<- read_excel("predictions_12_30.xlsx") 

predictions_data<-predictions_data %>% 
  dplyr::filter(data_type=="corr_clayton") %>% 
  dplyr::filter(data_type!="ind_clayton")

predictions_data<-predictions_data %>% 
  dplyr::mutate(decade=as.character(decade))

ggplot(predictions_ind_gumbel, aes(x=decade, y=cv_i)) + 
  geom_violin() + geom_boxplot(width=0.1)+
  scale_y_continuous(label=comma, breaks=seq(-70,-30,5))

ggplot(predictions_ind_gumbel, aes(x=decade, y=hadd_keep_i)) + 
  geom_violin() + geom_boxplot(width=0.1)+
  scale_y_continuous(label=comma)

ggplot(predictions_data, aes(x=decade, y=k_tau_keep_est)) + 
  geom_violin() + geom_boxplot(width=0.1)+
  scale_y_continuous("Correlation in keep-per-trip of cod and haddock (Kendall's tau-b)", label=comma)

, breaks=seq(-70,-30,5))

  geom_violin(trim=FALSE) + geom_boxplot(width=0.1) +geom_hline(yintercept=0, color="red") 


