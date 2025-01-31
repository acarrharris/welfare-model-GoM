
args = commandArgs(trailingOnly=TRUE)
options(future.globals.maxSize= 1000000000)

# This is the modeling wrapper 


# Load needed packages and install if not currently installed.
pkgs_to_use <- c("tidyr",  "magrittr", "tidyverse", "reshape2","splitstackshape",
                 "doBy","WriteXLS","Rcpp", "ggplot2","dplyr", "rlist","fitdistrplus",
                 "MASS",  "psych", "rgl","copula", "VineCopula","scales","univariateML",
                 "logspline","readr","data.table","conflicted", "readxl", "writexl", 
                 "plyr" , "furrr", "profvis", "future", "Hmisc", "tictoc","purrr" ,
                 "rmdformats", "prettydoc", "hrbrthemes", "tint", "tufte", "rstatix", "ggpubr", 
                 "future.apply", "DescTools", "listenv")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("count", "dplyr")
options(scipen = 10000, digits = 10)


###Key input data### 
#For the calibration model:
#1)Directed trips for cod and haddock by state, mode, area, and bi-monthly period. 
#This file also contains the baseline year regulations by species
  #C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\directed trips GoM 2020.do

#2)Catch in calibration year 2021: 3,000 draws (100 choice occasions * 30 draws of catch) of cod/haddock catch 
#  per state, month, area, mode, and bi-monthly period.
  #C:\Users\andrew.carr-harris\Desktop\Git\welfare-model-GoM\calibration catch by period.do


input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

n_drawz<-100
n_draws<-100
ndraws<-100

n_catch_draws<-30
p_star_values_i<-list()
p_star_values_i_y<-list()

#yrz<- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
yrz<- c(2021)

for (y in yrz){
for (i in 1:100){
    catch_data_all_split <- data.frame(read_csv(paste0(input_data_cd, "calib_catch_yr", y, "_draw", i, ".csv"), show_col_types = FALSE))
    
  
  #Directed trips and regulations by period
  directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE)) %>% 
    dplyr::mutate(dtrip=round(dtrip)) %>% dplyr::filter(dtrip!=0) %>% dplyr::filter(year==y)
  
  directed_trips_p <- directed_trips %>% 
    mutate(period2 = as.character(period2)) %>% mutate(n_trips = floor(dtrip), n_draws = n_drawz) 
  
  period_vec <- directed_trips_p %>%dplyr::select(period2, n_draws, month) %>%  uncount(n_draws) 
  
  period_vec2 <- directed_trips %>% dplyr::select(period2, month, area, mode, st) %>% dplyr::rename(period=period2)
  
  period_vec4 <- directed_trips %>%  dplyr::select(period2, month, area, mode, st) 
  
  period_vec<-distinct(period_vec,.keep_all = TRUE)
  
  regs <- directed_trips_p %>% 
    dplyr::select(period2, cod_bag, cod_min, hadd_bag, hadd_min, dtrip)
  
  regs_check <- directed_trips_p %>%   dplyr::select(period2, dtrip)
  
  # Run the p-star routine and save the output
  source(paste0(input_code_cd,"find p-star values 2010-2020.R"))
  
  p_starz<-cbind(p_star_cod_variable, p_star_hadd_variable, cod_harvest_perc_diff, cod_harvest_diff, hadd_harvest_perc_diff, hadd_harvest_diff)
  
  p_starz<-p_starz %>% 
    as.data.frame() %>% dplyr::mutate(draw=i, year=y)
  
  p_star_values_i[[i]] <-p_starz
  
  }
  
  p_star_values_all= list.stack(p_star_values_i, fill=TRUE)
  p_star_values_i_y[[y]]= p_star_values_all
  
}

p_star_values_all_y= list.stack(p_star_values_i_y, fill=TRUE)
saveRDS(p_star_values_all_y, file = paste0(input_data_cd, "p_star_values_2010_2021.rds")) 




# Once the p_star values are found, re-run the calibration and save the output including correlations 
calibration_data_i<-list()
costs_new_all_i<-list()
all_ktaus_all_i<-list()

calibration_data_i_y<-list()
costs_new_all_i_y<-list()
all_ktaus_all_i_y<-list()

yrz<- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
yrz<- c(2021)

for(y in yrz){
  
  for (i in 1:2){
    
  directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE)) %>% 
    dplyr::mutate(dtrip=round(dtrip)) %>% dplyr::filter(dtrip!=0) %>% dplyr::filter(year==y)
  
  dtrips1<-directed_trips %>%   dplyr::select(period2, dtrip, month)
  
  p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2010_2021.rds")) %>%  dplyr::filter(year==y, draw==i)
  
  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable

  catch_data_all = data.frame(read_csv(paste0(input_data_cd, "calib_catch_yr", y, "_draw", i, ".csv"), show_col_types = FALSE)) %>% 
      dplyr::filter(year==y) 
    
  source(paste0(input_code_cd,"calibration2 2010-2021.R"))

  # save the output  
  saveRDS(aggregate_trip_data, file = paste0(output_data_cd, "calibration_data_", y,"draw", i, ".rds")) 
  saveRDS(costs_new, file = paste0(output_data_cd, "costs_data_", y,"draw", i, ".rds")) 
  saveRDS(all_ktaus, file = paste0(output_data_cd, "ktau_data_", y,"draw", i, ".rds")) 
  
  }
  
}



########################################
####Decadal projections
directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE)) %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0) %>% dplyr::filter(year==2021) %>% dplyr::select(-year)

dtrips1<-directed_trips %>% dplyr::select(period2, dtrip, month)

directed_trips_p <- directed_trips %>% 
  mutate(period2 = as.character(period2)) %>% mutate(n_trips = floor(dtrip), n_draws = n_drawz) 

period_vec <- directed_trips_p %>%  dplyr::select(period2, n_draws, month) %>%  uncount(n_draws) 

period_vec2 <- directed_trips %>%
  dplyr::select(period2, month, area, mode, st) %>% dplyr::rename(period=period2)

period_vec4 <- directed_trips %>% dplyr::select(period2, month, area, mode, st) 

period_vec<-distinct(period_vec,  .keep_all = TRUE)

regs <- directed_trips_p %>% dplyr::select(period2,cod_bag, cod_min, hadd_bag, hadd_min, dtrip)

regs_check <- directed_trips_p %>% dplyr::select(period2, dtrip)


# Start the clock!
ptm <- proc.time()

k_taus<-list()
output<-list()

for (i in 1:2){
  
  p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2010_2021.rds")) %>% 
    dplyr::filter(year==2021, draw==i)
  
  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable
  
  catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "projection_catch_draw", i, ".csv"), show_col_types = FALSE))

  source(paste0(input_code_cd,"projection function new2.R")) 
  
  k_taus[[i]]<-all_ktaus
  output[[i]]<-sims_all
  
}

ktau_all<-list.stack(k_taus, fill=TRUE)
output_all<-list.stack(output, fill=TRUE)

# Stop the clock
proc.time() - ptm

  
  
  

########################################
####Decadal projections

# Start the clock!
ptm <- proc.time()

p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2010_2021.rds"))
p_starz<- p_starz %>% 
  dplyr::filter(year==2021)

p_star_cod_variable<- p_starz$p_star_cod_variable
p_star_hadd_variable<- p_starz$p_star_hadd_variable

p_star_cod <- p_star_cod_variable
p_star_hadd<-p_star_hadd_variable

directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0) %>% 
  dplyr::filter(year==2021) %>% 
  dplyr::select(-year)


# Set up an output file for the separately simulated within-season regulatory periods  
directed_trips_p <- directed_trips %>% #subset(directed_trips, period == p)
  mutate(period2 = as.character(period2)) %>% 
  mutate(n_trips = floor(dtrip),
         n_draws = n_drawz) 

period_vec <- directed_trips_p %>% 
  dplyr::select(period2, n_draws, month) %>% 
  uncount(n_draws) 

period_vec2 <- directed_trips %>%
  dplyr::select(period2, month, area, mode, st) %>% 
  dplyr::rename(period=period2)

period_vec4 <- directed_trips %>%
  dplyr::select(period2, month, area, mode, st) 

period_vec<-distinct(period_vec,  .keep_all = TRUE)

regs <- directed_trips_p %>% 
  dplyr::select(period2,
                cod_bag, cod_min, 
                hadd_bag, hadd_min, dtrip)

regs_check <- directed_trips_p %>% 
  dplyr::select(period2, dtrip)



catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "projection catch per trip.csv"), show_col_types = FALSE))
catch_data_all_split <- split(catch_data_all, catch_data_all$decade)
rm(catch_data_all)


{
  ######## Clayton independent  ######## 
  #choose which copula data to use. cop_name = _gumbel, _frank, clayton
  cop_name<-"_clayton"
  
  #independent or correlated = corr or ind
  ind_or_corr<-"ind"
  
  source(paste0(input_code_cd,"projection function2.R")) 
  
  write_xlsx(predictions_all, paste0(output_data_cd, "decadal_proj_", cop_name, "_", ind_or_corr, ".xlsx"))  #save the data
  write_xlsx(ktaus_all1, paste0(output_data_cd, "ktaus_", cop_name, "_", ind_or_corr, ".xlsx"))  #save the data
  
  
  
  ######## Clayton correlated  ######## 
  cop_name<-"_clayton"
  
  ind_or_corr<-"corr"
  
  source(paste0(input_code_cd,"projection function2.R")) 
  
  write_xlsx(predictions_all, paste0(output_data_cd, "decadal_proj_", cop_name, "_", ind_or_corr, ".xlsx")) #save the data 
  write_xlsx(ktaus_all1, paste0(output_data_cd, "ktaus_", cop_name, "_", ind_or_corr, ".xlsx"))  #save the data
  
  
  ######## Gumbel independent  ######## 
  cop_name<-"_gumbel"
  
  ind_or_corr<-"ind"
  
  source(paste0(input_code_cd,"projection function2.R")) 
  
  write_xlsx(predictions_all, paste0(output_data_cd, "decadal_proj_", cop_name, "_", ind_or_corr, ".xlsx")) #save the data 
  write_xlsx(ktaus_all1, paste0(output_data_cd, "ktaus_", cop_name, "_", ind_or_corr, ".xlsx"))  #save the data
  
  
  ######## Gumbel correlated  ######## 
  cop_name<-"_gumbel"
  
  ind_or_corr<-"corr"
  
  source(paste0(input_code_cd,"projection function2.R")) 
  
  write_xlsx(predictions_all, paste0(output_data_cd, "decadal_proj_", cop_name, "_", ind_or_corr, ".xlsx")) #save the data 
  write_xlsx(ktaus_all1, paste0(output_data_cd, "ktaus_", cop_name, "_", ind_or_corr, ".xlsx"))  #save the data
  
  
  ######## Frank independent  ######## 
  cop_name<-"_frank"
  
  ind_or_corr<-"ind"
  
  source(paste0(input_code_cd,"projection function2.R")) 
  
  write_xlsx(predictions_all, paste0(output_data_cd, "decadal_proj_", cop_name, "_", ind_or_corr, ".xlsx")) #save the data 
  write_xlsx(ktaus_all1, paste0(output_data_cd, "ktaus_", cop_name, "_", ind_or_corr, ".xlsx"))  #save the data
  
  
  ######## Frank correlated  ######## 
  cop_name<-"_frank"
  
  ind_or_corr<-"corr"
  
  source(paste0(input_code_cd,"projection function2.R")) 
  
  write_xlsx(predictions_all, paste0(output_data_cd, "decadal_proj_", cop_name, "_", ind_or_corr, ".xlsx")) #save the data 
  write_xlsx(ktaus_all1, paste0(output_data_cd, "ktaus_", cop_name, "_", ind_or_corr, ".xlsx"))  #save the data
  
  
  ######## Plackett independent  ######## 
  cop_name<-"_plackett"
  
  ind_or_corr<-"ind"
  
  source(paste0(input_code_cd,"projection function2.R")) 
  
  write_xlsx(predictions_all, paste0(output_data_cd, "decadal_proj_", cop_name, "_", ind_or_corr, ".xlsx")) #save the data 
  write_xlsx(ktaus_all1, paste0(output_data_cd, "ktaus_", cop_name, "_", ind_or_corr, ".xlsx"))  #save the data
  
  
  ######## Plackett correlated  ######## 
  cop_name<-"_plackett"
  
  ind_or_corr<-"corr"
  
  source(paste0(input_code_cd,"projection function2.R")) 
  
  write_xlsx(predictions_all, paste0(output_data_cd, "decadal_proj_", cop_name, "_", ind_or_corr, ".xlsx")) #save the data 
  write_xlsx(ktaus_all1, paste0(output_data_cd, "ktaus_", cop_name, "_", ind_or_corr, ".xlsx"))  #save the data
  
  
  ######## Gaussian independent  ######## 
  cop_name<-"_guassian"
  
  ind_or_corr<-"ind"
  
  source(paste0(input_code_cd,"projection function2.R")) 
  
  write_xlsx(predictions_all, paste0(output_data_cd, "decadal_proj_", cop_name, "_", ind_or_corr, ".xlsx")) #save the data 
  write_xlsx(ktaus_all1, paste0(output_data_cd, "ktaus_", cop_name, "_", ind_or_corr, ".xlsx"))  #save the data
  
  
  ######## Gaussian correlated  ######## 
  cop_name<-"_guassian"
  
  ind_or_corr<-"corr"
  
  source(paste0(input_code_cd,"projection function2.R")) 
  
  write_xlsx(predictions_all, paste0(output_data_cd, "decadal_proj_", cop_name, "_", ind_or_corr, ".xlsx")) #save the data 
  write_xlsx(ktaus_all1, paste0(output_data_cd, "ktaus_", cop_name, "_", ind_or_corr, ".xlsx"))  #save the data
  
  
  
  ##################
  
  }

# Stop the clock
proc.time() - ptm



########################################
#Projection evaluating baseline welfare in 2020. Do this by setting the bag limits = 0

yrz<- c(2021)
for(y in yrz){
  directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE))
  directed_trips<-directed_trips %>% 
    dplyr::mutate(dtrip=round(dtrip)) %>% 
    dplyr::filter(dtrip!=0) %>% 
    dplyr::filter(year==y) %>% 
    dplyr::mutate()
  
  p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2010_2021.rds"))
  p_starz<- p_starz %>% 
    dplyr::filter(year==y)
  
  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable
  
  
  ########################################
  ###Run the calibration
  
  for (x in 1:100){
    
    catch_data_all = data.frame(read_csv(paste0(input_data_cd,"calibration catch per trip 2010_2020.csv"), show_col_types = FALSE))
    catch_data_all<-catch_data_all %>% 
      dplyr::filter(year==y) 
    
    source(paste0(input_code_cd,"calibration 2010-2020.R"))
    
    pds_new_all$draw<-x
    calibration_data_table_base <- pds_new_all
    calibration_data_table_base[is.na(calibration_data_table_base)] = 0
    
    period_to_month<-period_vec %>% 
      dplyr::distinct(period2, month, .keep_all = TRUE)
    
    calibration_data_table_base<-calibration_data_table_base %>% 
      dplyr::mutate(period2=period) %>% 
      dplyr::left_join(period_to_month, by="period2")
    
    saveRDS(calibration_data_table_base, file = paste0(output_data_cd, "calibration_data_", y,"_",x,".rds")) 
    
    costs_new_all$draw<-x
    cost_files_all_base <- costs_new_all
    saveRDS(cost_files_all_base, file = paste0(output_data_cd, "cost_files_", y,"_",x,".rds")) 
    
    keep_rel_pairs_annual$draw<-x
    keep_rel_pairz <- keep_rel_pairs_annual
    saveRDS(keep_rel_pairz, file = paste0(output_data_cd, "k_tau_annual", y,"_",x,".rds")) 
    
    keep_rel_pairs_month_all$draw<-x
    keep_rel_pairz_month <- keep_rel_pairs_month_all
    saveRDS(keep_rel_pairz_month, file = paste0(output_data_cd, "k_tau_month", y,"_",x,".rds")) 
    
  }
  
  
}






# p_starz <- readRDS("p_stars_all_years.rds")
# p_starz<- p_starz %>% 
#   dplyr::filter(year==2020)
# 
# p_star_cod_variable<- p_starz$p_star_cod_variable
# p_star_hadd_variable<- p_starz$p_star_hadd_variable
# 
# catch_data_all = data.frame(read_csv("calibration catch per trip 2010_2020_10k_draws.csv", show_col_types = FALSE))
# catch_data_all<-catch_data_all %>% 
#   dplyr::filter(year==2020)
# 
# calibration_data_all <- readRDS("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/calibration_data/calibration_data_all_2020.rds")
# 
# directed_trips<-data.frame(read_csv("directed trips and regulations 2010_2020.csv", show_col_types = FALSE))
# directed_trips<-directed_trips %>% 
#   dplyr::mutate(dtrip=round(dtrip)) %>% 
#   dplyr::filter(year==2020) %>% 
#   dplyr::filter(dtrip!=0) %>% 
#   dplyr::select(-year)
# 
# predictions<-list()
# 
# #{
#   # Start the clock!
#   ptm <- proc.time()
#   
#   
#   for (x in 1:100){
#     
#     source("projection - baseline welfare.R")
#     
#     #Metrics at the choice occasion level
#     cv_i<- weighted.mean(trip_level_output$change_CS, trip_level_output$expand)
#     cod_keep_i<- weighted.mean(trip_level_output$tot_keep_cod, trip_level_output$expand)
#     hadd_keep_i<- weighted.mean(trip_level_output$tot_keep_hadd, trip_level_output$expand)
#     cod_rel_i<- weighted.mean(trip_level_output$tot_rel_cod, trip_level_output$expand)
#     hadd_rel_i<- weighted.mean(trip_level_output$tot_rel_hadd, trip_level_output$expand)
#     
#     
#     trip_level_output <- trip_level_output %>%
#       as.data.table() %>%
#       .[, cv_sum := expand*change_CS] %>%
#       .[, cod_keep_sum := expand*tot_keep_cod] %>%
#       .[, hadd_keep_sum := expand*tot_keep_hadd] %>%
#       .[, cod_rel_sum := expand*tot_rel_cod] %>%
#       .[, hadd_rel_sum := expand*tot_rel_hadd] %>%
#       .[, ntrips_alt := expand*probA]
#     
#     trip_level_output <- trip_level_output %>%
#       mutate_if(is.numeric, replace_na, replace = 0)
#     
#     
#     #Metrics a coast level
#     cv_sum<- sum(trip_level_output$cv_sum)
#     cod_keep_sum<- sum(trip_level_output$cod_keep_sum)
#     hadd_keep_sum<- sum(trip_level_output$hadd_keep_sum)
#     cod_rel_sum<- sum(trip_level_output$cod_rel_sum)
#     hadd_rel_sum<- sum(trip_level_output$hadd_rel_sum)
#     ntrips_sum<-sum(trip_level_output$ntrips_alt)
#     
#     
#     calibration_data <- calibration_data_all %>% 
#       dplyr::filter(draw==x)
#     
#     n_choice_occasions_sum<-sum(calibration_data$n_choice_occasions)
#     ntrips_sum_baseline<-sum(calibration_data$estimated_trips)
#     cod_keep_sum_baseline<- sum(calibration_data$tot_keep_cod)
#     hadd_keep_sum_baseline<- sum(calibration_data$tot_keep_hadd)
#     cod_rel_sum_baseline<- sum(calibration_data$tot_rel_cod)
#     hadd_rel_sum_baseline<- sum(calibration_data$tot_rel_hadd)
#     
#     cod_keep_i_baseline<- weighted.mean(calibration_data$tot_keep_cod, calibration_data$n_choice_occasions)
#     hadd_keep_i_baseline<- weighted.mean(calibration_data$tot_keep_hadd, calibration_data$n_choice_occasions)
#     cod_rel_i_baseline<- weighted.mean(calibration_data$tot_rel_cod, calibration_data$n_choice_occasions)
#     hadd_rel_i_baseline<- weighted.mean(calibration_data$tot_rel_hadd, calibration_data$n_choice_occasions)
#     
#     
#     predictions[[x]] <- as.data.frame(
#       cbind(
#         cv_i,
#         cod_keep_i,
#         hadd_keep_i,
#         cod_rel_i,
#         hadd_rel_i,
#         
#         cv_sum,
#         cod_keep_sum,
#         hadd_keep_sum,
#         cod_rel_sum,
#         hadd_rel_sum,
#         ntrips_sum,
#         
#         n_choice_occasions_sum,
#         
#         cod_keep_i_baseline,
#         hadd_keep_i_baseline,
#         cod_rel_i_baseline,
#         hadd_rel_i_baseline,
#         
#         cod_keep_sum_baseline,
#         hadd_keep_sum_baseline,
#         cod_rel_sum_baseline,
#         hadd_rel_sum_baseline,
#         ntrips_sum_baseline
#       )
#       
#       
#     )
#     
#     predictions[[x]]$draw<-x
#     
#   }
# #}
# predictions2= list.stack(predictions, fill=TRUE)
# predictions2[is.na(predictions2)] = 0
# 
# write_xlsx(predictions2,"baseline_welfare.xlsx")
# 
# 
# # Stop the clock
# proc.time() - ptm
# 
# 
# ########################################
# #Projection evaluating baseline welfare from closing cod only  in 2020. Do this by setting the bag limits for cod = 0 
# p_starz <- readRDS("p_stars_all_years.rds")
# p_starz<- p_starz %>% 
#   dplyr::filter(year==2020)
# 
# p_star_cod_variable<- p_starz$p_star_cod_variable
# p_star_hadd_variable<- p_starz$p_star_hadd_variable
# 
# catch_data_all = data.frame(read_csv("calibration catch per trip 2010_2020_10k_draws.csv", show_col_types = FALSE))
# catch_data_all<-catch_data_all %>% 
#   dplyr::filter(year==2020)
# 
# calibration_data_all <- readRDS("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/calibration_data/calibration_data_all_2020.rds")
# 
# directed_trips<-data.frame(read_csv("directed trips and regulations 2010_2020.csv", show_col_types = FALSE))
# directed_trips<-directed_trips %>% 
#   dplyr::mutate(dtrip=round(dtrip)) %>% 
#   dplyr::filter(year==2020) %>% 
#   dplyr::filter(dtrip!=0) %>% 
#   dplyr::select(-year)
# 
# 
# predictions<-list()
# {
#   # Start the clock!
#   ptm <- proc.time()
#   
#   
#   for (x in 1:100){
#     
#     source("projection - baseline welfare - close cod.R")
#     
#     #Metrics at the choice occasion level
#     cv_i<- weighted.mean(trip_level_output$change_CS, trip_level_output$expand)
#     cod_keep_i<- weighted.mean(trip_level_output$tot_keep_cod, trip_level_output$expand)
#     hadd_keep_i<- weighted.mean(trip_level_output$tot_keep_hadd, trip_level_output$expand)
#     cod_rel_i<- weighted.mean(trip_level_output$tot_rel_cod, trip_level_output$expand)
#     hadd_rel_i<- weighted.mean(trip_level_output$tot_rel_hadd, trip_level_output$expand)
#     
#     
#     trip_level_output <- trip_level_output %>%
#       as.data.table() %>%
#       .[, cv_sum := expand*change_CS] %>%
#       .[, cod_keep_sum := expand*tot_keep_cod] %>%
#       .[, hadd_keep_sum := expand*tot_keep_hadd] %>%
#       .[, cod_rel_sum := expand*tot_rel_cod] %>%
#       .[, hadd_rel_sum := expand*tot_rel_hadd] %>%
#       .[, ntrips_alt := expand*probA] 
#     
#     trip_level_output <- trip_level_output %>%
#       mutate_if(is.numeric, replace_na, replace = 0)    
#     
#     
#     #Metrics a coast level 
#     cv_sum<- sum(trip_level_output$cv_sum)
#     cod_keep_sum<- sum(trip_level_output$cod_keep_sum)
#     hadd_keep_sum<- sum(trip_level_output$hadd_keep_sum)
#     cod_rel_sum<- sum(trip_level_output$cod_rel_sum)
#     hadd_rel_sum<- sum(trip_level_output$hadd_rel_sum)
#     ntrips_sum<-sum(trip_level_output$ntrips_alt)
#     
#     n_choice_occasions_sum<-sum(calibration_data$n_choice_occasions)
#     ntrips_sum_baseline<-sum(calibration_data$estimated_trips)
#     cod_keep_sum_baseline<- sum(calibration_data$tot_keep_cod)
#     hadd_keep_sum_baseline<- sum(calibration_data$tot_keep_hadd)
#     cod_rel_sum_baseline<- sum(calibration_data$tot_rel_cod)
#     hadd_rel_sum_baseline<- sum(calibration_data$tot_rel_hadd)
#     
#     cod_keep_i_baseline<- weighted.mean(calibration_data$tot_keep_cod, calibration_data$n_choice_occasions)
#     hadd_keep_i_baseline<- weighted.mean(calibration_data$tot_keep_hadd, calibration_data$n_choice_occasions)
#     cod_rel_i_baseline<- weighted.mean(calibration_data$tot_rel_cod, calibration_data$n_choice_occasions)
#     hadd_rel_i_baseline<- weighted.mean(calibration_data$tot_rel_hadd, calibration_data$n_choice_occasions)
#     
#     
#     predictions[[x]] <- as.data.frame(
#       cbind(
#         cv_i,
#         cod_keep_i,
#         hadd_keep_i,
#         cod_rel_i,
#         hadd_rel_i,
#         
#         cv_sum,
#         cod_keep_sum,
#         hadd_keep_sum,
#         cod_rel_sum,
#         hadd_rel_sum,
#         ntrips_sum,
#         
#         n_choice_occasions_sum, 
#         
#         cod_keep_i_baseline, 
#         hadd_keep_i_baseline, 
#         cod_rel_i_baseline, 
#         hadd_rel_i_baseline, 
#         
#         cod_keep_sum_baseline, 
#         hadd_keep_sum_baseline, 
#         cod_rel_sum_baseline, 
#         hadd_rel_sum_baseline, 
#         ntrips_sum_baseline 
#       )
#       
#       
#     )
#     
#     predictions[[x]]$draw<-x
#     
#   }
#   
#   predictions2= list.stack(predictions, fill=TRUE)
#   predictions2[is.na(predictions2)] = 0
# }
# write_xlsx(predictions2,"baseline_welfare_close_cod.xlsx") 



## analyze closure scenario data
{
  # full_closure<-data.frame(read_excel("baseline_welfare.xlsx"))
  # full_closure<-full_closure %>% 
  #   dplyr::mutate(scenario="full_closure")
  # 
  # cod_closure<-data.frame(read_excel("baseline_welfare_close_cod.xlsx"))
  # cod_closure<-cod_closure %>% 
  #   dplyr::mutate(scenario="cod_closure")
  # 
  # join_df=rbind.fill(full_closure,cod_closure)
  # join_df<-join_df %>% dplyr::select(-group)
  # 
  # cv_sum_full_closure<- full_closure$cv_sum
  # cv_sum_full_cod_closure<- cod_closure$cv_sum
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
  #################################
}


########################################
#########Historical projections 
#Methods (from Jorge's notes): Concretely, we will run the bioeconomic model for each of the years 
# in the period 2010-2019 as follows: i) calibrate the model for year 201X, ii) predict year 201X+1 under 
# independent and dependent catch-per trip for cod and haddock, then calibrate year 201X+1 and predict 201X+2,
# and so forth. Jorge will generate the catch-per-trip data for each year for both the independent and 
# correlated scenarios as follows: i) use the biomass and ocean bottom temperatures for each year in 2010-2019
# to calculate the expect catch-per-trip for cod and haddock, ii) add to the expect catch-per-trip the 
# simulated residuals (correlated or independent) to generate the independent and correlated samples to use 
# in the simulations.

# Start the clock!
ptm <- proc.time()


################## Clayton independent

#choose which copula data to use. cop_name =_clayton,  _gumbel, _frank, _plackett, _gaussian
cop_name<-"_clayton"

#independent or correlated = corr or ind
ind_or_corr<-"ind"

source("projection function2-historical.R")

#save the data 
write_xlsx(predictions_all,paste0("predictions_historical_v2_", cop_name, "_", ind_or_corr, ".xlsx")) 



################## Clayton corr

#choose which copula data to use. cop_name =_clayton,  _gumbel, _frank, _plackett, _gaussian
cop_name<-"_clayton"

#independent or correlated = corr or ind
ind_or_corr<-"corr"

source("projection function2-historical.R")

#save the data 
write_xlsx(predictions_all,paste0("predictions_historical_v2_", cop_name, "_", ind_or_corr, ".xlsx")) 

{
  ################## Gumbel ind
  
  #choose which copula data to use. cop_name =_clayton,  _gumbel, _frank, _plackett, _gaussian
  cop_name<-"_gumbel"
  
  #independent or correlated = corr or ind
  ind_or_corr<-"ind"
  
  source("projection function2-historical.R")
  
  #save the data 
  write_xlsx(predictions_all,paste0("predictions_historical_v2_", cop_name, "_", ind_or_corr, ".xlsx")) 
  
  
  ################## Gumbel corr
  
  #choose which copula data to use. cop_name =_clayton,  _gumbel, _frank, _plackett, _gaussian
  cop_name<-"_gumbel"
  
  #independent or correlated = corr or ind
  ind_or_corr<-"corr"
  
  source("projection function2-historical.R")
  
  #save the data 
  write_xlsx(predictions_all,paste0("predictions_historical_v2_", cop_name, "_", ind_or_corr, ".xlsx")) 
  
  
  ################## Frank ind
  
  #choose which copula data to use. cop_name =_clayton,  _gumbel, _frank, _plackett, _gaussian
  cop_name<-"_frank"
  
  #independent or correlated = corr or ind
  ind_or_corr<-"ind"
  
  source("projection function2-historical.R")
  
  #save the data 
  write_xlsx(predictions_all,paste0("predictions_historical_v2_", cop_name, "_", ind_or_corr, ".xlsx")) 
  
  
  ################## Frank corr
  
  #choose which copula data to use. cop_name =_clayton,  _gumbel, _frank, _plackett, _gaussian
  cop_name<-"_frank"
  
  #independent or correlated = corr or ind
  ind_or_corr<-"corr"
  
  source("projection function2-historical.R")
  
  #save the data 
  write_xlsx(predictions_all,paste0("predictions_historical_v2_", cop_name, "_", ind_or_corr, ".xlsx")) 
  
  
  ################## Plackett ind
  
  #choose which copula data to use. cop_name =_clayton,  _gumbel, _frank, _plackett, _gaussian
  cop_name<-"_plackett"
  
  #independent or correlated = corr or ind
  ind_or_corr<-"ind"
  
  source("projection function2-historical.R")
  
  #save the data 
  write_xlsx(predictions_all,paste0("predictions_historical_v2_", cop_name, "_", ind_or_corr, ".xlsx")) 
  
  
  ################## Plackett corr
  
  #choose which copula data to use. cop_name =_clayton,  _gumbel, _frank, _plackett, _gaussian
  cop_name<-"_plackett"
  
  #independent or correlated = corr or ind
  ind_or_corr<-"corr"
  
  source("projection function2-historical.R")
  
  #save the data 
  write_xlsx(predictions_all,paste0("predictions_historical_v2_", cop_name, "_", ind_or_corr, ".xlsx")) 
  
  
  
  ################## Gaussian ind
  
  #choose which copula data to use. cop_name =_clayton,  _gumbel, _frank, _plackett, _gaussian
  cop_name<-"_gaussian"
  
  #independent or correlated = corr or ind
  ind_or_corr<-"ind"
  
  source("projection function2-historical.R")
  
  #save the data 
  write_xlsx(predictions_all,paste0("predictions_historical_v2_", cop_name, "_", ind_or_corr, ".xlsx")) 
  
  
  ################## Gaussian corr
  
  #choose which copula data to use. cop_name =_clayton,  _gumbel, _frank, _plackett, _gaussian
  cop_name<-"_gaussian"
  
  #independent or correlated = corr or ind
  ind_or_corr<-"corr"
  
  source("projection function2-historical.R")
  
  #save the data 
  write_xlsx(predictions_all,paste0("predictions_historical_v2_", cop_name, "_", ind_or_corr, ".xlsx")) 
  
}

# Stop the clock
proc.time() - ptm









### compile results from decadal projections
{
  # calib_data <- readRDS("calibration_data_all.rds")
  # calib_data<-calib_data %>% 
  #   dplyr::group_by(draw) %>% 
  #   dplyr::summarise(estimated_trips=sum(estimated_trips),
  #                    tot_cod_catch= sum(tot_cod_catch), 
  #                    tot_keep_cod = sum(tot_keep_cod), 
  #                    tot_rel_cod = sum(tot_rel_cod), 
  #                    
  #                    tot_hadd_catch = sum(tot_hadd_catch), 
  #                    tot_keep_hadd = sum(tot_keep_hadd),
  #                    tot_rel_hadd = sum(tot_rel_hadd),
  #                    
  #                    n_choice_occasions = sum(n_choice_occasions)) %>% 
  #   dplyr::mutate(cod_keep_i=tot_keep_cod/n_choice_occasions, 
  #                 hadd_keep_i=tot_keep_hadd/n_choice_occasions,
  #                 cod_keep_trip=tot_keep_cod/estimated_trips, 
  #                 hadd_keep_trip=tot_keep_hadd/estimated_trips, 
  #                 cod_catch_i=tot_cod_catch/n_choice_occasions, 
  #                 hadd_catch_i=tot_hadd_catch/n_choice_occasions, 
  #                 cod_catch_trip=tot_cod_catch/estimated_trips, 
  #                 hadd_catch_trip=tot_hadd_catch/estimated_trips)
  # stats<-psych::describe(calib_data )
  # 
  # 
  # #Get the required statistics and convert the data into dataframe
  # summ_data <- data.frame(t(sapply(calib_data, function(x) 
  #   list(mean = mean(x,na.rm=TRUE), sd = sd(x,na.rm=TRUE)))))
  # #Change rownames to new column
  # summ_data$variable <- rownames(summ_data)
  # #Remove rownames
  # rownames(summ_data) <- NULL
  # #Make variable column as 1st column
  # cbind(summ_data[ncol(summ_data)], summ_data[-ncol(summ_data)])
  # 
  # 
  # descr(calib_data, stats = c('mean', 'sd'))
  # sapply(calib_data, c('mean', 'sd'), na.rm=TRUE)
  # pastecs::stat.desc(calib_data)
  # predictions_data<- read_excel("predictions__clayton_ind.xlsx") 
  # 
  # predictions_data<-predictions_data %>% 
  #   dplyr::mutate(decade=as.character(decade)) %>% 
  #   dplyr::group_by(decade, draw) %>% 
  #   dplyr::summarise(cod_keep_sum=sum(cod_keep_sum), 
  #                    hadd_keep_sum=sum(hadd_keep_sum),
  #                    cod_catch_sum=sum(cod_catch_sum), 
  #                    hadd_catch_sum=sum(hadd_catch_sum),
  #                    cv_sum=sum(cv_sum),
  #                    ntrip=sum(ntrips_alt_sum),
  #                    n_choices=sum(n_choice_occasions_sum), .groups = 'drop') %>% 
  #   dplyr::ungroup() %>% 
  #   dplyr::mutate(cod_keep_i=cod_keep_sum/n_choices,
  #                 hadd_keep_i=hadd_keep_sum/n_choices,
  #                 cod_catch_i=cod_catch_sum/n_choices,
  #                 hadd_catch_i=hadd_catch_sum/n_choices,
  #                 cv_i=cv_sum/n_choices)
  # 
  # 
  # plot1<-ggplot(predictions_data, aes(x=decade, y=cod_keep_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="cod keep per choice occasion", label=comma)
  # 
  # plot2<-ggplot(predictions_data, aes(x=decade, y=hadd_keep_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="haddock keep per choice occasion", label=comma) 
  # 
  # grid.arrange(plot1, plot2, ncol=2)
  # 
  # 
  # predictions_data<- read_excel("predictions__clayton_ind.xlsx") 
  # 
  # predictions_data<-predictions_data %>% 
  #   dplyr::mutate(decade=as.character(decade)) %>% 
  #   dplyr::group_by(decade, draw) %>% 
  #   dplyr::summarise(cod_keep_sum=sum(cod_keep_sum), 
  #                    hadd_keep_sum=sum(hadd_keep_sum),
  #                    cod_catch_sum=sum(cod_catch_sum), 
  #                    hadd_catch_sum=sum(hadd_catch_sum),
  #                    cv_sum=sum(cv_sum),
  #                    ntrip=sum(ntrips_alt_sum),
  #                    n_choices=sum(n_choice_occasions_sum),
  #                    k_tau_keep=mean(k_tau_keep_est), 
  #                    k_tau_catch=mean(k_tau_catch_est),
  #                    .groups = 'drop') %>% 
  #   dplyr::ungroup() %>% 
  #   dplyr::mutate(cod_keep_i=cod_keep_sum/n_choices,
  #                 hadd_keep_i=hadd_keep_sum/n_choices,
  #                 cod_catch_i=cod_catch_sum/n_choices,
  #                 hadd_catch_i=hadd_catch_sum/n_choices,
  #                 cv_i=cv_sum/n_choices)
  # 
  # predictions_data1<- read_excel("predictions__frank_corr.xlsx") 
  # predictions_data1 <-predictions_data1 %>% 
  #   mutate(copula= str_replace_all(copula, "([_])", "")) %>% 
  #   filter(decade==1)
  # 
  # predictions_data2<- read_excel("predictions__clayton_corr.xlsx") 
  # predictions_data2 <-predictions_data2 %>% 
  #   mutate(copula= str_replace_all(copula, "([_])", "")) %>% 
  #   filter(decade==1)
  # 
  # predictions_data3<- read_excel("predictions__gumbel_corr.xlsx") 
  # predictions_data3 <-predictions_data3 %>% 
  #   mutate(copula= str_replace_all(copula, "([_])", "")) %>% 
  #   filter(decade==1)
  # 
  # predictions_data4<-rbind(predictions_data1, predictions_data2, predictions_data3)
  # predictions_data5<- predictions_data4  %>%
  #   dplyr::group_by(copula, draw) %>% 
  #   dplyr::summarise(cod_keep_sum=sum(cod_keep_sum), 
  #                    hadd_keep_sum=sum(hadd_keep_sum),
  #                    cod_catch_sum=sum(cod_catch_sum), 
  #                    hadd_catch_sum=sum(hadd_catch_sum),
  #                    cv_sum=sum(cv_sum),
  #                    ntrip=sum(ntrips_alt_sum),
  #                    n_choices=sum(n_choice_occasions_sum),
  #                    k_tau_keep=mean(k_tau_keep_est), 
  #                    k_tau_catch=mean(k_tau_catch_est),
  #                    .groups = 'drop') %>% 
  #   dplyr::ungroup() %>% 
  #   dplyr::mutate(cod_keep_i=cod_keep_sum/n_choices,
  #                 hadd_keep_i=hadd_keep_sum/n_choices,
  #                 cod_catch_i=cod_catch_sum/n_choices,
  #                 hadd_catch_i=hadd_catch_sum/n_choices,
  #                 cv_i=cv_sum/n_choices)
  # 
  # predictions_data<- read_excel("predictions__clayton_ind.xlsx") 
  # 
  # predictions_data_month_d1<-predictions_data %>% 
  #   dplyr::mutate(decade=as.character(decade)) %>% 
  #   dplyr::mutate(cod_keep_i=cod_keep_sum/n_choice_occasions_sum,
  #                 hadd_keep_i=hadd_keep_sum/n_choice_occasions_sum,
  #                 cod_catch_i=cod_catch_sum/n_choice_occasions_sum,
  #                 hadd_catch_i=hadd_catch_sum/n_choice_occasions_sum,
  #                 cv_i=cv_sum/n_choice_occasions_sum) %>% 
  #   # dplyr::filter(decade %in% c("1", "3", "5", "7")) %>% 
  #   dplyr::arrange(month, decade, draw)
  # 
  # plot2<-ggplot(predictions_data_month_d1, aes(x=decade, y=ntrips_alt_sum)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="Number of trips", label=comma) +
  #   facet_wrap(~month, nrow = 2)
  # plot2
  # 
  # plot2<-ggplot(predictions_data_month_d1, aes(x=decade, y=cv_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="CV per choice occasion", label=comma) +
  #   facet_wrap(~month, nrow = 2)
  # plot2
  # 
  # 
  # plot2<-ggplot(predictions_data_month_d1, aes(x=decade, y=cod_keep_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="Cod keep per choice occasion", label=comma) +
  #   facet_wrap(~month, nrow = 2)
  # plot2
  # 
  # plot2<-ggplot(predictions_data_month_d1, aes(x=decade, y=hadd_keep_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="Haddock keep per choice occasion", label=comma) +
  #   facet_wrap(~month, nrow = 2)
  # plot2
  # 
  # plot2<-ggplot(predictions_data_month_d1, aes(x=decade, y=cod_catch_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="Cod catch per choice occasion", label=comma) +
  #   facet_wrap(~month, nrow = 2)
  # plot2
  # 
  # plot2<-ggplot(predictions_data_month_d1, aes(x=decade, y=hadd_catch_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="Haddock catch per choice occasion", label=comma) +
  #   facet_wrap(~month, nrow = 2)
  # plot2
  # 
  # plot2<-ggplot(predictions_data_month_d1[], aes(x=decade, y=k_tau_keep_est_mnth)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="Correlation in keep per choice occasion", label=comma) + geom_hline(yintercept=0, linetype="dashed", color = "red")+
  #   facet_wrap(~month, nrow = 2)
  # plot2
  # 
  # plot2<-ggplot(predictions_data_month_d1, aes(x=decade, y=k_tau_catch_est_mnth)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="Correlation in catch per choice occasion", label=comma) + geom_hline(yintercept=0, linetype="dashed", color = "red")+
  #   facet_wrap(~month, nrow = 2)
  # plot2
  # 
  # 
  # plot2<-ggplot(predictions_data_month_d1, aes(x=decade, y=ntrips_alt_sum)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="Number of trips", label=comma) +
  #   facet_wrap(~month, nrow = 1)
  # plot2
  # 
  # 
  # 
  # 
  # plot1<-ggplot(predictions_data_month_d1, aes(x=decade, y=cod_keep_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="cod keep per choice occasion", label=comma)
  # 
  # plot2<-ggplot(predictions_data_month_d1, aes(x=decade, y=hadd_keep_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="haddock keep per choice occasion", label=comma) 
  # 
  # plot3<-ggplot(predictions_data_month_d1, aes(x=decade, y=cod_catch_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="cod catch per choice occasion", label=comma)
  # 
  # plot4<-ggplot(predictions_data_month_d1, aes(x=decade, y=hadd_catch_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="haddock catch per choice occasion", label=comma) 
  # 
  # grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
  # 
  # 
  # plot1<-ggplot(predictions_data_month_d1, aes(x=decade, y=n_choices)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="Number of choice occasions", label=comma)
  # 
  # 
  # 
  # plot2<-ggplot(predictions_data_month_d1, aes(x=decade, y=ntrip)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="Number of trips", label=comma) +
  #   facet_wrap(~month, nrow = 1)
  # plot2
  # 
  # 
  # 
  # plot2<-ggplot(predictions_data_month_d1, aes(x=decade, y=ntrip)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="Number of trips", label=comma) 
  # 
  # plot2
  # grid.arrange(plot1, plot2, ncol=2)
  # 
  # 
  # 
  # 
  # plot1<-ggplot(predictions_data_month_d1, aes(x=month, y=cod_keep_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="cod keep per choice occasion", label=comma)
  # 
  # plot2<-ggplot(predictions_data_month_d1, aes(x=month, y=hadd_keep_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="haddock keep per choice occasion", label=comma) 
  # 
  # grid.arrange(plot1, plot2, ncol=2)
  # 
  # 
  # 
  # predictions_data_d1<-predictions_data_month %>% 
  #   dplyr::filter(decade==1) %>% 
  #   dplyr::mutate(decade=as.character(decade)) %>% 
  #   dplyr::group_by(decade, draw, month) %>% 
  #   dplyr::summarise(cod_keep_sum=sum(cod_keep_sum), 
  #                    hadd_keep_sum=sum(hadd_keep_sum),
  #                    cod_catch_sum=sum(cod_catch_sum), 
  #                    hadd_catch_sum=sum(hadd_catch_sum),
  #                    cv_sum=sum(cv_sum),
  #                    ntrip=sum(ntrips_alt_sum),
  #                    n_choices=sum(n_choice_occasions_sum), .groups = 'drop') %>% 
  #   dplyr::ungroup() %>% 
  #   dplyr::mutate(cod_keep_i=cod_keep_sum/n_choices,
  #                 hadd_keep_i=hadd_keep_sum/n_choices,
  #                 cod_catch_i=cod_catch_sum/n_choices,
  #                 hadd_catch_i=hadd_catch_sum/n_choices,
  #                 cv_i=cv_sum/n_choices, 
  #                 month=as.factor(month)) 
  # 
  # describeBy(predictions_data_d1[ , c('cod_keep_i', 'hadd_keep_i')], group=predictions_data_d1$month, fast=TRUE)
  # 
  # plot1<-ggplot(predictions_data_d1, aes(x=month, y=cod_keep_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="cod keep per choice occasion", label=comma)
  # 
  # plot2<-ggplot(predictions_data_d1, aes(x=month, y=hadd_keep_i)) + 
  #   geom_violin() + geom_boxplot(width=0.1)+
  #   scale_y_continuous(name="haddock keep per choice occasion", label=comma) 
  # 
  # grid.arrange(plot1, plot2, ncol=2)
  
}



########################################
####Decadal projections, but open cod for every other month except for its current open month (September)
####Months with effort: 4, 5, 6, 7, 8, 9, 10, 11
####Do this for the Clayton ind 

# Start the clock!
ptm <- proc.time()

predictions_all2<-list()
for(m in c(5, 6, 7, 8, 10 )){
  
  ##################
  directed_trips<-data.frame(read_csv("directed trips and regulations 2010_2020.csv", show_col_types = FALSE))
  directed_trips<-directed_trips %>% 
    dplyr::mutate(dtrip=round(dtrip)) %>% 
    dplyr::filter(year==2020) %>% 
    dplyr::filter(dtrip!=0) %>% 
    dplyr::select(-year)
  
  if(m==5){
    directed_trips<-directed_trips %>% 
      dplyr::mutate(cod_bag=ifelse(period2 %in% c("9_fh","10_fh", "10_pr"), 1, 0), 
                    cod_min=ifelse(period2 %in% c("9_fh","10_fh", "10_pr"), 21, 0))
  }                                          
  
  if(m==6){
    directed_trips<-directed_trips %>% 
      dplyr::mutate(cod_bag=ifelse(period2 %in% c("11_fh","12_fh", "12_pr"), 1, 0), 
                    cod_min=ifelse(period2 %in% c("11_fh","12_fh", "12_pr"), 21, 0))
  }       
  
  if(m==7){
    directed_trips<-directed_trips %>% 
      dplyr::mutate(cod_bag=ifelse(period2 %in% c("13_fh","14_fh", "14_pr"), 1, 0), 
                    cod_min=ifelse(period2 %in% c("13_fh","14_fh", "14_pr"), 21, 0))
  }       
  
  if(m==8){
    directed_trips<-directed_trips %>% 
      dplyr::mutate(cod_bag=ifelse(period2 %in% c("15_fh","16_fh", "16_pr"), 1, 0), 
                    cod_min=ifelse(period2 %in% c("15_fh","16_fh", "16_pr"), 21, 0))
  }       
  
  if(m==10){
    directed_trips<-directed_trips %>% 
      dplyr::mutate(cod_bag=ifelse(period2 %in% c("19_fh","20_fh", "20_pr"), 1, 0), 
                    cod_min=ifelse(period2 %in% c("19_fh","20_fh", "20_pr"), 21, 0))
  }       
  
  
  
  #choose which copula data to use
  
  #cop_name = _gumbel, _frank, clayton
  cop_name<-"_clayton"
  
  #independat or correlations = corr or ind
  ind_or_corr<-"ind"
  
  
  source("projection function2.R")
  
  predictions_all<- predictions_all %>% 
    dplyr::mutate(closure_month=m)
  
  predictions_all2[[m]]<-predictions_all
  
}
predictions_all3<- list.stack(predictions_all2, fill=TRUE)


# Stop the clock
proc.time() - ptm

#save the data 
write_xlsx(predictions_all3,paste0("predictions_", cop_name, "_", ind_or_corr, "_cod_open_season.xlsx")) 


####NOw do this for the Clayton corr 

# Start the clock!
ptm <- proc.time()

predictions_all2<-list()
for(m in c(5, 6, 7, 8, 10 )){
  
  #m<-8
  ##################
  directed_trips<-data.frame(read_csv("directed trips and regulations 2010_2020.csv", show_col_types = FALSE))
  directed_trips<-directed_trips %>% 
    dplyr::mutate(dtrip=round(dtrip)) %>% 
    dplyr::filter(year==2020) %>% 
    dplyr::filter(dtrip!=0) %>% 
    dplyr::select(-year)
  
  if(m==5){
    directed_trips<-directed_trips %>% 
      dplyr::mutate(cod_bag=ifelse(period2 %in% c("9_fh","10_fh", "10_pr"), 1, 0), 
                    cod_min=ifelse(period2 %in% c("9_fh","10_fh", "10_pr"), 21, 0))
  }                                          
  
  if(m==6){
    directed_trips<-directed_trips %>% 
      dplyr::mutate(cod_bag=ifelse(period2 %in% c("11_fh","12_fh", "12_pr"), 1, 0), 
                    cod_min=ifelse(period2 %in% c("11_fh","12_fh", "12_pr"), 21, 0))
  }       
  
  if(m==7){
    directed_trips<-directed_trips %>% 
      dplyr::mutate(cod_bag=ifelse(period2 %in% c("13_fh","14_fh", "14_pr"), 1, 0), 
                    cod_min=ifelse(period2 %in% c("13_fh","14_fh", "14_pr"), 21, 0))
  }       
  
  if(m==8){
    directed_trips<-directed_trips %>% 
      dplyr::mutate(cod_bag=ifelse(period2 %in% c("15_fh","16_fh", "16_pr"), 1, 0), 
                    cod_min=ifelse(period2 %in% c("15_fh","16_fh", "16_pr"), 21, 0))
  }       
  
  if(m==10){
    directed_trips<-directed_trips %>% 
      dplyr::mutate(cod_bag=ifelse(period2 %in% c("19_fh","20_fh", "20_pr"), 1, 0), 
                    cod_min=ifelse(period2 %in% c("19_fh","20_fh", "20_pr"), 21, 0))
  }       
  
  
  
  #choose which copula data to use
  
  #cop_name = _gumbel, _frank, clayton
  cop_name<-"_clayton"
  
  #independat or correlations = corr or ind
  ind_or_corr<-"corr"
  
  
  source("projection function2.R")
  
  predictions_all<- predictions_all %>% 
    dplyr::mutate(closure_month=m)
  
  predictions_all2[[m]]<-predictions_all
  
}
predictions_all3<- list.stack(predictions_all2, fill=TRUE)


# Stop the clock
proc.time() - ptm

#save the data 
write_xlsx(predictions_all3,paste0("predictions_", cop_name, "_", ind_or_corr, "_cod_open_season.xlsx")) 












######################
######Now run the simulation and close cod entirely. Do this for the Clayton datasets 
#Clayton corr

# Start the clock!
ptm <- proc.time()

predictions_all2<-list()

##################
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0) %>% 
  dplyr::mutate(cod_bag=0, cod_min=100)



#choose which copula data to use

#cop_name = _gumbel, _frank, clayton
cop_name<-"_clayton"

#independat or correlations = corr or ind
ind_or_corr<-"corr"


source("projection function2.R")

predictions_all<- predictions_all %>% 
  dplyr::mutate(closure_month=99)

# Stop the clock
proc.time() - ptm

#save the data 
write_xlsx(predictions_all,paste0("predictions_", cop_name, "_", ind_or_corr, "_cod_fully_closed.xlsx")) 



#Clayton independent

# Start the clock!
ptm <- proc.time()

predictions_all2<-list()

##################
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0) %>% 
  dplyr::mutate(cod_bag=0, cod_min=100)



#choose which copula data to use

#cop_name = _gumbel, _frank, clayton
cop_name<-"_clayton"

#independat or correlations = corr or ind
ind_or_corr<-"ind"


source("projection function2.R")

predictions_all<- predictions_all %>% 
  dplyr::mutate(closure_month=99)


# Stop the clock
proc.time() - ptm

#save the data 
write_xlsx(predictions_all,paste0("predictions_", cop_name, "_", ind_or_corr, "_cod_fully_closed.xlsx")) 

######################





##Analyze data 

### OLS meta-analaysis of simulation results
# ```{r,results='asis'}
# predictions_data1<- read_excel("predictions__frank_corr.xlsx") 
# predictions_data1 <-predictions_data1 %>% 
#   mutate(copula= str_replace_all(copula, "([_])", "")) 
# 
# predictions_data2<- read_excel("predictions__clayton_corr.xlsx") 
# predictions_data2 <-predictions_data2 %>% 
#   mutate(copula= str_replace_all(copula, "([_])", "")) 
# 
# predictions_data3<- read_excel("predictions__gumbel_corr.xlsx") 
# predictions_data3 <-predictions_data3 %>% 
#   mutate(copula= str_replace_all(copula, "([_])", "")) 
# 
# predictions_data4<- read_excel("predictions__plackett_corr.xlsx") 
# predictions_data4 <-predictions_data4 %>% 
#   mutate(copula= str_replace_all(copula, "([_])", "")) 
# 
# predictions_data5<- read_excel("predictions__gaussian_corr.xlsx") 
# predictions_data5 <-predictions_data5 %>% 
#   mutate(copula= str_replace_all(copula, "([_])", "")) 
# 
# predictions_data6<- read_excel("predictions__frank_ind.xlsx") 
# predictions_data6 <-predictions_data6 %>% 
#   mutate(copula= str_replace_all(copula, "([_])", "")) 
# 
# predictions_data7<- read_excel("predictions__clayton_ind.xlsx") 
# predictions_data7 <-predictions_data7 %>% 
#   mutate(copula= str_replace_all(copula, "([_])", "")) 
# 
# predictions_data8<- read_excel("predictions__gumbel_ind.xlsx") 
# predictions_data8 <-predictions_data8 %>% 
#   mutate(copula= str_replace_all(copula, "([_])", "")) 
# 
# predictions_data9<- read_excel("predictions__plackett_ind.xlsx") 
# predictions_data9 <-predictions_data9 %>% 
#   mutate(copula= str_replace_all(copula, "([_])", "")) 
# 
# predictions_data10<- read_excel("predictions__gaussian_ind.xlsx") 
# predictions_data10 <-predictions_data10 %>% 
#   mutate(copula= str_replace_all(copula, "([_])", "")) 
# 
# 
# predictions_data11<-rbind(predictions_data1, predictions_data2, predictions_data3, predictions_data4, predictions_data5,predictions_data6, predictions_data7, predictions_data8, predictions_data9, predictions_data10 )
# 
# predictions_data11<-predictions_data11 %>% 
#   dplyr::mutate(copula=as.factor(copula), 
#                 corr_type=as.factor(corr_type))
# 
# fit <- lm(cv_sum ~  factor(month)+ factor(decade)+ k_tau_catch_est_mnth+ k_tau_keep_est_mnth +cod_keep_sum + hadd_keep_sum +cod_rel_sum +hadd_rel_sum +factor(corr_type)+ factor(copula), data=predictions_data11)
# 
# library(xtable)
# print(xtable(summary(fit)),type='html')
# ```
