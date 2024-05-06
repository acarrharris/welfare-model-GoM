
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
                 "plyr" , "furrr", "profvis", "future", "Hmisc", "SDMTools", "tictoc", "rpudplus", 
                 "rmdformats", "prettydoc", "hrbrthemes", "tint", "tufte", "rstatix", "ggpubr")
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

########################################
###Run the calibration
{
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
}

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

keep_rel_pairz_month_all <- readRDS("k_tau_values_month_calibration.rds")


########################################
#Projection evaluating baseline welfare. Do this by setting the bag limits = 0
catch_data_all = data.frame(read_csv("calibration catch per trip.csv", show_col_types = FALSE))

calibration_data_all <- readRDS("calibration_data_all.rds")
predictions<-list()
{
# Start the clock!
ptm <- proc.time()


for (x in 1:100){

source("projection - baseline welfare.R")

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


calibration_data <- calibration_data_all %>% 
  dplyr::filter(draw==x)

n_choice_occasions_sum<-sum(calibration_data$n_choice_occasions)
ntrips_sum_baseline<-sum(calibration_data$estimated_trips)
cod_keep_sum_baseline<- sum(calibration_data$tot_keep_cod)
hadd_keep_sum_baseline<- sum(calibration_data$tot_keep_hadd)
cod_rel_sum_baseline<- sum(calibration_data$tot_rel_cod)
hadd_rel_sum_baseline<- sum(calibration_data$tot_rel_hadd)

cod_keep_i_baseline<- weighted.mean(calibration_data$tot_keep_cod, calibration_data$n_choice_occasions)
hadd_keep_i_baseline<- weighted.mean(calibration_data$tot_keep_hadd, calibration_data$n_choice_occasions)
cod_rel_i_baseline<- weighted.mean(calibration_data$tot_rel_cod, calibration_data$n_choice_occasions)
hadd_rel_i_baseline<- weighted.mean(calibration_data$tot_rel_hadd, calibration_data$n_choice_occasions)


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
}
predictions2= list.stack(predictions, fill=TRUE)
predictions2[is.na(predictions2)] = 0

write_xlsx(predictions2,"baseline_welfare.xlsx")


# Stop the clock
proc.time() - ptm


########################################
#Projection evaluating baseline welfare from closing cod only. Do this by setting the bag limits for cod = 0 
catch_data_all = data.frame(read_csv("calibration catch per trip.csv", show_col_types = FALSE))

calibration_data_all <- readRDS("calibration_data_all.rds")
predictions<-list()
{
# Start the clock!
ptm <- proc.time()


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
  
  n_choice_occasions_sum<-sum(calibration_data$n_choice_occasions)
  ntrips_sum_baseline<-sum(calibration_data$estimated_trips)
  cod_keep_sum_baseline<- sum(calibration_data$tot_keep_cod)
  hadd_keep_sum_baseline<- sum(calibration_data$tot_keep_hadd)
  cod_rel_sum_baseline<- sum(calibration_data$tot_rel_cod)
  hadd_rel_sum_baseline<- sum(calibration_data$tot_rel_hadd)
  
  cod_keep_i_baseline<- weighted.mean(calibration_data$tot_keep_cod, calibration_data$n_choice_occasions)
  hadd_keep_i_baseline<- weighted.mean(calibration_data$tot_keep_hadd, calibration_data$n_choice_occasions)
  cod_rel_i_baseline<- weighted.mean(calibration_data$tot_rel_cod, calibration_data$n_choice_occasions)
  hadd_rel_i_baseline<- weighted.mean(calibration_data$tot_rel_hadd, calibration_data$n_choice_occasions)
  
  
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
}
write_xlsx(predictions2,"baseline_welfare_close_cod.xlsx") 



## analyze closure scenario data
{
full_closure<-data.frame(read_excel("baseline_welfare.xlsx"))
full_closure<-full_closure %>% 
  dplyr::mutate(scenario="full_closure")

cod_closure<-data.frame(read_excel("baseline_welfare_close_cod.xlsx"))
cod_closure<-cod_closure %>% 
  dplyr::mutate(scenario="cod_closure")

join_df=rbind.fill(full_closure,cod_closure)
join_df<-join_df %>% dplyr::select(-group)

cv_sum_full_closure<- full_closure$cv_sum
cv_sum_full_cod_closure<- cod_closure$cv_sum
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
}





########################################
####Decadal projections
# Start the clock!
ptm <- proc.time()
{
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
write_xlsx(predictions_all,paste0("predictions_", cop_name, "_", ind_or_corr, ".xlsx")) 



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
write_xlsx(predictions_all,paste0("predictions_", cop_name, "_", ind_or_corr, ".xlsx")) 


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
write_xlsx(predictions_all,paste0("predictions_", cop_name, "_", ind_or_corr, ".xlsx")) 



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
write_xlsx(predictions_all,paste0("predictions_", cop_name, "_", ind_or_corr, ".xlsx")) 



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
write_xlsx(predictions_all,paste0("predictions_", cop_name, "_", ind_or_corr, ".xlsx")) 



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
write_xlsx(predictions_all,paste0("predictions_", cop_name, "_", ind_or_corr, ".xlsx")) 

##################


##################
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0)


#choose which copula data to use

#cop_name = _gumbel, _frank, _clayton, _plackett
cop_name<-"_plackett"

#independat or correlations = corr or ind
ind_or_corr<-"ind"

# Start the clock!
ptm <- proc.time()

source("projection function2.R")

#save the data 
write_xlsx(predictions_all,paste0("predictions_", cop_name, "_", ind_or_corr, ".xlsx")) 

##################



##################
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0)


#choose which copula data to use

#cop_name = _gumbel, _frank, _clayton, _plackett
cop_name<-"_plackett"

#independat or correlations = corr or ind
ind_or_corr<-"corr"

# Start the clock!
ptm <- proc.time()

source("projection function2.R")

#save the data 
write_xlsx(predictions_all,paste0("predictions_", cop_name, "_", ind_or_corr, ".xlsx")) 

##################



##################
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0)


#choose which copula data to use

#cop_name = _gumbel, _frank, _clayton, _plackett, _gaussian
cop_name<-"_gaussian"

#independat or correlations = corr or ind
ind_or_corr<-"ind"

# Start the clock!
ptm <- proc.time()

source("projection function2.R")

#save the data 
write_xlsx(predictions_all,paste0("predictions_", cop_name, "_", ind_or_corr, ".xlsx")) 

##################



##################
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
directed_trips<-directed_trips %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0)


#choose which copula data to use

#cop_name = _gumbel, _frank, _clayton, _plackett, _gaussian
cop_name<-"_gaussian"

#independat or correlations = corr or ind
ind_or_corr<-"corr"

# Start the clock!
ptm <- proc.time()

source("projection function2.R")

#save the data 
write_xlsx(predictions_all,paste0("predictions_", cop_name, "_", ind_or_corr, ".xlsx")) 

##################

# Stop the clock
proc.time() - ptm
##################

}


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
  directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
  directed_trips<-directed_trips %>% 
    dplyr::mutate(dtrip=round(dtrip)) %>% 
    dplyr::filter(dtrip!=0) 
  
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
  directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv", show_col_types = FALSE))
  directed_trips<-directed_trips %>% 
    dplyr::mutate(dtrip=round(dtrip)) %>% 
    dplyr::filter(dtrip!=0) 
  
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
