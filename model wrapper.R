
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
pkgs_to_use <- c("tidyr",
                 "magrittr",
                 "tidyverse",
                 "reshape2",
                 "splitstackshape",
                 "doBy",
                 "WriteXLS",
                 "Rcpp",
                 "ggplot2",
                 "dplyr",
                 "rlist",
                 "fitdistrplus",
                 "MASS",
                 "psych",
                 "rgl",
                 "copula",
                 "VineCopula",
                 "scales",
                 "univariateML",
                 "logspline",
                 "readr",
                 "data.table",
                 "conflicted", 
                 "readxl", 
                 "writexl", 
                 "plyr" , "furrr", "profvis", "future", "Hmisc", "SDMTools", "tictoc")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
# library(readxl)
# library(writexl)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("count", "dplyr")


###Input data### 
#For the calibration model:
#1)Directed trips for summer flounder and black sea bass by state, mode, and bi-monthly period. 
#This file also contains the baseline year regulations by species
#2)Observed catch (10,000) draws of catch per state and month
#3)Utility model parameters are hard-coded
#4)Trip costs by state and mode, inflation adjusted to the baseline year



n_drawz<-1000
n_catch_draws<-30

# Run the calibration files

# Below is a code that finds p* values which lead to projected harvest approximating actual harvest
# Don't need to run this every time

# source("find_pstar_values.R")

#once the p_star values are found, run the calibrations and save the output files 


p_star_cod_variable<- 0.915
p_star_hadd_variable<- 0.45



#Directed trips and regulations by period
directed_trips<-data.frame(read_csv("directed trips and regulations 2020.csv ", show_col_types = FALSE))
directed_trips$dtrip=round(directed_trips$dtrip)


# Catch
# Here input  catch-per-trip projections that are based on 2020 by period 
catch_data_all = data.frame(read_csv("simulated_ind_catch.csv ", show_col_types = FALSE))



# Here input  catch-per-trip projections that are based on 2020
#catch_data_all <-data.frame(read_csv("simulated_ind_catch.csv ", show_col_types = FALSE))


# For the baseline (no correlation) scenario, use catch-per-trip distributions from the baseline year (observed_catch_2020_SS_MM_md.csv)
# For all scenarios, use the adjusted catch-at-length distribution and no stock adjustments


#Catch at length
size_data_cod <- subset(read_csv("length_distns_2020.csv",  show_col_types = FALSE), 
                       species=="atlanticcod",select=c(fitted_length, prob_star))


size_data_hadd <- subset(read_csv("length_distns_2020.csv",  show_col_types = FALSE), 
                        species=="haddock",select=c(fitted_length, prob_star))


################
#Decade 8, Sub=.25
################

# d<-8
# 
# 
# #here we can adjust the substitution parameter
# 
# #substitution_mean_parameter<- -.0596595
# #substitution_sd_parameter<- .161084
# 
# substitution_mean_parameter<- -.25
# substitution_sd_parameter<- .67501404


calibration_data_table_base<-list()
cost_files_all_base<-list()

for (x in 1:10){

source("calibration loop.R")

pds_new_all_MA$draw<-x
calibration_data_table_base[[x]] <- pds_new_all_MA

costs_new_all_MA$draw<-x
cost_files_all_base[[x]] <- costs_new_all_MA
}


# Start the clock!
ptm <- proc.time()

##Run the catch function 
predictions<-list()

for (x in 1:10){

 source("projection function.R")

 prediction_output_by_period1<- trip_level_output
 prediction_output_by_period1 <- prediction_output_by_period1 %>%  separate(period2, c("period", "mode"), "_")
  
  
  #Metrics at the choice occasion level
  cv_i<- weighted.mean(prediction_output_by_period1$change_CS, prediction_output_by_period1$expand)
  cod_keep_i<- weighted.mean(prediction_output_by_period1$tot_keep_cod, prediction_output_by_period1$expand)
  hadd_keep_i<- weighted.mean(prediction_output_by_period1$tot_keep_hadd, prediction_output_by_period1$expand)
  cod_rel_i<- weighted.mean(prediction_output_by_period1$tot_rel_cod, prediction_output_by_period1$expand)
  hadd_rel_i<- weighted.mean(prediction_output_by_period1$tot_rel_hadd, prediction_output_by_period1$expand)
  
  
  
  prediction_output_by_period1 <- prediction_output_by_period1 %>%
    as.data.table() %>%
    .[, cv_sum := expand*change_CS] %>%
    .[, cod_keep_sum := expand*tot_keep_cod] %>%
    .[, hadd_keep_sum := expand*tot_keep_hadd] %>%
    .[, cod_rel_sum := expand*tot_rel_cod] %>%
    .[, hadd_rel_sum := expand*tot_rel_hadd] %>%
    .[, ntrips_alt := expand*probA] 

  prediction_output_by_period1 <- prediction_output_by_period1 %>%
    mutate_if(is.numeric, replace_na, replace = 0)    
  
  
  #Metrics a coast level 
  cv_sum<- sum(prediction_output_by_period1$cv_sum)
  cod_keep_sum<- sum(prediction_output_by_period1$cod_keep_sum)
  hadd_keep_sum<- sum(prediction_output_by_period1$hadd_keep_sum)
  cod_rel_sum<- sum(prediction_output_by_period1$cod_rel_sum)
  hadd_rel_sum<- sum(prediction_output_by_period1$hadd_rel_sum)
  ntrips_sum<-sum(prediction_output_by_period1$ntrips_alt)
  n_choice_occasions_sum<-sum(calibration_data_table_base[[x]]$n_choice_occasions)
  
  
  
  
  predictions[[x]]<- as.data.frame(cbind(
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
  
  predictions[[x]]$draw<-x
  #predictions[[x]]$decade<-d
  
  # predictions[[x]]<-list.append(predictions[[x]],draw=x)
  # predictions[[x]]<-list.append(predictions[[x]],decade=d)
  
  #predictions[[x]]$draw<-x
  #predictions[[x]]$decade<-d
  
}


# Stop the clock
proc.time() - ptm


predictions_all<-list.rbind(predictions)
predictions_all$group<-1

ggplot(predictions_all, aes(x=group, y=cv_sum)) + 
  geom_violin(trim=FALSE) + geom_boxplot( width=0.1) +geom_hline(yintercept=0, color="red") 


