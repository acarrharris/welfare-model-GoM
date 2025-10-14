
# This is the simulation model wrapper 


# Install packages and set globals 
args = commandArgs(trailingOnly=TRUE)
options(future.globals.maxSize= 1000000000)

packages <- c("tidyr",  "tidyverse", "reshape2","splitstackshape",
              "doBy","WriteXLS","Rcpp", "ggplot2", "rlist","fitdistrplus",
              "MASS",  "psych", "rgl","copula", "VineCopula","scales","univariateML",
              "logspline","readr","data.table","conflicted", "readxl", "writexl", 
              "furrr", "profvis", "future", "Hmisc", "tictoc","purrr" , "rlist",
              "rmdformats", "prettydoc", "hrbrthemes", "tint", "tufte", "rstatix", "ggpubr", 
              "future.apply", "DescTools", "listenv", "akima", "reshape2", "stringr", "plyr", "dplyr")

# Install only those not already installed
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only = TRUE)
library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)
options(scipen = 999)


input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions


# Simulation approach:
    # 1) calibrate the model by finding the value of p* for each draw (out of 100) of catch 
    #    that explains observed harvest. Save baseline trip outcomes, # choice occasions, ktau's
    # 2) Using those p* values and saved data, re-run the model with projected catch-per-trip data
    #    for each copula/decade

# Part A - A1) calibrate the model to 2021 using observed 2021 catch-per-trip data that retains inter-species catch dependence
#          A2) project outcomes using copula data based on correlated residuals    

# Part B - B1) calibrate the model to 2021 using 2021 catch-per-trip that explicitly removes inter-species catch dependence
#          B2) project outcomes using copula data based on independent residuals              

# Part C - auxiliary -  close the baseline year fishery and compute welfare
#          C1) Close cod 
#          C2) Close cod and haddock    


###### PART A ##########

#### A1) Calibration


p_star_values_i<-list()
p_star_values_i_y<-list()

yrz<- c(2019, 2020, 2021)

for (y in yrz){
  for (i in 1:100){

  seed<-i
  
  # Pull in calibration-year catch draws
  
  # catch_data_all_split <- data.frame(read_csv(paste0(input_data_cd, "calib_catch_yr", y, "_draw", i, ".csv"), show_col_types = FALSE)) %>% 
  #   filter(tripid<=100)
  
  #code for assigning zero to uncertain MRIP estimates
  catch_data_all_split <- readr::read_csv(
    paste0(input_data_cd,  "calib_catch_yr", y, "_draw", i, ".csv"), show_col_types = FALSE) %>% 
    dplyr::mutate(
      omit = dplyr::if_else(
        state == 23 & year == 2019 & month %in% c(5, 6), 1,0)) %>%
    dplyr::mutate(
      omit = dplyr::if_else(
        state == 23 & year == 2019 & month %in% c(9,10), 1,omit)) %>%
    dplyr::mutate(
      omit = dplyr::if_else(
        state == 23 & year == 2020 & month %in% c(5,6), 1,omit)) %>%
    dplyr::mutate(
      omit = dplyr::if_else(state == 23 & year == 2020 & month %in% c(9,10), 1,omit)) %>%
    dplyr::mutate(
      omit = dplyr::if_else(
        state == 25 & year == 2020 & month %in% c(11,12), 1,omit)) %>%
    dplyr::mutate(
      omit = dplyr::if_else(
        state == 23 & year == 2021 & month %in% c(9,10), 1,omit)) %>%
    dplyr::filter(omit != 1) %>%
    filter(tripid<=100)
  
  # Directed trips and regulations by period
  #directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE)) %>% 
  #  dplyr::mutate(dtrip=round(dtrip)) %>% dplyr::filter(dtrip!=0) %>% dplyr::filter(year==y)

  #code for assigning zero to uncertain MRIP estimates
  directed_trips <- readr::read_csv(
    paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
    show_col_types = FALSE
  )   %>%
    dplyr::mutate(dtrip = round(dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
    dplyr::mutate(
      dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
    dplyr::mutate(
      dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
    dplyr::filter(dtrip != 0) %>%
    dplyr::filter(year == y)
  
    directed_trips_p <- directed_trips %>% 
    mutate(period2 = as.character(period2)) %>% mutate(n_trips = floor(dtrip), n_draws = n_draws) 
  
  regs <- directed_trips_p %>% dplyr::select(period2, cod_bag, cod_min, hadd_bag, hadd_min, dtrip)
  
  # number of simulated annual periods
  n_periods<-length(unique(regs$period2))
  
  regs_check <- directed_trips_p %>% dplyr::select(period2, dtrip)
  
  MRIP_data <- catch_data_all_split %>%
    dplyr::group_by(period2) %>%
    dplyr::summarise(mean_cod_keep = mean(landing_cod),
                     mean_hadd_keep = mean(landing_hadd),
                     mean_hadd_cat = mean(tot_cat_hadd),
                     mean_cod_cat = mean(tot_cat_cod),.groups="drop") %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(regs_check, by="period2") %>% 
    mutate(cod_harvest=mean_cod_keep*dtrip, 
           hadd_harvest=mean_hadd_keep*dtrip, 
           cod_tot_cat=mean_cod_cat*dtrip, 
           hadd_tot_cat=mean_hadd_cat*dtrip) %>% 
    dplyr::summarise(cod_harvest = sum(cod_harvest),
                     hadd_harvest = sum(hadd_harvest),
                     cod_tot_cat = sum(cod_tot_cat),
                     hadd_tot_cat = sum(hadd_tot_cat),.groups="drop")
  
  # Run the p-star routine and save the output
  source(paste0(input_code_cd,"find p-star values 2010-2020.R")) # this calls "calibration loop2 2010-2020.R"
  
  p_starz<-cbind(p_star_cod_variable, p_star_hadd_variable, cod_harvest_perc_diff, cod_harvest_diff, hadd_harvest_perc_diff, hadd_harvest_diff)
  
  p_starz<-p_starz %>% as.data.frame() %>% dplyr::mutate(draw=i, year=y)
  
  p_star_values_i[[i]] <-p_starz
  

}
  p_star_values_all= rlist::list.stack(p_star_values_i, fill=TRUE)
  p_star_values_i_y[[y]]= p_star_values_all
  
}

p_star_values_all_y= rlist::list.stack(p_star_values_i_y, fill=TRUE)
saveRDS(p_star_values_all_y, file = paste0(input_data_cd, "p_star_values_2019_2021_outliers.rds"))
# saveRDS(p_star_values_all_y, file = paste0(input_data_cd, "p_star_values_2019_2021_outliers_missing.rds"))

# check whether calibration data exists for each draw. If not, re-run the calibration routine above 
# for that draw and combine with other calibration data 

# for (i in 1:100){
#   test<- readRDS(paste0(input_data_cd, "costs_data_",y,"draw", i,".rds"))
# }

# check<-readRDS(paste0(input_data_cd,  "p_star_values_2019_2021_outliers.rds")) %>%
#    dplyr::filter(year %in% c(2020, 2021) | (year == 2019 & !(draw %in% c(3, 8, 22, 26, 54, 60, 82, 90))))
#  
# check2<-readRDS(paste0(input_data_cd,  "p_star_values_2019_2021_outliers_missing.rds"))
# check3<- plyr::rbind.fill(check, check2)
# saveRDS(check3, file = paste0(input_data_cd, "p_star_values_2019_2021_outliers.rds"))


########################################
#Check correlation in projected catch files 

# directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE)) %>% 
#   dplyr::mutate(dtrip=round(dtrip)) %>% 
#   dplyr::filter(dtrip!=0) %>% dplyr::filter(year==2021) %>% dplyr::select(-year)
# 
# regs <- directed_trips %>% 
#   mutate(period2 = as.character(period2)) %>% 
#   mutate(n_trips = floor(dtrip), n_draws = n_draws) 
# 
# regs_check <- directed_trips %>% dplyr::select(period2, dtrip)
# 
# results_by_draw<-list()
# for (i in 1:100){
#   
# # Preliminary check on correlation
# catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "projection_catch_draw", i, ".csv"), show_col_types = FALSE))
#   
# cod_hadd_catch_data <- catch_data_all %>%
#     dplyr::right_join(regs_check, by="period2") %>% 
#     dplyr::select(-dtrip, -period) 
#   
# dtrip_wts<- regs %>%
#   dplyr::group_by(period2) %>%
#   dplyr::summarise(dtrip = sum(dtrip),
#                    .groups="drop") %>%
#   dplyr::ungroup()    %>%
#   mutate(weight = dtrip / sum(dtrip))
# 
# cod_hadd_catch_data_check <- cod_hadd_catch_data %>%
#   left_join(dtrip_wts, by = "period2")
# 
# cod_hadd_catch_data_check <- cod_hadd_catch_data_check %>%
#   slice_sample(n = 1000, weight_by = weight)
# 
# varnames <- names(cod_hadd_catch_data_check)
# 
# # Step 2: Identify all unique shared suffixes where both cod_* and had_* exist
# suffixes <- varnames %>%
#   grep("^cod_", ., value = TRUE) %>%
#   sub("^cod_", "", .) %>%
#   intersect(
#     varnames %>%
#       grep("^had_", ., value = TRUE) %>%
#       sub("^had_", "", .)
#   )
# 
# # Step 3: Loop through suffixes and compute Kendall's tau
# results <- data.frame(suffix = character(),
#                       tau = numeric(),
#                       p_value = numeric(),
#                       stringsAsFactors = FALSE)
# 
# for (suf in suffixes) {
#   cod_var <- paste0("cod_", suf)
#   had_var <- paste0("had_", suf)
# 
#   # Extract vectors
#   x <- cod_hadd_catch_data_check[[cod_var]]
#   y <- cod_hadd_catch_data_check[[had_var]]
# 
#   # Compute Kendall's tau
#   test <- cor.test(x, y, method = "kendall", use = "pairwise.complete.obs")
# 
#   # Store result
#   results <- rbind(results, data.frame(
#     suffix = suf,
#     tau = test$estimate,
#     p_value = test$p.value,
#     draw=i
#   ))
# }
# 
# results_by_draw[[i]]<-results
# }
# results_all<-list.stack(results_by_draw, fill=TRUE)
# 
# # Use extract to split the string into parts
# results_all <- results_all %>%
#   tidyr::extract(suffix, into = c("correlation", "copula", "decade"), 
#           regex = "^(corr|ind)_(clayton|gumbel|plackett|frank|gaussian)([1-8])$")
# 
# # Convert decade to numeric
# results_all$decade <- as.integer(results_all$decade)
# 
# results_summary<- results_all %>% 
#   dplyr::group_by(correlation, copula, decade) %>% 
#   dplyr::summarize(ktau=mean(tau), .groups="drop")
# 
# ggplot(results_all, aes(x=factor(decade), y=tau, color = factor(correlation)))+
#   geom_boxplot() +
#   labs(x = "Decade", y = "Kendall's tau")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))




#### A2) Decadal projections
# code duplicated for each year to avoid memory issues

 rm(list = ls())
 input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
 input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
 output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

 library(plyr)
 library(dplyr)

 conflicts_prefer(here::here)
 conflicts_prefer(dplyr::filter)
 conflicts_prefer(dplyr::select)
 conflicts_prefer(dplyr::mutate)
 conflicts_prefer(dplyr::rename)
 conflicts_prefer(dplyr::summarize)
 conflicts_prefer(dplyr::summarise)
 conflicts_prefer(dplyr::count)
 options(scipen = 999)

 n_draws<-100 #number of simulated choice occasions per period
 n_catch_draws<-30 #number of catch draws per choice occasions

 y<-2021

 # directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE)) %>%
 #   dplyr::mutate(dtrip=round(dtrip)) %>%
 #   dplyr::filter(dtrip!=0) %>% dplyr::filter(year==y) %>% dplyr::select(-year)

 # code for assigning zero to uncertain MRIP estimates
 directed_trips <- readr::read_csv(
   paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
   show_col_types = FALSE)   %>%
   dplyr::mutate(dtrip = round(dtrip)) %>%
   dplyr::mutate(
     dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
   dplyr::mutate(
     dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
   dplyr::mutate(
     dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
   dplyr::mutate(
     dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
   dplyr::mutate(
     dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
   dplyr::mutate(
     dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
   dplyr::filter(dtrip != 0) %>%
   dplyr::filter(year == y) %>% 
   dplyr::select(-year)
 
 regs <- directed_trips %>%
   mutate(period2 = as.character(period2)) %>%
   mutate(n_trips = floor(dtrip), n_draws = n_draws)

 regs_check <- directed_trips %>% dplyr::select(period2, dtrip)

 # Start the clock!
 ptm <- proc.time()

 k_taus<-list()
 output<-list()
 keep_rel_pairs_i<-list()

 for (i in 1:100){

   p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2021_outliers.rds")) %>%
      dplyr::filter(year==2021, draw==i)

   p_star_cod_variable<- p_starz$p_star_cod_variable
   p_star_hadd_variable<- p_starz$p_star_hadd_variable

   catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "projection_catch_draw", i, "_2021.csv"), show_col_types = FALSE))

   source(paste0(input_code_cd,"projection function new2.R"))

   k_taus[[i]]<-ktaus_all
   output[[i]]<-sims_all
   #keep_rel_pairs_i[[i]]<-keep_rel_pairs
 }

 ktau_all<-list.stack(k_taus, fill=TRUE)
 output_all<-list.stack(output, fill=TRUE)
 #keep_rel_pairs_all<-list.stack(keep_rel_pairs_i, fill=TRUE)

 # Stop the clock
 proc.time() - ptm # about 6 minutes per draw
 write_xlsx(output_all, paste0(output_data_cd,"model_output_y2021_outliers_10-8-25.xlsx"))  #save the data
 write_xlsx(ktau_all, paste0(output_data_cd,"ktau_output_y2021_outliers_10-8-25.xlsx"))  #save the data

# 2020
rm(list = ls())
input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)
options(scipen = 999)

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions

y<-2020

# code for assigning zero to uncertain MRIP estimates
directed_trips <- readr::read_csv(
  paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
  show_col_types = FALSE)   %>%
  dplyr::mutate(dtrip = round(dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::filter(dtrip != 0) %>%
  dplyr::filter(year == y) %>% 
  dplyr::select(-year)

regs <- directed_trips %>%
    mutate(period2 = as.character(period2)) %>%
    mutate(n_trips = floor(dtrip), n_draws = n_draws)

regs_check <- directed_trips %>% dplyr::select(period2, dtrip)


# Start the clock!
ptm <- proc.time()

k_taus<-list()
output<-list()
keep_rel_pairs_i<-list()

for (i in 57:100){

  p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2021_outliers.rds")) %>%
    dplyr::filter(year==2020, draw==i)

  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable

  catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "projection_catch_draw", i, "_2020.csv"), show_col_types = FALSE))

  source(paste0(input_code_cd,"projection function new2.R"))

  k_taus[[i]]<-ktaus_all
  output[[i]]<-sims_all
  #keep_rel_pairs_i[[i]]<-keep_rel_pairs
}

ktau_all<-list.stack(k_taus, fill=TRUE)
output_all<-list.stack(output, fill=TRUE)
#keep_rel_pairs_all<-list.stack(keep_rel_pairs_i, fill=TRUE)

# Stop the clock
proc.time() - ptm # about 6 minutes per draw
write_xlsx(output_all, paste0(output_data_cd,"model_output_y2020_outliers_10-8-25.xlsx"))  #save the data
write_xlsx(ktau_all, paste0(output_data_cd,"ktau_output_y2020_outliers_10-8-25.xlsx"))  #save the data

# 2019 
rm(list = ls())
input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)
options(scipen = 999)

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions

y<-2019

# code for assigning zero to uncertain MRIP estimates
directed_trips <- readr::read_csv(
  paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
  show_col_types = FALSE)   %>%
  dplyr::mutate(dtrip = round(dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::filter(dtrip != 0) %>%
  dplyr::filter(year == y) %>% 
  dplyr::select(-year)

regs <- directed_trips %>%
  mutate(period2 = as.character(period2)) %>%
  mutate(n_trips = floor(dtrip), n_draws = n_draws)

regs_check <- directed_trips %>% dplyr::select(period2, dtrip)

# Start the clock!
ptm <- proc.time()

k_taus<-list()
output<-list()
keep_rel_pairs_i<-list()

for (i in 1:100){

  p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2021_outliers.rds")) %>%
    dplyr::filter(year==2019, draw==i)

  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable

  catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "projection_catch_draw", i, "_2019.csv"), show_col_types = FALSE))

  source(paste0(input_code_cd,"projection function new2.R"))

  k_taus[[i]]<-ktaus_all
  output[[i]]<-sims_all
  #keep_rel_pairs_i[[i]]<-keep_rel_pairs
}

ktau_all<-list.stack(k_taus, fill=TRUE)
output_all<-list.stack(output, fill=TRUE)
#keep_rel_pairs_all<-list.stack(keep_rel_pairs_i, fill=TRUE)

# Stop the clock
proc.time() - ptm # about 6 minutes per draw
write_xlsx(output_all, paste0(output_data_cd,"model_output_y2019_outliers_10-8-25.xlsx"))  #save the data
write_xlsx(ktau_all, paste0(output_data_cd,"ktau_output_y2019_outliers_10-8-25.xlsx"))  #save the data


# # Load and process projected bottom temp data
# bt_temp_project <- read_csv(file.path(input_data_cd, "decadal_bottom_temp_distributions_levels_3_12_2025.csv"), show_col_types = FALSE) %>%
#   mutate(
#     lower = average_temp - 1.96 * sd_temp,
#     upper = average_temp + 1.96 * sd_temp
#   ) %>%
#   group_by(decade) %>%
#   mutate(mean_decadal_tmp = mean(average_temp)) %>%
#   rename(decade2 = decade) %>%
#   dplyr::mutate(decade = case_when(
#     decade2 == 1 ~ "2021-2030",
#     decade2 == 2 ~ "2031-2040",
#     decade2 == 3 ~ "2041-2050",
#     decade2 == 4 ~ "2051-2060",
#     decade2 == 5 ~ "2061-2070",
#     decade2 == 6 ~ "2071-2080",
#     decade2 == 7 ~ "2081-2090",
#     decade2 == 8 ~ "2091-3000"
#   )) %>%
#   ungroup() %>%
#   select(-decade2) %>%
#   dplyr::rename(mean_temp = average_temp) %>%
#   mutate(month = as.numeric(month))
#
# # Load and process historical bottom temp data
# # By decade
# bt_temp_historical <- read_excel(file.path(input_data_cd, "hist_daily_bt_tmps_agg_GoM.xlsx")) %>%
#   mutate(
#     date = as.Date(date),
#     year = as.numeric(format(date, "%Y")),
#     month = as.numeric(format(date, "%m"))
#   ) %>%
#   group_by(year, month) %>%
#   summarise(mean_tmp_ym = mean(mean_tmp), .groups = "drop") %>%
#   mutate(decade = case_when(
#     year >= 1961 & year <= 1970 ~ "1961-1970",
#     year >= 1971 & year <= 1980 ~ "1971-1980",
#     year >= 1981 & year <= 1990 ~ "1981-1990",
#     year >= 1991 & year <= 2000 ~ "1991-2000",
#     year >= 2001 & year <= 2010 ~ "2001-2010",
#     year >= 2011 & year <= 2020 ~ "2011-2020"
#   )) %>%
#   filter(!is.na(decade)) %>%
#   group_by(decade, month) %>%
#   mutate(
#     mean_temp = mean(mean_tmp_ym),
#     sd_temp = sd(mean_tmp_ym),
#     lower = mean_temp - 1.96 * sd_temp,
#     upper = mean_temp + 1.96 * sd_temp
#   ) %>%
#   ungroup() %>%
#   group_by(decade) %>%
#   mutate(mean_decadal_tmp = mean(mean_temp)) %>%
#   ungroup()
#
# historical_and_projections <- bind_rows(bt_temp_historical, bt_temp_project)
# write_xlsx(historical_and_projections, paste0(output_data_cd,"bottom_temp_reformat1.xlsx"))  #save the data
#
# # By year
# # Load and process historical data
# bt_temp_historical_annual <- read_excel(file.path(input_data_cd, "hist_daily_bt_tmps_agg_GoM.xlsx")) %>%
#   mutate(
#     date = as.Date(date),
#     year = as.numeric(format(date, "%Y")),
#     month = as.numeric(format(date, "%m"))
#   ) %>%
#   group_by(year, month) %>%
#   summarise(mean_tmp_ym = mean(mean_tmp), .groups = "drop") %>%
#   dplyr::mutate(decade = case_when(
#     year == 2011  ~ "2011",
#     year == 2012  ~ "2012",
#     year == 2013  ~ "2013",
#     year == 2014  ~ "2014",
#     year == 2015  ~ "2015",
#     year == 2016  ~ "2016",
#     year == 2017  ~ "2017",
#     year == 2018  ~ "2018",
#     year == 2019  ~ "2019",
#     year == 2020  ~ "2020",
#     year == 2021  ~ "2021",
#     year == 2022  ~ "2022")) %>%
#   filter(!is.na(decade)) %>%
#   group_by(decade, month) %>%
#   mutate(
#     mean_temp = mean(mean_tmp_ym),
#     sd_temp = sd(mean_tmp_ym),
#     lower = mean_temp - 1.96 * sd_temp,
#     upper = mean_temp + 1.96 * sd_temp
#   ) %>%
#   ungroup() %>%
#   group_by(decade) %>%
#   mutate(mean_decadal_tmp = mean(mean_temp)) %>%
#   ungroup()
#
# historical_and_projections_annual <- bind_rows(bt_temp_historical_annual, bt_temp_project)
# write_xlsx(historical_and_projections_annual, paste0(output_data_cd,"bottom_temp_reformat2.xlsx"))  #save the data


# Process the input catch-per-trip data
# Read the projected catch per trip .dta
# proj_data <- read_dta("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/input_data/projection_catch_base_wide.dta") %>%
#   rename(month = month1, mode = mode1) %>%
#   mutate(state = case_when(
#     state == "MA" ~ 25,
#     state == "ME" ~ 23,
#     state == "NH" ~ 33,
#     TRUE ~ NA_real_
#   ))
#
# # Collapse (mean) of had* and cod* by month/mode/area/state
# proj_data <- proj_data %>%
#   dplyr::group_by(month, mode, area, state) %>%
#   dplyr::summarise(across(starts_with("had"), \(x) mean(x, na.rm = TRUE)),
#                    across(starts_with("cod"),\(x) mean(x, na.rm = TRUE)),
#                    .groups = "drop")
#
# # Reshape wide to long format for multiple 'decade' groups
# proj_data_long <- proj_data %>%
#   pivot_longer(
#     cols = c(
#       starts_with("cod_corr_clayton"), starts_with("cod_ind_clayton"),
#       starts_with("had_corr_clayton"), starts_with("had_ind_clayton"),
#       starts_with("cod_corr_plackett"), starts_with("cod_ind_plackett"),
#       starts_with("had_corr_plackett"), starts_with("had_ind_plackett"),
#       starts_with("cod_corr_gaussian"), starts_with("cod_ind_gaussian"),
#       starts_with("had_corr_gaussian"), starts_with("had_ind_gaussian"),
#       starts_with("cod_corr_frank"), starts_with("cod_ind_frank"),
#       starts_with("had_corr_frank"), starts_with("had_ind_frank"),
#       starts_with("cod_corr_gumbel"), starts_with("cod_ind_gumbel"),
#       starts_with("had_corr_gumbel"), starts_with("had_ind_gumbel")
#     ),
#     names_to = c(".value", "decade"),
#     names_pattern = "(cod_corr_clayton|had_corr_clayton|cod_ind_clayton|had_ind_clayton|cod_corr_plackett|had_corr_plackett|cod_ind_plackett|had_ind_plackett|cod_corr_gaussian|had_corr_gaussian|cod_ind_gaussian|had_ind_gaussian|cod_corr_frank|had_corr_frank|cod_ind_frank|had_ind_frank|cod_corr_gumbel|had_corr_gumbel|cod_ind_gumbel|had_ind_gumbel)(.+)"
#   )
#
# # Read the directed trips CSV
# directed_trips <- fread("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/input_data/directed trips and regulations 2010_2020_disaggregated.csv") %>%
#   filter(year == 2021) %>%
#   rename(state = st)
#
# # Collapse (sum) dtrip by month/mode/area/state
# directed_trips_summarized <- directed_trips %>%
#   group_by(month, mode, area, state) %>%
#   summarise(dtrip = sum(dtrip, na.rm = TRUE), .groups = "drop")
#
# # Merge with reshaped projection data
# merged_data <- directed_trips_summarized %>%
#   left_join(proj_data_long, by = c("month", "mode", "area", "state")) %>%
#   dplyr::filter(!is.na(decade)) %>%  # equivalent to dropping _merge==1
#   select(-contains("ind"))  # drop *ind*
#
# # Collapse weighted means by decade
# final_result <- merged_data %>%
#   group_by(decade) %>%
#   summarise(
#     cod_corr_clayton = weighted.mean(cod_corr_clayton, dtrip, na.rm = TRUE),
#     had_corr_clayton = weighted.mean(had_corr_clayton, dtrip, na.rm = TRUE),
#     cod_corr_frank = weighted.mean(cod_corr_frank, dtrip, na.rm = TRUE),
#     had_corr_frank = weighted.mean(had_corr_frank, dtrip, na.rm = TRUE),
#     cod_corr_gumbel = weighted.mean(cod_corr_gumbel, dtrip, na.rm = TRUE),
#     had_corr_gumbel = weighted.mean(had_corr_gumbel, dtrip, na.rm = TRUE),
#     cod_corr_plackett = weighted.mean(cod_corr_plackett, dtrip, na.rm = TRUE),
#     had_corr_plackett = weighted.mean(had_corr_plackett, dtrip, na.rm = TRUE),
#     cod_corr_gaussian = weighted.mean(cod_corr_gaussian, dtrip, na.rm = TRUE),
#     had_corr_gaussian = weighted.mean(had_corr_gaussian, dtrip, na.rm = TRUE),
#     # If cod_ind and had_ind are still there
#     #cod_ind = weighted.mean(cod_ind, dtrip, na.rm = TRUE),
#     #had_ind = weighted.mean(had_ind, dtrip, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   select(-contains("ind"))  # drop *ind*
#
# write_xlsx(final_result, paste0(output_data_cd,"model_input_proj_data.xlsx"))  #save the data



###### PART B ##########
# rm(list = ls())
# input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
# input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
# output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"
# 
# n_draws<-100 #number of simulated choice occasions per period
# n_catch_draws<-30 #number of catch draws per choice occasions
# 
# library(plyr)
# library(dplyr)
# 
# conflicts_prefer(here::here)
# conflicts_prefer(dplyr::filter)
# conflicts_prefer(dplyr::select)
# conflicts_prefer(dplyr::mutate)
# conflicts_prefer(dplyr::rename)
# conflicts_prefer(dplyr::summarize)
# conflicts_prefer(dplyr::summarise)
# conflicts_prefer(dplyr::count)
# options(scipen = 999)
# 
# ##### B1) Calibration
# 
# p_star_values_i<-list()
# p_star_values_i_y<-list()
# 
# yrz<- c(2019, 2020, 2021)
# for (y in yrz){
# 
#   for (i in 1:100){
# 
#     seed<-i
#     
#     # Pull in calibration-year catch draws
#     catch_data_all_split <- readr::read_csv(
#       paste0(input_data_cd,  "calib_catch_yr", y, "_draw", i, ".csv"), show_col_types = FALSE) %>% 
#       dplyr::mutate(
#         omit = dplyr::if_else(
#           state == 23 & year == 2019 & month %in% c(5, 6), 1,0)) %>%
#       dplyr::mutate(
#         omit = dplyr::if_else(
#           state == 23 & year == 2019 & month %in% c(9,10), 1,omit)) %>%
#       dplyr::mutate(
#         omit = dplyr::if_else(
#           state == 23 & year == 2020 & month %in% c(5,6), 1,omit)) %>%
#       dplyr::mutate(
#         omit = dplyr::if_else(state == 23 & year == 2020 & month %in% c(9,10), 1,omit)) %>%
#       dplyr::mutate(
#         omit = dplyr::if_else(
#           state == 25 & year == 2020 & month %in% c(11,12), 1,omit)) %>%
#       dplyr::mutate(
#         omit = dplyr::if_else(
#           state == 23 & year == 2021 & month %in% c(9,10), 1,omit)) %>%
#       dplyr::filter(omit != 1) %>%
#       filter(tripid<=100)
# 
#      catch_data_all_split <- catch_data_all_split %>%
#       dplyr::group_by(state, mode, area, month) %>%
#       dplyr::mutate(
#         tot_cat_cod_shuffled  = sample(tot_cat_cod),
#         tot_cat_hadd_shuffled = sample(tot_cat_hadd)
#       ) %>%
#       ungroup()
# 
#     # #Compute and display per-group correlation before and after shuffling
#     # cor_by_group <- catch_data_all_split %>%
#     #   group_by(state, mode, area, month) %>%
#     #   slice_sample(n=1000) %>%
#     #   summarise(
#     #     #cor_before = cor(tot_cat_cod, tot_cat_hadd, use = "complete.obs"),
#     #     #cor_after  = cor(tot_cat_cod_shuffled, tot_cat_hadd, use = "complete.obs"),
#     #     cor_before = cor.test(tot_cat_cod, tot_cat_hadd, method = c("kendall"))[["estimate"]],
#     #     cor_after  = cor.test(tot_cat_cod_shuffled, tot_cat_hadd, method = c("kendall"))[["estimate"]],
#     #     n = n()
#     #   ) %>%
#     #   ungroup()
#     # 
#     # ggplot(cor_by_group, aes(x = cor_before, y = cor_after)) +
#     #   geom_point(alpha = 0.6) +
#     #   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
#     #   coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
#     #   labs(x = "Before shuffling", y = "After shuffling",
#     #        title = "Per-Group Correlation: tot_cat_cod vs tot_cat_hadd")
# 
#     # Shuffle catch per-trip data to remove inter-species correlation 
#     catch_data_all_split <- catch_data_all_split %>%
#       dplyr::mutate(
#         tot_cat_cod  = tot_cat_cod_shuffled,
#         tot_cat_hadd = tot_cat_hadd_shuffled) %>%
#       dplyr::select(-tot_cat_cod_shuffled, -tot_cat_hadd_shuffled)
#     
#     # Directed trips and regulations by period
#     # code for assigning zero to uncetrain MRIP estimates
#     directed_trips <- readr::read_csv(
#       paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
#       show_col_types = FALSE)   %>%
#       dplyr::mutate(dtrip = round(dtrip)) %>%
#       dplyr::mutate(
#         dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
#       dplyr::mutate(
#         dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
#       dplyr::mutate(
#         dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
#       dplyr::mutate(
#         dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
#       dplyr::mutate(
#         dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
#       dplyr::mutate(
#         dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
#       dplyr::filter(dtrip != 0) %>%
#       dplyr::filter(year == y) %>% 
#       dplyr::select(-year)
#     
#     directed_trips_p <- directed_trips %>%
#       mutate(period2 = as.character(period2)) %>% mutate(n_trips = floor(dtrip), n_draws = n_draws)
# 
#     regs <- directed_trips_p %>% dplyr::select(period2, cod_bag, cod_min, hadd_bag, hadd_min, dtrip)
# 
#     #number of periods
#     n_periods<-length(unique(regs$period2))
# 
#     regs_check <- directed_trips_p %>%   dplyr::select(period2, dtrip)
# 
#     MRIP_data <- catch_data_all_split %>%
#       dplyr::group_by(period2) %>%
#       dplyr::summarise(mean_cod_keep = mean(landing_cod),
#                        mean_hadd_keep = mean(landing_hadd),
#                        mean_hadd_cat = mean(tot_cat_hadd),
#                        mean_cod_cat = mean(tot_cat_cod),.groups="drop") %>%
#       dplyr::ungroup() %>%
#       dplyr::left_join(regs_check, by="period2") %>%
#       mutate(cod_harvest=mean_cod_keep*dtrip,
#              hadd_harvest=mean_hadd_keep*dtrip,
#              cod_tot_cat=mean_cod_cat*dtrip,
#              hadd_tot_cat=mean_hadd_cat*dtrip) %>%
#       dplyr::summarise(cod_harvest = sum(cod_harvest),
#                        hadd_harvest = sum(hadd_harvest),
#                        cod_tot_cat = sum(cod_tot_cat),
#                        hadd_tot_cat = sum(hadd_tot_cat),.groups="drop")
# 
#     # Run the p-star routine and save the output
#     source(paste0(input_code_cd,"find p-star values 2010-2020_ind.R")) # this calls "calibration loop2 2010-2020.R"
# 
#     p_starz<-cbind(p_star_cod_variable, p_star_hadd_variable, cod_harvest_perc_diff, cod_harvest_diff, hadd_harvest_perc_diff, hadd_harvest_diff)
# 
#     p_starz<-p_starz %>% as.data.frame() %>% dplyr::mutate(draw=i, year=y)
# 
#     p_star_values_i[[i]] <-p_starz
# 
# 
#  }
#   p_star_values_all= rlist::list.stack(p_star_values_i, fill=TRUE)
#   p_star_values_i_y[[y]]= p_star_values_all
# 
# }
# p_star_values_all_y= rlist::list.stack(p_star_values_i_y, fill=TRUE)
# 
# saveRDS(p_star_values_all_y, file = paste0(input_data_cd, "p_star_values_2019_2021_ind_outliers.rds"))


#### B2) Decadal projections

# 2021 

rm(list = ls())
library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)

input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

y<-2021

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions

#code for assigning zero to uncertain MRIP estimates
directed_trips <- readr::read_csv(
  paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
  show_col_types = FALSE)   %>%
  dplyr::mutate(dtrip = round(dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::filter(dtrip != 0) %>%
  dplyr::filter(year == y) %>% 
  dplyr::select(-year)

regs <- directed_trips %>% 
  mutate(period2 = as.character(period2)) %>% 
  mutate(n_trips = floor(dtrip), n_draws = n_draws) 

regs_check <- directed_trips %>% dplyr::select(period2, dtrip)

# Start the clock!
ptm <- proc.time()

k_taus<-list()
output<-list()
keep_rel_pairs_i<-list()

for (i in 1:100){
  
  p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2021_ind_outliers.rds")) %>% 
    dplyr::filter(year==2021, draw==i)

  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable
  
  catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "projection_catch_draw", i, "_2021.csv"), show_col_types = FALSE))
  
  source(paste0(input_code_cd,"projection function new2_ind.R")) 
  
  k_taus[[i]]<-ktaus_all
  output[[i]]<-sims_all
  #keep_rel_pairs_i[[i]]<-keep_rel_pairs
}

ktau_all<-list.stack(k_taus, fill=TRUE)
output_all<-list.stack(output, fill=TRUE)
# Stop the clock
proc.time() - ptm # about 6 minutes per draw

write_xlsx(output_all, paste0(output_data_cd,"model_output_y2021_outliers_10-8-25_ind.xlsx"))  #save the data
write_xlsx(ktau_all, paste0(output_data_cd,"ktau_output_y2021_outliers_10-8-25_ind.xlsx"))  #save the data


# 2020
rm(list = ls())
library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)

input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

y<-2020

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions

#code for assigning zero to uncertain MRIP estimates
directed_trips <- readr::read_csv(
  paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
  show_col_types = FALSE)   %>%
  dplyr::mutate(dtrip = round(dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::filter(dtrip != 0) %>%
  dplyr::filter(year == y) %>% 
  dplyr::select(-year)

regs <- directed_trips %>% 
  mutate(period2 = as.character(period2)) %>% 
  mutate(n_trips = floor(dtrip), n_draws = n_draws) 

regs_check <- directed_trips %>% dplyr::select(period2, dtrip)

# Start the clock!
ptm <- proc.time()

k_taus<-list()
output<-list()
keep_rel_pairs_i<-list()

for (i in 1:100){

  p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2021_ind_outliers.rds")) %>% 
    dplyr::filter(year==y, draw==i)
  
  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable
  
  catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "projection_catch_draw", i, "_2020.csv"), show_col_types = FALSE))
  
  source(paste0(input_code_cd,"projection function new2_ind.R")) 
  
  k_taus[[i]]<-ktaus_all
  output[[i]]<-sims_all
  #keep_rel_pairs_i[[i]]<-keep_rel_pairs
}

ktau_all<-list.stack(k_taus, fill=TRUE)
output_all<-list.stack(output, fill=TRUE)

# Stop the clock
proc.time() - ptm # about 6 minutes per draw

write_xlsx(output_all, paste0(output_data_cd,"model_output_y2020_outliers_10-8-25_ind.xlsx"))  #save the data
write_xlsx(ktau_all, paste0(output_data_cd,"ktau_output_y2020_outliers_10-8-25_ind.xlsx"))  #save the data

# 2019 
rm(list = ls())
library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)

input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

y<-2019

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions

#code for assigning zero to uncertain MRIP estimates
directed_trips <- readr::read_csv(
  paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
  show_col_types = FALSE)   %>%
  dplyr::mutate(dtrip = round(dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::filter(dtrip != 0) %>%
  dplyr::filter(year == y) %>% 
  dplyr::select(-year)

regs <- directed_trips %>% 
  mutate(period2 = as.character(period2)) %>% 
  mutate(n_trips = floor(dtrip), n_draws = n_draws) 

regs_check <- directed_trips %>% dplyr::select(period2, dtrip)

# Start the clock!
ptm <- proc.time()

k_taus<-list()
output<-list()
keep_rel_pairs_i<-list()

for (i in 1:100){
  
  p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2021_ind_outliers.rds")) %>% 
    dplyr::filter(year==y, draw==i)
  
  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable
  
  catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "projection_catch_draw", i, "_2019.csv"), show_col_types = FALSE))
  
  source(paste0(input_code_cd,"projection function new2_ind.R")) 
  
  k_taus[[i]]<-ktaus_all
  output[[i]]<-sims_all
  #keep_rel_pairs_i[[i]]<-keep_rel_pairs
}

ktau_all<-rlist::list.stack(k_taus, fill=TRUE)
output_all<-rlist::list.stack(output, fill=TRUE)

# Stop the clock
proc.time() - ptm # about 6 minutes per draw
write_xlsx(output_all, paste0(output_data_cd,"model_output_y2019_outliers_10-8-25_ind.xlsx"))  #save the data
write_xlsx(ktau_all, paste0(output_data_cd,"ktau_output_y2019_outliers_10-8-25_ind.xlsx"))  #save the data


#### Re-format the output data #### 

# Coastwide/mode-wide data aggregation
disc_mortality<- read_csv(paste0(input_data_cd, "Discard_Mortality.csv"), show_col_types = FALSE)
library(haven)

for(yr in c(2019, 2020, 2021)){
  
yr_minus1=yr-1

model_output<- paste0("model_output_y", yr, "_outliers_10-8-25_ind.xlsx")

historical_catch <- read_dta(paste0(input_data_cd,"cod_hadd_catch_data_1_15.dta"))
historical_catch<-historical_catch %>% 
  dplyr::filter(year>=2011 & year<=yr_minus1) %>% 
  dplyr::mutate(dtrip=1) %>% 
  dplyr::group_by(year) %>%
  summarise(across(c(tot_cat_cod, tot_cat_hadd, dtrip), \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% 
  dplyr::mutate(codcattrip=tot_cat_cod/dtrip, 
                haddcattrip=tot_cat_hadd/dtrip, 
                copula="baseline", 
                correlation="baseline", decade=0) %>% 
  dplyr::ungroup()

base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_base,
                haddcat=tot_cat_hadd_base,
                codkeep=tot_keep_cod_base,
                haddkeep=tot_keep_hadd_base,
                codrel=tot_rel_cod_base,
                haddrel=tot_rel_hadd_base, 
                ntrips=ntrips_base) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, 
                     haddcat, haddkeep, haddrel, hadd_dead_disc, hadd_tot_mort, 
                     ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()

base_output_summarized_ind<-base_output_summarized %>% 
    dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips,
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                year=yr, decade=0) %>%
  group_by(year, draw, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort,
                     haddcat, haddkeep, haddrel, hadd_dead_disc, hadd_tot_mort, 
                     ntrips, n_choice_occasions, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(copula="independent") 

  
# Decadal projections- independent residuals
output_summarized<-read_excel(paste0(output_data_cd,model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_new,
                haddcat=tot_cat_hadd_new,
                codkeep=tot_keep_cod_new,
                haddkeep=tot_keep_hadd_new,
                codrel=tot_rel_cod_new,
                haddrel=tot_rel_hadd_new, 
                ntrips=ntrips_alt, 
                cv=change_CS) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, 
                     haddcat, haddkeep, haddrel, hadd_dead_disc, hadd_tot_mort, 
                     ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

output_summarized<-output_summarized %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                cvtrip=cv/ntrips,
                cv_choice=cv/n_choice_occasions) %>% 
  dplyr::mutate(correlation="independent") %>% 
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, 
                     haddcat, haddkeep, haddrel, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice, 
                     ntrips, n_choice_occasions, 
                     cv, cvtrip, cv_choice,
                     cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  


#Decadal projections - correlated residuals
model_output<- paste0("model_output_y", yr,"_outliers_10-8-25.xlsx")

base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_base,
                haddcat=tot_cat_hadd_base,
                codkeep=tot_keep_cod_base,
                haddkeep=tot_keep_hadd_base,
                codrel=tot_rel_cod_base,
                haddrel=tot_rel_hadd_base, 
                ntrips=ntrips_base) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
    group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()

base_output_summarized_corr<-base_output_summarized %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips,
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                year=yr, decade=0) %>%
  group_by(year, draw, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(copula="observed") 


output_summarized_corr<-read_excel(paste0(output_data_cd,model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_new,
                haddcat=tot_cat_hadd_new,
                codkeep=tot_keep_cod_new,
                haddkeep=tot_keep_hadd_new,
                codrel=tot_rel_cod_new,
                haddrel=tot_rel_hadd_new, 
                ntrips=ntrips_alt, 
                cv=change_CS) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

output_summarized_corr<-output_summarized_corr %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                cvtrip=cv/ntrips,
                cv_choice=cv/n_choice_occasions) %>% 
  #dplyr::mutate(copula="independent") %>% 
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice, 
                     ntrips, n_choice_occasions, 
                     cv, cvtrip, cv_choice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  


#Append the baseline output
output_combined <- output_summarized_corr %>%
  plyr::rbind.fill(output_summarized, base_output_summarized_corr, 
                   base_output_summarized_ind) #, historical_catch) 

output_combined0 <- output_combined %>%
  mutate(period = case_when(
    !is.na(year) & year >= 2011 & year <= yr ~ as.character(year),
    !is.na(decade) & decade >= 1 & decade <= 8 ~ paste0("Decade ", decade),
    TRUE ~ NA_character_
  ))

output_combined0$period <- as.character(output_combined0$period)
str(output_combined0$period)

write_rds(output_combined0, paste0(output_data_cd,"model_output_y", yr,"_outliers_10-8-25_reformat_coast.rds"))  #save the data


# Coastwide by fishing mode data aggregation
model_output<- paste0("model_output_y",yr,"_outliers_10-8-25_ind.xlsx")

base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_base,
                haddcat=tot_cat_hadd_base,
                codkeep=tot_keep_cod_base,
                haddkeep=tot_keep_hadd_base,
                codrel=tot_rel_cod_base,
                haddrel=tot_rel_hadd_base, 
                ntrips=ntrips_base)  %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, mode) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()

base_output_summarized_ind<-base_output_summarized %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips,
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                year=yr, decade=0) %>%
  group_by(year, draw, decade, mode) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(copula="independent") 


# Decadal projections- independent residuals
output_summarized<-read_excel(paste0(output_data_cd,model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_new,
                haddcat=tot_cat_hadd_new,
                codkeep=tot_keep_cod_new,
                haddkeep=tot_keep_hadd_new,
                codrel=tot_rel_cod_new,
                haddrel=tot_rel_hadd_new, 
                ntrips=ntrips_alt, 
                cv=change_CS)  %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, mode) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

output_summarized<-output_summarized %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                cvtrip=cv/ntrips,
                cv_choice=cv/n_choice_occasions) %>% 
  dplyr::mutate(correlation="independent") %>% 
  group_by(draw, copula, correlation, decade, mode) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice, 
                     ntrips, n_choice_occasions, 
                     cv, cvtrip, cv_choice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  


#Decadal projections - correlated residuals
model_output<- paste0("model_output_y",yr,"_outliers_10-8-25.xlsx")

base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_base,
                haddcat=tot_cat_hadd_base,
                codkeep=tot_keep_cod_base,
                haddkeep=tot_keep_hadd_base,
                codrel=tot_rel_cod_base,
                haddrel=tot_rel_hadd_base, 
                ntrips=ntrips_base)  %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, mode) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()

base_output_summarized_corr<-base_output_summarized %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips,
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                year=yr, decade=0) %>%
  group_by(year, draw, decade, mode) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(copula="observed") 


output_summarized_corr<-read_excel(paste0(output_data_cd,model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_new,
                haddcat=tot_cat_hadd_new,
                codkeep=tot_keep_cod_new,
                haddkeep=tot_keep_hadd_new,
                codrel=tot_rel_cod_new,
                haddrel=tot_rel_hadd_new, 
                ntrips=ntrips_alt, 
                cv=change_CS)  %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, mode) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

output_summarized_corr<-output_summarized_corr %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                cvtrip=cv/ntrips,
                cv_choice=cv/n_choice_occasions) %>% 
  #dplyr::mutate(copula="independent") %>% 
  group_by(draw, copula, correlation, decade, mode) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice, 
                     ntrips, n_choice_occasions, 
                     cv, cvtrip, cv_choice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  


#Append the baseline output
output_combined <- output_summarized_corr %>%
  plyr::rbind.fill(output_summarized, base_output_summarized_corr, 
                   base_output_summarized_ind) #, historical_catch) 


output_combined0 <- output_combined %>%
  mutate(period = case_when(
    !is.na(year) & year >= 2011 & year <= yr ~ as.character(year),
    !is.na(decade) & decade >= 1 & decade <= 8 ~ paste0("Decade ", decade),
    TRUE ~ NA_character_
  ))

output_combined0$period <- as.character(output_combined0$period)
str(output_combined0$period)

write_rds(output_combined0, paste0(output_data_cd,"model_output_y", yr,"_outliers_10-8-25_reformat_mode.rds"))  #save the data


# Model output by state aggregation

model_output<- paste0("model_output_y", yr,"_outliers_10-8-25_ind.xlsx")

base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_base,
                haddcat=tot_cat_hadd_base,
                codkeep=tot_keep_cod_base,
                haddkeep=tot_keep_hadd_base,
                codrel=tot_rel_cod_base,
                haddrel=tot_rel_hadd_base, 
                ntrips=ntrips_base)  %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, state) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()

base_output_summarized_ind<-base_output_summarized %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips,
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                year=yr, decade=0) %>%
  group_by(year, draw, decade, state) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(copula="independent") 


# Decadal projections- independent residuals
output_summarized<-read_excel(paste0(output_data_cd,model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_new,
                haddcat=tot_cat_hadd_new,
                codkeep=tot_keep_cod_new,
                haddkeep=tot_keep_hadd_new,
                codrel=tot_rel_cod_new,
                haddrel=tot_rel_hadd_new, 
                ntrips=ntrips_alt, 
                cv=change_CS)  %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, state) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

output_summarized<-output_summarized %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                cvtrip=cv/ntrips,
                cv_choice=cv/n_choice_occasions) %>% 
  dplyr::mutate(correlation="independent") %>% 
  group_by(draw, copula, correlation, decade, state) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice, 
                     ntrips, n_choice_occasions, 
                     cv, cvtrip, cv_choice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  


#Decadal projections - correlated residuals
model_output<- paste0("model_output_y", yr,"_outliers_10-8-25.xlsx")

base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_base,
                haddcat=tot_cat_hadd_base,
                codkeep=tot_keep_cod_base,
                haddkeep=tot_keep_hadd_base,
                codrel=tot_rel_cod_base,
                haddrel=tot_rel_hadd_base, 
                ntrips=ntrips_base)  %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, state) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()

base_output_summarized_corr<-base_output_summarized %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips,
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                year=yr, decade=0) %>%
  group_by(year, draw, decade, state) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(copula="observed") 


output_summarized_corr<-read_excel(paste0(output_data_cd,model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_new,
                haddcat=tot_cat_hadd_new,
                codkeep=tot_keep_cod_new,
                haddkeep=tot_keep_hadd_new,
                codrel=tot_rel_cod_new,
                haddrel=tot_rel_hadd_new, 
                ntrips=ntrips_alt, 
                cv=change_CS)  %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, state) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

output_summarized_corr<-output_summarized_corr %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                cvtrip=cv/ntrips,
                cv_choice=cv/n_choice_occasions) %>% 
  #dplyr::mutate(copula="independent") %>% 
  group_by(draw, copula, correlation, decade, state) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice, 
                     ntrips, n_choice_occasions, 
                     cv, cvtrip, cv_choice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  


#Append the baseline output
output_combined <- output_summarized_corr %>%
  plyr::rbind.fill(output_summarized, base_output_summarized_corr, 
                   base_output_summarized_ind) #, historical_catch) 


output_combined0 <- output_combined %>%
  mutate(period = case_when(
    !is.na(year) & year >= 2011 & year <= yr ~ as.character(year),
    !is.na(decade) & decade >= 1 & decade <= 8 ~ paste0("Decade ", decade),
    TRUE ~ NA_character_
  ))

output_combined0$period <- as.character(output_combined0$period)
str(output_combined0$period)

write_rds(output_combined0, paste0(output_data_cd,"model_output_y", yr,"_outliers_10-8-25_reformat_state.rds"))  #save the data
}

########################
#### Re-format the output data BY MONTH#### 

# Coastwide/mode-wide data aggregation
disc_mortality<- read_csv(paste0(input_data_cd, "Discard_Mortality.csv"), show_col_types = FALSE)

library(haven)
for(yr in c(2019, 2020, 2021)){

yr_minus1=yr-1

model_output<- paste0("model_output_y",yr,"_outliers_10-8-25_ind.xlsx")

historical_catch <- read_dta(paste0(input_data_cd,"cod_hadd_catch_data_1_15.dta"))
historical_catch<-historical_catch %>% 
  dplyr::filter(year>=2011 & year<=yr_minus1) %>% 
  dplyr::mutate(dtrip=1)  %>% 
  dplyr::mutate(month=as.numeric(month)) %>% 
   dplyr::group_by(year, month) %>%
  summarise(across(c(tot_cat_cod, tot_cat_hadd, dtrip), \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% 
  dplyr::mutate(codcattrip=tot_cat_cod/dtrip, 
                haddcattrip=tot_cat_hadd/dtrip, 
                copula="baseline", 
                correlation="baseline", decade=0) %>% 
  dplyr::ungroup()

base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_base,
                haddcat=tot_cat_hadd_base,
                codkeep=tot_keep_cod_base,
                haddkeep=tot_keep_hadd_base,
                codrel=tot_rel_cod_base,
                haddrel=tot_rel_hadd_base, 
                ntrips=ntrips_base) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, 
                     haddcat, haddkeep, haddrel, hadd_dead_disc, hadd_tot_mort, 
                     ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()

base_output_summarized_ind<-base_output_summarized %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips,
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                year=yr, decade=0) %>%
  group_by(year, draw, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort,
                     haddcat, haddkeep, haddrel, hadd_dead_disc, hadd_tot_mort, 
                     ntrips, n_choice_occasions, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(copula="independent") 


# Decadal projections- independent residuals
output_summarized<-read_excel(paste0(output_data_cd,model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_new,
                haddcat=tot_cat_hadd_new,
                codkeep=tot_keep_cod_new,
                haddkeep=tot_keep_hadd_new,
                codrel=tot_rel_cod_new,
                haddrel=tot_rel_hadd_new, 
                ntrips=ntrips_alt, 
                cv=change_CS) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, 
                     haddcat, haddkeep, haddrel, hadd_dead_disc, hadd_tot_mort, 
                     ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

output_summarized<-output_summarized %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                cvtrip=cv/ntrips,
                cv_choice=cv/n_choice_occasions) %>% 
  dplyr::mutate(correlation="independent") %>% 
  group_by(draw, copula, correlation, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, 
                     haddcat, haddkeep, haddrel, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice, 
                     ntrips, n_choice_occasions, 
                     cv, cvtrip, cv_choice,
                     cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  


#Decadal projections - correlated residuals
model_output<- paste0("model_output_y",yr,"_outliers_10-8-25.xlsx")

base_output_summarized<-read_excel(paste0(output_data_cd, model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_base,
                haddcat=tot_cat_hadd_base,
                codkeep=tot_keep_cod_base,
                haddkeep=tot_keep_hadd_base,
                codrel=tot_rel_cod_base,
                haddrel=tot_rel_hadd_base, 
                ntrips=ntrips_base) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()

base_output_summarized_corr<-base_output_summarized %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips,
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                year=yr, decade=0) %>%
  group_by(year, draw, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(copula="observed") 


output_summarized_corr<-read_excel(paste0(output_data_cd,model_output)) %>% 
  separate(domain, into = c("correlation", "rest"), sep = "_") %>%
  mutate(copula = str_extract(rest, "^[a-z]+"),
         decade = as.integer(str_extract(rest, "[0-9]+"))) %>%
  select(-rest) %>% 
  dplyr::rename(codcat=tot_cat_cod_new,
                haddcat=tot_cat_hadd_new,
                codkeep=tot_keep_cod_new,
                haddkeep=tot_keep_hadd_new,
                codrel=tot_rel_cod_new,
                haddrel=tot_rel_hadd_new, 
                ntrips=ntrips_alt, 
                cv=change_CS) %>% 
  dplyr::left_join(disc_mortality, by="month") %>% 
  dplyr::mutate(cod_dead_disc=codrel*disc_mort_cod, 
                hadd_dead_disc=haddrel*disc_mort_hadd_sm) %>% 
  dplyr::mutate(cod_tot_mort=codkeep+cod_dead_disc, 
                hadd_tot_mort=haddkeep+hadd_dead_disc) %>% 
  group_by(draw, copula, correlation, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     ntrips, n_choice_occasions, cv), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

output_summarized_corr<-output_summarized_corr %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                codcatchoice=codcat/n_choice_occasions, 
                codkeepchoice=codkeep/n_choice_occasions, 
                haddcatchoice=haddcat/n_choice_occasions, 
                haddkeepchoice=haddkeep/n_choice_occasions,
                cvtrip=cv/ntrips,
                cv_choice=cv/n_choice_occasions) %>% 
  group_by(draw, copula, correlation, decade, month) %>%
  summarise(across(c(codcat, codkeep, codrel, cod_dead_disc, cod_tot_mort, hadd_dead_disc, hadd_tot_mort,
                     haddcat, haddkeep, haddrel, 
                     codcattrip, codkeeptrip, 
                     haddcattrip, haddkeeptrip, 
                     codcatchoice, codkeepchoice, 
                     haddcatchoice, haddkeepchoice, 
                     ntrips, n_choice_occasions, 
                     cv, cvtrip, cv_choice), \(x) mean(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  


#Append the baseline output
output_combined <- output_summarized_corr %>%
  plyr::rbind.fill(output_summarized, base_output_summarized_corr, 
                   base_output_summarized_ind) #, historical_catch) 


output_combined0 <- output_combined %>%
  mutate(period = case_when(
    !is.na(year) & year >= 2011 & year <= yr ~ as.character(year),
    !is.na(decade) & decade >= 1 & decade <= 8 ~ paste0("Decade ", decade),
    TRUE ~ NA_character_
  ))

output_combined0$period <- as.character(output_combined0$period)
str(output_combined0$period)


write_rds(output_combined0, paste0(output_data_cd,"model_output_y",yr,"_outliers_10-8-25_reformat_coast_mnth.rds"))  #save the data
}


########################
# Auxillary simulations - close cod, close cod and haddock
########################

#### C1) close cod

rm(list = ls())
library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)

input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions

for(yr in c(2019, 2020, 2021)){
  
directed_trips <- readr::read_csv(
  paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
  show_col_types = FALSE)   %>%
  dplyr::mutate(dtrip = round(dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::filter(dtrip != 0) %>%
  dplyr::filter(year == yr) %>% 
  dplyr::select(-year) %>% 
  dplyr::mutate(cod_bag=0) # set cod bag limit to zero

regs <- directed_trips %>% 
  mutate(period2 = as.character(period2)) %>% 
  mutate(n_trips = floor(dtrip), n_draws = n_draws) 

regs_check <- directed_trips %>% dplyr::select(period2, dtrip)

k_taus<-list()
output<-list()
keep_rel_pairs_i<-list()

for (i in 1:100){
  
  p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2021_outliers.rds")) %>%
    dplyr::filter(year==yr, draw==i)
  
  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable
  

  catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "calib_catch_yr", yr, "_draw", i, ".csv"), show_col_types = FALSE)) %>% 
    filter(tripid<=100)
  
  source(paste0(input_code_cd,"projection function new2 - close fishery.R")) 
  
  output[[i]]<-sims_all
}

output_all<-list.stack(output, fill=TRUE)
write_xlsx(output_all, paste0(output_data_cd,"model_output_y", yr, "_outliers_9-21-25_close cod.xlsx"))  #save the data
}


#### C2) close cod and haddock 
rm(list = ls())
library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)

input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions

for(yr in c(2019, 2020, 2021)){
  
directed_trips <- readr::read_csv(
  paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"),
  show_col_types = FALSE)   %>%
  dplyr::mutate(dtrip = round(dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2019 & month %in% c(5, 6), 0,dtrip))%>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2019 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2020 & month %in% c(5,6),0,dtrip )) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else(st == 23 & year == 2020 & month %in% c(9,10),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 25 & year == 2020 & month %in% c(11,12),0,dtrip)) %>%
  dplyr::mutate(
    dtrip = dplyr::if_else( st == 23 & year == 2021 & month %in% c(9,10), 0,dtrip)) %>%
  dplyr::filter(dtrip != 0) %>%
  dplyr::filter(year == yr) %>% 
  dplyr::select(-year) %>% 
  dplyr::mutate(cod_bag=0,hadd_bag=0) # set cod and haddock bag limit to zero

regs <- directed_trips %>% 
  mutate(period2 = as.character(period2)) %>% 
  mutate(n_trips = floor(dtrip), n_draws = n_draws) 

regs_check <- directed_trips %>% dplyr::select(period2, dtrip)

k_taus<-list()
output<-list()
keep_rel_pairs_i<-list()

for (i in 1:100){
  
  p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2021_outliers.rds")) %>%
    dplyr::filter(year==yr, draw==i)
  
  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable
  
  
  catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "calib_catch_yr", yr, "_draw", i, ".csv"), show_col_types = FALSE)) %>% 
    filter(tripid<=100)
  
  source(paste0(input_code_cd,"projection function new2 - close fishery.R")) 
  
  output[[i]]<-sims_all
}

output_all<-list.stack(output, fill=TRUE)
write_xlsx(output_all, paste0(output_data_cd,"model_output_y", yr, "_outliers_9-21-25_close cod and haddock.xlsx"))  #save the data

}


#format closure data
yr<-2019
close_cod19<-read_excel(paste0(output_data_cd,"model_output_y", yr, "_outliers_9-21-25_close cod and haddock.xlsx")) %>% 
  dplyr::rename(cv=change_CS) %>% 
  dplyr::group_by(draw)  %>% 
  summarise(across(c(cv, tot_keep_cod_new, tot_keep_hadd_new), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  %>% 
  dplyr::mutate(year=yr) %>% 
  dplyr::group_by(year)  %>% 
  summarise(mean_cv=mean(cv), sd_cv=sd(cv)) %>% 
  dplyr::ungroup() 

yr<-2020
close_cod20<-read_excel(paste0(output_data_cd,"model_output_y", yr, "_outliers_9-21-25_close cod and haddock.xlsx")) %>% 
  dplyr::rename(cv=change_CS) %>% 
  dplyr::group_by(draw)  %>% 
  summarise(across(c(cv, tot_keep_cod_new, tot_keep_hadd_new), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  %>% 
  dplyr::mutate(year=yr) %>% 
  dplyr::group_by(year)  %>% 
  summarise(mean_cv=mean(cv), sd_cv=sd(cv)) %>% 
  dplyr::ungroup()  

yr<-2021
close_cod21<-read_excel(paste0(output_data_cd,"model_output_y", yr, "_outliers_9-21-25_close cod and haddock.xlsx")) %>% 
  dplyr::rename(cv=change_CS) %>% 
  dplyr::group_by(draw)  %>% 
  summarise(across(c(cv, tot_keep_cod_new, tot_keep_hadd_new), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  %>% 
  dplyr::mutate(year=yr) %>% 
  dplyr::group_by(year)  %>% 
  summarise(mean_cv=mean(cv), sd_cv=sd(cv)) %>% 
  dplyr::ungroup() 

close_cod<-rbind.fill(close_cod19,close_cod20,close_cod21 ) 
head(close_cod)


#### C2) close cod and haddock - baseline year 2020
rm(list = ls())
library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)

#input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/input_data/"
input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions

directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE)) %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0) %>% dplyr::filter(year==2021) %>% dplyr::select(-year) %>% 
  dplyr::mutate(cod_bag=0, hadd_bag=0)

directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE)) %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% dplyr::filter(year>2018 & cod_bag>0) 

regs <- directed_trips %>% 
  mutate(period2 = as.character(period2)) %>% 
  mutate(n_trips = floor(dtrip), n_draws = n_draws) 

regs_check <- directed_trips %>% dplyr::select(period2, dtrip)

y<-2020

# Start the clock!
ptm <- proc.time()

k_taus<-list()
output<-list()
keep_rel_pairs_i<-list()

for (i in 1:100){
  
  #p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2010_2021.rds")) %>% 
  #  dplyr::filter(year==2021, draw==i)
  
  
  p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2020_ind.rds")) %>% 
    dplyr::filter(year==y, draw==i)
  
  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable
  
  
  catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "calib_catch_yr", y, "_draw", i, ".csv"), show_col_types = FALSE)) %>% 
    filter(tripid<=100)
  
  source(paste0(input_code_cd,"projection function new2 - close fishery.R")) 
  
  output[[i]]<-sims_all
}

output_all<-list.stack(output, fill=TRUE)

# Stop the clock
proc.time() - ptm # about 6 minutes per draw

write_xlsx(output_all, paste0(output_data_cd,"model_output_y2020_6-2-25_close both.xlsx"))  #save the data

#format closure data
close_cod<-read_excel(paste0(output_data_cd,"model_output_y2020_6-2-25_close both.xlsx")) %>% 
  dplyr::rename(cv=change_CS) %>% 
  dplyr::group_by(draw) %>% 
  summarise(across(c(cv, tot_keep_cod_base, tot_keep_hadd_base), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  
mean(close_cod$cv)
mean(close_cod$tot_keep_hadd_base)

close_cod_and_haddock<-read_excel(paste0(output_data_cd,"model_output_y2020_6-2-25_close both.xlsx")) %>% 
  dplyr::rename(cv=change_CS) %>% 
  dplyr::group_by(draw) %>% 
  summarise(across(c(cv, ), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

mean(close_cod_and_haddock$cv)


#### C2) close cod and haddock - baseline year 2019
rm(list = ls())
library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)

#input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/input_data/"
input_data_cd <- "E:/Lou_projects/welfare-model-GoM/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"

n_draws<-100 #number of simulated choice occasions per period
n_catch_draws<-30 #number of catch draws per choice occasions

directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE)) %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% 
  dplyr::filter(dtrip!=0) %>% dplyr::filter(year==2019) %>% dplyr::select(-year) %>% 
  dplyr::mutate(cod_bag=0, hadd_bag=0)

regs <- directed_trips %>% 
  mutate(period2 = as.character(period2)) %>% 
  mutate(n_trips = floor(dtrip), n_draws = n_draws) 

regs_check <- directed_trips %>% dplyr::select(period2, dtrip)

y<-2019

# Start the clock!
ptm <- proc.time()

k_taus<-list()
output<-list()
keep_rel_pairs_i<-list()

for (i in 1:100){
  
  #p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2010_2021.rds")) %>% 
  #  dplyr::filter(year==2021, draw==i)
  
  
  p_starz <- readRDS(paste0(input_data_cd, "p_star_values_2019_2020_ind.rds")) %>% 
    dplyr::filter(year==y, draw==i)
  
  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable
  
  
  catch_data_all <- data.frame(read_csv(paste0(input_data_cd, "calib_catch_yr", y, "_draw", i, ".csv"), show_col_types = FALSE)) %>% 
    filter(tripid<=100)
  
  source(paste0(input_code_cd,"projection function new2 - close fishery.R")) 
  
  output[[i]]<-sims_all
}

output_all<-list.stack(output, fill=TRUE)

# Stop the clock
proc.time() - ptm # about 6 minutes per draw

write_xlsx(output_all, paste0(output_data_cd,"model_output_y2019_6-2-25_close both.xlsx"))  #save the data

#format closure data
close_cod<-read_excel(paste0(output_data_cd,"model_output_y2019_6-2-25_close both.xlsx")) %>% 
  dplyr::rename(cv=change_CS) %>% 
  dplyr::group_by(draw) %>% 
  summarise(across(c(cv, tot_keep_cod_base, tot_keep_hadd_base), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  
mean(close_cod$cv)
mean(close_cod$tot_keep_hadd_base)

close_cod_and_haddock<-read_excel(paste0(output_data_cd,"model_output_y2019_6-2-25_close both.xlsx")) %>% 
  dplyr::rename(cv=change_CS) %>% 
  dplyr::group_by(draw) %>% 
  summarise(across(c(cv, ), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::ungroup()  

mean(close_cod_and_haddock$cv)

# compute baseline ktaus

# directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE)) %>% 
#   dplyr::mutate(dtrip=round(dtrip)) %>% 
#   dplyr::filter(dtrip!=0) %>% dplyr::filter(year==2021) %>% dplyr::select(-year)
# 
# regs <- directed_trips %>% 
#   mutate(period2 = as.character(period2)) %>% 
#   mutate(n_trips = floor(dtrip), n_draws = n_draws) 
# 
# regs_check <- directed_trips %>% dplyr::select(period2, dtrip)
# n_draws<-100 #number of simulated choice occasions per period
# n_catch_draws<-30 #number of catch draws per choice occasions
# 
# keep_rel_pairs_i<-list()
# 
# for (i in 1:100){
#   #i<-1
#   keep_rel_pairs <- readRDS(paste0(output_data_cd, "ktau_draws_2021draw", i, ".rds"))
#   keep_rel_pairs <- keep_rel_pairs %>%
#     left_join(regs_check, by = "period2")
#   keep_rel_pairs <- keep_rel_pairs %>%
#     slice_sample(n = 5000, weight_by = dtrip)
#   
#   
#   sum_keep_cod<-sum(keep_rel_pairs$tot_keep_cod)
#   sum_keep_hadd<-sum(keep_rel_pairs$tot_keep_hadd)
#   
#   sum_catch_cod<-sum(keep_rel_pairs$tot_cat_cod)
#   sum_catch_hadd<-sum(keep_rel_pairs$tot_cat_hadd)
#   
# 
#   if(sum_keep_cod>0 & sum_keep_hadd>0){
#     
#     ktau_keep<- cor.test(keep_rel_pairs$tot_keep_cod,
#                          keep_rel_pairs$tot_keep_hadd, method = c("kendall"))
#     
#     k_tau_keep_est<-ktau_keep[["estimate"]]
#     k_tau_keep_p<- ktau_keep[["p.value"]]
#   }
#   
#   if(sum_keep_cod==0 | sum_keep_hadd==0){
#     
#     k_tau_keep_est<-0
#     k_tau_keep_p<- 1
#   }
#   
#   
#   if(sum_catch_cod>0 & sum_catch_hadd>0){
#     
#     ktau_catch<- cor.test(keep_rel_pairs$tot_cat_cod,
#                           keep_rel_pairs$tot_cat_hadd, method = c("kendall"))
#     
#     k_tau_catch_est<-ktau_catch[["estimate"]]
#     k_tau_catch_p<- ktau_catch[["p.value"]]
#   }
#   
#   if(sum_catch_cod==0 | sum_catch_hadd==0){
#     
#     k_tau_catch_est<-0
#     k_tau_catch_p<- 1
#   }
#   
#   ktaus_annual<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p), names="TRUE")
#   ktaus_annual$domain<-"all"
#   ktaus_annual$draw<-i
#   ktaus_annual$month<-0
#   ktaus_annual$decade<-0
#   ktaus_annual$copula<-"observed"
#   
#   keep_rel_pairs_i[[i]]<-ktaus_annual
# }
#   ktau_all_obs<-list.stack(keep_rel_pairs_i, fill=TRUE)
#   
#   
#   
# #ind
#   keep_rel_pairs_i<-list()
#   
#   for (i in 1:100){
#     #i<-1
#     keep_rel_pairs <- readRDS(paste0(output_data_cd, "ktau_draws_2021draw", i, "_ind.rds"))
#     keep_rel_pairs <- keep_rel_pairs %>%
#       left_join(regs_check, by = "period2")
#     keep_rel_pairs <- keep_rel_pairs %>%
#       slice_sample(n = 5000, weight_by = dtrip)
#     
#     
#     sum_keep_cod<-sum(keep_rel_pairs$tot_keep_cod)
#     sum_keep_hadd<-sum(keep_rel_pairs$tot_keep_hadd)
#     
#     sum_catch_cod<-sum(keep_rel_pairs$tot_cat_cod)
#     sum_catch_hadd<-sum(keep_rel_pairs$tot_cat_hadd)
#     
#     
#     if(sum_keep_cod>0 & sum_keep_hadd>0){
#       
#       ktau_keep<- cor.test(keep_rel_pairs$tot_keep_cod,
#                            keep_rel_pairs$tot_keep_hadd, method = c("kendall"))
#       
#       k_tau_keep_est<-ktau_keep[["estimate"]]
#       k_tau_keep_p<- ktau_keep[["p.value"]]
#     }
#     
#     if(sum_keep_cod==0 | sum_keep_hadd==0){
#       
#       k_tau_keep_est<-0
#       k_tau_keep_p<- 1
#     }
#     
#     
#     if(sum_catch_cod>0 & sum_catch_hadd>0){
#       
#       ktau_catch<- cor.test(keep_rel_pairs$tot_cat_cod,
#                             keep_rel_pairs$tot_cat_hadd, method = c("kendall"))
#       
#       k_tau_catch_est<-ktau_catch[["estimate"]]
#       k_tau_catch_p<- ktau_catch[["p.value"]]
#     }
#     
#     if(sum_catch_cod==0 | sum_catch_hadd==0){
#       
#       k_tau_catch_est<-0
#       k_tau_catch_p<- 1
#     }
#     
#     ktaus_annual<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p), names="TRUE")
#     ktaus_annual$domain<-"all"
#     ktaus_annual$draw<-i
#     ktaus_annual$month<-0
#     ktaus_annual$decade<-0
#     ktaus_annual$copula<-"independent"
#     
#     keep_rel_pairs_i[[i]]<-ktaus_annual
#   }
#   ktau_all_ind<-list.stack(keep_rel_pairs_i, fill=TRUE)
#   
#   ktau_all_baseline<-ktau_all_ind %>%
#     plyr::rbind.fill(ktau_all_obs)
#   
#   
# write_rds(ktau_all_baseline, paste0(output_data_cd,"k_tau_baseline.rds"))  #save the data
  