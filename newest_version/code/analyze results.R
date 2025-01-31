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

new_output_cd<-"C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"



##Pull in the baseline-year calibration data 
ktaus<-list()
for (x in 1:50){
  ktaus[[x]] <- readRDS(paste0(new_output_cd, "ktaus_2021_", x, ".rds"))
}

ktaus_all= list.stack(ktaus, fill=TRUE)
saveRDS(ktaus_all, file = paste0(new_output_cd, "k_tau_values_calibration_2021.rds")) 


##Aggregate results 
calib_data <- readRDS(paste0(new_output_cd, "calibration_data_all_2021.rds"))
calib_data<-calib_data %>% 
  dplyr::group_by(draw ) %>% 
  dplyr::summarise(estimated_trips=sum(estimated_trips),
                   n_choice_occasions = sum(n_choice_occasions),
                   tot_cod_catch= sum(tot_cod_catch), 
                   tot_keep_cod = sum(tot_keep_cod), 
                   tot_rel_cod = sum(tot_rel_cod), 
                   tot_hadd_catch = sum(tot_hadd_catch), 
                   tot_keep_hadd = sum(tot_keep_hadd),
                   tot_rel_hadd = sum(tot_rel_hadd)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cod_keep_trip=tot_keep_cod/estimated_trips, 
                hadd_keep_trip=tot_keep_hadd/estimated_trips, 
                cod_catch_trip=tot_cod_catch/estimated_trips, 
                hadd_catch_trip=tot_hadd_catch/estimated_trips) %>% 
  dplyr::mutate(copula="baseline", decade=0, corr_type="baseline")

#### Total CV
predictions_data1<- rbind(read_excel(paste0(new_output_cd, "decadal_proj__clayton_corr.xlsx")),
                          read_excel(paste0(new_output_cd,"decadal_proj__gumbel_corr.xlsx"))) %>% 
  #read_excel(paste0(new_output_cd,"decadal_proj__frank_corr.xlsx")), 
  #read_excel(paste0(new_output_cd,"decadal_proj__plackett_corr.xlsx")),
  #read_excel("predictions_v2__gaussian_corr.xlsx"),
  # %>% 
  dplyr::mutate(decade=as.numeric(decade)) %>% 
  dplyr::group_by(draw, copula, decade) %>% 
  dplyr::summarise(estimated_trips=sum(ntrips_alt_sum),
                   tot_cod_catch= sum(cod_catch_sum), 
                   tot_keep_cod = sum(cod_keep_sum), 
                   tot_hadd_catch = sum(hadd_catch_sum), 
                   tot_keep_hadd = sum(hadd_keep_sum),
                   cv_sum=sum(cv_sum),.groups = 'drop') %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cod_keep_trip=tot_keep_cod/estimated_trips, 
                hadd_keep_trip=tot_keep_hadd/estimated_trips, 
                cod_catch_trip=tot_cod_catch/estimated_trips, 
                hadd_catch_trip=tot_hadd_catch/estimated_trips, 
                cv_trip=cv_sum/estimated_trips) %>% 
  dplyr::filter(draw<=50)

predictions_data2<- rbind(read_excel(paste0(new_output_cd, "decadal_proj__clayton_ind.xlsx")),
                          read_excel(paste0(new_output_cd,"decadal_proj__gumbel_ind.xlsx"))) %>%
  #read_excel(paste0(new_output_cd,"decadal_proj__plackett_ind.xlsx")),
  #read_excel("predictions_v2__gaussian_corr.xlsx"),
  #read_excel(paste0(new_output_cd,"decadal_proj__gumbel_ind.xlsx"))) %>% 
  dplyr::mutate(decade=as.numeric(decade)) %>% 
  dplyr::group_by(draw, copula, decade) %>% 
  dplyr::summarise(estimated_trips_ind=sum(ntrips_alt_sum),
                   tot_cod_catch_ind= sum(cod_catch_sum), 
                   tot_keep_cod_ind = sum(cod_keep_sum), 
                   tot_hadd_catch_ind = sum(hadd_catch_sum), 
                   tot_keep_hadd_ind = sum(hadd_keep_sum),
                   cv_sum_ind=sum(cv_sum),.groups = 'drop') %>% 
  
  dplyr::ungroup() %>% 
  dplyr::mutate(cod_keep_trip_ind=tot_keep_cod_ind/estimated_trips_ind, 
                hadd_keep_trip_ind=tot_keep_hadd_ind/estimated_trips_ind, 
                cod_catch_trip_ind=tot_cod_catch_ind/estimated_trips_ind, 
                hadd_catch_trip_ind=tot_hadd_catch_ind/estimated_trips_ind, 
                cv_trip_ind=cv_sum_ind/estimated_trips_ind) %>% 
  dplyr::filter(draw<=50)

CV_diffs<-predictions_data2 %>% 
  dplyr::left_join(predictions_data1, by=c("draw", "copula", "decade")) %>% 
  dplyr::mutate(cv_trip_diff=cv_trip-cv_trip_ind, 
                cv_sum_diff=cv_sum-cv_sum_ind) %>% 
  dplyr::select(draw, copula, decade, cv_trip_diff, cv_sum_diff)


predictions_data1<- rbind(read_excel(paste0(new_output_cd, "decadal_proj__clayton_corr.xlsx")),
                          read_excel(paste0(new_output_cd,"decadal_proj__gumbel_corr.xlsx"))) %>%
  #read_excel(paste0(new_output_cd,"decadal_proj__plackett_corr.xlsx")),
  #read_excel("predictions_v2__gaussian_corr.xlsx"),
  #read_excel(paste0(new_output_cd,"decadal_proj__gumbel_corr.xlsx"))) %>% 
  dplyr::mutate(decade=as.numeric(decade)) %>% 
  dplyr::group_by(draw, copula, decade, corr_type) %>% 
  dplyr::summarise(estimated_trips=sum(ntrips_alt_sum),
                   tot_cod_catch= sum(cod_catch_sum), 
                   tot_keep_cod = sum(cod_keep_sum), 
                   tot_hadd_catch = sum(hadd_catch_sum), 
                   tot_keep_hadd = sum(hadd_keep_sum),
                   cv_sum=sum(cv_sum),.groups = 'drop') %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cod_keep_trip=tot_keep_cod/estimated_trips, 
                hadd_keep_trip=tot_keep_hadd/estimated_trips, 
                cod_catch_trip=tot_cod_catch/estimated_trips, 
                hadd_catch_trip=tot_hadd_catch/estimated_trips, 
                cv_trip=cv_sum/estimated_trips) %>% 
  dplyr::filter(draw<=50)

predictions_data2<- rbind(read_excel(paste0(new_output_cd, "decadal_proj__clayton_ind.xlsx")),
                          read_excel(paste0(new_output_cd,"decadal_proj__gumbel_ind.xlsx"))) %>%
  #read_excel(paste0(new_output_cd,"decadal_proj__plackett_ind.xlsx")),
  #read_excel("predictions_v2__gaussian_corr.xlsx"),
  #read_excel(paste0(new_output_cd,"decadal_proj__gumbel_ind.xlsx"))) %>% 
  dplyr::mutate(decade=as.numeric(decade)) %>% 
  dplyr::group_by(draw, copula, decade, corr_type) %>% 
  dplyr::summarise(estimated_trips=sum(ntrips_alt_sum),
                   tot_cod_catch= sum(cod_catch_sum), 
                   tot_keep_cod = sum(cod_keep_sum), 
                   tot_hadd_catch = sum(hadd_catch_sum), 
                   tot_keep_hadd = sum(hadd_keep_sum),
                   cv_sum=sum(cv_sum),.groups = 'drop') %>% 
  
  dplyr::ungroup() %>% 
  dplyr::mutate(cod_keep_trip=tot_keep_cod/estimated_trips, 
                hadd_keep_trip=tot_keep_hadd/estimated_trips, 
                cod_catch_trip=tot_cod_catch/estimated_trips, 
                hadd_catch_trip=tot_hadd_catch/estimated_trips, 
                cv_trip=cv_sum/estimated_trips) %>% 
  dplyr::filter(draw<=50)



calib_data_ktau<-readRDS(paste0(new_output_cd,"k_tau_values_calibration_2021.rds")) %>% 
  dplyr::filter(domain=="all" & month==0) %>% 
  dplyr::select(-month) %>% 
  dplyr::mutate(decade=0) %>% 
  dplyr::mutate(copula="baseline") 

proj_data_ktau_ind<-rbind(read_excel(paste0(new_output_cd,"ktaus__clayton_ind.xlsx")),
                         read_excel(paste0(new_output_cd,"ktaus__gumbel_ind.xlsx"))) %>% 
  dplyr::filter(domain=="all" & month==0) %>% 
  dplyr::select(-month) 

proj_data_ktau_corr<-rbind(read_excel(paste0(new_output_cd,"ktaus__clayton_corr.xlsx")),
                     read_excel(paste0(new_output_cd,"ktaus__gumbel_corr.xlsx"))) %>%
  dplyr::filter(domain=="all" & month==0) %>% 
  dplyr::select(-month) 

proj_data_ktau<- rbind.fill(calib_data_ktau, proj_data_ktau_ind, proj_data_ktau_corr) 


predictions_data<-rbind.fill(calib_data, predictions_data1, predictions_data2)

ktau_data<-rbind.fill(proj_data_ktau, calib_data_ktau) %>% 
  dplyr::mutate(copula_corr=paste0(copula, "_", corr_type)) %>% 
  dplyr::filter(copula!="_gumbel")

predictions_data<-predictions_data %>% 
  dplyr::mutate(copula_corr=paste0(copula, "_", corr_type)) %>% 
  dplyr::filter(copula!="_gumbel")


#plot results
ggplot(predictions_data, aes(x=factor(decade), y=cod_catch_trip, color = factor(copula_corr)))+
  geom_boxplot() 

ggplot(predictions_data, aes(x=factor(decade), y=hadd_catch_trip, color = factor(copula_corr)))+
  geom_boxplot() 

ggplot(predictions_data, aes(x=factor(decade), y=cod_keep_trip, color = factor(copula_corr)))+
  geom_boxplot() 

ggplot(predictions_data, aes(x=factor(decade), y=hadd_keep_trip, color = factor(copula_corr)))+
  geom_boxplot() 

ggplot(predictions_data, aes(x=factor(decade), y=cv_trip, color = factor(copula_corr)))+
  geom_boxplot() 

ggplot(ktau_data, aes(x=factor(decade), y=k_tau_catch_est, color = factor(copula_corr)))+
  geom_boxplot() 

ggplot(ktau_data, aes(x=factor(decade), y=k_tau_keep_est, color = factor(copula_corr)))+
  geom_boxplot() 





##Results by individual domains
calib_data <- readRDS(paste0(new_output_cd, "calibration_data_all_2021.rds"))
calib_data<-calib_data %>% 
  tidyr::separate(period, into = c("mode", "period2", "area", "state")) %>%
  dplyr::group_by(draw, state, mode, area ) %>% 
  dplyr::summarise(estimated_trips=sum(estimated_trips),
                   n_choice_occasions = sum(n_choice_occasions),
                   tot_cod_catch= sum(tot_cod_catch), 
                   tot_keep_cod = sum(tot_keep_cod), 
                   tot_rel_cod = sum(tot_rel_cod), 
                   tot_hadd_catch = sum(tot_hadd_catch), 
                   tot_keep_hadd = sum(tot_keep_hadd),
                   tot_rel_hadd = sum(tot_rel_hadd)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cod_keep_trip=tot_keep_cod/estimated_trips, 
                hadd_keep_trip=tot_keep_hadd/estimated_trips, 
                cod_catch_trip=tot_cod_catch/estimated_trips, 
                hadd_catch_trip=tot_hadd_catch/estimated_trips) %>% 
  dplyr::mutate(copula="baseline", decade=0, corr_type="baseline")

#### Total CV
predictions_data1<- rbind(read_excel(paste0(new_output_cd, "decadal_proj__clayton_corr.xlsx")),
                          read_excel(paste0(new_output_cd,"decadal_proj__gumbel_corr.xlsx"))) %>%
                          #read_excel(paste0(new_output_cd,"decadal_proj__plackett_corr.xlsx")),
                          #read_excel("predictions_v2__gaussian_corr.xlsx"),
                          #read_excel(paste0(new_output_cd,"decadal_proj__gumbel_corr.xlsx"))) %>% 
  tidyr::separate(period2, into = c("mode", "period", "area", "state")) %>%
  dplyr::mutate(decade=as.numeric(decade)) %>% 
  dplyr::group_by(mode, area, state, draw, copula, decade) %>% 
  dplyr::summarise(estimated_trips=sum(ntrips_alt_sum),
                   tot_cod_catch= sum(cod_catch_sum), 
                   tot_keep_cod = sum(cod_keep_sum), 
                   tot_hadd_catch = sum(hadd_catch_sum), 
                   tot_keep_hadd = sum(hadd_keep_sum),
                   cv_sum=sum(cv_sum),.groups = 'drop') %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cod_keep_trip=tot_keep_cod/estimated_trips, 
                hadd_keep_trip=tot_keep_hadd/estimated_trips, 
                cod_catch_trip=tot_cod_catch/estimated_trips, 
                hadd_catch_trip=tot_hadd_catch/estimated_trips, 
                cv_trip=cv_sum/estimated_trips) %>% 
  dplyr::filter(draw<=50)

predictions_data2<- rbind(read_excel(paste0(new_output_cd, "decadal_proj__clayton_ind.xlsx")),
                          read_excel(paste0(new_output_cd,"decadal_proj__gumbel_ind.xlsx"))) %>%
                          #read_excel(paste0(new_output_cd,"decadal_proj__plackett_ind.xlsx")),
                          #read_excel("predictions_v2__gaussian_corr.xlsx"),
                          #read_excel(paste0(new_output_cd,"decadal_proj__gumbel_ind.xlsx"))) %>% 
  tidyr::separate(period2, into = c("mode", "period", "area", "state")) %>%
  dplyr::mutate(decade=as.numeric(decade)) %>% 
  dplyr::group_by(mode, area, state, draw, copula, decade) %>% 
  dplyr::summarise(estimated_trips_ind=sum(ntrips_alt_sum),
                   tot_cod_catch_ind= sum(cod_catch_sum), 
                   tot_keep_cod_ind = sum(cod_keep_sum), 
                   tot_hadd_catch_ind = sum(hadd_catch_sum), 
                   tot_keep_hadd_ind = sum(hadd_keep_sum),
                   cv_sum_ind=sum(cv_sum),.groups = 'drop') %>% 

  dplyr::ungroup() %>% 
  dplyr::mutate(cod_keep_trip_ind=tot_keep_cod_ind/estimated_trips_ind, 
                hadd_keep_trip_ind=tot_keep_hadd_ind/estimated_trips_ind, 
                cod_catch_trip_ind=tot_cod_catch_ind/estimated_trips_ind, 
                hadd_catch_trip_ind=tot_hadd_catch_ind/estimated_trips_ind, 
                cv_trip_ind=cv_sum_ind/estimated_trips_ind) %>% 
  dplyr::filter(draw<=50)

CV_diffs<-predictions_data2 %>% 
  dplyr::left_join(predictions_data1, by=c("mode", "area", "state", "draw", "copula", "decade")) %>% 
  dplyr::mutate(cv_trip_diff=cv_trip-cv_trip_ind, 
                cv_sum_diff=cv_sum-cv_sum_ind) %>% 
  dplyr::select(mode, area, state, draw, copula, decade, cv_trip_diff, cv_sum_diff)


predictions_data1<- rbind(read_excel(paste0(new_output_cd, "decadal_proj__clayton_corr.xlsx")),
                          read_excel(paste0(new_output_cd,"decadal_proj__gumbel_corr.xlsx"))) %>%
  #read_excel(paste0(new_output_cd,"decadal_proj__plackett_corr.xlsx")),
  #read_excel("predictions_v2__gaussian_corr.xlsx"),
  #read_excel(paste0(new_output_cd,"decadal_proj__gumbel_corr.xlsx"))) %>% 
  tidyr::separate(period2, into = c("mode", "period", "area", "state")) %>%
  dplyr::mutate(decade=as.numeric(decade)) %>% 
  dplyr::group_by(mode, area, state, draw, copula, decade, corr_type) %>% 
  dplyr::summarise(estimated_trips=sum(ntrips_alt_sum),
                   tot_cod_catch= sum(cod_catch_sum), 
                   tot_keep_cod = sum(cod_keep_sum), 
                   tot_hadd_catch = sum(hadd_catch_sum), 
                   tot_keep_hadd = sum(hadd_keep_sum),
                   cv_sum=sum(cv_sum),.groups = 'drop') %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cod_keep_trip=tot_keep_cod/estimated_trips, 
                hadd_keep_trip=tot_keep_hadd/estimated_trips, 
                cod_catch_trip=tot_cod_catch/estimated_trips, 
                hadd_catch_trip=tot_hadd_catch/estimated_trips, 
                cv_trip=cv_sum/estimated_trips) %>% 
  dplyr::filter(draw<=50)

predictions_data2<- rbind(read_excel(paste0(new_output_cd, "decadal_proj__clayton_ind.xlsx")),
                          read_excel(paste0(new_output_cd,"decadal_proj__gumbel_ind.xlsx"))) %>%
  #read_excel(paste0(new_output_cd,"decadal_proj__plackett_ind.xlsx")),
  #read_excel("predictions_v2__gaussian_corr.xlsx"),
  #read_excel(paste0(new_output_cd,"decadal_proj__gumbel_ind.xlsx"))) %>% 
  tidyr::separate(period2, into = c("mode", "period", "area", "state")) %>%
  dplyr::mutate(decade=as.numeric(decade)) %>% 
  dplyr::group_by(mode, area, state, draw, copula, decade, corr_type) %>% 
  dplyr::summarise(estimated_trips=sum(ntrips_alt_sum),
                   tot_cod_catch= sum(cod_catch_sum), 
                   tot_keep_cod = sum(cod_keep_sum), 
                   tot_hadd_catch = sum(hadd_catch_sum), 
                   tot_keep_hadd = sum(hadd_keep_sum),
                   cv_sum=sum(cv_sum),.groups = 'drop') %>% 
  
  dplyr::ungroup() %>% 
  dplyr::mutate(cod_keep_trip=tot_keep_cod/estimated_trips, 
                hadd_keep_trip=tot_keep_hadd/estimated_trips, 
                cod_catch_trip=tot_cod_catch/estimated_trips, 
                hadd_catch_trip=tot_hadd_catch/estimated_trips, 
                cv_trip=cv_sum/estimated_trips) %>% 
  dplyr::filter(draw<=50)




calib_data_ktau<-readRDS(paste0(new_output_cd,"k_tau_values_calibration_2021.rds")) %>% 
  dplyr::filter(domain!="all" & month==0) %>% 
  dplyr::select(-month) %>% 
  tidyr::separate(domain, into = c("mode","area", "state")) %>%
  dplyr::mutate(decade=0) %>% 
  dplyr::mutate(copula="baseline") 

proj_data_ktau_ind<-rbind(read_excel(paste0(new_output_cd,"ktaus__clayton_ind.xlsx")),
                          read_excel(paste0(new_output_cd,"ktaus__gumbel_ind.xlsx"))) %>% 
  dplyr::filter(domain!="all" & month==0) %>% 
  dplyr::select(-month) %>% 
  tidyr::separate(domain, into = c("mode","area", "state"))

proj_data_ktau_corr<-rbind(read_excel(paste0(new_output_cd,"ktaus__clayton_corr.xlsx")) ,
                    read_excel(paste0(new_output_cd,"ktaus__gumbel_ind.xlsx"))) %>%  
  dplyr::filter(domain!="all" & month==0) %>% 
  dplyr::select(-month) %>% 
  tidyr::separate(domain, into = c("mode","area", "state"))

proj_data_ktau<- rbind.fill(proj_data_ktau_ind, proj_data_ktau_corr) 


predictions_data<-rbind.fill(calib_data, predictions_data1, predictions_data2)

ktau_data<-rbind.fill(proj_data_ktau, calib_data_ktau)  %>% 
  dplyr::filter(mode=="pr" & state==33 & area=="offshore") %>% 
  dplyr::filter(copula!="_gumbel") %>% 
  dplyr::mutate(copula_corr=paste0(copula, "_", corr_type)) 
  

predictions_data<-predictions_data %>% 
  dplyr::mutate(domain=paste0(mode, "_", state, "_", area)) %>% 
  dplyr::mutate(copula_corr=paste0(copula, "_", corr_type))


predictions_data<-predictions_data %>% 
  dplyr::filter(mode=="pr" & state==33 & area=="offshore") %>% 
  dplyr::filter(copula!="_gumbel")
  


ggplot(predictions_data, aes(x=factor(decade), y=cod_catch_trip, color = factor(copula_corr)))+
  geom_boxplot() 

ggplot(predictions_data, aes(x=factor(decade), y=hadd_catch_trip, color = factor(copula_corr)))+
  geom_boxplot() 

ggplot(predictions_data, aes(x=factor(decade), y=cod_keep_trip, color = factor(copula_corr)))+
  geom_boxplot() 

ggplot(predictions_data, aes(x=factor(decade), y=hadd_keep_trip, color = factor(copula_corr)))+
  geom_boxplot() 

ggplot(predictions_data, aes(x=factor(decade), y=cv_trip, color = factor(copula_corr)))+
  geom_boxplot() 

ggplot(ktau_data, aes(x=factor(decade), y=k_tau_catch_est, color = factor(copula_corr)))+
  geom_boxplot() 

ggplot(ktau_data, aes(x=factor(decade), y=k_tau_keep_est, color = factor(copula_corr)))+
  geom_boxplot() 