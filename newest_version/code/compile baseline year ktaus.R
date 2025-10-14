

# Assess the inter-species correlation in catch versus  correlation in keep in the baseline year
# a) Draw 10,000 catch draws in proportion to the projected number of trips across the fishing year 
# b) Compute kendall's tau
y<-2021
n_draws<-100 #number of simulated choice occasions per period

input_data_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/input_data/"
input_code_cd <- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/code/"
output_data_cd<- "C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/"


all_ktaus_list<-list()

for (i in 1:100){
# i<-1

directed_trips<-data.frame(read_csv(paste0(input_data_cd, "directed trips and regulations 2010_2020_disaggregated.csv"), show_col_types = FALSE)) %>% 
  dplyr::mutate(dtrip=round(dtrip)) %>% dplyr::filter(dtrip!=0) %>% dplyr::filter(year==y)

directed_trips_p <- directed_trips %>% 
  mutate(period2 = as.character(period2)) %>% mutate(n_trips = floor(dtrip), n_draws = n_draws) 

regs <- directed_trips_p %>% dplyr::select(period2, cod_bag, cod_min, hadd_bag, hadd_min, dtrip)

regs_check <- directed_trips_p %>%   dplyr::select(period2, dtrip, month)

dtrip_wts<- regs_check %>%
  dplyr::group_by(period2) %>%
  dplyr::summarise(ntrips_alt = sum(dtrip),
                   .groups="drop") %>%
  dplyr::ungroup()    %>%
  mutate(weight = ntrips_alt / sum(ntrips_alt)) 

# Fishery-wide ktau's
keep_rel_pairs_annual <- readRDS(paste0(output_data_cd, "ktau_draws_2021draw", i, ".rds"))  
  
keep_rel_pairs_annual <-data.table::as.data.table(keep_rel_pairs_annual) 

keep_rel_pairs_annual <- keep_rel_pairs_annual %>%
  left_join(dtrip_wts, by = "period2")

keep_rel_pairs_annual <- keep_rel_pairs_annual %>%
  slice_sample(n = 10000, weight_by = weight)

#keep_rel_pairs_annual %>% count(period2)

keep_rel_pairs_annual<-keep_rel_pairs_annual %>%
  dplyr::mutate(tot_cod_catch_new=tot_keep_cod+tot_rel_cod,
                tot_hadd_catch_new=tot_keep_hadd+tot_rel_hadd, 
                tot_cod_hadd_catch_new=tot_cod_catch_new+tot_hadd_catch_new)

sum_keep_cod<-sum(keep_rel_pairs_annual$tot_keep_cod)
sum_keep_hadd<-sum(keep_rel_pairs_annual$tot_keep_hadd)

sum_catch_cod<-sum(keep_rel_pairs_annual$tot_cod_catch_new)
sum_catch_hadd<-sum(keep_rel_pairs_annual$tot_hadd_catch_new)


# # Reshape z into matrix form
# keep_rel_pairs_annual_filtered<-keep_rel_pairs_annual %>% 
#   dplyr::filter(tot_cod_catch_new!=0 & tot_hadd_catch_new!=0) #remove zero catch of both species
# 
# z_matrix <- acast(keep_rel_pairs_annual_filtered, tot_cod_catch_new ~ tot_hadd_catch_new, value.var = "tot_cod_hadd_catch_new")
# 
# # Get unique x and y
# x_vals <- sort(unique(keep_rel_pairs_annual_filtered$tot_cod_catch_new))
# y_vals <- sort(unique(keep_rel_pairs_annual_filtered$tot_hadd_catch_new))
# 
# # Plot
# persp3d(x_vals, y_vals, z_matrix,
#         col = "lightgreen", xlab = "tot_cod_catch_new", ylab = "tot_hadd_catch_new", zlab = "tot_cod_hadd_catch_new")
# 


if(sum_keep_cod>0 & sum_keep_hadd>0){
  
  ktau_keep<- cor.test(keep_rel_pairs_annual$tot_keep_cod,
                       keep_rel_pairs_annual$tot_keep_hadd, method = c("kendall"))
  
  k_tau_keep_est<-ktau_keep[["estimate"]]
  k_tau_keep_p<- ktau_keep[["p.value"]]
}

if(sum_keep_cod==0 | sum_keep_hadd==0){
  
  k_tau_keep_est<-0
  k_tau_keep_p<- 1
}


if(sum_catch_cod>0 & sum_catch_hadd>0){
  
  ktau_catch<- cor.test(keep_rel_pairs_annual$tot_cod_catch_new,
                        keep_rel_pairs_annual$tot_hadd_catch_new, method = c("kendall"))
  
  k_tau_catch_est<-ktau_catch[["estimate"]]
  k_tau_catch_p<- ktau_catch[["p.value"]]
}

if(sum_catch_cod==0 | sum_catch_hadd==0){
  
  k_tau_catch_est<-0
  k_tau_catch_p<- 1
}

ktaus_annual<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p), names="TRUE")
ktaus_annual$domain<-"all"
ktaus_annual$draw<-i
ktaus_annual$month<-0


#Fishery-wide ktau's by month
# Perform the operations
dtrip_wts<- regs_check %>%
  dplyr::group_by(month) %>%
  dplyr::mutate(total_weight_month = sum(dtrip, na.rm = TRUE),
                pct_of_month = (dtrip / total_weight_month)) %>%
  dplyr::ungroup() %>% 
  dplyr::select(period2, month, total_weight_month, pct_of_month, dtrip)

keep_rel_pairs_month <- readRDS(paste0(output_data_cd, "ktau_draws_2021draw", i, ".rds"))  %>% 
  dplyr::select(-month)

keep_rel_pairs_month <- keep_rel_pairs_month %>%
  left_join(dtrip_wts, by = "period2")

keep_rel_pairs_month<-keep_rel_pairs_month %>%
  dplyr::mutate(tot_cod_catch_new=tot_keep_cod+tot_rel_cod,
                tot_hadd_catch_new=tot_keep_hadd+tot_rel_hadd)

ktaus_month<-list()
for(m in unique(keep_rel_pairs_month$month)){
  
  keep_rel_pairs_month_p<-keep_rel_pairs_month %>%
    dplyr::filter(month==m) 
  
  keep_rel_pairs_month_p <- keep_rel_pairs_month_p %>%
    slice_sample(n = 10000, weight_by = pct_of_month)
  
  sum_keep_cod<-sum(keep_rel_pairs_month_p$tot_keep_cod)
  sum_keep_hadd<-sum(keep_rel_pairs_month_p$tot_keep_hadd)
  
  sum_catch_cod<-sum(keep_rel_pairs_month_p$tot_cod_catch_new)
  sum_catch_hadd<-sum(keep_rel_pairs_month_p$tot_hadd_catch_new)
  
  if(sum_keep_cod>0 & sum_keep_hadd>0){
    
    ktau_keep<- cor.test(keep_rel_pairs_month_p$tot_keep_cod,
                         keep_rel_pairs_month_p$tot_keep_hadd, method = c("kendall"))
    
    k_tau_keep_est<-ktau_keep[["estimate"]]
    k_tau_keep_p<- ktau_keep[["p.value"]]
    
  }
  
  if(sum_keep_cod==0 | sum_keep_hadd==0){
    
    k_tau_keep_est<-0
    k_tau_keep_p<- 1
    
  }
  
  if(sum_catch_cod>0 & sum_catch_hadd>0){
    
    ktau_catch<- cor.test(keep_rel_pairs_month_p$tot_cod_catch_new,
                          keep_rel_pairs_month_p$tot_hadd_catch_new, method = c("kendall"))
    
    k_tau_catch_est<-ktau_catch[["estimate"]]
    k_tau_catch_p<- ktau_catch[["p.value"]]
  }
  
  if(sum_catch_cod==0 | sum_catch_hadd==0){
    
    k_tau_catch_est<-0
    k_tau_catch_p<- 1
    
  }
  
  ktaus_month[[m]]<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p), names="TRUE")
  ktaus_month[[m]]$domain<-"all"
  ktaus_month[[m]]$draw<-i
  ktaus_month[[m]]$month<-m
  
}

ktaus_month_all<-as.data.frame(list.stack(ktaus_month,  fill=TRUE))
all_ktaus<-rbind.fill(ktaus_annual, ktaus_month_all)
all_ktaus$year<-y

all_ktaus_list[[i]]<-all_ktaus
}
all_ktaus_combine<-rbind.fill(all_ktaus_list)
write_xlsx(all_ktaus_combine, paste0(output_data_cd,"ktau_baseline_year_output_6-2-25.xlsx"))  #save the data



