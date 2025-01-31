

p_star_cod <- p_star_cod_variable
p_star_hadd<-p_star_hadd_variable



#profvis::profvis({
#y_string<-as.character(y)
#catch_data_all1<-catch_data_all_split[[y_string]] 


# Input the calibration output which contains the number of choice occasions needed to simulate
#calibration_data_all <- readRDS(paste0(output_data_cd, "calibration_data_2021", "_", x, ".rds"))



######################################
##   Begin simulating trip outcomes ##
######################################

# catch_data <- catch_data_all1 %>%
#   dplyr::group_by(period2) %>%
#   dplyr::slice_sample(n = n_drawz * n_catch_draws, replace = TRUE)   %>%
#   dplyr::mutate(catch_draw = rep(1:n_catch_draws, length.out = n_drawz * n_catch_draws),
#                 tripid = rep(1:n_drawz, each = n_catch_draws)) %>%
#   dplyr::ungroup() %>% 
#   dplyr::right_join(regs_check, by="period2") %>% 
#   dplyr::select(-dtrip) 

catch_data <- catch_data_all %>%
  dplyr::right_join(regs_check, by="period2") %>% 
  dplyr::select(-dtrip) 



#Here we can loop around the the suffix on the catch variables 
catch_data0 <- catch_data  %>% 
  dplyr::select(-period)

catch_data1<- catch_data  %>% 
  dplyr::select(-period) 


cod_hadd_catch_data <- catch_data1

rm(catch_data0, catch_data)


specs<- c("corr_clayton1", "corr_clayton2","corr_clayton3","corr_clayton4","corr_clayton5","corr_clayton6","corr_clayton7","corr_clayton8",
          "ind_clayton1", "ind_clayton2","ind_clayton3","ind_clayton4","ind_clayton5","ind_clayton6","ind_clayton7","ind_clayton8",
          "corr_frank1", "corr_frank2","corr_frank3","corr_frank4","corr_frank5","corr_frank6","corr_frank7","corr_frank8",
          "ind_frank1", "ind_frank2","ind_frank3","ind_frank4","ind_frank5","ind_frank6","ind_frank7","ind_frank8",
          "corr_guassian1", "corr_guassian2","corr_guassian3","corr_guassian4","corr_guassian5","corr_guassian6","corr_guassian7","corr_guassian8",
          "ind_guassian1", "ind_guassian2","ind_guassian3","ind_guassian4","ind_guassian5","ind_guassian6","ind_guassian7","ind_guassian8",
          "corr_gumbel1", "corr_gumbel2","corr_gumbel3","corr_gumbel4","corr_gumbel5","corr_gumbel6","corr_gumbel7","corr_gumbel8",
          "ind_gumbel1", "ind_gumbel2","ind_gumbel3","ind_gumbel4","ind_gumbel5","ind_gumbel6","ind_gumbel7","ind_gumbel8",
          "corr_plackett1", "corr_plackett2","corr_plackett3","corr_plackett4","corr_plackett5","corr_plackett6","corr_plackett7","corr_plackett8",
          "ind_plackett1", "ind_plackett2","ind_plackett3","ind_plackett4","ind_plackett5","ind_plackett6","ind_plackett7","ind_plackett8")


 for (k in specs){
  #k<-"corr_clayton1"
  catch_data1a<-catch_data1 %>% 
    dplyr::select(mode, month, period2, state, tripid, area, catch_draw, paste0("cod_", k), paste0("had_", k)) %>% 
    dplyr::rename(tot_cod_catch=paste0("cod_", k), tot_hadd_catch=paste0("had_", k))
  
  cod_hadd_catch_data1<-cod_hadd_catch_data %>% 
    dplyr::select(mode, month, period2, state, tripid, area, catch_draw, paste0("cod_", k), paste0("had_", k)) %>% 
    dplyr::rename(tot_cod_catch=paste0("cod_", k), tot_hadd_catch=paste0("had_", k))
  
# subset trips with zero catch, as no size draws are required
cod_zero_catch <- catch_data1a %>% 
  dplyr::filter(tot_cod_catch == 0) %>% 
  dplyr::select(-c("mode", "area", "state"))


#remove trips with zero summer flounder catch
catch_data1a <- filter( catch_data1a, tot_cod_catch > 0) 
catch_data1a<-as.data.table(catch_data1a)

#expand the sf_catch_data so that each row represents a fish
row_inds <- seq_len(nrow(catch_data1a))

catch_data1a<-  catch_data1a %>%  
  slice(rep(row_inds,tot_cod_catch))   


#########New code for assigning keeps and releases
# Assuming catch_data1 and regs are already data.tables
regs<-as.data.table(regs)

catch_data_cod <- copy(catch_data1a) # Create a copy of catch_data1
setkey(catch_data_cod, period2)
setkey(regs, period2)

# Perform the equivalent of a left_join
catch_data_cod <- regs[catch_data_cod, nomatch = 0]

# Add a uniform random number column
catch_data_cod[, uniform := runif(.N)]

# Calculate posskeep
catch_data_cod[, posskeep := ifelse(uniform >= p_star_cod, 1, 0)]

# Group by and calculate cumulative sum of posskeep
catch_data_cod[, csum_keep := cumsum(posskeep), by = .(period2, tripid, catch_draw)]

# Calculate keep_adj
catch_data_cod[, keep_adj := fifelse(cod_bag > 0, 
                                     fifelse(csum_keep <= cod_bag & posskeep == 1, 1, 0), 
                                     0)]

# Calculate keep_tot and release
catch_data_cod[, `:=`(
  keep_tot = keep_adj,
  release = fifelse(keep_adj == 0, 1, 0)
)]

# Select and rename columns
catch_data_cod <- catch_data_cod[, .(tripid, keep = keep_tot, release, period2, catch_draw, month)]
###end new code 


summed_catch_data <- catch_data_cod %>%
  as.data.table() %>%
  .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid", "month" ), .SDcols = c("keep", "release")]

trip_data <- summed_catch_data %>%
  rename(tot_keep_cod = keep, 
         tot_rel_cod = release) %>% 
  dplyr::bind_rows(cod_zero_catch) %>% 
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  dplyr::select(-c("tot_cod_catch", "tot_hadd_catch"))



#######haddock

# subset trips with zero catch, as no size draws are required
hadd_zero_catch <- cod_hadd_catch_data1 %>% 
  dplyr::filter(tot_hadd_catch == 0) %>% 
  dplyr::select(-c("mode", "area", "state"))

#remove trips with zero summer flounder catch
hadd_catch_data <- filter(cod_hadd_catch_data1, tot_hadd_catch > 0) 
hadd_catch_data<-as.data.table(hadd_catch_data)

#expand the sf_catch_data so that each row represents a fish
row_inds <- seq_len(nrow(hadd_catch_data))

hadd_catch_data<- hadd_catch_data %>%  
  slice(rep(row_inds,tot_hadd_catch))

rownames(hadd_catch_data) <- NULL

###new code for assigning keeps to releases
# Assuming hadd_catch_data and regs are already data.tables
setkey(hadd_catch_data, period2)
setkey(regs, period2)

# Perform the equivalent of a left_join
hadd_catch_data <- regs[hadd_catch_data, nomatch = 0]

# Add a uniform random number column
hadd_catch_data[, uniform := runif(.N)]

# Calculate posskeep
hadd_catch_data[, posskeep := ifelse(uniform >= p_star_hadd, 1, 0)]

# Group by and calculate cumulative sum of posskeep
hadd_catch_data[, csum_keep := cumsum(posskeep), by = .(tripid, period2, catch_draw)]

# Calculate keep_adj
hadd_catch_data[, keep_adj := fifelse(
  hadd_bag > 0, 
  fifelse(csum_keep <= hadd_bag & posskeep == 1, 1, 0), 
  0
)]


hadd_catch_data <- hadd_catch_data %>%
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  mutate(release = ifelse(keep_adj==0,1,0))  

hadd_catch_data<- subset(hadd_catch_data, select=c(tripid, keep_adj, release, period2, catch_draw,  month)) %>% 
  rename(keep = keep_adj)

summed_catch_data <- hadd_catch_data %>%
  .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid", "month"), .SDcols = c("keep", "release")]


summed_catch_data <- summed_catch_data %>%
  rename(tot_keep_hadd = keep, 
         tot_rel_hadd = release)


trip_data_hadd<-summed_catch_data %>% 
  dplyr::bind_rows(hadd_zero_catch) %>%  #add the zero catch trips 
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  dplyr::select(-c("tot_cod_catch", "tot_hadd_catch"))


trip_data<-trip_data %>% 
  dplyr::left_join(trip_data_hadd, by=c("period2", "catch_draw", "tripid",  "month"))


param_draws<- readRDS(paste0(output_data_cd, "costs_data_2021draw", i,".rds"))

trip_data<-trip_data %>% 
  dplyr::left_join(param_draws,by=c("period2", "catch_draw", "tripid"))


trip_data <- trip_data %>%
  dplyr::mutate(period = as.numeric(as.factor(period2)))

period_names<-subset(trip_data, select=c("period", "period2"))
period_names <- period_names[!duplicated(period_names), ]


#  utility (prediction year)
trip_data <-trip_data %>%
  dplyr::mutate(vA=
       beta_sqrt_cod_keep*sqrt(tot_keep_cod) +
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


mean_trip_data <- trip_data %>%
  data.table::data.table() %>% 
  .[, group_index := .GRP, by = .(period2, catch_draw, tripid)]



# Now expand the data to create two alternatives, representing the alternatives available in choice survey
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

# Calculate consumer surplus 
mean_trip_data[, `:=`(
  CS_base = log_sum_base / -beta_cost,
  CS_alt = log_sum_alt / -beta_cost
)]

# Calculate change consumer surplus 
mean_trip_data[, `:=`(
  change_CS = CS_alt - CS_base
)]


mean(mean_trip_data$change_CS)
ls(mean_trip_data)

# Get rid of things we don't need.
mean_trip_data <- mean_trip_data %>% 
  dplyr::filter(alt==1) %>% 
  dplyr::select(-c(alt, beta_cost,beta_opt_out, beta_opt_out_age, 
                   beta_opt_out_likely, beta_opt_out_prefer, beta_sqrt_cod_hadd_keep, 
                   beta_sqrt_cod_keep, beta_sqrt_cod_release, beta_sqrt_hadd_keep, 
                   beta_sqrt_hadd_release, likely_to_fish, fish_pref_more,  v0, vA, cost_base, age, 
                   exp_vA, exp_v0, log_sum_base, log_sum_alt, group_index, opt_out))

mean_trip_data <- mean_trip_data %>% 
  dplyr::rename(tot_keep_cod_new=tot_keep_cod, 
                tot_rel_cod_new=tot_rel_cod, 
                tot_keep_hadd_new=tot_keep_hadd, 
                tot_rel_hadd_new=tot_rel_hadd)

mean_trip_data <- mean_trip_data %>% 
  dplyr::mutate(tot_cat_cod_new=tot_keep_cod_new+tot_rel_cod_new, 
               tot_cat_hadd_new=tot_keep_hadd_new+tot_rel_hadd_new, 
               tot_cat_cod_base=tot_keep_cod_base+tot_rel_cod_base, 
               tot_cat_hadd_base=tot_keep_hadd_base+tot_rel_hadd_base)                

# Multiply the trip probability by each of the catch variables to get probability-weighted catch
# Update 9/97/24 - multiply CS by probA to get probability-weighted change CS
list_names <- c("tot_keep_cod_new","tot_rel_cod_new", "tot_cat_cod_new",
                "tot_keep_hadd_new", "tot_rel_hadd_new" , "tot_cat_hadd_new" , "change_CS" )


mean_trip_data<-mean_trip_data %>%
  .[,as.vector(list_names) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = list_names] %>%
  .[]


# Multiply the trip probability in baseline year by each of the catch variables in the basleine year to get probability-weighted catch
list_names <- c("tot_keep_cod_base","tot_rel_cod_base", "tot_cat_cod_base",
                "tot_keep_hadd_base", "tot_rel_hadd_base" , "tot_cat_hadd_base"  )

mean_trip_data <- mean_trip_data %>%
  data.table::as.data.table() %>%
  .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
  .[]


mean_trip_data_prob_catch_draw<-mean_trip_data %>% 
  dplyr::select("period2","tripid", "catch_draw", "probA")


#Average the outcomes over catch draws 
all_vars<-c()
all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c( "period","tripid", "period2")]

mean_trip_data <- mean_trip_data %>%
  .[,lapply(.SD, base::mean), by = c("tripid", "period2"), .SDcols = all_vars]


mean_trip_data <- mean_trip_data %>%
  dplyr::mutate(n_choice_occasions_alt = rep(1,nrow(.))) %>%
  dplyr::left_join(period_names, by = c("period2"))

calibration_data_table<-readRDS(paste0(output_data_cd, "calibration_data_2021draw", i,".rds"))

  
sims <- calibration_data_table %>%
  dplyr::select(c(n_choice_occasions, period2)) %>%
  dplyr::left_join(mean_trip_data, by = c("period2")) %>%
  dplyr::mutate(ndraws = ndraws) %>%
  tidyr::separate(period2, into = c("mode", "period", "area", "state")) %>%
  dplyr::mutate(month = as.numeric(month)) %>%
  dplyr::mutate(expand = n_choice_occasions/ndraws)

sims <- sims %>%
  dplyr::select(c(mode, period, month, area, state, n_choice_occasions, tripid, expand, change_CS, 
                  CS_base, CS_alt,  probA, prob0,expand,  probA, prob0,
                  tot_keep_cod_new, tot_rel_cod_new, tot_keep_hadd_new, tot_rel_hadd_new,
                  tot_keep_cod_base, tot_rel_cod_base, tot_keep_hadd_base,tot_rel_hadd_base, 
                  tot_cat_cod_base, tot_cat_cod_new, tot_cat_hadd_base, tot_cat_hadd_new)) %>% 
    as.data.frame()


#Metrics at the choice occasion level
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
  .[, hadd_rel_base_sum := expand*tot_rel_hadd_new] %>%
  
  .[, hadd_catch_base_sum := expand*tot_cat_hadd_base] %>%
  .[, cod_catch_base_sum := expand*tot_cat_cod_base] %>%
  
  .[, ntrips_alt := expand*probA] %>%
  .[, ntrips_base := expand*prob0] 

dtrip_wts<- sims %>% 
  dplyr::mutate(period2 = paste0(mode, "_", period, "_", area, "_", state)) %>%
  dplyr::group_by(period2) %>%
  dplyr::summarise(ntrips_alt = sum(ntrips_alt),
                   .groups="drop") %>%  
  dplyr::ungroup()

sims<- sims %>% 
  dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
  dplyr::group_by(mode, month, area, state) %>%
  dplyr::summarise(cv_sum= sum(cv_sum),
                   cod_keep_sum = sum(cod_keep_sum),
                   cod_rel_sum = sum(cod_rel_sum),
                   hadd_keep_sum = sum(hadd_keep_sum),
                   hadd_rel_sum = sum(hadd_rel_sum),
                   hadd_catch_sum = sum(hadd_catch_sum),
                   cod_catch_sum = sum(cod_catch_sum),
                   cod_keep_base_sum = sum(cod_keep_base_sum),
                   cod_rel_base_sum = sum(cod_rel_base_sum),
                   hadd_keep_base_sum = sum(hadd_keep_base_sum),
                   hadd_rel_base_sum = sum(hadd_rel_base_sum),
                   hadd_catch_base_sum = sum(hadd_catch_base_sum),
                   cod_catch_base_sum = sum(cod_catch_base_sum),
                   ntrips_alt = sum(ntrips_alt),
                   ntrips_base = sum(ntrips_base), 
                   .groups="drop") %>%  
  dplyr::ungroup() %>% 
  dplyr::mutate(draw=i)


# Now rename the variables so that I can merge each copula-decade specification at the end
# Variables to keep unchanged
keep_vars <- c("mode", "month", "area", "state", "draw")

# String to append or include in the variable names
add_string <- paste0("_", k)

# Rename variables
sims <- sims %>%
  rename_with(
    .fn = ~ paste0(., add_string), 
    .cols = !all_of(keep_vars)
  )

assign(paste0("sims_new_", k), sims)


#Now assess the correlation in catch versus the correlation in keep 
#To do so, draw 10,000 catch draws in proportion to the the number of trips across the period 
#Then compute kendall's tau for catch and for keep and save in the output list. 

#Fishery-wide ktau's

keep_rel_pairs_annual <- trip_data[catch_draw == 1]  # Filter first for catch_draw == 1
keep_rel_pairs_annual <- merge(keep_rel_pairs_annual, dtrip_wts, by = c("period2"), all.x = TRUE)  # Left join
keep_rel_pairs_annual <- keep_rel_pairs_annual[sample(.N, 1000, prob = ntrips_alt)]  # Weighted sample

keep_rel_pairs_annual<-keep_rel_pairs_annual %>% 
  dplyr::mutate(tot_cod_catch=tot_keep_cod+tot_rel_cod, 
                tot_hadd_catch=tot_keep_hadd+tot_rel_hadd)

sum_keep_cod<-sum(keep_rel_pairs_annual$tot_keep_cod)
sum_keep_hadd<-sum(keep_rel_pairs_annual$tot_keep_hadd)

sum_catch_cod<-sum(keep_rel_pairs_annual$tot_cod_catch)
sum_catch_hadd<-sum(keep_rel_pairs_annual$tot_hadd_catch)



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
  
  ktau_catch<- cor.test(keep_rel_pairs_annual$tot_cod_catch, 
                        keep_rel_pairs_annual$tot_hadd_catch, method = c("kendall"))
  
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
keep_rel_pairs_month <- trip_data[catch_draw == 1]  # Filter for catch_draw == 1
keep_rel_pairs_month <- merge(keep_rel_pairs_month, dtrip_wts, by = c("period2"), all.x = TRUE)  # Left join

keep_rel_pairs_month<-keep_rel_pairs_month %>% 
  dplyr::mutate(tot_cod_catch=tot_keep_cod+tot_rel_cod, 
                tot_hadd_catch=tot_keep_hadd+tot_rel_hadd)

# Select relevant columns
keep_rel_pairs_month <- keep_rel_pairs_month[, .(month, tot_cod_catch, tot_hadd_catch, ntrips_alt, tot_keep_cod, tot_keep_hadd)]

ktaus_month<-list()
for(m in unique(keep_rel_pairs_month$month)){
  
  keep_rel_pairs_month_p<-keep_rel_pairs_month %>%   
    dplyr::filter(month==m) %>% 
    dplyr::slice_sample(weight_by=ntrips_alt, n=1000) 
  
  sum_keep_cod<-sum(keep_rel_pairs_month_p$tot_keep_cod)
  sum_keep_hadd<-sum(keep_rel_pairs_month_p$tot_keep_hadd)
  
  sum_catch_cod<-sum(keep_rel_pairs_month_p$tot_cod_catch)
  sum_catch_hadd<-sum(keep_rel_pairs_month_p$tot_hadd_catch)
  
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
    
    ktau_catch<- cor.test(keep_rel_pairs_month_p$tot_cod_catch, 
                          keep_rel_pairs_month_p$tot_hadd_catch, method = c("kendall"))
    
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





#Rename the variables so that I can merge each copula-decade specification at the end

# Variables to keep unchanged
keep_vars <- c("domain", "draw", "month", "year")

# String to append or include in the variable names
add_string <- paste0("_", k)

# Rename variables
all_ktaus <- all_ktaus %>%
  rename_with(
    .fn = ~ paste0(., add_string), 
    .cols = !all_of(keep_vars)
  )

assign(paste0("all_ktaus_new_", k), all_ktaus)

}


#Merge the output from all copula specifications

# catch/harvest/welfare/trips output
# Find all datasets with names starting with "sims_new_"
dataset_names <- ls(pattern = "^sims_new_")

# Retrieve the datasets as a list
datasets <- mget(dataset_names)

# Merge the datasets 
sims_all <- reduce(datasets, merge, by = c("month", "mode", "area", "state", "draw"), all = TRUE)

# correlations output
# Find all datasets with names starting with "sims_new_"
dataset_names <- ls(pattern = "^all_ktaus_new_")

# Retrieve the datasets as a list
datasets <- mget(dataset_names)

# Merge the datasets 
ktaus_all <- reduce(datasets, merge, by = c("domain", "draw", "month", "year"), all = TRUE)



