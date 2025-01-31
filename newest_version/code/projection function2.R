




#profvis::profvis({

 
predictions_d<-list()
all_ktauz_d<-list()
for(d in 1:1){
  #d<-1

catch_data_all1<-catch_data_all_split[[d]] 

predictions<-list()
all_ktauz<-list()

for(x in 1:2){
  #x<-1
  time_a<-Sys.time()
  
set.seed(130+d+x)

# Input the calibration output which contains the number of choice occasions needed to simulate
calibration_data_all <- readRDS(paste0(output_data_cd, "calibration_data_2021", "_", x, ".rds"))



######################################
##   Begin simulating trip outcomes ##
######################################

catch_data <- catch_data_all1 %>%
  dplyr::group_by(period2, decade) %>%
  dplyr::slice_sample(n = n_drawz * n_catch_draws, replace = TRUE)   %>%
  dplyr::mutate(catch_draw = rep(1:n_catch_draws, length.out = n_drawz * n_catch_draws),
                 tripid = rep(1:n_drawz, each = n_catch_draws)) %>%
  dplyr::ungroup() %>% 
  dplyr::right_join(regs_check, by="period2") %>% 
  dplyr::select(-dtrip) 


#Here we can loop around the the suffix on the catch variables 
c<-cop_name
#copulas<- c("_gumbel", "_frank", "_clayton")
#copulas<- c("_gumbel")

#for(c in copulas){
  #c<- "_gumbel"
  catch_data0 <- catch_data  %>% 
    dplyr::select(catch_draw, decade, mode,month, area, state, period2, tripid, 
                  paste0("cod_corr",c ), paste0("had_corr",c ), 
                  paste0("cod_ind",c ),  paste0("had_ind",c ))
  
  
  #corr_types<- c("ind", "corr")
  #corr_types<- c("ind")
t<-ind_or_corr  
  #for(t in corr_types){
    #t<- "corr"
    catch_data1<- catch_data0  %>% 
      dplyr::select(catch_draw, decade, mode,month, area, state, period2, tripid, 
                    paste0("cod_", t, c ), paste0("had_", t, c ))
    
    catch_data1<- catch_data1  %>% 
      dplyr::rename(tot_cod_catch=paste0("cod_", t, c ), tot_hadd_catch=paste0("had_", t, c ))
    
    cod_hadd_catch_data <- catch_data1
    
    rm(catch_data0, catch_data)
    
    
    
    # subset trips with zero catch, as no size draws are required
    cod_zero_catch <- catch_data1 %>% 
      dplyr::filter(tot_cod_catch == 0) %>% 
      dplyr::select(-c("mode", "area", "state"))
    
    
    #remove trips with zero summer flounder catch
    catch_data1 <- filter( catch_data1, tot_cod_catch > 0) 
    catch_data1<-as.data.table(catch_data1)
    
    #expand the sf_catch_data so that each row represents a fish
    row_inds <- seq_len(nrow(catch_data1))
    
    catch_data1<-  catch_data1 %>%  
      slice(rep(row_inds,tot_cod_catch))   
    
    
    #########New code for assigning keeps and releases
      # Assuming catch_data1 and regs are already data.tables
    regs<-as.data.table(regs)
    
    catch_data_cod <- copy(catch_data1) # Create a copy of catch_data1
    setkey(catch_data_cod, period2)
    setkey(regs, period2)
    
    # Perform the equivalent of a left_join
    catch_data_cod <- regs[catch_data_cod, nomatch = 0]
    
    # Add a uniform random number column
    catch_data_cod[, uniform := runif(.N)]
    
    # Calculate posskeep
    catch_data_cod[, posskeep := ifelse(uniform >= p_star_cod, 1, 0)]
    
    # Group by and calculate cumulative sum of posskeep
    catch_data_cod[, csum_keep := cumsum(posskeep), by = .(decade, period2, tripid, catch_draw)]
    
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
    catch_data_cod <- catch_data_cod[, .(decade, tripid, keep = keep_tot, release, period2, catch_draw, month)]
    ###end new code 
    
    
    summed_catch_data <- catch_data_cod %>%
      as.data.table() %>%
      .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid", "month", "decade"), .SDcols = c("keep", "release")]
    
    trip_data <- summed_catch_data %>%
      rename(tot_keep_cod = keep, 
             tot_rel_cod = release) %>% 
      dplyr::bind_rows(cod_zero_catch) %>% 
      mutate_if(is.numeric, replace_na, replace = 0) %>% 
      dplyr::select(-c("tot_cod_catch", "tot_hadd_catch"))
    
    #trip_data <- data.table(trip_data, key = c("period2", "catch_draw", "tripid", "decade", "month"))
    
    
    #######haddock
    
      # subset trips with zero catch, as no size draws are required
      hadd_zero_catch <- cod_hadd_catch_data %>% 
        dplyr::filter(tot_hadd_catch == 0) %>% 
        dplyr::select(-c("mode", "area", "state"))
      
      #remove trips with zero summer flounder catch
      hadd_catch_data <- filter(cod_hadd_catch_data, tot_hadd_catch > 0) 
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
      hadd_catch_data[, csum_keep := cumsum(posskeep), by = .(tripid, period2, decade, catch_draw)]
      
      # Calculate keep_adj
      hadd_catch_data[, keep_adj := fifelse(
        hadd_bag > 0, 
        fifelse(csum_keep <= hadd_bag & posskeep == 1, 1, 0), 
        0
      )]
      
  
      hadd_catch_data <- hadd_catch_data %>%
        mutate_if(is.numeric, replace_na, replace = 0) %>% 
        mutate(release = ifelse(keep_adj==0,1,0))  
      
      hadd_catch_data<- subset(hadd_catch_data, select=c(decade, tripid, keep_adj, release, period2, catch_draw,  month)) %>% 
        rename(keep = keep_adj)
      
      summed_catch_data <- hadd_catch_data %>%
        .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid", "month", "decade"), .SDcols = c("keep", "release")]
      
      
      summed_catch_data <- summed_catch_data %>%
        rename(tot_keep_hadd = keep, 
               tot_rel_hadd = release)
      
      
      trip_data_hadd<-summed_catch_data %>% 
        dplyr::bind_rows(hadd_zero_catch) %>%  #add the zero catch trips 
        mutate_if(is.numeric, replace_na, replace = 0) %>% 
        dplyr::select(-c("tot_cod_catch", "tot_hadd_catch"))
      
      #trip_data_hadd <- data.table(trip_data_hadd, key = c("period2", "catch_draw", "tripid", "decade", "month"))

      #trip_data<-data.frame(trip_data[trip_data_hadd])
      trip_data<-trip_data %>% 
        dplyr::left_join(trip_data_hadd, by=c("period2", "catch_draw", "tripid", "decade", "month"))
      

    rm(trip_data_hadd, catch_data_cod, cod_hadd_catch_data, cod_zero_catch, hadd_zero_catch, summed_catch_data,hadd_catch_data)

    costs_new_all <- readRDS(paste0(output_data_cd, "cost_files_2021", "_", x, ".rds")) %>% 
      filter(catch_draw<=n_catch_draws)
    
    
    trip_data <- trip_data %>% 
      left_join(costs_new_all, by=c("period2", "catch_draw", "tripid")) %>%
      dplyr::select(-month) %>% 
      left_join(period_vec4, by=c("period2")) %>% 
        dplyr::mutate(                   
        domain2 = paste0(mode, "_", area, "_", st),
        tot_cod_catch = tot_keep_cod + tot_rel_cod,
        tot_hadd_catch = tot_keep_hadd + tot_rel_hadd)

    
    rm(costs_new_all)
    
    trip_data <-trip_data %>%
      dplyr::mutate(
        #utility (prediction year)
        vA=beta_sqrt_cod_keep*sqrt(trip_data$tot_keep_cod) +
          beta_sqrt_cod_release*sqrt(trip_data$tot_rel_cod) +  
          beta_sqrt_hadd_keep*sqrt(trip_data$tot_keep_hadd) +
          beta_sqrt_hadd_release*sqrt(trip_data$tot_rel_hadd) + 
          beta_sqrt_cod_hadd_keep*(sqrt(trip_data$tot_keep_cod)*sqrt(trip_data$tot_keep_hadd)) +
          beta_cost*trip_data$cost, 
        #utility (base year)
        v0 = beta_sqrt_cod_keep*sqrt(trip_data$tot_keep_cod_base) +
          beta_sqrt_cod_release*sqrt(trip_data$tot_rel_cod_base) +  
          beta_sqrt_hadd_keep*sqrt(trip_data$tot_keep_hadd_base) +
          beta_sqrt_hadd_release*sqrt(trip_data$tot_rel_hadd_base) + 
          beta_sqrt_cod_hadd_keep*(sqrt(trip_data$tot_keep_cod_base)*sqrt(trip_data$tot_keep_hadd_base)) +
          beta_cost*trip_data$cost) 
    
    
    #These stats should be roughly the same under no chnage in fishery conditions
    
    # mean(trip_data$vA)
    # mean(trip_data$v0)
    
    # 
    # mean(trip_data$tot_keep_cod)
    # mean(trip_data$tot_keep_cod_base)
    # 
    # mean(trip_data$tot_keep_hadd)
    # mean(trip_data$tot_keep_hadd_base)
    # 
    # mean(trip_data$tot_rel_sf)
    # mean(trip_data$tot_rel_sf_base)
    # 
    # mean(trip_data$tot_keep_bsb)
    # mean(trip_data$tot_keep_bsb_base)
    # 
    # mean(trip_data$tot_rel_bsb)
    # mean(trip_data$tot_rel_bsb_base)
    # 
    # mean(trip_data$tot_scup_catch)
    # mean(trip_data$tot_cat_scup_base)
    
    
    
    #New code to calculate probability of each choice occasion
    
    # Now expand the data to create two alternatives, representing the alternatives available in choice survey
    mean_trip_data <- trip_data %>% 
      data.table::data.table() %>% 
      mutate(n_alt = rep(2,nrow(.))) %>% 
      uncount(n_alt) %>% 
      mutate(alt = rep(1:2,nrow(.)/2),
             opt_out = ifelse(alt == 2, 1, 0))
    
    
    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[, vA_optout := beta_opt_out*opt_out+
          beta_opt_out_age*age + 
          beta_opt_out_likely*likely_to_fish +
          beta_opt_out_prefer*fish_pref_more] %>%
      .[, v0_optout := beta_opt_out*opt_out+
          beta_opt_out_age*age + 
          beta_opt_out_likely*likely_to_fish +
          beta_opt_out_prefer*fish_pref_more] %>%
      .[alt==1, expon_vA := exp(vA)] %>%
      .[alt==2, expon_vA := exp(vA_optout)] %>%
      .[alt==1, expon_v0 := exp(v0)] %>%
      .[alt==2, expon_v0 := exp(v0_optout)] 
    
    
    
    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[, vA_col_sum := sum(expon_vA), by=list(period2, catch_draw, tripid, decade)]  %>%
      .[, v0_col_sum := sum(expon_v0), by=list(period2, catch_draw, tripid, decade)]
    
    
    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[, change_CS := (1/beta_cost)*(log(vA_col_sum)-log(v0_col_sum))] %>%
      .[, change_CS_prob := (1/beta_cost)*(log(vA_col_sum)-log(v0_col_sum))] %>%
      .[, probA :=expon_vA/vA_col_sum] %>%
      .[, prob0 :=expon_v0/v0_col_sum] 
    
    # mean(mean_trip_data$change_CS)
    
    mean_trip_data<- subset(mean_trip_data, alt==1)
    
    mean_trip_data<- mean_trip_data %>% 
      dplyr::mutate(tot_cod_cat=tot_keep_cod+tot_rel_cod, 
                    tot_cod_cat_base=tot_keep_cod_base+tot_rel_cod_base, 
                    tot_hadd_cat=tot_keep_hadd+tot_rel_hadd, 
                    tot_hadd_cat_base=tot_keep_hadd_base+tot_rel_hadd_base)
    
    # mean_trip_data1<-mean_trip_data %>% group_by(period,tripid) %>% summarise(across(everything(), mean), .groups = 'drop') %>% 
    #   tibble()
    
    # Get rid of things we don't need. 
    mean_trip_data <- mean_trip_data %>% 
      dplyr::select(-c(age, alt, beta_cost,beta_opt_out,beta_opt_out_age,       
                       beta_opt_out_likely,beta_opt_out_prefer,beta_sqrt_cod_hadd_keep,  
                       beta_sqrt_cod_keep,beta_sqrt_cod_release,beta_sqrt_hadd_keep, 
                       beta_sqrt_hadd_release, catch_draw, cost_base,draw, expon_v0, 
                       expon_vA, fish_pref_more, likely_to_fish, opt_out,v0, v0_col_sum, 
                       v0_optout, vA, vA_col_sum, vA_optout, mode, area, st, domain2)) 
    
    all_vars<-c()
    all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period2","tripid", "decade", "month")]
    all_vars
    
    
    mean_trip_data<-mean_trip_data  %>% as.data.table() %>%
      .[,lapply(.SD, mean), by = c("period2","tripid", "decade",  "month"), .SDcols = all_vars]
    

    
    # Multiply the average trip probability in the Alternative scenario (probA) 
    #by each of the catch variables ( the variables below) to get probability-weighted catch
    
    list_names <- c("tot_keep_cod","tot_keep_hadd", "tot_rel_cod", "tot_rel_hadd", "tot_cod_cat", "tot_hadd_cat", "change_CS_prob")
    
    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
      .[]
    
    # Multiply the average trip probability in the base scenario (prob0) 
    #by each of the catch variables to get probability-weighted catch
    
    list_names <- c("tot_keep_cod_base","tot_keep_hadd_base", "tot_rel_cod_base", "tot_rel_hadd_base",
                    "tot_cod_cat_base", "tot_hadd_cat_base")

    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
      .[]
    
    
    #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in 
    #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
    # calibration_data <- calibration_data  #%>%   rename(period2 = period)
    
    trip_level_output <- calibration_data_all %>% 
      dplyr::select(c(n_choice_occasions, period)) %>% 
      rename(period2=period)  %>% 
      right_join(mean_trip_data, by = "period2") %>% 
      mutate(expand = n_choice_occasions/n_drawz)
    
    
    # trip_level_output[[x]] <-trip_level_output %>% 
    #   dplyr::mutate(corr_type=t, copula=c) 
    
    # trip_level_output_d1<-trip_level_output
    #   dplyr::filter(decade==1)
    
    #Metrics at the choice occasion level
    # cv_i<- weighted.mean(trip_level_output$change_CS, trip_level_output$expand)
    # 
    # cod_keep_i<- weighted.mean(trip_level_output$tot_keep_cod, trip_level_output$expand)
    # hadd_keep_i<- weighted.mean(trip_level_output$tot_keep_hadd, trip_level_output$expand)
    # 
    # cod_rel_i<- weighted.mean(trip_level_output$tot_rel_cod, trip_level_output$expand)
    # hadd_rel_i<- weighted.mean(trip_level_output$tot_rel_hadd, trip_level_output$expand)
    #   
    # cod_catch_i<- weighted.mean(trip_level_output$tot_cod_cat, trip_level_output$expand)
    # hadd_catch_i<- weighted.mean(trip_level_output$tot_hadd_cat, trip_level_output$expand)
      
    
    
    trip_level_output <- trip_level_output %>%
        as.data.table() %>%
        .[, cv_sum := expand*change_CS] %>%
        .[, cv_sum_prob := expand*change_CS_prob] %>%
      
        .[, cod_keep_sum := expand*tot_keep_cod] %>%
        .[, hadd_keep_sum := expand*tot_keep_hadd] %>%
      
        .[, cod_rel_sum := expand*tot_rel_cod] %>%
        .[, hadd_rel_sum := expand*tot_rel_hadd] %>%
      
        .[, cod_catch_sum := expand*tot_cod_cat] %>%
        .[, hadd_catch_sum := expand*tot_hadd_cat] %>%
      
        .[, ntrips_alt := expand*probA] %>% 
      
        .[, cod_keep_sum_base := expand*tot_keep_cod_base] %>%
        .[, hadd_keep_sum_base := expand*tot_keep_hadd_base] %>%
      
        .[, cod_rel_sum_base := expand*tot_rel_cod_base] %>%
        .[, hadd_rel_sum_base := expand*tot_rel_hadd_base] %>%
      
        .[, cod_catch_sum_base := expand*tot_cod_cat_base] %>%
        .[, hadd_catch_sum_base := expand*tot_hadd_cat_base] %>%
      
        .[, ntrips_base := expand*prob0]
    
    

    trip_level_output <- trip_level_output %>%
      mutate_if(is.numeric, replace_na, replace = 0) %>% 
      dplyr::mutate(period3=period2) %>% 
      tidyr::separate(period2, into = c("mode", "period", "area", "state")) %>% 
      dplyr::rename(period2=period3)
      
    # check<-trip_level_output %>% 
    #   dplyr::group_by(mode, area, state, month) %>% 
    #   dplyr::summarise(ntrips_base=sum(ntrips_base), 
    #                    ntrips_alt=sum(ntrips_alt), 
    #                    cod_catch_base=sum(cod_catch_sum_base), 
    #                    cod_catch_alt=sum(cod_catch_sum), 
    #                    hadd_catch_base=sum(hadd_catch_sum_base), 
    #                    hadd_catch_alt=sum(hadd_catch_sum)) %>% 
    #   dplyr::ungroup() %>% 
    #   dplyr::mutate(cod_catch_trip_calib=cod_catch_base/ntrips_base, 
    #                 cod_catch_trip_proj=cod_catch_alt/ntrips_alt,
    #                 hadd_catch_trip_calib=hadd_catch_base/ntrips_base, 
    #                 hadd_catch_trip_proj=hadd_catch_alt/ntrips_alt)
    
    
    
    trip_level_output1<- trip_level_output %>% 
      dplyr::group_by(period2, decade) %>% 
      dplyr::summarise(cv_sum = sum(cv_sum),
                       cv_sum_prob = sum(cv_sum_prob),
                       
                       cod_keep_sum = sum(cod_keep_sum),
                       cod_keep_sum_base = sum(cod_keep_sum_base),
                    
                       hadd_keep_sum = sum(hadd_keep_sum),
                       hadd_keep_sum_base = sum(hadd_keep_sum_base),
                       
                       cod_rel_sum = sum(cod_rel_sum),
                       cod_rel_sum_base = sum(cod_rel_sum_base),
                       
                       hadd_rel_sum = sum(hadd_rel_sum),
                       hadd_rel_sum_base = sum(hadd_rel_sum_base),
                       
                       cod_catch_sum = sum(cod_catch_sum),
                       cod_catch_sum_base = sum(cod_catch_sum_base),
                      
                       hadd_catch_sum = sum(hadd_catch_sum),
                       hadd_catch_sum_base = sum(hadd_catch_sum_base),
                       
                       ntrips_alt_sum = sum(ntrips_alt),
                       ntrips_base_sum = sum(ntrips_base),

                       n_choice_occasions_sum = sum(expand),
                       .groups = "drop") %>% 
      dplyr::ungroup() 
  
    
    predictions0<-trip_level_output1 %>% 
      dplyr::mutate(decade=d, corr_type=t, copula=c, draw=x) 
    
    
    
    #Now assess the correlation in catch versus the correlation in keep 
    #To do so, draw 10,000 catch draws in proportion to the the number of trips across the period 
    #Then compute kendall's tau for catch and for keep and save in the output list. 
    
    period_vec3 <- predictions0 %>%
      dplyr::select(period2,   ntrips_alt_sum) %>% 
      dplyr::mutate(ntrips_alt_sum=round(ntrips_alt_sum)) %>% 
      dplyr::left_join(period_vec4, by=c( "period2"))

    
    setDT(trip_data)
    setDT(period_vec3)
    
    
    #Now assess the correlation in catch versus the correlation in keep 
    #To do so, draw 10,000 catch draws in proportion to the the number of trips across the period 
    #Then compute kendall's tau for catch and for keep and save in the output list. 
    
    #Fishery-wide ktau's
    
    keep_rel_pairs_annual <- trip_data[catch_draw == 1]  # Filter first for catch_draw == 1
    keep_rel_pairs_annual <- merge(keep_rel_pairs_annual, period_vec3, by = c("month", "period2"), all.x = TRUE)  # Left join
    keep_rel_pairs_annual <- keep_rel_pairs_annual[sample(.N, 1000, prob = ntrips_alt_sum)]  # Weighted sample

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
    ktaus_annual$draw<-x
    ktaus_annual$month<-0
    ktaus_annual$decade<-d
    
    

    #Fishery-wide ktau's by month
    # Perform the operations
    keep_rel_pairs_month <- trip_data[catch_draw == 1]  # Filter for catch_draw == 1
    keep_rel_pairs_month <- merge(keep_rel_pairs_month, period_vec3, by = c("period2", "month"), all.x = TRUE)  # Left join

    
    # Select relevant columns
    keep_rel_pairs_month <- keep_rel_pairs_month[, .(month, tot_cod_catch, tot_hadd_catch, ntrips_alt_sum, tot_keep_cod, tot_keep_hadd)]
    
    ktaus_month<-list()
    for(m in unique(keep_rel_pairs_month$month)){
      
      keep_rel_pairs_month_p<-keep_rel_pairs_month %>%   
        dplyr::filter(month==m) %>% 
        dplyr::slice_sample(weight_by=ntrips_alt_sum, n=1000) 
      
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
      ktaus_month[[m]]$draw<-x
      ktaus_month[[m]]$month<-m
      ktaus_month[[m]]$decade<-d
      
    }
    
    ktaus_month_all<-as.data.frame(list.stack(ktaus_month,  fill=TRUE))
    
  
    
    ##now compute correlation in keep and catch by mode, area, and state
    #First, at the annual level

    keep_rel_pairs_domain_annual <- trip_data %>%
      dplyr::left_join(period_vec3, by = "period2") %>%
      dplyr::select(domain2, tot_cod_catch, tot_hadd_catch, ntrips_alt_sum, tot_keep_cod, tot_keep_hadd)
    
    
    
    #new code 
    # Split data by domain2 once
    domain_split <- split(keep_rel_pairs_domain_annual, keep_rel_pairs_domain_annual$domain2)

    ktaus_domain_all <- future_lapply(domain_split, function(data) {
      # Perform sampling
      data <- data[sample(seq_len(nrow(data)), size = 1000, prob = data$ntrips_alt_sum, replace = FALSE), ]
      
      # Calculate sums
      sum_keep_cod <- sum(data$tot_keep_cod)
      sum_keep_hadd <- sum(data$tot_keep_hadd)
      sum_catch_cod <- sum(data$tot_cod_catch)
      sum_catch_hadd <- sum(data$tot_hadd_catch)
      
      # Check conditions once
      keep_cod_positive <- (sum_keep_cod > 0 & sum_keep_hadd > 0)
      catch_cod_positive <- (sum_catch_cod > 0 & sum_catch_hadd > 0)
      
      # Calculate Kendall's Tau for "keep"
      if (keep_cod_positive) {
        ktau_keep <- cor.test(data$tot_keep_cod, data$tot_keep_hadd, method = "kendall")
        k_tau_keep_est <- ktau_keep[["estimate"]]
        k_tau_keep_p <- ktau_keep[["p.value"]]
      } else {
        k_tau_keep_est <- 0
        k_tau_keep_p <- 1
      }
      
      # Calculate Kendall's Tau for "catch"
      if (catch_cod_positive) {
        ktau_catch <- cor.test(data$tot_cod_catch, data$tot_hadd_catch, method = "kendall")
        k_tau_catch_est <- ktau_catch[["estimate"]]
        k_tau_catch_p <- ktau_catch[["p.value"]]
      } else {
        k_tau_catch_est <- 0
        k_tau_catch_p <- 1
      }
      
      # Return results
      data.frame(
        k_tau_keep_est = k_tau_keep_est,
        k_tau_keep_p = k_tau_keep_p,
        k_tau_catch_est = k_tau_catch_est,
        k_tau_catch_p = k_tau_catch_p,
        domain = data$domain2[1]
      )
    }, future.seed = TRUE)
    
    # Combine results
    ktaus_domain_all <- do.call(rbind, ktaus_domain_all)
    ktaus_domain_all <- ktaus_domain_all %>% dplyr::mutate(month=0)
    

    #next, at the monthly level
    keep_rel_pairs_domain_month <- merge(trip_data, period_vec3, by = c("period2", "month"), all.x = TRUE)[
      , .(domain2, month,tot_cod_catch,tot_hadd_catch,ntrips_alt_sum,tot_keep_cod,tot_keep_hadd
      )
    ]

    
    #new code
    # Ensure keep_rel_pairs_domain_month is a data.table
    setDT(keep_rel_pairs_domain_month)
    
    # Split data by month to avoid nested loops
    month_split <- split(keep_rel_pairs_domain_month, by = "month")
    
    # Define a function to process each month and domain
    process_domain <- function(domain_data, x) {
      # Sample data by `ntrips_alt_sum` weights
      sampled_data <- domain_data[
        sample(.N, size = 1000, prob = ntrips_alt_sum, replace = FALSE)
      ]
      
      # Calculate sums
      sum_keep_cod <- sum(sampled_data$tot_keep_cod)
      sum_keep_hadd <- sum(sampled_data$tot_keep_hadd)
      sum_catch_cod <- sum(sampled_data$tot_cod_catch)
      sum_catch_hadd <- sum(sampled_data$tot_hadd_catch)
      
      # Calculate Kendall's Tau for "keep"
      if (sum_keep_cod > 0 & sum_keep_hadd > 0) {
        ktau_keep <- cor.test(sampled_data$tot_keep_cod, sampled_data$tot_keep_hadd, method = "kendall")
        k_tau_keep_est <- ktau_keep$estimate
        k_tau_keep_p <- ktau_keep$p.value
      } else {
        k_tau_keep_est <- 0
        k_tau_keep_p <- 1
      }
      
      # Calculate Kendall's Tau for "catch"
      if (sum_catch_cod > 0 & sum_catch_hadd > 0) {
        ktau_catch <- cor.test(sampled_data$tot_cod_catch, sampled_data$tot_hadd_catch, method = "kendall")
        k_tau_catch_est <- ktau_catch$estimate
        k_tau_catch_p <- ktau_catch$p.value
      } else {
        k_tau_catch_est <- 0
        k_tau_catch_p <- 1
      }
      
      # Return results as a data.table
      return(data.table(
        k_tau_keep_est = k_tau_keep_est,
        k_tau_keep_p = k_tau_keep_p,
        k_tau_catch_est = k_tau_catch_est,
        k_tau_catch_p = k_tau_catch_p,
        domain = domain_data$domain2[1],
        draw = x
      ))
    }
    
    # Define a function to process each month
    process_month <- function(month_data, x, d) {
      # Split by domain
      domain_split <- split(month_data, by = "domain2")
      
      # Apply process_domain to each domain
      domain_results <- lapply(domain_split, process_domain, x = x)
      
      # Combine results for all domains
      domain_results <- rbindlist(domain_results)
      domain_results[, `:=`(month = month_data$month[1], decade = d)]
      
      return(domain_results)
    }
    
    # Apply processing for each month in parallel
    future::plan(multisession) # Adjust the number of workers as needed
    ktaus_month <- future_lapply(month_split, process_month, x = x, d = d, future.seed = NULL)
    
    # Combine results for all months
    ktaus_domain_month_all <- rbindlist(ktaus_month)

    
    all_ktaus<-rbind.fill(ktaus_annual, ktaus_month_all, ktaus_domain_all, ktaus_domain_month_all) %>% 
      dplyr::mutate(decade=d, corr_type=t, copula=c, draw=x)
    
    Sys.time()-time_a
    
   predictions[[x]]<-predictions0 
   all_ktauz[[x]]<-all_ktaus

   rm(trip_level_output, trip_level_output1, trip_data,  mean_trip_data, keep_rel_pairs_month, keep_rel_pairs_annual,catch_data1, 
      keep_rel_pairs_domain_annual, keep_rel_pairs_domain_month)
   

}
  predictions2<-list.stack(predictions, fill=TRUE)
  predictions_d[[d]]<-predictions2
  
  all_ktauz2<-list.stack(all_ktauz, fill=TRUE)
  all_ktauz_d[[d]]<-all_ktauz2
  
}
  predictions_all<- list.stack(predictions_d, fill=TRUE)
  ktaus_all1<- list.stack(all_ktauz_d, fill=TRUE)
  
  
##check results
# check_results<-predictions_all %>% 
#   dplyr::left_join(period_vec4, by="period2") %>% 
#   dplyr::group_by(draw, month) %>% 
#     dplyr::summarise(estimated_trips_base=sum(ntrips_base_sum),
#                      estimated_trips_alt=sum(ntrips_alt_sum),
#                      
#                      tot_cod_catch_base= sum(cod_catch_sum_base), 
#                      tot_cod_catch_alt= sum(cod_catch_sum), 
#                      
#                      tot_keep_cod_base = sum(cod_keep_sum_base), 
#                      tot_keep_cod_alt = sum(cod_keep_sum), 
#                      
#                      tot_rel_cod_base = sum(cod_rel_sum_base), 
#                      tot_rel_cod_alt = sum(cod_rel_sum), 
#                      
#                      tot_hadd_catch_base = sum(hadd_catch_sum_base), 
#                      tot_hadd_catch_alt = sum(hadd_catch_sum), 
#                      
#                      tot_keep_hadd_base = sum(hadd_keep_sum_base),
#                      tot_keep_hadd_alt = sum(hadd_keep_sum),
#                      
#                      tot_rel_hadd_base = sum(hadd_rel_sum_base), 
#                      tot_rel_hadd_alt = sum(hadd_rel_sum)) %>% 
#   
#     dplyr::ungroup() %>% 
#     dplyr::mutate(cod_keep_trip_base=tot_keep_cod_base/estimated_trips_base, 
#                   cod_keep_trip_alt=tot_keep_cod_alt/estimated_trips_alt, 
#                   
#                   hadd_keep_trip_base=tot_keep_hadd_base/estimated_trips_base, 
#                   hadd_keep_trip_alt=tot_keep_hadd_alt/estimated_trips_alt, 
#                   
#                   cod_catch_trip_base=tot_cod_catch_base/estimated_trips_base, 
#                   cod_catch_trip_alt=tot_cod_catch_alt/estimated_trips_alt, 
#                   
#                   hadd_catch_trip_base=tot_hadd_catch_base/estimated_trips_base,
#                   hadd_catch_trip_alt=tot_hadd_catch_alt/estimated_trips_alt) %>% 
#   dplyr::filter(month==9)
# 
# 
# 
