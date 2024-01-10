



#profvis::profvis({

catch_data_all <-readr::read_csv(file.path(here::here("projection catch per trip.csv")),  show_col_types = FALSE) 

predictions_d<-list()

for(d in 1:8){
  #d<-1
  catch_data_all1<-catch_data_all %>% 
    dplyr::filter(decade==d)

predictions<-list()
for(x in 1:100){
# x<-1


# Input the calibration output which contains the number of choice occasions needed to simulate
#calibration_data <- as.data.frame(calibration_data_table_base[[x]]) %>% tibble() 
calibration_data <- readRDS("calibration_data_all.rds") %>% 
  dplyr::filter(draw==x)

# Input regs
#directed_trips <- as.data.frame(read.csv("directed trips and regulations 2020.csv"))
#directed_trips$dtrip=round(directed_trips$dtrip)

#cod_size_data <- size_data_cod %>%  rename(fitted_prob = prob_star) 
#hadd_size_data <- size_data_hadd  %>%  rename(fitted_prob = prob_star)



######################################
##   Begin simulating trip outcomes ##
######################################



# Set up an output file for the separately simulated within-season regulatory periods  
directed_trips_p <- directed_trips %>% #subset(directed_trips, period == p)
  mutate(period2 = as.character(period2)) %>% 
  mutate(n_trips = floor(dtrip),
         n_draws = n_drawz) 

period_vec <- directed_trips_p %>% 
  dplyr::select(period2, n_draws, month) %>% 
  uncount(n_draws) 

regs <- directed_trips_p %>% 
  dplyr::select(period2,
                cod_bag, cod_min, 
                hadd_bag, hadd_min, dtrip)

regs_check <- directed_trips_p %>% 
  dplyr::select(period2, dtrip)

catch_data <- catch_data_all1 %>% 
  # dplyr::filter(decade==d) %>% 
  group_by(period2, decade) %>%
  slice_sample(n = n_drawz*n_catch_draws, replace = TRUE)   %>%
  mutate(catch_draw = rep(1:n_catch_draws, length.out = n_drawz*n_catch_draws), 
         tripid = rep(1:n_drawz, each=n_catch_draws)) %>%
  ungroup %>% 
  dplyr::right_join(regs_check, by="period2") %>% 
  dplyr::select(-dtrip) 


#Here we can loop around the the suffix on the catch variables 
c<-cop_name
#copulas<- c("_gumbel", "_frank", "_clayton")
#copulas<- c("_gumbel")

#for(c in copulas){
  #c<- "_gumbel"
  catch_data0 <- catch_data  %>% 
    dplyr::select(catch_draw, decade, mode1,month, period2, tripid, 
                  paste0("cod_corr",c ), paste0("had_corr",c ), 
                  paste0("cod_ind",c ),  paste0("had_ind",c ))
  
  
  
  
  #corr_types<- c("ind", "corr")
  #corr_types<- c("ind")
t<-ind_or_corr  
  #for(t in corr_types){
    #t<- "corr"
    catch_data1<- catch_data0  %>% 
      dplyr::select(catch_draw, decade, mode1, month, period2, tripid, 
                    paste0("cod_", t, c ), paste0("had_", t, c ))
    
    
    catch_data1<- catch_data1  %>% 
      dplyr::rename(tot_cod_catch=paste0("cod_", t, c ), tot_hadd_catch=paste0("had_", t, c ))
    
    
    cod_hadd_catch_data <- catch_data1
    
    
    
    
    
    # subset trips with zero catch, as no size draws are required
    cod_zero_catch <- filter( catch_data1, tot_cod_catch == 0)
    
    #Check to see if there is no catch for either species and if so, pipe code around keep/release determination
    cod_catch_check<-sum( catch_data1$tot_cod_catch)
    hadd_catch_check<-sum( catch_data1$tot_hadd_catch)
    
    
    #remove trips with zero summer flounder catch
    catch_data1 <- filter( catch_data1, tot_cod_catch > 0) 
    
    
    #expand the sf_catch_data so that each row represents a fish
    row_inds <- seq_len(nrow( catch_data1))
    
    catch_data1<-  catch_data1 %>%  
      slice(rep(row_inds,tot_cod_catch))   %>%  
      mutate(fishid=row_number())
    
    
    
    catch_data_cod <- catch_data1 %>%
      dplyr::left_join(regs, by = "period2") %>%
      dplyr::mutate(uniform=runif(nrow(catch_data1))) %>% 
      dplyr::mutate(posskeep = ifelse(uniform>=p_star_cod, 1,0)) %>% 
      dplyr::group_by(decade, period2, tripid, catch_draw)   %>%
      dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        keep_adj =dplyr:: case_when(
          cod_bag > 0 ~ ifelse(csum_keep<=cod_bag & posskeep==1,1,0),TRUE ~ 0)) %>% 
      dplyr::mutate(keep_tot = keep_adj,
                    release = ifelse(keep_adj==0,1,0)) %>% 
      dplyr::select(fishid, decade, tripid, keep_tot, release, period2, catch_draw,  month) %>% 
      dplyr::rename(keep = keep_tot)
    
    
    summed_catch_data <- catch_data_cod %>%
      as.data.table() %>%
      .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid", "month", "decade"), .SDcols = c("keep", "release")]
    
    trip_data <- summed_catch_data %>%
      rename(tot_keep_cod = keep, 
             tot_rel_cod = release) %>% 
      dplyr::bind_rows(cod_zero_catch) %>% 
      mutate_if(is.numeric, replace_na, replace = 0) %>% 
      dplyr::select(-c("tot_cod_catch", "tot_hadd_catch", "mode1"))
    
    trip_data <- data.table(trip_data, key = c("period2", "catch_draw", "tripid", "decade", "month"))
    
    
    #######haddock
    
      # subset trips with zero catch, as no size draws are required
      hadd_zero_catch <- filter(cod_hadd_catch_data, tot_hadd_catch == 0)
      
      #remove trips with zero summer flounder catch
      hadd_catch_data <- filter(cod_hadd_catch_data, tot_hadd_catch > 0) 
      
      #expand the sf_catch_data so that each row represents a fish
      row_inds <- seq_len(nrow(hadd_catch_data))
      
      hadd_catch_data<- hadd_catch_data %>%  
        slice(rep(row_inds,tot_hadd_catch))
      
      rownames(hadd_catch_data) <- NULL
      hadd_catch_data$fishid <- 1:nrow(hadd_catch_data)
      
      
      
      hadd_catch_data <- hadd_catch_data %>%
        dplyr::left_join(regs, by = "period2") %>%
        dplyr::mutate(uniform=runif(nrow(hadd_catch_data))) %>%
        dplyr::mutate(posskeep = ifelse(uniform>=p_star_hadd, 1,0)) %>%
        dplyr::group_by(tripid, period2, decade, catch_draw)   %>%
        dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          keep_adj =dplyr:: case_when(
            hadd_bag > 0 ~ ifelse(csum_keep<=hadd_bag & posskeep==1,1,0),
            TRUE ~ 0))
      
      hadd_catch_data <- hadd_catch_data %>%
        mutate_if(is.numeric, replace_na, replace = 0)
      
      #f_dowle3(hadd_catch_data)
      
      hadd_catch_data <- hadd_catch_data %>%
        mutate(release = ifelse(keep_adj==0,1,0))  
      
      #catch_size_data$release<-ifelse((catch_size_data$keep_adj==0), 1,0)
      
      hadd_catch_data<- subset(hadd_catch_data, select=c(fishid, decade, tripid, keep_adj, release, period2, catch_draw,  month)) %>% 
        rename(keep = keep_adj)
      
      summed_catch_data <- hadd_catch_data %>%
        as.data.table() %>%
        .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid", "month", "decade"), .SDcols = c("keep", "release")]
      
      
      summed_catch_data <- summed_catch_data %>%
        rename(tot_keep_hadd = keep, 
               tot_rel_hadd = release)
      
      
      trip_data_hadd<-summed_catch_data %>% 
        #add the zero catch trips 
        dplyr::bind_rows(hadd_zero_catch) %>% 
        mutate_if(is.numeric, replace_na, replace = 0) %>% 
        dplyr::select(-c("tot_cod_catch", "tot_hadd_catch", "mode1"))
      
      trip_data_hadd <- data.table(trip_data_hadd, key = c("period2", "catch_draw", "tripid", "decade", "month"))
      
      
      
      trip_data<-data.frame(trip_data[trip_data_hadd])
      

    rm(trip_data_hadd, catch_data_cod, cod_hadd_catch_data, cod_zero_catch, hadd_zero_catch, summed_catch_data,hadd_catch_data)
    #rm(catch_data, catch_data0, catch_data1)
    
    
    #Now assess the correlation in catch versus the correlation in keep (boat mode only)
    #To do so, draw 5,000 catch draws in proportion to the the number of trips across the period 
    #Then compute kendall's tau for catch and for keep and save in the output list. 
    
    # yrs<- c(1, 2, 3, 4, 5, 6, 7, 8)
    # ktaus<-list()
    # for(y in yrs){
      keep_rel_pairs<- trip_data %>% 
        dplyr::filter(decade==d) %>% 
        dplyr::mutate(tot_cod_catch=tot_keep_cod+tot_rel_cod, 
                      tot_hadd_catch=tot_keep_hadd+tot_rel_hadd)
      
      keep_rel_pairs<- stratified(keep_rel_pairs, "period2", 
                                  size=c("9_fh"=18,"10_fh"=88,"11_fh"=86,"12_fh"=28,"13_fh"=59,"14_fh"=217,
                                         "15_fh"=84,"16_fh"=106,"17_fh"=87,"18_fh"=87,"19_fh"=5,"20_fh"=7,
                                         "7_pr"=397,"9_pr"=245,"10_pr"=230,"11_pr"=403,"12_pr"=353,"13_pr"=266,
                                         "14_pr"=594,"15_pr"=256,"16_pr"=214,"17_pr"=221,"18_pr"=382,"19_pr"=57,
                                         "21_pr"=512))
      
      
      ktau_keep<- cor.test(keep_rel_pairs$tot_keep_cod, 
                           keep_rel_pairs$tot_keep_hadd, method = c("kendall"))
      
      k_tau_keep_est<-ktau_keep[["estimate"]]
      k_tau_keep_p<- ktau_keep[["p.value"]]
      
      ktau_catch<- cor.test(keep_rel_pairs$tot_cod_catch, 
                            keep_rel_pairs$tot_hadd_catch, method = c("kendall"))
      
      k_tau_catch_est<-ktau_catch[["estimate"]]
      k_tau_catch_p<- ktau_catch[["p.value"]]
      
      keep_rel_pairs<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p), names="TRUE")
      keep_rel_pairs<-keep_rel_pairs %>% 
        dplyr::mutate(decade=d, corr_type=t, copula=c, draw=x)
      
    #   ktaus[[y]]=keep_rel_pairs
    #   
    # }
    # 
    # ktaus_all<- list.stack(ktaus, fill=TRUE)
    
    
    
    
    
    
    
    costs_new_all<- readRDS(here::here(paste0("cost_files/cost_files_all_draw_",x,".rds")))
    costs_new_all<-data.table(costs_new_all, key = c("period2", "catch_draw", "tripid"))
    
    
    
    trip_data <- data.table(trip_data, key = c("period2", "catch_draw", "tripid"))
    trip_data<-data.frame(trip_data[costs_new_all])
    
    # trip_data <- trip_data %>% 
    #   left_join(costs_new_all, by = c("period2","catch_draw","tripid")) 
    
    
    # %>%
    # trip_data <-trip_data %>% 
    #   dplyr::arrange(decade, period2, tripid, catch_draw)
    
    
    # period_vec1 <- period_vec %>%
    #       group_by(period2) %>% mutate(tripid = row_number(period2))
    
    
    #  utility (prediction year)
    trip_data <-trip_data %>%
      dplyr::mutate(
        vA=beta_sqrt_cod_keep*sqrt(trip_data$tot_keep_cod) +
          beta_sqrt_cod_release*sqrt(trip_data$tot_rel_cod) +  
          beta_sqrt_hadd_keep*sqrt(trip_data$tot_keep_hadd) +
          beta_sqrt_hadd_release*sqrt(trip_data$tot_rel_hadd) + 
          beta_sqrt_cod_hadd_keep*(sqrt(trip_data$tot_keep_cod)*sqrt(trip_data$tot_keep_hadd)) +
          beta_cost*trip_data$cost, 
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
    #mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
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
      .[, probA :=expon_vA/vA_col_sum] %>%
      .[, prob0 :=expon_v0/v0_col_sum] 
    
    # mean(mean_trip_data$change_CS)
    
    mean_trip_data<- subset(mean_trip_data, alt==1)
    
    # mean_trip_data1<-mean_trip_data %>% group_by(period,tripid) %>% summarise(across(everything(), mean), .groups = 'drop') %>% 
    #   tibble()
    
    
    all_vars<-c()
    all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period2","tripid", "decade")]
    all_vars
    
    
    mean_trip_data<-mean_trip_data  %>% as.data.table() %>%
      .[,lapply(.SD, mean), by = c("period2","tripid", "decade"), .SDcols = all_vars]
    
    
    
    # Get rid of things we don't need. 
    mean_trip_data <- mean_trip_data %>% 
      dplyr::select(-c(age, alt, beta_cost,beta_opt_out,beta_opt_out_age,       
                       beta_opt_out_likely,beta_opt_out_prefer,beta_sqrt_cod_hadd_keep,  
                       beta_sqrt_cod_keep,beta_sqrt_cod_release,beta_sqrt_hadd_keep, 
                       beta_sqrt_hadd_release, catch_draw, cost_base,draw, expon_v0, 
                       expon_vA, fish_pref_more, likely_to_fish, opt_out,v0, v0_col_sum, 
                       v0_optout, vA, vA_col_sum, vA_optout, tot_keep_cod_base,tot_keep_hadd_base, 
                       tot_rel_cod_base, tot_rel_hadd_base, prob0)) 
    
    
    # Multiply the average trip probability in the Alternative scenario (probA) 
    #by each of the catch variables ( the variables below) to get probability-weighted catch
    
    list_names <- c("tot_keep_cod","tot_keep_hadd", "tot_rel_cod", "tot_rel_hadd" )
    
    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
      .[]
    
    # Multiply the average trip probability in the base scenario (prob0) 
    #by each of the catch variables to get probability-weighted catch
    
    # list_names <- c("tot_keep_cod_base","tot_keep_hadd_base", "tot_rel_cod_base", "tot_rel_hadd_base" )
    # 
    # 
    # mean_trip_data <- mean_trip_data %>%
    #   as.data.table() %>%
    #   .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
    #   .[]
    
    
    #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in 
    #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
    # calibration_data <- calibration_data  #%>%   rename(period2 = period)
    
    trip_level_output <- calibration_data %>% 
      dplyr::select(c(n_choice_occasions, period)) %>% 
      rename(period2=period)  %>% 
      right_join(mean_trip_data, by = "period2") %>% 
      mutate(expand = n_choice_occasions/n_drawz)
    
    
    # trip_level_output[[x]] <-trip_level_output %>% 
    #   dplyr::mutate(corr_type=t, copula=c) 
    
    # trip_level_output_d1<-trip_level_output
    #   dplyr::filter(decade==1)
    
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
    
    ##add k_tau values
    # k_tauz_check<-  list.stack(ktau_values, fill=TRUE)
    # 
    # k_tauz_d1<-ktaus_all %>%
    #   dplyr::filter(decade==1) %>% 
    #   dplyr::mutate(data_type=paste0(as.character(corr_type),"", as.character(copula))) %>% 
    #   dplyr::left_join(data_types, by="data_type") %>% 
    #   dplyr::filter(data_type_num==k) %>% 
    #   dplyr::select(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p, data_type_num)  
      
      
      
    # trip_level_output2<-trip_level_output4 %>% 
    #   dplyr::filter(decade==d)
    
    
  #   ktau_values[[x]]<-ktaus_all %>% dplyr::mutate(draw=x)
  #   
  #   
  # }
  
    predictions1<- as.data.frame(cbind(
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
    
    predictions[[x]]<-predictions1 %>% 
      dplyr::mutate(decade=d, corr_type=t, copula=c, draw=x) %>% 
      dplyr::left_join(keep_rel_pairs, by=c("decade", "corr_type", "copula", "draw"))

}
  predictions2<-list.stack(predictions, fill=TRUE)
  predictions_d[[d]]<-predictions2
  
}
  predictions_all<- list.stack(predictions_d, fill=TRUE)
  
  
  




