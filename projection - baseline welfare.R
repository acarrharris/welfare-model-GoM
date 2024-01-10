


# predict_rec_catch <- function(calibration_data_table,
#                               directed_trips_table,
#                               cod_size_data_read,
#                               hadd_size_data_read,
#                               costs_new_all,
#                               catch_data_all){

#test vals to run the function directly
# 

# calibration_data_table <- calibration_data_table_base
# directed_trips_table <- directed_trips_table_base
# cod_size_data_read <- size_data_cod
# hadd_size_data_read <- size_data_hadd
# costs_new_all <- costs_new_all_MA
# catch_data_all <- catch_files_all_corr

#profvis::profvis({
#if (state1 %in% c("MA", "RI", "CT", "NY", "NJ", "VA")) {

#x<-1


trip_level_output3=list()
trip_level_output1=list()

# Input the calibration output which contains the number of choice occasions needed to simulate
calibration_data <- as.data.frame(calibration_data_table_base[[x]]) %>% tibble() 

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
  #group_by(period) %>% 
  mutate(#n_trips = floor(mean(dtrip_2019)),
    n_trips = floor(dtrip),
    n_draws = n_drawz) 


period_vec <- directed_trips_p %>% 
  dplyr::select(period2, n_draws, month) %>% 
  uncount(n_draws) # %>% mutate(sample_id=1:nrow(period_vec))

regs <- directed_trips_p %>% 
  dplyr::select(period2,
                cod_bag, cod_min, 
                hadd_bag, hadd_min)



catch_data <- catch_data_all %>%
  group_by(period2) %>%
  slice_sample(n = n_drawz * n_catch_draws, replace = TRUE)   %>%
  mutate(
    catch_draw = rep(1:n_catch_draws, length.out = n_drawz * n_catch_draws),
    tripid = rep(1:n_drawz, each = n_catch_draws)
  ) %>%
  rename(tot_cod_catch=tot_cat_cod, tot_hadd_catch=tot_cat_hadd) %>% 
  ungroup



  cod_hadd_catch_data <- catch_data
    
    
    
  # subset trips with zero catch, as no size draws are required
  cod_zero_catch <- filter(catch_data, tot_cod_catch == 0)
  
  #Check to see if there is no catch for either species and if so, pipe code around keep/release determination
  cod_catch_check <- sum(catch_data$tot_cod_catch)
  hadd_catch_check <- sum(catch_data$tot_hadd_catch)
  
  
  #remove trips with zero summer flounder catch
  catch_data <- filter(catch_data, tot_cod_catch > 0)
  
  
  #expand the sf_catch_data so that each row represents a fish
  row_inds <- seq_len(nrow(catch_data))
  catch_data <-  catch_data %>%
    slice(rep(row_inds, tot_cod_catch))   %>%
    mutate(fishid = row_number())
    
  
  #set bag limits to zero and compute keep/release

    catch_data_cod <- catch_data %>%
      dplyr::left_join(regs, by = "period2") %>%
      dplyr:: mutate(cod_bag=0) %>% 
      dplyr::mutate(uniform=runif(nrow(catch_data))) %>%
      dplyr::mutate(posskeep = ifelse(uniform>=p_star_cod, 1,0)) %>%
      dplyr::group_by(tripid, period2, catch_draw)   %>%
      dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        keep_adj =dplyr:: case_when(
          cod_bag > 0 ~ ifelse(csum_keep<=cod_bag & posskeep==1,1,0),
          TRUE ~ 0))
    
    
    catch_data_cod <- catch_data_cod %>%
      mutate(keep_tot = keep_adj,
             release = ifelse(keep_adj==0,1,0))
    
    catch_data_cod<-catch_data_cod %>% dplyr::select(fishid, tripid, keep_tot, release, period2, catch_draw) %>% 
      rename(keep = keep_tot)
    
    summed_catch_data <- catch_data_cod %>%
      as.data.table() %>%
      .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid"), .SDcols = c("keep", "release")]
    
    summed_catch_data <- summed_catch_data %>%
      rename(tot_keep_cod = keep, 
             tot_rel_cod = release)
    
 
    #end retaining sizes of fish kept and released

    #add the zero catch trips 
    trip_data <- summed_catch_data %>% 
      dplyr::bind_rows(cod_zero_catch) %>% 
      mutate_if(is.numeric, replace_na, replace = 0) %>% 
      dplyr::select(-c("tot_cod_catch", "tot_hadd_catch"))
    
    
    
    #######haddock
    
    if (hadd_catch_check!=0){
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
        dplyr::left_join(regs, by = "period2")  %>%
        dplyr:: mutate(hadd_bag=0) %>%        
        dplyr::mutate(uniform=runif(nrow(hadd_catch_data))) %>%
        dplyr::mutate(posskeep = ifelse(uniform>=p_star_hadd, 1,0)) %>%
        dplyr::group_by(tripid, period2, catch_draw)   %>%
        dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          keep_adj =dplyr:: case_when(
            hadd_bag > 0 ~ ifelse(csum_keep<=hadd_bag & posskeep==1,1,0),
            TRUE ~ 0))
      
      #catch_size_data[is.na(catch_size_data)] <- 0
      hadd_catch_data <- hadd_catch_data %>%
        mutate_if(is.numeric, replace_na, replace = 0)
      
      hadd_catch_data <- hadd_catch_data %>%
        mutate(release = ifelse(keep_adj==0,1,0))  
      
      #catch_size_data$release<-ifelse((catch_size_data$keep_adj==0), 1,0)
      
      hadd_catch_data<- hadd_catch_data %>% dplyr::select(fishid, tripid, keep_adj, release, period2, catch_draw) %>% 
        rename(keep = keep_adj)
      
   
      summed_catch_data <- hadd_catch_data %>%
        as.data.table() %>%
        .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid"), .SDcols = c("keep", "release")]
      
      
      summed_catch_data <- summed_catch_data %>%
        rename(tot_keep_hadd = keep, 
               tot_rel_hadd = release)
      
    
      #end retaining sizes of fish kept and released
      
      trip_data_hadd<-summed_catch_data %>% 
        #add the zero catch trips 
        dplyr::bind_rows(hadd_zero_catch) %>% 
        mutate_if(is.numeric, replace_na, replace = 0) %>% 
        dplyr::select(-c("tot_cod_catch", "tot_hadd_catch"))
      
      
      
      # merge the bsb trip data with the rest of the trip data 
      #trip_data <-  merge(trip_data,trip_data_bsb,by=c("period2", "catch_draw", "tripid", "state", "mode", "month" ))
      
      trip_data <- trip_data %>% 
        dplyr::left_join(trip_data_hadd, by = c("period2", "catch_draw", "tripid")) 
      
      # %>%
      
    }
    
    if (hadd_catch_check==0){
      trip_data$tot_hadd_catch<-0
      trip_data$tot_keep_hadd<-0
      trip_data$tot_rel_hadd<-0
    }
    
    rm(trip_data_hadd, catch_data_cod, cod_hadd_catch_data, cod_zero_catch, hadd_zero_catch, summed_catch_data,hadd_catch_data)
    
    
    #names<- c(grep("*beta*", names(costs_new_all), value=TRUE, invert=TRUE))
    costs_new_all <- as.data.frame(cost_files_all_base[[x]])   %>% #tibble() %>% 
      filter(catch_draw<=n_catch_draws) 
  
    
    
    # merge the trip data (summer flounder catch + lengths) with the other species data (numbers kept and released))
    trip_data <- trip_data %>% 
      left_join(costs_new_all, by = c("period2","catch_draw","tripid")) 
    
    # %>%
    trip_data <-trip_data %>% 
      dplyr::arrange(period2, tripid, catch_draw)
    
    
    period_vec1 <- period_vec %>%
      group_by(period2) %>% mutate(tripid = row_number(period2))
    

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
      .[, vA_col_sum := sum(expon_vA), by=list(period2, catch_draw, tripid)]  %>%
      .[, v0_col_sum := sum(expon_v0), by=list(period2, catch_draw, tripid)]
    
    
    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[, change_CS := (1/beta_cost)*(log(vA_col_sum)-log(v0_col_sum))] %>%
      .[, probA :=expon_vA/vA_col_sum] %>%
      .[, prob0 :=expon_v0/v0_col_sum] 
    
    mean_trip_data<- subset(mean_trip_data, alt==1)
    
    # mean_trip_data1<-mean_trip_data %>% group_by(period,tripid) %>% summarise(across(everything(), mean), .groups = 'drop') %>% 
    #   tibble()
    
    
    all_vars<-c()
    all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period2","tripid")]
    all_vars
    
    
    mean_trip_data<-mean_trip_data  %>% as.data.table() %>%
      .[,lapply(.SD, mean), by = c("period2","tripid"), .SDcols = all_vars]
    
    
    
    # Get rid of things we don't need. 
    mean_trip_data <- mean_trip_data %>% 
      dplyr::select(-c(age ,  alt ,  beta_cost ,  beta_opt_out ,  beta_opt_out_age ,       
                       beta_opt_out_likely ,  beta_opt_out_prefer ,  beta_sqrt_cod_hadd_keep ,  beta_sqrt_cod_keep ,  beta_sqrt_cod_release ,  
                       beta_sqrt_hadd_keep ,  beta_sqrt_hadd_release ,  catch_draw ,  cost_base ,  draw ,  expon_v0 ,  expon_vA ,   fish_pref_more  ,        
                       likely_to_fish ,  opt_out ,  v0 ,  v0_col_sum ,  v0_optout , vA , vA_col_sum , vA_optout )) 
    
    
    # Multiply the average trip probability in the Alternative scenario (probA) 
    #by each of the catch variables ( the variables below) to get probability-weighted catch
    
    list_names <- c("tot_keep_cod","tot_keep_hadd", "tot_rel_cod", "tot_rel_hadd" )
    
    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
      .[]
    
    # Multiply the average trip probability in the base scenario (prob0) 
    #by each of the catch variables to get probability-weighted catch
    
    list_names <- c("tot_keep_cod_base","tot_keep_hadd_base", "tot_rel_cod_base", "tot_rel_hadd_base" )
    
    
    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
      .[]
    
    
     #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in 
    #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
    # calibration_data <- calibration_data  #%>%   rename(period2 = period)
    
    trip_level_output <- calibration_data %>% 
      dplyr::select(c(n_choice_occasions, period)) %>% 
      rename(period2=period)  %>% 
      right_join(mean_trip_data, by = "period2") %>% 
      mutate(expand = n_choice_occasions/ndraws, 
             draw=x)
      
    



#return(trip_level_output4)

#mean_trip_data$sim=1
# mean_trip_data <- mean_trip_data %>%
#   mutate(sim = rep(1,nrow(.))) 
# 
# #Here add other trip quality statistics
# #keep_one = ifelse(tot_keep>0,1,0))
# 
# #datset to compute mean cv over all trips
# trip_level_output <- mean_trip_data %>% 
#   left_join(sims , by = c("period2")) %>% 
#   #mutate(seed=eff_seed)   %>%
#   select(c(period2, tripid, expand, change_CS,  probA, prob0, tot_keep_cod, tot_keep_hadd, tot_keep_cod_base, tot_keep_hadd_base,
#            tot_rel_cod, tot_rel_hadd, tot_rel_cod_base, tot_rel_hadd_base)) 


#sum(trip_level_output$prob0)

# seed_stats<-c(draw, state1, eff_seed)
# seeds <- append(seed, seed_stats)
# 
#})
#return(trip_level_output)










#end function
#}

#sum probability weighted catch over all choice occasions
#aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)
# aggregate_trip_data <- mean_trip_data %>% 
#   group_by(period2, sim) %>% 
#   summarize_all(sum, na.rm = TRUE) %>% 
#   ungroup() %>% 
#   left_join(sims , by = c("period2"))
# # right_join(sims %>% mutate(period = as.numeric(period)) %>% dplyr::select(-n_choice_occasions),
# #            by = c("period","sim"))
# 
# 
# ls(aggregate_trip_data)
# list_names = colnames(aggregate_trip_data)[ colnames(aggregate_trip_data) !="tripid" 
#                                             & colnames(aggregate_trip_data) !="catch_draw" & colnames(aggregate_trip_data) !="period2"
#                                             & colnames(aggregate_trip_data) !="vA" & colnames(aggregate_trip_data) !="v0"
#                                             & colnames(aggregate_trip_data) != "state" & colnames(aggregate_trip_data) !="period"
#                                             & colnames(aggregate_trip_data) != "ndraws" 
#                                             & colnames(aggregate_trip_data) != "expand" & colnames(aggregate_trip_data) != "n_choice_occasions"
#                                             & colnames(aggregate_trip_data) != "parameter_draw" ]
# 
# 
# aggregate_trip_data <- aggregate_trip_data %>% 
#   mutate(across(.cols = all_of(list_names),.fns=function(x) expand*x)) %>%
#   select(-c(tripid, sim, expand, n_choice_occasions)) %>%
#   rename(projected_trips = probA, 
#           baseline_trips = prob0 )
# 
# 
# projection_output <- aggregate_trip_data %>%  #list.stack(pds_new, fill=TRUE) %>% 
#   #mutate_at(vars(contains("length")), replace_na, replace = 0) %>% 
#   #pds_new_all_MA[is.na(pds_new_all_MA)] = 0
#   mutate(state = state1) %>%
#   select(baseline_trips, projected_trips, change_CS, CS_alt, CS_base, state) %>%
#   group_by(state)  %>%
#   summarise(across(everything(), sum),
#             .groups = 'drop')  %>%
#   as.data.frame()   #%>%   mutate(avg_cv=avg_cv)
# 
# #sum(projection_output$change_CS)
# #})
# # write_xlsx(pds_new_all_MA,"MA_prediction_output_check.xlsx")
# return(projection_output)

#end function
#}

