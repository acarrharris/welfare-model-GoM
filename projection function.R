


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
  
  # Input the calibration output which contains the number of choice occasions needed to simulate
  calibration_data <- as.data.frame(calibration_data_table_base[[x]]) %>% tibble() 
  
  # Input regs
  #directed_trips <- as.data.frame(read.csv("directed trips and regulations 2020.csv"))
  #directed_trips$dtrip=round(directed_trips$dtrip)
  
  cod_size_data <- size_data_cod %>%  rename(fitted_prob = prob_star) 
  hadd_size_data <- size_data_hadd  %>%  rename(fitted_prob = prob_star)

  
  
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
    rename(tot_cod_catch = cod_tot_cat,  tot_hadd_catch = hadd_tot_cat)  %>%
    select(-c(month1)) #%>%    subset(decade==d)
  
  catch_data <- catch_data %>% 
    group_by(period2) %>%
    slice_sample(n = n_drawz*n_catch_draws, replace = TRUE)   %>%
    mutate(#period = rep(period_vec$period2, each = nsamp),
      catch_draw = rep(1:n_catch_draws, length.out = n_drawz*n_catch_draws), 
      tripid = rep(1:n_drawz, each=n_catch_draws)) %>%
    ungroup
  
  
   cod_hadd_catch_data <- catch_data
  
  
  # subset trips with zero catch, as no size draws are required
  cod_zero_catch <- filter(catch_data, tot_cod_catch == 0)
  
  #Check to see if there is no catch for either species and if so, pipe code around keep/release determination
  cod_catch_check<-sum(catch_data$tot_cod_catch)
  hadd_catch_check<-sum(catch_data$tot_hadd_catch)

  
  #remove trips with zero summer flounder catch
  catch_data <- filter(catch_data, tot_cod_catch > 0) 
  
  
  #expand the sf_catch_data so that each row represents a fish
  row_inds <- seq_len(nrow(catch_data))
  catch_data<- catch_data %>%  
    slice(rep(row_inds,tot_cod_catch))   %>%  
    mutate(fishid=row_number())
  
  
  # generate lengths for each fish
  catch_size_data <- catch_data %>% 
    mutate(fitted_length = sample(cod_size_data$fitted_length,
                                  nrow(.),
                                  prob = cod_size_data$fitted_prob,
                                  replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
  
  # Impose regulations, calculate keep and release per trip
  # For summer flounder, retain keep- and release-at-length
  
  catch_size_data <- catch_size_data %>%
    left_join(regs, by = "period2") %>%
    mutate(posskeep = ifelse(fitted_length>=cod_min,1,0)) %>%
    group_by(tripid, period2, catch_draw)   %>%
    # keep = case_when(
    # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    # TRUE ~ 0),
    mutate(csum_keep = cumsum(posskeep)) %>%
    ungroup() %>%
    mutate(
      keep_adj = case_when(
        cod_bag > 0 ~ ifelse(csum_keep<=cod_bag & posskeep==1,1,0),
        TRUE ~ 0))  
  
  # %>%
  #   
  #   mutate(posskeep2 = ifelse(fitted_length>=fluke_min2 & fitted_length<fluke_max2,1,0)) %>%
  #   group_by(tripid, period2, catch_draw) %>%
  #   # keep = case_when(
  #   # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
  #   # TRUE ~ 0),
  #   mutate(csum_keep2 = cumsum(posskeep2)) %>%
  #   ungroup() %>%
  #   mutate(
  #     keep_adj2 = case_when(
  #       fluke_bag2 > 0 ~ ifelse(csum_keep2<=fluke_bag2 & posskeep2==1,1,0)))
  
  #catch_size_data[is.na(catch_size_data)] <- 0
  catch_size_data <- catch_size_data %>%
    mutate_if(is.numeric, replace_na, replace = 0)
  
  # catch_size_data <- catch_size_data %>%
  #   mutate(keep_tot = keep_adj+keep_adj2,
  #          release = ifelse(keep_adj==0 & keep_adj2==0,1,0))  
  
  catch_size_data <- catch_size_data %>%
    mutate(keep_tot = keep_adj,
           release = ifelse(keep_adj==0,1,0))
  
  catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_tot, release, period2, catch_draw,  month)) %>% 
    rename(keep = keep_tot)
  
  #Uncomment this if you want sizes of fish 
  # new_size_data <- catch_size_data %>%
  #   group_by(period2, catch_draw, tripid, fitted_length, mode, month) %>%
  #   summarize(keep = sum(keep),
  #             release = sum(release), .groups = "drop") #%>%    dplyr::arrange(tripid, period2,  catch_draw)
  
  # summed_catch_data <- catch_size_data %>%
  #   group_by(period2, catch_draw, tripid,  mode, month) %>%
  #   summarize(tot_keep_sf = sum(keep),
  #             tot_rel_sf = sum(release),
  #             .groups = "drop") #%>%
  
  summed_catch_data <- catch_size_data %>%
    as.data.table() %>%
    .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid", "month"), .SDcols = c("keep", "release")]
  
  summed_catch_data <- summed_catch_data %>%
    rename(tot_keep_cod = keep, 
           tot_rel_cod = release)
  
  #The following code will retain the length of fish kept and released. 
  #This takes up a lot of memory, so for now will comment out these lines. 
  
  
  # keep_size_data <- new_size_data %>%
  #   #ungroup() %>%
  #   dplyr::select(-release) %>% 
  #   pivot_wider(names_from = fitted_length, #_length,
  #               names_glue = "keep_length_sf_{fitted_length}",
  #               names_sort = TRUE,
  #               values_from = keep, 
  #               values_fill = 0) # %>% 
  # #I()
  # #keep_size_data
  # 
  # release_size_data <- new_size_data %>%
  #   #ungroup() %>% 
  #   dplyr::select(-keep) %>% 
  #   pivot_wider(names_from = fitted_length, #_length,
  #               names_glue = "release_length_sf_{fitted_length}",
  #               names_sort = TRUE,
  #               values_from = release, 
  #               values_fill = 0) #%>% 
  # 
  # 
  # trip_data <- summed_catch_data %>% 
  #   left_join(keep_size_data, by = c("period2", "catch_draw","tripid", "mode1", "month1")) %>% 
  #   left_join(release_size_data, by = c("period2", "catch_draw","tripid", "mode1", "month1")) #%>% 
  
  #end retaining sizes of fish kept and released
  
  trip_data<-summed_catch_data
  
  #add the zero catch trips 
  trip_data <- bind_rows(trip_data, cod_zero_catch) %>% 
    #arrange(period, catch_draw, tripid) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(tot_cod_catch = tot_keep_cod + tot_rel_cod) %>% 
    dplyr::select(-c("tot_cod_catch", "tot_hadd_catch"))
    #dplyr::select(-c("tot_cod_catch", "tot_hadd_catch", "decade"))
  
  
  
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
    
    
    # generate lengths for each fish
    catch_size_data <- hadd_catch_data %>% 
      mutate(fitted_length = sample(hadd_size_data$fitted_length,
                                    nrow(.),
                                    prob = hadd_size_data$fitted_prob,
                                    replace = TRUE)) #%>%
    
    
    catch_size_data <- catch_size_data %>% 
      left_join(regs, by = "period2") %>% 
      mutate(posskeep = ifelse(fitted_length>=hadd_min ,1,0)) %>% 
      group_by(tripid, period2, catch_draw) %>% 
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),
      mutate(csum_keep = cumsum(posskeep)) %>% 
      ungroup() %>% 
      mutate(
        keep_adj = case_when(
          hadd_bag > 0 ~ ifelse(csum_keep<=hadd_bag & posskeep==1,1,0),
          TRUE ~ 0))
    #,
    # keep_adj = case_when(
    #   csum_keep<=bag & keep==1 ~ 1,
    #   TRUE ~ 0),
    #release = case_when(
    # bsb_bag > 0 ~ ifelse(posskeep==0 | (posskeep==1 & csum_keep>bsb_bag ), 1,0)))
    
    #catch_size_data[is.na(catch_size_data)] <- 0
    catch_size_data <- catch_size_data %>%
      mutate_if(is.numeric, replace_na, replace = 0)
    
    catch_size_data <- catch_size_data %>%
      mutate(release = ifelse(keep_adj==0,1,0))  
    
    #catch_size_data$release<-ifelse((catch_size_data$keep_adj==0), 1,0)
    
    catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_adj, release, period2, catch_draw, month)) %>% 
      rename(keep = keep_adj)
    
    #Uncomment this if you want sizes of fish 
    # new_size_data <- catch_size_data %>%
    # group_by(period2, catch_draw, tripid, fitted_length,  mode, month) %>%
    # summarize(keep = sum(keep),
    #           release = sum(release), .groups = "drop") #%>%
    
    # summed_catch_data <- catch_size_data %>%
    #   group_by(period2, catch_draw, tripid,  mode, month) %>%
    #   summarize(tot_keep_bsb = sum(keep),
    #             tot_rel_bsb = sum(release),
    #             .groups = "drop") #%>%
    
    summed_catch_data <- catch_size_data %>%
      as.data.table() %>%
      .[,lapply(.SD, sum), by =c("period2", "catch_draw", "tripid",  "month"), .SDcols = c("keep", "release")]
    
    summed_catch_data <- summed_catch_data %>%
      rename(tot_keep_hadd = keep, 
             tot_rel_hadd = release)
    
    #The following code will retain the length of fish kept and released. 
    #This takes up a lot of memory, so for now will comment out these lines. 
    
    # keep_size_data <- new_size_data %>%
    #   #ungroup() %>%
    #   dplyr::select(-release) %>% 
    #   pivot_wider(names_from = fitted_length, #_length,
    #               names_glue = "keep_length_bsb_{fitted_length}",
    #               names_sort = TRUE,
    #               values_from = keep, 
    #               values_fill = 0) # %>% 
    # #I()
    # #keep_size_data
    # 
    # release_size_data <- new_size_data %>%
    #   #ungroup() %>% 
    #   dplyr::select(-keep) %>% 
    #   pivot_wider(names_from = fitted_length, #_length,
    #               names_glue = "release_length_bsb_{fitted_length}",
    #               names_sort = TRUE,
    #               values_from = release, 
    #               values_fill = 0) #%>% 
    # 
    # 
    # trip_data_bsb <- summed_catch_data %>% 
    #   left_join(keep_size_data, by = c("period2", "catch_draw","tripid", "mode1", "month1")) %>% 
    #   left_join(release_size_data, by = c("period2", "catch_draw","tripid", "mode1", "month1")) #%>% 
    #end retaining sizes of fish kept and released
    
    trip_data_hadd<-summed_catch_data
    #add the zero catch trips 
    trip_data_hadd <- bind_rows(trip_data_hadd, hadd_zero_catch) %>% 
      #arrange(period, catch_draw, tripid) %>% 
      mutate_if(is.numeric, replace_na, replace = 0) %>% 
      mutate(tot_hadd_catch = tot_keep_hadd + tot_rel_hadd)  %>% 
      dplyr::select(-c("tot_hadd_catch"))
    
    
    # merge the bsb trip data with the rest of the trip data 
    #trip_data <-  merge(trip_data,trip_data_bsb,by=c("period2", "catch_draw", "tripid", "state", "mode", "month" ))
    
    trip_data <- trip_data %>% 
      left_join(trip_data_hadd, by = c("period2", "catch_draw", "tripid",  "month" )) #%>%  select(-decade.x, -decade.y)
    
    # %>%
    
  }
  
  if (hadd_catch_check==0){
    trip_data$tot_hadd_catch<-0
    trip_data$tot_keep_hadd<-0
    trip_data$tot_rel_hadd<-0
  }
  
  
  
  #names<- c(grep("*beta*", names(costs_new_all), value=TRUE, invert=TRUE))
  costs_new_all <- as.data.frame(cost_files_all_base[[x]])   %>% #tibble() %>% 
    filter(catch_draw<=n_catch_draws) 
  
  # %>% #%>%select(all_of(names)) %>%
  #   rename(beta_sqrt_cod_keep_base=beta_sqrt_cod_keep,
  #          beta_sqrt_cod_release_base=beta_sqrt_cod_release,
  #          beta_sqrt_hadd_keep_base=beta_sqrt_hadd_keep,
  #          beta_sqrt_hadd_release_base=beta_sqrt_hadd_release,
  #          beta_sqrt_cod_hadd_keep_base=beta_sqrt_cod_hadd_keep,
  #          beta_trip_length_base=beta_trip_length,
  #          beta_opt_out_base=beta_opt_out, 
  #          beta_cost_base=beta_cost)
  
  # names<- c(grep("*length*", names(trip_data), value=TRUE, invert=TRUE))
  # trip_data<-trip_data %>%
  #   select(all_of(names))
  
  
  # merge the trip data (summer flounder catch + lengths) with the other species data (numbers kept and released))
  trip_data <- trip_data %>% 
    left_join(costs_new_all, by = c("period2","catch_draw","tripid")) #%>%  select(-decade.x, -decade.y)
  
  # %>%
  
  
  
  #Draw from the distirbtuion of utility parameters for each iteration of the projection 
  
  period_vec1 <- period_vec %>%
    #mutate(#beta_sqrt_sf_keep= rnorm(nrow(period_vec), mean = .8093247, sd = 1.288768), 
      #        beta_sqrt_sf_release = rnorm(nrow(period_vec), mean = .0656076 , sd = .2454453) , 
      #        beta_sqrt_bsb_keep = rnorm(nrow(period_vec), mean = .3607657, sd = .2085837), 
      #        beta_sqrt_bsb_release = rnorm(nrow(period_vec), mean = .0665897 , sd = .0711506), 
      #        beta_sqrt_sf_bsb_keep = rnorm(nrow(period_vec), mean =-.0596595  , sd = .161084), 
      #        beta_sqrt_scup_catch = rnorm(nrow(period_vec), mean = .019203 , sd = 0), 
      #        beta_opt_out = rnorm(nrow(period_vec), mean =-1.637635 , sd = 2.059597), 
      #beta_cost = -.0114955) %>% 
    group_by(period2) %>% mutate(tripid = row_number(period2))
  
  trip_data <- trip_data %>% 
    left_join(period_vec1, by = c("period2","tripid")) 
  
  trip_data <- trip_data %>% 
    select(-month.x, -month.y)
  #trip_data <- trip_data  %>%  dplyr::arrange(period2, tripid, catch_draw) 
  
  
  #  utility (prediction year)
  trip_data <-trip_data %>%
    mutate(
      # vA = beta_sqrt_sf_keep*sqrt(tot_keep_sf) +
      #   beta_sqrt_sf_release*sqrt(tot_rel_sf) +  
      #   beta_sqrt_bsb_keep*sqrt(tot_keep_bsb) +
      #   beta_sqrt_bsb_release*sqrt(tot_rel_bsb) +  
      #   beta_sqrt_sf_bsb_keep*(sqrt(tot_keep_sf)*sqrt(tot_keep_bsb)) +
      #   beta_sqrt_scup_catch*sqrt(tot_scup_catch) +
      #   beta_cost*cost,
      
      vA =  trip_data$beta_sqrt_cod_keep*sqrt(trip_data$tot_keep_cod) +
        trip_data$beta_sqrt_cod_release*sqrt(trip_data$tot_rel_cod) +  
        trip_data$beta_sqrt_hadd_keep*sqrt(trip_data$tot_keep_hadd) +
        trip_data$beta_sqrt_hadd_release*sqrt(trip_data$tot_rel_hadd) + 
        trip_data$beta_sqrt_cod_hadd_keep*(sqrt(trip_data$tot_keep_cod)*sqrt(trip_data$tot_keep_hadd)) +
        trip_data$beta_trip_length*sqrt(trip_data$trip_length) +
        trip_data$beta_cost*trip_data$cost, 
      
      #  utility (base year)
      v0 =  trip_data$beta_sqrt_cod_keep*sqrt(trip_data$tot_keep_cod_base) +
        trip_data$beta_sqrt_cod_release*sqrt(trip_data$tot_rel_cod_base) +  
        trip_data$beta_sqrt_hadd_keep*sqrt(trip_data$tot_keep_hadd_base) +
        trip_data$beta_sqrt_hadd_release*sqrt(trip_data$tot_rel_hadd_base) + 
        trip_data$beta_sqrt_cod_hadd_keep*(sqrt(trip_data$tot_keep_cod_base)*sqrt(trip_data$tot_keep_hadd_base)) +
        trip_data$beta_trip_length*sqrt(trip_data$trip_length_base) +
        trip_data$beta_cost*trip_data$cost_base) 
  
  
  #These stats should be roughly the same under no chnage in fishery conditions
  
    mean(trip_data$vA)
    mean(trip_data$v0)
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
  
  
  trip_data <- trip_data %>% 
    mutate(period = as.numeric(as.factor(period2)))
  
  period_names<-subset(trip_data, select=c("period", "period2"))
  period_names <- period_names[!duplicated(period_names), ]
  
  
  mean_trip_data <- trip_data %>% 
    dplyr::select(-c("period2")) %>% data.table() #%>% dplyr::arrange(period, tripid, catch_draw) 
  
  #mean_trip_data<-mean_trip_data %>% dplyr::arrange(period, tripid, catch_draw)
  
  #New code to calculate probability of each choice occasion
  
  # Now expand the data to create two alternatives, representing the alternatives available in choice survey
  #mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
  mean_trip_data <- mean_trip_data %>% 
    mutate(n_alt = rep(2,nrow(.))) %>% 
    uncount(n_alt) %>% 
    mutate(alt = rep(1:2,nrow(.)/2),
           opt_out = ifelse(alt == 2, 1, 0))
  
  
  #Caluculate the expected utility of alts 2 parameters of the utility function, 
  #put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)
  # mean_trip_data<-mean_trip_data %>%
  #   mutate(vA_optout= beta_opt_out*opt_out, 
  #          v0_optout= beta_opt_out*opt_out, 
  #          expon_vA= case_when(alt==1 ~ exp(vA), 
  #                                alt==2 ~ exp(vA_optout)), 
  #          expon_v0= case_when(alt==1 ~ exp(v0), 
  #                                alt==2 ~ exp(v0_optout)))
  
  mean_trip_data <- mean_trip_data %>%
    as.data.table() %>%
    .[, vA_optout := beta_opt_out*opt_out] %>%
    .[, v0_optout := beta_opt_out*opt_out] %>%
    .[alt==1, expon_vA := exp(vA)] %>%
    .[alt==2, expon_vA := exp(vA_optout)] %>%
    .[alt==1, expon_v0 := exp(v0)] %>%
    .[alt==2, expon_v0 := exp(v0_optout)] 
  
  
  
  # mean_trip_data <- mean_trip_data %>% 
  #   group_by(period, tripid, catch_draw) %>% 
  #   mutate(vA_col_sum = sum(expon_vA),
  #          v0_col_sum = sum(expon_v0)) %>% 
  #   ungroup()
  
  
  mean_trip_data <- mean_trip_data %>%
    as.data.table() %>%
    .[, vA_col_sum := sum(expon_vA), by=list(period, catch_draw, tripid)]  %>%
    .[, v0_col_sum := sum(expon_v0), by=list(period, catch_draw, tripid)]
  
  
  # 
  # mean_trip_data1 <- mean_trip_data %>% 
  #   mutate(change_CS =(1/beta_cost)*(log(vA_col_sum)-log(v0_col_sum)), 
  #          probA = expon_vA/vA_col_sum,
  #          prob0 = expon_v0/v0_col_sum) 
  # 
  
  mean_trip_data <- mean_trip_data %>%
    as.data.table() %>%
    .[, change_CS := (1/beta_cost)*(log(vA_col_sum)-log(v0_col_sum))] %>%
    .[, probA :=expon_vA/vA_col_sum] %>%
    .[, prob0 :=expon_v0/v0_col_sum] 
  
  mean(mean_trip_data$change_CS)

  mean_trip_data<- subset(mean_trip_data, alt==1)
  
  # mean_trip_data1<-mean_trip_data %>% group_by(period,tripid) %>% summarise(across(everything(), mean), .groups = 'drop') %>% 
  #   tibble()
  
  
  all_vars<-c()
  all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period","tripid")]
  
  
  
  mean_trip_data<-mean_trip_data  %>% as.data.table() %>%
    .[,lapply(.SD, mean), by = c("period","tripid"), .SDcols = all_vars]
  
  
  #original code
  # Collapse data from the X catch draws so that each row contains mean values
  #mean_trip_data <-aggregate(trip_data, by=list(trip_data$tripid),FUN=mean, na.rm=TRUE)
  # mean_trip_data <- trip_data %>% 
  #   dplyr::select(-c("state", "period2", "mode1", "month1")) %>% data.table() #%>% dplyr::arrange(period, tripid, catch_draw) 
  # 
  # mean_trip_data <- mean_trip_data[, lapply(.SD, mean), by=list(period,tripid)] %>% 
  #   tibble() #%>% 
  # 
  # mean(mean_trip_data$vA)
  # mean(mean_trip_data$v0)
  # # 
  # mean(mean_trip_data$tot_keep_sf)
  # mean(mean_trip_data$tot_keep_sf_base)
  # 
  # mean(mean_trip_data$tot_rel_sf)
  # mean(mean_trip_data$tot_rel_sf_base)
  # 
  # mean(mean_trip_data$tot_keep_bsb)
  # mean(mean_trip_data$tot_keep_bsb_base)
  # 
  # mean(mean_trip_data$tot_rel_bsb)
  # mean(mean_trip_data$tot_rel_bsb_base)
  # 
  # mean(mean_trip_data$tot_scup_catch)
  # mean(mean_trip_data$tot_cat_scup_base)
  # 
  # mean(mean_trip_data$prob0)
  # mean(mean_trip_data$probA)
  
  # 
  # 
  # 
  # # nkeep <- trip_data %>%
  # #   group_by(period, tripid) %>%
  # #   summarise(keep_one = length(which(tot_keep>0))/length(tot_keep), #n(),
  # #             .groups = "drop")
  # # mean_trip_data <- left_join(mean_trip_data, nkeep, by = c("period", "tripid"))
  # 
  # 
  # # Now expand the data to create two alternatives, representing the alternatives available in choice survey
  # #mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
  # mean_trip_data <- mean_trip_data %>% 
  #   mutate(n_alt = rep(2,nrow(.))) %>% 
  #   uncount(n_alt) %>% 
  #   mutate(alt = rep(1:2,nrow(.)/2),
  #          opt_out = ifelse(alt == 2, 1, 0))
  # 
  # 
  # 
  # #Caluculate the expected utility of alts 2 parameters of the utility function
  # mean_trip_data$vA_optout= mean_trip_data$beta_opt_out*mean_trip_data$opt_out 
  # mean_trip_data$v0_optout= mean_trip_data$beta_opt_out*mean_trip_data$opt_out 
  # 
  # 
  # #Now put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)
  # mean_trip_data$expon_vA <- case_when(mean_trip_data$alt==1 ~ exp(mean_trip_data$vA), 
  #                                      mean_trip_data$alt==2 ~ exp(mean_trip_data$vA_optout)) 
  # 
  # mean_trip_data$expon_v0 <- case_when(mean_trip_data$alt==1 ~ exp(mean_trip_data$v0), 
  #                                      mean_trip_data$alt==2 ~ exp(mean_trip_data$v0_optout)) 
  # 
  # mean_trip_data <- mean_trip_data %>% 
  #   group_by(period, tripid) %>% 
  #   mutate(vA_col_sum = sum(expon_vA),
  #          v0_col_sum = sum(expon_v0)) %>% 
  #   ungroup()
  # 
  # mean_trip_data <- mean_trip_data %>% 
  #   mutate(CS_base = (1/beta_cost)*log(v0_col_sum),
  #          CS_alt = (1/beta_cost)*log(vA_col_sum), 
  #          change_CS = CS_alt-CS_base, 
  #          probA = expon_vA/vA_col_sum,
  #          prob0 = expon_v0/v0_col_sum) 
  
  #end original code
  
  #   vars<- grep("*length*", names(mean_trip_data), invert=TRUE, value=TRUE)
  #   mean_trip_data_check[[x]]  <- subset(mean_trip_data,select = c(vars))
  #   mean_trip_data_check[[x]]$run<-x
  # }
  #   mean_trip_data_check_all<- list.stack(mean_trip_data_check)
  #   mean_trip_data_check_all <- subset(mean_trip_data_check_all, alt==1)
  # 
  #   aggregate_mean_trip_data_check_all <- mean_trip_data_check_all %>% 
  #     group_by(run) %>% 
  #     summarize_all(sum, na.rm = TRUE) %>% 
  #     ungroup()
  #     
  # 
  
  
  # mutate(change_CS = -(1/beta_cost)*(log(expon_v0) - log(expon_vA))) %>% 
  #   mutate(probA = expon_vA/vA_col_sum,
  #          prob0 = expon_v0/v0_col_sum)
  #mean(mean_trip_data$prob0) 
  # Get rid of things we don't need. 
  mean_trip_data <- subset(mean_trip_data, alt==1,select=-c(alt, catch_draw, expon_v0 ,v0_col_sum, expon_vA, 
                                                            opt_out, v0, v0_optout, vA, vA_optout, vA_col_sum)) 
  
  # Multiply the average trip probability in the Alternative scenario (probA) 
  #by each of the catch variables ( the variables below) to get probability-weighted catch
  list_names <- c("tot_keep_cod","tot_keep_hadd", "tot_rel_cod", "tot_rel_hadd" )
  
  # for (l in list_names){
  #   mean_trip_data[,l] <- mean_trip_data[,l]*mean_trip_data$probA
  # }
  
  
  mean_trip_data <- mean_trip_data %>%
    as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
    .[]
  
  # Multiply the average trip probability in the base scenario (prob0) 
  #by each of the catch variables (not the variables below) to get probability-weighted catch

  list_names <- c("tot_keep_cod_base","tot_keep_hadd_base", "tot_rel_cod_base", "tot_rel_hadd_base" )
  
  
  
  mean_trip_data <- mean_trip_data %>%
    as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
    .[]
  
  
  
  mean_trip_data <- mean_trip_data %>%
    mutate( n_choice_occasions_alt = rep(1,nrow(.))) %>%
    left_join(period_names, by = c("period")) 
  
  mean_trip_data <- mean_trip_data %>%
    dplyr::select(-c("period")) 
  
  
  #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in 
  #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
  calibration_data <- calibration_data  #%>%   rename(period2 = period)
  
  sims <- calibration_data %>% 
    dplyr::select(c(n_choice_occasions, period)) %>% 
    rename(period2=period) %>% 
    left_join(mean_trip_data %>% count(period2, name = "ndraws") %>% mutate(period = as.character(period2)), by = "period2") %>% 
    mutate(expand = n_choice_occasions/ndraws)
  
  
  
  #mean_trip_data$sim=1
  mean_trip_data <- mean_trip_data %>%
    mutate(sim = rep(1,nrow(.))) 
  
  #Here add other trip quality statistics
  #keep_one = ifelse(tot_keep>0,1,0))
  
  #datset to compute mean cv over all trips
  trip_level_output <- mean_trip_data %>% 
    left_join(sims , by = c("period2")) %>% 
    #mutate(seed=eff_seed)   %>%
    select(c(period2, tripid, expand, change_CS,  probA, prob0, tot_keep_cod, tot_keep_hadd, tot_keep_cod_base, tot_keep_hadd_base,
             tot_rel_cod, tot_rel_hadd, tot_rel_cod_base, tot_rel_hadd_base)) 
  
  
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

