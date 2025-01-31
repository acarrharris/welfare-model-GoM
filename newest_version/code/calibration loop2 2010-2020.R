

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
    
  catch_data <- catch_data_all_split %>%
    dplyr::right_join(regs_check, by="period2") %>% 
    dplyr::select(-dtrip) 


    
    #Here we can loop around the the suffix on the catch variables 
    catch_data0 <- catch_data  %>% 
      dplyr::select(catch_draw, mode,month, area, state, period2, tripid, tot_cat_cod, tot_cat_hadd)

    
    catch_data1<- catch_data  %>% 
      dplyr::select(catch_draw, mode,month, area, state, period2, tripid, tot_cat_cod, tot_cat_hadd) %>% 
      dplyr::rename(tot_cod_catch=tot_cat_cod, tot_hadd_catch=tot_cat_hadd)


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
    

    #trip_data<-data.frame(trip_data[trip_data_hadd])
    trip_data<-trip_data %>% 
      dplyr::left_join(trip_data_hadd, by=c("period2", "catch_draw", "tripid",  "month"))
    
    
    rm(trip_data_hadd, catch_data_cod, cod_hadd_catch_data, cod_zero_catch, hadd_zero_catch, summed_catch_data,hadd_catch_data)
    
    # costs_new_all <- readRDS(paste0(output_data_cd, "cost_files_2021", "_", x, ".rds")) %>% 
    #   filter(catch_draw<=n_catch_draws)
    # 
    # 
    # trip_data <- trip_data %>% 
    #   left_join(costs_new_all, by=c("period2", "catch_draw", "tripid")) %>%
    #   dplyr::select(-month) %>% 
    #   left_join(period_vec4, by=c("period2")) %>% 
    #   dplyr::mutate(                   
    #     domain2 = paste0(mode, "_", area, "_", st),
    #     tot_cod_catch = tot_keep_cod + tot_rel_cod,
    #     tot_hadd_catch = tot_keep_hadd + tot_rel_hadd)
    # 
    # 
    # rm(costs_new_all)
  param_draws<-trip_data %>% 
    dplyr::select(period2, tripid) %>% 
    dplyr::distinct(period2, tripid, .keep_all = TRUE)  
  
  nrows<-nrow(param_draws)
  

  param_draws<-param_draws%>% 
    dplyr::mutate(beta_sqrt_cod_keep = rnorm(nrows, mean = 1.594, sd = .615),
                  beta_sqrt_cod_release = rnorm(nrows, mean = 0.162 , sd = 0.445),
                  beta_sqrt_hadd_keep = rnorm(nrows, mean = 1.156, sd = 0.603 ),
                  beta_sqrt_hadd_release = rnorm(nrows, mean = 0.094 , sd = 0 ),
                  beta_sqrt_cod_hadd_keep = rnorm(nrows, mean =-0.314  , sd = 0.778 ),
                  beta_cost = rnorm(nrows, mean =-0.015 , sd =0 ),
                  beta_opt_out = rnorm(nrows, mean =-1.871 , sd = 3.208 ), 
                  beta_opt_out_age = rnorm(nrows, mean =0.047 , sd = 0 ), 
                  beta_opt_out_likely = rnorm(nrows, mean =-1.272 , sd = 0 ), 
                  beta_opt_out_prefer = rnorm(nrows, mean =-1.079 , sd = 0 ), 
                  id=1:nrow(param_draws))
  
  
  if (y==2010){
    param_draws<-param_draws %>% 
      dplyr::mutate(cost=rnorm(nrow(param_draws), mean=48.43147,sd= 3.460814))
  }
  
  if (y==2011){
    param_draws<-param_draws %>% 
      dplyr::mutate(cost=rnorm(nrow(param_draws), mean=49.51982,sd= 3.538585))
  }
  
  if (y==2012){
    param_draws<-param_draws %>% 
      dplyr::mutate(cost=rnorm(nrow(param_draws), mean=50.60817,sd= 3.616356))
  }
  
  if (y==2013){
    param_draws<-param_draws %>% 
      dplyr::mutate(cost=rnorm(nrow(param_draws), mean=51.69652,sd= 3.694127))
  }
  
  if (y==2014){
    param_draws<-param_draws %>% 
      dplyr::mutate(cost=rnorm(nrow(param_draws), mean=52.24069,sd= 3.733012))
  }
  
  if (y==2015){
    param_draws<-param_draws %>% 
      dplyr::mutate(cost=rnorm(nrow(param_draws), mean=52.24069,sd= 3.733012))
  }
  
  if (y==2016){
    param_draws<-param_draws %>% 
      dplyr::mutate(cost=rnorm(nrow(param_draws), mean=53.32904,sd= 3.810784))
  }
  
  if (y==2017){
    param_draws<-param_draws %>% 
      dplyr::mutate(cost=rnorm(nrow(param_draws), mean=54.41739,sd= 3.888555))
  }
  
  if (y==2018){
    param_draws<-param_draws %>% 
      dplyr::mutate(cost=rnorm(nrow(param_draws), mean=55.50573,sd= 3.966326))
  }
  
  if (y==2019){
    param_draws<-param_draws %>% 
      dplyr::mutate(cost=rnorm(nrow(param_draws), mean=56.59408,sd= 4.044097))
  }
  
  if (y==2020){
    param_draws<-param_draws %>% 
      dplyr::mutate(cost=rnorm(nrow(param_draws), mean=57.68243,sd= 4.121868))
  }
  
  if (y==2021){
    param_draws<-param_draws %>% 
      dplyr::mutate(cost=rnorm(nrow(param_draws), mean=58.68243,sd= 4.121868))
  }
  #trip_data$trip_length<-rnorm(nrow(trip_data), mean=6.74066,sd= .1470764)
  
  #Ages 
  age_distn <- data.frame(read.csv(paste0(input_data_cd,"age_distribution_by_state.csv")))%>% 
    dplyr::filter(state == "MA")  %>% 
    dplyr::select(-state)
  
  #next two commands ensure there are enough observations  per period
  expand_rows=ceiling((nrows/nrow(age_distn))+10)
  
  age_distn <- age_distn %>% 
    dplyr::slice(rep(1:dplyr::n(), each = expand_rows)) 
   
  # Number of rows to sample
  n <- nrow(param_draws)
  
  # Randomly sample n rows
  age_distn <- age_distn[sample(nrow(age_distn), n), ]
  
  age_distn<-as.data.frame(age_distn) %>% 
    dplyr::rename(age=age_distn)
  
  age_distn <- age_distn %>% 
    dplyr::mutate(id=1:nrow(age_distn))

  param_draws<-param_draws %>% 
    dplyr::left_join(age_distn, by=c("id"))
  
  
  
  ##pull in choice experiment demographics
  angler_dems <- read.csv(paste0(input_data_cd,"angler CE demographics.csv")) %>% 
    dplyr::slice(rep(row_number(), each = round(n/448+5))) %>% 
    dplyr::mutate(uniform=runif(n(), min=0, max=1)) %>% 
    dplyr::arrange(uniform)
  
  angler_dems <- angler_dems[sample(nrow(angler_dems), n), ]
  
  angler_dems<-as.data.frame(angler_dems) %>% 
    dplyr::select(-uniform) %>% 
    dplyr::mutate(id=1:nrow(age_distn))
  
  param_draws<-param_draws %>% 
    dplyr::left_join(angler_dems, by=c("id"))
  
  trip_data <-trip_data %>%
      dplyr::left_join(param_draws, by=c("period2", "tripid"))
    
  trip_data <-trip_data %>%
    dplyr::arrange(period2, tripid, catch_draw)

  trip_data<-  trip_data%>% 
      dplyr::mutate(vA=
                      beta_sqrt_cod_keep*sqrt(trip_data$tot_keep_cod) +
                      beta_sqrt_cod_release*sqrt(trip_data$tot_rel_cod) +  
                      beta_sqrt_hadd_keep*sqrt(trip_data$tot_keep_hadd) +
                      beta_sqrt_hadd_release*sqrt(trip_data$tot_rel_hadd) + 
                      beta_sqrt_cod_hadd_keep*(sqrt(trip_data$tot_keep_cod)*sqrt(trip_data$tot_keep_hadd)) +
                      beta_cost*trip_data$cost) %>% 
      dplyr::select(-id)
   
  
  trip_data <- trip_data %>%
    dplyr::mutate(period = as.numeric(as.factor(period2)))
  
  period_names<-subset(trip_data, select=c("period", "period2"))
  period_names <- period_names[!duplicated(period_names), ]
  

  
  mean_trip_data <- trip_data %>% data.table::data.table() %>% 
    .[, group_index := .GRP, by = .(period2, catch_draw, tripid)]
  
  # Now expand the data to create two alternatives, representing the alternatives available in choice survey
  mean_trip_data <- mean_trip_data %>%
    dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
    tidyr::uncount(n_alt) %>%
    dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                  opt_out = ifelse(alt == 2, 1, 0))
  
  #Calculate the expected utility of alts 2 parameters of the utility function,
  setDT(mean_trip_data)
  
  # Filter only alt == 2 once, and calculate vA 
  mean_trip_data[alt == 2, "vA" := .(
    beta_opt_out * opt_out +
      beta_opt_out_age * (age * opt_out) +
      beta_opt_out_likely * (likely_to_fish * opt_out) +
      beta_opt_out_prefer * (fish_pref_more * opt_out)
  )]
  
  # Pre-compute exponential terms
  mean_trip_data[, `:=`(exp_vA = exp(vA))]
  
  # Group by group_index and calculate probabilities and log-sums
  mean_trip_data[, `:=`(
    probA = exp_vA / sum(exp_vA)
  ), by = group_index]
  
  
  
  mean_trip_data<- subset(mean_trip_data, alt==1) %>% 
    dplyr::select(-group_index) %>% 
    dplyr::mutate(tot_cat_cod=tot_keep_cod+tot_rel_cod, 
                  tot_cat_hadd=tot_keep_hadd+tot_rel_hadd)
  
  
  # Get rid of things we don't need.
  mean_trip_data <- mean_trip_data %>% 
    dplyr::filter(alt==1) %>% 
    dplyr::select(-alt, -beta_cost,-beta_opt_out, -beta_opt_out_age, 
                  -beta_opt_out_likely, -beta_opt_out_prefer, -beta_sqrt_cod_hadd_keep, 
                  -beta_sqrt_cod_keep, -beta_sqrt_cod_release, -beta_sqrt_hadd_keep, 
                  -beta_sqrt_hadd_release, -likely_to_fish, -fish_pref_more, -period, 
                  -opt_out,-vA, -cost, -age, -exp_vA)
  
  # Multiply the trip probability by each of the catch variables (not the variables below) to get probability-weighted catch
  list_names <- colnames(mean_trip_data)[colnames(mean_trip_data) !="tripid" 
                                         & colnames(mean_trip_data) !="period2" 
                                         & colnames(mean_trip_data) !="probA" 
                                         & colnames(mean_trip_data) !="catch_draw"
                                         & colnames(mean_trip_data) !="month"]
  
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
    .[]
  
  
  
  mean_trip_data_prob_catch_draw<-mean_trip_data %>% 
    dplyr::select("period2","tripid", "catch_draw", "probA")
  
  
  #Take the average outcomes across catch draws
  all_vars<-c()
  all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period2","tripid", "month")]
  all_vars
  
  mean_trip_data<-mean_trip_data  %>% data.table::as.data.table() %>%
    .[,lapply(.SD, mean), by = c("period2","tripid", "month"), .SDcols = all_vars]
  
  
  mean_trip_data <- mean_trip_data %>%
    dplyr::mutate(n_choice_occasions = rep(1,nrow(.))) %>%
    dplyr::left_join(period_names, by = c("period2"))
  
  
  
  
  
  
  
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
      .[alt==1, expon_vA := exp(vA)] %>%
      .[alt==2, expon_vA := exp(vA_optout)] 
    
    
    
    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[, vA_col_sum := sum(expon_vA), by=list(period2, catch_draw, tripid)]  
    
    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[, probA :=expon_vA/vA_col_sum] 

    # mean(mean_trip_data$change_CS)
    
    mean_trip_data<- subset(mean_trip_data, alt==1)
    
    mean_trip_data<- mean_trip_data %>% 
      dplyr::mutate(tot_cod_cat=tot_keep_cod+tot_rel_cod, 
                    tot_hadd_cat=tot_keep_hadd+tot_rel_hadd)
    
    # mean_trip_data1<-mean_trip_data %>% group_by(period,tripid) %>% summarise(across(everything(), mean), .groups = 'drop') %>% 
    #   tibble()
    
    # Get rid of things we don't need. 
    mean_trip_data <- mean_trip_data %>% 
      dplyr::select(-c(age, alt, beta_cost,beta_opt_out,beta_opt_out_age,       
                       beta_opt_out_likely,beta_opt_out_prefer,beta_sqrt_cod_hadd_keep,  
                       beta_sqrt_cod_keep,beta_sqrt_cod_release,beta_sqrt_hadd_keep, 
                       beta_sqrt_hadd_release, catch_draw, cost,opt_out,  
                       expon_vA, fish_pref_more, likely_to_fish, month,
                       vA, vA_col_sum, vA_optout)) 
    
    all_vars<-c()
    all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period2","tripid")]
    all_vars
    
    
    mean_trip_data<-mean_trip_data  %>% as.data.table() %>%
      .[,lapply(.SD, mean), by = c("period2","tripid"), .SDcols = all_vars]
    
    
    
# Multiply the average trip probability in the Alternative scenario (probA) 
#by each of the catch variables ( the variables below) to get probability-weighted catch
    
    list_names <- c("tot_keep_cod","tot_keep_hadd", "tot_rel_cod", "tot_rel_hadd", "tot_cod_cat", "tot_hadd_cat")
    
    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
      .[]
    
   
    
    
#Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in 
    #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
    # calibration_data <- calibration_data  #%>%   rename(period2 = period)
    dtrips <- directed_trips_p %>% 
      dplyr::select(c(dtrip, period2)) 
    
    mean_trip_data<-mean_trip_data %>% 
      left_join(dtrips, by = "period2") 
    
    mean_trip_data <-mean_trip_data %>% 
      group_by(period2) %>% 
      dplyr::mutate(mean_prob=mean(probA), 
                    sims=dtrip/mean_prob, 
                    expand=sims/n_draws, 
                    n_choice_occasions=1)
      
    list_names = c("tot_cod_cat","tot_keep_cod","tot_rel_cod", 
                   "tot_hadd_cat","tot_keep_hadd","tot_rel_hadd",
                   "probA","n_choice_occasions" )
    
    
    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * expand), .SDcols = list_names] %>%
      .[]
    
    #mean_trip_data$sim=1
    
    #sum probability weighted catch over all choice occasions
    aggregate_trip_data <- mean_trip_data %>%
      dplyr::select(-tripid) %>%
      data.table::as.data.table() %>%
      .[,lapply(.SD, sum),  .SDcols = list_names]
    
    
    names(aggregate_trip_data)[names(aggregate_trip_data) == "probA"] = "estimated_trips"
    
    aggregate_trip_data$sim =1
    
    
    ###Compare calibration model output with MRIP 
    
    MRIP_data <- data.frame( read.csv(paste0(input_data_cd,"total AB1B2 2010_2020 GoM.csv")))
    MRIP_data<-MRIP_data %>% 
      dplyr::filter(year==y)
    
    rm(catch_data1, mean_trip_data, mean_trip_data_prob_catch_draw, param_draws, trip_data)
    


    #cod
    #print("model cod harvest")
    sum(aggregate_trip_data$tot_keep_cod)
    #print("MRIP cod harvest")
    sum(MRIP_data$cod_harvest)
    #print("percent diff cod harvest")
    cod_harvest_perc_diff<-((sum(aggregate_trip_data$tot_keep_cod)-sum(MRIP_data$cod_harvest))/sum(MRIP_data$cod_harvest))*100
    cod_harvest_perc_diff
    cod_harvest_diff=abs(sum(MRIP_data$cod_harvest)-sum(aggregate_trip_data$tot_keep_cod))

    sum(aggregate_trip_data$tot_rel_cod)
    sum(MRIP_data$cod_releases)
    cod_rel_perc_diff<-((sum(aggregate_trip_data$tot_rel_cod)-sum(MRIP_data$cod_releases))/sum(MRIP_data$cod_releases))*100
    cod_rel_perc_diff

    sum(aggregate_trip_data$tot_cod_cat)
    sum(MRIP_data$cod_tot_cat)
    cod_tot_cat_perc_diff<-((sum(aggregate_trip_data$tot_cod_cat)-sum(MRIP_data$cod_tot_cat))/sum(MRIP_data$cod_tot_cat))*100
    cod_tot_cat_perc_diff




    ##haddock
    #print("model hadd harvest")
    sum(aggregate_trip_data$tot_keep_hadd)
    #print("mrip hadd harvest")
    sum(MRIP_data$hadd_harvest)
    #print("percent diff hadd harvest")
    hadd_harvest_perc_diff<-((sum(aggregate_trip_data$tot_keep_hadd)-sum(MRIP_data$hadd_harvest))/sum(MRIP_data$hadd_harvest))*100
    hadd_harvest_perc_diff
    hadd_harvest_diff=abs(sum(MRIP_data$hadd_harvest)-sum(aggregate_trip_data$tot_keep_hadd))

    #print("model hadd release")
    sum(aggregate_trip_data$tot_rel_hadd)
    #print("mrip hadd release")
    sum(MRIP_data$hadd_releases)
    #print("percent diff hadd release")
    hadd_rel_perc_diff<- ((sum(aggregate_trip_data$tot_rel_hadd)-sum(MRIP_data$hadd_releases))/sum(MRIP_data$hadd_releases))*100
    hadd_rel_perc_diff


    sum(aggregate_trip_data$tot_hadd_cat)
    sum(MRIP_data$hadd_tot_cat)
    hadd_tot_cat_perc_diff<-((sum(aggregate_trip_data$tot_hadd_cat)-sum(MRIP_data$hadd_tot_cat))/sum(MRIP_data$hadd_tot_cat))*100
    hadd_tot_cat_perc_diff


   

