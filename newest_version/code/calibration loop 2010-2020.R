

time_a<-Sys.time()
p_star_cod <- p_star_cod_variable
p_star_hadd<-p_star_hadd_variable


######################################
##   Begin simulating trip outcomes ##
######################################

#Import directed trips file - gives directed trips by regulatory period in 2020
#directed_trips <- data.frame(read.csv("directed trips and regulations 2020.csv"))                                                                            

#directed_trips= subset(directed_trips, state == state1)

# min_period=min(directed_trips$period)
# max_period=max(directed_trips$period)



# Set up an output file for the separately simulated within-season regulatory periods  
pds = list()

periodz=as.factor(directed_trips$period2)
levels(periodz)


for(p in levels(periodz)){
  p<- "pr_15_offshore"
  directed_trips_p = subset(directed_trips, period2 == p)
  n_trips = mean(directed_trips_p$dtrip)
  #n_draws = min(1000,n_trips*2.5 )
  n_draws = n_drawz
  
  
  cod_bag = mean(directed_trips_p$cod_bag)
  cod_min = mean(directed_trips_p$cod_min)
  hadd_bag = mean(directed_trips_p$hadd_bag)
  hadd_min = mean(directed_trips_p$hadd_min)
  
  
  #obtain the month for the period and draw from the relevant catch file 
  month <-as.numeric(directed_trips_p[1,]$month)
  month1 <- sprintf("%02d", month)
  
  # Set up an output file for catch draw files 
  dfs = list()
  
  #Run the catch loop X times to give X draws of catch for each species
  for(i in 1:n_catch_draws) {
    
    #######
    #GOM_catch_data = data.frame(read.csv(paste0('simulated_catch_',month1, '.csv')))
    GOM_catch_data = subset(catch_data_all, period2==p)
    #GOM_catch_data[is.na(GOM_catch_data)] <- 0
    
    cod_tot_cat = GOM_catch_data$tot_cat_cod
    hadd_tot_cat = GOM_catch_data$tot_cat_hadd
    GOM_catch_data = data.frame(cod_tot_cat,hadd_tot_cat)
    
    #end method 2)
    #######
    
    # random draw of cod and hadd catch
    GOM_catch_data = as.data.frame(GOM_catch_data[sample(1:nrow(GOM_catch_data), n_drawz), ])
    
    #see if there is positive catch for all species; if not, then skip the keep/release allocation 
    catch_check_cod<-sum(GOM_catch_data$cod_tot_cat)
    catch_check_hadd<-sum(GOM_catch_data$hadd_tot_cat)    
    
    
    GOM_catch_data$tripid = 1:nrow(GOM_catch_data)
    codd_hadd_catch_data = GOM_catch_data
    
    
    # subset trips with zero catch, as no size draws are required
    cod_zero_catch = subset(GOM_catch_data, cod_tot_cat == 0)
    
    
    
    if (catch_check_cod!=0){
      
      #remove trips with zero cod catch, will add them on later
      GOM_catch_data=GOM_catch_data[GOM_catch_data$cod_tot_cat!=0, ]
      
      
      #expand the GOM_catch_data so that each row represents a fish
      row_inds = seq_len(nrow(GOM_catch_data))
      GOM_catch_data = GOM_catch_data[c(rep(row_inds, GOM_catch_data$cod_tot_cat)), ]
      rownames(GOM_catch_data) = NULL
      GOM_catch_data$fishid = 1:nrow(GOM_catch_data)
      
      
      
      #Execute the following code if the seasonal period has a positive bag limit 
      if(cod_bag>0){
        
        GOM_catch_data1= as.data.frame(GOM_catch_data)  
        GOM_catch_data1$uniform=runif(nrow(GOM_catch_data1))
        GOM_catch_data1$keep = ifelse(GOM_catch_data1$uniform>=p_star_cod, 1,0)
        
        GOM_catch_data1$csum_keep <- ave(GOM_catch_data1$keep, GOM_catch_data1$tripid, FUN=cumsum)
        GOM_catch_data1$keep_adj = ifelse(GOM_catch_data1$csum_keep>cod_bag, 0,GOM_catch_data1$keep)
        
        # #Add the following lines to end the trip once the bag limit is reached (rather than continuing to discard)
        # ###
        # sf_catch_data1$post_bag_fish=ifelse(sf_catch_data1$csum_keep>fluke_bag, 1,0)
        # sf_catch_data1= subset(sf_catch_data1,post_bag_fish==0 )
        # sf_catch_data1 <- subset(sf_catch_data1, select=-c(post_bag_fish ))
        # ###
        
        GOM_catch_data1 <- subset(GOM_catch_data1, select=-c(keep, csum_keep))
        names(GOM_catch_data1)[names(GOM_catch_data1) == "keep_adj"] = "keep"
        
        GOM_catch_data1$release = ifelse(GOM_catch_data1$keep==0, 1,0) 
        GOM_catch_data1=subset(GOM_catch_data1, select=c(tripid, keep, release))
        
        GOM_catch_data1 <- GOM_catch_data1 %>% 
          group_by(tripid) %>% 
          summarize(keep = sum(keep),
                    release = sum(release),
                    .groups = "drop") #%>% 
        
        names(GOM_catch_data1)[names(GOM_catch_data1) == "keep"] = "tot_keep_cod"
        names(GOM_catch_data1)[names(GOM_catch_data1) == "release"] = "tot_rel_cod"
        
      }
      
      if(cod_bag==0){
        
        GOM_catch_data1= as.data.frame(GOM_catch_data)  
        GOM_catch_data1$keep = 0
        GOM_catch_data1$release = 1
        
        GOM_catch_data1=subset(GOM_catch_data1, select=c(tripid, keep, release))
        GOM_catch_data1 <- GOM_catch_data1 %>% 
          group_by(tripid) %>% 
          summarize(keep = sum(keep),
                    release = sum(release),
                    .groups = "drop") #%>% 
        
        names(GOM_catch_data1)[names(GOM_catch_data1) == "keep"] = "tot_keep_cod"
        names(GOM_catch_data1)[names(GOM_catch_data1) == "release"] = "tot_rel_cod"
        
      }
      
      
      
      trip_data =  as.data.frame(GOM_catch_data1)
      
      #add the zero catch trips 
      trip_data = bind_rows(trip_data, cod_zero_catch)
      
      #quick sort and cleanup 
      trip_data = trip_data[order(trip_data$tripid),]
      rownames(trip_data) <- NULL
      
      trip_data<-subset(trip_data, select=-c(cod_tot_cat, hadd_tot_cat))
      trip_data[is.na(trip_data)] = 0
      trip_data$tot_cod_catch = trip_data$tot_keep_cod+trip_data$tot_rel_cod
      trip_data[is.na(trip_data)] = 0
      
    }
    
    
    if (catch_check_cod==0){
      trip_data<-GOM_catch_data 
      trip_data$tot_keep_cod<-0
      trip_data$tot_rel_cod<-0
      trip_data$tot_cod_catch = trip_data$tot_keep_cod+trip_data$tot_rel_cod
      trip_data <- subset(trip_data, select=-c(cod_tot_cat, hadd_tot_cat))
      
    }
    
    #########################
    ###  Haddock  ####
    #########################
    
    
    #draw sizes for haddock catch
    hadd_catch_data =subset(codd_hadd_catch_data, select=c(tripid, hadd_tot_cat))
    hadd_catch_data = hadd_catch_data[!duplicated(hadd_catch_data), ]
    
    #subset trips with zero bsb catch 
    hadd_zero_catch = subset(hadd_catch_data, hadd_tot_cat == 0, select=c(tripid, hadd_tot_cat))
    
    if (catch_check_hadd!=0){
      
      
      #remove trips with zero bsb catch, will add them on later
      hadd_catch_data=hadd_catch_data[hadd_catch_data$hadd_tot_cat!=0, ]
      rownames(hadd_catch_data) = NULL
      
      
      #expand the bsb_catch_data so that each row represents a fish
      row_inds = seq_len(nrow(hadd_catch_data))
      hadd_catch_data[is.na(hadd_catch_data)] = 0
      hadd_catch_data = hadd_catch_data[c(rep(row_inds, hadd_catch_data$hadd_tot_cat)), ]
      rownames(hadd_catch_data) = NULL
      hadd_catch_data$fishid = 1:nrow(hadd_catch_data)
      
      
      
      #Execute the following code if the seasonal period has a positive bag limit 
      if(hadd_bag>0){
        
        hadd_catch_data1= as.data.frame(hadd_catch_data)  
        hadd_catch_data1$uniform=runif(nrow(hadd_catch_data1))
        hadd_catch_data1$keep = ifelse(hadd_catch_data1$uniform>=p_star_hadd, 1,0) 
        
        hadd_catch_data1$csum_keep <- ave(hadd_catch_data1$keep, hadd_catch_data1$tripid, FUN=cumsum)
        hadd_catch_data1$keep_adj = ifelse(hadd_catch_data1$csum_keep>hadd_bag, 0,hadd_catch_data1$keep)
        hadd_catch_data1 <- subset(hadd_catch_data1, select=-c(keep, csum_keep))
        names(hadd_catch_data1)[names(hadd_catch_data1) == "keep_adj"] = "keep"
        
        
        hadd_catch_data1$release = ifelse(hadd_catch_data1$keep==0, 1,0) 
        
        hadd_catch_data1=subset(hadd_catch_data1, select=c(tripid, keep, release))
        
        hadd_catch_data1 <- subset(hadd_catch_data1, select=c(tripid, keep, release))
        hadd_catch_data1 <- hadd_catch_data1 %>% 
          group_by(tripid) %>% 
          summarize(keep = sum(keep),
                    release = sum(release),
                    .groups = "drop") #%>% 
        
        names(hadd_catch_data1)[names(hadd_catch_data1) == "keep"] = "tot_keep_hadd"
        names(hadd_catch_data1)[names(hadd_catch_data1) == "release"] = "tot_rel_hadd"
        
      }
      
      if(hadd_bag==0){
        
        hadd_catch_data1= as.data.frame(hadd_catch_data)  
        hadd_catch_data1$keep = 0
        hadd_catch_data1$release = 1
        
        hadd_catch_data1 <- subset(hadd_catch_data1, select=c(tripid, keep, release))
        hadd_catch_data1 <- hadd_catch_data1 %>% 
          group_by(tripid) %>% 
          summarize(keep = sum(keep),
                    release = sum(release),
                    .groups = "drop") #%>% 
        
        names(hadd_catch_data1)[names(hadd_catch_data1) == "keep"] = "tot_keep_hadd"
        names(hadd_catch_data1)[names(hadd_catch_data1) == "release"] = "tot_rel_hadd"
        
      }
      
      
      #add the zero catch trips 
      hadd_catch_data1 = bind_rows(hadd_catch_data1, hadd_zero_catch)
      hadd_catch_data1 = subset(hadd_catch_data1, select=-c(hadd_tot_cat))
      
      #quick sort and cleanup 
      hadd_catch_data1 = hadd_catch_data1[order(hadd_catch_data1$tripid),]
      rownames(hadd_catch_data1) <- NULL
      
      
      
      hadd_catch_data1[is.na(hadd_catch_data1)] = 0
      
      
      # merge the trip data (summer flounder catch, lengths, and cost) with the bsb data (numbers kept and released))
      trip_data <- trip_data %>% 
        left_join(hadd_catch_data1,by="tripid")
      
      trip_data[is.na(trip_data)] = 0
      
      trip_data$tot_hadd_catch = trip_data$tot_keep_hadd+trip_data$tot_rel_hadd
      trip_data[is.na(trip_data)] = 0
      
    }
    
    if (catch_check_hadd==0){ 
      trip_data_hadd<-hadd_catch_data
      trip_data_hadd$tot_keep_hadd<-0
      trip_data_hadd$tot_rel_hadd<-0
      trip_data_hadd$tot_hadd_catch<-0
      trip_data_hadd<-trip_data_hadd %>% 
        dplyr::select(-hadd_tot_cat)
      
      trip_data <-  trip_data %>% 
        left_join(trip_data_hadd,by="tripid")
      
    }
    
    
    
    trip_data[is.na(trip_data)] = 0
    
    
    trip_data$catch_draw=i
    dfs[[i]]=trip_data
    
  }
  
  #trip_data_test<- trip_data %>% 
  # dplyr::filter(tot_rel_cod.x!=tot_rel_cod.y)
  
  #combine all the catch draw files 
  dfs_all<- list.stack(dfs, fill=TRUE)
  
  dfs_all[is.na(dfs_all)] = 0
  dfs_all <- dfs_all[order(dfs_all$tripid),]
  rownames(dfs_all) = NULL
  
  dfs_all$period=p
  pds[[p]] = dfs_all
}

######################################
##   End simulating trip outcomes   ##
######################################

pds_all= list.stack(pds, fill=TRUE)

pds_all[is.na(pds_all)] = 0


rm(pds)



#param_draws$parameter_draw=d




#                          Linearized
#                 Mean     std. err.      [95% conf. interval]
#-------------+------------------------------------------------
#  boat_hrs |    6.74066   .1470764      6.450947    7.030374


# Now calculate trip probabilities and utilities based on the multiple catch draws for each choice occasion
costs_new = list()
pds_new = list()
levels(periodz)
params = list()

for(p in levels(periodz)){
  #p<- "pr_15_inshore"
  directed_trips_p <- subset(directed_trips, period2 == p)
  n_trips <- mean(directed_trips_p$dtrip)  
  mode_val <- directed_trips_p[1,]$mode1
  
  # Add trip costs. These are mean and sd estimates from over all modes from the expenditure survey
  pds<-subset(pds_all, period==p)
  
  trip_data<-pds
  # trip_data$cost<-rnorm(nrow(trip_data), mean=55.50573,sd= 3.966326)
  # trip_data[is.na(trip_data)] <- 0
  # trip_data$cost<-ifelse(trip_data$cost<0, 0,trip_data$cost )
  
  #Create random draws of preference parameters based on the estimated means and SD from the choice model
  param_draws = as.data.frame(1:n_drawz)
  names(param_draws)[1] <- 'tripid'
  
  param_draws<-param_draws %>% 
    dplyr::mutate(beta_sqrt_cod_keep = rnorm(n_drawz, mean = 1.594, sd = .615),
                  beta_sqrt_cod_release = rnorm(n_drawz, mean = 0.162 , sd = 0.445),
                  beta_sqrt_hadd_keep = rnorm(n_drawz, mean = 1.156, sd = 0.603 ),
                  beta_sqrt_hadd_release = rnorm(n_drawz, mean = 0.094 , sd = 0 ),
                  beta_sqrt_cod_hadd_keep = rnorm(n_drawz, mean =-0.314  , sd = 0.778 ),
                  beta_cost = rnorm(n_drawz, mean =-0.015 , sd =0 ),
                  beta_opt_out = rnorm(n_drawz, mean =-1.871 , sd = 3.208 ), 
                  beta_opt_out_age = rnorm(n_drawz, mean =0.047 , sd = 0 ), 
                  beta_opt_out_likely = rnorm(n_drawz, mean =-1.272 , sd = 0 ), 
                  beta_opt_out_prefer = rnorm(n_drawz, mean =-1.079 , sd = 0 ))
  
  
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
    dplyr::filter(state == "MA")  
  
  #next two commands ensure there are enough observations  per period
  expand_rows=ceiling((n_drawz/nrow(age_distn)))+2
  
  age_distn <- age_distn %>%
    dplyr::slice(rep(1:dplyr::n(), each = expand_rows))   
  
  #now need to assign tripid in order to merge to trip data
  age_distn <- age_distn %>%  dplyr::slice_sample(n = n_drawz) %>% 
    dplyr::mutate(period=p, 
                  tripid = 1:n_drawz) %>% 
    dplyr::select(-state)
  
  #Avidities
  avid_distn <- data.frame(read.csv(paste0(input_data_cd,"angler CE demographics.csv"))) 
  
  
  #next two commands ensure there are enough observations per period
  expand_rows=ceiling((n_drawz/nrow(avid_distn)))+2
  
  avid_distn <- avid_distn %>%
    dplyr::slice(rep(1:dplyr::n(), each = expand_rows))   
  
  #now need to assign tripid in order to merge to trip data
  avid_distn <- avid_distn %>%  dplyr::slice_sample(n = n_drawz) %>% 
    dplyr::mutate(period=p, 
                  tripid = 1:n_drawz) 
  
  avid_distn<- avid_distn %>%
    dplyr::left_join(age_distn, by = c("tripid", "period"))
  
  param_draws<-param_draws %>% 
    dplyr::left_join(avid_distn, by="tripid") 
  
  
  
  #now merge the utility parameters, ages and avidities to the trip data
  
  trip_data<- trip_data %>%
    dplyr::left_join(param_draws, by = c("tripid",  "period"))
  
  
  
  # Costs_new_state data sets will retain raw trip outcomes from the baseline scenario. 
  # We will merge these data to the prediction year outcomes to calculate changes in CS. 
  costs_new[[p]] = subset(trip_data, select=c(tripid, cost, catch_draw, tot_keep_cod, tot_rel_cod,
                                              tot_keep_hadd, tot_rel_hadd, beta_cost, beta_opt_out, beta_sqrt_cod_keep, 
                                              beta_sqrt_cod_release, beta_sqrt_cod_hadd_keep, 
                                              beta_sqrt_hadd_keep, beta_sqrt_hadd_release,beta_opt_out_age, 
                                              beta_opt_out_likely, beta_opt_out_prefer, 
                                              likely_to_fish, fish_pref_more))
  
  names(costs_new[[p]])[names(costs_new[[p]]) == "tot_keep_cod"] = "tot_keep_cod_base"
  names(costs_new[[p]])[names(costs_new[[p]]) == "tot_rel_cod"] = "tot_rel_cod_base"
  names(costs_new[[p]])[names(costs_new[[p]]) == "tot_keep_hadd"] = "tot_keep_hadd_base"
  names(costs_new[[p]])[names(costs_new[[p]]) == "tot_rel_hadd"] = "tot_rel_hadd_base"
  names(costs_new[[p]])[names(costs_new[[p]]) == "cost"] = "cost_base"
  
  costs_new[[p]]$period2 = p
  
  
  #Expected utility
  trip_data<-  trip_data%>% 
    dplyr::mutate(vA=
                    beta_sqrt_cod_keep*sqrt(trip_data$tot_keep_cod) +
                    beta_sqrt_cod_release*sqrt(trip_data$tot_rel_cod) +  
                    beta_sqrt_hadd_keep*sqrt(trip_data$tot_keep_hadd) +
                    beta_sqrt_hadd_release*sqrt(trip_data$tot_rel_hadd) + 
                    beta_sqrt_cod_hadd_keep*(sqrt(trip_data$tot_keep_cod)*sqrt(trip_data$tot_keep_hadd)) +
                    beta_cost*trip_data$cost) %>% 
    dplyr::select(-id)
  
  
  
  mean_trip_data <- trip_data %>%
    data.table::data.table()
  
  mean_trip_data <- mean_trip_data %>%
    dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
    tidyr::uncount(n_alt) %>%
    dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                  opt_out = ifelse(alt == 2, 1, 0))
  
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[, vA_optout := beta_opt_out*opt_out+
        beta_opt_out_age*age + 
        beta_opt_out_likely*likely_to_fish +
        beta_opt_out_prefer*fish_pref_more] %>%
    .[alt==1, expon_vA := exp(vA)] %>%
    .[alt==2, expon_vA := exp(vA_optout)]
  
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[, vA_col_sum := sum(expon_vA), by=list(period, catch_draw, tripid)]
  
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[, prob0 :=expon_vA/vA_col_sum]
  
  
  mean_trip_data<- subset(mean_trip_data, alt==1)
  
  all_vars<-c()
  all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period","tripid")]
  all_vars
  
  mean_trip_data<-mean_trip_data  %>% data.table::as.data.table() %>%
    .[,lapply(.SD, mean), by = c("period","tripid"), .SDcols = all_vars]
  
  
  # Get rid of things we don't need. 
  mean_trip_data <- mean_trip_data %>% 
    dplyr::select(prob0, period, tripid, tot_hadd_catch,tot_cod_catch, tot_keep_cod, tot_keep_hadd,tot_rel_cod, tot_rel_hadd)
  
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table()
  
  # Multiply the trip probability by each of the catch and cost variables  to get probability-weighted catch
  list_names = c("tot_hadd_catch","tot_cod_catch", "tot_keep_cod", "tot_keep_hadd","tot_rel_cod", "tot_rel_hadd")
  
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
    .[]
  
  
  
  # Multiply each choice occasion's trip outcomes (catch, cost, trip probabilities) in mean_trip_pool 
  # by the expansion factor (expand), so that each choice occasion represents a certain number of choice occasions
  
  mean_prob=mean(mean_trip_data$prob)
  observed_trips=n_trips
  sims = round(observed_trips/mean_prob)
  ndraws = nrow(mean_trip_data)
  expand=sims/ndraws
  mean_trip_data$n_choice_occasions=1
  
  list_names = c("tot_cod_catch","tot_keep_cod","tot_rel_cod", 
                 "tot_hadd_catch","tot_keep_hadd","tot_rel_hadd",
                 "prob0","n_choice_occasions" )
  
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * expand), .SDcols = list_names] %>%
    .[]
  
  #mean_trip_data$sim=1
  
  #sum probability weighted catch over all choice occasions

  
  aggregate_trip_data <- mean_trip_data %>%
    dplyr::select(-tripid) %>%
    data.table::as.data.table() %>%
    .[,lapply(.SD, sum), by =c("period"), .SDcols = list_names]
  
  
  names(aggregate_trip_data)[names(aggregate_trip_data) == "prob0"] = "estimated_trips"
  
  aggregate_trip_data$sim =1
  
  
  pds_new[[p]]=aggregate_trip_data
  
}


pds_new_all=list.stack(pds_new, fill=TRUE)

pds_new_all[is.na(pds_new_all)] = 0
rm(pds_new)


# pds_new_all <- pds_new_all %>%
#   tidyr::separate(period, into = c("mode", "period", "area"), sep = "_")
# pds_new_all<-pds_new_all %>% 
#   dplyr::mutate(month=0) %>% 
#   dplyr::mutate(month=case_when(period ==1~1,TRUE~month), 
#                 month=case_when(period ==2~1,TRUE~month), 
#                 month=case_when(period ==3~2,TRUE~month), 
#                 month=case_when(period ==4~2,TRUE~month), 
#                 month=case_when(period ==5~3,TRUE~month), 
#                 month=case_when(period ==6~3,TRUE~month), 
#                 month=case_when(period ==7~4,TRUE~month), 
#                 month=case_when(period ==8~4,TRUE~month), 
#                 month=case_when(period ==9~5,TRUE~month), 
#                 month=case_when(period ==10~5,TRUE~month), 
#                 month=case_when(period ==11~6,TRUE~month), 
#                 month=case_when(period ==12~6,TRUE~month), 
#                 month=case_when(period ==13~7,TRUE~month), 
#                 month=case_when(period ==14~7,TRUE~month), 
#                 month=case_when(period ==15~8,TRUE~month), 
#                 month=case_when(period ==16~8,TRUE~month), 
#                 month=case_when(period ==17~9,TRUE~month), 
#                 month=case_when(period ==18~9,TRUE~month), 
#                 month=case_when(period ==19~10,TRUE~month), 
#                 month=case_when(period ==20~10,TRUE~month), 
#                 month=case_when(period ==21~11,TRUE~month), 
#                 month=case_when(period ==22~11,TRUE~month), 
#                 month=case_when(period ==23~12,TRUE~month), 
#                 month=case_when(period ==24~12,TRUE~month))
# 
# pds_new_all<-pds_new_all %>% 
#   dplyr::group_by(month,area, state, mode, year) %>% 
#   dplyr::summarise(mean_hadd=mean(tot_cat_hadd), 
#                    mean_cod=mean(tot_cat_cod))
# costs_new_all contain trip outcomes for the baseline period. Will use to calculate welfare changes, 
# and assign catch-per-trip in the prediction years. 

costs_new_all=list.stack(costs_new, fill=TRUE)
costs_new_all[is.na(costs_new_all)] = 0
rm(costs_new)


###Compare calibration model output with MRIP 

MRIP_data <- data.frame( read.csv(paste0(input_data_cd,"total AB1B2 2010_2020 GoM.csv")))
MRIP_data<-MRIP_data %>% 
  dplyr::filter(year==y)
# 
# 
# 
##cod
print("model cod harvest")
sum(pds_new_all$tot_keep_cod)
print("MRIP cod harvest")
sum(MRIP_data$cod_harvest)
print("percent diff cod harvest")
cod_harvest_diff<-((sum(MRIP_data$cod_harvest)-sum(pds_new_all$tot_keep_cod))/sum(MRIP_data$cod_harvest))*100
cod_harvest_diff
abs_cod_harvest_diff=abs(sum(MRIP_data$cod_harvest)-sum(pds_new_all$tot_keep_cod))

sum(pds_new_all$tot_rel_cod)
sum(MRIP_data$cod_releases)
cod_rel_diff<-((sum(MRIP_data$cod_releases)-sum(pds_new_all$tot_rel_cod))/sum(MRIP_data$cod_releases))*100
cod_rel_diff

sum(pds_new_all$tot_cod_catch)
sum(MRIP_data$cod_tot_cat)
cod_tot_cat_diff<-((sum(MRIP_data$cod_tot_cat)-sum(pds_new_all$tot_cod_catch))/sum(MRIP_data$cod_tot_cat))*100
cod_tot_cat_diff




##haddock
print("model hadd harvest")
sum(pds_new_all$tot_keep_hadd)
print("mrip hadd harvest")
sum(MRIP_data$hadd_harvest)
print("percent diff hadd harvest")
hadd_harvest_diff<-((sum(MRIP_data$hadd_harvest)-sum(pds_new_all$tot_keep_hadd))/sum(MRIP_data$hadd_harvest))*100
hadd_harvest_diff
abs_hadd_harvest_diff=abs(sum(MRIP_data$hadd_harvest)-sum(pds_new_all$tot_keep_hadd))

print("model hadd release")
sum(pds_new_all$tot_rel_hadd)
print("mrip hadd release")
sum(MRIP_data$hadd_releases)
print("percent diff hadd release")
hadd_rel_diff<- ((sum(MRIP_data$hadd_releases)-sum(pds_new_all$tot_rel_hadd))/sum(MRIP_data$hadd_releases))*100
hadd_rel_diff


sum(pds_new_all$tot_hadd_catch)
sum(MRIP_data$hadd_tot_cat)
hadd_tot_cat_diff<-((sum(MRIP_data$hadd_tot_cat)-sum(pds_new_all$tot_hadd_catch))/sum(MRIP_data$hadd_tot_cat))*100
hadd_tot_cat_diff

Sys.time()-time_a
