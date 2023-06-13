


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
  #p<- "13_pr"
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
    # Input catch-per-trip numbers 
    # Two ways to do this, I've included code for both.
    # 1) import 2020 MRIP trip-level catch of the three species - this retains any correlation in catch between the species  
    # 2) estimate the NB parameters using the raw data then simulate the distribution - this assumes independence in catch between the species
    
    
    #method 1)
    
    # nbs_params<- subset(read_csv("nb_params_baseline.csv",  show_col_types = FALSE))
    # nbs_params$month <-as.numeric(nbs_params$month)
    # nbs_params$month <- sprintf("%02d", nbs_params$month)
    # nbs_params
    # 
    # nbs_params<-subset(nbs_params, state==state1)
    # nbs_params<-subset(nbs_params, mode==mode1)
    # nbs_params<-subset(nbs_params, month==month1)
    # 
    # 
    # sf_mu_param <- nbs_params$sf_mu
    # sf_size_param <- nbs_params$sf_size
    # bsb_mu_param <- nbs_params$bsb_mu
    # bsb_size_param <- nbs_params$bsb_size
    # scup_mu_param <- nbs_params$scup_mu
    # scup_size_param <- nbs_params$scup_size
    # ###
    # 
    # if (sf_mu_param!=0){
    #   tot_sf_catch <- rnbinom(1:10000, mu = sf_mu_param, size = sf_size_param)
    # }
    # 
    # if (sf_mu_param==0){
    #   tot_sf_catch <- data.frame(1:10000)
    #   tot_sf_catch$tot_sf_catch<-0
    #   tot_sf_catch<-subset(tot_sf_catch, select=c(tot_sf_catch))
    # }
    # 
    # 
    # if (bsb_mu_param!=0){
    #   tot_bsb_catch <- rnbinom(1:10000, mu = bsb_mu_param, size = bsb_size_param)
    # }
    # 
    # if (bsb_mu_param==0){
    #   tot_bsb_catch <- data.frame(1:10000)
    #   tot_bsb_catch$tot_bsb_catch<-0
    #   tot_bsb_catch<-subset(tot_bsb_catch, select=c(tot_bsb_catch))
    # }
    # 
    # if (scup_mu_param!=0){
    #   tot_scup_catch <- rnbinom(1:10000, mu = scup_mu_param, size = scup_size_param)
    # }
    # 
    # if (scup_mu_param==0){
    #   tot_scup_catch <- data.frame(1:10000)
    #   tot_scup_catch$tot_scup_catch<-0
    #   tot_scup_catch<-subset(tot_scup_catch, select=c(tot_scup_catch))
    # }
    
    # sf_catch_data <- data.frame(tot_sf_catch,tot_bsb_catch,tot_scup_catch)
    #end method 1)
    
    #######
    #method 2)
    #GOM_catch_data = data.frame(read.csv(paste0('simulated_catch_',month1, '.csv')))
    GOM_catch_data = subset(catch_data_all, period2==p)
    
    #sf_catch_data = subset(sf_catch_data, decade==d)

    #sf_catch_data = data.frame(read.csv(paste0('observed_catch_2020_',state_no,'_', month1, '_', mode1, '.csv')))
    
    cod_tot_cat = GOM_catch_data$cod_tot_cat
    hadd_tot_cat = GOM_catch_data$hadd_tot_cat
    GOM_catch_data = data.frame(cod_tot_cat,hadd_tot_cat)
    
    #end method 2)
    #######
    
    # random draw of fluke and bsb catch
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
    
    
    #draw sizes for black sea bass catch
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
      trip_data =  merge(trip_data,hadd_catch_data1,by="tripid")
      trip_data[is.na(trip_data)] = 0
      
      trip_data$tot_hadd_catch = trip_data$tot_keep_hadd+trip_data$tot_rel_hadd
      trip_data[is.na(trip_data)] = 0
      
    }
    
    if (catch_check_hadd==0){ 
      trip_data_hadd<-hadd_catch_data
      trip_data_hadd$tot_keep_hadd<-0
      trip_data_hadd$tot_rel_hadd<-0
      trip_data <-  merge(trip_data,trip_data_hadd,by="tripid")
      
    }
    
    
    
   trip_data[is.na(trip_data)] = 0
    
    
    trip_data$catch_draw=i
    dfs[[i]]=trip_data
    
  }
  
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

#pds_all<-subset(pds_all, select=-c(tot_bsb_catch.x,tot_bsb_catch.y, tot_scup_catch.x, tot_scup_catch.y))

#Create random draws of preference parameters based on the estimated means and SD from the choice model
param_draws_MA = as.data.frame(1:n_drawz)
names(param_draws_MA)[1] <- 'tripid'
param_draws_MA$beta_sqrt_cod_keep = rnorm(n_drawz, mean = 1.827, sd = 0)
param_draws_MA$beta_sqrt_cod_release = rnorm(n_drawz, mean = 0.173 , sd = 0.457 )
param_draws_MA$beta_sqrt_hadd_keep = rnorm(n_drawz, mean = 1.310, sd = 0.607 )
param_draws_MA$beta_sqrt_hadd_release = rnorm(n_drawz, mean = 0 , sd = 0 )
param_draws_MA$beta_sqrt_cod_hadd_keep = rnorm(n_drawz, mean =-0.403  , sd = 0.940 )
param_draws_MA$beta_opt_out = rnorm(n_drawz, mean =0 , sd = 2.1298286 )
param_draws_MA$beta_trip_length = rnorm(n_drawz, mean =0 , sd = .097 )
param_draws_MA$beta_cost = rnorm(n_drawz, mean =-0.015 , sd =0 )
#param_draws_MA$parameter_draw=d




#                          Linearized
#                 Mean     std. err.      [95% conf. interval]
#-------------+------------------------------------------------
#  boat_hrs |    6.74066   .1470764      6.450947    7.030374


# Now calculate trip probabilities and utilities based on the multiple catch draws for each choice occasion
costs_new_MA = list()
pds_new = list()
levels(periodz)

for(p in levels(periodz)){
  #p<-'13_pr'
  directed_trips_p <- subset(directed_trips, period2 == p)
  n_trips <- mean(directed_trips_p$dtrip)  
  mode_val <- directed_trips_p[1,]$mode1
  
  # Add trip costs. These are mean and sd estimates from over all modes from the expenditure survey
  pds<-subset(pds_all, period==p)
  
  trip_data<-pds
  trip_data$cost<-rnorm(nrow(trip_data), mean=53.21681,sd= 4.211999)
  trip_data[is.na(trip_data)] <- 0
  trip_data$cost<-ifelse(trip_data$cost<0, 0,trip_data$cost )

  trip_data$trip_length<-rnorm(nrow(trip_data), mean=6.74066,sd= .1470764)
  
  trip_data =  merge(param_draws_MA,trip_data,by="tripid")
  
  # Costs_new_state data sets will retain raw trip outcomes from the baseline scenario. 
  # We will merge these data to the prediction year outcomes to calculate changes in CS. 
  costs_new_MA[[p]] = subset(trip_data, select=c(tripid, trip_length, cost, catch_draw, tot_keep_cod, tot_rel_cod,
                                                 tot_keep_hadd, tot_rel_hadd, beta_cost, beta_opt_out, beta_sqrt_cod_keep, 
                                                 beta_sqrt_cod_release, beta_sqrt_cod_hadd_keep, 
                                                 beta_sqrt_hadd_keep, beta_sqrt_hadd_release, beta_trip_length))
  
  names(costs_new_MA[[p]])[names(costs_new_MA[[p]]) == "tot_keep_cod"] = "tot_keep_cod_base"
  names(costs_new_MA[[p]])[names(costs_new_MA[[p]]) == "tot_rel_cod"] = "tot_rel_cod_base"
  names(costs_new_MA[[p]])[names(costs_new_MA[[p]]) == "tot_keep_hadd"] = "tot_keep_hadd_base"
  names(costs_new_MA[[p]])[names(costs_new_MA[[p]]) == "tot_rel_hadd"] = "tot_rel_hadd_base"
  names(costs_new_MA[[p]])[names(costs_new_MA[[p]]) == "trip_length"] = "trip_length_base"
  names(costs_new_MA[[p]])[names(costs_new_MA[[p]]) == "cost"] = "cost_base"
  
  costs_new_MA[[p]]$period2 = p
  
  
  #Expected utility
  trip_data$vA = 
    trip_data$beta_sqrt_cod_keep*sqrt(trip_data$tot_keep_cod) +
    trip_data$beta_sqrt_cod_release*sqrt(trip_data$tot_rel_cod) +  
    trip_data$beta_sqrt_hadd_keep*sqrt(trip_data$tot_keep_hadd) +
    trip_data$beta_sqrt_hadd_release*sqrt(trip_data$tot_rel_hadd) + 
    trip_data$beta_sqrt_cod_hadd_keep*(sqrt(trip_data$tot_keep_cod)*sqrt(trip_data$tot_keep_hadd)) +
    trip_data$beta_trip_length*sqrt(trip_data$trip_length) +
    trip_data$beta_cost*trip_data$cost 
  
  
  
  mean_trip_data <- trip_data %>% 
    mutate(n_alt = rep(2,nrow(.))) %>% 
    uncount(n_alt) %>% 
    mutate(alt = rep(1:2,nrow(.)/2),
           opt_out = ifelse(alt == 2, 1, 0))
  
  mean_trip_data$v0_optout= mean_trip_data$beta_opt_out*mean_trip_data$opt_out 
  
  
  #Now put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)
  
  mean_trip_data$expon_v0 <- case_when(mean_trip_data$alt==1 ~ exp(mean_trip_data$vA), 
                                       mean_trip_data$alt==2 ~ exp(mean_trip_data$v0_optout)) 
  
  mean_trip_data <- mean_trip_data %>% 
    group_by(period, tripid, catch_draw) %>% 
    mutate( v0_col_sum = sum(expon_v0)) %>% 
    ungroup()
  
  mean_trip_data <- mean_trip_data %>% 
    mutate(prob = expon_v0/v0_col_sum) 
  
  mean_trip_data<- subset(mean_trip_data, alt==1)
  
  mean_trip_data<-mean_trip_data %>% group_by(period,tripid) %>% summarise(across(everything(), mean), .groups = 'drop') %>% 
    tibble()
  
  
  
  # Get rid of things we don't need. 
  mean_trip_data = subset(mean_trip_data,  select=-c(alt, opt_out, v0_optout,  catch_draw, vA, v0_optout,beta_cost, beta_opt_out, beta_trip_length,
                                                     beta_sqrt_cod_release, beta_sqrt_cod_keep, beta_sqrt_hadd_release, beta_sqrt_hadd_keep, beta_sqrt_cod_hadd_keep,
                                                     period ))
  
  
  # Multiply the trip probability by each of the catch and cost variables  to get probability-weighted catch
  list_names = c("tot_cod_catch","tot_keep_cod","tot_rel_cod", 
                 "tot_hadd_catch","tot_keep_hadd","tot_rel_hadd",
                 "cost")
  
  for (l in list_names){
    mean_trip_data[,l] = mean_trip_data[,l]*mean_trip_data$prob
  }
  
  
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
                 "cost", "prob","n_choice_occasions" )
  
  for (l in list_names){
    mean_trip_data[,l] = mean_trip_data[,l]*expand
  }
  
  
  
  mean_trip_data$sim=1
  
  #sum probability weighted catch over all choice occasions
  aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)
  
  
  aggregate_trip_data = subset(aggregate_trip_data, select=-c(Group.1, tripid, expon_v0, v0_col_sum,sim))
  names(aggregate_trip_data)[names(aggregate_trip_data) == "prob"] = "estimated_trips"
  
  aggregate_trip_data$period<-p
  
  
  aggregate_trip_data$sim =1
  
  
  aggregate_trip_data$period=p
  pds_new[[p]]=aggregate_trip_data
  
}


pds_new_all_MA=list.stack(pds_new, fill=TRUE)

pds_new_all_MA[is.na(pds_new_all_MA)] = 0
rm(pds_new)

# costs_new_all contain trip outcomes for the baseline period. Will use to calculate welfare changes, 
# and assign catch-per-trip in the prediction years. 

costs_new_all_MA=list.stack(costs_new_MA, fill=TRUE)
costs_new_all_MA[is.na(costs_new_all_MA)] = 0
rm(costs_new_MA)


###Compare calibration model output with MRIP 

# MRIP_data <- data.frame( read.csv("total AB1B2 2020 GoM.csv"))
# 
# 
# 
# ##cod
# sum(pds_new_all_MA$tot_keep_cod)
# sum(MRIP_data$cod_harvest)
# cod_harvest_diff<-((sum(MRIP_data$cod_harvest)-sum(pds_new_all_MA$tot_keep_cod))/sum(MRIP_data$cod_harvest))*100
# cod_harvest_diff
# 
# sum(pds_new_all_MA$tot_rel_cod)
# sum(MRIP_data$cod_releases)
# cod_rel_diff<-((sum(MRIP_data$cod_releases)-sum(pds_new_all_MA$tot_rel_cod))/sum(MRIP_data$cod_releases))*100
# cod_rel_diff
# 
# sum(pds_new_all_MA$tot_cod_catch)
# sum(MRIP_data$cod_tot_cat)
# cod_tot_cat_diff<-((sum(MRIP_data$cod_tot_cat)-sum(pds_new_all_MA$tot_cod_catch))/sum(MRIP_data$cod_tot_cat))*100
# cod_tot_cat_diff
# 
# 
# 
# 
# ##haddock
# sum(pds_new_all_MA$tot_keep_hadd)
# sum(MRIP_data$hadd_harvest)
# hadd_harvest_diff<-((sum(MRIP_data$hadd_harvest)-sum(pds_new_all_MA$tot_keep_hadd))/sum(MRIP_data$hadd_harvest))*100
# hadd_harvest_diff
# 
# sum(pds_new_all_MA$tot_rel_hadd)
# sum(MRIP_data$hadd_releases)
# hadd_rel_diff<- ((sum(MRIP_data$hadd_releases)-sum(pds_new_all_MA$tot_rel_hadd))/sum(MRIP_data$hadd_releases))*100
# hadd_rel_diff
# 
# sum(pds_new_all_MA$tot_hadd_catch)
# sum(MRIP_data$hadd_tot_cat)
# hadd_tot_cat_diff<-((sum(MRIP_data$hadd_tot_cat)-sum(pds_new_all_MA$tot_hadd_catch))/sum(MRIP_data$hadd_tot_cat))*100
# hadd_tot_cat_diff

