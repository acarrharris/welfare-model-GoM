


#profvis::profvis({

#catch_data_all <-readr::read_csv(file.path(here::here("projection catch per trip 5_6.csv")),  show_col_types = FALSE) 
catch_data_all <-readr::read_csv(file.path(here::here("historical projected catch per trip 6_1.csv")),  show_col_types = FALSE) 

predictions_d<-list()

for(d in 2010:2019){
  
  #d<-2010
  y<-d
  y_plus_1<-d+1
  
  ##Want catch_draws 
  catch_data_all1<-catch_data_all %>% 
    dplyr::filter(decade==y_plus_1)
  
  p_starz <- readRDS("p_stars_all_years.rds")
  p_starz<- p_starz %>% 
    dplyr::filter(year==d)
  
  p_star_cod_variable<- p_starz$p_star_cod_variable
  p_star_hadd_variable<- p_starz$p_star_hadd_variable
  
  
  directed_trips<-data.frame(read_csv("directed trips and regulations 2010_2020.csv", show_col_types = FALSE))
  directed_trips<-directed_trips %>% 
    dplyr::mutate(dtrip=round(dtrip)) %>% 
    dplyr::filter(year==d) %>% 
    dplyr::filter(dtrip!=0) %>% 
    dplyr::select(-year)
  

  p_star_cod <- p_star_cod_variable
  p_star_hadd<-p_star_hadd_variable
  
  
  predictions<-list()
  for(x in 1:100){
    #x<-1
    
    set.seed(130+d+x)
    # Input the calibration output which contains the number of choice occasions needed to simulate
    #calibration_data <- as.data.frame(calibration_data_table_base[[x]]) %>% tibble() 
    
    calibration_data <- readRDS(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/calibration_data/calibration_data_all_",y,".rds")) %>% 
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
                    cod_bag_t_plus1, cod_min_t_plus1,
                    hadd_bag_t_plus1, hadd_min_t_plus1, dtrip)
    
    
    # regs<-data.frame(read_csv("directed trips and regulations 2010_2020.csv", show_col_types = FALSE))
    # regs<-regs %>% 
    #   dplyr::filter(year==y_plus_1) %>% 
    #   dplyr::select(period2,
    #                 cod_bag, cod_min, 
    #                 hadd_bag, hadd_min)
    

    regs_check <- directed_trips_p %>%
      dplyr::select(period2, dtrip)
    
    # regs_check <- regs %>% 
    #    dplyr::select(period2)
    
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
          cod_bag_t_plus1 > 0 ~ ifelse(csum_keep<=cod_bag_t_plus1 & posskeep==1,1,0),TRUE ~ 0)) %>% 
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
          hadd_bag_t_plus1 > 0 ~ ifelse(csum_keep<=hadd_bag_t_plus1 & posskeep==1,1,0),
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
    
    
    costs_new_all <- readRDS(paste0("calibration_data/cost_files_all_draw_", y, "_",x,".rds"))  %>% #tibble() %>% 
      filter(catch_draw<=n_catch_draws)
    
    costs_new_all<-data.table(costs_new_all, key = c("period2", "catch_draw", "tripid"))
    
    
    
    trip_data <- data.table(trip_data, key = c("period2", "catch_draw", "tripid"))
    trip_data<-data.frame(trip_data[costs_new_all])
    
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
    
    
    #These stats should be roughly the same under no change in fishery conditions
    
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
    
    mean_trip_data<- mean_trip_data %>% 
      dplyr::mutate(tot_cod_cat=tot_keep_cod+tot_rel_cod, 
                    tot_cod_cat_base=tot_keep_cod_base+tot_rel_cod_base, 
                    tot_hadd_cat=tot_keep_hadd+tot_rel_hadd, 
                    tot_hadd_cat_base=tot_keep_hadd_base+tot_rel_hadd_base)
    
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
                       v0_optout, vA, vA_col_sum, vA_optout)) 
    
    
    # Multiply the average trip probability in the Alternative scenario (probA) 
    #by each of the catch variables ( the variables below) to get probability-weighted catch
    
    list_names <- c("tot_keep_cod","tot_keep_hadd", "tot_rel_cod", "tot_rel_hadd", "tot_cod_cat", "tot_hadd_cat")
    
    mean_trip_data <- mean_trip_data %>%
      as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
      .[]
    
    # Multiply the average trip probability in the base scenario (prob0) 
    #by each of the catch variables to get probability-weighted catch
    
    # list_names <- c("tot_keep_cod_base","tot_keep_hadd_base", "tot_rel_cod_base", "tot_rel_hadd_base", 
    #                 "tot_cod_cat_base", "tot_hadd_cat_base")
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
      .[, cod_keep_sum := expand*tot_keep_cod] %>%
      .[, hadd_keep_sum := expand*tot_keep_hadd] %>%
      .[, cod_rel_sum := expand*tot_rel_cod] %>%
      .[, hadd_rel_sum := expand*tot_rel_hadd] %>%
      .[, cod_catch_sum := expand*tot_cod_cat] %>%
      .[, hadd_catch_sum := expand*tot_hadd_cat] %>%
      .[, ntrips_alt := expand*probA] 
    
    trip_level_output <- trip_level_output %>%
      mutate_if(is.numeric, replace_na, replace = 0)    
    
    
    #based on MRIP average weights across all modes. The number_weight var is set to "Weight"   
    trip_level_output1<- trip_level_output %>% 
      dplyr::group_by(month, decade) %>% 
      dplyr::summarise(cv_sum = sum(cv_sum), 
                       cod_keep_sum = sum(cod_keep_sum),
                       hadd_keep_sum = sum(hadd_keep_sum),
                       cod_rel_sum = sum(cod_rel_sum),
                       hadd_rel_sum = sum(hadd_rel_sum),
                       cod_catch_sum = sum(cod_catch_sum),
                       hadd_catch_sum = sum(hadd_catch_sum),
                       ntrips_alt_sum = sum(ntrips_alt),
                       n_choice_occasions_sum = sum(expand),
                       .groups = "drop") %>% 
      dplyr::ungroup() 
    
    
    predictions0<-trip_level_output1 %>% 
      dplyr::mutate(decade=d, corr_type=t, copula=c, draw=x) 
    
    
    
    #Now assess the correlation in catch versus the correlation in keep (boat mode only)
    #To do so, draw 5,000 catch draws in proportion to the the number of trips across the period 
    #Then compute kendall's tau for catch and for keep and save in the output list. 
    
    ###Annual correlations 
    keep_rel_pairs<- trip_data %>% 
      dplyr::mutate(period=period2, 
                    tot_cod_catch=tot_keep_cod+tot_rel_cod, 
                    tot_hadd_catch=tot_keep_hadd+tot_rel_hadd)
    
    if(y==2010){
      
      keep_rel_pairs<- stratified(keep_rel_pairs, "period", 
                                  size=c( "7_fh"=24,"7_pr"=61,"8_fh"=64,"8_pr"=128,"9_fh"=97,"9_pr"=525,"10_fh"=65,
                                          "10_pr"=610,"11_fh"=78,"11_pr"=505,"12_fh"=48, "12_pr"=284,"13_fh"=91,
                                          "13_pr"=411,"14_fh"=51,"14_pr"=248,"15_fh"=26,"15_pr"=131,"16_fh"=114,
                                          "16_pr"=254,"17_fh"=32, "17_pr"=36,"18_fh"=45,"18_pr"=549,"19_fh"=39,"19_pr"=28,
                                          "20_fh"=27,"20_pr"=345,"21_fh"=1,"21_pr"=30,"22_fh"=1,"22_pr"=53,"23_fh"=1))
      
      
    }
    
    if(y==2011){
      keep_rel_pairs<- stratified(keep_rel_pairs, "period", 
                                  size=c( "7_fh"=11,"7_pr"=688,"8_fh"=64,"9_fh"=133,"9_pr"=99,"10_fh"=111,"10_pr"=319,
                                          "11_fh"=140,"11_pr"=370,"12_fh"=200,"12_pr"=557,"13_fh"=61,"13_pr"=230,"14_fh"=61,
                                          "14_pr"=647,"15_fh"=53,"15_pr"=125,"16_fh"=83,"16_pr"=109,"17_fh"=14, "17_pr"=213,
                                          "18_fh"=56,"18_pr"=271,"19_fh"=18,"19_pr"=22,"20_fh"=34,"20_pr"=38,"21_fh"=1,
                                          "21_pr"=255,"22_fh"=1,"22_pr"=16))
    }                                   
    
    if(y==2012){
      keep_rel_pairs<- stratified(keep_rel_pairs, "period", 
                                  size=c( "6_pr"=103,"7_fh"=14,"7_pr"=146,"8_fh"=68,"8_pr"=232,"9_fh"=29,"9_pr"=284,
                                          "10_fh"=360,"10_pr"=183,"11_fh"=111,"11_pr"=167,"12_fh"=124,"12_pr"=42,
                                          "13_fh"=145,"13_pr"=578,"14_fh"=102,"14_pr"=312,"15_fh"=124,"15_pr"=187,
                                          "16_fh"=165,"16_pr"=420,"17_fh"=44,"17_pr"=678,"18_fh"=43,"18_pr"=45,
                                          "19_fh"=80,"19_pr"=50,"20_fh"=15,"21_fh"=1,"21_pr"=68,"22_fh"=2,"22_pr"=79,
                                          "23_fh"=1))
      
    }                                   
    
    if(y==2013){
      keep_rel_pairs<- stratified(keep_rel_pairs, "period", 
                                  size=c( "7_fh"=10,"8_fh"=53,"8_pr"=841,"9_fh"=35,"9_pr"=8,"10_fh"=182,"10_pr"=515,
                                          "11_fh"=84,"11_pr"=176,"12_fh"=60,"12_pr"=161,"13_fh"=200,"13_pr"=296,
                                          "14_fh"=129,"14_pr"=260,"15_fh"=73,"15_pr"=113,"16_fh"=76,"16_pr"=613,
                                          "17_fh"=27,"17_pr"=376,"18_fh"=96,"18_pr"=214,"19_fh"=28,"19_pr"=320,
                                          "20_fh"=33,"20_pr"=22))
      
    }  
    
    if(y==2014){
      keep_rel_pairs<- stratified(keep_rel_pairs, "period", 
                                  size=c( "7_fh"=39,"8_fh"=48,"9_fh"=97,"9_pr"=147,"10_fh"=318,"10_pr"=567,"11_fh"=166,"11_pr"=256,"12_fh"=152,
                                          "12_pr"=357,"13_fh"=324,"13_pr"=436,"14_fh"=158,"14_pr"=539,"15_fh"=178,"15_pr"=329,"16_fh"=235,
                                          "16_pr"=391,"17_fh"=8,"17_pr"=22,"18_fh"=59,"18_pr"=13,"19_fh"=11,"19_pr"=120,"20_fh"=24,"20_pr"=7))
    }  
    
    if(y==2015){
      keep_rel_pairs<- stratified(keep_rel_pairs, "period", 
                                  size=c( "8_fh"=12,"9_fh"=113,"9_pr"=49,"10_fh"=70,"10_pr"=9,"11_fh"=152,
                                          "11_pr"=383,"12_fh"=174,"12_pr"=137,"13_fh"=53,"13_pr"=466,
                                          "14_fh"=112,"14_pr"=252,"15_fh"=385,"15_pr"=260,"16_fh"=131,
                                          "16_pr"=1199,"17_fh"=58,"17_pr"=156,"18_fh"=49,"18_pr"=648,
                                          "19_fh"=11,"20_fh"=120))
      
    }  
    
    if(y==2016){
      keep_rel_pairs<- stratified(keep_rel_pairs, "period", 
                                  size=c( "7_fh"=3,"8_fh"=2,"9_fh"=21,"9_pr"=114,"10_fh"=100,"10_pr"=789,
                                          "11_fh"=67,"11_pr"=266,"12_fh"=55,"12_pr"=364,"13_fh"=85,
                                          "13_pr"=330,"14_fh"=98,"14_pr"=273,"15_fh"=31,"15_pr"=206,
                                          "16_fh"=170,"16_pr"=394,"17_fh"=6,"17_pr"=570,"18_fh"=90,
                                          "18_pr"=22,"19_fh"=10,"19_pr"=299,"20_fh"=5,"20_pr"=21,"22_pr"=609))
      
    }  
    
    if(y==2017){
      keep_rel_pairs<- stratified(keep_rel_pairs, "period", 
                                  size=c( "7_fh"=15,"7_pr"=611,"8_fh"=17,"9_fh"=16,"9_pr"=20,"10_fh"=117,
                                          "10_pr"=342,"11_fh"=68,"11_pr"=684,"12_fh"=45,"12_pr"=280,
                                          "13_fh"=92,"13_pr"=337,"14_fh"=80,"14_pr"=578,"15_fh"=72,
                                          "15_pr"=197,"16_fh"=200,"16_pr"=486,"17_fh"=14,"17_pr"=393,
                                          "18_fh"=37,"18_pr"=83,"19_fh"=4,"19_pr"=81,"20_fh"=8,"20_pr"=54,
                                          "21_fh"=1,"21_pr"=70,"22_fh"=1))
      
    }  
    
    if(y==2018){
      keep_rel_pairs<- stratified(keep_rel_pairs, "period", 
                                  size=c( "7_fh"=1,"8_fh"=20,"9_fh"=34,"9_pr"=473,"10_fh"=112,"10_pr"=44,
                                          "11_fh"=162,"11_pr"=281,"12_fh"=54,"12_pr"=986,"13_fh"=74,
                                          "13_pr"=572,"14_fh"=236,"14_pr"=366,"15_fh"=208,"15_pr"=251,
                                          "16_fh"=127,"16_pr"=411,"17_fh"=21,"17_pr"=405,"18_fh"=38,
                                          "18_pr"=70,"19_fh"=36,"19_pr"=19,"20_fh"=1))
      
    }  
    
    if(y==2019){
      keep_rel_pairs<- stratified(keep_rel_pairs, "period", 
                                  size=c( "7_fh"=1,"7_pr"=1057,"8_fh"=22,"9_fh"=46,"9_pr"=264,"10_fh"=134,
                                          "10_pr"=152,"11_fh"=88,"11_pr"=127,"12_fh"=59,"12_pr"=44,"13_fh"=124,
                                          "13_pr"=444,"14_fh"=155,"14_pr"=400,"15_fh"=58,"15_pr"=191,"16_fh"=84,
                                          "16_pr"=399,"17_fh"=54,"17_pr"=419,"18_fh"=51,"18_pr"=383,"19_fh"=5,
                                          "19_pr"=127,"20_fh"=3,"20_pr"=110))
      
    }  
    
    if(y==2020){
      keep_rel_pairs<- stratified(keep_rel_pairs, "period", 
                                  size=c( "7_pr"=397,"9_fh"=18,"9_pr"=245,"10_fh"=88,"10_pr"=230,"11_fh"=86,
                                          "11_pr"=403,"12_fh"=28,"12_pr"=353,"13_fh"=59, "13_pr"=266,
                                          "14_fh"=217,"14_pr"=594,"15_fh"=84,"15_pr"=256,"16_fh"=106,
                                          "16_pr"=214,"17_fh"=87,"17_pr"=221,"18_fh"=87,"18_pr"=382,
                                          "19_fh"=5,"19_pr"=57,"20_fh"=7,"21_pr"=512))
      
    }  
    
    
    sum_keep_cod<-sum(keep_rel_pairs$tot_keep_cod)
    sum_keep_hadd<-sum(keep_rel_pairs$tot_keep_hadd)
    
    if(sum_keep_cod>0 & sum_keep_hadd>0){
    ktau_keep<- cor.test(keep_rel_pairs$tot_keep_cod, 
                         keep_rel_pairs$tot_keep_hadd, method = c("kendall"))
    
    k_tau_keep_est<-ktau_keep[["estimate"]]
    k_tau_keep_p<- ktau_keep[["p.value"]]
    }
    
    if(sum_keep_cod==0 | sum_keep_hadd==0){

      k_tau_keep_est<-0
      k_tau_keep_p<- 1
    }
    
    
    ktau_catch<- cor.test(keep_rel_pairs$tot_cod_catch, 
                          keep_rel_pairs$tot_hadd_catch, method = c("kendall"))
    
    k_tau_catch_est<-ktau_catch[["estimate"]]
    k_tau_catch_p<- ktau_catch[["p.value"]]
    

    keep_rel_pairs<- as.data.frame(cbind(k_tau_keep_est,k_tau_keep_p, k_tau_catch_est, k_tau_catch_p), names="TRUE")
    keep_rel_pairs<-keep_rel_pairs %>% 
       dplyr::mutate(decade=d, corr_type=t, copula=c, draw=x)
    
   
    
    # #Monthly correlations ktau 
    period_to_month<-period_vec %>% 
      dplyr::distinct(period2, month, .keep_all = TRUE) #%>% 
    
    trip_level_output3<- trip_level_output %>% 
      dplyr::group_by(period2, decade) %>% 
      dplyr::summarise( ntrips_alt_sum = sum(ntrips_alt), .groups="drop") %>%
      dplyr::left_join(period_to_month, by="period2")  
    
    # dplyr::ungroup() %>% 
    # dplyr:: mutate(trip_sum = sum(ntrips_alt_sum), 
    #                trip_props = ntrips_alt_sum/trip_sum, 
    #                n_sample=round(5000*trip_props)) 
    
    
    trip_level_output4<-trip_level_output3 %>% 
      dplyr::group_by(month) %>% 
      dplyr::summarise(ntrips_alt_month_sum = sum(ntrips_alt_sum)) 
    
    trip_level_output4<-trip_level_output4 %>% 
      dplyr::right_join(trip_level_output3, by="month") %>% 
      dplyr::ungroup() %>% 
      dplyr:: mutate(trip_props = ntrips_alt_sum/ntrips_alt_month_sum, 
                     n_sample=round(5000*trip_props)) %>% 
      dplyr::mutate(n_sample=case_when(n_sample==0 ~ 1, TRUE~n_sample))
    
    domains=as.factor(trip_level_output4$period2)
    levels(domains)
    
    for(z in levels(domains)){
      assign(paste0("ntrip_", z), mean(trip_level_output4[(trip_level_output4$period2 == z),]$n_sample))
    }
    
    keep_rel_pairs_month1<- trip_data %>% 
      dplyr::mutate(tot_cod_catch=tot_keep_cod+tot_rel_cod, 
                    tot_hadd_catch=tot_keep_hadd+tot_rel_hadd) %>% 
      dplyr::select(period2, month, tot_cod_catch,tot_keep_cod,tot_rel_cod,tot_hadd_catch,tot_keep_hadd,tot_rel_hadd ) %>% 
      dplyr::mutate(period=period2)
    
    
    if(y==2010){
      
      keep_rel_pairs_month1<- stratified(keep_rel_pairs_month1, "period", 
                                  size=c(  "7_fh"=ntrip_7_fh,"7_pr"=ntrip_7_pr,"8_fh"=ntrip_8_fh,"8_pr"=ntrip_8_pr,"9_fh"=ntrip_9_fh,
                                           "9_pr"=ntrip_9_pr,"10_fh"=ntrip_10_fh,"10_pr"=ntrip_10_pr,"11_fh"=ntrip_11_fh,"11_pr"=ntrip_11_pr,
                                           "12_fh"=ntrip_12_fh,"12_pr"=ntrip_12_pr,"13_fh"=ntrip_13_fh,"13_pr"=ntrip_13_pr,"14_fh"=ntrip_14_fh,
                                           "14_pr"=ntrip_14_pr,"15_fh"=ntrip_15_fh,"15_pr"=ntrip_15_pr,"16_fh"=ntrip_16_fh,"16_pr"=ntrip_16_pr,
                                           "17_fh"=ntrip_17_fh,"17_pr"=ntrip_17_pr,"18_fh"=ntrip_18_fh,"18_pr"=ntrip_18_pr,"19_fh"=ntrip_19_fh,
                                           "19_pr"=ntrip_19_pr,"20_fh"=ntrip_20_fh,"20_pr"=ntrip_20_pr,"21_fh"=ntrip_21_fh,"21_pr"=ntrip_21_pr,
                                           "22_fh"=ntrip_22_fh,"22_pr"=ntrip_22_pr,"23_fh"=ntrip_23_fh))
      
      
    }
    
    if(y==2011){
      
      keep_rel_pairs_month1<- stratified(keep_rel_pairs_month1, "period", 
                                  size=c( "7_fh"=ntrip_7_fh,"7_pr"=ntrip_7_pr,"8_fh"=ntrip_8_fh,"9_fh"=ntrip_9_fh,"9_pr"=ntrip_9_pr,"10_fh"=ntrip_10_fh,
                                          "10_pr"=ntrip_10_pr,"11_fh"=ntrip_11_fh,"11_pr"=ntrip_11_pr,"12_fh"=ntrip_12_fh,"12_pr"=ntrip_12_pr,
                                          "13_fh"=ntrip_13_fh,"13_pr"=ntrip_13_pr,"14_fh"=ntrip_14_fh,"14_pr"=ntrip_14_pr,"15_fh"=ntrip_15_fh,
                                          "15_pr"=ntrip_15_pr,"16_fh"=ntrip_16_fh,"16_pr"=ntrip_16_pr,"17_fh"=ntrip_17_fh,"17_pr"=ntrip_17_pr,
                                          "18_fh"=ntrip_18_fh,"18_pr"=ntrip_18_pr,"19_fh"=ntrip_19_fh,"19_pr"=ntrip_19_pr,"20_fh"=ntrip_20_fh,
                                          "20_pr"=ntrip_20_pr,"21_fh"=ntrip_21_fh,"21_pr"=ntrip_21_pr,"22_fh"=ntrip_22_fh,"22_pr"=ntrip_22_pr))
      
    }                                   
    
    if(y==2012){
      
      keep_rel_pairs_month1<- stratified(keep_rel_pairs_month1, "period", 
                                  size=c(  "6_pr"=ntrip_6_pr,"7_fh"=ntrip_7_fh,"7_pr"=ntrip_7_pr,"8_fh"=ntrip_8_fh,"8_pr"=ntrip_8_pr,
                                           "9_fh"=ntrip_9_fh,"9_pr"=ntrip_9_pr,"10_fh"=ntrip_10_fh,"10_pr"=ntrip_10_pr,"11_fh"=ntrip_11_fh,
                                           "11_pr"=ntrip_11_pr,"12_fh"=ntrip_12_fh,"12_pr"=ntrip_12_pr,"13_fh"=ntrip_13_fh,"13_pr"=ntrip_13_pr,
                                           "14_fh"=ntrip_14_fh,"14_pr"=ntrip_14_pr,"15_fh"=ntrip_15_fh,"15_pr"=ntrip_15_pr,"16_fh"=ntrip_16_fh,
                                           "16_pr"=ntrip_16_pr,"17_fh"=ntrip_17_fh,"17_pr"=ntrip_17_pr,"18_fh"=ntrip_18_fh,"18_pr"=ntrip_18_pr,
                                           "19_fh"=ntrip_19_fh,"19_pr"=ntrip_19_pr,"20_fh"=ntrip_20_fh,"21_fh"=ntrip_21_fh,"21_pr"=ntrip_21_pr,
                                           "22_fh"=ntrip_22_fh,"22_pr"=ntrip_22_pr,"23_fh"=ntrip_23_fh))
    }                                   
    
    if(y==2013){
      keep_rel_pairs_month1<- stratified(keep_rel_pairs_month1, "period", 
                                  size=c( "7_fh"=ntrip_7_fh,"8_fh"=ntrip_8_fh,"8_pr"=ntrip_8_pr,"9_fh"=ntrip_9_fh,"9_pr"=ntrip_9_pr,"10_fh"=ntrip_10_fh,
                                          "10_pr"=ntrip_10_pr,"11_fh"=ntrip_11_fh,"11_pr"=ntrip_11_pr,"12_fh"=ntrip_12_fh,"12_pr"=ntrip_12_pr,
                                          "13_fh"=ntrip_13_fh,"13_pr"=ntrip_13_pr,"14_fh"=ntrip_14_fh,"14_pr"=ntrip_14_pr,"15_fh"=ntrip_15_fh,
                                          "15_pr"=ntrip_15_pr,"16_fh"=ntrip_16_fh,"16_pr"=ntrip_16_pr,"17_fh"=ntrip_17_fh,"17_pr"=ntrip_17_pr,
                                          "18_fh"=ntrip_18_fh,"18_pr"=ntrip_18_pr,"19_fh"=ntrip_19_fh,"19_pr"=ntrip_19_pr,"20_fh"=ntrip_20_fh,
                                          "20_pr"=ntrip_20_pr))
      
    }  
    
    if(y==2014){
      keep_rel_pairs_month1<- stratified(keep_rel_pairs_month1, "period", 
                                  size=c( "7_fh"=ntrip_7_fh,"8_fh"=ntrip_8_fh,"9_fh"=ntrip_9_fh,"9_pr"=ntrip_9_pr,"10_fh"=ntrip_10_fh,
                                          "10_pr"=ntrip_10_pr,"11_fh"=ntrip_11_fh,"11_pr"=ntrip_11_pr,"12_fh"=ntrip_12_fh,"12_pr"=ntrip_12_pr,
                                          "13_fh"=ntrip_13_fh,"13_pr"=ntrip_13_pr,"14_fh"=ntrip_14_fh,"14_pr"=ntrip_14_pr,"15_fh"=ntrip_15_fh,
                                          "15_pr"=ntrip_15_pr,"16_fh"=ntrip_16_fh,"16_pr"=ntrip_16_pr,"17_fh"=ntrip_17_fh,"17_pr"=ntrip_17_pr,
                                          "18_fh"=ntrip_18_fh,"18_pr"=ntrip_18_pr,"19_fh"=ntrip_19_fh,"19_pr"=ntrip_19_pr,"20_fh"=ntrip_20_fh,
                                          "20_pr"=ntrip_20_pr))
      
    }  
    
    if(y==2015){
      keep_rel_pairs_month1<- stratified(keep_rel_pairs_month1, "period", 
                                  size=c( "8_fh"=ntrip_8_fh,"9_fh"=ntrip_9_fh,"9_pr"=ntrip_9_pr,"10_fh"=ntrip_10_fh,"10_pr"=ntrip_10_pr,
                                          "11_fh"=ntrip_11_fh,"11_pr"=ntrip_11_pr,"12_fh"=ntrip_12_fh,"12_pr"=ntrip_12_pr,"13_fh"=ntrip_13_fh,
                                          "13_pr"=ntrip_13_pr,"14_fh"=ntrip_14_fh,"14_pr"=ntrip_14_pr,"15_fh"=ntrip_15_fh,"15_pr"=ntrip_15_pr,
                                          "16_fh"=ntrip_16_fh,"16_pr"=ntrip_16_pr,"17_fh"=ntrip_17_fh,"17_pr"=ntrip_17_pr,"18_fh"=ntrip_18_fh,
                                          "18_pr"=ntrip_18_pr,"19_fh"=ntrip_19_fh,"20_fh"=ntrip_20_fh))
      
    }  
    
    if(y==2016){
      keep_rel_pairs_month1<- stratified(keep_rel_pairs_month1, "period", 
                                  size=c( "7_fh"=ntrip_7_fh,
                                          "8_fh"=ntrip_8_fh,
                                          "9_fh"=ntrip_9_fh,
                                          "9_pr"=ntrip_9_pr,
                                          "10_fh"=ntrip_10_fh,
                                          "10_pr"=ntrip_10_pr,
                                          "11_fh"=ntrip_11_fh,
                                          "11_pr"=ntrip_11_pr,
                                          "12_fh"=ntrip_12_fh,
                                          "12_pr"=ntrip_12_pr,
                                          "13_fh"=ntrip_13_fh,
                                          "13_pr"=ntrip_13_pr,
                                          "14_fh"=ntrip_14_fh,
                                          "14_pr"=ntrip_14_pr,
                                          "15_fh"=ntrip_15_fh,
                                          "15_pr"=ntrip_15_pr,
                                          "16_fh"=ntrip_16_fh,
                                          "16_pr"=ntrip_16_pr,
                                          "17_fh"=ntrip_17_fh,
                                          "17_pr"=ntrip_17_pr,
                                          "18_fh"=ntrip_18_fh,
                                          "18_pr"=ntrip_18_pr,
                                          "19_fh"=ntrip_19_fh,
                                          "19_pr"=ntrip_19_pr,
                                          "20_fh"=ntrip_20_fh,
                                          "20_pr"=ntrip_20_pr,
                                          "22_pr"=ntrip_22_pr))
      
    }  
    
    if(y==2017){
      keep_rel_pairs_month1<- stratified(keep_rel_pairs_month1, "period", 
                                  size=c( "7_fh"=ntrip_7_fh,
                                          "7_pr"=ntrip_7_pr,
                                          "8_fh"=ntrip_8_fh,
                                          "9_fh"=ntrip_9_fh,
                                          "9_pr"=ntrip_9_pr,
                                          "10_fh"=ntrip_10_fh,
                                          "10_pr"=ntrip_10_pr,
                                          "11_fh"=ntrip_11_fh,
                                          "11_pr"=ntrip_11_pr,
                                          "12_fh"=ntrip_12_fh,
                                          "12_pr"=ntrip_12_pr,
                                          "13_fh"=ntrip_13_fh,
                                          "13_pr"=ntrip_13_pr,
                                          "14_fh"=ntrip_14_fh,
                                          "14_pr"=ntrip_14_pr,
                                          "15_fh"=ntrip_15_fh,
                                          "15_pr"=ntrip_15_pr,
                                          "16_fh"=ntrip_16_fh,
                                          "16_pr"=ntrip_16_pr,
                                          "17_fh"=ntrip_17_fh,
                                          "17_pr"=ntrip_17_pr,
                                          "18_fh"=ntrip_18_fh,
                                          "18_pr"=ntrip_18_pr,
                                          "19_fh"=ntrip_19_fh,
                                          "19_pr"=ntrip_19_pr,
                                          "20_fh"=ntrip_20_fh,
                                          "20_pr"=ntrip_20_pr,
                                          "21_fh"=ntrip_21_fh,
                                          "21_pr"=ntrip_21_pr,
                                          "22_fh"=ntrip_22_fh))
      
    }  
    
    if(y==2018){
      keep_rel_pairs_month1<- stratified(keep_rel_pairs_month1, "period", 
                                  size=c( "7_fh"=ntrip_7_fh,
                                          "8_fh"=ntrip_8_fh,
                                          "9_fh"=ntrip_9_fh,
                                          "9_pr"=ntrip_9_pr,
                                          "10_fh"=ntrip_10_fh,
                                          "10_pr"=ntrip_10_pr,
                                          "11_fh"=ntrip_11_fh,
                                          "11_pr"=ntrip_11_pr,
                                          "12_fh"=ntrip_12_fh,
                                          "12_pr"=ntrip_12_pr,
                                          "13_fh"=ntrip_13_fh,
                                          "13_pr"=ntrip_13_pr,
                                          "14_fh"=ntrip_14_fh,
                                          "14_pr"=ntrip_14_pr,
                                          "15_fh"=ntrip_15_fh,
                                          "15_pr"=ntrip_15_pr,
                                          "16_fh"=ntrip_16_fh,
                                          "16_pr"=ntrip_16_pr,
                                          "17_fh"=ntrip_17_fh,
                                          "17_pr"=ntrip_17_pr,
                                          "18_fh"=ntrip_18_fh,
                                          "18_pr"=ntrip_18_pr,
                                          "19_fh"=ntrip_19_fh,
                                          "19_pr"=ntrip_19_pr,
                                          "20_fh"=ntrip_20_fh))
      
    }  
    
    if(y==2019){
      keep_rel_pairs_month1<- stratified(keep_rel_pairs_month1, "period", 
                                  size=c( "7_fh"=ntrip_7_fh,
                                          "7_pr"=ntrip_7_pr,
                                          "8_fh"=ntrip_8_fh,
                                          "9_fh"=ntrip_9_fh,
                                          "9_pr"=ntrip_9_pr,
                                          "10_fh"=ntrip_10_fh,
                                          "10_pr"=ntrip_10_pr,
                                          "11_fh"=ntrip_11_fh,
                                          "11_pr"=ntrip_11_pr,
                                          "12_fh"=ntrip_12_fh,
                                          "12_pr"=ntrip_12_pr,
                                          "13_fh"=ntrip_13_fh,
                                          "13_pr"=ntrip_13_pr,
                                          "14_fh"=ntrip_14_fh,
                                          "14_pr"=ntrip_14_pr,
                                          "15_fh"=ntrip_15_fh,
                                          "15_pr"=ntrip_15_pr,
                                          "16_fh"=ntrip_16_fh,
                                          "16_pr"=ntrip_16_pr,
                                          "17_fh"=ntrip_17_fh,
                                          "17_pr"=ntrip_17_pr,
                                          "18_fh"=ntrip_18_fh,
                                          "18_pr"=ntrip_18_pr,
                                          "19_fh"=ntrip_19_fh,
                                          "19_pr"=ntrip_19_pr,
                                          "20_fh"=ntrip_20_fh,
                                          "20_pr"=ntrip_20_pr))
      
    }  
    
    if(y==2020){
      keep_rel_pairs_month1<- stratified(keep_rel_pairs_month1, "period", 
                                  size=c( "7_pr"=ntrip_7_pr,
                                          "9_fh"=ntrip_9_fh,
                                          "9_pr"=ntrip_9_pr,
                                          "10_fh"=ntrip_10_fh,
                                          "10_pr"=ntrip_10_pr,
                                          "11_fh"=ntrip_11_fh,
                                          "11_pr"=ntrip_11_pr,
                                          "12_fh"=ntrip_12_fh,
                                          "12_pr"=ntrip_12_pr,
                                          "13_fh"=ntrip_13_fh,
                                          "13_pr"=ntrip_13_pr,
                                          "14_fh"=ntrip_14_fh,
                                          "14_pr"=ntrip_14_pr,
                                          "15_fh"=ntrip_15_fh,
                                          "15_pr"=ntrip_15_pr,
                                          "16_fh"=ntrip_16_fh,
                                          "16_pr"=ntrip_16_pr,
                                          "17_fh"=ntrip_17_fh,
                                          "17_pr"=ntrip_17_pr,
                                          "18_fh"=ntrip_18_fh,
                                          "18_pr"=ntrip_18_pr,
                                          "19_fh"=ntrip_19_fh,
                                          "19_pr"=ntrip_19_pr,
                                          "20_fh"=ntrip_20_fh,
                                          "21_pr"=ntrip_21_pr))
      
    }  
    

    
    unique(keep_rel_pairs_month1$month)
    keep_rel_pairs_month<-list()
    for(z in unique(keep_rel_pairs_month1$month)){
      keep_rel_pairs_month2<-keep_rel_pairs_month1 %>% 
        dplyr::filter(month==z)
      
      sum_keep_cod<-sum(keep_rel_pairs_month2$tot_keep_cod)
      sum_keep_hadd<-sum(keep_rel_pairs_month2$tot_keep_hadd)
      
      if(sum_keep_cod>0 & sum_keep_hadd>0){
        ktau_keep<- cor.test(keep_rel_pairs_month2$tot_keep_cod, 
                             keep_rel_pairs_month2$tot_keep_hadd, method = c("kendall"))
        
        k_tau_keep_est_mnth<-ktau_keep[["estimate"]]
        k_tau_keep_p_mnth<- ktau_keep[["p.value"]]
        
        ktau_catch<- cor.test(keep_rel_pairs_month2$tot_cod_catch, 
                              keep_rel_pairs_month2$tot_hadd_catch, method = c("kendall"))
        
        k_tau_catch_est_mnth<-ktau_catch[["estimate"]]
        k_tau_catch_p_mnth<- ktau_catch[["p.value"]]
        
        keep_rel_pairs_month2<- as.data.frame(cbind(k_tau_keep_est_mnth,k_tau_keep_p_mnth, k_tau_catch_est_mnth, k_tau_catch_p_mnth), names="TRUE")
        keep_rel_pairs_month[[z]]<-keep_rel_pairs_month2 %>% 
          dplyr::mutate(month=z, draw=x)
      }
      
      if(sum_keep_cod==0 | sum_keep_hadd==0){
        
        k_tau_keep_est_mnth<-0
        k_tau_keep_p_mnth<- 1
        
        ktau_catch<- cor.test(keep_rel_pairs_month2$tot_cod_catch, 
                              keep_rel_pairs_month2$tot_hadd_catch, method = c("kendall"))
        
        k_tau_catch_est_mnth<-ktau_catch[["estimate"]]
        k_tau_catch_p_mnth<- ktau_catch[["p.value"]]
        
        keep_rel_pairs_month2<- as.data.frame(cbind(k_tau_keep_est_mnth,k_tau_keep_p_mnth, k_tau_catch_est_mnth, k_tau_catch_p_mnth), names="TRUE")
        keep_rel_pairs_month[[z]]<-keep_rel_pairs_month2 %>% 
          dplyr::mutate(month=z, draw=x)
      }
    }
    keep_rel_pairs_month_all=list.stack(keep_rel_pairs_month, fill=TRUE) 
    keep_rel_pairs_month_all<-keep_rel_pairs_month_all  %>% 
      dplyr::mutate(decade=d, corr_type=t, copula=c, draw=x)
    
    
    
    
    predictions[[x]]<-predictions0 %>%  
      dplyr::left_join(keep_rel_pairs, by=c("decade", "corr_type", "copula", "draw")) %>% 
      dplyr::left_join(keep_rel_pairs_month_all, by=c("month", "decade", "corr_type", "copula", "draw")) %>% 
      dplyr::mutate(calibration_year=d, prediction_year=y_plus_1)
    
    
  }
  predictions2<-list.stack(predictions, fill=TRUE)
  predictions_d[[d]]<-predictions2
  
}
predictions_all<- list.stack(predictions_d, fill=TRUE)







