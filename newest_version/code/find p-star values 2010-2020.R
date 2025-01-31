



#starting points 
#y<-2020 

MRIP_data <- data.frame(read.csv(paste0(input_data_cd,"total AB1B2 2010_2020 GoM.csv")))
MRIP_data<-MRIP_data %>% 
  dplyr::filter(year==y)


if (!(y %in% c(2018, 2019))){
  p_star_cod_variable<-MRIP_data$cod_harvest/MRIP_data$cod_tot_cat
  p_star_hadd_variable<-MRIP_data$hadd_harvest/MRIP_data$hadd_tot_cat
}

if (y==2018){
  p_star_cod_variable<-1
  p_star_hadd_variable<-0.49069481
}

if (y==2019){
  p_star_cod_variable<-0
  p_star_hadd_variable<-0.22310685
}


#In 2018, the federal cod fishery was closed with no states offering different regulations. 
#However, we do see some harvest in the MRIP data. I will ignore this harvest and chalk it up to misreporting 
#or illegal harvest, which we cannot account for rn, and instead only calibrate such that simulated haddock 
#harvest approximates haddock harvest from MRIP. Similarly, in 2019, there was a two-week open cod season
#from September 15-30 with a 1-fish BL; total cod harvest from MRIP in this year was 16,613 fish. However, 
#given the catch-per-trip and number of directed trip that occurred in that period, the simulation model can
#never simulate enough harvest to approximate the MRIP estimate within 2%. Looking at the MRIP data shows 
#that about 44% of the cod landings in 2019 occurred by Maine private boat anglers in the first half of June. 
#Since Maine state regulation prohibited GoM cod that year, this indicates illegal harvest that we cannot
#account for. Therefore, like 2018, I will calibrate the simulation model in 2019 to the MRIP estimate of 
#haddock only and allow the maximum harvest of cod in the period during which cod was open.  

cod_achieved<-0
hadd_achieved<-0

  repeat {
    
    print("p_star_cod_variable")
    print(p_star_cod_variable)
    print("p_star_hadd_variable")
    print(p_star_hadd_variable)
    print(y)
    
    source(paste0(input_code_cd,"calibration loop2 2010-2020.R"))
    
    
    
    #cod
    print(paste0("model cod harvest:", round(sum(aggregate_trip_data$tot_keep_cod))))
    print(paste0("MRIP cod harvest:", round(sum(MRIP_data$cod_harvest))))
    print(paste0("diff cod harvest:", sum(aggregate_trip_data$tot_keep_cod)-sum(MRIP_data$cod_harvest)))
    print(paste0("% diff cod harvest:", round(((sum(aggregate_trip_data$tot_keep_cod)-sum(MRIP_data$cod_harvest))/sum(MRIP_data$cod_harvest)*100), 2)))

    print(paste0("model hadd harvest:", round(sum(aggregate_trip_data$tot_keep_hadd))))
    print(paste0("MRIP hadd harvest:", round(sum(MRIP_data$hadd_harvest))))
    print(paste0("diff hadd harvest:", sum(aggregate_trip_data$tot_keep_hadd)-sum(MRIP_data$hadd_harvest)))
    print(paste0("% diff hadd harvest:", round(((sum(aggregate_trip_data$tot_keep_hadd)-sum(MRIP_data$hadd_harvest))/sum(MRIP_data$hadd_harvest)*100), 2)))
    
    
    if (y %in%  c(2018, 2019)){
      cod_achieved<-1
      hadd_achieved <- ifelse(hadd_harvest_diff<500 | abs(hadd_harvest_perc_diff)<5, 1, 0) 
      
    }
    
    if (!(y %in%  c(2018, 2019))){
      cod_achieved <- ifelse(cod_harvest_diff<500 | abs(cod_harvest_perc_diff)<5, 1, 0) 
      hadd_achieved <- ifelse(hadd_harvest_diff<500 | abs(hadd_harvest_perc_diff)<5, 1, 0) 
      
    }

    
    if  ((cod_achieved==1 & hadd_achieved==1))  break

    if (cod_harvest_perc_diff<0 & cod_achieved!=1){
      
      if (cod_harvest_perc_diff< -20){
        p_star_cod_variable<-p_star_cod_variable - 0.075
      }
      
      if (cod_harvest_perc_diff< -10 & cod_harvest_perc_diff>= -20){
        p_star_cod_variable<-p_star_cod_variable - 0.035
      }
      
      if (cod_harvest_perc_diff< -5 & cod_harvest_perc_diff>= -10){
        p_star_cod_variable<-p_star_cod_variable - 0.015
      }
      
    } 
    
    
    if (hadd_harvest_perc_diff<0 & hadd_achieved!=1){
      
      if (hadd_harvest_perc_diff< -20){
        p_star_hadd_variable<-p_star_hadd_variable - 0.075
      }
      
      if (hadd_harvest_perc_diff< -10 & hadd_harvest_perc_diff>= -20){
        p_star_hadd_variable<-p_star_hadd_variable - 0.035
      }
      
      if (hadd_harvest_perc_diff< -5 & hadd_harvest_perc_diff>= -10){
        p_star_hadd_variable<-p_star_hadd_variable - 0.015
      }
      
    } 
    
    
    if (cod_harvest_perc_diff>0 & cod_achieved!=1){
      
      if (cod_harvest_perc_diff> 20){
        p_star_cod_variable<-p_star_cod_variable + 0.07
      }
      
      if (cod_harvest_perc_diff> 10 & cod_harvest_perc_diff<= 20){
        p_star_cod_variable<-p_star_cod_variable + 0.03
      }
      
      if (cod_harvest_perc_diff> 5 & cod_harvest_perc_diff<= 10){
        p_star_cod_variable<-p_star_cod_variable + 0.01
      }
      
    } 
    
    if (hadd_harvest_perc_diff>0 & hadd_achieved!=1){
      
      if (hadd_harvest_perc_diff> 20){
        p_star_hadd_variable<-p_star_hadd_variable + 0.07
      }
      
      if (hadd_harvest_perc_diff> 10 & hadd_harvest_perc_diff<= 20){
        p_star_hadd_variable<-p_star_hadd_variable + 0.03
      }
      
      if (hadd_harvest_perc_diff> 5 & hadd_harvest_perc_diff<= 10){
        p_star_hadd_variable<-p_star_hadd_variable + 0.01
      }
      
    } 
    
  }
  
  
  p_star_cod_variable<- p_star_cod
  p_star_hadd_variable<- p_star_hadd
  
