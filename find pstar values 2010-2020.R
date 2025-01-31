



#starting points 
#y<-2020 

MRIP_data <- data.frame( read.csv("total AB1B2 2010_2020 GoM.csv"))
MRIP_data<-MRIP_data %>% 
  dplyr::filter(year==y)



if (y==2015){
  p_star_cod_variable<-0.98686030 
  p_star_hadd_variable<-0.58678162
}


if (y==2016){
  p_star_cod_variable<-0.54572447 
  p_star_hadd_variable<-0.76205426
}


if (y==2017){
  p_star_cod_variable<-0.91532953 
  p_star_hadd_variable<-0.60596406
}

if (y==2018){
  p_star_cod_variable<-1.00000000 
  p_star_hadd_variable<-0.49069481
}


if (y==2019){
  p_star_cod_variable<-0
  p_star_hadd_variable<-0.22310685
}


if (y %in% c(2020)){
  p_star_cod_variable<-.9105
  p_star_hadd_variable<-.4
}


#In 2018, the federal cod fishery was closed with no states offering different regulations. 
#However, we do see some harvest in the MRIP data. I will ignore this harvest and chalk it up to misreporting 
#or illegal harves, which we cannot account for rn, and instead only calibrate such that simulated haddock 
#harvest approximates haddock harvest from MRIP. Similarly, in 2019, there was a two-week open cod season
#from September 15-30 with a 1-fish BL; total cod harvest from MRIP in this year was 16,613 fish. However, 
#given the catch-per-trip and number of directed trip that occurred in that period, the simulation model can
#never simulate enough harvest to approximate the MRIP estimate within 2%. Looking at the MRIP data shows 
#that about 44% of the cod landings in 2019 occurred by Maine private boat anglers in the first half of June. 
#Since Maine state regulation prohibited GoM cod that year, this indicates illegal harvest that we cannot
#account for. Therefore, like 2018, I will calibrate the simulation model in 2019 to the MRIP estimate of 
#haddock only and allow the maximum harvest of cod in the period during which cod was open.  


if (y %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2020)){
repeat {
  print(p_star_cod_variable)
  print(p_star_hadd_variable)
  print(y)
  
  source("calibration loop 2010-2020.R")
  
  if (cod_harvest_diff<0 & abs(cod_harvest_diff)>1 & sum(MRIP_data$cod_harvest)<15000){
    p_star_cod_variable<-p_star_cod_variable +.0005
  }
  
  if (cod_harvest_diff>0 & abs(cod_harvest_diff)>1 & sum(MRIP_data$cod_harvest)<15000){
    p_star_cod_variable<-p_star_cod_variable -.0005
  }
  
  if (cod_harvest_diff<0 & abs(cod_harvest_diff)>1 & sum(MRIP_data$cod_harvest)>=15000){
    p_star_cod_variable<-p_star_cod_variable +.005
  }
  
  if (cod_harvest_diff>0 & abs(cod_harvest_diff)>1 & sum(MRIP_data$cod_harvest)>=15000){
    p_star_cod_variable<-p_star_cod_variable -.005
  }
  
  
  if (hadd_harvest_diff<0 & abs(hadd_harvest_diff)>1){
    p_star_hadd_variable<-p_star_hadd_variable +.005
  }
  
  if (hadd_harvest_diff<0 & abs(hadd_harvest_diff)>10){
    p_star_hadd_variable<-p_star_hadd_variable +.05
  }
  
  
  if (hadd_harvest_diff>0 & abs(hadd_harvest_diff)>1){
    p_star_hadd_variable<-p_star_hadd_variable -.005
  }
  

  if (hadd_harvest_diff>0 & abs(hadd_harvest_diff)>10){
    p_star_hadd_variable<-p_star_hadd_variable -.05
  }
  
  print(cod_harvest_diff)
  print(hadd_harvest_diff)
  
  
  if ((abs(cod_harvest_diff)<2) & (abs(hadd_harvest_diff)<2)) break
  
}


p_star_cod_variable<- p_star_cod
p_star_hadd_variable<- p_star_hadd

}



if (y %in% c(2018)){
  repeat {
    source("calibration loop 2010-2020.R")

    if (hadd_harvest_diff<0 & abs(hadd_harvest_diff)>1){
      p_star_hadd_variable<-p_star_hadd_variable +.005
    }
    
    if (hadd_harvest_diff>0 & abs(hadd_harvest_diff)>1){
      p_star_hadd_variable<-p_star_hadd_variable -.005
    }
    
    
    
    if (abs(hadd_harvest_diff)<2) break
    
  }
  
  
  p_star_cod_variable<- 1
  p_star_hadd_variable<- p_star_hadd
  
}


if (y %in% c(2019)){
  repeat {
    source("calibration loop 2010-2020.R")
    
    if (hadd_harvest_diff<0 & abs(hadd_harvest_diff)>1){
      p_star_hadd_variable<-p_star_hadd_variable +.005
    }
    
    if (hadd_harvest_diff>0 & abs(hadd_harvest_diff)>1){
      p_star_hadd_variable<-p_star_hadd_variable -.005
    }
    
    
    
    if (abs(hadd_harvest_diff)<2) break
    
  }
  
  
  p_star_cod_variable<- 0
  p_star_hadd_variable<- p_star_hadd
  
}
