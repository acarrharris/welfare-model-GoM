



#starting points 

p_star_cod_variable<-.915
p_star_hadd_variable<-.45


repeat {
  source("calibration loop.R")
  
  
  if (cod_harvest_diff<0 & abs(cod_harvest_diff)>1){
    p_star_cod_variable<-p_star_cod_variable +.005
  }
  
  if (cod_harvest_diff>0 & abs(cod_harvest_diff)>1){
    p_star_cod_variable<-p_star_cod_variable -.005
  }
  
  if (hadd_harvest_diff<0 & abs(hadd_harvest_diff)>1){
    p_star_hadd_variable<-p_star_hadd_variable +.005
  }
  
  if (hadd_harvest_diff>0 & abs(hadd_harvest_diff)>1){
    p_star_hadd_variable<-p_star_hadd_variable -.005
  }
  

  
  if ((abs(cod_harvest_diff)<2) & (abs(hadd_harvest_diff)<2)) break
  
}


cod_harvest_diff
hadd_harvest_diff
p_star_cod_variable
p_star_hadd_variable

p_star_cod_variable<- p_star_cod
p_star_hadd_variable<- p_star_hadd



