create_regimes <- function(sensor, regimes){
  #a regime vector is given in the following format 
  #regime[1] = earlier date 
  #regime[2] = later date 
  #regime[3] = smaller ws 
  #regime[4] = larger wws 
  #regime[5] = smaller wd
  #regime[6] = larger wd
  #regime[7] = regime name

  for(regimeN in regimes){

    print(regimeN[1])
    print(regimeN[5])
    print(regimeN[6])
    sensor$regime[which(sensor$date_local > regimeN[1] & sensor$date_local< regimeN[2] 
                        & sensor$ws > regimeN[3] & sensor$ws <= regimeN[4]  
                          & sensor$wd >= regimeN[5] & sensor$wd < regimeN[6])
    ] <- regimeN[7]
  

    
      
  }
  
  return(sensor)
}


# trial <- snfiles$sn45
# for(regime in sn45regimes1){
#   trial$regime[which(trial$date_local > regime[1] & trial$date_local< regime[2] 
#                       & trial$ws > regime[3], trial$ws <= regime[4] & 
#                         trial$wd >= regime[5] & trial$wd < regime[6])
#   ] <- regime[7]
#   
#   
#   
# }