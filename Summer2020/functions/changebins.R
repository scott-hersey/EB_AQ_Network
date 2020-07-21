changebins <- function(sn45){
  #this is the function I created for changing the bins from 15 sizes to 6
  
  library(openair)
  timePlot(sn45, pollutant=c("bin0","bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13", "bin14"), group=TRUE) #plot what it looks like before
  barplot(unlist(sn45[3, c("bin0","bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13", "bin14")]),
          main = "Particle Number vs Particle Size",
          names.arg = c("bin0","bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13", "bin14"),
  )
  barplot(unlist(sn45[250000, c("bin0","bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13", "bin14")]),
          main = "Particle Number vs Particle Size",
          names.arg = c("bin0","bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13", "bin14"),
  )
  enddate <- 34707 #this is the datapoint (date) that I determined that they went from 15 bins to 6
  sn45$bin0[ 1:enddate ] <- sn45$bin0[ 1:enddate] / 0.05 #since it's in reverse chronological order, we go from enddate to end of the dataframe
  sn45$bin1[ 1:enddate] <- sn45$bin1[ 1:enddate] / 0.05 # dividing by bin width
  sn45$bin2[ 1:enddate] <- sn45$bin2[ 1:enddate] / 0.05
  sn45$bin3[ 1:enddate] <- sn45$bin3[ 1:enddate] / 0.05
  sn45$bin4[ 1:enddate] <- sn45$bin4[ 1:enddate] / 0.1
  sn45$bin5[ 1:enddate] <- sn45$bin5[ 1:enddate] / 0.1
  sn45$bin6[ 1:enddate] <- sn45$bin6[ 1:enddate] / 0.1
  sn45$bin7[ 1:enddate] <- sn45$bin7[ 1:enddate] / 0.1
  sn45$bin8[ 1:enddate] <- sn45$bin8[ 1:enddate] / 0.1
  sn45$bin9[ 1:enddate] <- sn45$bin9[ 1:enddate] / 0.5
  sn45$bin10[ 1:enddate] <- sn45$bin10[ 1:enddate] / 0.5
  sn45$bin11[ 1:enddate] <- sn45$bin11[ 1:enddate] / 0.5 
  sn45$bin12[ 1:enddate] <- sn45$bin12[ 1:enddate] / 2.5
  sn45$bin13[ 1:enddate] <- sn45$bin13[ 1:enddate] / 2.5
  sn45$bin14[ 1:enddate] <- sn45$bin14[ 1:enddate] / 2.5
  
  #next we add the bins together
  sn45$bin0[1: enddate] <- sn45$bin0[ 1:enddate]  + 
    sn45$bin1[ 1:enddate] + sn45$bin2[ 1:enddate]
  
  sn45$bin1[ 1:enddate] <- sn45$bin3[ 1:enddate]  + sn45$bin4[ 1:enddate]
  
  sn45$bin2[ 1:enddate] <- sn45$bin5[ 1:enddate]  + sn45$bin6[ 1:enddate]
  
  sn45$bin3[ 1:enddate] <- sn45$bin7[ 1:enddate]  + sn45$bin8[ 1:enddate]
  
  sn45$bin4[ 1:enddate] <- sn45$bin9[ 1:enddate]  + 
    sn45$bin10[ 1:enddate] + sn45$bin11[ 1:enddate]
  
  sn45$bin5[ 1:enddate] <- sn45$bin12[ 1:enddate]  + 
    sn45$bin13[ 1:enddate] + sn45$bin14[ 1:enddate]
  
  #getting rid of unused variables
  sn45$bin6 <- NULL
  sn45$bin7 <- NULL
  sn45$bin8 <- NULL
  sn45$bin9 <- NULL
  sn45$bin10 <- NULL
  sn45$bin11 <- NULL
  sn45$bin12 <- NULL
  sn45$bin13 <- NULL
  sn45$bin14 <- NULL
  sn45$X.1 <- NULL
  sn45$X <- NULL
  
  timePlot(sn45, pollutant=c("bin0","bin1", "bin2", "bin3", "bin4", "bin5"), group=TRUE) #plot what it looks like now
  barplot(unlist(sn45[3, c("bin0","bin1", "bin2", "bin3", "bin4", "bin5")]),
          main = "Particle Number vs Particle Size",
          names.arg = c("bin0","bin1", "bin2", "bin3", "bin4", "bin5"),
  )
  barplot(unlist(sn45[250000, c("bin0","bin1", "bin2", "bin3", "bin4", "bin5")]),
          main = "Particle Number vs Particle Size",
          names.arg = c("bin0","bin1", "bin2", "bin3", "bin4", "bin5"),
  )
  #return(sn45)
}

changebins2 <- function(sn45){
  #this is the function I created for changing the bins from 15 sizes to 6
  
  library(openair)
  timePlot(sn45, pollutant=c("bin0","bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13", "bin14"), group=TRUE) #plot what it looks like before
  barplot(unlist(sn45[3, c("bin0","bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13", "bin14")]),
          main = "Particle Number vs Particle Size",
          names.arg = c("bin0","bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13", "bin14"),
  )
  barplot(unlist(sn45[250000, c("bin0","bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13", "bin14")]),
          main = "Particle Number vs Particle Size",
          names.arg = c("bin0","bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13", "bin14"),
  )
  enddate <- 34707 #this is the datapoint (date) that I determined that they went from 15 bins to 6

  
  #next we add the bins together
  sn45$bin0[1: enddate] <- sn45$bin0[ 1:enddate]  + 
    sn45$bin1[ 1:enddate] + sn45$bin2[ 1:enddate]
  
  sn45$bin1[ 1:enddate] <- sn45$bin3[ 1:enddate]  + sn45$bin4[ 1:enddate]
  
  sn45$bin2[ 1:enddate] <- sn45$bin5[ 1:enddate]  + sn45$bin6[ 1:enddate]
  
  sn45$bin3[ 1:enddate] <- sn45$bin7[ 1:enddate]  + sn45$bin8[ 1:enddate]
  
  sn45$bin4[ 1:enddate] <- sn45$bin9[ 1:enddate]  + 
    sn45$bin10[ 1:enddate] + sn45$bin11[ 1:enddate]
  
  sn45$bin5[ 1:enddate] <- sn45$bin12[ 1:enddate]  + 
    sn45$bin13[ 1:enddate] + sn45$bin14[ 1:enddate]
  
  sn45$bin0[ 1:enddate ] <- sn45$bin0[ 1:enddate] / 0.15 #since it's in reverse chronological order, we go from enddate to end of the dataframe
  sn45$bin1[ 1:enddate] <- sn45$bin1[ 1:enddate] / 0.15 # dividing by bin width
  sn45$bin2[ 1:enddate] <- sn45$bin2[ 1:enddate] / 0.15
  sn45$bin3[ 1:enddate] <- sn45$bin3[ 1:enddate] / 0.25
  sn45$bin4[ 1:enddate] <- sn45$bin4[ 1:enddate] / 1.5
  sn45$bin5[ 1:enddate] <- sn45$bin5[ 1:enddate] / 7.5

  
  # #getting rid of unused variables
  # sn45$bin6 <- NULL
  # sn45$bin7 <- NULL
  # sn45$bin8 <- NULL
  # sn45$bin9 <- NULL
  # sn45$bin10 <- NULL
  # sn45$bin11 <- NULL
  # sn45$bin12 <- NULL
  # sn45$bin13 <- NULL
  # sn45$bin14 <- NULL
  # sn45$X.1 <- NULL
  # sn45$X <- NULL
  
  timePlot(sn45, pollutant=c("bin0","bin1", "bin2", "bin3", "bin4", "bin5"), group=TRUE) #plot what it looks like now
  barplot(unlist(sn45[3, c("bin0","bin1", "bin2", "bin3", "bin4", "bin5")]),
          main = "Particle Number vs Particle Size",
          names.arg = c("bin0","bin1", "bin2", "bin3", "bin4", "bin5"),
  )
  barplot(unlist(sn45[250000, c("bin0","bin1", "bin2", "bin3", "bin4", "bin5")]),
          main = "Particle Number vs Particle Size",
          names.arg = c("bin0","bin1", "bin2", "bin3", "bin4", "bin5"),
  )
  #return(sn45)
}

