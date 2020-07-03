changebins <- function(sn45){
  #this is the function I created for changing the bins from 15 sizes to 6
  
  library(openair)
  timePlot(sn45, pollutant=c("bin0", "bin3", "bin5", "bin6", "bin9", "bin14"), group=TRUE) #plot what it looks like before
  enddate <- 252141 #this is the datapoint (date) that I determined that they went from 15 bins to 6
  sn45$bin0[enddate: length(sn45$bin0)] <- sn45$bin0[enddate:length(sn45$bin0)] / 0.05 #since it's in reverse chronological order, we go from enddate to end of the dataframe
  sn45$bin1[enddate:length(sn45$bin0)] <- sn45$bin1[enddate:length(sn45$bin0)] / 0.05 # dividing by bin width
  sn45$bin2[enddate:length(sn45$bin0)] <- sn45$bin2[enddate:length(sn45$bin0)] / 0.05
  sn45$bin3[enddate:length(sn45$bin0)] <- sn45$bin3[enddate:length(sn45$bin0)] / 0.05
  sn45$bin4[enddate:length(sn45$bin0)] <- sn45$bin4[enddate:length(sn45$bin0)] / 0.1
  sn45$bin5[enddate:length(sn45$bin0)] <- sn45$bin5[enddate:length(sn45$bin0)] / 0.1
  sn45$bin6[enddate:length(sn45$bin0)] <- sn45$bin6[enddate:length(sn45$bin0)] / 0.1
  sn45$bin7[enddate:length(sn45$bin0)] <- sn45$bin7[enddate:length(sn45$bin0)] / 0.1
  sn45$bin8[enddate:length(sn45$bin0)] <- sn45$bin8[enddate:length(sn45$bin0)] / 0.1
  sn45$bin9[enddate:length(sn45$bin0)] <- sn45$bin9[enddate:length(sn45$bin0)] / 0.5
  sn45$bin10[enddate:length(sn45$bin0)] <- sn45$bin10[enddate:length(sn45$bin0)] / 0.5
  sn45$bin11[enddate:length(sn45$bin0)] <- sn45$bin11[enddate:length(sn45$bin0)] / 0.5 
  sn45$bin12[enddate:length(sn45$bin0)] <- sn45$bin12[enddate:length(sn45$bin0)] / 2.5
  sn45$bin13[enddate:length(sn45$bin0)] <- sn45$bin13[enddate:length(sn45$bin0)] / 2.5
  sn45$bin14[enddate:length(sn45$bin0)] <- sn45$bin14[enddate:length(sn45$bin0)] / 2.5
  
  #next we add the bins together
  sn45$bin0[enddate:length(sn45$bin0)] <- sn45$bin0[enddate:length(sn45$bin0)]  + 
    sn45$bin1[enddate:length(sn45$bin0)] + sn45$bin2[enddate:length(sn45$bin0)]
  
  sn45$bin1[enddate:length(sn45$bin0)] <- sn45$bin3[enddate:length(sn45$bin0)]  + sn45$bin4[enddate:length(sn45$bin0)]
  
  sn45$bin2[enddate:length(sn45$bin0)] <- sn45$bin5[enddate:length(sn45$bin0)]  + sn45$bin6[enddate:length(sn45$bin0)]
  
  sn45$bin3[enddate:length(sn45$bin0)] <- sn45$bin7[enddate:length(sn45$bin0)]  + sn45$bin8[enddate:length(sn45$bin0)]
  
  sn45$bin4[enddate:length(sn45$bin0)] <- sn45$bin9[enddate:length(sn45$bin0)]  + 
    sn45$bin10[enddate:length(sn45$bin0)] + sn45$bin11[enddate:length(sn45$bin0)]
  
  sn45$bin5[enddate:length(sn45$bin0)] <- sn45$bin12[enddate:length(sn45$bin0)]  + 
    sn45$bin13[enddate:length(sn45$bin0)] + sn45$bin14[enddate:length(sn45$bin0)]
  
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
  
  timePlot(sn45, pollutant=c("bin0", "bin3", "bin5"), group=TRUE) #plot what it looks like now
  
}