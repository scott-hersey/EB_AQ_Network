change_bins <- function(sn){
  
  six_bin_data <- sn[which( sn$bin14 == 0 & 
                              sn$bin13 == 0 & 
                              sn$bin12 == 0 & 
                              sn$bin11 == 0 & 
                              sn$bin10 == 0 &
                              sn$bin9 == 0 & 
                              sn$bin8 == 0 & 
                              sn$bin7 == 0 &
                              sn$bin6 == 0 
  ), ]
  
  
  six_bin_data_filter <- six_bin_data[!is.na(six_bin_data$bin0) & !is.na(six_bin_data$bin1) & !is.na(six_bin_data$bin2) & 
                                        !is.na(six_bin_data$bin3) & !is.na(six_bin_data$bin4) & !is.na(six_bin_data$bin5)
                                      , ]
  six_bin_data_filter <- six_bin_data_filter[which(rowSums(six_bin_data_filter[, c( "bin0", "bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10", "bin11", "bin12", "bin13", "bin14")] )> 0.005),] 
  
  fifteen_bin_data <- sn[!(sn$date %in% six_bin_data$date), ]
  
  fifteen_bin_data_filter <- fifteen_bin_data[ !is.na(fifteen_bin_data$bin0) & !is.na(fifteen_bin_data$bin1) & 
                                                 !is.na(fifteen_bin_data$bin2) & 
                                                 !is.na(fifteen_bin_data$bin3) & 
                                                 !is.na(fifteen_bin_data$bin4) & !is.na(fifteen_bin_data$bin5) , ]
  
  fourth_method <- fifteen_bin_data_filter
  
  fourth_method$bin0 <- ( (fifteen_bin_data_filter$bin0 *0.05) + (fifteen_bin_data_filter$bin1 *0.05) + (fifteen_bin_data_filter$bin2 *0.05) ) / (0.15)
  
  fourth_method$bin1 <- ( (fifteen_bin_data_filter$bin3 *0.05) + (fifteen_bin_data_filter$bin4 *0.1)  ) / (0.15)
  
  fourth_method$bin2 <- ( (fifteen_bin_data_filter$bin5 *0.1) + (fifteen_bin_data_filter$bin6 *0.1)  ) / (0.15)
  
  fourth_method$bin3 <- ( (fifteen_bin_data_filter$bin7 *0.1) + (fifteen_bin_data_filter$bin8 *0.1)  ) / (0.25)
  
  fourth_method$bin4 <- ( (fifteen_bin_data_filter$bin9 *0.5) + (fifteen_bin_data_filter$bin10 *0.5)  + (fifteen_bin_data_filter$bin11 *0.5) ) / (1.5)
  
  fourth_method$bin5 <- ( (fifteen_bin_data_filter$bin12 *2.5) + (fifteen_bin_data_filter$bin13 *2.5)  + (fifteen_bin_data_filter$bin14 *2.5) ) / (7.5)
  
  sn[which(sn$date %in% fourth_method$date), c( "bin0", "bin1", "bin2", "bin3", "bin4", "bin5")] <- fourth_method[, c( "bin0", "bin1", "bin2", "bin3", "bin4", "bin5")]
  
  #getting rid of unused variables
  sn$bin6 <- NULL
  sn$bin7 <- NULL
  sn$bin8 <- NULL
  sn$bin9 <- NULL
  sn$bin10 <- NULL
  sn$bin11 <- NULL
  sn$bin12 <- NULL
  sn$bin13 <- NULL
  sn$bin14 <- NULL
  

  return(sn)
  
}