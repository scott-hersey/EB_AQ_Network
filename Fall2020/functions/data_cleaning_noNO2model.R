final_data_cleaning_sansNO2 <- function(sensor, metdata){
  
  # CALLABLE FUNCTION VERSION ON final-data-cleaning.Rmd algorithm

  ## PREPARE DATES and DATAFRAMES
  
  sensor$date <- ymd_hms(sensor$timestamp, tz="UTC") #parse datetime
  sensor$date <- with_tz(sensor$date, "America/New_York")
  sensor[, c("X1","X", "sn" )] <- list(NULL) #removing unused variables
  sensor <- mutate(sensor, originaldate = date) #keeping original times for comparing to flight data
  sensor$date <- round_date(ymd_hms(sensor$date, tz="America/New_York"), unit="minute") #round date for merging
  sensor<- sensor[order(sensor$originaldate),] #put it in chronological order
  setDT(sensor, key = "date") #change object type to data.table
  sensor <- unique(sensor, by = "date") #remove duplicates
  
  
  
  sensor <- metdata[sensor, nomatch = 0] #second join
  
  sensor$o3 <- replace(sensor$o3, sensor$o3 < 0, 0) #filter for O3
  
  # NO AE Filter 
  
  #create a vector that shows the derivatives 
  no_ae_derivative <- c(0,diff(sensor$no_ae, na.rm = TRUE))
  #creating logical vector to pick which things to get rid of 
  logical_vec <- abs(no_ae_derivative) < abs(2.5*(sd(no_ae_derivative, na.rm=TRUE)))
  
  #make sure SN NO values that == 0 are not removed
  if (any(sensor$no == 0, na.rm = TRUE)){
    sensor$no <- replace(sensor$no, 0, 0.00001)
  }
  # if logical_vec is true, then set the NO value to NA 
  sensor$no <- sensor$no * logical_vec
  sensor$no[sensor$no == 0] <- NA
  
  # NO BASELINE CORRECTION 
  
  tz(sensor$date) <- "America/New_York"
  # create day column
  sensor[, day := as.Date(date, format="%Y-%m-%d", tz = "America/New_York")]
  # create corrected column 
  sensor[, correctedNO := seq(0,0,length.out= length(sensor$no))]
  sensor$correctedNO[sensor$correctedNO == 0] <- NA  #set them actually to NA
  
  dropNAsensor<- sensor[!is.na(sensor$no), ] # drop NO NAs
  
  unique_days <- c(unique(dropNAsensor$day, na.rm=TRUE)) #get all of the unique days in the sensor
  
  for (i in 2:(length(unique_days)-1)){ #for all days
    temp <- subset(dropNAsensor, day %in% unique_days[i], c("day", "no", "date")) #create temp dataset
    
    if (nrow(temp) > 450){
      wholebase.peakDetection <- baseline(t(temp$no), method='peakDetection',left=50, right=50, lwin=10, rwin=10) #baseline correction
      
      #replace the correctedNO column values with the baseline correction from earlier
      dropNAsensor$correctedNO[which(dropNAsensor$date == temp$date[1]): which(dropNAsensor$date == tail(temp$date, n=1))] <-    c(getCorrected(wholebase.peakDetection))
    }
    
    else{
      if ( (sum(temp$no < 0, na.rm = TRUE) / nrow(temp)) < 0.25){
        dropNAsensor$correctedNO[which(dropNAsensor$date == temp$date[1]): which(dropNAsensor$date == tail(temp$date, n=1))] <-    
          sensor$no[which(sensor$date == temp$date[1]): which(sensor$date == tail(temp$date, n=1))]
      }
      
    }
    
  }
  
  sensor$correctedNO[which(sensor$date %in% dropNAsensor$date)] <- dropNAsensor$correctedNO # replace values based on date 
  
  return(sensor)
}