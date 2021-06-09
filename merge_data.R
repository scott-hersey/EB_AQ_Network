library(lubridate) #date and time functions 
library(data.table) #to use the data.table variable type
library(dplyr) #this library allows you to use the %>% operator
library(tidyr) # use the complete function to account for time syncing
library(openair) #for plotting and analysis
library(stringr)


# Note: MCPC Data is usually .TXT
import_cpc_files <- function(filepath, regex_pattern="*.TXT",
                             skip_key="YY/MM/DD") {
  room_files <- list.files(
    path=filepath, 
    pattern=regex_pattern, 
    full.names = TRUE
    )

  room_cpc <- do.call(
    rbind, 
    lapply(room_files, 
           function(x) fread(file=x, header = TRUE, data.table = TRUE, 
                             skip=skip_key, fill=TRUE) 
           )
    )
  return(room_cpc)
}

# Change time zone to America/New York and format datetime correctly
set_correct_datetime <- function(room) {
  room$date <- with(room, ymd(`#YY/MM/DD`) + hms(`HR:MN:SC`)) #formatting datetime
  tz(room$date) <- "America/New_York" # define timezone
  return(room)
}

# Remove zeroes from CPC dataframes
remove_cpc_zero <- function(room) {
  room$concent[room$concent <= 20] <- NA
  return(room)
}

import_pm_files <- function(filepath, regex_pattern="*.csv") {
  room_pm <- do.call(cbind, 
          lapply(list.files(path=filepath, pattern=regex_pattern, 
                            full.names = TRUE), 
                 function(x) fread(file=x, header = TRUE)))
  return(room_pm)
}


#Clean the PM data
clean_PM_data <- function(sensor){
  indx = which(!duplicated(colnames(sensor))) #find duplicate columns
  sensor <- subset(sensor, select = indx) #remove duplicate columns from the pm files
  sensor$date <- ymd_hms(sensor$timestamp, tz="UTC") #parse datetime
  sensor$date <- with_tz(sensor$date, "America/New_York") #change the time according to shifted timezone
  sensor$original_date <- sensor$date
  sensor$date <- round_date(ymd_hms(sensor$date, tz="America/New_York"), unit="minute") #round date for merging
  sensor<- sensor[order(sensor$original_date),] #put it in chronological order
  sensor <- sensor[!duplicated(sensor$date), ]
  return(sensor)
}

# Average CPC data
average_cpc <- function(cpc){
  
  cpc$date1min <- round_date(cpc$date, "1 min") #creating date1min vector. Turns 00:00:01 and 00:00:59 into 00:00 and 00:01, respectively.
  
  new_cpc <- cpc %>% 
    select(c("date1min", "aveconc","concent","rawconc", "cnt_sec",    "condtmp",   "satttmp",   "satbtmp",   "optctmp",   "inlttmp" ,  "smpflow" ,  "satflow", "pressur",   "condpwr",   "sattpwr" ,  "satbpwr" ,  "optcpwr",   "satfpwr" ,  "fillcnt",  "err_num")) %>% 
    dplyr::group_by(date1min) %>%  
    mutate_if(is.character,as.numeric) %>%  #converting strings to numeric
    dplyr::summarize(across(everything(), list(mean), na.rm= TRUE))
  
  new_cpc <- new_cpc[!duplicated(new_cpc$date1min), ]
    
  return(new_cpc)
}



# Apply correction factor for CPC sensors
apply_correction <- function(sensor, correctionfactor){
  sensor$concent <- (sensor$concent) * correctionfactor
  return(sensor)
}

# Merge indoor and outdoor data together 
ratio_merge <- function(room_df, outdoor_df, cpc = TRUE){
  # if the dataset has CPC data
  if(cpc == TRUE){
    ratio_df <- merge(room_df, outdoor_df, by.x = "date1min", by.y = "date1min", all.x = TRUE, suffixes = c(".indoor", ".outdoor")) #merge the data from the room and the outdoors
    
    ratio_df$pm1_ratio <- ratio_df$pm1.indoor / ratio_df$pm1.outdoor #create a column that's a ratio of pm1's
    ratio_df$bin0_ratio <- ratio_df$bin0.indoor / ratio_df$bin0.outdoor #ratio of bin0
    ratio_df$neph_bin0_ratio <- ratio_df$neph_bin0.indoor / ratio_df$neph_bin0.outdoor #ratio of neph_bin0
    ratio_df$pm25_ratio <- ratio_df$pm25.indoor / ratio_df$pm25.outdoor #ratio of pm2.5
    ratio_df$pm10_ratio <- ratio_df$pm10.indoor / ratio_df$pm10.outdoor #ratio of pm10
    ratio_df$concent_ratio <- ratio_df$concent_1.indoor / ratio_df$concent_1.outdoor #create a column that's a ratio of cpc
  }
  
  #if the data doesn't have CPC data
  else{
    ratio_df <- merge(room_df, outdoor_df, by.x = "date", by.y = "date1min", all.x = TRUE, suffixes = c(".indoor", ".outdoor"))
    
    ratio_df$pm1_ratio <- ratio_df$pm1.indoor / ratio_df$pm1.outdoor #create a column that's a ratio of pm1's
    ratio_df$bin0_ratio <- ratio_df$bin0.indoor / ratio_df$bin0.outdoor #ratio of bin0
    ratio_df$neph_bin0_ratio <- ratio_df$neph_bin0.indoor / ratio_df$neph_bin0.outdoor #ratio of neph_bin0
    ratio_df$pm25_ratio <- ratio_df$pm25.indoor / ratio_df$pm25.outdoor #ratio of pm2.5
    ratio_df$pm10_ratio <- ratio_df$pm10.indoor / ratio_df$pm10.outdoor #ratio of pm10
    
    ratio_df <- ratio_df %>%
      rename(date1min = date)
  }
  return(ratio_df)
}