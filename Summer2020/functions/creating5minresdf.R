creating5minresdf <- function(sensorpath, asospath){
  
  ##This function imports the data, gets the sensor and met datasets ready to merge, sets the timezone and merges them so that there's 5 minute resolution
  #takes in path to sensor and met data
  #note: takes in old sensor data, taken from QuantAQ website 
  
  sensor = read.csv(sensorpath, header=TRUE, fileEncoding="UTF-8-BOM") #import sensor file
  asos = read.csv(asospath, header=TRUE, fileEncoding="UTF-8-BOM") #import meteorology file
  
  #getting our sensor data ready
  setnames(sensor, old = c('timestamp_local'), new = c('date')) #renaming
  sensor$date <- ymd_hms(sensor$date, tz="America/New_York") #parse datetime
  sensor <- sensor[,c("date", "temp_manifold", "no", "co2", "co", "pm1", "pm25", "pm10",  "o3")] #extracting relevant parameters, to increase speed
  
  sensor <- mutate(sensor, originaldate = date)
  sensor$date5min <- round_date(sensor$date, "5 mins") #rounds to 5 from halfway, ie 02:40 -> 0:00, 02:50 -> 05:00
  sensor<- aggregate(sensor[,names(sensor) != "date5min"], by = list(sensor$date5min), FUN = mean, na.rm = TRUE) #takes mean of everything based on date5min
  colnames(sensor)[1:2] <- c("date", "olddate") #renaming, for merging
  
  #getting meteorology data ready 
  setnames(asos, old = c("valid","drct", "sped"), new = c("date","wd", "ws")) #renaming
  Sys.setlocale("LC_TIME", "C")
  asos <- asos[,c("date", "ws", "wd")] #extracting the most relevant factors
  asos$ws <- as.numeric(asos$ws)
  asos$ws <- asos$ws * (1609/3600) #convert mph to m/s
  asos$date <- parse_date_time(asos$date, "%y/%m/%d %H:%M", tz="America/New_York", locale = "C") #ymd doesn't recognize its format, so use parse_datetime
  
  #merging
  finaldf <- merge(x = sensor, y = asos, by = "date", all = FALSE) #all=FALSE gets rid of datapoints if one of the two datasets don't have them
  
  #final editing of ASOS format
  finaldf$wd[finaldf$wd == "null"] <- NA
  finaldf$ws[finaldf$ws == "null"] <- NA
  finaldf$wd <- as.integer(finaldf$wd)   
  finaldf$ws <- as.integer(finaldf$ws)
  finaldf$olddate <- NULL
  return(finaldf)
}


