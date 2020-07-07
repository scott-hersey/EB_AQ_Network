createsensordf <- function(sensorpath, no2modelpath, metdf){
  #the sensor path is the name of the sensor data 
  #no2modelpath is the name of the no2model data 
  
  sensor = read.csv(sensorpath, header=TRUE, fileEncoding="UTF-8-BOM") #import sensor file
  #getting our sensor data ready
  setnames(sensor, old = c('timestamp_local'), new = c('date')) #renaming
  sensor$date <- ymd_hms(sensor$date, tz="America/New_York") #parse datetime
  sensor[, c("lat", "long", "X", "X.1")] <- list(NULL) #removing columns for speed
  sensor <- mutate(sensor, originaldate = date) #keeping original times for comparing to flight data
  sensor$date <- round_date(sensor$date, unit="minute")
  
  #merging
  finaldf <- merge(x = sensor, y = metdf, by = "date", all = FALSE) #all=FALSE gets rid of datapoints if one of the two datasets don't have them
  
  no2model <- read.csv(no2modelpath) 
  # the timestamp is given as seconds from some origin, so I'll deduce the origin
  no2model$date <-  as_datetime(no2model$timestamp , origin = (sensor$originaldate[1] - no2model$timestamp[1])) 
  no2model$date <- round_date(no2model$date, unit = "minute")   #to merge with finaldf
  no2model$timestamp <- NULL #excluding this variable
  
  names(no2model)[names(no2model)=="no2"] <- "no2model"#renaming, for the merge
  
  finaldf <- merge(x = finaldf, y = no2model, by = "date", all = FALSE)
  
  return(finaldf)
  
}