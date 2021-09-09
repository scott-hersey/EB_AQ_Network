library(lubridate)
library(data.table)

format_dates <- function(sensor){
  sensor$date <- ymd_hms(sensor$date, tz = "UTC")
  sensor$date_local <- ymd_hms(sensor$date_local, tz = "America/New_York")
  
  return(sensor)
}



import_data <- function(){
  
  #importing snfiles 
  temp2 = list.files(path = "./data/joined/", pattern = "*.csv", full.names = TRUE) #specify files we're looking for
  snfiles = sapply(temp2, function(x) fread(file=x, data.table = TRUE) , simplify = FALSE,USE.NAMES = TRUE) #import them in
  names(snfiles) <- lapply(names(snfiles), function(x) substr(x, 3, 6)) #change the names
  
  #reformatting the dates 
  snfiles <- lapply(snfiles, function(x) format_dates(x))
  
  return(snfiles)
}