generate_comparison <- function(aeth_path, cpc_path, sn45_path, sn46_path){
  library(data.table)
  library(lubridate)
  library(tidyr)
  library(dplyr)
  library(openair)
  library(baseline)
  
  # -----------------------------------------------------------------------------------------
  #import and clean AETH 
  nm <- list.files(path=aeth_path, all.files = TRUE, full.names = TRUE, pattern="*.csv")
  
  aeth_data <- do.call(rbind, lapply(nm, function(x) fread(file=x) ) )
  
  aeth_data <- aeth_data[ , c( "Date / time local",  "Timezone offset (mins)" , "Date local (yyyy/MM/dd)", "Time local (hh:mm:ss)", "Status" ,  "UV BC1", "Blue BC1", "Green BC1", "Red BC1", "IR BC1")]
  
  ## formatting date time 
  
  aeth_data$date <- ymd_hms(aeth_data$`Date / time local`)
  tz(aeth_data$date) <- "America/New_York"
  
  # ----------------------------------------------------------------------------------------
  
  #import and clean CPC 
  nm <- list.files(path=cpc_path, all.files = TRUE, full.names = TRUE, pattern="*.TXT")
  
  cpc_upload <- do.call(rbind, lapply(nm, function(x) read.delim(file=x, header = TRUE, skip = 13) ) )
  
  ## removing unncessary variables 
  
  cpc_upload <- cpc_upload[ , c("X.YY.MM.DD", "HR.MN.SC", "concent", "fillcnt"  )]
  
  ##formatting datetime
  
  cpc_upload$date <- with(cpc_upload, ymd(`X.YY.MM.DD`) + hms(`HR.MN.SC`))
  
  tz(cpc_upload$date) <- "America/New_York"
  
  #hour(cpc_upload$date) <- hour(cpc_upload$date) + 8 
  
  cpc_upload$binary_fillcnt <- ifelse(
    ( 
      (cpc_upload$fillcnt != 0 )
    ),
    1,  # if condition is met, put 1
    0   # else put 0
  )
  
  start_indx <- which(diff(c(0L, cpc_upload$binary_fillcnt)) == 1L)
  #[1] 1 5 7
  end_indx <- which(diff(c(cpc_upload$binary_fillcnt, 0L)) == -1L)
  
  filtering_start_indx <- start_indx - 60*5
  filtering_end_indx <- end_indx + 60*5
  
  filtering <- data.frame(filtering_start_indx,filtering_end_indx   )
  
  if(any(filtering$filtering_start_indx < 1)){
    #print(which(filtering$filtering_start_indx <1))
    filtering$filtering_start_indx <- replace(filtering$filtering_start_indx, which(filtering$filtering_start_indx <1), 1 )
  }
  
  
  #filtering pairs that start after end 
  if(any(filtering$filtering_start_indx > length(cpc_upload$fillcnt))){
    #print(which(filtering$filtering_start_indx <1))
    indxs_pairs <- which(filtering$filtering_start_indx > length(cpc_upload$fillcnt))
    filtering <- filtering[!indxs_pairs]
  }
  
  
  
  
  if(any(filtering$filtering_end_indx > length(cpc_upload$fillcnt))){
    #print(which(filtering$filtering_end_indx > length(cpc_upload$fillcnt)))
    filtering$filtering_end_indx <-replace(filtering$filtering_end_indx, which(filtering$filtering_end_indx > length(cpc_upload$fillcnt)), length(cpc_upload$fillcnt) )
  }
  setDT(filtering)
  filtering <- filtering[!duplicated(filtering$filtering_end_indx)]
  
  
  remove_indxs <- unlist(Map(':',filtering$filtering_start_indx, filtering$filtering_end_indx))
  
  
  remove_indxs <- unique(remove_indxs)
  
  cpc_upload[remove_indxs, c("concent" )] <- NA 
  
  # ------------------------------------------------------------------
  
  # Import and clean SN45 and SN46
  
  sn45_cpc <- fread(sn45_path, header = TRUE)
  sn45_cpc$date <- ymd_hms(sn45_cpc$timestamp, tz="UTC")
  sn45_cpc$date <- with_tz(sn45_cpc$date, "America/New_York")
  sn46_cpc <- fread(sn46_path, header = TRUE)
  sn46_cpc$date <- ymd_hms(sn46_cpc$timestamp, tz="UTC")
  sn46_cpc$date <- with_tz(sn46_cpc$date, "America/New_York")
  
  # -----------------------------------------------------------------
  
  #time sync 
  
  time_sync <- function(sensor){
    sensor <- mutate(sensor, originaldate = date) #keeping original times for comparing to flight data
    sensor$date <- round_date(ymd_hms(sensor$date, tz="America/New_York"), unit="minute") #round date for merging
    sensor<- sensor[order(sensor$originaldate),] #put it in chronological order
    setDT(sensor, key = "date") #change object type to data.table
    sensor <- unique(sensor, by = "date") #remove duplicates
    
    return(sensor)
    
  }
  
  sn45_cpc <- time_sync(sn45_cpc)
  sn46_cpc <- time_sync(sn46_cpc)
  aeth_data <- time_sync(aeth_data)
  
  cpc_timesync <- cpc_upload[, c("concent", "fillcnt","date")]
  cpc_timesync$date1min <- round_date(cpc_timesync$date, "1 min")
  cpc_timesync_1min <- aggregate(cpc_timesync[, names(cpc_timesync) != c("date1min", "date")], by = list(cpc_timesync$date1min), FUN = mean, na.rm = TRUE)
  
  # --------------------------------------------------------------
  # merge 
  
  setDT(sn45_cpc, key = "date") # change to data.table object
  setDT(sn46_cpc, key = "date")
  setDT(cpc_timesync_1min, key = "Group.1")
  setDT(aeth_data, key = "date")
  
  sn45_sn46 <- sn45_cpc[sn46_cpc, nomatch = 0] #first join
  sn_cpc <- cpc_timesync_1min[sn45_sn46, nomatch = 0] #second join
  full_dtst <- sn_cpc[aeth_data, nomatch = 0]
  
  return(full_dtst)
}