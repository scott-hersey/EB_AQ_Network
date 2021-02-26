apply_flags <- function(sensor){
  # creating a flags column to flag high values 
  # flag number to pollutant definition given below:
  # co - 1 
  # co2 - 2
  # no2 - 3
  # bin0 - 4
  # pm1 - 5
  # no - 6
  
  sensor$flag <- replicate(nrow(sensor), c(), simplify = FALSE) #create flags column which has a list for every data point
  sensor$flag[which(sensor$co2 > 2000)] <- lapply(sensor$flag[which(sensor$co2 > 2000)], function(x) c(x, 2)) #add 2 to the flags data points with high CO2 values
  sensor$flag[which(sensor$no2 > 1000)] <- lapply(sensor$flag[which(sensor$no2 > 1000)], function(x) c(x, 3))
  sensor$flag[which(sensor$no > 300)] <- lapply(sensor$flag[which(sensor$no > 300)], function(x) c(x, 6))
  
  return(sensor)
}


check_flags <- function(sensor, flagval, var_name){
  #plots flagged intervals
  
  index_df = create_interval_vector(sensor, flagval) #generate dataframe of flagged intervals
  
  for(pair in 1:nrow(index_df)){   # for each interval
    start_indx = index_df[pair,1] #get the start index of the interval
    end_indx = index_df[pair,2] #get the end index
    
    if(!is.null(start_indx) & !is.null(end_indx) & length(is.na(start_indx)) != 0 & length(is.na(end_indx)) != 0 & length(difftime(sensor$date[end_indx], sensor$date[start_indx], units="mins") < 2)!=0){ #if the interval lasts less than 2 minutes, set to NA
      sensor[[var_name]][start_indx:end_indx] <- NA
    }
    
    else{ #otherwise plot it
      if(length(is.na(start_indx)) != 0 & length(is.na(end_indx)) != 0){
        sprintf("start and stop : %f, %f", start_indx, end_indx)
        subset_sensor <- sensor[start_indx:end_indx,]
        #cat(sprintf("start and end intervals: %s - %s \n", as.character(sensor$date[start_indx]), as.character(sensor$date[end_indx] )))
        timePlot(subset_sensor, pollutant = var_name, key = FALSE, main = paste0(as.character(sensor$date[start_indx]) ," to ", as.character(sensor$date[end_indx] )))
      }}}}

print_flag_intervals <- function(sensor, flagval){
  #plots flagged intervals
  
  index_df = create_interval_vector(sensor, flagval) #generate dataframe of flagged intervals
  if(nrow(index_df) == 0){
    print("No flagged data")
  }
  else{
    mod_start_indx <- index_df[,1]
    mod_end_indx <- index_df[,2]
    
    rows_to_keep <- which( difftime(sensor$date[mod_end_indx], sensor$date[mod_start_indx], units="mins") > 2)
    
    sprintf("Number of intervals %f", rows_to_keep)
  }
  
}

create_interval_vector <- function(sensor, flagval){
  # input a sensor dataframe and a flag (as numeric) to plot 
  # returns a dataframe that has intervals where the high values start and end 
  
  
  flagindxs <- ifelse(
    ( 
      (sensor$flag %like% flagval )
    ),
    1,  # if condition is met, put 1
    0   # else put 0
  )
  
  start_indx <- which(diff(c(0L, flagindxs)) == 1L)
  end_indx <- which(diff(c(flagindxs, 0L)) == -1L)
  
  mod_start_indx <- start_indx - 5 #adding 5/10 minutes before, depending on whether data is taken at 1 min or 2 min intervals
  mod_end_indx <- end_indx + 5 #adding 5/10 minutes after
  
  index_df <- data.frame(mod_start_indx,mod_end_indx) #create dataframe with corresponding start and end indices where high values happen
  
  
  #clean dataframe for intervals that start before 0
  if(any(index_df$mod_start_indx < 1)){
    
    index_df$mod_start_indx <- replace(index_df$mod_start_indx, which(index_df$mod_start_indx <1), 1 )
  }
  
  #clean dataframe for intervals that start after the end of the pollutant vector
  if(any(index_df$mod_start_indx > nrow(sensor))){
    
    indxs_pairs <- which(index_df$mod_start_indx > nrow(sensor))
    index_df <- index_df[!indxs_pairs]
  }
  
  #clean dataframe for intervals that end after the end of the pollutant vector
  if(any(index_df$mod_end_indx > nrow(sensor))){
    
    index_df$mod_end_indx <-replace(index_df$mod_end_indx, which(index_df$mod_end_indx > nrow(sensor)), nrow(sensor) )
  }
  
  #index_df <- index_df[!duplicated(index_df$mod_end_indx)]
  
  return(index_df)
  
}

remove_flags <- function(sensor, flag_val, pollutant){
  sensor[which(sensor$flag %like% flag_val), pollutant] <- NA
  return(sensor)
}