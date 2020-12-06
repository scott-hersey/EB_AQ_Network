# negative value filter
negative_value_filter <- function(sensor){
  sensor$o3 <- replace(sensor$o3, sensor$o3 < 0, 0)
  sensor$co <- replace(sensor$co, sensor$co <0, 0)
  sensor$co2 <- replace(sensor$co2, sensor$co2 <0, 0)
  sensor$no2 <- replace(sensor$no2, sensor$no2 <0, 0)
  sensor$bin0 <- replace(sensor$bin0, sensor$bin0 <0, 0)
  sensor$pm1 <- replace(sensor$pm1, sensor$pm1 <0, 0)
  return(sensor)
}

# high value filter

## clean o3
o3_filter <- function(sensor){ #create o3 filter function
  sensor$o3 <- replace(sensor$o3, sensor$o3 > 300, NA)
  return(sensor)
}

## apply flags 
apply_flags <- function(sensor){
  # co - 1 
  # co2 - 2
  # no2 - 3
  # bin0 - 4
  # pm1 - 5
  # no - 6
  sensor$flag <- rep(nrow(sensor))
  sensor$flag[which(sensor$co2 > 2000)] <- 2
  sensor$flag[which(sensor$no2 > 100)] <- 3
  sensor$flag[which(sensor$no > 300)] <- 6
  return(sensor)
}

## inspect flags
check_flags <- function(sensor, flagval, var_name){
  
  index_df = create_interval_vector(sensor, flagval)
  for(pair in 1:nrow(index_df)){
    start_indx = index_df[i,0]
    end_indx = index_df[i,1]
    subset_sensor <- sensor[start_indx:end_indx,]
    polarPlot(subset_sensor, pollutant = var_name)
  }
}

create_interval_vector <- function(sensor, flagval){
  # input a sensor dataframe and a flag to plot 
  # return sensor 
  
  flagindxs <- ifelse(
    ( 
      (sensor$flag == flagval )
    ),
    1,  # if condition is met, put 1
    0   # else put 0
  )
  
  start_indx <- which(diff(c(0L, flagindxs)) == 1L)
  end_indx <- which(diff(c(flagindxs, 0L)) == -1L)
  
  mod_start_indx <- start_indx - 60*5
  mod_end_indx <- end_indx + 60*5
  
  index_df <- data.frame(mod_start_indx,mod_end_indx)
  
  
  if(any(index_df$mod_start_indx < 1)){
    
    index_df$mod_start_indx <- replace(index_df$mod_start_indx, which(index_df$mod_start_indx <1), 1 )
  }
  
  
  #filtering pairs that start after end 
  if(any(index_df$mod_start_indx > nrow(sensor))){
    #print(which(filtering$mod_start_indx <1))
    indxs_pairs <- which(index_df$mod_start_indx > nrow(sensor))
    index_df <- index_df[!indxs_pairs]
  }
  
  
  
  
  if(any(index_df$mod_end_indx > nrow(sensor))){
    #print(which(filtering$mod_end_indx > length(cpc_upload$fillcnt)))
    index_df$mod_end_indx <-replace(index_df$mod_end_indx, which(index_df$mod_end_indx > nrow(sensor)), nrow(sensor) )
  }

  index_df <- index_df[!duplicated(index_df$mod_end_indx)]
  
  return(index_df)
  
}