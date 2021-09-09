
map_sensor <- function(dataset, sensorname, measure_vars){
  #have each datapoint (row of data) say which sensor it was taken from
  dataset$sn <- sensorname
  return(dataset)
}

melt_dataframes <- function(dataset, measure_vars){
  #"melt" a dataframe into long format (ie columns will be regime, sn, var and value)
  dataset.melted <- melt.data.table(dataset, id.vars = c("regime", "sn"), measure.vars = measure_vars )
  return(dataset.melted)
}


transform_data <- function(snfiles, input_measure_vars, removeSN72 = TRUE){
  #mapping, melting and then merging the data into one dataframe, so that making boxplots is easier
  
  #map each sensor
  snfiles$sn45 <- map_sensor(snfiles$sn45, "sn45")
  snfiles$sn46 <- map_sensor(snfiles$sn46, "sn46")
  snfiles$sn49 <- map_sensor(snfiles$sn49, "sn49")
  snfiles$sn62 <- map_sensor(snfiles$sn62, "sn62")
  snfiles$sn67 <- map_sensor(snfiles$sn67, "sn67")
  snfiles$sn72 <- map_sensor(snfiles$sn72, "sn72")
  
  if(removeSN72 == TRUE){
    # remove sn72 for now 
    snfiles <- snfiles[-6]
    
  }
 
  #melt each dataframe
  snfiles.melted <- lapply(snfiles, function(x) melt_dataframes(x, measure_vars = input_measure_vars))
  
  #combine into one dataset
  merged_df <- do.call("rbind", snfiles.melted) 
  
  #combine the regime and sensor into one column (ie sn45 upwind)
  merged_df$group <- paste(merged_df$sn, merged_df$regime)
  
  #filter out data that is NA (ie a datapoint that doesn't fit into any regime)
  merged_df1 <- merged_df %>% filter(!is.na(group))
  merged_df1 <- merged_df1 %>% filter(!is.na(regime))
  
  return(merged_df1)
  
  }
