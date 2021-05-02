hourlytally <- function(runwaystring){
  #input which runway as a string , ie "22L"
  #this function will return an hourly dataset that describes LTO 
  
  hourlyactivity <- flightdf %>% #call the flight dataset
    filter(RW == runwaystring) %>% #filter by runway
    mutate(hourly = format(Date, format="%Y-%m-%d %H")) %>% #create datetime object that shows the hour
    group_by(hourly , Opr) %>% #group by hour and operation
    summarise(count = n() ) #how many counts of each operation during that hour 
  
  
  hourlyactivity$regimeflag <- ifelse( #create a flag column that marks which regime the hour is in 
    # if hourly data is unique (no takeoffs AND landings) and Opr is A -> regime 3
    (
      (hourlyactivity$Opr %in% c("A")) &
        (duplicated(hourlyactivity$hourly)) == FALSE & ( duplicated(hourlyactivity$hourly, fromLast=TRUE) == FALSE  ) ),
    3,
    # if hourly data is unique (no takeoffs AND landings) and Opr is D -> regime 2
    ifelse(
      (
        ( hourlyactivity$Opr %in% c("D")) &
          (duplicated(hourlyactivity$hourly)) == FALSE & ( duplicated(hourlyactivity$hourly, fromLast=TRUE) == FALSE  ) ),
      2,
      
      5 )) #if it's neither regime 2 or 3, set the flag to 5
  
  return(hourlyactivity)
}