gen_regime <- function(sn, flightdf, runwaystring, wd1, wd2){
  # sn -> sensor data 
  # flightdf -> flight dataframe 
  # runwaystring -> string that denotes the runway in question, ie : "22L"
  # wd1 -> wind direction 1 , the lower bound for your wind direction window 
  # wd2 -> wind direction 2, the upper bound for your wind direction window
  
  #preparing dataframe and iteration variables 
  temp_runway <- flightdf %>%   filter(RW == runwaystring) # extracting flights from just the right runway
  curr_reg <- temp_runway$Opr[1] #current regime
  intervals_list <- list() #list of intervals
  opr_list <- list() #list of corresponding intervals
  curr_start <- temp_runway$Date[1] #current time
  i <- 1 # counter
  
  #this for loop goes through each flight, and determines whether to create a new time range or keep going 
  for (flightindex in 2:length(temp_runway$Date)){ #for each of the flights, starting at the second
    #first var to check : time 
    if (difftime(temp_runway[flightindex, "Date"], temp_runway[flightindex - 1, "Date"], units="mins")  <  30){
      #second check : operation 
      
      #first check if operation is the same 
      if (temp_runway[flightindex, "Opr"] == curr_reg){ # if it's the same regime, keep going
        next
      } 
      #if it's not, but it's only one plane making a difference 
      else{
        if(temp_runway[flightindex+1, "Opr"] == curr_reg | temp_runway[flightindex+2, "Opr"] == curr_reg){ #if there's only one, two or three of that given opr
          next #keep going
        }
        
        else{ #otherwise, record this time range and start recording the next one
          intervals_list<- c(intervals_list, list(interval(start=curr_start, end=temp_runway[flightindex-1, "Date"], tz = "America/New_York"))) # record what the interval is
          opr_list <- c(opr_list, curr_reg) #record which operation this interval corresponds to 
          #update variables
          i <- i+ 1  
          curr_start <- temp_runway[flightindex , "Date"]
          curr_reg <- temp_runway[flightindex , "Opr"] 
        }
      } 
    }
    
    else{ #if there's a gap in time
      intervals_list<- c(intervals_list, list(interval(start=curr_start, end=temp_runway[flightindex-1, "Date"], tz = "America/New_York"))) # record what the interval is
      opr_list <- c(opr_list, curr_reg) #record which operation this interval corresponds to  
      intervals_list<- c(intervals_list, list(interval(start=temp_runway[flightindex-1, "Date"], end=temp_runway[flightindex, "Date"], tz = "America/New_York"))) # record what the interval is
      opr_list <- c(opr_list, "Other") #record which operation this interval corresponds to 
      #update variables
      i <- i+ 1  
      curr_start <- temp_runway[flightindex , "Date"]
      curr_reg <- temp_runway[flightindex , "Opr"] 
    }
  }
  
  #gather the data from the for loop, and split it into the right categories
  (intervals <-  do.call(rbind, Map(data.frame, intervals_list=intervals_list, Opr=opr_list))) #put the interval and operation lists together
  print(intervals)
  intervals_arrival <- filter(intervals, Opr == "A") #filter time ranges by arrivals
  intervals_departures <- filter(intervals, Opr == "D") #same for departures
  intervals_other <- filter(intervals, Opr == "Other") #same for departures
  
  #create df for arrival intervals
  sn_arrivals <- sn[which(sn45$date %within% intervals_arrival$intervals_list[1]) , ] #extracting all the sensor data in the first interval
  for (indx in 2:length(intervals_arrival$intervals_list)){
    sn_arrivals <- rbind(sn_arrivals, sn[which(sn$date %within% intervals_arrival$intervals_list[indx]) , ]) 
  }
  
  #create df for departure intervals
  sn_departures <- sn[which(sn$date %within% intervals_departures$intervals_list[1] ), ]
  for (indx in 2:length(intervals_departures$intervals_list)){
    sn_departures <- rbind(sn_departures, sn[which(sn$date %within% intervals_departures$intervals_list[indx]) , ]) 
  }
  
  #create intermediate df for all other datapoints (not arrival or departure)
  sn_other <- sn[which(sn$date %within% intervals_other$intervals_list[1] ), ]
  for (indx in 2:length(intervals_other$intervals_list)){
    sn_other <- rbind(sn_other, sn[which(sn$date %within% intervals_other$intervals_list[indx] ), ]) 
  }
  
  #filter the remaining datapoints based on wind direction 
  sn_reg1 <- filter(sn_other, wd > wd1 & wd < wd2)
  sn_reg4 <- setdiff(sn_other, sn_reg1)
  return(list("reg1" = sn_reg1, "reg2" = sn_departures, "reg3" = sn_arrivals, "reg4" = sn_reg4))
}


gen_regime_norunway <- function(sn, wd1, wd2){
  # This function filters a sensor dataframe solely based on wind direction 
  # if a datapoint is between two wind directions, it's in one dataframe; else, it's in the other
  sn_reg1 <- filter(sn, wd > wd1 & wd < wd2)
  sn_reg2 <- setdiff(sn, sn_reg1)
  return(list("reg1" = sn_reg1, "reg2" = sn_reg2))
}