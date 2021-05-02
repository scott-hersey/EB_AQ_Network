highres_regimes <- function(sn, flightdf, runwaystring){
  # sn -> sensor data 
  # flightdf -> flight dataframe 
  # runwaystring -> string that denotes the runway in question, ie : "22L"
  
  # sn <- snfiles$sn45
  # runwaystring <- "22R"
  # flightdf <- flightdata
  #create start time and end time for each datapoint in flight df (+/- 90 s)
  flightdf$starttime <- flightdf$Date - 90
  flightdf$endtime <- flightdf$Date + 90
  
  # arrival and departure
  A_runway <- flightdf %>%
    filter(Opr == "A", RW == runwaystring)
  
  D_runway <- flightdf %>%
    filter(Opr == "D", RW == runwaystring)
  
  require(data.table) #for foverlaps function
  
  #preparing data for foverlaps function
  sn$date_local <- ymd_hms(sn$date_local, tz="America/New_York")
  A_runway$Date <- ymd_hms(A_runway$Date, tz="America/New_York")
  A_runway$starttime <- ymd_hms(A_runway$starttime, tz="America/New_York")
  A_runway$endtime <- ymd_hms(A_runway$endtime, tz="America/New_York")
  
  D_runway$Date <- ymd_hms(D_runway$Date, tz="America/New_York")
  D_runway$starttime <- ymd_hms(D_runway$starttime, tz="America/New_York")
  D_runway$endtime <- ymd_hms(D_runway$endtime, tz="America/New_York")
  
  setnames(A_runway, old="Date", new="date_local")
  
  
  setDT(A_runway) #makes df into datatable without wasting memory
  setDT(sn)
  sn[, dummy:=date_local]
  setkey(A_runway, starttime, endtime) #telling foverlaps to look for times between starttime and endtime
  reg2 <- foverlaps(sn, A_runway, by.x=c("date_local", "dummy"), type ="within", nomatch=0L)[, dummy := NULL][] #executing foverlaps for arrivals
  
  #doing the same for departures
  setnames(D_runway, old="Date", new="date_local")
  
  setDT(D_runway) #makes df into datatable without wasting memory
  setDT(sn)
  sn[, dummy:=date_local]
  setkey(D_runway, starttime, endtime)
  reg3 <- foverlaps(sn, D_runway, by.x=c("date_local", "dummy"), type ="within", nomatch=0L)[, dummy := NULL][]
  
  sn_reglist <- list("arrivals"= reg2, "departures" = reg3) #you can't return multiple objects from R function, so put them in one list
  return(sn_reglist)
}

