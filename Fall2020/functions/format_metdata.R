format_METtime <- function(metdata){
  metdata$date <- as.POSIXct(metdata$valid, format = "%Y-%m-%d %H:%M", tz = "America/New_York") #setting datetime, using correct timezone
  metdata <- metdata %>%
    setnames(old = c("drct", "sped"), new = c("wd", "ws")) %>% #rename
    complete(date = seq(from = min(date), to= max(date),  by = "1 min")) %>%#make 1 minute intervals
    fill(c("wd", "ws")) #fill those new 1 minute interval rows 
  
  metdata <- metdata[!grepl("null", metdata$ws),] #getting rid of met's null data
  metdata <- metdata[!grepl("null", metdata$wd),]
  metdata$wd <- as.numeric(metdata$wd)  #reformatting so it's integers instead of strings
  metdata$ws <- as.numeric(metdata$ws)
  metdata$ws <- metdata$ws * (1609/3600) #converting to m/s
  metdata[,c("tmpc", "valid", "station")] <- list(NULL) #getting rid of unnecessary variables
  
  return(metdata)
}