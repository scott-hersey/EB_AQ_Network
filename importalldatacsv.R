importalldatacsv <- function(sn45, sn49, sn62, sn67, sn72){
  sn72 <- read.csv("sn72.csv")
  sn72$X <- NULL
  sn72$date <- ymd_hms(sn72$date)
  
  sn67 <- read.csv("sn67.csv")
  sn67$X <- NULL
  sn67$date <- ymd_hms(sn67$date)
  
  sn62 <- read.csv("sn62.csv")
  sn62$X <- NULL
  sn62$date <- ymd_hms(sn62$date)
  
  sn49 <- read.csv("sn49.csv")
  sn49$X <- NULL
  sn49$date <- ymd_hms(sn49$date)
  
  sn45 <- read.csv("sn45.csv")
  sn45$X <- NULL
  sn45$date <- ymd_hms(sn45$date)
}