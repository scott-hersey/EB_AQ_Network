generic_import <- function(sn62){
  sn62 <- sn62[,c("timestamp",setdiff(names(sn62),"timestamp"))]
  print("here")
  sn62$timestamp <- ymd_hms(sn62$timestamp) 
  print("here")
  sn62 <- rename(sn62, ws = wind_speed, wd = wind_dir, date = timestamp) 
  print("here")
  
  sn62 <- dplyr::filter(sn62, between(sn62$wd, 0, 360))
  sn62 <-dplyr::filter(sn62, between(sn62$ws, 0, 30))
  sn62 <-dplyr::filter(sn62, between(sn62$pressure, 98000, 120000))
  sn62 <- dplyr::filter(sn62, between(sn62$temp_manifold, -10, 40))
  print("here")
  return(sn62)
}

