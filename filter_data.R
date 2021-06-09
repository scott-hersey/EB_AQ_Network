library(readr) #read .csv files
library(ggplot2) #plotting
library(gtable) #create grids of graphs
library(grid) #create grids of graphs
library(tidyr)

# import HEPA data while ensuring proper numeric type on load
# is_CPC: whether file contains CPC data ("concent" columns)
time_to_eastern <- function(room) {
  tz(room$date1min) <- "America/New_York"
  return(room)
}

import_ratio_data <- function(csv_name, is_CPC=FALSE) {
  if (is_CPC) {
    room_ratio <- read_csv(csv_name, col_types = cols(
      concent_ratio = col_double(),
      concent_1.indoor = col_double(),
      concent_1.outdoor = col_double(),
      bin0_ratio = col_double(),
      bin0.indoor = col_double(),
      bin0.outdoor = col_double(),
      neph_bin0.indoor = col_double(),
      neph_bin0.indoor = col_double(),
      neph_bin0.outdoor = col_double(),
      pm1_ratio = col_double(),
      pm1.indoor = col_double(),
      pm1.outdoor = col_double(),
      pm25_ratio = col_double(),
      pm25.indoor = col_double(),
      pm25.outdoor = col_double(),
      pm10_ratio = col_double(),
      pm10.indoor = col_double(),
      pm10.outdoor = col_double()
    ))
  } else {
    room_ratio <- read_csv(csv_name, col_types = cols(
      bin0_ratio = col_double(),
      bin0.indoor = col_double(),
      bin0.outdoor = col_double(),
      neph_bin0.indoor = col_double(),
      neph_bin0.indoor = col_double(),
      neph_bin0.outdoor = col_double(),
      pm1_ratio = col_double(),
      pm1.indoor = col_double(),
      pm1.outdoor = col_double(),
      pm25_ratio = col_double(),
      pm25.indoor = col_double(),
      pm25.outdoor = col_double(),
      pm10_ratio = col_double(),
      pm10.indoor = col_double(),
      pm10.outdoor = col_double()
    ))
  }
  room_ratio <- time_to_eastern(room_ratio)
  return(room_ratio)
}

# plot indoor and outdoor values for different measurements
plot_time_resolved <- function(room, is_CPC=FALSE, temp=FALSE) {
  
  p1 <- ggplot(data=room, aes(x=date1min, y=bin0.indoor)) +
    geom_line(color="blue") +
    geom_line(aes(y=bin0.outdoor))

  p2 <- ggplot(data=room, aes(x=date1min, y=neph_bin0.indoor)) +
    geom_line(color="blue") +
    geom_line(aes(y=neph_bin0.outdoor))

  p3 <- ggplot(data=room, aes(x=date1min, y=pm1.indoor)) +
    geom_line(color="blue") +
    geom_line(aes(y=pm1.outdoor))

  p4 <- ggplot(data=room, aes(x=date1min, y=pm25.indoor)) +
    geom_line(color="blue") +
    geom_line(aes(y=pm25.outdoor))

  p5 <- ggplot(data=room, aes(x=date1min, y=pm10.indoor)) +
    geom_line(color="blue") +
    geom_line(aes(y=pm10.outdoor))

  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  g4 <- ggplotGrob(p4)
  g5 <- ggplotGrob(p5)
  g <- rbind(g1, g2, size = "first")
  g <- rbind(g, g3, size = "first")
  g <- rbind(g, g4, size = "first")
  g <- rbind(g, g5, size = "first")
  g$widths <- unit.pmax(g$widths, g2$widths)
  
  if (is_CPC) {
    p6 <- ggplot(data=room, aes(x=date1min, y=concent_1.indoor)) +
      geom_line(color="blue") +
      geom_line(aes(y=concent_1.outdoor))
    g6 <- ggplotGrob(p6)
    g <- rbind(g, g6, size = "first")
  }
  if(temp) {
    p7 <- ggplot(data=room, aes(x=date1min, y=sample_temp.indoor)) +
      geom_line(color="blue") +
      geom_line(aes(y=sample_temp.outdoor))
    g7 <- ggplotGrob(p7)
    g <- rbind(g, g7, size = "first")
    p8 <- ggplot(data=room, aes(x=date1min, y=sample_rh.indoor)) +
      geom_line(color="blue") +
      geom_line(aes(y=sample_rh.outdoor))
    g8 <- ggplotGrob(p8)
    g <- rbind(g, g8, size = "first")
  }
  
  grid.newpage()
  grid.draw(g)
}

set_NA <- function(df, start, end) {
  df <- df[!(df$date1min > as.POSIXct(start, tz='America/New_York') &
             df$date1min < as.POSIXct(end, tz='America/New_York')), ]
  
  df <- complete(df, date1min = seq(min(date1min, na.rm=TRUE), 
                                    max(date1min, na.rm=TRUE), 
                                    by = "min"))
  return(df)
}

# remove_indoor_sources <- function(room) {
#   
# }