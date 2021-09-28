library(lubridate) #date and time functions 
library(dplyr) #this library allows you to use the %>% operator
library(tidyr) #this library lets you use the complete function to account for time syncing
library(ggplot2) #plotting


important_cols <- c ('bin0_ratio', 'bin0.indoor', 'bin0.outdoor',
                    'neph_bin0_ratio', 'neph_bin0.indoor', 'neph_bin0.outdoor',
                    'pm1_ratio', 'pm1.indoor', 'pm1.outdoor',
                    'pm25_ratio', 'pm25.indoor', 'pm25.outdoor',
                    'pm10_ratio', 'pm10.indoor', 'pm10.outdoor', 'HEPA_on')

simplify_ratio_data <- function(room, is_CPC=FALSE) {
  if(is_CPC) {
    room <- room %>%
      # Keep relevant columns
      select(date1min, 
             concent_ratio, concent_1.indoor, concent_1.outdoor,
             bin0_ratio, bin0.indoor, bin0.outdoor,
             neph_bin0_ratio, neph_bin0.indoor, neph_bin0.outdoor,
             pm1_ratio, pm1.indoor, pm1.outdoor,
             pm25_ratio, pm25.indoor, pm25.outdoor,
             pm10_ratio, pm10.indoor, pm10.outdoor, HEPA_on) 
      # drop_na(important_cols_CPC)
  }
  else {
    room <- room %>%
      # Keep relevant columns
      select(date1min,
             bin0_ratio, bin0.indoor, bin0.outdoor,
             neph_bin0_ratio, neph_bin0.indoor, neph_bin0.outdoor,
             pm1_ratio, pm1.indoor, pm1.outdoor,
             pm25_ratio, pm25.indoor, pm25.outdoor,
             pm10_ratio, pm10.indoor, pm10.outdoor, HEPA_on)
  }
  
  room <- room %>%
    drop_na(important_cols)
  
  room <- room %>%
    complete(date1min = seq(min(date1min, na.rm=TRUE), 
                                    max(date1min, na.rm=TRUE), 
                                    by = "min"))
  return(room)
}

tidy_data <- function(room) {
  tidy_room <- room %>%
    select(-HEPA_on)
  tidy_room <- tidy_room %>%
    pivot_longer(-date1min, names_to = "source", values_to = "ratio")
  
  HEPA_room <- room %>%
    select(date1min, HEPA_on)
  tidy_room <- tidy_room %>%
    left_join(HEPA_room)
  
  
  return(tidy_room)
}

filter_hours <- function(room, start_hour, end_hour, start_day, end_day) {
  room <- room %>%
    filter(hour(date1min) >= start_hour & hour(date1min) <= end_hour) %>%
    filter(wday(date1min) %in% start_day:end_hour)
  return(room)
}

calculate_mean_ratio <- function(room, is_CPC=FALSE) {
  
  # Calculate mean conc before and after for indoor, outdoor, and ratio
  # Calculate standard deviations for all as well
  bin0 <- room %>%
    filter(source=="bin0_ratio") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(bin0 = "mean(ratio, na.rm = TRUE)")
  bin0_sd <- room %>%
    filter(source=="bin0_ratio") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(bin0_sd = "sd(ratio, na.rm = TRUE)")
  bin0.indoor <- room %>%
    filter(source=="bin0.indoor") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(bin0.indoor = "mean(ratio, na.rm = TRUE)")
  bin0_sd.indoor <- room %>%
    filter(source=="bin0.indoor") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(bin0_sd.indoor = "sd(ratio, na.rm = TRUE)")
  bin0.outdoor <- room %>%
    filter(source=="bin0.outdoor") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(bin0.outdoor = "mean(ratio, na.rm = TRUE)")
  bin0_sd.outdoor <- room %>%
    filter(source=="bin0.outdoor") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(bin0_sd.outdoor = "sd(ratio, na.rm = TRUE)")
  
  neph_bin0 <- room %>%
    filter(source=="neph_bin0_ratio") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(neph_bin0 = "mean(ratio, na.rm = TRUE)")
  neph_bin0_sd <- room %>%
    filter(source=="neph_bin0_ratio") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(neph_bin0_sd = "sd(ratio, na.rm = TRUE)")
  neph_bin0.indoor <- room %>%
    filter(source=="neph_bin0.indoor") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(neph_bin0.indoor = "mean(ratio, na.rm = TRUE)")
  neph_bin0_sd.indoor <- room %>%
    filter(source=="neph_bin0.indoor") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(neph_bin0_sd.indoor = "sd(ratio, na.rm = TRUE)")
  neph_bin0.outdoor <- room %>%
    filter(source=="neph_bin0.outdoor") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(neph_bin0.outdoor = "mean(ratio, na.rm = TRUE)")
  neph_bin0_sd.outdoor <- room %>%
    filter(source=="neph_bin0.outdoor") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(neph_bin0_sd.outdoor = "sd(ratio, na.rm = TRUE)")
  
  
  pm1 <- room %>%
    filter(source=="pm1_ratio") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(pm1 = "mean(ratio, na.rm = TRUE)")
  pm1_sd <- room %>%
    filter(source=="pm1_ratio") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(pm1_sd = "sd(ratio, na.rm = TRUE)")
  pm1.indoor <- room %>%
    filter(source=="pm1.indoor") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(pm1.indoor = "mean(ratio, na.rm = TRUE)")
  pm1_sd.indoor <- room %>%
    filter(source=="pm1.indoor") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(pm1_sd.indoor = "sd(ratio, na.rm = TRUE)")
  pm1.outdoor <- room %>%
    filter(source=="pm1.outdoor") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(pm1.outdoor = "mean(ratio, na.rm = TRUE)")
  pm1_sd.outdoor <- room %>%
    filter(source=="pm1.outdoor") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(pm1_sd.outdoor = "sd(ratio, na.rm = TRUE)")
  
  
  pm25 <- room %>%
    filter(source=="pm25_ratio") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(pm25 = "mean(ratio, na.rm = TRUE)")
  pm25_sd <- room %>%
    filter(source=="pm25_ratio") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(pm25_sd = "sd(ratio, na.rm = TRUE)")
  pm25.indoor <- room %>%
    filter(source=="pm25.indoor") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(pm25.indoor = "mean(ratio, na.rm = TRUE)")
  pm25_sd.indoor <- room %>%
    filter(source=="pm25.indoor") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(pm25_sd.indoor = "sd(ratio, na.rm = TRUE)")
  pm25.outdoor <- room %>%
    filter(source=="pm25.outdoor") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(pm25.outdoor = "mean(ratio, na.rm = TRUE)")
  pm25_sd.outdoor <- room %>%
    filter(source=="pm25.outdoor") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(pm25_sd.outdoor = "sd(ratio, na.rm = TRUE)")
  
  
  pm10 <- room %>%
    filter(source=="pm10_ratio") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(pm10 = "mean(ratio, na.rm = TRUE)")
  pm10_sd <- room %>%
    filter(source=="pm10_ratio") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(pm10_sd = "sd(ratio, na.rm = TRUE)")
  pm10.indoor <- room %>%
    filter(source=="pm10.indoor") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(pm10.indoor = "mean(ratio, na.rm = TRUE)")
  pm10_sd.indoor <- room %>%
    filter(source=="pm10.indoor") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(pm10_sd.indoor = "sd(ratio, na.rm = TRUE)")
  pm10.outdoor <- room %>%
    filter(source=="pm10.outdoor") %>%
    summarize(mean(ratio, na.rm=TRUE)) %>%
    rename(pm10.outdoor = "mean(ratio, na.rm = TRUE)")
  pm10_sd.outdoor <- room %>%
    filter(source=="pm10.outdoor") %>%
    summarize(sd(ratio, na.rm=TRUE)) %>%
    rename(pm10_sd.outdoor = "sd(ratio, na.rm = TRUE)")
  
  if(is_CPC) {
    conc <- room %>%
      filter(source=="concent_ratio") %>%
      summarize(mean(ratio, na.rm=TRUE)) %>%
      rename(conc = "mean(ratio, na.rm = TRUE)")
    conc_sd <- room %>%
      filter(source=="concent_ratio") %>%
      summarize(sd(ratio, na.rm=TRUE)) %>%
      rename(conc_sd = "sd(ratio, na.rm = TRUE)")
    conc.indoor <- room %>%
      filter(source=="concent_1.indoor") %>%
      summarize(mean(ratio, na.rm=TRUE)) %>%
      rename(conc.indoor = "mean(ratio, na.rm = TRUE)")
    conc_sd.indoor <- room %>%
      filter(source=="concent_1.indoor") %>%
      summarize(sd(ratio, na.rm=TRUE)) %>%
      rename(conc_sd.indoor = "sd(ratio, na.rm = TRUE)")  
    conc.outdoor <- room %>%
      filter(source=="concent_1.outdoor") %>%
      summarize(mean(ratio, na.rm=TRUE)) %>%
      rename(conc.outdoor = "mean(ratio, na.rm = TRUE)")
    conc_sd.outdoor <- room %>%
      filter(source=="concent_1.outdoor") %>%
      summarize(sd(ratio, na.rm=TRUE)) %>%
      rename(conc_sd.outdoor = "sd(ratio, na.rm = TRUE)")
    
    # Connect into one dataframe
    mean_ratios = cbind(
      conc, conc_sd, conc.indoor, conc_sd.indoor, conc.outdoor, conc_sd.outdoor,
      bin0, bin0_sd, bin0.indoor, bin0_sd.indoor, bin0.outdoor, bin0_sd.outdoor,
      neph_bin0, neph_bin0_sd, neph_bin0.indoor, neph_bin0_sd.indoor, 
      neph_bin0.outdoor, neph_bin0_sd.outdoor, 
      pm1, pm1_sd, pm1.indoor, pm1_sd.indoor, pm1.outdoor, pm1_sd.outdoor,
      pm25, pm25_sd, pm25.indoor, pm25_sd.indoor, pm25.outdoor, pm25_sd.outdoor,
      pm10, pm10_sd, pm10.indoor, pm10_sd.indoor, pm10.outdoor, pm10_sd.outdoor
    )
  }
  
  else {
    mean_ratios = cbind(
      bin0, bin0_sd, bin0.indoor, bin0_sd.indoor, bin0.outdoor, bin0_sd.outdoor,
      neph_bin0, neph_bin0_sd, neph_bin0.indoor, neph_bin0_sd.indoor, 
      neph_bin0.outdoor, neph_bin0_sd.outdoor, 
      pm1, pm1_sd, pm1.indoor, pm1_sd.indoor, pm1.outdoor, pm1_sd.outdoor,
      pm25, pm25_sd, pm25.indoor, pm25_sd.indoor, pm25.outdoor, pm25_sd.outdoor,
      pm10, pm10_sd, pm10.indoor, pm10_sd.indoor, pm10.outdoor, pm10_sd.outdoor
    )
  }
  return(mean_ratios)
}

get_quartile <- function(room) {
  quartile <- room %>% 
    group_by(source, HEPA_on) %>%
      summarize(
        "5th_percentile"  = quantile(ratio, 0.05, na.rm = TRUE),
        "25th_percentile" = quantile(ratio, 0.25, na.rm = TRUE),
        "median"          = quantile(ratio, 0.50, na.rm = TRUE),
        "75th_percentile" = quantile(ratio, 0.75, na.rm = TRUE),
        "95th_percentile" = quantile(ratio, 0.95, na.rm = TRUE)
      ) %>%
    drop_na(HEPA_on)
  return(quartile)
}
  