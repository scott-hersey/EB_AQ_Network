library(lubridate) #date and time functions 
library(data.table) #to use the data.table variable type
library(dplyr) #this library allows you to use the %>% operator
library(tidyr) #this library lets you use the complete function to account for time syncing
library(openair) #for plotting and analysis
library(readr)
library(ggplot2)
library(sjmisc)
library(plotly)


source("./Fall2020/helper functions/time_averaging.R")

#---------------------------
#Importing Files
sn45 <- fread("./Fall2020/data/cleaned_sn45.csv", header = TRUE)
sn45$date <- ymd_hms(sn45$date, tz= "America/New_York") #modifying datetime

nm <- list.files(path="./Fall2020/data/reference_data/MCPC data", pattern="*.TXT", full.names = TRUE)
cpc_upload <- do.call(rbind, lapply(nm, function(x) read.delim(file=x, header = TRUE, skip = 13) ) )
cpc_upload <- cpc_upload[ , c("X.YY.MM.DD", "HR.MN.SC", "concent", "fillcnt"  )]   ## removing unnecessary variables 
cpc_upload$date <- with(cpc_upload, ymd(`X.YY.MM.DD`) + hms(`HR.MN.SC`)) ##formatting datetime
tz(cpc_upload$date) <- "America/New_York" # define timezone
cpc_upload2 <- cpc_upload #to avoid mistakes

aeth_files <- list.files(path="./Fall2020/data/reference_data/AETHLOMETER", all.files = TRUE, full.names = TRUE, pattern="*.csv")
aeth_data <- do.call(rbind, lapply(aeth_files, function(x) fread(file=x) ) )
aeth_data <- aeth_data[ , c( "Date / time local",  "Timezone offset (mins)" , "Date local (yyyy/MM/dd)", "Time local (hh:mm:ss)", "Status" ,  "UV BC1", "Blue BC1", "Green BC1", "Red BC1", "IR BC1")]
aeth_data$date <- ymd_hms(aeth_data$`Date / time local`)   ## formatting date time 
tz(aeth_data$date) <- "America/New_York"

#changing sn45 date 
length(unique(aeth_data$`Timezone offset (mins)`)) == 1 #checking that I can just call the first offset
sn45$date <- sn45$date + (aeth_data$`Timezone offset (mins)`[1] *60)
#---------------------------
#Cleaning Data

#create list for cleaning
cpc_aeth = list("cpc" = cpc_upload2, "aeth" = aeth_data)

#checking data type
sapply(cpc_aeth, function(x) sapply(x,typeof))

#removing negative and zero values 
sprintf("Number of negative and zero values in cpc: %d", sum(cpc_upload2$concent <= 0, na.rm = TRUE))
lapply(c( "UV BC1", "Blue BC1", "Green BC1", "Red BC1", "IR BC1"), function(x) sprintf("Number of negative and zero values in aethlometer data, %s : %d",x, sum(aeth_data[[x]] <= 0, na.rm = TRUE)))

sprintf("Percent of negative and zero values in cpc: %f", 100*sum(cpc_upload2$concent <= 0, na.rm = TRUE)/ nrow(cpc_upload2))
lapply(c( "UV BC1", "Blue BC1", "Green BC1", "Red BC1", "IR BC1"), function(x) sprintf("Percent of negative and zero values in aethlometer data, %s : %f",x, 100*sum(aeth_data[[x]] <= 0, na.rm = TRUE)/ nrow(aeth_data[[x]])))


cpc_upload2$concent[which(cpc_upload2$concent <= 0)] <- NA

bc <- list("UV BC1", "Blue BC1" ,  "Green BC1" , "Red BC1" , "IR BC1" )   
for(col in bc){
  aeth_data[[col]] <- replace(aeth_data[[col]], aeth_data[[col]] <= 0, NA)
}


#inspecting NA values
sprintf("Number of NA values in cpc: %d", sum(is.na(cpc_upload2$concent)))
sprintf("Percentage of NA values in cpc: %f", 100*sum(is.na(cpc_upload2$concent))/ nrow(cpc_upload2))


#---------------------------
#Format for plotting

#create subset of data 
#sn45_subset <- sn45[date %between% c("2020-10-15 00:00:00 EDT", "2020-10-15 23:00:00 EDT")]
aeth_subset  <- aeth_data[date %between% c("2020-10-15 00:00:00 EDT", "2020-10-15 23:00:00 EDT")]
#cpc_subset <-  cpc_upload2[date %between% c("2020-10-15 12:00:00 EDT", "2020-10-15 16:00:00 EDT")]
cpc_subset <- subset(cpc_upload2, date > "2020-10-15 00:00:00 EDT" & date < "2020-10-15 23:00:00 EDT")


#---------------------------
#Plot

#generate list of line colors 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# creating shorter sn45 dataframe 
sn45_subset <- sn45[date %between% c(aeth_data$date[1],tail(aeth_data$date, n=1))]

#create sn45 and BC plot
no_aeth <- ggplot() + 
  #plot sn45 data
  geom_line(data=sn45_subset, aes(x=date, y=no), color=cbPalette[1]) + 
  #plot BC data
  geom_line(data=aeth_data, aes(x=date, y=`Blue BC1`/100), color=cbPalette[2]) +
  geom_line(data=aeth_data, aes(x=date, y=`Red BC1`/100), color=cbPalette[3]) +
  geom_line(data=aeth_data, aes(x=date, y=`Green BC1`/100), color=cbPalette[4]) +
  geom_line(data=aeth_data, aes(x=date, y=`IR BC1`/100), color=cbPalette[5]) +
  
  scale_x_datetime("Date", date_labels = "%b %d %I %p")+
  
  #add axes
  scale_y_continuous(
    # Features of the first axis
    name = "NO (ppb)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*100, name="BC (nanograms / m3)")
  )


#ggplotly(no_aeth)

#create cpc and BC plot
cpc_aeth <- ggplot() + 
  #plot cpc data
  geom_line(data=cpc_upload2, aes(x=date, y=concent), color=cbPalette[1]) + 
  #plot BC data
  geom_line(data=aeth_data, aes(x=date, y=100*`Blue BC1`), color=cbPalette[2]) +
  geom_line(data=aeth_data, aes(x=date, y=100*`Red BC1`), color=cbPalette[3]) +
  geom_line(data=aeth_data, aes(x=date, y=100*`Green BC1`), color=cbPalette[4]) +
  geom_line(data=aeth_data, aes(x=date, y=100*`IR BC1`), color=cbPalette[5]) +
  
  #add axes
  scale_y_continuous(
    # Features of the first axis
    name = "CPC concentration",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*100, name="BC")
  )


ggplotly(cpc_aeth)

#----------------------------------------------------------------
#Time averaging 

#save cpc as a csv
#write.csv(cpc_upload2, file = "concatinated_cpc.csv",  row.names = FALSE)

#average cpc to 1 minute
time_averaged_cpc <- average_cpc(cpc_upload2)
#round pm to the nearest minute 
aeth_data$original_date <- aeth_data$date
aeth_data$date <- round_date(ymd_hms(aeth_data$date, tz="America/New_York"), unit="minute") #round date for merging
aeth_data<- aeth_data[order(aeth_data$original_date),] #put it in chronological order
#merge with sn45 and aethlometer data 
aeth_cpc <- merge(aeth_data, time_averaged_cpc, by.x = "date", by.y = "date1min", all = FALSE, suffixes = c(".BC",".cpc"))
merged_df <- merge(aeth_cpc, sn45_subset, by = "date", all = FALSE)

#----------------------------------------------------------------
#Plot time averaging

cpc_aeth <- ggplot() + 
  #plot cpc data
  geom_line(data=merged_df, aes(x=date, y=concent_1), color=cbPalette[1]) + 
  #plot BC data
  geom_line(data=merged_df, aes(x=date, y=50*`Blue BC1`), color=cbPalette[2]) +
  geom_line(data=merged_df, aes(x=date, y=50*`Red BC1`), color=cbPalette[3]) +
  geom_line(data=merged_df, aes(x=date, y=50*`Green BC1`), color=cbPalette[4]) +
  geom_line(data=merged_df, aes(x=date, y=50*`IR BC1`), color=cbPalette[5]) +
  
  #add axes
  scale_y_continuous(
    # Features of the first axis
    name = "CPC concentration",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*50, name="BC")
  )


ggplotly(cpc_aeth)

#----------------------------------------------------------------
# Export merged data 
write.csv(merged_df, file = "merged_data_full.csv",  row.names = FALSE)

merged_subset <- merged_df[date %between% c("2020-10-15 11:44:00 EDT", "2020-10-15 23:00:00 EDT")]
write.csv(merged_df, file = "merged_data_Oct15.csv",  row.names = FALSE)

write.csv(time_averaged_cpc, file = "cpc_time_averaged.csv",  row.names = FALSE)

#----------------------------------------------------------------
# Export merged data 

cpc_comparison <- ggplot() + 
  #plot cpc data
  geom_line(data=time_averaged_cpc, aes(x=date1min, y=concent_1), color=cbPalette[1]) + 
  #plot BC data
  geom_line(data=cpc_upload2, aes(x=date, y=concent), color=cbPalette[2]) 
  
  #add axes
  scale_y_continuous(
    # Features of the first axis
    name = "CPC concentration",
    # Add a second axis and specify its features
    sec.axis = dup_axis( name="CPC original")
  )


ggplotly(cpc_comparison)
