average_cpc <- function(cpc){
  
  cpc$date1min <- round_date(cpc$date, "1 min") #creating date1min vector. Turns 00:00:01 and 00:00:59 into 00:00 and 00:01, respectively.
  
  new_cpc <- cpc %>% 
    #select(c("date1min", "aveconc","concent","rawconc", "cnt_sec",    "condtmp",   "satttmp",   "satbtmp",   "optctmp",   "inlttmp" ,  "smpflow" ,  "satflow", "pressur",   "condpwr",   "sattpwr" ,  "satbpwr" ,  "optcpwr",   "satfpwr" ,  "fillcnt",  "err_num")) %>% 
    select(c("date1min", "concent", "fillcnt")) %>%
    dplyr::group_by(date1min) %>%  
    mutate_if(is.character,as.numeric) %>%  #converting strings to numeric
    dplyr::summarize(across(everything(), list(mean), na.rm= TRUE))
  
  return(new_cpc)
}


