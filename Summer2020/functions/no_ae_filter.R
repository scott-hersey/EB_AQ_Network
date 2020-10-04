no_ae_filter <- function(sn, mag){
  #create a vector that shows the derivatives 
  no_ae_derivative <- c(0,diff(sn$no_ae, na.rm = TRUE))
  #creating logical vector to pick which things to get rid of 
  logical_vec <- abs(no_ae_derivative) < abs(mag*(sd(no_ae_derivative, na.rm=TRUE)))
  
  # if logical_vec is true, then set the NO value to NA 
  sn$no <- sn$no * logical_vec
  replace(sn$no, 0, NA)
  
  return(sn)
  
} 