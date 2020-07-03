creategraph <- function(sn){
  #creates graphs for validation
  #input df must contain these columns : no2, rh_manifold, no, date, filtered
  
  #goes with filtering NO2, July 3 Rmd
  layout(matrix(c(1, 1, 1,1,
                  1, 1, 1,1, 
                  2, 2, 2, 2,
                  2, 2, 2, 2, 
                  3, 3, 3, 3,
                  3,3,3, 3), nrow=6, byrow=TRUE))
  
  plot(sn$date, sn$no, xlab="")
  
  
  plot(sn$date, sn$rh_manifold, xlab="")
  
  plot(sn$date, sn$no2, axes=F, xlab="", ylab="", type="b",col="black")
  axis(2, col="black")
  mtext("NO2 ",side=2,line=2.5)
  box()
  
  par(new=T)
  
  plot(sn$date, sn$filtered,xlab="", ylab="",  axes=F, type="l", col="red")
  mtext("Filtered",side=4,col="red",line=2.5)
  axis(4, col="red",col.axis="red")
  
  
  axis(2,pretty(range(time),10))
  mtext("Time",side=1,col="black",line=2.5)
}