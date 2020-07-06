creategraph <- function(sn, date, plot1, plot2, plot3a, plot3b){
  #creates graphs for validation
  #input df must contain names of plot variables as sn45$date, etc
  
  #goes with filtering NO2, July 3 Rmd
  layout(matrix(c(1, 1, 1,1,
                  1, 1, 1,1, 
                  2, 2, 2, 2,
                  2, 2, 2, 2, 
                  3, 3, 3, 3,
                  3,3,3, 3), nrow=6, byrow=TRUE))
  
  plot(date, plot1, xlab="", type="l")
  
  
  plot(date, plot2, xlab="", type="l")
  
  plot(date, plot3a, axes=F, xlab="", ylab="", type="l",col="black")
  axis(2, col="black")
  mtext("NO2 ",side=2,line=2.5)
  box()
  
  par(new=T)
  
  plot(date, plot3b ,xlab="", ylab="",  axes=F, type="l", col="red")
  mtext("Filtered",side=4,col="red",line=2.5)
  axis(4, col="red",col.axis="red")
  
  
  axis(2,pretty(range(date),10))
  mtext("Time",side=1,col="black",line=2.5)
}