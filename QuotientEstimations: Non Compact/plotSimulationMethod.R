
plot.simulation <- function(object){
  #print("Calling the plot.simulation function...")
  n <-object$n
  y0 <-object$y0
  phi0 <- object$phi0
  Y0 <- c((phi0+conv)*cos(y0),(phi0+conv)*sin(y0))
  Data <-object$Data
  poses <- object$poses
  
  #Main plot
  emptyplot(xlim=c(-1,2),ylim=c(-1,2),asp=1,frame.plot=FALSE)
  #Add title
  title=paste('Simulation for n=',n)
  title(main=title,cex.main=0.8)
    #Plot axis
  plotcircle(r=conv,lcol="black",lwd=2)
  
 axis(side=1, at=c(-1,0,1,2),pos=c(0,0),outer=FALSE, 
       cex.axis=0.7,tck=0.01,xpd = NA)
  #axis(side=2,at=c(-1,0,1,2), pos=c(0,0),outer=FALSE, 
       #cex.axis=0.7,las=2,tck=0.01,xpd = TRUE)
  
  #Plot Data
  points(Data$X1,Data$X2,xlim=c(-1,2),ylim=c(-1,2),
         col="darkgrey",pch=19, xaxt="n",yaxt="n") #axis = FALSE 
  
  # Plot real mean in green on quotient
  points(conv*cos(y0),conv*sin(y0), col="green",lwd=6, type="p",pch=19)
  plotcircle(r=conv, from=0, to=y0, lcol="green",lwd=2)
  
  # Plot real mean in green on R^2 
  points(Y0[1],Y0[2], col="green",lwd=6, type="p",pch=19)
 
  # Plot orbit and Y0 on orbit
 segments(x0 = conv*cos(y0), x1 = Y0[1],y0 = conv*sin(y0), y1 = Y0[2], col = "green",lwd=4)
 segments(x0=0, x1=2*cos(y0), y0=0,y1=2*sin(y0),col="green",lwd=2, lty=2)
 
  # Plot poses on the orbit
  points(poses*cos(y0),poses*sin(y0), col="darkgreen", type="p",pch=19)
  
}


