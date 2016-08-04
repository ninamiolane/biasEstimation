
plot.simulation <- function(object){
  #print("Calling the plot.simulation function...")
  
  n <-object$n
  y0 <-object$y0
  phi0 <- object$phi0
  Y0 <- c(y0*cos(phi0),y0*sin(phi0))
  Data <-object$Data
  poses <- object$orbitData
  
  #Main plot
  emptyplot(xlim=c(-1,2),ylim=c(-1,2),asp=1,frame.plot=FALSE)
  #Add title
  title=paste('Simulation for n=',n)
  title(main=title,cex.main=1)
  #Plot axis
  axis(side=1, at=c(-1,0,1,2),pos=c(0,0),outer=FALSE, 
       cex.axis=0.7,tck=0.01,xpd = NA)
  # axis(side=2,at=c(-1,0,1,2), pos=c(0,0),outer=FALSE, 
  # cex.axis=0.7,las=2,tck=0.01,xpd = TRUE)
  
  #Plot Data
  points(Data$x1,Data$x2,xlim=c(-1,2),ylim=c(-1,2),
         col="darkgrey",pch=19, xaxt="n",yaxt="n") #axis = FALSE  
  # Plot real mean in green on R_+
  points(y0,0, col="green",lwd=6, type="p",pch=19)
  segments(x0 = 0, x1 = y0,y0 = 0, y1 = 0, col = "green",lwd=4)
  # Plot real mean in green on R^2 
  points(Y0[1],Y0[2], col="green",lwd=6, type="p",pch=19)
  segments(x0 = 0, x1 = Y0[1],y0 = 0, y1 = Y0[2], col = "green",lwd=4)
  # Plot orbit and Y0 on orbit
  plotcircle(r=y0,from=0, to=phi0, lcol="green",lwd=4)
  plotcircle(r=y0,lcol="green",lwd=2, lty=2)
  # Plot poses on the orbit
  points(y0*cos(poses),y0*sin(poses), col="darkgreen", type="p",pch=19)
  
}