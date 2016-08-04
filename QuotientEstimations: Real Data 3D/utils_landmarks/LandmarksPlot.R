# Plot landmarks
require(plot3D) # has points3D function
library(ggplot2)
library(graphics)
require(rgl)

LandmarksPlot <- function(landmarks, add=FALSE){
  DF <- Landmarks2Matrix(landmarks)
  K= nrow(DF)
  x = DF[,1]
  y = DF[,2]
  z = DF[,3]
  color = 1:K
  points3D(x,y,z, add=add,type = "p", pch=19, cex=1,colvar=color) #col=(DF$col),
}
