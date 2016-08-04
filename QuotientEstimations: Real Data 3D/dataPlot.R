dataPlot <- function(data){
  n = nrow(data)
  #   maxmin = matrix(c(0,0,0,0,0,0), nrow=2) #row are max/min, col are x,y,z
  #   for (i in 1:n){
  #     landmarksi = Landmarks2Matrix(data[i,])
  #     if (max(landmarksi[,1])> maxmin[1,1]) maxmin[1,1]=max(landmarksi[,1])
  #     if (min(landmarksi[,1])> maxmin[2,1]) maxmin[2,1]=min(landmarksi[,1])
  #     if (max(landmarksi[,2])> maxmin[1,2]) maxmin[1,2]=max(landmarksi[,2])
  #     if (min(landmarksi[,2])> maxmin[2,2]) maxmin[2,2]=min(landmarksi[,2])
  #     if (max(landmarksi[,3])> maxmin[1,3]) maxmin[1,3]=max(landmarksi[,3])
  #     if (min(landmarksi[,3])> maxmin[2,3]) maxmin[2,3]=min(landmarksi[,3])
  #   }
  plot.new()
  LandmarksPlot(data[1,])
  #, xlim = c(maxmin[2,1],maxmin[1,1]),ylim = c(maxmin[2,2],maxmin[1,2]),zlim= c(maxmin[2,3],maxmin[1,3]))
  for (i in 2:n){
    LandmarksPlot(data[i,],add=T)
  }
}
