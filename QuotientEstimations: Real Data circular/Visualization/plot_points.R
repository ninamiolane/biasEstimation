plot_points=function(theta,phi,color="black",size=4){
  if (length(which(theta>pi))>0) {cat("Error in plot.points: theta>pi.")}
  else if (length(which(theta<0))>0){cat("Error in plot.points: theta <0. ")}
  else{
    points3D(x = sin(theta)*cos(phi),y=1-cos(theta), z =sin(theta)*sin(phi),add=TRUE,col=color,type="p",pch=19,pwd=size)
  }
}