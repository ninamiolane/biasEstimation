plot_sphere=function(orientation_phi=15,orientation_theta=60){#plot.sphere(80,15)
  phi=orientation_phi
  theta=orientation_theta
  par(mfrow=c(1,1))
  x0 <- c(0,0,0); y0 <- c(0,0,0); z0 <- c(0,0,0)
  x1 <-c(1,0,0); y1 <-c(0,1,0);z1 <-c(0,0,1)
  f <- seq(-2*pi, 2*pi, 0.3)
  arrows3D(x0,y0,z0,x1,y1,z1, lwd = 1, xlim = c(-1, 1.3), ylim = c(0, 2.3),zlim = c(-1, 1.3),phi=phi,theta=theta)
  for (angle in seq(0, pi, 0.3)){
    points3D(x = sin(angle)*cos(f), z =sin(angle)*sin(f),y=rep(1+cos(angle),length(f)),add=TRUE ,type = "l",col="lightgrey",lty=3)
    #points3D(x = sin(angle)*cos(f), y =1+sin(angle)*sin(f),z=rep(cos(angle),length(f)),add=TRUE ,type = "l",col="lightgrey",lty=3)
    Rot=matrix(c(cos(angle),0,-sin(angle),0,1,0,-sin(angle),0,-cos(angle)),nrow=3,ncol=3,byrow=TRUE)
    vec=matrix(c(cos(f),1+sin(f),rep(0,length(f))),ncol=3)
    vec.rotated=matrix(0,ncol=3,nrow=length(f))
    for(i in 1:length(f)){vec.rotated[i,]=vec[i,]%*%Rot}
    points3D(x =vec.rotated[,1] , y =vec.rotated[,2] ,z=vec.rotated[,3] ,add=TRUE ,type = "l",col="lightgrey",lty=3,lwd=1)
  }
  f <- seq(-pi/2, pi/2, 0.1)
  points3D(x = cos(f), y =1+sin(f),z=rep(0,length(f)),add=TRUE ,type = "l",col="black",lty=1,lwd=4)
}