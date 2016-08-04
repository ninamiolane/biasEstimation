#From rotvec to point coordinate
rotpoint=function(r){
  r <-regrot(r) 
  R <- RotMat(r)
  u <- R %*% a
  theta=atan(sqrt(u[1]^2+u[3]^2)/(-u[2]))
  if (theta<0){theta <- theta +pi}
  phi=atan(u[3]/u[1])
  point=c(theta,phi)
  return(point)
}