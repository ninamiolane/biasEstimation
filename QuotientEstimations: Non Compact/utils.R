# utils

modulo2pi <- function(phi){ 
  return(phi-floor(phi/(2*pi))*2*pi)
}

sqdist <- function(x,y0,phi){
  res <- (x[1]-y0*cos(phi))^2 +(x[2]-y0*sin(phi))^2
  return(res[1]) #squeeze
}

sqdistvec <- function(X,y0,phi){
  all <- cbind(X,phi)
  res <- apply(all, 1,function(u) sqdist(u[1:2],y0,u[3]))
  return(res)
}