# utils

modulo2pi <- function(phi){ 
  return(phi-floor(phi/(2*pi))*2*pi)
}

modulopi <- function(phi){ 
  return(phi-floor(phi/(pi))*pi)
}

sqdist <- function(x,q,phi){  
  res <- (x[1]-q*cos(phi))^2 +(x[2]-q*sin(phi))^2
#   cat('sqdist=',res[1],'\n')
  return(res[1]) #squeeze
}

sqdistvec <- function(X,q,phi){
  all <- cbind(X,phi)
  res <- apply(all, 1,function(u) sqdist(u[1:2],q,u[3]))
  return(res)
}

g.assign <- function(i, pos=1, ..., value) {
  x <- if (pos > 0) get(i, pos) else get(i, , parent.frame())
  x[...] <- value
  if (pos > 0) assign(i, x, pos) else assign(i, x, , parent.frame())
}