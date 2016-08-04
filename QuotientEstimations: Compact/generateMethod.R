library(CircStats)

generate.simulation <- function(object){
  # Extract simulation parameters
  n=object$n
  y0 = object$y0
  phi0=object$phi0
  sR=object$sR
  sC=object$sC
  eta=object$eta
  # Variability in the quotient
  y0noisy <- rnorm(n,mean=y0,sd=sR)
  # Variability in the orbit
  if (eta==0){poses <-rep(phi0,n)}
  else{ 
  kappa = 1/eta^2
  poses <- rvm(n,mean = phi0, k=kappa)}
  # Form real images in the ambient space
  x1 <- apply(cbind(y0noisy,poses),1, function(u) u[1]*cos(u[2]))
  x2 <- apply(cbind(y0noisy,poses),1, function(u) u[1]*sin(u[2]))
  x <- cbind(x1,x2)
  # Noise in ambient space
  X <- apply(x,1, function(y) mvrnorm(1,y,sC*diag(2)))
  # Create a data frame of observations
  Data <- data.frame(t(X))
  # Set values in simulation
  object <- setorbitData(object,poses)
  object <- setData(object,Data)
  return(object)
}

generate <- function(object){
  UseMethod("generate",object)
}

generate.default <- function(object){
  print("I do not know how to handle this object.")
  return(object)
}
