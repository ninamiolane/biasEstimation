############################################################
## Generate Data using simulation with NON compact orbits ##
###############################################################

generate.simulation <- function(object){
  n=object$n
  y0 = object$y0
  phi0=object$phi0
  sigma=object$sigma
  eta=object$eta
  # Pose the template
    Y0 <- c((phi0+conv)*cos(y0),(phi0+conv)*sin(y0))
  # Generate Relative poses of the data, wrt to the posed template
  if (eta ==0) {poses <- rep(conv+phi0,n)}
  else {poses <- rnorm(n,mean=conv+phi0,sd=eta)}
  # Add noise 
  X <- apply(cbind(poses*cos(y0),poses*sin(y0)),1, function(y) mvrnorm(1,y,sigma*diag(2)))
  # Create a data frame of observations, set it in the simulation object
  Data <- data.frame(t(X))
  # Set values in object
  object <- setposes(object,poses)
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


