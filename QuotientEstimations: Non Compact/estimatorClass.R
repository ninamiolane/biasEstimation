#http://www.cyclismo.org/tutorial/R/s3Classes.html

estimator <- function(coefficients=NULL, phi=NULL, method=NULL, meansqerror=NULL)
{
  est <- list(coef = coefficients,  phi=phi, meth = method, mse = meansqerror)
  names(est) <- c('coefficients','phi','method','MSE')
  class(est) <- append(class(est),"estimator")
  return(est)
}

#Set Method

setcoef <- function(object, newValue){
  UseMethod("setcoef",object)
}

setcoef.default <- function(object, newValue){
  print("I do not know how to handle this object.")
  return(object)
}

setcoef.estimator <- function(object, newValue){
  object$coefficients <- newValue  
  names(object$coefficients) <- names(newValue)
  return(object)
}
##

setphi <- function(object, newValue){
  UseMethod("setphi",object)
}

setphi.default <- function(object, newValue){
  print("I do not know how to handle this object.")
  return(object)
}

setphi.estimator <- function(object, newValue){
  object$phi <- newValue  
  return(object)
}
 ## set MSE 

setmse <- function(object, newValue){
  UseMethod("setmse",object)
}

setmse.default <- function(object, newValue){
  print("I do not know how to handle this object.")
  return(object)
}

setmse.estimator <- function(object, newValue){
  object$mse <- newValue  
  return(object)
}


