#http://www.cyclismo.org/tutorial/R/s3Classes.html

estimator <- function(coefficients=NULL, q=NULL, phi=NULL, method=NULL){
  est <- list(coefficients, q, phi, method)
  names(est) <- c('coefficients','q','phi','method')
  class(est) <- append(class(est),"estimator")
#   switch(method,
#          'mle_FF'={ est$coefficients=list(NULL)
#                     names(est$coefficients)=list('s')
#          },
#          'mle_FS'={ 
#          },
#          'mle_SF'={ estimator <- 
#          },
#          'mle_SS'={ estimator
#          )
  return(est)
}

#Set Method

setcoefficients <- function(object, newValue){
  UseMethod("setcoefficients",object)
}

setcoefficients.default <- function(object, newValue){
  print("I do not know how to handle this object.")
  return(object)
}

setcoefficients.estimator <- function(object, newValue){
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

##
setq <- function(object, newValue){
  UseMethod("setq",object)
}

setq.default <- function(object, newValue){
  print("I do not know how to handle this object.")
  return(object)
}

setq.estimator <- function(object, newValue){
  object$q <- newValue  
  return(object)
}


