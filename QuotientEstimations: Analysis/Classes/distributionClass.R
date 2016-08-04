#http://www.cyclismo.org/tutorial/R/s3Classes.html

distributions <- function(distribution= NULL, method=NULL){
  distr <- list(distributions, method)
  names(distr) <- c('distributions','method')
  class(distr) <- append(class(distr),"distributions")
  return(distr)
}

#Set Method

setdistributions.distributions <- function(object, distributions){
  object$distributions <- distributions
  names(object$distributions) <- names(distributions)
  return(object)
}

setdistributions <- function(object, distributions){
  UseMethod("setdistributions",object)
}

setdistributions.default <- function(object, distributions){
  print("I do not know how to handle this object.")
  return(object)
}