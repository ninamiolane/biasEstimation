#http://www.cyclismo.org/tutorial/R/s3Classes.html

distributions <- function(y0Distribution= NULL,  phi0Distribution=NULL,
                                  sigmaDistribution = NULL,etaDistribution = NULL,
                                  mseDistribution = NULL, method=NULL)
{
  distr <- list(y0Distribution= y0Distribution,
                         phi0Distribution=phi0Distribution,
                         sigmaDistribution = sigmaDistribution,
                         etaDistribution = etaDistribution,
                         mseDistribution = mseDistribution,
                         method = method)
  names(distr) <- c('y0Distribution','phi0Distribution','sigmaDistribution','etaDistribution','mseDistribution','method')
  class(distr) <- append(class(distr),"distributions")
  return(distr)
}

#Set Method

setdistributions.distributions <- function(object, y0D, phi0D=NULL,sigmaD=NULL,etaD=NULL,mseD){
  object$y0Distribution <- y0D  
  object$phi0Distribution <- phi0D  
  object$sigmaDistribution <- sigmaD 
  object$etaDistribution <- etaD  
  object$mseDistribution <- mseD  
  return(object)
}

setdistributions <- function(object, y0D, phi0D=NULL,sigmaD=NULL,etaD=NULL,mseD){
  UseMethod("setdistributions",object)
}

setdistributions.default <- function(object, y0D, phi0D=NULL,sigmaD=NULL,etaD=NULL,mseD){
  print("I do not know how to handle this object.")
  return(object)
}