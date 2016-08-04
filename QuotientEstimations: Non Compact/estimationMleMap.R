##########################################
### NON compact orbits case ##############
##########################################

# Estimation procedures for MLE and MAP.

# Optimization -with bounds- is performed using nlminb.
# Code for optimization using optimx is in comments at the end of this script.

estimationMleDelta <- function(object, sim)
{
  # Initialization of the parameters
  v <- c(0,0,0.1)
  names(v) <- c('y0','phi0','sigma')
  # Minimization of -logLikelihood.
  # --> Note that we do not provide Gradient, nor Hessian.
  result <- nlminb(v, llf_delta,sim=sim, scale = 1, lower = c(-pi,-conv,0), upper = c(pi,Inf,Inf),control=list(trace=0))
  # Test and Print the result of the convergence for the optimization.
  #if (result$convergence == 0) cat('Successful convergence in', result$iterations,'iterations.\n')
  # else print('ERROR in the convergence.')
  # Get the argmin of the minimization, i.e. the estimates
  y0est=result$par[1]
  phi0est=result$par[2]
  sigmaest =result$par[3]
  newVal = c(y0est,phi0est,sigmaest)
  names(newVal) = c('y0','phi0','sigma')
  # Computing the MSE
  y0sim = sim$y0
  mse <- (y0est-y0sim)^2
  # Set the estimates in the object
  object <- setcoef(object,newVal)
  object <- setmse(object,mse)
  return(object)
}

estimationMleGaussian <- function(object, sim)
{
  # Initialization of the parameters
  v <- c(0,0,0.1,0.1)
  names(v) <- c('y0','phi0','sigma','eta')
  # Minimization of -logLikelihood.
  # --> Note that we do not provide Gradient, nor Hessian.
  result <- nlminb(v, llf_gaussian,sim=sim, scale = 1, lower = c(-pi,-conv,0,-conv), upper = c(pi,Inf,Inf,Inf),control=list(trace=0))
  # Test and Print the result of the convergence for the optimization.
  #if (result$convergence == 0) cat('Successful convergence in', result$iterations,'iterations.\n')
  #else print('ERROR in the convergence.')
  # Get the argmin of the minimization, i.e. the estimates
  y0est=result$par[1]
  phi0est=result$par[2]
  sigmaest=result$par[3]
  etaest=result$par[4]
  newVal = c(y0est,phi0est,sigmaest,etaest) 
  names(newVal) <- c('y0','phi0','sigma','eta')
  # Computing the MSE
  y0sim = sim$y0
  mse <- (y0est-y0sim)^2
  # Set the estimates in the object
  object <- setcoef(object,newVal)
  object <- setmse(object,mse)
  return(object)
}

estimationMapDelta <- function(object, sim)
{
  # Initialization of the parameters
  v <- c(0,0,0.1)
  names(v) <- c('y0','phi0','sigma')
  # Definition of the -LogPosterior
  posterior <- function(x) llf_delta(x,sim)+GaussPrior_y0(x[1])+GaussPrior_phi0(x[2])+InvWishartPrior_sigma(x[3])
  # Minimization of -logPosterior
  result <- nlminb(v, posterior,scale = 1, lower = c(-pi,-conv,0), upper = c(pi,Inf,Inf),control=list(trace=0))
  # Test and Print the result of the convergence for the optimization.
  #if (result$convergence == 0) cat('Successful convergence in', result$iterations,'iterations.\n')
  #else print('ERROR in the convergence.')
  # Get the argmin of the minimization, i.e. the estimates
  y0est=result$par[1]
  phi0est=result$par[2]
  sigmaest =result$par[3]
  # Computing the MSE
  y0sim = sim$y0
  mse <- (y0est-y0sim)^2
  # Setting values in the object
  newVal = c(y0est,phi0est,sigmaest)
  names(newVal) = c('y0','phi0','sigma')
  object <- setcoef(object,newVal)
  object <- setmse(object,mse)
  return(object)
}

estimationMapGaussian <- function(object, sim)
{
  # Initialization of the parameters
  v <- c(0,0,0.1,0.1)
  names(v) <- c('y0','phi0','sigma','eta')
  # Definition of the -LogPosterior
  posterior <- function(x) return(llf_gaussian(x,sim)+GaussPrior_y0(x[1])+GaussPrior_phi0(x[2])+InvWishartPrior_sigma(x[3])+InvWishartPrior_eta(x[4]))
  # Minimization of -logPosterior
  result <- nlminb(v, posterior,gradient=NULL, hessian=NULL ,
                   scale = 1, lower = c(-pi,-conv,0,-conv), upper = c(pi,Inf,Inf,Inf),control=list(trace=0))
  # Test and Print the result of the convergence for the optimization.
  #   if (result$convergence == 0) cat('Successful convergence in', result$iterations,'iterations.\n')
  #   else print('ERROR in the convergence.')
  # Get the argmin of the minimization, i.e. the estimates
  y0est=result$par[1]
  phi0est=result$par[2]
  sigmaest=result$par[3]
  etaest=result$par[4]
  # Computing the MSE
  y0sim = sim$y0
  mse <- (y0est-y0sim)^2
  # Setting values in the object
  newVal = c(y0est,phi0est,sigmaest,etaest) 
  names(newVal) = c('y0','phi0','sigma','eta')
  object <- setcoef(object,newVal)
  object <- setmse(object,mse)
  return(object)
}

### Code for Optimization using optimx (FOR COMPACT HERE!)

## MLE Delta
# result <- optimx(v, function(x) llf_delta(x,sim),method = 'BFGS', itnmax=100, control=list(trace=1))
#   y0est=result$estimate[1]
#   phi0est=result$estimate[2]
#   sigmaest =result$estimate[3]

## MLE Gaussian
#   result <- optimx(v, function(x) llf_gaussian(x,sim), lower = c(0,0,0,0), upper=c(100,2*pi,100,2*pi), method = 'L-BFGS-B', itnmax=100)

## MAP Delta
#   result <- optimx(v, function(x) posterior(x), lower = c(0,0,0), upper = c(100,2*pi,100),method = 'L-BFGS-B', itnmax=100) 

## MAP Gaussian
#   result <- optimx(v, function(x) posterior(x), lower = c(0,0,0,0),upper=c(100,2*pi,100,2*pi), method = 'L-BFGS-B', itnmax=100) 
