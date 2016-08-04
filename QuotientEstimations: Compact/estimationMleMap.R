# Estimation procedures for MLE and MAP.

# Optimization -with bounds- is performed using nlminb.
# Code for optimization using optimx is in comments at the end of this script.
# --> Note that we do not provide Gradient, nor Hessian.

# Test and Print the result of the convergence for the optimization.
#   if (result$convergence == 0) cat('Successful convergence in', result$iterations,'iterations.\n')
#   else print('ERROR in the convergence.')

estimationMleStructural <- function(object, sim){
  # Initialization of the parameters
  x = sim$Data
  iny0 = sqrt(x[1,1]^2 +x[1,2]^2)
  inphi0 = atan2(x[1,2],x[1,1])
  insigma = sqrt((x[1,1]-x[2,1])^2 +(x[1,2]-x[2,2])^2)
  
  if (sim$sC ==0) {
    y0est = mean(apply(x,1, function(u) sqrt(u[1]^2 +u[1]^2))) #shall be all sames
    phi0est = mean(apply(x,1, function(u) atan2(u[2],u[1])))
    newVal = c(y0est,phi0est)
    names(newVal) = c('y0','phi0')
  }
  else{ 
    v <- c(iny0,inphi0,insigma)
    names(v) <- c('y0','phi0','sigma')
    eta = sim$eta
    etachar = as.character(eta)
    switch(etachar,
           '0' = {
             ## CLOSED FORM
             phi0est = atan2(sum(x[,2]),sum(x[,1]))
             y0est = sqrt(sum(x[,1])^2+ sum(x[,2])^2)/n
             sigmaest = 0 #we dont estimate it for now
             ##################################################################      
             #     # Minimization of -logLikelihood.
             #     result <- nlminb(v, llf_delta,sim=sim, scale = 1, lower = c(0,-pi,0), upper = c(Inf,pi,Inf),control=list(trace=0))
             #     # Get the argmin of the minimization, i.e. the estimates
             #     y0est=result$par[1]
             #     phi0est=result$par[2]
             #     sigmaest =result$par[3]
             ##################################################################
             newVal = c(y0est,phi0est,sigmaest)
             names(newVal) = c('y0','phi0','sigma')
           },
           'Inf' = { 
             # Minimization of -logLikelihood.
             result <- nlminb(v, llf_uniform,sim=sim, scale = 1, lower = c(0,-pi,0), upper = c(Inf,pi,Inf),control=list(trace=0))
             # Get the argmin of the minimization, i.e. the estimates
             y0est=result$par[1]
             phi0est=result$par[2]
             sigmaest=result$par[3]
             #cat('y0est=',y0est,'phi0est=',phi0est)
             newVal = c(y0est,phi0est,sigmaest) 
             names(newVal) <- c('y0','phi0','sigma')
           },
{    
  # Initialization of eta
  ineta <- abs(atan2(x[1,2],x[1,1]) - atan2(x[2,2],x[2,1]))
  v[4] <- ineta
  names(v)[4] <- 'eta'
  # Minimization of -logLikelihood.
  result <- nlminb(v, llf_gaussian,sim=sim, scale = 1, lower = c(0,-pi,0,0), upper = c(Inf,pi,Inf,pi),control=list(trace=0))
  # Get the argmin of the minimization, i.e. the estimates
  y0est=result$par[1]
  phi0est=result$par[2]
  sigmaest=result$par[3]
  etaest=result$par[4]
  #cat('y0est=',y0est,'phi0est=',phi0est)
  newVal = c(y0est,phi0est,sigmaest,etaest) 
  names(newVal) <- c('y0','phi0','sigma','eta')
})
  }
# Computing the MSE
y0sim = sim$y0
mse <- (y0est-y0sim)^2
# Set the estimates in the object
object <- setcoef(object,newVal)
object <- setmse(object,mse)
return(object)
}

estimationMapStructural <- function(object, sim){
  x = sim$Data
  iny0 = sqrt(x[1,1]^2 +x[1,2]^2)
  inphi0 = atan2(x[1,2],x[1,1])
  insigma = sqrt((x[1,1]-x[2,1])^2 +(x[1,2]-x[2,2])^2)
  v <- c(iny0,inphi0,insigma)
  names(v) <- c('y0','phi0','sigma')
  eta = sim$eta
  etachar = as.character(eta)
  switch(etachar,
         '0'={ 
           # Definition of the -LogPosterior
           posterior <- function(x) llf_delta(x,sim)+GaussPrior_y0(x[1])+GaussPrior_phi0(x[2])+InvWishartPrior_sigma(x[3])
           # Minimization of -logPosterior
           result <- nlminb(v, posterior,scale = 1, lower = c(0,-pi,0), upper = c(Inf,pi,Inf),control=list(trace=0))
           # Get the argmin of the minimization, i.e. the estimates
           y0est=result$par[1]
           phi0est=result$par[2]
           sigmaest =result$par[3]
           # Setting values in the object
           newVal = c(y0est,phi0est,sigmaest)
           names(newVal) = c('y0','phi0','sigma')
         },
         'Inf'={   
           # Definition of the -LogPosterior
           posterior <- function(x) return(llf_gaussian(x,sim)+GaussPrior_y0(x[1])+GaussPrior_phi0(x[2])+InvWishartPrior_sigma(x[3])+InvWishartPrior_eta(x[4]))
           # Minimization of -logPosterior
           result <- nlminb(v, posterior,gradient=NULL, hessian=NULL ,
                            scale = 1, lower = c(0,-pi,0), upper = c(Inf,pi,Inf),control=list(trace=0))
           # Get the argmin of the minimization, i.e. the estimates
           y0est=result$par[1]
           phi0est=result$par[2]
           sigmaest=result$par[3]
           # Setting values in the object
           newVal = c(y0est,phi0est,sigmaest) 
           names(newVal) = c('y0','phi0','sigma')
         },
         
{   
  # Initialization of eta
  ineta <- abs(modulopi(atan2(x[1,2],x[1,1]) - atan2(x[2,2],x[2,1])))
  v[4] <- ineta
  names(v)[4] <- 'eta'
  # Definition of the -LogPosterior
  posterior <- function(x) return(llf_gaussian(x,sim)+GaussPrior_y0(x[1])+GaussPrior_phi0(x[2])+InvWishartPrior_sigma(x[3])+InvWishartPrior_eta(x[4]))
  # Minimization of -logPosterior
  result <- nlminb(v, posterior,gradient=NULL, hessian=NULL ,
                   scale = 1, lower = c(0,-pi,0,0), upper = c(Inf,pi,Inf,pi),control=list(trace=0))
  # Get the argmin of the minimization, i.e. the estimates
  y0est=result$par[1]
  phi0est=result$par[2]
  sigmaest=result$par[3]
  etaest=result$par[4]
  # Setting values in the object
  newVal = c(y0est,phi0est,sigmaest,etaest) 
  names(newVal) = c('y0','phi0','sigma','eta')
})
# Computing the MSE
y0sim = sim$y0
mse <- (y0est-y0sim)^2

object <- setcoef(object,newVal)
object <- setmse(object,mse)
return(object)
}


### Code for Optimization using optimx

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
