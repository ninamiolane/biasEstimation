#########################################3
### NON compact orbits case #################
#########################################


## Estimation procedures with Max-Max algorithms.
# 1. Max-Max with a perfect registration: 
# - data are perfectly aligned on the quotient
# - mean is computed on R+
# 2. Max-Max with iteration of minimization procedure (when one does not have closed form): 
# - each data is registered by minimization of a cost
# - the template is computed by minimization
# - the two previous steps are iterated until convergence of the template.
# 3. Idem but a regularization term is added in the cost for the registration.

# Optimization -with bounds- is performed using nlminb.
# Code for optimization using optimx is in comments at the end of this script.


estimationMaxmaxPerf <- function(object, sim){
  # Perfect registration of each data on the quotient: ie projection on the circle
  phi <- apply(sim$Data,1,function(x) sqrt(sum(x^2))-conv) #Note: phi in [-conv, +Inf[
  # Compute the quotient data: ie the position on the circle
  quotientData <- apply(sim$Data,1,function(x) atan2(x[1],x[2]))  #we want [-pi,pi]: atan2??
  # Computing the mean in the quotient, ie in the circle
  y0 = mean(quotientData) # uniqueness of the mean in the circle?
  names(y0) = 'y0'
  # Computing the MSE
  y0sim = sim$y0
  mse <- (y0-y0sim)^2
  # Setting the values in o0bject
  object <- setphi(object,t(t(phi)))
  object <- setcoef(object,y0)
  object <- setmse(object,mse)
  return(object)
}

estimationMaxmaxUsual <- function(object,sim){
  n = sim$n
  x = sim$Data
  # Definition of the costs -to be minimized iteratively
  cost_phi <- function(phi,xi,y0) return(sqdist(xi,conv+phi,y0))
  cost_y0 <- function(y0,x,phi) return(sum(sqdistvec(x,conv+phi,y0)))
  # Initialization of the parameters
  phiaux = matrix(conv,n,1)
  phi = matrix(conv,n,1)
  y0aux = atan2(x[1,1],x[1,2]) # is this in [-pi,pi]?
  y0= 10 #pour que la condition while ne soit pas verifee
  
  # Start: Iterations of double minimizations
  while (abs(y0- y0aux) > 0.01){
    phi = phiaux
    y0 = y0aux
    for (i in 1:n){
      # Minimization 1: registration of each data, separately
      result <- nlminb(phi[i,], cost_phi, xi=x[i,], y0=y0,scale = 1, control = list(trace=0), lower = -conv, upper = Inf)
      phiaux[i,] <-result$par 
      # Print if error in a registration
      if (result$convergence != 0) print('ERROR in the convergence for the registration.')
    }
    # Minimization 2 : update of the template
    result <- nlminb(y0, cost_y0, x=x, phi=phiaux, scale = 1, control = list(trace=0), lower = -pi, upper = pi)
    y0aux <-result$par 
    # Print the result of the optimization
    #if (result$convergence == 0) cat('Successful convergence for y0 in', result$iterations,'iterations.\n')
    #else  print('ERROR in the convergence.')
  }
  # Compute the MSE for the estimate of the template
  y0sim = sim$y0
  mse <- (y0aux-y0sim)^2
  # Setting the values in o0bject
  names(y0aux) = 'y0'
  object <- setcoef(object,y0aux)
  object <- setmse(object,mse)
  object <- setphi(object,phiaux)
  return(object)
}

estimationMaxmaxReg <- function(object,sim){
  n = sim$n
  x = sim$Data
  # Definition of the costs -to be minimized iteratively
  cost_phi <- function(phi,xi,y0) return(sqdist(xi,conv+phi,y0))
  cost_y0 <- function(y0,x,phi) return(sum(sqdistvec(x,conv+phi,y0)))
  lambda = 1
  # Initialization of the parameters
  phiaux = matrix(conv,n,1)
  phi = matrix(conv,n,1)
  y0aux = atan2(x[1,1],x[1,2]) # is this in [-pi,pi]?
  y0= 10 #pour que la condition while ne soit pas verifee
  
  # Start: iterations of double-minimimzation
  while (abs(y0- y0aux) > 0.01) {
    phi = phiaux
    y0 = y0aux
    for (i in 1:n){
      # Minimization 1: registration of each data, adding a regularization term weighted by lamda
      result <- nlminb(phi[i,], function(phi,xi,y0) cost_phi(phi,xi,y0) + lambda*phi^2, xi=x[i,], y0=y0,scale = 1, control = list(trace=0), lower = -conv, upper = Inf)
      phiaux[i,] <-result$par 
      # Print if error in the convergence
      if (result$convergence != 0) print('ERROR in the convergence for the registration.')
    }
    # Minimization 2: Update of the template
    result <- nlminb(y0, cost_y0, x=x, phi=phiaux,scale = 1, control = list(trace=0), lower = -pi, upper = pi)
    y0aux <-result$par  
    # Print the result of the convergence
    #if (result$convergence == 0) cat('Successful convergence for y0 in', result$iterations,'iterations.\n')
    #else print('ERROR in the convergence.')
  }
  # Compute the MSE of the template's estimate
  y0sim = sim$y0
  mse <- (y0aux-y0sim)^2
  # Set the values in the object
  names(y0aux) = 'y0'
  object <- setcoef(object,y0aux)
  object <- setphi(object,phiaux)
  object <- setmse(object,mse)
  return(object)
}


## Max-Max Usual procedure (FOR COMPACT HERE!)
##... registration step
# result <- optimx(phi[i,], function(phiin) sqdist(x[i,],y0,phiin), lower = 0, upper= 2*pi, method = 'L-BFGS-B', itnmax=100)
# phiaux[i,] <-coef(result)[1]
##... template step
# result <- optimx(y0, function(y) sum(sqdistvec(x,y0,phiaux)),method = 'L-BFGS-B', itnmax=100)
# y0aux <-coef(result)[1]

## Max-Max with regularization in the registration
##... registration step
#       result <- optimx(phi[i,], function(phiin) sqdist(x[i,],y0,phiin) + phiin^2, lower = 0, upper= 2*pi, method = 'L-BFGS-B')
#       phiaux[i,] <-coef(result)[1]
##... template step
#     result <- optimx(y0, function(y) sum(sqdistvec(x,y0,phiaux)),method = 'L-BFGS-B', itnmax=100)
#     y0aux <-coef(result)[1]
