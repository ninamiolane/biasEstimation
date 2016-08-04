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

estimationMleFunctional <- function(object,sim){
  n = sim$n
  x = sim$Data
  ############################################################
  ######### Using closed forms ###############################
  phiest = apply(x,1, function(u) atan2(u[2],u[1]))
  norms = apply(x,1,function(u) sqrt(u[1]^2+u[2]^2))
  y0est = mean(norms)
  aux = apply(x,1,function(u) (sqrt(u[1]^2+u[2]^2)-y0est)^2)
  sigmaest = sqrt(sum(aux)/(2*n)) 
  #   ##########################################################
  #   ######## Using double minimization procedure #############
  #   # Definition of the costs -to be minimized iteratively
  #   cost_phi <- function(phi,xi,y0) return(sqdist(xi,y0,phi))
  #   cost_y0 <- function(y0,x,phi) return(sum(sqdistvec(x,y0,phi)))
  #   # Initialization of the parameters
  #   phiaux = matrix(0,n,1)
  #   phi = matrix(0,n,1)
  #   y0aux = sqrt(x[1,1]^2 +x[1,2]^2)
  #   y0= 100 #pour que la condition while ne soit pas verifee
  #   it=1
  #   # Start: Iterations of double minimizations
  #   while (abs(y0- y0aux) > 0.1*abs(y0)){
  #    # cat('At it=',it,'y0=',y0aux,'\n')
  #    # it=it+1
  #     phi = phiaux
  #     y0 = y0aux
  #     for (i in 1:n){
  #       # Minimization 1: registration of each data, separately
  #       #cat("Cost_phi:   ")
  #       result <- nlminb(phi[i,], cost_phi, xi=x[i,], y0=y0,scale = 1, control = list(trace=0), lower = -pi, upper = pi)
  #       phiaux[i,] <-result$par 
  #       # Print if error in a registration
  #       #cat('cvgce=',result$convergence,'\n')
  #       #cat(result$message,'\n')
  #       if (result$convergence != 0) {
  #         print('ERROR in the convergence for the registration.')
  #         #cat('result$par=',result$par,'\n')
  #       }
  #     }
  #     # Minimization 2 : update of the template
  #     cat("Cost_y0 \n")
  #     result <- nlminb(y0, cost_y0, x=x, phi=phiaux, scale = 1, control = list(trace=1), lower = 0, upper = Inf)
  #     y0aux <-result$par 
  #     # Print the result of the optimization
  #     #if (result$convergence == 0) cat('Successful convergence for y0 in', result$iterations,'iterations.\n')
  #     #else  print('ERROR in the convergence.')
  #   }
  #   y0est= y0aux
  #   phiest=phiaux
  #   ###############################################################
  #   ###############################################################
  # Compute the MSE for the estimate of the template
  y0sim = sim$y0
  mse <- (y0est-y0sim)^2
  # Setting the values in o0bject
  v <- c(y0est,sigmaest)
  names(v) = c('y0','sigma')
  object <- setcoef(object,v)
  object <- setmse(object,mse)
  object <- setphi(object,phiest)
  return(object)
}

estimationMapFunctional <- function(object,sim){
  n = sim$n
  x = sim$Data
  # Definition of the costs -to be minimized iteratively
  cost_phi <- function(phi,xi,y0) return(sqdist(xi,y0,phi))
  cost_y0 <- function(y0,x,phi) return(sum(sqdistvec(x,y0,phi)))
  lambda = 1 # coefficient for regularization
#   ## Initialization of the parameters
#   phiaux = matrix(0,n,1)
#   phi = matrix(0,n,1)
#   y0aux = sqrt(x[1,1]^2 +x[1,2]^2)
#   y0= 100 #pour que la condition while ne soit pas verifee
  ## Alternative: Initialization of the parameters with mleF
  object$method <- 'mleF'
  object <- estimationMleFunctional(object,sim)
  object$method <- 'mapF'
  phiaux = t(t(object$phi))
  phi = matrix(0,n,1)
  y0aux = object$coefficients[['y0']]
  y0= 100 #pour que la condition while ne soit pas verifee
  # Start: iterations of double-minimimzation
  while (abs(y0- y0aux) > 0.1*abs(y0)) {
    phi = phiaux
    y0 = y0aux
    for (i in 1:n){
      # Minimization 1: registration of each data, adding a regularization term weighted by lamda
      result <- nlminb(phi[i,], function(phi,xi,y0) cost_phi(phi,xi,y0) + lambda*phi^2, xi=x[i,], y0=y0,scale = 1, control = list(trace=0), lower = -pi, upper = pi)
      phiaux[i,] <-result$par 
      # Print if error in the convergence
      #if (result$convergence != 0) print('ERROR in the convergence for the registration.')
    }
    # Minimization 2: Update of the template
    result <- nlminb(y0, cost_y0, x=x, phi=phiaux,scale = 1, control = list(trace=0), lower = 0, upper = Inf)
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

## Max-Max Usual procedure
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