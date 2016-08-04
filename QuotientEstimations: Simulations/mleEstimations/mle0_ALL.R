# MLE estimation when NO NOISE.

# mle0_FF <- function(estimator,sim){
#   x = sim$Data
#   # Closed forms
#   q= apply(x,1, function(u) sqrt(u[1]^2+u[2]^2))
#   phi = apply(x,1,function(u) atan2(u[2],u[1]))
# #   coef <-c(0,0,0,0,0)
# #   names(coef) <-c('q0','xi','phi0', 'eta','s')
#   # Set the estimates in the estimator
#   estimator <- setq(estimator,q)
#   estimator <- setphi(estimator,phi)
#   estimator <- setcoefficients(estimator,coef)
#   return(estimator)
# }

# mle0_FS <- function(estimator,sim){
#   x = sim$Data
#   # Closed forms
#   r= apply(x,1, function(u) sqrt(u[1]^2+u[2]^2))
#   phi0 = mean(apply(x,1,function(u) atan2(u[2],u[1])))
#   eta = NaN
#   coef <-c(phi0,eta)
#   names(coef) <-c('phi0', 'eta')
#   # Set the estimates in the estimator
#   estimator <- setq(estimator,r)
#   estimator <- setcoefficients(estimator,coef)
#   return(estimator)
# }
# 
# mle0_SF <- function(estimator,sim){
#   x = sim$Data
#   # Closed forms
#   q0= mean(apply(x,1, function(u) sqrt(u[1]^2+u[2]^2)))
#   xi = NaN
#   coef <-c(q0,xi)
#   names(coef) <-c('q0', 'xi')
#   phi = apply(x,1,function(u) atan2(u[2],u[1]))
#   # Set the estimates in the estimator
#   estimator <- setphi(estimator,phi)
#   estimator <- setcoefficients(estimator,coef)
# 
#   return(estimator)
# }
# 
# mle0_SS <- function(estimator,sim){
#   x = sim$Data
#   # Closed forms
#   q0= mean(apply(x,1, function(u) sqrt(u[1]^2+u[2]^2)))
#   phi0 = mean(apply(x,1,function(u) atan2(u[2],u[1])))
#   eta= NaN
#   xi = NaN
#   coef <- c(q0,xi,phi0,eta)
#   names(coef)<- c('q0','xi','phi0','eta')
#   # Set the estimates in the estimator
#   estimator <- setcoefficients(estimator,coef)
#   return(estimator)
# }

