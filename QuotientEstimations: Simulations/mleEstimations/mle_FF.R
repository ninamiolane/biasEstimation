mle_FF <- function(estimator,sim){
  x = sim$Data
  q= apply(x,1, function(u) sqrt(u[1]^2+u[2]^2))
  phi = apply(x,1,function(u) atan2(u[2],u[1]))
  s = 0
  coef = s
  names(coef)='s'
  # Set the estimates in the estimator
  estimator <- setq(estimator,q)
  estimator <- setphi(estimator,phi)
  estimator <- setcoefficients(estimator,coef)
  return(estimator)
}
