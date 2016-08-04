# MAP Priors


GaussPrior_y0 <- function(y0){
  # Hyper-parameters for gaussian prior
  bary0 = 1
  sigmay0=1
  #
  if ((sigmay0 == 0) || (is.nan(y0))) { res=1e+4} #Case L=Inf-Inf
  else{res = (bary0 -y0)^2/(2*sigmay0^2)}
  
  return(res)
}

GaussPrior_phi0 <- function(phi0){
  # Hyper-parameters for gaussian prior
  barphi0=0
  sigmaphi0=pi/20
  #
  if ((sigmaphi0 == 0) || is.nan(phi0)) { res=1e+4} #Case L=Inf-Inf
  else{  res = (barphi0 -phi0)^2/(2*sigmaphi0^2)}
  
  return(res)
}

InvWishartPrior_phi0 <- function(phi0){
  barphi0=0
  aphi0=1 #??
  
  if ((phi0 == 0) || is.nan(phi0)) {res =1e+4}
  else {res = aphi0/2*(barphi0^2/phi0^2+log(phi0^2))}
  return(res)
}

InvWishartPrior_sigma <- function(sigma){
  barsigma=0.1
  asigma=1 #??
  if ((sigma == 0) || is.nan(sigma)) {res=1e+4}
  else {res = asigma/2*(barsigma^2/sigma^2+log(sigma^2))}
  return(res)
}

InvWishartPrior_eta <- function(eta){
  bareta=0.1
  aeta=1 #??
  if ((eta == 0) || is.nan(eta)) {res=1e+4}
  else{  res = aeta/2*(bareta^2/eta^2+log(eta^2))}
  return(res)
}


