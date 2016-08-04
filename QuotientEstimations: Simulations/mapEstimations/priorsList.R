# MAP Priors
GaussPrior_q0 <- function(q0){
  barq0 = 1
  sq0=1
  if ((sq0 == 0) || (is.nan(q0))) { res=1e+4} #Case L=Inf-Inf
  else{res = (barq0 -q0)^2/(2*sq0^2)}
  return(res)
}

GaussPrior_phi0 <- function(phi0){
  barphi0=0
  sphi0=pi/20
  if ((sphi0 == 0) || is.nan(phi0)) { res=1e+4} #Case L=Inf-Inf
  else{  res = (barphi0 -phi0)^2/(2*sphi0^2)}
  return(res)
}


InvWishartPrior_xi <- function(xi){
  barxi=0.1
  axi=1 #??
  if ((xi == 0) || is.nan(xi)) {res=1e+4}
  else{ res = axi/2*(barxi^2/xi^2+log(xi^2))}
  return(res)
}


InvWishartPrior_eta <- function(eta){
  bareta=0.1
  aeta=1 #??
  if ((eta == 0) || is.nan(eta)) {res=1e+4}
  else{  res = aeta/2*(bareta^2/eta^2+log(eta^2))}
  return(res)
}

InvWishartPrior_s <- function(s){
  bars=0.1
  as=1 #??
  if ((s == 0) || is.nan(s)) {res=1e+4}
  else {res = as/2*(bars^2/s^2+log(s^2))}
  return(res)
}


