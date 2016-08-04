# MAP Priors: NON compact version ########
##########################################


GaussPrior_y0 <- function(y0){ # y0 in [-pi,pi]
  # Hyper-parameters for gaussian prior
  bary0 = 0
  sigmay0=0.1
  #
  if (sigmay0==0){ res=1e+4} #Case L=Inf-Inf
  else{res = (bary0 -y0)^2/(2*sigmay0^2)}
  
  return(res)
}

GaussPrior_phi0 <- function(phi0){ #phi0 in [-conv,Inf[
  # Hyper-parameters for gaussian prior
  barphi0=0
  sigmaphi0=1
  #
  if (sigmaphi0==0){ res=1e+4} #Case L=Inf-Inf
  else{  res = (barphi0 -phi0)^2/(2*sigmaphi0^2)}
  
  return(res)
}

InvWishartPrior_phi0 <- function(phi0){ #phi0 in [-conv,Inf[
  barphi0=0
  aphi0=1 #??
  
  if (phi0==0){res =1e+4}
  else {res = aphi0/2*(barphi0^2/phi0^2+log(phi0^2))}
  return(res)
}

InvWishartPrior_sigma <- function(sigma){
  barsigma=0.1
  asigma=1 #??
  if (sigma==0) {res=1e+4}
  else {res = asigma/2*(barsigma^2/sigma^2+log(sigma^2))}
  return(res)
}

InvWishartPrior_eta <- function(eta){ #eta in [-conv,Inf[
  bareta=0
  aeta=1 #??
  if (eta==0) {res=1e+4}
  else{  res = aeta/2*(bareta^2/eta^2+log(eta^2))}
  return(res)
}


