##########################################
### NON compact orbits case ##############
##########################################


babyllf_delta <- function(v,x){
  y0 <- v[1]
  phi0 <- v[2]
  sigma <- v[3]
  if (sigma==0){ L=1e+4} #Case L=Inf-Inf
  else{L= sqdist(x,conv+phi0,y0)/(2*sigma^2)+log(2*pi*sigma)}
  return(L)
}

llf_delta <- function(v,sim) {
  obs=sim$Data
  L=apply(obs,1, function(x) babyllf_delta(v,x))
  return(sum(L))
}

babyllf_gaussian <- function(v,x) {
  y0 <- v[1]
  phi0 <- v[2]
  sigma <- v[3]
  eta <- v[4]
  L = 1e+4
  
  if (sigma==0){ probaM <-function(phi) 0}
  else{probaM <- function(phi) exp(-(sqdist(x,conv+phi,y0))/(2*sigma^2))/(2*pi*sigma)}
  if (eta==0){ probaO <-function(phi) 0}
  else{probaO <-function(phi) exp(-(phi-phi0)^2/(2*eta^2))/(2*pi*sqrt(eta)) }  

  integrand <- function(phi) probaM(phi)*probaO(phi)
  tryCatch({res = integrate(Vectorize(integrand),-conv,Inf);L=res$value},
                error=function(cond){message("Encountered an error: ", cond) ; L= 1e+4},
                finally={NA}) 

  return(-log(L))# to have a MINIMIZATION procedure
  
}

llf_gaussian <- function(v,sim) {
  obs=sim$Data
  L=apply(obs,1, function(x) babyllf_gaussian(v,x))
  return(sum(L))
}
