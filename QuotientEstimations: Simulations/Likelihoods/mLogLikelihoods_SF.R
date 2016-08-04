#Minus Log Likelihood
# ordre pour v: q0,phi0,sigma,xi,eta

source('Likelihoods/baby_SF.R')
mLogLikelihood_SF <- function(v,sim){
  n=sim$n
  l=length(v)
startphi= l-n+1
endcoef=l-n
phi= v[startphi:l]
  coef=v[1:endcoef]
  xphi=cbind(sim$Data,phi)
L=apply(xphi,1, function(u) baby_SF(coef,u[1:2],u[3]))
  return(sum(L))
}
