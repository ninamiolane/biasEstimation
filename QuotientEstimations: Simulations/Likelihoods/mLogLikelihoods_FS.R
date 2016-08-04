#Minus Log Likelihood
# ordre pour v: q0,phi0,sigma,xi,eta
source('Likelihoods/baby_FS.R')

mLogLikelihood_FS <- function(v,sim,flag){
  n=sim$n
  l=length(v)
  q= v[(l-n+1):l]
  xq= cbind(sim$Data,q)
coef=v[1:(l-n)]
  if (flag== 'etaInf'){
    L=apply(xq,1, function(x) baby_FS_etaInf(coef,x[1:2],x[3]))
  }
  else{
    L=apply(xq,1, function(x) baby_FS(coef,x[1:2],x[3]))
  }
  return(sum(L))
}
