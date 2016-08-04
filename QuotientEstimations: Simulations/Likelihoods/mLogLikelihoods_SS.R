#Minus Log Likelihood
# ordre pour v: q0,phi0,sigma,xi,eta

source('Likelihoods/baby_SS.R')
mLogLikelihood_SS <- function(v,sim,flag){
  switch(flag,
         'xi0etaInf'={L=apply(sim$Data,1, function(x) baby_SS_xi0_etaInf(v,x))},
         'xi0'={L=apply(sim$Data,1, function(x) baby_SS_xi0(v,x))},
         'eta0'={L=apply(sim$Data,1, function(x) baby_SS_eta0(v,x))},
         'etaInf'={L=apply(sim$Data,1, function(x) baby_SS_etaInf(v,x))},
{L=apply(sim$Data,1, function(x) baby_SS(v,x))})
return(sum(L))
}



