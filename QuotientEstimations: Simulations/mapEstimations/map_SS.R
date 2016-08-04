# MLE estimation when NO NOISE.
# ordre pour v: q0,phi0,sigma,xi,eta

map_SS <- function(estimator,sim){
  n=sim$n
  x = sim$Data
  if (sim$s ==0 ){ #CLOSED FORMS
    q0= mean(apply(x,1, function(u) sqrt(u[1]^2+u[2]^2)))
    phi0 = mean(apply(x,1,function(u) atan2(u[2],u[1])))
    eta= NaN
    xi = NaN
    coef <- c(q0,xi,phi0,eta)
    names(coef)<- c('q0','phi0','xi','eta')
  }
  else if ((sim$xi==0)&&(sim$eta==0)){ #CLOSED FORM
    flagCoef='q0phi0s'
    flag = 'xi0eta0'
    phi0 = atan2(sum(x[,2]),sum(x[,1]))
    q0 = (sqrt((sum(x[,1]))^2+ (sum(x[,2]))^2))/n
    sigma = NaN #we dont estimate it for now
    coef = c(q0,phi0,s)
    names(coef) =c('q0','phi0','s')
  }
  
  else{
    if ((sim$xi==0)&&(sim$eta==Inf)) { namesCoef=c('q0','phi0','s'); flag='xi0etaInf'}
    else if (sim$xi==0) {namesCoef=c('q0','phi0','s','eta'); flag='xi0'}
    else if (sim$eta==0) {namesCoef=c('q0','phi0','s','xi'); flag='eta0'}
    else if(sim$eta==Inf) {namesCoef=c('q0','phi0','s','xi'); flag='etaInf'}
    else { namesCoef=c('q0','phi0','s','xi','eta'); flag=''}
    
    initial <- initialization(sim,namesCoef)
    #     print(names(initial))
    l = lowerbound(namesCoef)
    u = upperbound(namesCoef)
    mLL <- function(v) mLogLikelihood_SS(v,sim,flag)
    mLPrior <- function(v) mLogPrior(v,namesCoef)
    mLPosterior <- function(v) mLL(v)+mLPrior(v)
    result <- nlminb(initial, mLPosterior, scale = 1, lower = l, upper = u, control=list(trace=0))
    coef=result$par[1:length(namesCoef)]
    names(coef)=namesCoef
  }
  
  
  estimator <- setcoefficients(estimator,coef)
  return(estimator)
}
