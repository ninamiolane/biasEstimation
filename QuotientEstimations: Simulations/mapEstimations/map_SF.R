

map_SF <- function(estimator,sim){
  x = sim$Data
  n=sim$n
  if (sim$s ==0){ #CLOSED FORM
    q0= mean(apply(x,1, function(u) sqrt(u[1]^2+u[2]^2)))
    xi = NaN
    phi = apply(x,1,function(u) atan2(u[2],u[1]))
    coef <-c(q0,xi)
    names(coef) <-c('q0', 'xi')
  }
  else if (sim$xi==0) {  # CLOSED FORM
    q0= mean(apply(x,1, function(u) sqrt(u[1]^2+u[2]^2)))
    phi=apply(x,1,function(u) atan2(u[2],u[1]))
    s= NaN
    coef=c(q0,s)
    names(coef)=c('q0','s')
  }
  else {  
    namesCoef = c('q0','s','xi')
    initial <- initialization(sim, namesCoef)
    initial=append(initial,rep(0,n)) #adding for the phii
    l = lowerbound(namesCoef)
    l=append(l,rep(-pi,n))
    u = upperbound(namesCoef)
    u=append(u,rep(pi,n))
    mLL <- function(v) mLogLikelihood_SF(v, sim)#HERE v=(v,phii)
    mLPrior <- function(v) mLogPrior(v,namesCoef)
    mLRegularization <- function(v) regularizationphii(v,sim,namesCoef)
    mLPosterior <- function(v) mLL(v)+mLPrior(v)+mLRegularization(v)
    res <- nlminb(initial, mLPosterior, scale = 1, lower = l, upper = u, control=list(trace=0))
    # Get the argmin of the minimization, i.e. the estimates
    l=length(namesCoef)
    coef=res$par[1:l]
    names(coef)=namesCoef
    phi=res$par[(l+1):(l+n)]
  }
  estimator <- setphi(estimator,phi)
  estimator <- setcoefficients(estimator,coef)
  return(estimator)
}