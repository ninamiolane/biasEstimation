# Maximum likelihood estimation for FS

mle_FS <- function(estimator,sim){
  x = sim$Data
  n=sim$n
  if (sim$s ==0){ #CLOSED FORM
    q= apply(x,1, function(u) sqrt(u[1]^2+u[2]^2))
    phi0 = mean(apply(x,1,function(u) atan2(u[2],u[1])))
    eta = NaN
    coef <-c(phi0,eta)
    names(coef) <-c('phi0', 'eta')
  }
  
  else if (sim$eta ==0 ){ #Closed forms here 
    num= sum(apply(x,1,function(u) 2*u[1]*u[2]))
    denom = sum(apply(x,1, function(u) u[1]^2-u[2]^2))
    phi0=atan2(denom,num)/2
    q= apply(x,1, function(u) u[1]*cos(phi0)+u[2]*sin(phi0))
    s= NaN #not yet
    coef=c(phi0,s)
    names(coef)=c('phi0','s')
  }
  else if (sim$eta==Inf){ 
    flag= 'etaInf'
    namesCoef <- c('phi0','s')
    initial <- initialization(sim,namesCoef)
    initial=append(initial,rep(0,n)) #adding for the qi
    l = lowerbound(namesCoef)
    l=append(l,rep(0,n))
    u = upperbound(namesCoef)
    l=append(l,rep(Inf,n))
    mLL <- function(v) mLogLikelihood_FS(v, sim, flag)#HERE v=(v,qi)
    res <- nlminb(initial, mLL, scale = 1,lower = l, upper = u,control=list(trace=0))
    # Get the argmin of the minimization, i.e. the estimates
    l=length(namesCoef)
    coef=res$par[1:l]
    names(coef)=namesCoef
    q=res$par[l+1:l+n]
  }
  else { #TODO     
    flag= 'general'
    namesCoef <- c('phi0','s','eta')
    initial <- initialization(sim,namesCoef)
    initial=append(initial,rep(0,n)) #adding for the qi
    l = lowerbound(namesCoef)
    l=append(l,rep(0,n))
    u = upperbound(namesCoef)
    u=append(u,rep(Inf,n))
    mLL <- function(v) mLogLikelihood_FS(v, sim, flag)#HERE v=(v,qi)
    res <- nlminb(initial, mLL, scale = 1,lower = l, upper = u,control=list(trace=0))
    # Get the argmin of the minimization, i.e. the estimates
    l=length(namesCoef)
    coef=res$par[1:l]
    names(coef)=namesCoef
    q=res$par[l+1:l+n]
  }
  
  # Set the estimates in the estimator
  estimator <- setq(estimator,q)
  estimator <- setcoefficients(estimator,coef)
  return(estimator)
}