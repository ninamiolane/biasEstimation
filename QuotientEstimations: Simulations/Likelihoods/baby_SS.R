library(cubature) #for double integrals
#Note: adaptIntegrate does NOT take Inf as a upperLimit

baby_SS_xi0 <- function(v,x){
  if (any(is.nan(v))||(v['eta']==0)||(v['s']==0)) return(1e+20)
  integrand <- function(phi) (1e+40)*probaM(v,x,v['q0'],phi)*probaO(v,x,phi)
  tryCatch({res = integrate(Vectorize(integrand),-pi+v['phi0'],pi+v['phi0']);
            L=(1e-40)*res$value},
           error=function(cond){message(cond); 
                                return(1e+20)},
           finally={NA}) 
  if (L==0) return(1e+20)
  return(-log(L))
}

baby_SS_xi0_etaInf <- function(v,x){
  if (any(is.nan(v))||(v['s']==0)) return(1e+20)
  integrand <- function(phi) (1e+40)*probaM(v,x,v['q0'],phi)/(2*pi)
  tryCatch({res = integrate(Vectorize(integrand),-pi,pi);
            L=(1e-40)*res$value},
           error=function(cond){message(cond); return(1e+20)},
           finally={NA}) 
  if (L==0) {return(1e+20)}
  return(-log(L))
}

baby_SS_eta0 <- function(v,x){
  if (any(is.nan(v))||(v['xi']==0)||(v['s']==0)) return(1e+20)
  integrand <- function(q) (1e+40)*probaM(v,x,q,v['phi0'])*probaQ(v,x,q)
  tryCatch({res = integrate(Vectorize(integrand),0,Inf);
            L=(1e-40)*res$value},
           error=function(cond){message(cond);return(1e+20)},
           finally={NA}) 
  if (L==0) {return(1e+20)}
  return(-log(L))
}

baby_SS_etaInf <- function(v,x){
  if (any(is.nan(v))||(v['xi']==0)||(v['s']==0)) return(1e+20)
  integrand <- function(z) (1e+40)*probaM(v,x,z[1],z[2])*probaQ(v,x,z[1])/(2*pi)
  L= adaptIntegrate(integrand, lowerLimit= c(0, -pi), upperLimit=c(1e+30, pi))$integral #write Inf=1e+30
#   print(L)
  if (L==0) return(1e+20)
  return(-(1e-40)*log(L))
}


baby_SS <- function(v,x) {
  if (any(is.nan(v))||(v['xi']==0)||(v['eta']==0)||(v['s']==0)) return(1e+20)  
  integrand <- function(z) (1e+40)*probaM(v,x,z[1],z[2])*probaQ(v,x,z[1])*probaO(v,x,z[2])
  L = adaptIntegrate(integrand, lowerLimit=c(0,-pi ),upperLimit= c(1e+30, pi))$integral
  if (L==0) {return(1e+20)}
  return(-(1e-40)*log(L))# to have a MINIMIZATION procedure
}

# plot(Vectorize(in_expQ),-pi+phi0,phi0+pi);
# plot(Vectorize(in_expO),-pi+phi0,phi0+pi);
# plot(Vectorize(probaO),-pi+phi0,phi0+pi);
# plot(Vectorize(probaQ),-pi+phi0,phi0+pi);
# plot(Vectorize(integrand),-pi+phi0,pi+phi0)
