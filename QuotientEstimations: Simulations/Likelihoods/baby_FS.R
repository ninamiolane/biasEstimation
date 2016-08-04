# baby_FS_eta0: closed form

baby_FS_etaInf <- function(v,x,qi){
  if (any(is.nan(v))||(v['s']==0)) return(1e+20)
  integrand <- function(phi) probaM(v,x,qi,phi)/(2*pi)
  tryCatch({res = integrate(Vectorize(integrand),-pi,pi);
            L=(1e-40)*res$value},
           error=function(cond){message(cond); return(1e+20)},
           finally={NA}) 
  return(-log(L))
}

baby_FS <- function(v,x,qi){
  if (any(is.nan(v))||(v['eta']==0)||(v['s']==0)) return(1e+20)
  integrand <- function(phi) probaM(v,x,qi,phi)*probaO(v,x,phi)
  tryCatch({res = integrate(Vectorize(integrand),-pi+v['phi0'],pi+v['phi0']);
            L=(1e-40)*res$value},
           error=function(cond){message(cond); 
                                return(1e+20)},
           finally={NA}) 
  return(-log(L))
}