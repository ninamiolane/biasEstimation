baby_SF <- function(v,x,phii){
  if (any(is.nan(v))||(v['xi']==0)||(v['s']==0)) return(1e+20)
  integrand <- function(q) (1e+40)*probaM(v,x,q,phii)*probaQ(v,x,q)
  tryCatch({res = integrate(Vectorize(integrand),0,Inf);
            L=(1e-40)*res$value},
           error=function(cond){message(cond);return(1e+20)},
           finally={NA}) 
  if (L==0) {return(1e+20)}
  return(-log(L))
}
