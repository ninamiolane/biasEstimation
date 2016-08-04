in_expM <- function(v,x,q,phi){
#   cat('v[s]=',v['s'],'\n') #phi=Inf ici
#   cat('x=',x,'q=',q,'phi=',phi,'\n')
  res=-sqdist(x,q,phi)/(2*v['s']^2)
#   cat('expM=')
#   print(res)
  return(res)
}
probaM <- function(v,x,q,phi) {
  res=exp(in_expM(v,x,q,phi))/(2*pi*v['s'])
#   cat('probaM=')
#   print(res)
  return(res)
}

in_expQ <- function(v,x,q) -(q-v['q0'])^2/(2*v['xi']^2)
probaQ <- function(v,x,q){
  res=exp(in_expQ(v,x,q))/(sqrt(2*pi)*v['xi'])
#   cat('probaQ=')
#   print(res)
  return(res)
} 

in_expO <- function(v,x,phi) -(phi-v['phi0'])^2/(2*v['eta']^2)
probaO <- function(v,x,phi) exp(in_expO(v,x,phi))/(sqrt(2*pi)*v['eta'])