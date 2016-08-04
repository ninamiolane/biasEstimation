mLogLikelihood <- function(Data,mean,snoise,srot){
  L=apply(Data,1, function(x) baby_likelihood(mean,x,snoise,srot))
  return(sum(L))
}