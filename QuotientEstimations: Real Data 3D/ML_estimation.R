
ML_estimation <- function(Data,snoise,srot){ 
  K = ncol(Data)/3
  initial <- Data[1,]
  #     print(names(initial))
  l = rep(-2000,3*K)
  u = rep(2000,3*K)
  mLL <- function(v) mLogLikelihood(Data,v,snoise,srot)
  result <- nlminb(initial, mLL, scale = 1, lower = l, upper = u, control=list(trace=0))
  estimated.mean=result$par[1:3*K]
return(estimated.mean)
}