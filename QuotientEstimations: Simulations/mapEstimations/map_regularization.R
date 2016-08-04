# ordre pour v: q0,phi0,s,xi,eta

regularizationphii <- function(v,sim,namesCoef){
  sphii = 0.2
  barphii = 0
  n=sim$n
  l = length(namesCoef)
  vphii = v[(l+1):(l+n)]
  reg = 0
  for (i in 1:n){ 
reg = reg + (vphii[i]-barphii)^2/(2*sphii^2)
  }
  return(reg)
}

regularizationqi <- function(v,sim,namesCoef){
  sqi = 0.2
  barqi = 1
  n=sim$n
  l = length(namesCoef)
  vqi = v[(l+1):(l+n)]
  reg = 0
  for (i in 1:n){ 
    reg = reg + (vqi[i]-barqi)^2/(2*sqi^2)
  }
  return(reg)
}
