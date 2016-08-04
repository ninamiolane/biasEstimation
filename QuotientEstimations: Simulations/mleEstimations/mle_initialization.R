# ordre pour v: q0,phi0,s,xi,eta

initialization <- function(sim,namesCoef){
  x = sim$Data
  q0 = sqrt(x[1,1]^2 +x[1,2]^2)
  phi0 = atan2(x[1,2],x[1,1])
  s = sqrt((x[1,1]-x[2,1])^2 +(x[1,2]-x[2,2])^2)
  xi = abs(sqrt(x[1,1]^2 +x[1,2]^2) - sqrt(x[2,1]^2 +x[2,2]^2))
  eta = abs(atan2(x[1,2],x[1,1]) - atan2(x[2,2],x[2,1]))
  
  initial = NULL
  for (name in namesCoef){ 
  switch(name,
         'q0'={initial[length(initial)+1]=q0},
         'phi0'={initial[length(initial)+1]=phi0},
         's'={initial[length(initial)+1]=s},
         'xi'={initial[length(initial)+1]=xi},
         'eta'={initial[length(initial)+1]=eta },
         )
  names(initial)[length(initial)]=name
  }
  return(initial)
}