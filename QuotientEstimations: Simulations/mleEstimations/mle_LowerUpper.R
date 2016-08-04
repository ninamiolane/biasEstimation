# ordre pour v: q0,phi0,sigma,xi,eta

lowerbound <- function(namesCoef){
  lower=NULL
  for (name in namesCoef){
    switch(name,
           'q0'={lower[length(lower)+1]=0},
           'phi0'={lower[length(lower)+1]=-pi},
           's'={lower[length(lower)+1]=0},
           'xi'={lower[length(lower)+1]=0},
           'eta'={lower[length(lower)+1]=0},
          )
  }
  return(lower)
}

upperbound <- function(namesCoef){
  upper=NULL
  for (name in namesCoef){
    switch(name,
           'q0'={upper[length(upper)+1]=Inf},
           'phi0'={upper[length(upper)+1]=pi},
           's'={upper[length(upper)+1]=Inf},
           'xi'={upper[length(upper)+1]=Inf},
           'eta'={upper[length(upper)+1]=pi},
    )
  }
  return(upper)
}