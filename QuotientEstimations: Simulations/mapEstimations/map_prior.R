# ordre pour v: q0,phi0,s,xi,eta

mLogPrior <- function(v,namesCoef){
  mLogPrior = 0
  for (name in namesCoef){ 
  switch(name,
         'q0'={mLogPrior=mLogPrior + GaussPrior_q0(v[1])},
         'phi0'={mLogPrior=mLogPrior + GaussPrior_phi0(v[2])},
         's'={mLogPrior=mLogPrior + InvWishartPrior_s(v[3])},
         'xi'={mLogPrior=mLogPrior + InvWishartPrior_xi(v[4])},
         'eta'={mLogPrior=mLogPrior + InvWishartPrior_eta(v[5])},
         )
  }
  return(mLogPrior)
}

