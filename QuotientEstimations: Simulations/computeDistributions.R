
computeDistributions.distributions<- function(object,sim){
  # Get the estimation method to evaluate
Nexp=1  
method= object$method
  estimator <- estimator(method=method) #
  # Initialize distributions
  q0Distribution = rep(0,Nexp)
  xiDistribution =rep(0,Nexp)
  phi0Distribution = rep(0,Nexp)
  etaDistribution = rep(0,Nexp)
  sDistribution = rep(0,Nexp)
  
  # Perform Nexp experiments; record the results in the distributions
  cat('Starting the',Nexp,'simulations for estimator:',method,'\n')
  cat('Iterations:')
  for (i in 1:Nexp){ 
    if (floor(i/5)*5==i) cat('#',i,'... ')
    sim <- generate(sim)
    switch(method,
           # MLE estimators
           'mle_FF'={ estimator <- mle_FF(estimator,sim)},
           'mle_FS'={ estimator <- mle_FS(estimator,sim)},
           'mle_SF'={ estimator <- mle_SF(estimator,sim)},
           'mle_SS'={ estimator <- mle_SS(estimator,sim)},
           # MAP estimators
           'map_FF'={ estimator <- map_FF(estimator,sim)},
           'map_FS'={ estimator <- map_FS(estimator,sim)},
           'map_SF'={ estimator <- map_SF(estimator,sim)},
           'map_SS'={ estimator <- map_SS(estimator,sim)},
{print('ERROR: No method given.')})
# print(names(estimator$coefficients))

for (name in names(estimator$coefficients)){
  nameDistribution=paste0(name,'Distribution')
  g.assign(nameDistribution, pos=0, i, value= estimator$coefficients[[name]])
}
  }
DList = list()
for (name in names(estimator$coefficients)){
  nameDistribution=paste0(name,'Distribution')
  DList[[length(DList)+1]] <- get(nameDistribution)
  names(DList)[[length(DList)]] <- name
}

# print(DList)
object <- setdistributions(object,DList)
cat('\n')
return(object)
}

computeDistributions <- function(object, sim=NULL){
  UseMethod("computeDistributions",object)
}

computeDistributions.default <- function(object, sim=NULL){
  print("I do not know how to handle this object.")
  return(object)
}

