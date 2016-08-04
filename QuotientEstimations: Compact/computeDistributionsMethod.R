
computeDistributions.distributions<- function(object,sim){
  # Set nb of iterations for MC convergence
  # Get the estimation method to evaluate
  meth= object$method
  estimator <- estimator(method=meth)
  
  # Initialize distributions
  y0Distribution = rep(0,Nexp)
  phi0Distribution = rep(0,Nexp)
  sigmaDistribution = rep(0,Nexp)
  etaDistribution = rep(0,Nexp)
  mseDistribution = rep(0,Nexp)
  # Perform Nexp experiments; record the results in the distributions
  cat('Starting the',Nexp,'simulations for estimator:',meth,'\n')
  cat('Iterations:')
  for (i in 1:Nexp){ 
    if (floor(i/5)*5==i) cat('#',i,'...')
    sim <-generate.simulation(sim)
    switch(meth, 
           'mleF'={
             estimator <- estimationMleFunctional(estimator,sim)
           },
           'mapF'={
             estimator <- estimationMapFunctional(estimator,sim)
           },
           'mleS'={
             estimator <- estimationMleStructural(estimator,sim)
           },
           'mapS'={
             estimator <- estimationMapStructural(estimator,sim)
           },
{print('ERROR: No method given.')}
    )
y0Distribution[i] = estimator$coefficients['y0']
phi0Distribution[i] = estimator$coefficients['phi0']
sigmaDistribution[i] = estimator$coefficients['sigma']
etaDistribution[i] = estimator$coefficients['eta']
mseDistribution[i] = estimator$mse
  }
cat('\n')
object <- setdistributions(object,y0Distribution,
                           phi0Distribution,
                           sigmaDistribution,
                           etaDistribution,
                           mseDistribution)

return(object)
}

computeDistributions <- function(object, sim=NULL){
  UseMethod("computeDistributions",object)
}

computeDistributions.default <- function(object, sim=NULL){
  print("I do not know how to handle this object.")
  return(object)
}

