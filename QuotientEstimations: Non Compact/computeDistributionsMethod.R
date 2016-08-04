
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
  for (i in 1:Nexp){ 
    if (floor(i/10)*10==i) cat('Iteration #',i,'\n')
    sim <-generate.simulation(sim)
    switch(meth, 
           'maxmax with perfect registration'={
             estimator <- estimationMaxmaxPerf(estimator,sim)
           },
           'maxmax usual'={
             estimator <- estimationMaxmaxUsual(estimator,sim)
           },
           'maxmax with regularization'={
             estimator <- estimationMaxmaxReg(estimator,sim)
           },
           'mle Delta'={
             estimator <- estimationMleDelta(estimator,sim)
           },
           'mle Gaussian'={
             estimator <- estimationMleGaussian(estimator,sim)
           },
           'map Delta'={
             estimator <- estimationMapDelta(estimator,sim)
           },
           'map Gaussian'={
             estimator <- estimationMapGaussian(estimator,sim)
           },
{print('ERROR: No method given.')}
    )
y0Distribution[i] = estimator$coefficients['y0']
phi0Distribution[i] = estimator$coefficients['phi0']
sigmaDistribution[i] = estimator$coefficients['sigma']
etaDistribution[i] = estimator$coefficients['eta']
mseDistribution[i] = estimator$mse
  }
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

