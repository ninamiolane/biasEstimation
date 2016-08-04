plotDependence <- function(filesList,var, varList){
  mean = rep(0,length(filesList))
  for (file in filesList){ 
    object <- get(file)
    switch(varForBias,
           'y0'={mean[i] = mean(object$y0Distribution)},
           'phi0'={mean[i] = mean(object$phi0Distribution)},
           'sigma'={mean[i] = mean(object$sigmaDistribution)},
           'eta'={mean[i] = mean(object$etaDistribution)},
           'mse'={mean[i] = mean(object$mseDistribution)},
{print('ERROR: No variable given.')}
    )}

plot(varForStudy,mean)  
}