################################################
## Estimation method w.r.t. type of estimator ##
################################################
# Note: same as in the compact orbits case.

estimation <- function(object, sim=NULL)
{
  UseMethod("estimation",object)
}

estimation.default <- function(object, sim=NULL)
{
  print("I do not know how to handle this object.")
  return(object)
}

estimation.estimator <- function(object,sim=NULL)
{
  meth= object$method
  switch(meth, 
         'maxmax with perfect registration'={
           print('Using Maxmax procedure with perfect registration...')
           object <- estimationMaxmaxPerf(object,sim)
         },
         'maxmax usual'={
           print('Using Maxmax usual iterative procedure without regularization...')
           object <- estimationMaxmaxUsual(object,sim)
         },
         'maxmax with regularization'={
           print('Using Maxmax usual iterative procedure with regularization...')
           object <- estimationMaxmaxReg(object,sim)
         },
         'mle Delta'={
           print('Using MLE procedure with Delta model on orbits...')
           object <- estimationMleDelta(object,sim)
         },
         'mle Gaussian'={
           print('Using MLE procedure with Gaussian model on orbits...')
           object <- estimationMleGaussian(object,sim)
         },
         'map Delta'={
           print('Using MAP procedure with Delta model on orbits...')
           object <- estimationMapDelta(object,sim)
         },
         'map Gaussian'={
           print('Using MAP procedure with Gaussian model on orbits...')
           object <- estimationMapGaussian(object,sim)
         },
{
  print('ERROR: No method given for the estimation.')
}
  )
return(object)
}

