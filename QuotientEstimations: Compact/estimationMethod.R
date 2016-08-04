library(optimx)

estimation.estimator <- function(object,sim=NULL){
  meth= object$method
  switch(meth, 
         'mleF'={
           print('Using mle Functional...')
           object <- estimationMleFunctional(object,sim)
         },
         'mapF'={
           print('Using map Functional...')
           object <- estimationMapFunctional(object,sim)
         },
         'mleS'={
           print('Using mle Structural...')
           object <- estimationMleStructural(object,sim)
         },
         'mapS'={
           print('Using map Structural...')
           object <- estimationMapStructural(object,sim)
         },
{
  print('ERROR: No method given for the estimation.')
}
  )
return(object)
}

estimation <- function(object, sim=NULL){
  UseMethod("estimation",object)
}

estimation.default <- function(object, sim=NULL){
  print("I do not know how to handle this object.")
  return(object)
}


