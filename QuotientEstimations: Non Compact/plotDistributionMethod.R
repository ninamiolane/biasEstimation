plot.distributions <- function(object,var){
  switch(var,
         'y0'={distribution = object$y0Distribution},
         'phi0'={distribution = object$phi0Distribution},
         'sigma'={distribution = object$sigmaDistribution},
         'eta'={distribution = object$etaDistribution},
         'mse'={distribution = object$mseDistribution},
{print('ERROR: No method given.')}
  )

title=paste("Estimate of ",var," from:", object$method)
hist(distribution,main=title,breaks=10,xlim=c(0,1.5),xlab="Estimate",
     cex.main=0.4,cex.lab=0.4,cex=0.4,cex.axis=0.4,border="blue",tck=-0.01)
abline(v=mean(distribution),col="blue",lwd=2)
}