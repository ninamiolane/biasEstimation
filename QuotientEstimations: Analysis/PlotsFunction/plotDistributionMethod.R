plot.distributions <- function(object,var){
  switch(var,
         'y0'={distribution = object$y0Distribution},
         'phi0'={distribution = object$phi0Distribution},
         'sigma'={distribution = object$sigmaDistribution},
         'eta'={distribution = object$etaDistribution},
         'mse'={distribution = object$mseDistribution},
{print('ERROR: No variable given.')}
  )

title=paste(var," from:", object$method)
hist(distribution,main=title,breaks=10,xlim=c(0,2),xlab="Estimate",
     cex.main=0.9,cex.lab=1,cex=1,cex.axis=1,border="blue",tck=-0.01)
abline(v=mean(distribution),col="black",lwd=1)
}

plot2 <- function(estname,n, eta,sC,var){
  name <- paste0(estname,"_C_n",n,"_eta",eta,"_sC",sC)
  distribution <- readRDS(name)
  plot(distribution,var)
}