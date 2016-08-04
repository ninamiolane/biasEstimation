
## ~~ REAL Distribution of estimator~~ ##
distribution <- function(sim,method){
  y0_estimate <-rep(0,Nexp)

  for (i in 1:Nexp){
    sim <-generate.simulation(sim)

  }

}

## ~~ prepare for BOOTSTRAP distribution of estimator ~~ ##
estimation.fn=function(Data,index,witherrors=FALSE){
  noisy.y <- Data$noisy.y[index,]
  clean.phi <- Data$clean.phi[index]
  boot.Data <-data.frame(clean.phi,I(noisy.y))
  if (witherrors){
    dirty.phi <-Data$dirty.phi[index]#NOTE: bootstrap the residuals instead?
    Data <- cbind(boot.Data,data.frame(dirty.phi))
  }
  boot.estimate <- estimation(boot.Data)
  return(boot.estimate)
}
plot.boot=function(est,est0,template){
  par(mfrow=c(1,1))
  title=paste("Estimate=",est0)
  hist(est,breaks=10,xlab="Bootstrapped Estimate",main=title,border="blue")
  abline(v=template, col = "green", lwd=4)
  abline(v=est0, col = "darkgreen", lwd=4) 
  abline(v=mean(est), col = "darkblue", lwd=4)
  # qqnorm(est)
}