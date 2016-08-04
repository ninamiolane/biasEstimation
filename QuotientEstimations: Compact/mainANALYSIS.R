source('plotSimulationMethod.R')
source('plotAnalysis.R')#!!
source('distributionClass.R')
source('simulationClass.R')
source('generateMethod.R')
library(graphics)
require(mgcv)

sCList <- list(0,0.2, 1,10)
etaList <-list(0,0.2,1,3.14)
varList <- etaList

n=10
sC=0.2
eta= 0.2
sim <- simulation(in_n=n,in_sC=sC,in_eta=eta)
sim <- generate(sim)

estNames <-list('mleF','mapF')
par(mfrow=c(1,3),oma=c(0,0,2,2))
plot(sim)
## Plot histograms: distributions of estimates.
estimate='y0'

# # for (var in varList){
# #   sim$eta <- var
#   for (estname in estNames){ 
#     plotEstimator(estname,estimate,sim)
#     mtitle <- paste("Simulations: n=",n,", y0=1, phi0=0, eta=",eta,", sC=", sC)
#     mtext(mtitle, outer = TRUE, cex = 1)
#   }
# # }

## Which estimator are precise?
test = 'sC'
par(mfrow=c(1,1),oma=c(0,0,2,2))
sim <- simulation(in_n=n,in_sC=sC,in_eta=eta)
par(mfrow=c(1,2))
for (estname in estNames){
  plotBias(estname,estimate='y0', parOfSim=test,sim = sim)
  abline(h=0,col='green')
}
mtitle=paste0("Bias for ",'y0',' wrt ',test)
mtext(mtitle, outer = TRUE, cex = 1)


## Which estimator are consistent?
test = 'n'
nList <- list(10,20,50)
par(mfrow=c(1,1),oma=c(0,0,2,2))
sim <- simulation(in_n=n,in_sC=sC,in_eta=eta)
par(mfrow=c(1,2))
for (estname in estNames){
  plotBias(estname,estimate='y0', parOfSim=test,sim = sim)
  abline(h=0,col='green')
}
mtitle=paste0("Bias for ",'y0',' wrt ',test)
mtext(mtitle, outer = TRUE, cex = 1)

## Dependence in the distribution in the orbit?
test = 'eta'
sim <- simulation(in_n=n,in_sC=sC,in_eta=eta)
par(mfrow=c(1,2))
for (estname in estNames){
  plotBias(estname,estimate='y0', parOfSim=test,sim = sim)
  abline(h=0,col='green')
}
mtitle=paste0("Bias for ",'y0',' wrt ',test)
mtext(mtitle, outer = TRUE, cex = 1)
