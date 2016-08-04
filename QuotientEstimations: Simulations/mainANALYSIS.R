source('Plots/plotSimulationMethod.R')
source('Plots/plotAnalysis.R')#!!
source('Classes/distributionClass.R')
source('Classes/simulationClass.R')
setwd('/home/nmiolane/QuotientEstimations')
library(graphics)
require(mgcv)

estNames <-list('mle_SF','mle_SS')
nList <- list(10,20)
sList <- list(0,0.2)
etaList <-list(0,0.2)#,0.5,3.14)
xiList <- list(0,0.2)

n=20
s=0.2
eta= 0.2
xi = 0.2
sim <- simulation(n=n,s=s,xi=xi,eta=eta)

## Plot histograms: distributions of estimates.
estimate='q0'
varList <- etaList

for (var in varList){
  sim$eta <- var
  par(mfrow=c(2,4),oma=c(0,0,2,2))
  for (estname in estNames){ 
    sim <- simulation( n=n, s=s, xi=xi, eta=var)
    par(mfrow=c(1,1))
    plotEstimator(estname,estimate,sim)
    mtitle <- paste("Simulations: n=",n,", q0=1, phi0=0, xi=",xi,", eta=",var,", s=", s)
    mtext(mtitle, outer = TRUE, cex = 1)
  }
}

## Plot curves of the bias
#sim <- simulation( n=n, s=s,xi=xi, eta=eta)
#test = 's'
#par(mfrow=c(4,1))
#for (estname in estNames){
 # plotBias(estname,estimate='q0', parOfSim=test,sim = sim)
 # abline(h=0,col='green')
#}
#mtitle=paste0("Bias for ",'q0',' wrt ',test)
#mtext(mtitle, outer = TRUE, cex = 1)

# # Hists wrt sigma
# n=10
# eta=0.1
# for (s in sList){
#   par(mfrow=c(1,4),oma=c(0,0,2,2))
#   for (est in estNames){
#     plot2(est,n,eta,s,'q0')}
#   mtitle <- paste("For sigmaC=",s)
#   mtext(mtitle, outer = TRUE, cex = 1)
# }
# # Hists wrt n
# s=0.1
# eta=0.1
# for (n in nList){
#   par(mfrow=c(1,4),oma=c(0,0,2,2))
#   for (est in estNames){ 
#     plot2(est,n,eta,s,'q0')}
#   mtitle <- paste("For n=",n)
#   mtext(mtitle, outer = TRUE, cex = 1)
# }


#names(resList) <- list('mleFunctional','mapFunctional','mleStructural','mapStructural')
# etaVector <- c(0,0.1,0.5,10)
# # patternEtaList <- list("_C0$","_C0.1$","_C0.5$","_C10$")
# for (est in estNames){
#   filenames = list.files(path = "/home/nmiolane/Scripts/R/CompareEstimatorsompactO3/", 
#                          pattern = est)
#   for (file in filenames){ 
#     load(file)
#     
#   }
# }
