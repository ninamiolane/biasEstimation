source('./PlotsFunction/plotSimulationMethod.R')
source('./PlotsFunction/plotAnalysis.R')
source('./PlotsFunction/plot_utils.R')
source('./Classes/distributionClass.R')
source('./Classes/simulationClass.R')
library(graphics)
require(mgcv)
library(gsubfn)


# Goal: 
# Compare mle_SS and mle_SF for simulation with xi=0
eta=0
estNames <-list('mle_FS','mle_SS')
n=20
s=10
xi= 0

# 1. Role of n, number of observations
fixedparList = list(xi,eta,s)
names(fixedparList)=list('xi','eta','s') #shall respect the order of the parameters!
par(mfrow=c(2,3),oma=c(0,0,2,1))
# Compare evolution of bias of mle_SS and mle_SF 
plotBias('mle_FS','phi0','n',fixedparList)
plotSd('mle_FS','phi0','n',fixedparList)
plotMse('mle_FS','phi0','n',fixedparList)
# Compare evolution of std deviation of mle_SS and mle_SF 
plotBias('mle_SS','phi0','n',fixedparList)
plotSd('mle_SS','phi0','n',fixedparList)
plotMse('mle_SS','phi0','n',fixedparList)
# Make title
mtitle <- paste0('Comparison of the role of n, when xi=',xi,', eta= 0, s=',s)
mtext(mtitle, outer = TRUE, cex = 1)