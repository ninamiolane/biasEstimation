source('./PlotsFunction/plotSimulationMethod.R')
source('./PlotsFunction/plotAnalysis.R')
source('./PlotsFunction/plot_utils.R')
source('./Classes/distributionClass.R')
source('./Classes/simulationClass.R')
library(graphics)
require(mgcv)


# Goal: 
# Compare mle_SS and mle_SF for simulation with xi=0
xi=0
estNames <-list('mle_SF','mle_SS')
n=20
s=10
eta= 0.2

# 1. Role of n, number of observations
fixedparList = list(xi,eta,s)
names(fixedparList)=list('xi','eta','s') #shall respect the order of the parameters!
par(mfrow=c(2,3),oma=c(0,0,2,2))
# Impact of n on the functional model
plotBias('mle_SF','q0','n',fixedparList)
plotSd('mle_SF','q0','n',fixedparList)
plotMse('mle_SF','q0','n',fixedparList)
# Impact of n on the structural model
plotBias('mle_SS','q0','n',fixedparList)
plotSd('mle_SS','q0','n',fixedparList)
plotMse('mle_SS','q0','n',fixedparList)
# Make title
mtitle <- paste0("Comparison of the role of n, when xi=0,eta=",eta,', s=',s)
mtext(mtitle, outer = TRUE, cex = 1)