source('PlotsFunction/plotSimulationMethod.R')
source('PlotsFunction/plotAnalysis.R')
source('PlotsFunction/plot_utils.R')
source('Classes/distributionClass.R')
source('Classes/simulationClass.R')
library(graphics)
require(mgcv)


# Goal: 
# Compare mle_SS and mle_SF for simulation with xi=0
xi=0
estNames <-list('mle_SF','mle_SS')
n=20
s=0.2
eta= 0.2

# 1. Role of n, number of observations
fixedparList = list(xi,eta,s)
names(fixedparList)=list('xi','eta','s') #shall respect the order of the parameters!
par(mfrow=c(2,2),oma=c(0,0,2,2))
# Compare evolution of bias of mle_SS and mle_SF 
plotBias('mle_SF','q0','n',fixedparList)
plotBias('mle_SS','q0','n',fixedparList)
# Compare evolution of std deviation of mle_SS and mle_SF 
plotSd('mle_SF','q0','n',fixedparList)
plotSd('mle_SS','q0','n',fixedparList)
# Make title
mtitle <- paste0("Comparison of the role of n, when xi=0,eta=",eta,', s=',s)
mtext(mtitle, outer = TRUE, cex = 1)

# 2. Role of s, level of noise
fixedparList = list(n,xi,eta)
names(fixedparList)=list('n','xi','eta') #shall respect the order of the parameters!
par(mfrow=c(2,2),oma=c(0,0,2,2))
# Compare evolution of bias of mle_SS and mle_SF 
plotBias('mle_SF','q0','s',fixedparList)
plotBias('mle_SS','q0','s',fixedparList)
# Compare evolution of std deviation of mle_SS and mle_SF 
plotSd('mle_SF','q0','s',fixedparList)
plotSd('mle_SS','q0','s',fixedparList)
# Make title
mtitle <- paste0("Comparison of the role of s, when n=",n,", xi=0,eta=",eta)
mtext(mtitle, outer = TRUE, cex = 1)

# 2. Role of eta
fixedparList = list(n,xi,s)
names(fixedparList)=list('n','xi','s') #shall respect the order of the parameters!
par(mfrow=c(2,2),oma=c(0,0,2,2))
# Compare evolution of bias of mle_SS and mle_SF 
plotBias('mle_SF','q0','eta',fixedparList)
plotBias('mle_SS','q0','eta',fixedparList)
# Compare evolution of std deviation of mle_SS and mle_SF 
plotSd('mle_SF','q0','eta',fixedparList)
plotSd('mle_SS','q0','eta',fixedparList)
# Make title
mtitle <- paste0("Comparison of the role of eta, when n=",n,", xi=0,s=",s)
mtext(mtitle, outer = TRUE, cex = 1)