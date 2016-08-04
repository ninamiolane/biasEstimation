source('./PlotsFunction/plotSimulationMethod.R')
source('./PlotsFunction/plotAnalysis.R')
source('./PlotsFunction/plot_utils.R')
source('./Classes/distributionClass.R')
source('./Classes/simulationClass.R')
library(graphics)
require(mgcv)


# Goal: 
# Compare mle_SS and mle_SF for simulation with xi=0
eta=0
estNames <-list('mle_FS','mle_SS')
n=20
s=0.2
xi= 0.2

# 2. Role of s, level of noise
fixedparList = list(n,xi,eta)
names(fixedparList)=list('n','xi','eta') #shall respect the order of the parameters!
par(mfrow=c(2,3),oma=c(0,0,2,2))
# Compare evolution of bias of mle_SS and mle_SF 
plotBias('mle_FS','phi0','s',fixedparList)
plotSd('mle_FS','phi0','s',fixedparList)
plotMse('mle_FS','phi0','s',fixedparList)
# Compare evolution of std deviation of mle_SS and mle_SF 
plotBias('mle_SS','phi0','s',fixedparList)
plotSd('mle_SS','phi0','s',fixedparList)
plotMse('mle_SS','phi0','s',fixedparList)
# Make title
mtitle <- paste0("Comparison of the role of s, when n=",n,", xi=", xi, ",eta=",eta)
mtext(mtitle, outer = TRUE, cex = 1)