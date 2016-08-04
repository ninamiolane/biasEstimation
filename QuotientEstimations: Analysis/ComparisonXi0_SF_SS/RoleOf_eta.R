source('./PlotsFunction/plotSimulationMethod.R')
source('./PlotsFunction/plotAnalysis.R')
source('./PlotsFunction/plot_utils.R')
source('./Classes/distributionClass.R')
source('./Classes/simulationClass.R')
library(graphics)
require(mgcv)
require(gsubfn)


# Goal: 
# Compare mle_SS and mle_SF for simulation with xi=0
xi=0
estNames <-list('mle_SF','mle_SS')
n=20
s=0.2
eta= 0.2


# Role of eta
fixedparList = list(n,xi,s)
names(fixedparList)=list('n','xi','s') #shall respect the order of the parameters!
par(mfrow=c(2,3),oma=c(0,0,2,2))
# Impact of eta on the functional model
plotBias('mle_SF','q0','eta',fixedparList)
plotSd('mle_SF','q0','eta',fixedparList)
plotMse('mle_SF','q0','eta',fixedparList)
# Impact of eta on the structural model
plotBias('mle_SS','q0','eta',fixedparList)
plotSd('mle_SS','q0','eta',fixedparList)
plotMse('mle_SS','q0','eta',fixedparList)
# Make title
mtitle <- paste0("Comparison of the role of eta, when n=",n,", xi=0,s=",s)
mtext(mtitle, outer = TRUE, cex = 1)