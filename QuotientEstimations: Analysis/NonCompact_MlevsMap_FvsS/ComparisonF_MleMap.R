source('./PlotsFunction/plotSimulationMethod.R')
source('./PlotsFunction/plotAnalysis.R')
source('./PlotsFunction/plot_utils.R')
source('./Classes/distributionClass.R')
source('./Classes/simulationClass.R')
library(graphics)
require(mgcv)
require(gsubfn)


# Goal: 
# Compare mle_SF and map_SF for simulation with xi=0
eta=0
estNames <-list('mle_FS','map_FS')
n=20
s=0.2
xi= 0.2

# Role of eta
parList = list(n,xi,eta,s)
names(parList)=list('n','xi','eta','s')  #shall respect the order of the parameters!
par(mfrow=c(2,3),oma=c(0,0,0,0))
# Impact of eta on the functional model
plotAll('mle_FS','phi0','n',parList)
plotAll('mle_FS','phi0','s',parList)
plotAll('mle_FS','phi0','xi',parList)
# Impact of eta on the structural model
plotAll('map_FS','phi0','n',parList)
plotAll('map_FS','phi0','s',parList)
plotAll('map_FS','phi0','xi',parList)
# Make title
mtitle <- paste0("Comparison of the role of eta, when n=",n,", xi=0,s=",s)
mtext(mtitle, outer = TRUE, cex = 1)