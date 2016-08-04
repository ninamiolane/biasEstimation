source('Plots/plotSimulationMethod.R')
source('Plots/plotDistributionMethod.R')

## Load the file
# For delta
load('mmperfdelta_Distributions')
load('mmusualdelta_Distributions')
load('mmregdelta_Distributions')
load('mledelta_Distributions')
load('mapdelta_Distributions')
# For gaussian
load('mmperfgauss_Distributions')
load('mmusualgauss_Distributions')
load('mmreggauss_Distributions')
load('mlegauss_Distributions')
load('mapgauss_Distributions')

## Plot the distributions of the template y0
# For delta simulations
par(mfrow=c(2,3))
plot(simdelta)
plot(mmperfdelta_Distributions,'y0')
plot(mmusualdelta_Distributions,'y0')
plot(mmregdelta_Distributions,'y0')
plot(mledelta_Distributions,'y0')
plot(mapdelta_Distributions,'y0')
# For gaussian simulations
par(mfrow=c(2,3))
plot(simgauss)
plot(mmperfgauss_Distributions,'y0')
plot(mmusualgauss_Distributions,'y0')
plot(mmreggauss_Distributions,'y0')
plot(mlegauss_Distributions,'y0')
plot(mapgauss_Distributions,'y0')