source("utils.R")
source("likelihoods.R")
source("priors.R")
source('estimatorClass.R')
source('simulationClass.R')
source('generateMethod.R')
source('estimationMethod.R')
source('estimationMaxmax.R')
source('estimationMleMap.R')
source('distributionClass.R')
source('computeDistributionsMethod.R')
require(mgcv)

set.seed(1234)
# Number of simulations
Nexp = 5
# Structure of simulation
estNames <-list('mleF','mleS','mapF','mapS')
nList <- list(10)#,20,50)
sigmaCList <- list(0)#,0.2,1,10)
etaList <-list(0)#,0.2,1,3.14)
for (n in nList){ 
  for (sC in sigmaCList){ 
    for (eta in etaList){
      sim <- simulation(in_n=n, in_eta=eta, in_sC=sC)
      sim <- generate(sim)  
      cat('#### Parameters: n=',n,', sC=',sC,', eta=', eta,'\n')
      #plot(sim)
      for (est in estNames){
        name <- paste0(est,'_C_n',n,'_eta',eta,'_sC',sC)
        # Initialize the estimators
        assign(name, distributions(method=est))
        # Perform the estimation
        assign(name, computeDistributions(get(name),sim))
        # Save in file
        cat('Save in file: ',name,'\n')
       # saveRDS(get(name), name)
      }
    }
  }
}
