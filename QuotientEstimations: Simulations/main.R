source('Classes/estimatorClass.R')
source('Classes/simulationClass.R')
source('Classes/distributionClass.R')
source('mleEstimations/mle_FF.R')
source('mleEstimations/mle_FS.R')
source('mleEstimations/mle_SF.R')
source('mleEstimations/mle_SS.R')
source('mapEstimations/map_FS.R')
source('mapEstimations/map_SF.R')
source('mapEstimations/map_SS.R')
source('mapEstimations/map_prior.R')
source('mapEstimations/map_regularization.R')
source('mapEstimations/priorsList.R')
source('mleEstimations/mle_initialization.R')
source('mleEstimations/mle_LowerUpper.R')
source('Likelihoods/mLogLikelihoods_SS.R')
source('Likelihoods/mLogLikelihoods_SF.R')
source('Likelihoods/mLogLikelihoods_FS.R')
source('Likelihoods/baby_utils.R')
source('generate.R')
source('computeDistributions.R')
source("utils.R")
require(mgcv)

set.seed(1234)
Nexp = 1
#estNames <-list('mle_FF','mle_FS','mle_SF','mle_SS')#case s=0?
estNames = list('map_SS')
# nList <- list(10, 100, 1000)
# xiList <-list(0, 0.2,0.5, 1, 2, 3.14)
# etaList <-list(0, 0.2,0.5, 1, 2, 3.14)
# sList <- list(0, 0.2, 1, 10)
# 
# for (n in nList){ 
#   for (xi in xiList){ 
#     for (eta in etaList){ 
#       for (s in sList){
        n=3
        xi=0.1
        eta=0.1
        s=0.1
        sim <- simulation(n=n, xi=xi,eta=eta, s=s)
        sim <- generate(sim)  
        cat('# Parameters: n=',n,',xi=',xi, ', eta=', eta,',s=',s,'\n')
        for (est in estNames){
          name <- paste0(est,'_n',n,'_xi',xi,'_eta',eta,'_s',s)
          assign(name, distributions(method=est))
          assign(name, computeDistributions(get(name),sim))
          print(get(name))
          #saveRDS(get(name), name)
        }
#       }
#     }
#   }
# }


# parTable <- read.table("parametersTable.txt", stringsAsFactors = FALSE)
# 
# for(i in intersect(which(parTable[,1] == estname),which(parTable[,2]==n)) {
#   xi <- parTable[i,3]
#   eta <- parTable[i,4]
#   s <- parTable[i,5]
#   output_name <- paste0(estname,'_n',n,'_xi',xi,'_eta',eta,'_s',s)
#   sim <- simulation(n=n,xi=xi, eta=eta, s=s)
#   sim <- generate(sim)  
#   assign(output_name, distributions(method=est))
#   assign(output_name, computeDistributions(get(output_name),sim))
#   write(output_name, file.path("output", output_name), append = FALSE)
# }

