args <- commandArgs(trailingOnly = TRUE)
estname <- args[1]
n <- args[2]
n <- as.integer(n)
xi <- args[3]
	xi <- as.numeric(xi)
setwd("/home/nmiolane/QuotientEstimations")
source('Classes/estimatorClass.R')
source('Classes/simulationClass.R')
source('Classes/distributionClass.R')
source('mleEstimations/mle_FF.R')
source('mleEstimations/mle_FS.R')
source('mleEstimations/mle_SF.R')
source('mleEstimations/mle_SS.R')
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

parTable <- read.table("parametersTable", stringsAsFactors = FALSE)

for (i in intersect(intersect(which(parTable[,1] == estname),which(parTable[,2]==n)), which(parTable[,3]==xi))) {
  eta <- parTable[i,4]
  s <- parTable[i,5]
cat('#Parameters for estimator',estname,': n=',n,',xi=',xi,',eta=',eta,',s=',s,'\n')
  output_name <- paste0(estname,'_n',n,'_xi',xi,'_eta',eta,'_s',s)
  sim <- simulation(n=n,xi=xi, eta=eta, s=s)
  sim <- generate(sim)  
  assign(output_name, distributions(method=estname))
  assign(output_name, computeDistributions(get(output_name),sim))
fullname <- paste0('Results/',output_name)
saveRDS(get(output_name),fullname)
 # write(output_name, file.path("Results", output_name), append = FALSE)
}
