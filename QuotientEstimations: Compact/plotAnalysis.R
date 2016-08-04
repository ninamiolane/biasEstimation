plotEstimator <- function(estname,estimate,sim){
  n= sim$n
  eta = sim$eta
  sC = sim$sC
  filename <- paste0(estname,"_C_n",n,"_eta",eta,"_sC",sC)
  distributions <- readRDS(filename)
  switch(estimate,
         'y0'={data = distributions$y0Distribution},
         'phi0'={data = distributions$phi0Distribution},
         'sigma'={data = distributions$sigmaDistribution},
         'eta'={data = distributions$etaDistribution},
         'mse'={data = distributions$mseDistribution},
{print('ERROR: No variable given.')}
  )

title=paste0(estimate," from ", estname)
hist(data,main=title,breaks=10,xlim=c(0,2),xlab="Estimate",
     cex.main=1,cex.lab=1,cex=1,cex.axis=1,border="blue",tck=-0.01)
abline(v=mean(data),col="black",lwd=1)
}


##########
plotBias <- function(estname,estimate, parOfSim,sim){
  y0 = 1
  parListname <- paste0(parOfSim,'List') # to have access to etaList
  parList <- get(parListname)
  #cat('length of parList=',length(parList),'\n')
  parOfSimList <- list('n','eta','sC')
  names(parOfSimList) <- list('n','eta','sC')
  parOfSimList[[parOfSim]] <- NULL
  #cat('length of parOfSimList=',length(parOfSimList),'\n')
  globpattern = paste0(estname,"_C")
  for (parname in parOfSimList){
    assign(parname,sim[[parname]])
    globpattern = paste0(globpattern,"*",parname,get(parname),"_*")
    pat = glob2rx(globpattern)
  }
  if (parOfSim != 'sC'){ #quand le pattern doit finir par sC0.1, on enleve _.* et $
   pat= substr(pat, 1, nchar(pat)-1)
   pat = paste0(pat,"$")
  }
  else { #on enleve juste $ ajoute par glob2rx
    pat = substr(pat, 1, nchar(pat)-1)}
  #globpattern = paste0(estname,"_C_n",n,"_*_sC",sC)
  #cat(pat)
  filesList = list.files(path = ".", pattern = pat )
  if (length(parList)!=length(filesList)){cat("Erreur: longueur de listes differentes.")}
  #cat('length of filesList=',length(filesList),'\n')
  
  estimateList = as.list(rep(0,length(filesList)))
  names(estimateList) = filesList
  for (file in filesList){ 
    distributions <- readRDS(file)
    switch(estimate,
           'y0'={estimateList[[file]] = mean(distributions$y0Distribution)},
           'phi0'={estimateList[[file]] = mean(distributions$phi0Distribution)},
           'sigma'={estimateList[[file]] = mean(distributions$sigmaDistribution)},
           'eta'={estimateList[[file]] = mean(distributions$etaDistribution)},
           'mse'={estimateList[[file]] = mean(distributions$mseDistribution)},
{print('ERROR: No variable given.')}
    )}
parVec =unlist(parList)
estimateVec=unlist(estimateList)
biasVec = estimateVec - y0
plot(main=estname,parVec,biasVec,type="b", 
     xlab=parOfSim,ylab=estimate,xlim = c(min(parVec),max(parVec)),ylim=c(-0.005,0.11))  
}