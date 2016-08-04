plotEstimator <- function(estname,estimate,sim){
  n= sim$n
  xi = sim$xi
  eta = sim$eta
  s = sim$s
  filename <- paste0('Results/',estname,"_n",n,"_xi",xi,"_eta",eta,"_s",s)
  distributions <- readRDS(filename)
  switch(estimate,
         'q0'={data = distributions$distributions[['q0']]},
         'phi0'={data = distributions$distributions[['phi0']]},
         's'={data = distributions$distributions[['s']]},
         'xi'={data=distributions$distributions[['xi']]},
         'eta'={data = distributions$distributions[['eta']]},
         {print('ERROR: No variable given.')}
  )
  
  title=paste0(estimate," from ", estname)
  hist(data,main=title,breaks=10,xlim=c(0,2),xlab="Estimate",
       cex.main=1,cex.lab=1,cex=1,cex.axis=1,border="blue",tck=-0.01)
  abline(v=mean(data),col="black",lwd=1)
}


##########
plotBias <- function(estname,estimate, parOfSim,sim){
  q0 = 1
  parListname <- paste0(parOfSim,'List') # to have access to etaList
  parList <- get(parListname)
  #cat('length of parList=',length(parList),'\n')
  parOfSimList <- list('n','xi','eta','s')
  names(parOfSimList) <- list('n','xi','eta','s')
  parOfSimList[[parOfSim]] <- NULL
  #cat('length of parOfSimList=',length(parOfSimList),'\n')
  for (parname in parOfSimList){
    assign(parname,sim[[parname]])
    globpattern = paste0(estname,"*",parname,get(parname),"_*")
    pat = glob2rx(globpattern)
  }
  if (parOfSim != 's'){ #quand le pattern doit finir par s0.1, on enleve _.* et $
    pat= substr(pat, 1, nchar(pat)-1)
    pat = paste0(pat,"$")
  }
  else { #on enleve juste $ ajoute par glob2rx
    pat = substr(pat, 1, nchar(pat)-1)}
  #globpattern = paste0(estname,"_C_n",n,"_*_s",s)
  #cat(pat)
  filesList = list.files(path = ".", pattern = pat )
  if (length(parList)!=length(filesList)){cat("Erreur: longueur de listes differentes.")}
  #cat('length of filesList=',length(filesList),'\n')
  
  estimateList = as.list(rep(0,length(filesList)))
  names(estimateList) = filesList
  for (file in filesList){ 
    file =paste0('./Results/',file)
    distributions <- readRDS(file)
    switch(estimate,
           'q0'={estimateList[[file]] = mean(distributions$distributions[['q0']])},
           'phi0'={estimateList[[file]] = mean(distributions$distributions[['phi0']])},
           's'={estimateList[[file]] = mean(distributions$distributions[['s']])},
           'xi'={estimateList[[file]]=mean(distributions$distributions[['xi']])},
           'eta'={estimateList[[file]] = mean(distributions$distributions[['eta']])},
           {print('ERROR: No variable given.')}
    )}
  parVec =unlist(parList)
  estimateVec=unlist(estimateList)
  biasVec = estimateVec - q0
  plot(main=estname,parVec,biasVec,type="b", 
       xlab=parOfSim,ylab=estimate,xlim = c(min(parVec),max(parVec)),ylim=c(-0.5,3))  
}
