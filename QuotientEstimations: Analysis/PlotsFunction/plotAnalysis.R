plotEstimator <- function(estname,estimate,sim){
  data <- getdistributionFromSim(estimate,estname,sim)
  title=paste0(estimate," from ", estname)
  hist(data,main=title,breaks=10,xlim=c(0,2),xlab="Estimate",
       cex.main=1,cex.lab=1,cex=1,cex.axis=1,border="blue",tck=-0.01)
  abline(v=mean(data),col="black",lwd=1)
}


##########
plotBias <- function(estname,estimate,par,fixedparList){
  q0 = 1
  filesList <- getfilesList(estname,fixedparList)
  #print(filesList)
  distributionList <- lapply(filesList, function(file) getdistributionFromFile(estimate,file))
  biasList <- lapply(distributionList, function(x) mean(x) -q0)
  parList <- lapply(filesList, function(file) getvalue(file,par) )
  biasVec <- unlist(biasList)
  parVec <- sort(unlist(parList))
  #title <-paste0('Bias for ',estname)
  switch(estimate,
        'q0'={ylimit=c(-0.5,3)},
        'phi0'={ylimit=c(-3.14,3.14)},
        )
  plot(parVec,biasVec,type="b", 
       xlab=par,ylab=estimate,xlim = c(min(parVec),max(parVec)),ylim=ylimit, 
       mgp = c(3, 0.5, 0),ann=FALSE, col='blue') 
}

plotSd <- function(estname,estimate,par,fixedparList){
  q0 = 1
  filesList <- getfilesList(estname,fixedparList)
  #print(filesList)
  distributionList <- lapply(filesList, function(file) getdistributionFromFile(estimate,file))
  sdList <- lapply(distributionList, function(x) sd(x))
  parList <- lapply(filesList, function(file) getvalue(file,par) )
  sdVec <- unlist(sdList)
  parVec <- sort(unlist(parList))
  #title <-paste0('Std Dev. for ',estname)
  switch(estimate,
         'q0'={ylimit=c(-0.5,3)},
         'phi0'={ylimit=c(-3.14,3.14)},
  )
  plot(parVec,sdVec,type="b", 
       xlab=par,ylab=estimate,xlim = c(min(parVec),max(parVec)),ylim=ylimit, 
       mgp = c(3, 0.5, 0), ann=FALSE, col='darkgreen') 
 # mtext(side = 1, text = par, line = 1.5)
  #abline(h=0,col='green')
}

plotMse <- function(estname,estimate,par,fixedparList){
  q0 = 1
  filesList <- getfilesList(estname,fixedparList)
  #print(filesList)
  distributionList <- lapply(filesList, function(file) getdistributionFromFile(estimate,file))
  mseList <- lapply(distributionList, function(x) mean((x -q0)^2))
  parList <- lapply(filesList, function(file) getvalue(file,par) )
 mseVec <- unlist(mseList)
  parVec <- sort(unlist(parList))
  #title <-paste0('MSE for ',estname)
  switch(estimate,
         'q0'={ylimit=c(-0.5,3)},
         'phi0'={ylimit=c(-3.14,3.14)},
  )
  plot(parVec,mseVec,type="b", 
       xlab=par,ylab=estimate,xlim = c(min(parVec),max(parVec)),ylim=ylimit,
       mgp = c(3, 0.5, 0), ann=FALSE, col='red') 
 mtext(side = 1, text = par, line = 1.5)
  #abline(h=0,col='green')
}

plotAll <- function(estname,estimate,par,parList){
  parList[par] <-NULL
  # Impact of eta on the structural model
  plotBias(estname,estimate,par,parList)
  par(new=TRUE)
  plotSd(estname,estimate,par,parList)
  par(new=TRUE)
  plotMse(estname,estimate,par,parList)
  abline(h=0,col='green')
}