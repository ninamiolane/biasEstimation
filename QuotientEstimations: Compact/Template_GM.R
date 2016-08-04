library(MASS)
library(ggplot2)
library(graphics)
source("EiV_functions.R")

############# PARAMETERS ################################################
n <- 5
template <- 1
phi <- pi/5
clean.y <- c(template*cos(phi),template*sin(phi))
e <-0.02
alpha <- template/(sqrt(2)*e) #SNR ratio

################## PROCESS ########################################
# Data <- generate(n,witherrors=TRUE,my.alpha=alpha,my.eta=pi/50)
# z <-learn(Data,witherrors=TRUE)
# my.plot(Data,z)

## ~~ Unconsistency due to geometry - NO registration error ~~ ##
npts <- 1000
Data.ctcy <- generate(npts,my.alpha=alpha)
z.ctcy <-registration(Data.ctcy)
my.plot(Data.ctcy,z.ctcy)
qmean.ctcy <- c(mean(z.ctcy[,1]),mean(z.ctcy[,2]))
template.est <-sqrt(sum(qmean.ctcy^2))
unconsistency <- template.est-template
cat("unconsistency=",unconsistency,"\n")
templ.ratio <- template.est/template

## ~~ Unconsistency & bias due to geometry - NO registration error ~~ ##
Nexp <- 200
npts <- 1000
qmean <- matrix(0,nrow=Nexp,ncol=2)
for (i in 1:Nexp){
  Data.bias <- generate(npts,my.alpha=alpha)
  z.bias <-registration(Data.bias)
  qmean[i,]<-c(mean(z.bias[,1]),mean(z.bias[,2]))
}
my.hist(template,unconsistency,qmean)

## ~~ Unconsistency due to geometry - WITH registration error ~~ ##
npts<- 1000
Data.ctcy.err <- generate(npts,witherrors=TRUE,my.alpha=alpha,my.eta=pi/50)
z.ctcy.err <-registration(Data.ctcy.err,witherrors=TRUE)
my.plot(Data.ctcy.err,z.ctcy.err)
qmean.ctcy.err <- c(mean(z.ctcy.err[,1]),mean(z.ctcy.err[,2]))
unconsistency.err=sqrt(sum(qmean.ctcy.err^2))-template
cat("unconsistency.err=",unconsistency.err,"\n")

## ~~ Unconsistency & bias due to geometry - WITH registration error ~~ ##
Nexp <- 200
npts <- 100
qmean.err <- matrix(0,nrow=Nexp,ncol=2)
for (i in 1:Nexp){
  Data.bias.err <- generate(npts,witherrors=TRUE,my.alpha=alpha,my.eta=pi/50)
  z.bias.err <-registration(Data.bias.err,witherrors=TRUE)
  qmean.err[i,]<-c(mean(z.bias.err[,1]),mean(z.bias.err[,2]))
}

my.hist(template,unconsistency.err,qmean.err,witherrors=TRUE)
