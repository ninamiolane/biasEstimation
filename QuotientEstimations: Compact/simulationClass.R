#simulation Class
library(MASS)
library(graphics)
library(shape)
library(CircStats)

simulation <- function(in_n = 10, in_y0 = 1, in_phi0 =0,
                       in_sR =0, in_eta = 0, in_sC=0.1){
  sim <- list(
    n= in_n,
    y0= in_y0 ,
    phi0= in_phi0,
    sR= in_sR,
    eta= in_eta,
    sC= in_sC,
    quotientData = NULL,
    orbitData = NULL,
    Data = NULL
  )
  class(sim) <- append(class(sim),"simulation")
  return(sim)
}


#Set quotientData
setquotientData <- function(object, newValue){
  UseMethod("setquotientData",object)
}

setquotientData.default <- function(object, newValue){
  print("I do not know how to handle this object.")
  return(object)
}

setquotientData.simulation <- function(object, newValue){
  object$quotientData <- newValue  
  return(object)
}

#set orbitData
setorbitData <- function(object, newValue){
  UseMethod("setorbitData",object)
}

setorbitData.default <- function(object, newValue){
  print("I do not know how to handle this object.")
  return(object)
}

setorbitData.simulation <- function(object, newValue){
  object$orbitData <- newValue  
  return(object)
}

#set Data
setData <- function(object, newValue){
  UseMethod("setData",object)
}

setData.default <- function(object, newValue){
  print("I do not know how to handle this object.")
  return(object)
}

setData.simulation <- function(object, newValue){
  object$Data <- newValue  
  return(object)
}