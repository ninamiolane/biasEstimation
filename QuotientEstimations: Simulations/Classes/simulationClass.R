#simulation Class
library(MASS)
library(graphics)
library(shape)
library(CircStats)

simulation <- function( n,  q0 = 1, xi, phi0 =0, eta,  s){
  sim <- list(
    n=  n,
    q0=  q0 ,
    phi0=  phi0,
    xi=   xi,
    eta=  eta,
    s=  s,
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
