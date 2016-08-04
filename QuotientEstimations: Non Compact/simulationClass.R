###############################################
## simulation Class for NON compact orbits ####
###############################################

library(MASS)
library(graphics)
library(shape)
library(CircStats)
#Convention pour la representation dans le quotient; conv est le rayon du cercle.
conv=0.5

simulation <- function(in_n = 10, in_y0 = 0, in_phi0 =0,in_sigma =0.1, in_eta = 0){

  sim <- list(
    n= in_n,
    y0= in_y0 ,
    phi0= in_phi0,
    sigma= in_sigma,
    eta= in_eta,
    poses = NULL,
    Data = NULL,
    registeredData = NULL
  )
  class(sim) <- append(class(sim),"simulation")
  return(sim)
}


setposes <- function(object, newValue){
  UseMethod("setposes",object)
}

setposes.default <- function(object, newValue){
  print("I do not know how to handle this object.")
  return(object)
}

setposes.simulation <- function(object, newValue){
  object$poses <- newValue  
  return(object)
}


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

