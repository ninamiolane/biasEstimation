barycenter <- function(landmarks){
  DF <- Landmarks2Matrix(landmarks)
  bar = apply(DF,2,mean)
#   cat("bar=",bar,"\n")
  return(bar)
}