Landmarks2Matrix <- function(landmarks,dim=3){
  K = length(landmarks)/dim
  
  if (dim==2){
    xBOOL = rep(c(TRUE,FALSE),K)
    yBOOL = rep(c(FALSE,TRUE),K)
    x = as.vector(landmarks[xBOOL])
    y = as.vector(landmarks[yBOOL])
    DF  <- cbind(x,y)
  } else if (dim==3){ 
  xBOOL = rep(c(TRUE,FALSE,FALSE),K)
  yBOOL = rep(c(FALSE,TRUE,FALSE),K)
  zBOOL = rep(c(FALSE,FALSE,TRUE),K)
  x = as.vector(landmarks[xBOOL])
  y = as.vector(landmarks[yBOOL])
  z = as.vector(landmarks[zBOOL])
  DF  <- cbind(x,y,z)
#   DF <- data.frame(t(x),t(y),t(z))
#   names(DF) <-c("x","y","z")
#   rownames(DF) <- NULL
#   DF <- data.matrix(DF)
  }
  return(DF)
}