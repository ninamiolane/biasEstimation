computeH <- function(landmarks1,landmarks2){
  H = matrix(c(0,0,0,0,0,0,0,0,0), nrow=3)
  X1 = Landmarks2Matrix(landmarks1)
  X2 = Landmarks2Matrix(landmarks2)
  K = nrow(X1)
  for (k in 1:K){
    H = H + X1[k,]%*%t(X2[k,])
  }
  return(H)
}