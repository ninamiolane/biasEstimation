LandmarksSqDist <- function(landmarks1, landmarks2){
  K1 = length(landmarks1)/3
  K2 = length(landmarks2)/3
  if (K1 != K2) {cat("Error: Two objects don't have same number of landmarks. K1=",K1," and K2=",K2,"\n")}
  sqdist = 0
  for (k in 1:K1){
    sqdist = sqdist + normvec(landmarks1[3*k-2:3*k]-landmarks2[3*k-2:3*k])^2
  }
  return(sqdist)
}