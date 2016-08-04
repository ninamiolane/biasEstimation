#Action of the rotations on the K landmarks

LandmarksRotation <- function(R, landmarks){
  K = length(landmarks)/3
  rotated.landmarks = landmarks
  for (k in 1:K) {
  rotated.landmarks[(3*k-2):(3*k)]=(landmarks[(3*k-2):(3*k)])%*%t(R) #line vectors
}
return(rotated.landmarks)
}