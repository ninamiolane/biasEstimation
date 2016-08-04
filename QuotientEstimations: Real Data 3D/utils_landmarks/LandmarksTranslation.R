LandmarksTranslation <- function(t, landmarks){
  K = length(landmarks)/3
  translated.landmarks = landmarks
#   cat(t,"\n")
  for (k in 1:K) {
    translated.landmarks[(3*k-2):(3*k)]=(landmarks[(3*k-2):(3*k)])+t #line vectors

#    cat("length ld=",length((landmarks[(3*k-2):(3*k)])+t),"\n")
  }
  return(translated.landmarks)
}