Gauss_noise <- function(patient,mean,snoise,r){ 
sqdist = LandmarksSqDist(LandmarksRotation(RotMat(r),mean),patient)
return(exp(-sqdist/(2*snoise^2)))
}