baby_likelihood <- function(mean,patient,snoise,srot){
  in_exp_noise <- function(r) -LandmarksSqDist(LandmarksRotation(RotMat(r),mean),patient)/(2*snoise^2)
  in_exp_rot <- function(r) -normvec(r)^2/(2*s^2)
  integrand <- function(r) {
    if (normvec(r) < 1){
      exp((in_exp_noise + in_exp_rot))
    } else {
      return(0)
    }
  }
  (1e+40)*Gauss_noise(patient,mean,snoise,r)*truncatedGauss_rot(r,srot)
  tryCatch({res = adaptIntegrate(Vectorize(integrand),lowerLimit=c(-1,-1,-1),upperLimit=c(1,1,1));
            L=(1e-40)*res$value},
           error=function(cond){message(cond); 
                                return(1e+20)},
           finally={NA}) 
  return(-log(L))
}