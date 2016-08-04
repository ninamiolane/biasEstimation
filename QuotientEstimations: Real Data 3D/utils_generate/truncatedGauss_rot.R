truncatedGauss_rot <- function(r,s){
  if (normvec(r) < 1){
    exp(-normvec(r)^2/(2*s^2))
  } else {
    return(0)
  }
}