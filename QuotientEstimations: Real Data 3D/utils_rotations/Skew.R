Skew=function(r){
  r <-regrot(r) 
  S=matrix(c(0, -r[3], r[2],
             r[3], 0, -r[1],
             -r[2], r[1], 0),
           ncol=3,nrow=3,byrow=T)
  return(S)
}