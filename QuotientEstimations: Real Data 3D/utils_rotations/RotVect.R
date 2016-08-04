RotVect=function(R){
  #M=R%*%t(R)-diag(3)
  #if (tr(M)>1e-20){R=Rot(R);} accounts for numeric errors
  c <- (tr(R)-1)/2
  #   cat("c=")
  #   print(c)
  if (c>1){c=1}
  else if ( c< (-1)) {c=-1}
  theta <- acos(c)
  #   cat("theta=")
  #   print(theta)
  
  
  if (theta< 0.00001){
    fact=0.5*(1+theta^2/6)
    Sr <- fact*(R-t(R))
    r <- c(Sr[3,2], Sr[1,3], Sr[2,1])
  }
  
  else if (abs(theta-pi)< 0.00001){
    r <- c(0,0,0)
    for (i in 1:3){
      sq=1+(R[i,i]-1)/(1-c)
      if (sq<0) {sq=0}
      else if (sq>1) {sq=1}
      r[i] <- sqrt(sq)
    }
    
    # Normalize rotation vector
    Normr <- sqrt(sum(r^2))
    r <- r*theta/Normr
    #set signs with off -diagonal terms of n.n^T
    if (R[1,2]+R[2,1]<0) {r[2] <- -r[2]}
    else if (R[1,3]+R[3,1]<0) {r[3] <- -r[3]}
    #Determine wether r =+/-theta*n
    sinr=as.vector(c(0,0,0))
    sinr[1]=(R[3,2]-R[2,3])
    sinr[2]=(R[1,3]-R[3,1])
    sinr[3]=(R[2,1]-R[1,2])
    #Determine the most significant term
    k=1
    if (abs(sinr[2])>abs(sinr[k])) {k=2}
    else if ( abs(sinr[3])>abs(sinr[k])) {k=3}
    #Choose the sign
    if(sinr[k]*r[k]<0) {r <- -r}
  }
  
  else {
    fact <- 0.5*theta/sin(theta)
    Sr <- fact*(R-t(R))
    r<- c(Sr[3,2], Sr[1,3], Sr[2,1]) # should be column vector
  }
  return(regrot(r))
}