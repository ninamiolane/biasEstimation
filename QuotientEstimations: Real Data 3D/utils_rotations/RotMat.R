

RotMat=function(r){
  r <- regrot(r)
  theta=sqrt(sum(r^2))
  #disp("theta=");disp(theta);
  Sr=Skew(r)
  if (theta< 0.00001) {
    s=1-theta^2/6
    k=1/2-theta^2
    R=diag(3)+s*Sr+k*(Sr^2)
  }
  else {
    R=diag(3)+(sin(theta)/theta)*Sr+(1-cos(theta))/(theta^2)*(Sr^2)
  }
  return(R)
}