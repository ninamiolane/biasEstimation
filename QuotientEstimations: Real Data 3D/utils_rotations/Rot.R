

Rot=function(M){
  result=svd(M)
  u=result$u
  d=result$d
  v=result$v
  R=u %*% t(v)
  if (det(R)<0) {
    s(1,1)=1
    s(2,2)=1
    s(3,3)=-1
    R=u%*%d%*%t(v)}
  return(R)
}