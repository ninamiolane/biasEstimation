
rotvec=function(point){# les VRAIS theta phi
  theta=point[1]
  phi=point[2]
  r <- c(-theta*sin(phi),0,theta*cos(phi))
  return(regrot(r))
}