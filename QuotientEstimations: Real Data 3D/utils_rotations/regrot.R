regrot=function(r){
  phi=sqrt(sum(r^2))
  u=r
  if (phi!=0){
    k0=floor(phi/(2*pi)) #was written "int" in scilab
    u=(phi-2*pi*k0)*r/phi}
  return(u)
}