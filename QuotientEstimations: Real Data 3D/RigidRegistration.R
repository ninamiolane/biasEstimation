## Function Rigid Registration

RigidRegistration <- function(Data){
  n = nrow(Data)
  K = ncol(Data)/3
  RigidRegistered.Data <- Data
  patient1 = Data[1,]
  bar1 = barycenter(patient1)
  for (i in 2:n) {
    patienti = Data[i,]
    bari <- barycenter(patienti)
    # Recalage par translation sur le patient 1
    patienti <- LandmarksTranslation(bar1-bari,patienti)
    # Recalage par rotation sur le patient 1
    H = computeH(patient1,patienti)
    result=svd(H)
    U=result$u
    D=result$d
    V=result$v
    S = diag(3)
    S[3,3]=det(U)*det(V)#Achtung:sign!
    Ri=U %*% S %*% t(V)
    RigidRegistered.Data[i,] <- LandmarksRotation(Ri,patienti)
  }
  return(RigidRegistered.Data)
}