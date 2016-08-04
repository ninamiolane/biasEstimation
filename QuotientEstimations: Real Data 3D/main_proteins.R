# # ####################################################################
# ## UNCOMMENT TO GET THE PROTEIN DATA 3VQK
# data=read.csv("3VQK_atoms.txt",sep=" ", header=FALSE)
# data = data[,7:9]
# data = data[-(which(is.na(data))),]
# N = nrow(data)
# sus = 1.76 # from resolution given
# res = 4.5
# bfac = sqrt(30)
# # ####################################################################

# ####################################################################
## UNCOMMENT TO GET THE PROTEIN DATA 1AXC
data=read.csv("1AXC_atoms.txt",sep=" ", header=FALSE)
bfactors = data[,11]

data = data[,7:9]
data = data[-(which(is.na(data))),]
bfactors = bfactors[-(which(is.na(bfactors)))]
#########################################

N = nrow(data)
sus = 1.76 # from resolution given
res = 4.5
bfac = sqrt(mean(bfactors))/(4*pi)
# ####################################################################

center = apply(data,2,mean)
aux = apply(data,2, function(u) (u-center)^2)
sqdist = apply(aux, 1,sum)
Rg = sqrt(mean(sqdist))

Rg_real = sqrt(Rg^2 - 3*(N-1)/N*bfac^2)
cat(" Measured Radius of Gyration is: ", Rg, " \n Corrected Radius of Gyration is:", Rg_real)
