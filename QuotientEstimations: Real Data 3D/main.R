library(rpart)
source("source_all.R")
library(shapes)
library(R.matlab)
##### LOAD DATA and CREATE DATA.FRAME ####

# data(steroids)
# Steroids <- steroids$x
# 
# writeMat("Steroids.mat",Steroids=Steroids)

# data(brains)
# Brains <- brains$x
# 
# writeMat("Brains.mat",Brains=Brains)


# ####################################################################
## UNCOMMENT TO GET THE PELVIS DATA
data=read.csv("E4SA_AllSort.txt",sep=",", header=TRUE)
rownames(data)=data[,1]
data= data[,-1]
data = data[,1:48]
# ####################################################################



# ####################################################################
# ## UNCOMMENT TO GET THE MONKEY ONH DATA
# data =  read.table("monkey_optical_nerve_landmarks.txt", sep="\t", header=T)
# data=data[,-1] #Remove filenames from the data.frame
# data[16,13:15] = data[15,13:15] #replace the NA
# ####################################################################

data = data.matrix(data) #convert elements of data into numeric
n = nrow(data)
K = ncol(data)/3
#Pelvis.mean = apply(data,2,mean)

# dataPlot(data)

## Max-Max algorithm
RigidRegistered.data = RigidRegistration(data)
MaxMax.mean = apply(RigidRegistered.data,2,mean)

dataPlot(RigidRegistered.data)
## EM algorithm



