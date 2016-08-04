library(MASS)
library(ggplot2)
library(graphics)
library(plot3D)
library(pracma)
source('Visualization/plot_sphere.R')
source('Visualization/plot_points.R')
##### LOAD DATA and CREATE DATA.FRAME ####

# # ####################################################################
# ## UNCOMMENT TO GET THE WIND DATA
# data=read.csv("./data/wind_directions.txt",sep=" ", header=TRUE,  fileEncoding = "UCS-2LE")
# data = data[,1:2]
# data = data.matrix(data)  #convert elements of data into numeric
# n = nrow(data)
# for (i in 1:n){
#   for (j in 1:2){
#     data[i,j]=data[i,j]*pi/180
#   }
# }
# # ####################################################################
# # ####################################################################
# ## UNCOMMENT TO GET THE ORBITAL DATA
# data=read.csv("./data/orbitalNormals_planets.txt",sep=" ", header=TRUE,  fileEncoding = "UCS-2LE")
# data = data[,-1]
# data = data.matrix(data)
# data = cart2sph(data)
# data = data[,1:2]
# data[,1]=data[,1]+pi/2
# # ####################################################################

par(mfrow=c(1,2))
plot_sphere(20,50)
plot_points(data[,1],data[,2])

plot_sphere(30,50)
plot_points(data[,1],data[,2])



