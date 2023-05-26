#Prediction using Unsupervised learning
#importing libraries
library(ggplot2)
library(ggfortify)
library(stats)
library(dplyr)
library(cluster)
head(iris)
#since the cluster analysis is a branch of unsupervised machine learning, we need to unlabel our data
#the species column is to be ignored..
mydata = select(iris,c(1,2,3,4))

#WSS plot to choose the optimum number of clusters
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
#view wss plot to obtain the optimum number of clusters K:
wssplot(mydata) #Spot the value at the elbow
#perfrom the K-means cluster analysis
KM = kmeans(mydata,3) #insert the data and number of clusters by the wss recommended optimum number of clusters
#clusterplots:
autoplot(KM,mydata,frame=TRUE)
KM$centers #the centers of clusters
clusplot(mydata,KM$cluster,color=T,shade=T)
