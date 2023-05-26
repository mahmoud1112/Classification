library(ggplot2)
library(ggfortify)
library(stats)
library(dplyr)
View(iris) #View the iris data set
#since the cluster analysis is a branch of unsupervised machine learning, we need to unlabel our data
#the species column is to be ignored..
mydata = select(iris,c(1,2,3,4))

#For Your Information::::::#
#WSS plot to choose the optimum number of clusters
#WSS plot function
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
KM = kmeans(mydata, 2) #insert the data and number of clusters of your choice or by the wss recommended optimum number of clusters
#clusterplots:
autoplot(KM,mydata,frame=TRUE)
KM$centers #the centers of clusters
library(cluster)
clusplot(mydata,KM$cluster)
clusplot(mydata,KM$cluster,color=T,shade=T)

#hierarchical clustering:
#Agglomerative method:
x=(mydata[1:10,1:4]) #Perform cluster analysis on the top 10 rows of the data
x
distances=dist(x, method ="euclidean") #compute the distance between data rows
hclusters=hclust(distances)
plot(hclusters)
distances

#Agglomerative method "alternative":
agh=agnes(x,method="complete")
agh$ac #look at the agglomerative coeffecient
#the lower the agglomerative coeffecient, the better the clustering
pltree(agh)

#Divisive method:
dh=diana(x)
dh$dc #Disisive coeffecient
pltree(dh)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

# create function to calculate Manhattan distance
manhattan_dist <- function(a, b){
  dist <- abs(a-b)
  dist <- sum(dist)
  return(dist)
}
# get dist manhatan :
distance=function(x,y){
  dis=manhattan_dist(x, y)
  result <- dis
}

# calculate average distance  :
avg_dist=function(i,c,n){
  sum = 0 
  for (x in 1:nrow(c)){
    d=distance(c[x,],i)
    sum=sum+d
  }
  avg=sum/n
}


# caclulating ai values of all points in cluster ci 
ai_vect_for_ci_points=
  function(ci,nci){
    ai=c()
    for (row in 1:nrow(ci)){ 
      aii= avg_dist(ci[row,],ci,nci)
      ai=append(ai,aii)}
    result<-ai    }



# caclulating bi valuse of all points in cluster ci 
bi_vect_for_ci_points=
  function(ci,ncj,cj){
    bi=c()
    for (row in 1:nrow(ci)){ 
      bii= min(avg_dist(ci[row,],cj,ncj))
      bi=append(bi,bii)}
    result<-bi    }


#caclulating si values : 
si_vect_for_ci_points=
  function(a,b){
    si=c()
    for (i in 1:length(a)){
      max=max(a[i],b[i])
      s=(b[i]-a[i])/max
      si=append(si,s)
    }
    result <- si
    
  }

c=matrix(c(0,0,0,1,2,3,3,3,3,4) ,byrow=TRUE,nrow=5)
d=data.frame(c)
install.packages("cluster" ,dependencies = TRUE)
library(cluster)

k=4
for (i in 2:k){ 
  cat("cluster = ", i)
  print(" ")
  model_l <- kmeans( d , centers = i)
  data_cluster <- cbind(d, cluster = model_l$cluster)
  
  print("all data with thier clusters")
  print(" ")
  
  print(data_cluster)
  print(" ")
  sumaary_c=data.frame(k=c(1),clus=c(1),sum_s=c(5))
  print(" s_values  for points of each  cluster")
  t=c()
  summ=c()
  for (f in 1:i ){
    cat("cluster = ", f)
    print(" ")
    dc=filter(data_cluster, cluster == f )
    all=data.matrix(data_cluster[1:2])
    c_data=data.matrix(dc[1:2])
    a_c=ai_vect_for_ci_points(c_data,length(c_data)-1)
    b_c=bi_vect_for_ci_points(c_data,length(all),all)
    s_c=si_vect_for_ci_points(a_c,b_c)
    print(s_c)
    t=append(t,s_c)
    print(t)
    
    
  }
  
  summ=append(summ,sum(t))
  print("total of this iteration : ")
  print(summ)
}