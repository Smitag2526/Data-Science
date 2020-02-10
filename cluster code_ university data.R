#load Universities data

mydata1 <- read.csv("/R Excel Sessions/Day 4/Universities.csv")

head(mydata1)

mydata <- scale(mydata1[,2:7])

head(mydata)

d<- dist(mydata,method = "euclidean")#computing the distance matrix
d
fit <- hclust(d,method="average")#Building the algorithm try with centroid

plot(fit) #display dendrogram

groups <- cutree(fit,k=4)#cut tree into 4 clusters

#Draw a dendogram with red borders around 4 clusters

rect.hclust(fit, k=4, border = "red")

#Attach the cluster numbers to Uni

head(mydata1)
clusters = data.frame('Uni'=mydata1[,1],'Cluster'=groups)

View(clusters)

#Code for K-means cluster 
install.packages("plyr")
library(plyr)


x<- runif(50)
y<- runif(50)

data<- cbind(x,y)

View(data)

plot(data)

#First find referance value for k using formula k ~ sqrt(n/2)
sqrt(50/2)
k=5 in this case

###find Optimum K value using below formula (k=5 put in range 2:15)

wss <- c()

for (i in 2:15) wss[i] <-sum(kmeans(data,centers = i)$withinss)

plot(1:15,wss,type = "b",xlab = "No.of clusters",ylab="Avg distance")

#Using elbow plot we have got 7 clusters. So ,put 7 in the below formula

#Cluster Algorithm building
km <- kmeans(data,7) 

km$centers
km$cluster

###################Animated plot##############

install.packages("animation")
library(animation)

windows()
km <- kmeans.ani(data,7)

####################

