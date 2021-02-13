# k-means clustering 
# first to check working directory
getwd()
# set working directory
setwd("C:/Users/abhil/OneDrive/Desktop/AI")

#Again check working changed or not 
getwd()
#Now my working directory changed 

#loading the data set
data<-read.csv("abhi.csv")

#checking data table 
 head(data,10)

 #we are intrested to analysing Annual Income and Spending Score of our customer
 
#checking summary of data
data$CustomerID<-as.factor(data$CustomerID)
summary(data) 

X<-data[,4:5]

head(X) 

plot(X[,1],X[,2],main="Annual income vs Spending Score",xlab="Annual Income",ylab="Spending Score",pch=17)
# For above picture we can say there 6 cluster roughly
 
#hierarchical clustering 
 #for obtaining  the optimal number of cluster we use dendrogram
dendo<-hclust(dist(X,method="euclidean"),method="average")
plot(dendo,main="Dendrogram",xlab="Customers",ylab="Distance")
rect.hclust(dendo,k=7)
Y_hc<-cutree(dendo,k=7)
#library("cluster")

clusplot(X,
         Y_hc,lines=0,shade=T,
         labels=2,color=T,plotchar=F,span=T,
         main="cluster client",
         xlab="Anual Income",ylab="spending Score"
)


  
#by observing the our graph  k=6 


# Applying K- means algoritham on data set
#to get same result we use seed fuction 




#Gaussian Mixture dansity
#install.packages("mclust")
library("mclust")
fit<-Mclust(X,G=6,model="VEV")
summary(fit)
plot(fit,what="classification")

par(mfrow=c(2,2))

plot(X[,1],X[,2],main="Raw Data",xlab="Annual Income",ylab="Spending Score",pch=17)
Y_hc<-cutree(dendo,k=6)
clusplot(X,
         Y_hc,lines=0,shade=T,
         labels=2,color=T,plotchar=F,span=T,
         main="Hierarchical",
         xlab="Annual Income",ylab="Spending Score"
)

clusplot(X,
         k_means$cluster,lines=0,shade=T,
         labels=2,color=T,plotchar=F,span=T,
         xlab="Annual Income",ylab="Spending Score",
         main="K-means"
)
plot(fit,what="classification",xlab="Annual Income",ylab="Spending Score",main="Mixture-model")
mtext("All-Model", side = 3, line = -1, outer = TRUE)


