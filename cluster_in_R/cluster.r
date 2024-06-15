install.packages("pvclust")
library(mclust)
library(cluster)
library(fpc)
library(pvclust)

#-----------------------------------#
#             call data             #
#-----------------------------------#
getwd()
setwd("G:/R CLASS practice/cluser data")
mydata<- read.csv("data_cluster.csv",header=F)

# Prepare Data
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables

#-----------------------------------#
#        number of clusters         #
#-----------------------------------#
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#-----------------------------------#
#          K-Means Cluster          #
#-----------------------------------#
fit <- kmeans(mydata, 5) #5 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

#-----------------------------------#
#       Ward ClusteClustering       #
#-----------------------------------#
# Ward Hierarchical Clustering
d <- dist(mydata,
          method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")

plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")

#-----------------------------------#
# Ward Clustering with Bootstrapped #
#-----------------------------------#
# Ward Hierarchical Clustering with Bootstrapped p values
#for small dataset 
fit <-pvclust(mydata, method.hclust="ward",
          method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

#-----------------------------------#
#    Cluster Plot for K-Means 5     #
#-----------------------------------#
# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster)

clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)
