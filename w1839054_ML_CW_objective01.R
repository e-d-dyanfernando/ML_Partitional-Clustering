#Install packages
#install.packages("readxl")
#install.packages("stats")
#install.packages("ggplot2")
#install.packages("ggfortify")
#install.packages("NbClust")
#install.packages("knitr")
install.packages("factoextra")
install.packages("ClusterR")
install.packages("cluster")
install.packages("caret")

#Import libraries
library(readxl)
library(stats)
library(dplyr , warn.conflicts = FALSE)
library(ggplot2)
library(ggfortify)
library(NbClust)
library(knitr)
library(factoextra)
library(ClusterR)
library(cluster)
library(caret)

#Importing dataset 
#https://www.statmethods.net/input/importingdata.html
dataset <- read_excel("Whitewine_v2.xlsx")
View(dataset)

names(dataset) <- make.names(names(dataset), unique = TRUE)
names(dataset)

boxplot(dataset)

TSDQ1 <- quantile(dataset$total.sulfur.dioxide, .25)
TSDQ3 <- quantile(dataset$total.sulfur.dioxide, .75)
TSDIQR <-IQR(dataset$total.sulfur.dioxide)

FSDQ1 <- quantile(dataset$free.sulfur.dioxide, .25)
FSDQ3 <- quantile(dataset$free.sulfur.dioxide, .75)
FSDIQR <-IQR(dataset$free.sulfur.dioxide)

CQ1 <- quantile(dataset$chlorides, .25)
CQ3 <- quantile(dataset$chlorides, .75)
CIQR <-IQR(dataset$chlorides)

DQ1 <- quantile(dataset$density, .25)
DQ3 <- quantile(dataset$density, .75)
DIQR <-IQR(dataset$density)

PQ1 <- quantile(dataset$pH, .25)
PQ3 <- quantile(dataset$pH, .75)
PIQR <-IQR(dataset$pH)

SQ1 <- quantile(dataset$sulphates, .25)
SQ3 <- quantile(dataset$sulphates, .75)
SIQR <-IQR(dataset$sulphates)

AQ1 <- quantile(dataset$alcohol, .25)
AQ3 <- quantile(dataset$alcohol, .75)
AIQR <-IQR(dataset$alcohol)

RSQ1 <- quantile(dataset$residual.sugar, .25)
RSQ3 <- quantile(dataset$residual.sugar, .75)
RSIQR <-IQR(dataset$residual.sugar)

CAQ1 <- quantile(dataset$citric.acid, .25)
CAQ3 <- quantile(dataset$citric.acid, .75)
CAIQR <-IQR(dataset$citric.acid)

VAQ1 <- quantile(dataset$volatile.acidity, .25)
VAQ3 <- quantile(dataset$volatile.acidity, .75)
VAIQR <-IQR(dataset$volatile.acidity)

FAQ1 <- quantile(dataset$fixed.acidity, .25)
FAQ3 <- quantile(dataset$fixed.acidity, .75)
FAIQR <-IQR(dataset$fixed.acidity)

#component analysis data set
#Removing outliers 
#https://www.r-bloggers.com/2021/09/how-to-remove-outliers-in-r-3/
no_outliers <- subset(dataset, dataset$total.sulfur.dioxide > (TSDQ1 - 1.5*TSDIQR) & dataset$total.sulfur.dioxide < (TSDQ3 +1.5*TSDIQR)
                      & dataset$free.sulfur.dioxide > (FSDQ1 - 1.5*FSDIQR) & dataset$free.sulfur.dioxide < (FSDQ3 +1.5*FSDIQR)
                      & dataset$chlorides > (CQ1 - 1.5*CIQR) & dataset$chlorides < (CQ3 +1.5*CIQR)
                      & dataset$density > (DQ1 - 1.5*DIQR) & dataset$density < (DQ3 +1.5*DIQR)
                      & dataset$pH > (PQ1 - 1.5*PIQR) & dataset$pH < (PQ3 +1.5*PIQR)
                      & dataset$sulphates > (SQ1 - 1.5*SIQR) & dataset$sulphates < (SQ3 +1.5*SIQR)
                      & dataset$alcohol > (AQ1 - 1.5*AIQR) & dataset$alcohol < (AQ3 +1.5*AIQR)
                      & dataset$residual.sugar > (RSQ1 - 1.5*RSIQR) & dataset$residual.sugar < (RSQ3 +1.5*RSIQR)
                      & dataset$citric.acid > (CAQ1 - 1.5*CAIQR) & dataset$citric.acid < (CAQ3 +1.5*CAIQR)
                      & dataset$volatile.acidity > (VAQ1 - 1.5*VAIQR) & dataset$volatile.acidity < (VAQ3 +1.5*VAIQR)
                      & dataset$fixed.acidity > (FAQ1 - 1.5*FAIQR) & dataset$fixed.acidity < (FAQ3 +1.5*FAIQR)
)

dim(no_outliers)
boxplot(no_outliers)

#Normalization 
#https://www.statology.org/how-to-normalize-data-in-r/
dataset_scaled <- as.data.frame(scale(no_outliers[1:11]))
boxplot(dataset_scaled)

#Clustering 
#https://www.rdocumentation.org/packages/NbClust/versions/3.0/topics/NbClust
noOfClusters = NbClust(dataset_scaled, distance = "euclidean", min.nc = 2,max.nc = 4, method = "kmeans", index = "all")

#Elbow method 
#https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
k = 2:4
WSS_No_of_CLusters = sapply(k, function(k) {kmeans(dataset_scaled, centers=k)$tot.withinss})
plot(k, WSS_No_of_CLusters, type="l", xlab= "Number of k", ylab="Within sum of squares")

#Silhouette method 
#https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
fviz_nbclust(dataset_scaled, kmeans, method='silhouette')

#Analysis for each k attempts 
#https://www.datanovia.com/en/blog/k-means-clustering-visualization-in-r-step-by-step-guide/
# k = 2
kmeansClusters <- kmeans(dataset_scaled, centers = 2, nstart = 20)
kmeansClusters$centers

str(kmeansClusters)

fviz_cluster(kmeansClusters, data = dataset_scaled)
kmeansClusters$cluster
conMat = table(no_outliers$quality,kmeansClusters$cluster)
print(conMat)
columnC<-factor(no_outliers$quality)
qualityC<-as.numeric(columnC)
confusionMatrix(data=as.factor(c(as.factor(kmeansClusters$cluster))), reference=as.factor(qualityC))

# k = 3
kmeansClusters <- kmeans(dataset_scaled, centers = 3, nstart = 20)
kmeansClusters$centers

str(kmeansClusters)

fviz_cluster(kmeansClusters, data = dataset_scaled)
kmeansClusters$cluster
conMat = table(no_outliers$quality,kmeansClusters$cluster)
print(conMat)
columnC<-factor(no_outliers$quality)
qualityC<-as.numeric(columnC)
confusionMatrix(data=as.factor(c(as.factor(kmeansClusters$cluster))), reference=as.factor(qualityC))

# k = 4
kmeansClusters <- kmeans(dataset_scaled, centers = 4, nstart = 20)
kmeansClusters$centers

str(kmeansClusters)

fviz_cluster(kmeansClusters, data = dataset_scaled)
kmeansClusters$cluster
conMat = table(no_outliers$quality,kmeansClusters$cluster)
print(conMat)
columnC<-factor(no_outliers$quality)
qualityC<-as.numeric(columnC)
confusionMatrix(data=as.factor(c(as.factor(kmeansClusters$cluster))), reference=as.factor(qualityC))

#Principle Component Analysis(PCA)
#https://medium.com/@zullinira23/implementation-of-principal-component-analysis-pca-on-k-means-clustering-in-r-794f03ec15f
#PCA
pca_applyed = prcomp(dataset[1:11], scale = TRUE)
summary(pca_applyed)
ggbiplot(pca_applyed, labels=rownames(dataset))
data_transform = as.data.frame(-pca_applyed$x[,8:11])

fviz_pca_var(pca_applyed,
             col.var = "contrib", # Color by contributions to the PC
             repel = TRUE     # Avoid text overlapping
)

fviz_nbclust(data_transform, kmeans, method = 'wss')

kmeansClusters <- kmeans(data_transform, centers = 2, nstart = 20)

fviz_cluster(kmeansClusters, data = data_transform)

