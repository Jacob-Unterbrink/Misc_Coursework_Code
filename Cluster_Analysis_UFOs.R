# Jacob Unterbrink
# Intelligent Data Analytics: Homework 8
# Data: https://www.kaggle.com/datasets/NUFORC/ufo-sightings
# Deadline: 12/05/2021

library(tidyverse)
library(cluster)
library(useful)
library(NbClust)
library(rgl)
library(Rtsne)
library(plotly)

ufo <- read.csv("scrubbed.csv")

# -------------------------------------------------------------
# Cleaning
glimpse(ufo)
anyNA(ufo)

ufo$latitude <- as.double(ufo$latitude)
ufo$duration..seconds. <- as.numeric(ufo$duration..seconds.)

summary(ufo)

# -------------------------------------------------------------
# Kmeans clustering
# Can only use on numeric preds
ufoNumeric <- ufo %>% select_if(is.numeric)
ufoNumeric <- scale(ufoNumeric)

ufoKM <- kmeans(ufoNumeric,3, nstart=10)          

ufoKM$centers
ufoKM$size

wssplot <- function(data, nc=15){                    
  
  par(mfrow=c(1,2))
  
  wss <- NULL  
  pctExp <-NULL
  
  for (k in 1:nc)
  {
    kclus <- kmeans(data, centers=k)
    wss[k] <- kclus$tot.withinss      #store the total within SSE for given k
    pctExp[k] <- 1-wss[k]/kclus$totss
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  plot(1:nc, pctExp, type="b", xlab="Number of Clusters",
       ylab="Pct Explained")
  
  par(mfrow=c(1,1))
}
wssplot(ufoNumeric)

clusFit<-FitKMeans(ufoNumeric,max.clusters=30,nstart=20)
clusFit
PlotHartigan(clusFit)

# The below claims 5 is the best number of clusters
numClust <- NbClust(ufoNumeric[1:1000,], method="kmeans")

# Fit kmeans
ufoKM2 <- kmeans(ufoNumeric, 5)

# extraxt cluster info
ufoKM2$centers
ufoKM2$size
ufoKM2$betweenss
ufoKM2$withinss

ufoNumeric <- as.data.frame(ufoNumeric)
ufoNumeric$cluster <- as.factor(ufoKM2$cluster)
head(ufoNumeric)

# plot data
ggplot(ufoNumeric, aes(x = longitude, y = latitude, col = cluster)) + 
  geom_point() + xlab("Longitude") + ylab("Latitude")

# -------------------------------------------------------------
# K medoids cluster analysis
# Gowers distance for mixed data
ufoSub <- ufo[75333:nrow(ufo),]
view(ufoSub)
gowersMat <- as.matrix(daisy(ufoSub, stand = T))

wssplot <- function(data, nc=15){                    
  
  par(mfrow=c(1,2))
  
  wss <- NULL  
  pctExp <-NULL
  
  for (k in 1:nc)
  {
    kclus <- kmeans(data, centers=k)
    wss[k] <- kclus$tot.withinss      #store the total within SSE for given k
    pctExp[k] <- 1-wss[k]/kclus$totss
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  plot(1:nc, pctExp, type="b", xlab="Number of Clusters",
       ylab="Pct Explained")
  
  par(mfrow=c(1,1))
}

# Visualizations to select k:
wssplot(gowersMat)
sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gowersMat, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

# Two clusters seems to perform best.However, this might be too simple
# consider using six clusters instead (second best).
k <- 6
pamFit <- pam(gowersMat, diss = T, k)
pamResults <- ufoSub %>%
  mutate(cluster = pamFit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pamResults$the_summary

# create a cluster variable
ufoClusters <- ufoSub %>% mutate(cluster = pamFit$clustering)

# extract some information about the clusters
table(ufoClusters$cluster)
pamFit$medoids
pamFit$clusinfo

# Visualizing in lower-diminsional space
tsneObj <- Rtsne(gowersMat, is_distance = TRUE)
tsneData <- tsneObj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pamFit$clustering))
ggplot(aes(x = X, y = Y), data = tsneData) +
  geom_point(aes(color = cluster))

# -------------------------------------------------------------
# Hierarchical cluster analysis
gowersMat <- daisy(ufoSub,stand = T)

hclus<-hclust(gowersMat,method="single")
plot(hclus)
rect.hclust(hclus, k = 6, border = "red")

hclus<-hclust(gowersMat,method="complete") # seems best
plot(hclus)
rect.hclust(hclus, k = 6, border = "red")

hclus<-hclust(gowersMat,method="average")
plot(hclus)
rect.hclust(hclus, k = 6, border = "red")

hclusd<-hclust(gowersMat,method="ward.D")
plot(hclus)
rect.hclust(hclus, k = 6, border = "red")

# how balanced?
memb <- cutree(hclus, 6)
membDt <- table(memb)

# get the cluster number
memb <- as.data.frame(memb)
memb$memb

# bind the clusters to the data
ufoSub2 <- cbind(ufoSub, cluster = memb$memb)
head(ufoSub2)
table(ufoSub2$cluster)
