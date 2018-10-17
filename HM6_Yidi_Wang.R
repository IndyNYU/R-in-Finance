rm(list=ls())
setwd('D:/')
options(digits=7, scipen=0)
opar <- par(no.readonly=TRUE)

# FRE 6871 Homework Assignment 6 by Yidi Wang
# 5/9/2018

# 1. Clustering
# For this problem we will work with a classic data set 'iris'.
# The data set contains 4 physical measurements of 50 flowers from each of 3 species of iris flowers.

# (a) Load the explore the iris data set.
# The 'species' columns identifies which species each flower is. Take a look at what they are with.
data("iris")
?(iris)
View(iris)
summary(iris)
unique(iris$Species)
levels(iris$Species)

# (b) Create a data frame 'irisSub' that subsets the data excluding the Species column.
irisSub <- iris[c(-5)]
View(irisSub)

# (c) Glance at a summary() of irisSub.
summary(irisSub)
# Scale irisSub so that all measurements in our clustering are treated similarly and save the scaled data frame as "irisSub.scaled".
irisSub.scaled <- scale(irisSub)
View(irisSub.scaled)

# We know that our data actually come from 3 different groups so we can be pretty confident that:
# (1) cluster exist and that (2) the number of clusters will be 3.
# But we should confirm that these are true.

# (d) Let's first confirm that cluster exists.
# Load the NbCluster library and use set.seed(1234)
# Use the NbCluster() function on irisSub.scaled, with minimal number of clusters 2,
# maximal number of clusters 15 and a method of "average".
# Save the resutlt into a variable 'nc'.
library(NbClust)
nc <- NbClust(irisSub.scaled, distance = "euclidean",
              min.nc = 2, max.nc = 15, method = "average")

# (e) Plot the Cubic Cluster Criteria, which is returned in the 4th column of the matrix nc$All.index by using plot().
plot(nc$All.index[,4], type = "o", ylab = "CCC",
     xlab = "Number of Clusters", col = "blue")
# According to the graph, it fits my situation.
# We couldn't say that there are no clusters.

# (f) Next, take a look at the table showing how many criteria, from those provided by the NbClust Package,
#     are satisfied for possible number of clusters 2 thorugh 15 using table.
#     Also view the table as a barchart using barplot().
#     How many clusters does it suggest? Is this what you would expect?
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
        xlab = "Number of Clusters", ylab = "Number of Criteria",
        main = "Nubmer of Clusters Chose by 26 Criteria")
# According to the table and barplots, there are 9 criteria support 2 clusters.
# It is not actually I expect.

# (g) Let's do Hierarchical cluster analysis.
# Begin by creating a distance matrix using dist() on irisSub.scaled and placing the result in variable 'd'.
d <- dist(irisSub.scaled)

# (h) Use hclust() on d with the method of "average" placing the result in 'fit.average'.
#     And plot() your fit.average to view the resulting tree.
fit.average <- hclust(d, method = "average")
plot(fit.average, hang = -1, cex = .8, main = 'Average Linkage Clustering')

# (i) Since we decided on a specific number of clusters, use cuttree() on fit.average with k equal to your number of cluster.
#     Place the result of cuttree() in variable 'clusters'.
clusters <- cutree(fit.average, k = 3)

# Take a look at table(clusters).
table(clusters)
# What do you think might be the reason we got a different number of clusters than expected.
# The reason is that the difference between cluster 2 and cluster 3 is not that clear.
# There may exist containing relationship.

# (j) Describe the original data in the clusters using aggregate() finding the median of each measurement per cluster.
#     Use irisSub also do the same description on the scaled data irisSub.scaled.
aggregate(irisSub, by = list(cluster=clusters), median)
aggregate(irisSub.scaled, by = list(cluster=clusters), median)

# After scale, the difference which is obvious becomes not that obvious.

# (k) Use rect.hclust() to draw a rectangle over each cluster in our tree.
rect.hclust(fit.average, k = 3)
# It becomes 2 clusters.

# (l) Compare our result with the original flower types using table(clusters, iris$Species).
#     What do you think is going on with our flower types?
table(clusters, iris$Species)
# Type versicolor and virginica is not that easy to tell from each other.

# (g) Partitioning cluster analysis. We'll try k-means clustering on the data set.
# (n) Use wssplot() or irisSub.scaled.
#     How many clusters does it imply we should use?
wssplot <- function(data, nc = 15, seed = 1234){
               wss <- (nrow(data)-1)*sum(apply(data,2,var))
               for (i in 2:nc){
                 set.seed(seed)
                 wss[i] <- sum(kmeans(data, centers=i)$withinss)}
               plot(1:nc, wss, type="b", xlab="Number of Clusters",
                    ylab = "Within groups sum of squares.")}
wssplot(irisSub.scaled)

# (o) Set the seed to 1234 and use NbClust again, this time choose "kmeans" for the method and save it in variable 'nc'.
#     Produce the table() and barplot() to determine a number of clusters.
#     How many does it indicate we should use?
library(NbClust)
set.seed(1234)
devAskNewPage(ask=T)
nc <- NbClust(irisSub.scaled, min.nc = 2, max.nc = 15, method = "kmeans")
table(nc$Best.n[1,])
# It suggests to use 3.

# (p) Perform kmeans() cluster analysis on irisSub.scaled with the number of clusters just determined and save the result in varible 'fit.km'.
set.seed(1234)
fit.km <- kmeans(irisSub.scaled, 3)

# (q) To view the results.
fit.km$size
fit.km$centers
aggregate(irisSub, by = list(cluster = fit.km$cluster), mean)

# (r) Take a look at table(fit.km$cluster, iris$Species).
table(fit.km$cluster, iris$Species)
ct.km <- table(fit.km$cluster, iris$Species)

# (S) Load the flexclust library and run randIndex() on the table we just created.
library(flexclust)
randIndex(ct.km)

# (t) We will need to change the level ordering for the next graphs to look right so run the follwoing.
iris$Species = factor(iris$Species,levels = c("steosa","virginica","veriscolor"))

# (u) Now compare the following two graphs of the actual species separation vs H cluster analysis.
pairs(irisSub, col=iris$Species)
pairs(irisSub, col=clusters)

# (v) And compare these next two graphs of the actual species separation vs P cluster analysis with k-means.
pairs(irisSub, col=iris$Species)
pairs(irisSub, col=fit.km$cluster)

# 2. Let's see if we can use a support vector machine to make such a prediction.
# (a) Load the 'iris' data and combine the "virginica" and "veriscolor" to one "combined" variable with the following code.
data(iris) 
iris$Species=as.character(iris$Species) 
iris$Species[iris$Species=="virginica"]="combined" 
iris$Species[iris$Species=="versicolor"]="combined" 
iris$Species=factor(iris$Species,levels=c("setosa","combined")) 
table(iris$Species)

# (b) Set the seed to 1234. Seperate the iris into train and validate sets.
set.seed(1234)
train <- sample(nrow(iris), 0.7*nrow(iris))
iris.train <- iris[train,]
iris.validate <- iris[-train,]
table(iris.train$Species)
table(iris.validate$Species)

# (C) Load the e1071 library and set the seed to 1234.
library(e1071)
set.seed(1234)
fit.svm <- svm(class~., data=iris.train)

# (d) Use predict with fit.svm on iris.validate saving the results into 'svm.pred'.
svm.pred <- predict(fit.svm, na.omit(iris.validate))
svm.perf <- table(na.omit(iris.validate)$Species,
                  svm.pred, dnn = c("Actual", "Predicted"))

# (e) The next steps would be to:
#   (1) Tune the support vector machine and 
#   (2) evaluate our results looking at the confusion matrix.
set.seed(1234)
tuned <- tune.svm(class~., data=iris.train,
                  gamma=10^(-6:1),
                  cost=10^(-10:10))
