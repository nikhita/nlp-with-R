## Ex. 1
library(cluster)
library(HSAUR)
library(fpc)
data(pottery)
plot(pottery)
km2 <- kmeans(pottery,2)
class(km2)
km2
km3 <- kmeans(pottery,3)
km3
km4 <- kmeans(pottery,4)
km4

km4$cluster
km4$centers
km4$totss
km4$withinss
km4$tot.withinss
sum(km4$withinss)
km4$betweenss
km4$betweenss / km4$totss
km4$size
km4$iter
km4$ifault

plotcluster(pottery, km4$cluster)

### Elbow method
k.ss <- km4$totss
k <- 1:8
for (i in k[2:8]) k.ss[i] <- (kmeans(pottery, centers=i)$tot.withinss)
k.ss
plot(k, k.ss, type="b", xlab="Number of Clusters", ylab=" sum of squares")

## Ex. 2
library(fpc)
library(cluster)
data(iris)
data.points <- iris[, -5] # Remove the class variable
head(data.points)
# K-means clustring
clust <- kmeans(data.points, centers=3)
clust

table(iris$Species, clust$cluster)

plotcluster(data.points, clust$cluster)

plot(iris)

plot(iris$Sepal.Length, iris$Petal.Width, col = clust$cluster, pch = 3)
points(clust$centers[, c("Sepal.Length", "Petal.Width")], col = 1:3, pch = 10, cex = 2)

## Ex. 3 Hierarchical Clustering
data(iris)
data.points <- iris[, -5] # Remove the class variable
hClust <- hclust(dist(data.points))
hClust
hClust$height

plot(hClust, hang = -1, labels = iris$Species)

hClust <- hclust(dist(pottery))
plot(hClust)

## Ex. 4 Text Clustering

## k-means Clustering

library(tm)
library(tm.corpus.Reuters21578)
data("Reuters21578_DTM")

reuter.dtm <- Reuters21578_DTM
length(reuter.dtm[reuter.dtm$world>=1, ])
dim(reuter.dtm)
class(reuter.dtm)
dim(reuter.dtm[1:1000, ])

sampleDocs <- reuter.dtm[1:10000, ]
dim(sampleDocs)
sampleDocs <- removeSparseTerms(sampleDocs, 0.95)
dim(sampleDocs)

## perform tf idf
sample.tf.idf <- weightTfIdf(sampleDocs)
dim(sample.tf.idf)
inspect(sample.tf.idf[1:10, 150:160])

### perform clustering 
sample.mat <- as.matrix(sample.tf.idf)
dim(sample.mat)

### Normalize the vectors using euclidean distance
norm <- function(mat) mat/apply(mat, MARGIN=1, FUN=function(x) sum(x^2)^.5)
sample.norm <- norm(sample.mat)

library(functional)
sample.norm <- sample.norm[apply(sample.norm, 1, Compose(is.finite, all)),]
dim(sample.norm)
sample.norm[!is.finite(sample.norm)] <- 0
dim(sample.norm)
rownames(sample.norm) <- 1:nrow(sample.norm)

### cluster into 5 clusters
k.clust <- kmeans(sample.norm, 5)
k.clust

table(k.clust$cluster)

library(fpc)
plotcluster(sample.norm, k.clust$cluster)



## hierarchical Clustering

library(quanteda)
data("inaugCorpus")
summary(inaugCorpus)
str(inaugCorpus)
length(inaugCorpus$documents$texts)

speech.dfm <- dfm(inaugCorpus$documents$texts[c(seq(1,57, 2))], verbose = FALSE, stem = TRUE, ignoredFeatures = stopwords("english"))
class(speech.dfm)
length(speech.dfm)
speech.dfm <- trim(speech.dfm, minCount=3, minDoc=1)
# calculate distance matrix of documents
speech.dfm.mat <- dist(as.matrix(weight(speech.dfm, "relFreq")))

speech.kmeans <- kmeans(speech.dfm, 5)
plotcluster(speech.dfm, speech.kmeans$cluster)

# perform hiarchical clustering 
speech.clust <- hclust(speech.dfm.mat)
# assign labels to the clustered documents
speech.clust$labels <- inaugCorpus$documents$President[c(seq(1,57, 2))]
# plot the dendrogram
plot(speech.clust, xlab = "", ylab = "", cex=0.5)




## Exercise

install.packages("devtools")
devtools::install_github("kbenoit/quantedaData")
library(quantedaData)

# 1. Perform k-means and hierarchical clustering on SOTUCorpus. And analyse for factors like number of clusters (k-means), mergiability (hierarchical) 
