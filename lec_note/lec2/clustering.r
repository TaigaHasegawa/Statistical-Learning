# STAT 432, Spring 2019
# This is the R code for lecture note Unsupervised


library(datasets)

# the famous iris data 

head(iris)
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()


# some random test, kmean will give different values based on random initial values
# its better to try more nstart 
mat = matrix(rnorm(1000), 100, 10)

kmeans(mat, centers = 3, nstart = 100, trace = TRUE)$tot.withinss

# k mean clustering
set.seed(1)
iris.kmean <- kmeans(iris[, 3:4], centers = 3, nstart = 20, trace = TRUE) 

# the center of each class
iris.kmean$centers

# the within cluster variation 
iris.kmean$withinss

# the between cluster variation 
iris.kmean$betweenss

# plot the fitted clusters 
iris.kmean$cluster <- as.factor(iris.kmean$cluster)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + 
		geom_point(alpha = 0.4, size = 3.5) + # true cluster 
		geom_point(col = c("blue", "green", "red")[iris.kmean$cluster]) + # fitted cluster 
		scale_color_manual(values = c('red', 'green', 'blue'))

		
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris.kmean$cluster)) + geom_point()


# hierarchical clustering
# the default is a complete link
# the dist() function computes pairwise distance using euclidean norm

iris.hclust <- hclust(dist(iris[, 3:4]), method = "complete")
plot(iris.hclust)

# choose a cutoff for the tree

hclust.cut <- cutree(iris.hclust, 3)
hclust.cut
table(hclust.cut, iris$Species)

# use average link

iris.hclust <- hclust(dist(iris[, 3:4]), method = 'average')
plot(iris.hclust, hang = -1, cex = 0.7)

hclust.cut <- cutree(iris.hclust, 3)
table(hclust.cut , iris$Species)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + 
		geom_point(alpha = 0.4, size = 3.5) + # true cluster 
		geom_point(col = hclust.cut) + # fitted cluster 
		scale_color_manual(values = c('blue', 'red', 'green'))

# other ways to plot 
library(ape)
plot(as.phylo(iris.hclust), cex = 0.6, label.offset = 0.5, direction = "downwards")
plot(as.phylo(iris.hclust), type = "unrooted", cex = 0.6, no.margin = TRUE)
plot(as.phylo(iris.hclust), type = "fan")


# random forests 
# unsupervised random forests that only use the features to build prximity

library(randomForest)

rffit = randomForest(iris[, 3:4], mtry = 1, nodesize = 10, proximity = TRUE)
heatmap(rffit$proximity, symm = TRUE, Rowv = NA, hclustfun = NA)


#k - medoids method to get the clusters
library(cluster)
table(pam(1-rffit$proximity, 3, diss = TRUE)$clustering, iris$Species)





library(kernlab)

data(spirals)
plot(spirals)

sc <- specc(spirals, centers=2)

plot(spirals, col=sc)

