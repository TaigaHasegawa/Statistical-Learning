# install necessary packages

install.packages("kknn") # fitting knn regression
install.packages("deldir") # plot Voronoi tessellation

# the "1-nearest neighbour" example: regression

library(kknn)

# I set seed to be able to replicate the result

set.seed(1)

# generate training data with 2*sin(x) and random Gaussian errors
x <- runif(15, 0, 2*pi)
y <- 2*sin(x) + rnorm(length(x))

# generate testing data points
test.x = runif(10000, 0, 2*pi)
test.y = 2*sin(test.x) + rnorm(length(test.x))

# I reorder the data based on the x values, so that I can easily plot them.
test.y = test.y[order(test.x)]
test.x = test.x[order(test.x)]

# "1-nearest neighbour" regression
k = 1
knn.fit = kknn(y ~ x, train = data.frame(x = x, y = y), test = data.frame(x = test.x, y = test.y),
				k = k, kernel = "rectangular")
test.pred = knn.fit$fitted.values

# plot the data
par(mar=rep(2,4))
plot(x, y, xlim = c(0, 2*pi), pch = "o", cex = 2, xlab = "", ylab = "", cex.lab = 1.5)
title(main=paste(k, "-Nearest Neighbor Regression", sep = ""), cex.main = 1.5)

# plot the true regression line
lines(test.x, 2*sin(test.x), col = "deepskyblue", lwd = 3)
box()

# plot the fitted line
lines(test.x, test.pred, type = "s", col = "darkorange", lwd = 3)



# now we generate more data
set.seed(1)
x <- runif(200, 0, 2*pi)
y <- 2*sin(x) + rnorm(length(x))
test.x = runif(10000, 0, 2*pi)
test.y = 2*sin(test.x) + rnorm(length(test.x))

# Again I reorder the testing data 
test.y = test.y[order(test.x)]
test.x = test.x[order(test.x)]

# 1-nearest neighbour --- you can try different k values and see the results
k = 1
knn.fit = kknn(y ~ x, train = data.frame(x = x, y = y), test = data.frame(x = test.x, y = test.y),
				k = k, kernel = "rectangular")
test.pred = knn.fit$fitted.values
par(mar=rep(2,4))
plot(x, y, xlim = c(0, 2*pi), pch = 19, cex = 1, axes=FALSE, ylim = c(-4.25, 4.25))
title(main=paste(k, "-Nearest Neighbor Regression", sep = ""))
lines(test.x, 2*sin(test.x), col = "deepskyblue", lwd = 3)
lines(test.x, test.pred, type = "s", col = "darkorange", lwd = 3)
box()

# prediction error
mean((test.pred - test.y)^2)


# however, the real power of kNN is when k increase as we have more data:

# 15-nearest neighbour
k = 15
knn.fit = kknn(y ~ x, train = data.frame(x = x, y = y), test = data.frame(x = test.x, y = test.y),
               k = k, kernel = "rectangular")
test.pred = knn.fit$fitted.values
par(mar=rep(2,4))
plot(x, y, xlim = c(0, 2*pi), pch = 19, cex = 1, axes=FALSE, ylim = c(-4.25, 4.25))
title(main=paste(k, "-Nearest Neighbor Regression", sep = ""))
lines(test.x, 2*sin(test.x), col = "deepskyblue", lwd = 3)
lines(test.x, test.pred, type = "s", col = "darkorange", lwd = 3)
box()

# prediction error
mean((test.pred - test.y)^2)

# Compare that to a linear model 

points(test.x, predict(lm(y ~x), data.frame(x = test.x)), col = "red", type = "l", lwd = 2)

mean((predict(lm(y ~x), data.frame(x = test.x)) - test.y)^2)


######################## Cross Validation ###########################

# 10 fold cross validation

nfold = 10
infold = sample(rep(1:nfold, length.out=length(x)))

mydata = data.frame(x = x, y = y)

K = 50 # maximum number of k that I am considering
errorMatrix = matrix(NA, K, nfold) # save the prediction error of each fold

for (l in 1:nfold)
{
	for (k in 1:K)
	{
		knn.fit = kknn(y ~ x, train = mydata[infold != l, ], test = mydata[infold == l, ], k = k)
		errorMatrix[k, l] = mean((knn.fit$fitted.values - mydata$y[infold == l])^2)
	}
}

# plot the results
plot(rep(1:K, nfold), as.vector(errorMatrix), pch = 19, cex = 0.5)
points(1:K, apply(errorMatrix, 1, mean), col = "red", pch = 19, type = "l", lwd = 3)

# which k is the best? This might change each time you run it.
which.min(apply(errorMatrix, 1, mean))



###################################################
# knn for classification: 

# the "1-nearest neighbour" example

library(class)
set.seed(1)

# generate 20 random observations, with random class 1/0
x <- matrix(runif(40), 20, 2)
g <- rbinom(20, 1, 0.5)

# generate a grid for plot
xgd1 = xgd2 = seq(0, 1, 0.01)
gd = expand.grid(xgd1, xgd2)

# fit a 1-nearest neighbour model and get the fitted class
knn1 <- knn(x, gd, g, k=1)
knn1.class <- matrix(knn1, length(xgd1), length(xgd2))

# plot the data 
par(mar=rep(2,4))
plot(x, col=ifelse(g==1, "darkorange", "deepskyblue"), pch = 19, cex = 3, axes = FALSE, xlim= c(0, 1), ylim = c(0, 1))
points(0.7, 0.7, pch = 19) # a target point
points(0.7, 0.7, cex = 18) # the neighberhood circle
box()

# Voronoi tessalation plot (1NN)
library(deldir)
par(mar=rep(2,4))
z <- deldir(x = data.frame(x = x[,1], y = x[,2], z=as.factor(g)), rw = c(0, 1, 0, 1))
w <- tile.list(z)
plot(w, fillcol=ifelse(g==1, "bisque", "cadetblue1"), axes=FALSE, labels = "")
points(x, col=ifelse(g==1, "darkorange", "deepskyblue"), pch = 19, cex = 3)


# example: calculate the closest neighbor of point (0.4, 0.6)
points(0.4, 0.6, pch = 19, col = "red")
which.min(rowSums((sweep(x, 2, matrix(c(0.4, 0.6)), FUN = "-"))^2))
x[16, ]
points(x[16, 1], x[16, 2], cex = 4, lwd = 2, col = "red")



# Example from HTF text book
library(ElemStatLearn)
library(class)

x <- mixture.example$x
y <- mixture.example$y
xnew <- mixture.example$xnew

par(mar=rep(2,4))
plot(x, col=ifelse(y==1, "darkorange", "deepskyblue"), axes = FALSE)
box()

k = 15
knn.fit <- knn(x, test=xnew, cl=y, k=k)
?knn
px1 <- mixture.example$px1
px2 <- mixture.example$px2
pred <- matrix(knn.fit == "1", length(px1), length(px2))

contour(px1, px2, pred, levels=0.5, labels="",axes=FALSE)
box()
title(paste(k, "-Nearest Neighbour", sep= ""))
points(x, col=ifelse(y==1, "darkorange", "deepskyblue"))
mesh <- expand.grid(px1, px2)
points(mesh, pch=".", cex=1.2, col=ifelse(pred, "darkorange", "deepskyblue"))

# using linear regression to fit the data (not logistic)

lm.fit = lm(y~x)
lm.pred = matrix(as.matrix(cbind(1, xnew)) %*% as.matrix(lm.fit$coef) > 0.5, length(px1), length(px2))

par(mar=rep(2,4))
plot(mesh, pch=".", cex=1.2, col=ifelse(lm.pred, "darkorange", "deepskyblue"), axes=FALSE)

abline(a = (0.5 - lm.fit$coef[1])/lm.fit$coef[3], b = -lm.fit$coef[2]/lm.fit$coef[3], lwd = 2)
points(x, col=ifelse(y==1, "darkorange", "deepskyblue"))
title("Linear Regression of 0/1 Response")
box()


