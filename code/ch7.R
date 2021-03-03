## chap 7 Principal Component Analysis

## We perform PCA on the USArrests data set.
## The row of the data set contain 50 states, in alphabetical order.
states <-row.names(USArrests)
states
## The column of the data contain the four variables.
names(USArrests)
## 4 variables have vastly different means and variance, hence it is important to standarize the variables to have mean zero and std one before performing PCA.
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
## prcomp() function is used to perform PCA
## By default, the prcomp() function centers the variables to have mean zero.
## By using the option scale=TRUE, we scale the variables to have deviation one.
pr_out <-prcomp(USArrests, scale=TRUE)
names(pr_out)
## The center and scale components correspond to the means and standard deviations of the variables that were used for scaling prior to implementing PCA.
pr_out$center
pr_out$scale
## The rotation matrix provides the principal component loadings, each column contaisn the corresponding PC loading vector.
pr_out$rotation
## The 50 by 4 matrix x has its columns the principal component score vectors, i.e. the kth column is the kth PC score vector.
dim(pr_out$x)
head(pr_out$x, 5)
## We can plot the first two principal components as follows
## The scale=0 argument to biplot() ensures that the arrows are scaled to represent the loadings,
## other values for scale give slightly different biplots with different interpretations.
biplot(pr_out, scale=0, cex=0.7)
## The summary function for prcomp() outputs show the standard deviation, 
## proportion ofvariance explained (PEV) of each principal component,
## and the cumulative PEV
summary(pr_out)
## We can plot the PVE, as well as the cumulative PVE, as follows.
par(mfrow = c(1, 2))
pr_imp <- summary(pr_out)$importance
plot(pr_imp[2,], xlab="Principal Component", 
		 ylab="PVE", ylim=c(0,1), type='b')
plot(pr_imp[3,], xlab="Principal Component", 
		 ylab="Cumulative PVE", ylim=c(0,1), type='b')
par(mfrow = c(1, 1))
