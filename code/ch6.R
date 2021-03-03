#############################
##  Chapter 6:  Clustering ##
#############################

#### K - Means Clustering ####

# Generate an example dataset:
set.seed(1234)
# Generate a 50-by-2 Matrix, filled by std. normal dist.
x = matrix(data = rnorm(n = 100), ncol = 2)

# for the first 25 obs, x1(x axis) + 3
# for the first 25 obs, x2(y axis) - 3
x[1:25,1] = x[1:25,1] + 3
x[1:25,2] = x[1:25,2] - 3
plot(x, pch = 20, cex = 2)

km_cluster = kmeans(x = x, centers = 2, nstart = 50)
plot(x, col = (km_cluster$cluster + 1),
     main = "k-Means Clustering Results with k=2",
     pch = 20, cex = 2)
points(km_cluster$centers, pch = 2, col = c("red", "green"))

km_cluster = kmeans(x = x, centers = 4, nstart = 50)
plot(x, col = (km_cluster$cluster + 1),
     main = "K-Means Clustering Results with K=4",
     pch = 20, cex = 2)
points(km_cluster$centers, pch = 2,
       col = c("red", "green", "blue", "cyan"))

#### Hierarchical Clustering ####

d_euc = dist(x, method = "euclidean")
hc_complete = hclust(d = d_euc, method = "complete")
hc_average = hclust(d = d_euc, method = "average")
hc_single = hclust(d = d_euc, method = "single")

par(mfrow = c(1, 3))
plot(hc_complete, main = "complete")
plot(hc_average, main = "average")
plot(hc_single, main = "single")

par(mfrow = c(1,3), pch = 19)
hcCutComplete = cutree(hc_complete, k = 2)
plot(x, col = hcCutComplete + 1, main = "Complete")
hcCutAverage = cutree(hc_average, k = 2)
plot(x, col = hcCutAverage + 1, main = "Average")
hcCutSingle = cutree(hc_single, k = 2)
plot(x, col = hcCutSingle + 1, main = "Single")


# Generate a sample dataset:
set.seed(1234)
x0 = seq(0, 2*pi, length.out = 100)
x = matrix(0, nrow = 50, ncol = 100)
for(i in 1:50){
	if(i <=25){
		# %%: remainder after the exact division
		x[i,] = sin(x0*pi) + (i%%2)*4 + rnorm(100, sd = 0.2)
	}else{
		x[i,] = cos(x0*pi) + (i%%2)*4 + rnorm(100, sd = 0.2)
	}
}

# plot() the data
plot(x = c(1, 100), y = range(x), type = "n", xlab = "", ylab = "")
lines(x[1,], col = 2);lines(x[2,], col = 2);
lines(x[3,], col = 2);lines(x[4,], col = 2);
lines(x[26,], col = 3);lines(x[27,], col = 3)
lines(x[28,], col = 3);lines(x[29,], col = 3)


correlation = cor(t(x))
dist_cor = 1 - correlation
dist_cor = as.dist(dist_cor)

hc_complete = hclust(d = dist_cor, method = "complete")
hc_average = hclust(d = dist_cor, method = "average")
hc_single = hclust(d = dist_cor, method = "single")

par(mfrow = c(1, 3))
plot(hc_complete, main = "complete")
plot(hc_average, main = "average")
plot(hc_single, main = "single")
cutree(tree = hc_average, k = 2)
