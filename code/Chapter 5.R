install.packages('class')
library(ISLR)
library(class)
caravan = na.omit(Caravan)
View(caravan)
dim(Caravan)
# Classfication using NN
plot(caravan$Purchase) #proportion of the two classes
table(caravan$Purchase) #Count the numbers
y = caravan[,86]
x = scale(caravan[,-86]) #standrized table: 0 mean and 1 variance
n = nrow(caravan) #No. of the observations
test = 1:2000 # test set
knn_pred_3 = knn(train = x[-test,], test = x[test, ], cl = y[-test], k = 3)
(contTable3 = table(knn_pred_3, y[test]))
k_test = seq(3,15,2) #odd numbers: reason why using odd numbers is because of majority vote
err = rep(NA, 7)
set.seed(1234)
for (i in 1:7){
  out = knn.cv(x[-test,], cl = y[-test], k = k_test[i])
  err[i] = mean(out != y[-test]) #Misclassification rate
}
plot(1/k_test, err, type = 'b', ylab = 'Error Rate', xlab = '1/K')

knn_pred_7 = knn(train = x[-test,], test = x[test,], cl = y[-test], k = 7)
(contTable7 = table(knn_pred_7, y[test]))

dat = data.frame(y,x)
