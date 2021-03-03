########################################
##  Chapter 5: K - Nearest Neighbours ##
########################################

#### K - Nearest Neighbours - Classfication ####

library(ISLR)
caravan = na.omit(Caravan) # laod the data and delete the missing values
dim(caravan)
plot(caravan$Purchase)     # Proportion of the two classes
table(caravan$Purchase)    # Count the Numbers
y = caravan[,86]           # y - vecttor: Class Lable
x = scale(caravan[,-86])   # x - standardized table: Variables
n = nrow(caravan)          # No. of the observations

caravan[,-86]
caravan[,86]

test = 1:2000   # test set
library(class)  # knn() - train and predict at the same time
knn_pred_3 = knn(train = x[-test,],  # train dataset
                test = x[test,],     # test dataset
                cl = y[-test],       # class label of the train
                k = 3)
(contTable3 = table(knn_pred_3, y[test]))


k_test = seq(3, 15, 2) # odds numbers
err = rep(NA, 7)
set.seed(1234)
for (i in 1:7) {
  out = knn.cv(x[-test,], cl = y[-test], k = k_test[i])
  err[i] = mean(out != y[-test])
}
plot(1/k_test, err, type = "b", ylab = "Error Rate", xlab = "1/K")

knn_pred_7 = knn(train = x[-test,],  # train dataset
                 test = x[test,],    # test dataset
                 cl = y[-test],      # class label of the train
                 k = 7)
(contTable7 = table(knn_pred_7, y[test]))


# library(ISLR)
# Try Logit-regression as a comparation:
dat = data.frame(y, x)               # build a data frame using y and x
logit_fit = glm(formula = y ~.,      # formula: y on all the rest
                family = "binomial", # Logit Regression
                data = dat,          # the dataset we are using
                subset = -test)      # subset, the training data

# predict() using the estimated model to predict on new dataset
logit_prob = predict(logit_fit,            # the estimated model
                     newdata = dat[test,], # new dataset
                     type = "response")    # get the predicted probability

# for those predicted prob. > 0.25, we set them to be "Yes",
#   O/W, let it to be "No"
logit_pred = rep("No", length(test))
logit_pred[logit_prob > 0.25] = "Yes"
(contTable = table(logit_pred, y[test]))


#### K - Nearest Neighbours - Regression ####


# install.packages("FNN") - knn.reg()
library(FNN)
fileName = "Advertising.csv"
defaultDataDir = "/Users/kyle/Documents/LSE-PKU/tutorial_class/data/"
fileLocation = file.path(defaultDataDir, fileName)
ad = read.csv(fileLocation, header = T)[,-1]
yy = ad$Sales  
yy# y
xx = scale(ad[,-4])
xx# x - regressors
n = nrow(ad)        # n - sample size

# test - vector: randomly sample ID's of the test dataset
set.seed(1234)
test = sample(1:n, size = 80, replace = F)
# Prepare a vector to store the CV-Error
cvErr = rep(x = 0, times = 12)

# Try those "K" in a for loop 
for(i in 1:12){
  knnReg = knn.reg(train = xx[-test,],
                   y = yy[-test],
                   k = i)
  # CV-Error
  cvErr[i] = knnReg$PRESS / length(test)
}
plot(1/(1:12), cvErr, type = "b", xlab = "1/K")

knnReg = knn.reg(train = xx[-test,], test = xx[test,], 
                 y = yy[-test], k = 4)

mean((knnReg$pred - yy[test])^2)


dat = data.frame(yy, xx)      # build a data frame using y and x
ls_fit = lm(formula = yy~., data = dat, subset = -test)
ls_pred = predict(ls_fit, newdata = dat[test,])
mean((yy[test] - ls_pred)^2)
sqrt(mean((yy[test] - ls_pred)^2))
