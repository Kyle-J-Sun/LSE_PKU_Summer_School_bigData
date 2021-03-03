#------------------------- Chapter 3: Overfitting and Its Avoidance ---------------------------#

#------Cross Validation---------#
library(ISLR) # dataset: Auto
library(boot)
Auto = na.omit(Auto)

set.seed(17)
cvError = rep (0, 10)

for (i in 1:10) {
  glmFit = glm(mpg ~ poly(horsepower , i), data = Auto)
  cvError[i] = cv.glm(Auto, glmFit, K = 10)$delta[1]
}
plot(1:10, cvError, type = "b", xlab = "Degree of Polynomial", ylab = "Mean Squared Error", col = 2)


#------C_p, AIC, BIC---------#
install.packages('leaps')
library(leaps)
Hitters0 = na.omit(Hitters)

hitSubreg = regsubsets(Salary ~ ., data = Hitters0, nvmax = 19, method = "forward")
regSummary = summary(hitSubreg)
print(regSummary)

names(regSummary)
plot(regSummary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")
points(which.max(regSummary$adjr2), max(regSummary$adjr2), col = 2, cex = 2, pch = 20)


#------Ridge Regression---------#
install.packages('glmnet')
library(glmnet) # Elastic Net
library(ISLR)
hit = na.omit(Hitters)

x = model.matrix(Salary ~ ., data = hit)
x = x[,-1]
y = hit$Salary

set.seed(1234)
train = sample(1:nrow(x), nrow(x)/2)
test = -train

# Training and test data
set.seed(1234)
cvRidgeLR = cv.glmnet(x[train, ], y[train], alpha = 0) #alpha = 0: Ridge regression; alpha = 1: Lasso regression 
bestlambda = cvRidgeLR$lambda.min
#names(cvRidgeLR)
plot(log(cvRidgeLR$lambda), cvRidgeLR$cvm, xlab = "log(Lambda)", ylab = "Mean-Squared Error")
points(log(cvRidgeLR$lambda[which.min(cvRidgeLR$cvm)]), min(cvRidgeLR$cvm), col = 2, cex = 2, pch = 20)

ridge_fit = glmnet(x[train,], y[train], lambda = bestlambda, alpha = 0)
coef(ridge_fit)[1:nrow(coef(ridge_fit)),]
ridge_pred = predict(ridge_fit, newx = x[test, ])

trainNew = data.frame(y[train], x[train, ])
linearRegression = lm(y.train. ~., trainNew) 
lm_pred = predict(linearRegression, as.data.frame(x[-train,]))
c(mean((ridge_pred-y[test])^2), mean((lm_pred-y[test])^2))


#------Lasso Regression---------#
set.seed(1234)
cvLassoLR = cv.glmnet(x[train, ], y[train], alpha = 1)
bestlambda = cvLassoLR$lambda.min
lasso_fit = glmnet(x[train,], y[train], lambda = bestlambda, alpha = 1)
lasso_pred = predict(lasso_fit, newx = x[-train, ])
lassoCoef = coef(lasso_fit)[1:nrow(coef(ridge_fit)),]
lassoCoef[lassoCoef != 0]

c(mean((lasso_pred-y[test])^2), mean((ridge_pred-y[test])^2), mean((lm_pred-y[test])^2))
