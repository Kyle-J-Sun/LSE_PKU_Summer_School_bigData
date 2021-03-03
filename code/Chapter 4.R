install.packages('boot') #implement k-fold CV
install.packages('leaps') #performing best sub-set selection by identifying the best model that contains a given number of predictors, where best is quantified using RSS
install.packages('glmnet') #perform ridge regression and the LASSO
library(glmnet)
library(boot)
library(leaps)
set.seed(17)
cvError = rep(0, 10)
for (i in 1:10) {
  glmFit = glm(mpg ~ poly(horsepower, i), data = Auto_nonNA) #create a polynomial function to the power of i
  cvError[i] = cv.glm(Auto_nonNA, glmFit, K = 10)$delta[1] #Do 10-fold cross-validation and choose the first value of delta (raw cv estimate of prediction error)
}

par(mfrow = c(1,1))
plot(
  1:10,
  cvError,
  type = 'b',
  xlab = 'Degree of Polynomial',
  ylab = 'Mean Squared Error',
  col = 2
)

#Overfitting and Its Avoidance - Cp, AIC, BIC
Hitters0 = na.omit(Hitters)
hitSubreg = regsubsets(Salary~., data = Hitters0, nvmax = 19, method = 'forward') 
regSummary = summary(hitSubreg) #outputs the best set of variables for each model size
regSummary
par(mfrow = c(2,2))
plot(regSummary$adjr2, xlab = 'Number of Variables', ylab = 'Adjusted R^2', type = 'l')
points(which.max(regSummary$adjr2), max(regSummary$adjr2), col = 2, cex = 2, pch = 20)
plot(regSummary$bic, xlab = 'Number of Variables', ylab = 'BIC', type = 'l')
points(which.min(regSummary$bic), min(regSummary$bic), col = 2, cex = 2, pch = 20)
plot(regSummary$cp, xlab = 'Number of Variables', ylab = 'Cp', type = 'l')
points(which.min(regSummary$cp), min(regSummary$cp), col = 2, cex = 2, pch = 20)
plot(regSummary$rss, xlab = 'Number of Variables', ylab = 'RSS', type = 'l')

#Overfitting and Its Avoidence - Ridge Regression
Hitters = na.omit(Hitters)
x = model.matrix(Salary~., Hitters)[,-1] #output a dataframe
#model.matrix() not only does it produce a matrix corresponding to the 19 predictors but it also automatically transforms any qualitative variables into dummy variables
y = Hitters$Salary
set.seed(1234)
train = sample(1:nrow(x), nrow(x)/2)
test = -train
cvRidgeLR = cv.glmnet(x[train,], y[train], alpha = 0) #for alpha: 0 for Ridge Regression; 1 for LASSO
bestlamda = cvRidgeLR$lambda.min
plot(log(cvRidgeLR$lambda), cvRidgeLR$cvm, xlab = 'log(Lambda)', ylab = 'Mean-Squared Error')
points(log(cvRidgeLR$lambda)[which.min(cvRidgeLR$cvm)], min(cvRidgeLR$cvm), col = 2, cex = 2, pch = 20)
#to fit the data by a ridege reg. on a training set with the best lambda
ridge_fit = glmnet(x[train,], y[train], lambda = bestlamda, alpha = 0)
coef(ridge_fit)[1:nrow(coef(ridge_fit)),]#different results
#evaluate the test MSE of ridge regression
ridge_pred = predict(ridge_fit, newx = x[-train,])
trainNew = data.frame(y[train], x[train,])
linearReg = lm(y.train.~., trainNew)
lm_pred = predict(linearReg, as.data.frame(x[-train,]))
c(mean((ridge_pred-y[test])^2), mean((lm_pred-y[test])^2)) #different results

#LASSO Regression
set.seed(1234)
cvLassoLR = cv.glmnet(x[train,], y[train], alpha = 1)
bestlamda = cvLassoLR$lambda.min
lasso_fit = glmnet(x[train,], y[train], lambda = bestlamda, alpha = 1)
lasso_pred = predict(lasso_fit, newx = x[-train, ])
lassoCoef = coef(lasso_fit)[1:nrow(coef(ridge_fit)),]
lassoCoef[lassoCoef != 0] #select the variables whose their coeff are not equal to 0
c(mean((lasso_pred-y[test])^2), mean((ridge_pred-y[test])^2), mean((lm_pred-y[test])^2))
