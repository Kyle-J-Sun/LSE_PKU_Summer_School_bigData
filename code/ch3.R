#------------------------- Chapter 3: Regression ---------------------------#

#------Simple Linear Regression---------#
fileName = "Advertising.csv"
defaultDataDir = "/Users/kyle/Documents/LSE-PKU/tutorial_class/data/"
fileLocation = file.path(defaultDataDir, fileName)
Advertising = read.csv(fileLocation, header = T)
#par(mfrow = c(1, 3))
plot(Advertising$TV, Advertising$Sales, col = "red")
abline(lm(formula = Sales~TV , data = Advertising), lwd = 3, col = "blue")

#plot(Radio, Sales, col = "red")
#abline(lm(formula = Sales~Radio , data = Advertising), lwd = 3, col = "blue")
#plot(Newspaper, Sales, col = "red")
#abline(lm(formula = Sales~Newspaper , data = Advertising), lwd = 3, col = "blue")

adSLR = lm(formula = Sales~TV)
adSLR = lm(formula = Sales~TV, data = Advertising)
attach(Advertising)
adSLR = lm(formula = Sales~TV)
summary(adSLR) #detailed information
adSLR          #basic information
names(adSLR)   #what is in the list


#------Multiple Linear Regression---------#
multipleLR = lm(formula = Sales~TV+Radio+Newspaper , data = Advertising)
summary(multipleLR)
cor(Advertising[-1])

multipleLR = lm(formula = Sales~TV+Radio , data = Advertising)
predict(multipleLR, data.frame(TV = c(100), Radio = c(20)), interval = "confidence")
predict(multipleLR, data.frame(TV = c(100), Radio = c(20)), interval = "predict")
?predict

#------Interaction Terms---------#
nonLR = lm(formula = Sales~TV*Radio, data = Advertising)
coefficients(summary(nonLR))


#------Polynomial Regression---------#
library(ISLR)
plot(Auto$horsepower,Auto$mpg,col = "red")
polynomialLR = lm(formula = Auto$mpg~Auto$horsepower+I(Auto$horsepower^2), data = Auto)
coefficients(summary(polynomialLR))
#sort: sort a vector or factor (partially) into ascending or descending order
#order: returns a permutation which rearranges its first argument into ascending or descending order
lines(sort(Auto$horsepower), fitted(polynomialLR)[order(Auto$horsepower)], col='blue', type='l', lwd = 3)

polynomialLR  = lm(formula = Auto$mpg~poly(Auto$horsepower, 5), data = Auto)
lines(sort(Auto$horsepower), fitted(polynomialLR )[order(Auto$horsepower)], col = 6, type='l', lwd = 3)


#------Sort & Order---------#
set.seed(1)
a = sample(c(1:5))
a
sort(a)
order(a)


#------Regression Tree---------#
install.packages('MASS')
library(MASS)
library(tree)
??MASS
View(Boston)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~., Boston, subset = train)
summary(tree.boston)

#plot(tree.boston)
#text(tree.boston, pretty = 0)

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')

prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

yhat = predict(tree.boston, newdata = Boston[-train,])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
