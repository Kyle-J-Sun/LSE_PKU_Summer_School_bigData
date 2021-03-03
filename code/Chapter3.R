?par
fileName = "Advertising.csv"
defaultDataDir = "/Users/kyle/Documents/LSE-PKU/tutorial_class/data"
fileLocation = file.path(defaultDataDir, fileName)
Advertising = read.csv(file = fileLocation, header = T)
View(Advertising)

#Simple Linear Regression
par(mfro = c(1,4)) #to set the plots area
plot(Advertising$TV, Advertising$Sales, col = 'red')
abline(lm(formula = Sales~TV, data = Advertising), lwd = 3, col = 'blue') 
plot(Advertising$Radio, Advertising$Sales, col = 'red')
abline (lm(formula = Sales~Radio, data = Advertising), lwd = 3, col = 'blue')
plot(Advertising$Newspaper, Advertising$Sales, col = 'red')
abline(lm(formula = Sales~Newspaper, data = Advertising), lwd = 3, col = 'blue')

library(ISLR)
attach(Advertising)
adSLR = lm(formula = Sales~TV)

adSLR = lm(formula = Sales~TV, data = Advertising)
attach(Advertising)
adSLR = lm(formula = Sales~TV, data = Advertising)

adSLR
names(adSLR)

summary(adSLR)

#Multiple Linear Regression
multipleLR = lm(formula = Sales~TV + Radio + Newspaper, data = Advertising)
summary(multipleLR)
cor(Advertising) #The correlation between variables
multipleLR = lm(formula = Sales~TV + Radio, data = Advertising)
predict(multipleLR, data.frame(TV = c(100), Radio = c(20)), interval = 'confidence') #calculate its confidence interval
nonLR = lm(formula = Sales~TV * Radio, data = Advertising)
coefficients(summary(nonLR))
?coefficients

#Polynimial Regression
attach(Auto_nonNA)
polynomialLR = lm(formula = mpg~horsepower+I(horsepower^2), data = Auto_nonNA) #I() needed since the ^2 has a special meaning in a formula
summary(polynomialLR)
coefficients(summary(polynomialLR))
par(mfrow = c(1,1))
plot(horsepower, mpg, col = 'red')
lines(sort(horsepower), fitted(polynomialLR)[order(horsepower)], col = 'blue', type = 'l', lwd = 3) #Fitted: collect fitted value from regression (y hat) 
polynomialLR  = lm(formula = mpg~poly(horsepower, 5, raw = T), data = Auto_nonNA)
lines(sort(horsepower), fitted(polynomialLR)[order(horsepower)], col = 6, type = 'l', lwd = 3)
#Function about 'sort' & 'order'
set.seed(2)
a = sample(c(1:5))
a
sort(a) #sort a vector or factor (partially) into ascending or descending order
order(a) #return a permutation which rearrange its first argument into ascending or descending order.

#Regression Tree
install.packages('MASS')
library(MASS)
library(tree)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2) #choose half of data as traning data
treeBoston = tree(medv~., Boston, subset = train) 
treeBoston
summary(treeBoston)
?tree
View(Boston)
cvBoston = cv.tree(treeBoston)
plot(cvBoston$size, cvBoston$dev, type = 'b')

pruneBoston = prune.tree(treeBoston, best = 7)
plot(pruneBoston)
text(pruneBoston, pretty = 0)
