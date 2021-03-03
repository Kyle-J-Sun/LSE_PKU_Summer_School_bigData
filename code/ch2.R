#------------------------- Chapter 2: Classification ---------------------------#

#------Decision Tree---------#
install.packages('tree')
install.packages('ISLR')
library(ISLR)
library(tree)

#--1. Data
View(Carseats)
High = ifelse(Carseats$Sales > 8, "Yes", "No")
Carseats = data.frame(Carseats, High)

#--2. Build a Classification Tree
treeCarseats = tree(formula = High~. - Sales, data = Carseats)
summary(treeCarseats)
plot(treeCarseats)
text(treeCarseats, cex = 0.9, pretty = 0)
treeCarseats

#--3. Training Set and Test Set
set.seed(2)
train = sample(1:nrow(Carseats), 200)
CarseatsTest = Carseats[-train,]
HighTest = High[-train]
treeCarseats = tree(High~.-Sales, Carseats, subset = train)
treePred = predict(treeCarseats, CarseatsTest, type = "class")
table(treePred, HighTest)
(86+57)/200

#--4.Tree Pruning
set.seed(3)
cvCarseats = cv.tree(treeCarseats, FUN = prune.misclass)
cvCarseats
#par(mfrow = c(1,2))
#plot(cvCarseats$size, cvCarseats$dev, type = "b")
#plot(cvCarseats$k, cvCarseats$dev, type = "b")
pruneCarseats = prune.misclass(treeCarseats, best = 9)
plot(pruneCarseats)
text(pruneCarseats, cex = 0.9, pretty = 0)

#--5. Use Pruned Tree to Predict the Test Data
treePred = predict(pruneCarseats, CarseatsTest, type = "class")
table(treePred, HighTest)
(94+60)/200


#------Logistic Regression---------#

#--1. Data and Logistic Regression
View(Default)
lgDefault = glm(formula = default~balance, data = Default, family = binomial)
summary(lgDefault)
plot(balance, lgDefault$fitted.values, xlab = "Balance", ylab = "Probability of Default", col = 4)

#--2. Small Example of prediction
predict(lgDefault, data.frame(balance = c(1000,2000)), type = "response")

#--3. Logistic Regression with a discrete predictor
lgDefault = glm(formula = default~student, data = Default, family = binomial)
summary(lgDefault)
predict(lgDefault, data.frame(student = c("Yes","No")), type = "response")

#--4. Multiple Logistic Regression
lgDefault = glm(formula = default~., data = Default, family = binomial)
summary(lgDefault)
predict(lgDefault, data.frame(balance = c(1500), student = c("Yes", "No"), income = c(40000)), type = "response")


