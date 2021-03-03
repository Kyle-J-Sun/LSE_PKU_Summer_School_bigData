# Tree-Based Method
install.packages("tree")
install.packages("ISLR")
?ifelse
View(Carseats)
?cv.tree
?prune.misclass
View(Carseats)
library(ISLR)
library(tree)
High = ifelse(Carseats$Sales > 8, 'YES', 'No') #Recoding it as a binary variable, cuz we need binary variables for building the tree
Carseats = data.frame(Carseats, High) #Put 'High' column into Carseats data set
treeCarseats = tree(formula = High~.-Sales, data = Carseats) #Removing Sales since 'High' has strong correlation with 'Sales'
summary(treeCarseats)
plot(treeCarseats)
text(treeCarseats, cex = 0.9)

?predict() #Evalueate its performance on the test data
set.seed(2)
train = sample(1:nrow(Carseats), 200) #Half of data, cause we need to do trade-off between training data and test data
CarseatsTest = Carseats[-train, ] #We remove data in train variable
HighTest = High[-train] #Change Test Data(CarseatsTest) into binary varuable
treeCarseats = tree(High~. - Sales, Carseats, subset = train) #List the tree
treePred = predict(treeCarseats, CarseatsTest, type = 'class') #Evalueate its performance on the test data
table(treePred, HighTest) #????
(86+57)/200 #??????

#Tree Pruning
set.seed(3)
cvCarseats = cv.tree(treeCarseats, FUN = prune.misclass) #Cross-validation in order to determine the optimal level of tree complexity
cvCarseats

pruneCarseats = prune.misclass(treeCarseats, best = 9) #Tell R the best nodes is 9 due to lowest misclass rate
plot(pruneCarseats)
text(pruneCarseats, cex = 0.9, pretty = 0)

#Test pruning tree
pruningTreePred = predict(pruneCarseats, CarseatsTest, type = 'class')
table(pruningTreePred, HighTest)
(94+60)/200 #77% of the test observaitons are correctly classified

#Logistic Regression
attach(Default)
lgDefault = glm(formula = default~balance, data = Default, family = binomial) #Genaralize linear model between default and balance
summary(lgDefault)
plot(balance, lgDefault$fitted.values, xlab = 'Balance', ylab = 'Probability of Default', col = 4)
?predict
predict(lgDefault, data.frame(balance = c(1000,2000)), type = 'response') #Predict the probability of default, given income 1000 and 2000
#For qualitative predictors
lgDefaultDummy = glm(formula = default~student, data = Default, family = binomial)
summary(lgDefaultDummy)

lgDefaultStudents = glm(formula = default~., data = Default, family = binomial)
summary(lgDefaultStudents)
par(mfrow = c(1,2))
de1 = Default[Default["student"] == 'No',]
de2 = Default[Default['student'] == 'Yes',]
de1 = de1[sort(de1$balance),]
de2 = de2[sort(de2$balance),]

#Predict
pre1 = predict(lgDefaultStudents, de1, type = 'response')
pre2 = predict(lgDefaultStudents, de2, type = 'response')

#ploting line
plot(de1$balance, pre1, col = 4, ylab = 'Default rate', xlab = 'Credit Card Balance')
par(new = T)
plot(de2$balance, pre2, col = 6, axes = F, ann = F)
par(new = F)
plot(Default$student,Default$balance, col  = c('cyan3', 'coral2'), xlab = "Student status", ylab = 'Credit card Balance') #construct a boxplot

predict(lgDefaultStudents, data.frame(balance = c(1500), student = c('Yes', 'No'), income = c(40000)), type = 'response')

