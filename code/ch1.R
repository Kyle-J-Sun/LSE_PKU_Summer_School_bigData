rm(list = ls())

#############################################################################
#                                                                           #
#                             Introduction to R                             #
#                                                                           #
#############################################################################
str1 = "Hello World"
str1

numbers <- c(1,2,3)
people <- c("Ali","Bet","Cat")
numbers
people


c(1,2,3)+c(4,5,6)
c(1,2,4)*c(1,3,3)

small=c(1,2)
large=c(0,0,0,0,0,0)
large+small

ls()
rm(numbers,people)
ls()


weight = c(84.5, 72.6, 75.7, 94.8, 71.3)
height = c(86.5, 71.8, 77.2, 84.9, 75.4)
sheep = data.frame(weight, height)
mean(sheep$height)
View(sheep)

sheep$backlength = c(130.4, 100.2, 109.4, 140.6, 101.4)
sheep

rm(height,weight)
weight
sheep$weight


weight
attach(sheep)
weight
detach()
weight


setwd("D:/BigData")
save.image("Introduction.Rdata")



1:10
10:1
2*1:10
1:10-1
1:(10-1)
seq(from=1,to=10,length=10)
seq(from=1,to=10,by=1)
seq(from=1,by=1,length=10)
seq(1,10,1)


A = array(data = 1:16, dim = c(4, 4))
A


A = array(data = 1:13, dim = c(4, 4)) # check it by yourself
A = array(data = 1:20, dim = c(4, 4)) # check it by yourself


?matrix
x = matrix (data = c(1,2,3,4) , nrow = 2, ncol = 2)
x

x = matrix (c(1,2,3,4), 2, 2)

x = matrix (data = c(1,2,3,4) , nrow = 2, ncol = 2, byrow = TRUE)
x

x = x^2
x = sqrt(x)
x

A = array(1:16, dim = c(4, 4))
B = array(2:17, dim = c(4, 4))
A + B # check it by yourself
A %*% B # check it by yourself


A = array(c(1,3,2,1),c(2,2))
b = array(c(1,0), c(2,1))
solve(A, b)
solve(A)  # inverse of A

A = outer(0:5, 0:5) # every possible value of AD and BC
freq = table(outer(A, A, "-")) # frequency for all values of AD - BC
plot(freq/sum(freq), xlab="Determinant value", ylab = "Probability")


x = c(1, 3, 2, 5)
y = c(2, 3, 5, 3)
length(x) # 4
length(y) # 4
x + y     # 3 6 7 8


A = matrix (data = 1:16, nrow = 4, ncol = 4)  # Here a:b is a sequence of integers between a and b.
A
A[2,3]
A[c(1,3), c(2,4)]
A[1:2, ]
A[-c(1,3), -c(1,3,4)]
dim(A)

listEx = list("Test", c(2, 0, 6), sheep) # sheep is a data frame
length(listEx)
listEx[2]
listEx[[2]] + c(1, 2, 4)

names(listEx) = c("String", "Vector", "DataFrame")
df = as.data.frame(listEx[["DataFrame"]])
View(df)  # Please check modes of listEx[["String"]] and listEx[["Vector"]]

rm(list=ls())

fileName = "Auto.data"
defaultDataDir = "C:/Users/qianc/Desktop"
fileLocation = file.path(defaultDataDir, fileName)
Auto = read.table(file = fileLocation)
View(Auto) # check it

Auto = read.table(file = fileLocation, header = T, na.strings = "?")

fileName = "Auto.csv"
fileLocation = file.path(defaultDataDir, fileName)
Auto = read.csv(file = fileLocation, header = T, na.strings = "?")
dim(Auto)

Auto = na.omit(Auto)
dim(Auto)

names(Auto) #check following commands
names(Auto)[1] = "New_Name_MPG"
names(Auto)


summary(Auto) # check it by yourself
summary(Auto$mpg)


plot(cylinders, mpg)

plot(Auto$cylinders, Auto$mpg, type = "p", pch = "x", col = "blue", xlab = "cylinders", ylab = "mpg")
attach(Auto)
plot(cylinders, mpg, type = "p", pch = "x", col = "blue", xlab = "cylinders", ylab = "mpg")


cylinders = as.factor(cylinders)
plot(cylinders, mpg, col = "red", varwidth = T, xlab = "cylinders", ylab = "mpg")


boxplot(mpg, xlab = "MPG", ylab = "Value")
boxplot(acceleration, cylinders) # check it by your self


hist(mpg, col = 2, breaks = 15)

pairs(Auto) # check it by yourself
pairs(~mpg + displacement + horsepower + weight + acceleration, Auto)

qqnorm(mpg, ylab = "mpg Quantiles")
qqline(mpg, col = 2, lwd = 2)


x = seq(from = -2, to = 2, length=50)
y = seq(from =-1, to =1, length=50)
par(mfrow = c(1, 2))
z = outer(x^2,y^3)
contour(x, y, z)
persp(x, y, z, theta = -50, phi = 20)


x = 10
y = 20
z = 10
x == y
x == z
x != y
x > y
x <= y
x == z & x != y
x == z | x == y
! x == z


i = 1  # try to write repeat loop by yourself
while(i < 6){
  print(i)
  i = i + 1
} 


y = rep("NA", times = length(x))
for(i in 1:length(x)){
  if(x[i] >= 0)
    y[i] = "non-negative"
  else
    y[i] = "negative"
}

x = seq(-5, 10, by = 1.5)
y = ifelse(x < 0, "negative", "non-negative")


fn = function(arg1){
  if(arg1 <= 0){
    100
    }
  else{
    2 + fn(arg1 - 1)}
  }
m = fn(5) # please check fn(-5). Guess it first : )
m


binomplot = function(size, prob=0.5, colour=3, outputvals=FALSE){
  x = 0:size
  y = dbinom(x, size, prob)
  plot(x, y, type="h", col=colour)
  if (outputvals) y
  }
binomplot(50)
binomplot(55, outputvals = TRUE, colour = 1)


pbinom(q = 60, size = 85, prob = 0.6)
pnorm(q = 15, mean = 12, sd = 3, lower.tail = FALSE) # 1 - pnorm(q = 15, mean = 12, sd = 3)
pbinom(q = 59, size = 85, prob = 0.6) * pnorm(q = 15, mean = 12, sd = 3, lower.tail = FALSE)


x = seq(from = -3, to = 3, length=200)
plot(x, dnorm(x), type="l")
lines(x, dt(x, df = 16), col = 2)
lines(x, dt(x, df = 8), col = 3)
lines(x, dt(x, df = 4), col = 4)

hist(Auto$mpg, breaks=seq(0,60,length = 20), probability = TRUE)
lines(x, dnorm(x, mean = mean(Auto$mpg), sd = sd(Auto$mpg)), col=2)

poissamp = rpois(n = 400, lambda = 2)
hist(poissamp, breaks = 0:10, probability = TRUE)
normsamp = rnorm(n = 250, mean = 10, sd = 5)
hist(normsamp, breaks = seq(-10, 30, length = 15), probability = TRUE)
x = seq(-10,30,length = 200)
lines(x, dnorm(x, mean = 10, sd = 5), col = 2)

set.seed(1)
rnorm(5)

nvec = 1:6
sample(nvec)
sample(nvec, size = 3)
sample(nvec, replace = TRUE)
sample(nvec, size = 10, replace = TRUE)


poisSampMean = function(n, lambda, r){
  simvals = rpois(n * r, lambda)
  simvals = matrix(simvals, n, r)
  colMeans(simvals)
  }
set.seed(1)
poisSampMean(10, 3, 6)


histNorm = function(data, nbins = 21){
  hist(data, breaks = seq(min(data), max(data), length=nbins), probability = TRUE, col = 5)
  x = seq(min(data), max(data), length = 200)
  lines(x, dnorm(x, mean = mean(data), sd = sd(data)), col = 2)
  }
histNorm(poisSampMean(8, 1, 1000))



