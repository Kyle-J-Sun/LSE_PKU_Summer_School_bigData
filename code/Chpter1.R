str = 'Hello world'
str
numbers = c(2,3,4)
numbers
str1 = c('Ali','Pay')
str1
c(1,2,3)+c(2,3,4)

small = c(1,2)
large = c(0,0,0,0,0,0)
large_error = c(0,0,0,0,0)
large+small
large_error+small

ls()
rm()

rm(list = ls()) #remove all objects form memory
#Control + L: Clear the console  cat('/014')
weight = c(84.5, 72.6, 75.7, 94.8, 71.3)
height = c(86.5, 71.8, 77.2, 84.9, 75.4)
sheep = data.frame(weight, height)
mean(sheep$height)
View(sheep) # View it in a spreadsheet
sheep$backlength = c(130.4, 100.2, 109.4, 140.6, 101.4)
sheep

rm(height, weight)
weight

sheep$weight
attach(sheep) #it is equivalent to open your dataframe
weight
detach() # close your datafarme
weight

?setwd
#setwd('/Users/kyle/Downloads/BigData')

1:10
10:1

seq(1,10)
help("seq")

ar = array(data = 1:16, dim = c(4,4))
ar

ar1 = array(data = 1:13, dim = c(4,4)) # recycle rule applied
ar1
ar2 = array(data = 1:20, dim = c(4,4)) # discard extra values
ar2

?matrix
x = matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
x

Ar_1 = array(1:16, dim = c(4,4))
Ar_2 = array(2:17, dim = c(4,4))
Ar_1 + Ar_2

Ar_1%*%Ar_2 #矩阵相乘

#to solve Ax = B
A = array(c(1, 3, 2, 1), c(2, 2))
A
B = array(c(1, 0), c(2, 1))
B
solve(A,B)
solve(A) # inverse of A
#Ctrl + Shift + C: to change codes into comment
#Ctrl + Shift + A: to format the code

O = outer(0:5, 0:5) # every possible value of AD and BC
O
freq = table(outer(O, O, "-")) #frequency for all values of AD-BC
freq
?table
plot(freq/sum(freq),xlab = "Determinant Value", ylab = "Probability")

t(ar1) #Transpose
nrow(ar1) #The number of rows
ncol(ar1) #The number of columns
?rbind()
rbind(ar1) #????????????????
cbind(ar1) #????????????????
eigen(ar1) #eigenvectors and eigenvalues
svd(ar1) #singular value decomposition
z = c(1, 3, 2, 5)
h = c(2, 3, 5, 3)
length(z) 
length(h) 

#List
listEx = list("Test", c(2,0,6), sheep)
listEx
length(listEx)
listEx[2]
listEx[[2]] + c(1,2,4)

names(listEx) = c('String', 'Vector', 'DataFrame')
df = as.data.frame(listEx[['DataFrame']])
View(df)
dfs = as.data.frame(listEx[['String']])
View(dfs)
dfv = as.data.frame(listEx[['Vector']])
View(dfv)

#Loading data
fileName = "Auto.data"
defaultDataDir = "/Users/kyle/Documents/LSE-PKU/tutorial_class/data"
fileLocation = file.path(defaultDataDir, fileName)
Auto = read.table(file = fileLocation, header = TRUE, na.strings = "?")
View(Auto)

#Loading CSV
fileNameCSV = "Auto.csv"
fileLocationCSV = file.path(defaultDataDir, fileNameCSV)
AutoCSV = read.csv(file = fileLocationCSV, header = T, na.strings = "?")
dim(AutoCSV)
names(AutoCSV) #Check the variable names
names(AutoCSV)[1] = "New_Name_MPG" #change the name for the first column
names(AutoCSV)
summary(AutoCSV)
summary(AutoCSV$New_Name_MPG)
plot(Auto$cylinders,AutoCSV$mpg, type = 'p', pch = "x", col = 'Blue', xlab = "Cylinders", ylab = 'mpg')
attach(Auto)
cylinders = as.factor(cylinders) # converse it into numeric format
plot(cylinders, mpg, col = "red", varwidth = T, xlab = "cylinders", ylab = "mpg")
boxplot(mpg, xlab = "MPG", ylab = "Value")
boxplot(acceleration, cylinders)
hist(mpg, col = 2, breaks = 9)
pairs(Auto) #creates a scatterplot matrix
pairs(~mpg+displacement+horsepower+weight+acceleration, Auto)

#Graphics for qqnorm() Function
qqnorm(mpg, ylab = "mpg Quantiles") #plots the sample quantiles against the quantiles from a normal distribution
qqline(mpg, col = 2, lwd = 2)

#Graphics for 3D Plot
x = seq(from = -2, to = 2, length = 50)
y = seq(from = -1, to = 1, length = 50)
#par(): Set or query graphical parameters
#contour(): produces a contour plot in order to represent 3-dimensional data.
#persp(): draws perspetive plots of a surface over the xy plane.
#theta and phi: Control the angles at which the plot is viewd
par(mfrow = c(1,2))
z = outer(x^2,y^3)
contour(x,y,z)
persp(x,y,z, theta = -50, phi = 20)

setwd("/Users/kyle/Documents/LSE-PKU/tutorial_class/data")
A1 = read.csv("Advertising.csv")
A2 = read.table("Advertising.csv", sep = ',', na.strings = 'NA') #识别异常值
A3=na.omit(A2) #删除异常值

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

for (i in 1:5) print(i)

i = 1
while (i < 6) {
  print(i)
  i = i + 1
}

y = rep('NA', times = length(x))
for(i in 1:length(x)){
  if(x[i] >= 0)
    y[i] = 'non-negative'
  else
    y[i] = "negative"
  
}

#Define a function
x = seq(-5, 10, by = 1.5)
y = ifelse(x<0, "negative", "non-negatvie")

fn = function(arg1){
  if(arg1 <= 0){
    100
  }else{
    2 + fn(arg1 - 1)
  }
}
m = fn (5)
m

#Writing your own Function
#Draw the binomial mass function for some values of number of trial n (size) and probability of success p (prob)
#outputvals is a logical value; if outputvals is T, R will print out the values used to generate the plot
binomplot = function (size, prob = 0.5, colour = 3, outputvals = F)
  { xx = 0: size
    yy = dbinom(xx, size, prob)
    plot (xx, yy, type = "h", col = colour)
    if (outputvals) yy#????
}
binomplot(50)
binomplot(55, outputvals = T, colour = 1)

#Probability and Quantile Calculations
#X ~ Bin(85,0.6) Y ~ N(12,9)   Calculate P(X <= 60)  P(Y > 15)  P(X < 60, Y > 15)
pbinom(q = 60, size = 85, prob = 0.6)
pnorm(q = 15, mean = 12, sd = 3, lower.tail = F)
pbinom(q = 59, size = 85, prob = 0.6) * pnorm(q = 15, mean = 12, sd = 3, lower.tail = F)

#Density and Cumulative Distribution
x = seq(from = -3, to = 3, length = 200)
plot(x, dnorm(x), type = "l")
lines(x, dt(x, df = 16), col = 2)
?lines
?dt
lines(x, dt(x, df = 8), col = 3)
lines(x, dt(x, df = 4), col = 4)


hist(Auto$mpg, breaks = seq(0, 60, length = 20), probability = T)
x=0:60
lines(x, dnorm(x, mean = mean(Auto$mpg), sd = sd(Auto$mpg)), col = 2)
?hist


#Generating Random Samples
poissamp = rpois(n = 400, lambda = 2)
hist(poissamp, breaks = 0:10, probability = T)
?rpois
normsamp = rnorm(n = 250, mean = 10, sd = 5)      
hist(normsamp, breaks = seq(-10, 30, length = 15), probability = T)
x = seq(-10, 30, length = 200)
x
lines(x, dnorm(x, mean = 10, sd = 5), col = 2)
set.seed(1)
rnorm(5)

nvec = 1:6
sample(nvec)
sample(nvec, size = 3)
sample(nvec, replace = T)
sample(nvec, size = 10, replace = T)

#Simulation Experiments
poisSampMean = function(n, lambda, r){
  simvals = rpois(n*r, lambda)
  simvals = matrix(simvals, n, r)
  colMeans(simvals)
}
set.seed(1)
poisSampMean(10,3,6)


histNorm = function(data, nbins = 21){
  hist(data, breaks = seq(min(data), max(data), length = nbins), probability = T, col = 5)
       x = seq(min(data), max(data), length = 200)
       lines(x, dnorm(x, mean = mean(data), sd = sd(data)), col = 2)
}
histNorm(poisSampMean(8, 1, 1000))

sun = function(x,y = 1){
  s = x + y
  m = x - y
  return(s)
}
sun(2,3)
