x = 1:100
x
y = sum(x)
y
rm(y)
y = (1:100)^2
y[14]
log(y)
summary(y)
summary(x)
dim(y)
exam = read.table("book1.csv", header = TRUE, sep = ",")
help("read.table")
dim(exam)
summary(exam)
View(exam)
exam
plot(exam)
summary(exam$Score)
pie(exam$Score)
help(pie)
hist(exam$Age)
hist(exam[,3])
x = rnorm(100)
y = rnorm(100)
plot(x,y)

x = seq(1,10) # to create a sequence of numbers


