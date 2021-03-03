filename = 'communityCrimeData.txt'
filename2 = 'communityCrimeAttributes.txt'
defaultDataDir = "/Users/kyle/Documents/LSE-PKU/tutorial_class/Extra Data/communityCrimeData"
fileLocation = file.path(defaultDataDir,filename)
fileLocation2 = file.path(defaultDataDir,filename2)
CC = read.table(file = fileLocation, header = F, sep = ',')
View(CC)
dim(CC)
?read.delim
ccAttr = read.delim(file = fileLocation2, header = F, sep = ' ', skip = 2) #What is read.delim????
dim(ccAttr)
names(CC) = ccAttr[,1] #assign column names to file CC
install.packages('dplyr')
library(dplyr)
tbl_df(CC) #Displays only the first 10 rows and the number of columns fit to the display
bigPopul = filter(CC, population >= 800000) #select rows with population >= 800K
dim(bigPopul)
bigPopul[,1]
attach(CC)
summary(murdPerPop)
tt = filter(CC, population >= 800000, murdPerPop > 8.365)
tt[,1]
