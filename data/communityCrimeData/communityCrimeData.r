ccD=read.table("communityCrimeData.txt", header=F, sep=",")
dim(ccD)
# ccD=read.table("communityCrimeData.txt", header=F, sep=",", row.names=1)

# create row names by combining symbols in Columns 1 and 2 together with '-'
row.names(ccD)=paste(ccD[,1], ccD[,2], sep="-")
row.names(ccD) # print out row names

# add the names of attributes (in another file) to columns
ccDAttr=read.delim("communityCrimeAttributes.txt", header=F, sep=" ", skip=2)
dim(ccDAttr) # show size 147 x 23
names(ccD)=ccDAttr[,1] # assign column names to file ccD
ccD[1:5,6:13] # print out Columns 6-13 of Rows 1-5
ccD[c(22,128,1435,819,56,1430,349,168,2101,1434), c(130:134,136,138,142,144,146,147)]

# Using dplyr
install.packages("dplyr")
library(dplyr)

tbl_df(ccD) # display only a part of data
bigPopul=filter(ccD, population>=800000) # slect rows with population >= 800K
dim(bigPopul) # 10 147
bigPopul[,1]

attach(ccD) # make the columns recognizable in R
summary(murdPerPop) # murdPerPop: No. of murders per 100K population
t=filter(ccD, population>=800000, murdPerPop>8.365)
t[,1]
t=filter(ccD, population>=800000, murdPerPop>20)
t[,1]
t=filter(ccD, population>=800000, murdPerPop>30)
t[,1]

t=arrange(ccD, population)
t[2215,1]

