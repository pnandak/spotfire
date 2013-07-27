
# R Program for Aggregating/Summarizing Data.

setwd("/myRfolder")
load(file="myWorkspace.RData")
attach(mydata)
mydata

# The aggregate Function.

# Means by gender.
myAgg1 <- aggregate(q1, 
  by=data.frame(gender), 
  mean, na.rm=TRUE)
myAgg1

# Now by workshop and gender. 
myAgg2 <- aggregate(q1, 
  by=data.frame(workshop, gender), 
  mean, na.rm=TRUE) 
myAgg2
mode(myAgg2)
class(myAgg2)

# Aggregation with tapply.

myAgg2 <- tapply(q1, 
  data.frame(workshop,gender), 
  mean, na.rm=TRUE) 
myAgg2
class(myAgg2)
mode(myAgg2)

myAgg2 <- tapply(q1, 
  data.frame(workshop,gender), 
  range, na.rm=TRUE) 
myAgg2
mode(myAgg1)
class(myAgg2)
myAgg2[[1]]

# Example multi-level transformation.

mydata$Zq1 <- (q1 - mean(q1) ) / sd(q1)
mydata

mySubset <- mydata[ q1 < mean(q1), ]
mySubset

# Rename x to be mean.q1.
library("reshape")
myAgg3 <- rename(myAgg2, c(x="mean.q1"))
myAgg3

# Now merge means back with mydata.
mydata2 <- merge(mydata,myAgg3,
  by=c("workshop","gender") )
mydata2

# Tables of Counts
table(workshop)
table(gender,workshop)
myCounts <- table(gender, workshop)
mode(myCounts)
class(myCounts)

# Counts in Summary/Aggregate style.
myCountsDF <- as.data.frame(myCounts)
myCountsDF
class(myCountsDF)

# Clean up
mydata["Zq1"] <- NULL
rm(myAgg1, myAgg2, myAgg3,
  myComplete, myMeans, myCounts, myCountsDF)

