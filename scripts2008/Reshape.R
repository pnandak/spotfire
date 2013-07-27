
# R Program to Reshape Data.

setwd("/myRfolder")
load(file="myWorkspace.RData")

# Create an id variable.
mydata$subject <- 1:8
mydata

library("reshape")
attach(mydata)

# Melt data into "long" format.
mylong <- melt(mydata, id=c("subject","workshop","gender") )
mylong

# Cast data back into "wide" format.
mywide <- cast(mylong, 
  subject+workshop+gender ~ variable)
mywide

