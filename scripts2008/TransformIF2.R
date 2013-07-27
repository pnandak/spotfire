
# R Program for Multiple Conditional Transformations.
# Read the file into a data frame and print it.

setwd("/myRfolder")
load(file="myWorkspace.RData")
attach(mydata)
mydata 

# Using the ifelse approach.
mydata$score1 <- 
  ifelse( gender=="f",(2*q1)+q2,(20*q1)+q2 )
mydata$score2 <- 
  ifelse( gender=="f",(3*q1)+q2,(30*q1)+q2 )
mydata


# Using the index approach.

load(file="myWorkspace.RData")

# Create names in data frame.
mydata <- data.frame( mydata, score1=0, score2=0 )
attach(mydata)
mydata

# Find which are males and females.
gals <- which( gender=="f" ) 
gals
guys <- which( gender=="m" ) 
guys

mydata[gals,"score1"] <-  2*q1[gals] + q2[gals]
mydata[gals,"score2"] <-  3*q1[gals] + q2[gals]
mydata[guys,"score1"] <- 20*q1[guys] + q2[guys]
mydata[guys,"score2"] <- 30*q1[guys] + q2[guys]
mydata

# Clean up.
rm(guys, gals)

