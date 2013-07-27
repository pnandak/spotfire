
# R Program for Conditional transformations.

setwd("/myRfolder")
load(file="myWorkspace.RData")
mydata
attach(mydata) 

#Create a series of dichotomous 0/1 variables

# The new variable q4SAgree will be 1 if q4 equals 5, 
# (Strongly agree) otherwise zero.
mydata$q4Sagree <- ifelse( q4 == 5, 1,0)
mydata$q4Sagree

# This does the same as above.
mydata$q4Sagree <- as.numeric( q4 == 5 )
mydata$q4Sagree

# Create a score for people who agree with q4.
mydata$q4agree <- ifelse( q4 >= 4, 1,0)
mydata$q4agree

# Find the people only in workshop1 agree to item 5.
mydata$ws1agree <- ifelse( workshop == 1 & q4 >=4 , 1,0)
mydata$ws1agree

# Use equations to calculate values.
mydata$score <- ifelse( gender=="f",(2*q1)+q2,(3*q1)+q2 )
mydata

