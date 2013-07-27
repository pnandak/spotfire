
# R Program for Removing Duplicate Observations.

setwd("/myRfolder")
load("myWorkspace.RData")
mydata

# Create some duplicates.
myDuplicates <- rbind( mydata, mydata[1:2,] )
myDuplicates

# Get rid of duplicates without seeing them.
myNoDuplicates <- unique(myDuplicates)
myNoDuplicates

# This checks for location of duplicates
# before getting rid of them.

myDuplicates <- rbind( mydata, mydata[1:2,] )
myDuplicates

myDuplicates$Duplicated <- duplicated(myDuplicates)
myDuplicates 

# Print a report of just the duplicates.
myDuplicates[ Duplicated, ]

# Remove duplicates and Duplicated variable.
attach(myDuplicates)
myNoDuplicates <- myDuplicates[ !Duplicated, -7]
myNoDuplicates

