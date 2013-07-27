# R Program for Renaming Variables.

setwd("/myRfolder")
load(file="myWorkspace.RData")
mydata

#---This uses the data editor. 
#Make the changes by clicking on the names in the spreadsheet, then closing it.
fix(mydata)
mydata

# Restore original names for next example.
names(mydata) <- c("group", "gender", 
  "q1", "q2", "q3", "q4")

#---This method is most like SAS or SPSS.
# It is easy to understand but does not 
# help you understand what R is actually doing. 
# It requires the reshape package.

library("reshape")
myChanges <- c(q1="x1",q2="x2",q3="x3",q4="x4")
myChanges

mydata <- rename(mydata, myChanges)
mydata 

# Restore original names for next example.

names(mydata) <- c("group", "gender", 
  "q1", "q2", "q3", "q4")

#---Simplest renaming with no packages required.
# With this approach, you simply list every name 
# in order, even those you don't need to change.

names(mydata) <- c("group", "gender", 
  "x1", "x2", "x3", "x4")
mydata

# Restore original names for next example.
names(mydata) <- c("group", "gender", 
  "q1", "q2", "q3", "q4")

#---The edit function actually generates 
# a list of names for you. If you edit them 
# and close the window, they will change.
names(mydata) <- edit( names(mydata) )
mydata

# Restore original names for next example.
names(mydata) <- c ("workshop", "gender", 
  "q1", "q2", "q3", "q4")

#---This method uses the row index numbers.
# First, extract the names to work on.
mynames <- names(mydata) 

# Now print the names. Data.frame adds index numbers.
data.frame(mynames))

mynames[3] <- "q1" 
mynames[4] <- "q2"
mynames[5] <- "q3"
mynames[6] <- "q4"
names(mydata) <- mynames #Put new names into data frame.
mydata

# Restore original names for next example.
names(mydata) <- c("group", "gender", 
  "q1", "q2", "q3", "q4")

#---Here's the exact same example, but now you 
# do not need to know the order of each variable 
# i.e. that q1 is column [3].

mynames <- names(mydata) #Make a copy to work on
mynames

mynames[ mynames=="q1" ] <- "x1"
mynames[ mynames=="q2" ] <- "x2"
mynames[ mynames=="q3" ] <- "x3"
mynames[ mynames=="q4" ] <- "x4"
mynames

# Finally replace the names with the new ones.
names(mydata) <- mynames
mydata

# Restore original names for next example.
names(mydata) <- c ("group", "gender", 
  "q1", "q2", "q3", "q4")


#---You can do the steps above without working 
# on a copy, but I find it VERY confusing to read.

names(mydata)[names(mydata)=="q1"] <- "x1" 
names(mydata)[names(mydata)=="q2"] <- "x2" 
names(mydata)[names(mydata)=="q3"] <- "x3" 
names(mydata)[names(mydata)=="q4"] <- "x4" 
print(mydata)

# Restore original names for next example.

names(mydata) <- c("group", "gender", 
  "q1", "q2", "q3", "q4")


#---This approach works well for lots of numbered 
# variable names names like x1,x2...
# First we'll see what the names look like.
names(mydata)

# Next we'll generate x1,x2,x3,x4 
#          to replace q1,q2,q3,q4.

myXs <- paste( "x", 1:4, sep="")
myXs

# Now we want to find out where to put the new names.

myA <- which( names(mydata)=="q1" )
myA

myZ <- which( names(mydata)=="q4" )
myZ

# Replace q1 thru q4 at index values A thru Z with
# the character vector of new names.

names(mydata)[myA:myZ] <- myXs(mydata)

#remove the unneeded objects.

rm(myXs, myA, myZ)

