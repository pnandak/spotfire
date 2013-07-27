
# R Program to Read Data Within a Program.

# This stores the data as one long text string.
mystring <-
"id,workshop,gender,q1,q2,q3,q4
 1,1,f,1,1,5,1
 2,2,f,2,1,4,1
 3,1,f,2,2,4,3
 4,2, ,3,1, ,3
 5,1,m,4,5,2,4
 6,2,m,5,4,5,5
 7,1,m,5,3,4,4
 8,2,m,4,5,5,5"

# Read with more flexible read.table.
mydata <- read.table( textConnection(mystring),
   header=TRUE, sep=",", 
   row.names="id", na.strings=" ")
mydata

# Read again with shorter read.csv.
mydata <- read.csv( textConnection(mystring),
   row.names="id", na.strings="")
mydata

# Set working directory & save workspace.
setwd("/myRfolder")

save.image(file="myWorkspace.RData")

