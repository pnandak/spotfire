
# R Program to Read Delimited Text Files. 
setwd("/myRfolder")

# Read a tab delimited file with un-named ID column.
mydata <- read.table("mydata.tab")
mydata

# Read a Comma Separated Value (CSV) file.
mydata <- read.table("mydata.csv", header=TRUE,
  sep=",", row.names="id", na.strings=" ")
mydata

# Read it again with read.csv.
mydata <- read.csv("mydata.csv", 
  row.names="id", na.strings=" ")
mydata

# Now use colClasses to skip q1 and q2 with NULL.
 
myCols <- read.table("mydata.csv", header=TRUE,
  sep=",", row.names="id", na.strings=" ",
  colClasses=c("integer", "integer", "character",
  "NULL", "NULL", "integer", "integer") )
myCols

# Clean up and save workspace.
rm(myCols)
save.image(file="mydata.RData")

