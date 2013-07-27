
# DATA ACQUISITION

# Set our Working Directory
setwd("myRworkshop")




# ---THE DATA EDITOR---

# Create an empty data frame

mydata <- edit( data.frame() )

# Print it

mydata

#Edit it again

fix( mydata )

# This is same as fix(mydata)

mydata <- edit( mydata )




# ---TAB DELIMITED TEXT FILES---
# Missing is "NA"

mydata <- read.table("mydata.tab")

mydata




# ---COMMA SEPARATED VALUES

mydata <- read.csv("mydata.csv")

mydata


# Read again, this time
# Stripping out trailing "white" blanks
# and using ",," as missing

mydata <- read.csv("mydata.csv",
  strip.white=TRUE, na.strings="")

mydata


# If id variable has a name, add:
# ...header=TRUE,rownames=myIDvar)




# ---DATA WITIN AN R PROGRAM---

mystring <-
"  workshop,gender,q1,q2,q3,q4
 1,1,f,1,1,5,1
 2,2,f,2,1,4,1
 3,1,f,2,2,4,3
 4,2, ,3,1, ,3
 5,1,m,4,5,2,4
 6,2,m,5,4,5,5
 7,1,m,5,3,4,4
 8,2,m,4,5,5,5"

mystring


# Read mystring using textConnection.

mydata <- read.csv( textConnection(mystring),
   strip.white=TRUE, na.strings="")

mydata




# ---FIXED WIDTH TEXT FILES---

mydata <- read.fwf(
   file="mydataFWF.txt",
   width=c(2,-1,1,1,1,1,1),  #minus skips
   col.names=
     c("id","gender","q1","q2","q3","q4"),
   row.names="id",
   strip.white=TRUE,
   na.strings="",
   fill=TRUE #fills records that end early
)
mydata


# Do the same thing again using "macro substitution"

myfile <- "mydataFWF.txt"

myVarNames  <- 
  c("id","gender","q1","q2","q3","q4")

myRecord <- c(2,-1,1,1,1,1,1)

mydata <- read.fwf(
   file=myfile,
   width=myRecord,  
   col.names=myVarNames,
   row.names="id",
   na.strings="",
   fill=TRUE,
   strip.white=TRUE )

mydata


# ---MORE THAN 1 RECORD PER CASE---

myfile <- "mydataFWF.txt"

myVarNames  <- 
  c("id","group", "gender",
    "q1","q2","q3","q4",
    "q5","q6","q7","q8")

myRecord1  <- c( 2, 1, 1, 1, 1, 1, 1)

myRecord2  <- c(-2,-1,-1, 1, 1, 1, 1)

myRecords <- list( myRecord1, myRecord2 )

# Now plug them in and read the data.
# This code is an exact copy of the code for 1 record.
# Only the values of the arguments have changed.

mydata <- read.fwf(
   file=myfile,
   width=myRecords,  
   col.names=myVarNames,
   row.names="id",
   na.strings="",
   fill=TRUE,
   strip.white=TRUE )

mydata
