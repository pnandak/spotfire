
# R Program for By or Split File Processing.

setwd("/myRfolder")
load(file="myWorkspace.RData")
attach(mydata)
options(width=64)
mydata

# Get means of q variables for all observations.
mean( mydata[ c("q1","q2","q3","q4") ] , 
       na.rm=TRUE)

# Now get means by gender.
myBYout <- by( mydata[ c("q1","q2","q3","q4") ] , 
    mydata["gender"], 
    mean,na.rm=TRUE)
myBYout
mode(myBYout)
class(myBYout)

myBYdata <- as.data.frame( (as.table(myBYout) ) )
myBYdata

# Get range by workshop and gender
myVars <- c("q1","q2","q3","q4")
myBys  <- mydata[ c("workshop","gender") ]
myBYout <- by( mydata[myVars], 
  myBys, range, na.rm=TRUE )
myBYout

# Converting output to data frame.
mode(myBYout)
class(myBYout)
names(myBYout)
myBYout[[1]]

# A data frame the long way.
myBYdata <- data.frame( 
  rbind(myBYout[[1]], myBYout[[2]], 
        myBYout[[3]], myBYout[[4]])
)
myBYdata

# A data frame using do.call.
myBYdata <- data.frame( do.call( rbind, myBYout) ) 
myBYdata
mode(myBYdata)
class(myBYdata)

