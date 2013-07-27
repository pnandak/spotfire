
# R Program for Recoding Variables.

setwd("/myRfolder")
load(file="myWorkspace.RData")
mydata
attach(mydata)

library("car")
mydata$qr1 <- recode(q1, "1=2; 5=4")
mydata$qr2 <- recode(q2, "1=2; 5=4")
mydata$qr3 <- recode(q3, "1=2; 5=4")
mydata$qr4 <- recode(q4, "1=2; 5=4")
mydata

# Do it again, stored in new variable names.
load(file="myWorkspace.RData")
attach(mydata)

# Generate two sets of var names to use.
myQnames <-  paste( "q",  1:4, sep="")
myQnames
myQRnames <- paste( "qr", 1:4, sep="")
myQRnames

# Extract the q variables to a separate data frame.
myQRvars <- mydata[ ,myQnames]
myQRvars

# Rename all the variables with R for Recoded.
names(myQRvars) <- myQRnames
myQRvars

# Create a function to apply the labels to lots of variables.
myRecoder <- function(x) { recode(x,"1=2;5=4") }

# Here's how to use the function on one variable.
myQRvars$qr1
myRecoder(myQRvars$qr1) 

#Apply it to all the variables.
myQRvars <- sapply( myQRvars, myRecoder) 
myQRvars

# Save it back to mydata if you want.
mydata <- cbind(mydata,myQRvars)
mydata
summary(mydata)

