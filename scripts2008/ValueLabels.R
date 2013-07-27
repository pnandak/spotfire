
# R Program to Assign Value Labels & Factor Status.

setwd("/myRfolder")

# Character Factors

# Read gender as factor.
mydata <- read.table("mydata.tab")
mydata
class( mydata[ ,"gender"] )

# Read gender as character.
mydata2 <- read.table("mydata.tab", as.is=TRUE)
mydata2
class( mydata2[ ,"gender"] )
summary( mydata2$gender )
rm(mydata2) 


# Numeric Factors

class( mydata$workshop )
summary( mydata$workshop )

summary( as.factor(mydata$workshop) )

# Now change workshop into a factor:
mydata$workshop <- factor( mydata$workshop,
  levels=c(1,2,3,4),
  labels=c("R","SAS","SPSS","Stata") )
mydata

# Now see that summary only counts workshop attendance.
summary(mydata$workshop)

# Making the Q Variables Factors

# Store levels to use repeatedly.
myQlevels <- c(1,2,3,4,5)
myQlevels

# Store labels to use repeatedly.
myQlabels <- c("Strongly Disagree",
               "Disagree",
               "Neutral",
               "Agree",
               "Strongly Agree")
myQlabels

# Now create a new set of variables as factors.
mydata$qf1 <- ordered( mydata$q1, myQlevels, myQlabels)
mydata$qf2 <- ordered( mydata$q2, myQlevels, myQlabels)
mydata$qf3 <- ordered( mydata$q3, myQlevels, myQlabels)
mydata$qf4 <- ordered( mydata$q4, myQlevels, myQlabels)

# Get summary and see that workshops are now counted.
summary( mydata[ c("qf1","qf2","qf3","qf4") ] )

# Making Factors of Many Variables

# Generate two sets of var names to use.
myQnames  <- paste( "q",  1:4, sep="")
myQnames
myQFnames <- paste( "qf", 1:4, sep="")
myQFnames

# Extract the q variables to a separate data frame.
myQFvars <- mydata[ ,myQnames]
myQFvars

# Rename all the variables with F for Factor.
names(myQFvars) <- myQFnames
myQFvars

# Create a function to apply the labels to lots of variables.
myLabeler <- function(x) { 
  ordered(x, myQlevels, myQlabels) 
}

# Here's how to use the function on one variable.

summary( myLabeler(myQFvars[,"qf1"]) )
summary( myLabeler(myQFvars["qf1"]) ) # Doesn't work!

# Apply it to all the variables.
myQFvars <- data.frame( sapply( myQFvars, myLabeler ) ) 

# Get summary again, this time with labels.
summary(myQFvars)

# You can even join the new variables to mydata.
# (this gives us two labeled sets if you ran 
# the example above too.)
mydata <- cbind(mydata,myQFvars)
mydata

#---Converting Factors into Character or Numeric Variables

# Converting the gender factor, first with as.numeric.

mydata$genderNums <- as.numeric( mydata$gender ) 
mydata$genderNums

# and again with as.character.

mydata$genderChars <- as.character( mydata$gender)
mydata$genderChars

# Converting the qf1 factor.

mydata$qf1Nums <- as.numeric(mydata$qf1)
mydata$qf1Nums

mydata$qf1Chars <- as.character(mydata$qf1)
mydata$qf1Chars

# Example with bigger values.
x <- c(10,20,30)
x
xf <- factor(x)
xf
as.numeric(xf)
as.character(xf)
x10 <- as.numeric( as.character(xf) )
x10

