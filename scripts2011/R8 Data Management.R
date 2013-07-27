
# R PROGRAM FOR DATA MANAGEMENT

# Set our Working Directory, load & print mydata

setwd("myRworkshop")

load(file="mydata.RData")

mydata

# ---MISSING VALUES---
#
# NA is missing, Not Available
#
# NaN is Not a Number, e.g. x/0, also treated as missing
#
# Missing are not negative infinity like SAS,
# nor are they positive infinity like Stata
#
# Like SPSS, comparisons with NA are NA
# 
# R reads numeric blanks as missing
#
# Reading "." will convert var to character/factor
#
# You can omit cases with any missing using:
# myNoMissing <- na.omit(mydata)




# ---TRANSFORMATION BASICS---
#
# SAS has separate data steps & proc steps
#
# SPSS/Stata can alternate 
# transformations & analysis commands
#
# R can alternate them:

mydata$q4Log <- log(mydata$q4)
summary( mydata$q4Log )
 
# Or nest them:
summary( log( mydata$q4 ) )

# ---THE transform() FUNCTION---
#
# Any of the variable selection methods will work 
# when transforming variables, except:
#   -attach() works with temp copies, so avoid
#   -with() accepts only one function at a time
#
# The transform function accepts more, 
# but cannot use new vars until call is finished

mydata <- transform( mydata,
  score1=(q1+q2)/2,
  score2=(q3+q4)/2  )

# ---TABLE OF TRANFORMATIONS---

Addition           x+y
Antilog, base 10   10^x 
Antilog, Natural   exp(x)
Division	       x/y
Exponentiation	 x^2 
Logarithm, base 10 log10(x) 
Logarithm, Natural log(x) 
Multiplication	 x*y
Round off	       round(x) 
Square Root	       sqrt(x)
Subtraction	       x-y

# ---PROCEDURES OR FUNCTIONS?---
#
# In SAS/SPSS/Stata,
# Procedure (commands) calculate down through 
# the rows, e.g. the mean of q1
#
# Functions calculate across values columns, 
# e.g. mean of q1 to q4 for each observation
#
# In R, only functions exist.
#
# Some functions operate on components of a list 
# (e.g. the vectors of a data frame) by default, but…
# 
# The apply() function can determine if any 
# function goes down columns or across rows!
#
# See Hadley Wickham’s “plyr” package for a 
# popular alternative “apply” functions.

# ---THE APPLY FAMILY OF FUNCTIONS---
#
# apply( x, margin, fun, …)
#
# X is the array to use (or matrix) and data frames 
# are coerced into a matrix when possible
#
# Margin chooses row or column with values 1=row
# or 2=column (higher dimensional arrays can use 
# margin vectors)
#
# Fun is the function to apply
#
# "..." represents arguments that will be passed to 
# the applied function.

mean( mydata[3:6], na.rm=TRUE )

apply( mydata[3:6], 2, mean, # 2=Columns
    na.rm=TRUE )

apply( mydata[3:6], 1, mean, # 1=Rows
  na.rm=TRUE)

# ---lapply() AND sapply()---
#
# Both work like apply but on components of a 
# list, so margin is irrelevant
#
# lapply returns the results as a list
# 
# sapply “simplifies” its results in the form 
# of a vector, matrix or array

lapply(mydata[3:6], mean, # List is the result
  na.rm=TRUE)

sapply(mydata[3:6],mean, # Simplifies to vector
  na.rm=TRUE)


# ---COUNTING VALID RESPONSES (1)---
#
# In SAS/SPSS/Stata, the N/NVALID/rownonmiss 
# functions count non-missing values
#
# In R, counts all values with:
# length(mydata$q3) 
#
# Using the prettyR package, count valid 
# values with:

library(prettyR)

valid.n(mydata$q3)

# ---COUNTING VALID RESPONSES (2)---
#
# R’s built-in approach to counting valid is:

sum( !is.na(mydata$q3) )

valid.n # print the function

# is.na() checks to see if each value is missing
#
# "!" is logical NOT, so it checks non-missing
#
# Vector of TRUE/FALSE values results 
#
# sum() adds the TRUE/FALSE values, 
# coercing TRUE/FALSE to 1/0


# ---ADDING TO A DATA FRAME---

mydata$meanQ <- apply(  
 mydata[3:6], 1,
  mean, na.rm=TRUE )
library("prettyR")
mydata$nQ <- apply( 
mydata[3:6], 1, valid.n )
mydata


# === DATA MANAGEMENT FOR SELF-STUDY ===

# ---CONDITIONAL TRANSFORMATIONS (1)---

# Uses form: var <- ifelse( condition, yes, no)

mydata$gals <- 
  ifelse( gender=="f",  1,  0 )
mydata

mydata$satisfied <- 
  ifelse( q1 >= 4, 1, 0 )
mydata

# ---CONDITIONAL TRANSFORMATIONS (2)---

mydata$score <- ifelse( gender=="f",
  (2*q1)+q2,
  (3*q1)+q2  )
mydata

# ---BY OR SPLIT-FILE PROCESSING---
#
# BY in SAS, Stata
#
# Split File in SPSS
#
# Able to save multiple values to an output list
# Uses the form:
#
# by( data, factor(s), FUN, … )
#
# Multiple factors must be in a list or data frame

by( q1, gender, mean, na.rm=TRUE )


# ---RECODING VARIABLES---

# (Could use indexing or ifelse too)

load(file="mydata.RData")
mydata

library("car")

mydata <- mydata  #make a fresh copy

mydata$qr1 <- recode( mydata$q1, "1=2; 5=4")
mydata$qr2 <- recode( mydata$q2, "1=2; 5=4")
mydata$qr3 <- recode( mydata$q3, "1=2; 5=4")
mydata$qr4 <- recode( mydata$q4, "1=2; 5=4")
mydata




# ---Missing Data---

myNoMissing <- na.omit(mydata)
myNoMissing

# Read messy data with no instructions for missing data.

mydataNA <- read.table("mydataNA.txt")

mydataNA

# Read again, excluding all three missing codes.

mydataNA <- 
  read.table("mydataNA.txt",
  na.strings=c(".", "9", "99") )

mydataNA

# Mean substitution for missing

mydataNA$q1[ is.na( mydataNA$q1 ) ] <- 
  mean( mydata$q1, na.rm=TRUE )

mydataNA




# ---RENAMING VARIABLES---

# Using Text Editor

fix(mydata)


# Renaming with names Function

# load a fresh copy of our data.

load(file="mydata.RData")
mydata

names(mydata)
 
names(mydata) <- c("group", "gender", "x1", "x2", "x3", "x4")
 
mydata


# Renaming Variables with rename Function

# load a fresh copy of our data.

load(file="mydata.RData")
mydata

library("reshape")

myChanges <- 
   c(q1="x1", q2="x2", q3="x3", q4="x4")

myChanges
 
mydata <- rename(mydata, myChanges)

mydata




# ---KEEPING AND DROPPING VARIABLES---

load(file="mydata.RData")
mydata


myleft <- mydata[ ,1:4] # Keeping.

myleft

#Or assign NULL to vars to drop:

myleft <- mydata 

myleft

myleft$q3 <- myleft$q4 <- NULL  # Dropping

myleft




# ---STACKING / ADDING CASES---

# Create female data frame.

females <- mydata[ which(gender=="f"), ]
females

# Create male data frame.

males <- mydata[ which(gender=="m"), ]
males

#Bind their rows together with the rbind function.

both <- rbind(females, males)
both

# Drop q2 to see what happens.

males$q2 <- NULL
males

# See that row bind will not work.

both <- rbind(females, males)

# Use reshape's rbind.fill function.

library("reshape")
both <- rbind.fill(females, males)
both

# Add a q2 variable to males.
males <- data.frame( males, q2=NA )
males

# Now rbind can handle it.
both <- rbind(females,males)
both




# ---MERGING / JOINING DATA FRAMES---

setwd("/myRfolder")

# Read data keeping ID as a variable.
mydata <- read.table("mydata.csv",
  header=TRUE,sep=",",na.strings=" ")
mydata

#Create a data frame keeping the left two q variables.
myleft <- mydata[ c("id","workshop","gender","q1","q2") ]
myleft

#Create a data frame keeping the right two q variables.
myright <- mydata[ c("id","workshop","q3","q4") ]
myright

# Merge the two dataframes on both id and workshop.
# all argument keeps them all like SAS/SPSS merges
both <- merge(myleft, myright, all) 
both

#Merge the two dataframes by ID only.
both <- merge(myleft, myright,
            by.x="id", by.y="id" )
both

#Merge dataframes specifying both ID and workshop.
both <- merge(myleft,myright,by=c("id","workshop"))
both

#Merge dataframes by both ID and workshop,
#while allowing them to have different names.
both <- merge(myleft,
            myright,
            by.x=c("id","workshop"),
            by.y=c("id","workshop") )
both




# ---AGGREGATING / SUMMARIZING DATA---

attach(mydata)

# Using aggregate Function
# ------------------------
# Results easy to read
# Results easy to program
# Accepts only functions with 1 result

myAgg <- aggregate(q1,
 by=data.frame(gender,workshop), 
 mean, na.rm=TRUE)

myAgg # A data frame.


# Using tapply Function
# ------------------------------------------------
# Results not easy to read for more than 2 factors
# Results easy to program
# Accepts any function

myAgg <- tapply(q1, 
   data.frame(workshop,gender), 
   mean, na.rm=TRUE) 
 
myAgg # A matrix or array.

# Counted Aggregates

table(workshop) 
 
table(gender,workshop)

myCounts <- 
  table(gender, workshop)

myCounts  # A table.

# Convert to data frame for ease of programming

myCountsDF <- as.data.frame(myCounts)
myCountsDF


# ---BY OR SPLIT FILE PROCESSING---

# Using by Function
# ----------------------------
# Results very easy to read
# Results hard to program
# Accepts any function

by(q1, 
   data.frame(workshop,gender), 
   mean, na.rm=TRUE
) 


# ---RESHAPING DATA---

mydata$subject <- 1:8
mydata

library("reshape")
 
mylong <- melt(mydata, id=c("subject","workshop","gender") )
 
mylong

# Return to Wide Format

mywide <- cast(mylong, subject+workshop+gender ~ variable)
 
mywide


# ---SORTING DATA---

# See that it's sorted by gender.

mydata

# Now sort by workshop.

myW   <- order( workshop )
mydata[ myW, ]

# rev function can reverse it
# with numeric vars, negative sign does too

mydata[ rev(myW), ] #reverses.

myWG <- order( workshop,gender )
mydata[ myWG, ]

# Keep a sorted copy

mySortedWG <- mydata[ myWG, ]
mySortedWG


















