
# R5 PROGRAMMING LANGUAGE BASICS

# Set our Working Directory
setwd("myRworkshop")


# ---COMMANDS---

# This is one way to read & print data

mydata<-read.table("mydata.tab")
print(mydata)

# This is another, exactly equivalent, way

mydata  <- 
   read.table  ("mydata.tab")
print(
   mydata
   )





# ---SIMPLE CALCULATIONS---

2+3

1:20




# ---SETTING OPTIONS---

options(width=30)

1:20

options(width=60)




# ---USING SYMBOLS---

# The assignment operator is "<-"
# "=" will work, but avoid it

x<-2

y<-3

x+y

x*y




# ---VECTORS---

# A Numeric Vector

workshop <- c(1,2,1,2,1,2,1,2)

print(workshop)

workshop

# Addition appies to every value

workshop + 10 #this 10 is "recycled"

workshop+workshop

# Multiplication applies to every value too

5*workshop

table(workshop)

summary(workshop)


# A Character Vector

gender <- c("f","f","f",NA,"m","m","m","m")

gender

table(gender)

summary(gender)


q1 <- c(1,2,2,3,4,5,5,4)
q2 <- c(1,1,2,1,5,4,3,5)
q3 <- c(5,4,4,NA,2,5,4,5)
q4 <- c(1,1,3,3,4,5,4,5)


# Functions & their Arguments

# The numbers are "arguments"
workshop<-c(1,2,1,2,1,2,1,2)

# This function call counts
# but output is sparse!

table(workshop) 

# This function call gets the mean

mean(q3)

# ...but is missing unless you do this:

mean(q3, na.rm=TRUE)




# ---FACTORS---

# A Numeric Factor

summary(workshop)

workshop <- factor(
workshop,
levels=c(1,2,3,4),
labels=c("R","SAS","SPSS","STATA")
)
workshop
summary(workshop)

# A Character Factor

gender

summary(gender)

gender <- factor(gender)

gender

summary(gender)

gender <- factor(
  gender,
  levels=c("m","f"),
  labels=c("Male","Female")
)
gender

summary(gender)




# ---DATA FRAMES---

# Repeat our vectors, just for review

workshop <- c(1,2,1,2,1,2,1,2)
workshop <- factor(workshop,
   levels = c(1,2,3,4),
   labels = c("R","SAS","SPSS","STATA") )
gender <- c("f","f","f",NA,"m","m","m","m")
gender <- factor(gender)
q1 <- c(1,2,2,3,4,5,5,4)
q2 <- c(1,1,2,1,5,4,3,5)
q3 <- c(5,4,4,NA,2,5,4,5)
q4 <- c(1,1,3,3,4,5,4,5)

# Now combine them into a data frame

mydata <- data.frame(workshop,gender,q1,q2,q3,q4)

mydata

names(mydata)

row.names(mydata)




# ---MATRICES---

# Creating a matrix from vectors

mymatrix <- cbind(q1, q2, q3, q4)

mymatrix

# Creating the same matrix with the c function

mymatrix <- matrix(
  c( 1, 1, 5, 1,
     2, 1, 4, 1,
     2, 2, 4, 3,
     3, 1,NA, 3,
     4, 5, 2, 4,
     5, 4, 5, 5,
     5, 3, 4, 4,
     4, 5, 5, 5),
  nrow=8, ncol=4, byrow=TRUE)

mymatrix

# Calling a few functions on our matrix

dim(mymatrix)

table(mymatrix)

cor( mymatrix, use="pairwise" )




# ---LISTS---

# Create a list, notice mymatrix!

mylist <- list(workshop, gender, 
  q1, q2, q3, q4, mymatrix)

mylist




# ---SAVING RESULTS---

# These are commented out to stop us from 
# accidentally destroying our practice data.

# save(workshop,gender,q1,q2,q3,q4,
  file="myVectors.RData")

# save(mydata, file="mydata.RData")

# save(mylist, file="myList.RData")




# ----------------
# ---BREAK TIME---
# ----------------



#---CONTROLLING FUNCTIONS---
# This section is a systematic review of
# what we did above, so there is little programming here

class(mydata)

methods(summary)



#---HOW MUCH OUTPUT IS THERE?---

mymodel <- lm(mydata$q4~mydata$q1)

print(mymodel)

names(mymodel)

mymodel$coefficients

mymodel$model

class(mymodel)

print( unclass(mymodel) )




# ---WRITING FUNCTIONS---

# A bad function.

mystats <- function(x) 
{
  mean(x, na.rm=TRUE)
    sd(x, na.rm=TRUE) 
}

mystats

myvar <- c(1,2,3,4,5)

mystats(myvar) 


# A better function.
mystats <- function(x) 
{
  print( mean(x, na.rm=TRUE) )
  print(   sd(x, na.rm=TRUE) )  
}

mystats

mystats(myvar)

myResults <- mystats(myvar)
myResults

#Good function with vector output.
mystats <- function(x) 
{
  mymean <- mean(x, na.rm=TRUE) 
  mysd   <-   sd(x, na.rm=TRUE)     
  c(mean=mymean,sd=mysd) 
}
mystats
 
mystats(myvar)

myResults <- mystats(myvar)
myResults


# Good function with list output.

mystats <- function(x) 
{
  myinput <- x
  mymean  <- mean(x, na.rm=TRUE) 
  mysd    <-   sd(x, na.rm=TRUE)     
  list(data=myinput, mean=mymean, sd=mysd) 
}

mystats

mystats(myvar)


myResults <- mystats(myvar)

myResults

save(mydata,mymatrix,mylist,mystats,
  file="myAll.RData")




# ---DOCUMENTING YOUR PROGRAMS---
# See slides for examples




# ---VARIABLE LABELS---

library("Hmisc")

label(mydata$q1) <- "The instructor was well prepared."
label(mydata$q2) <- "The instructor communicated well."
label(mydata$q3) <- "The course materials were helpful."
label(mydata$q4) <- "Overall, I found this workshop useful."

# Hmisc describe function uses the labels.

describe( mydata[ ,3:6] )

# Buit-in summary function ignores the labels.

summary( mydata[ ,3:6] )








