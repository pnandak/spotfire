# Gov 2001
# Section I

# INTRODUCTION TO R
# Jens Hainmueller
# Feb 2008

## The goal for today is to introduce you to some of the most basic concepts and commands in R.
## R is a free software environment for statistical computing and graphics that we will use for the class.
## Today we will examine just a small number of important concepts such as the idea of data types and modes,
## dataset handling, and a bit of controls structures such as loops.
## Needless to say, we will only be able to cover a very limited set of commands today and
## we STRONGLY recommend you to work through the other R resources listed on the course website.
## In particular, Maindonald Guide to R, Chris Green's R Primer, Alexis Diamond's guide to R help.
## Also please take a look at the R website at http://www.r-project.org/ which has
## the details on how to install R, who wrote R, etc. Chris Green's R primer also contains installation instructions.

## also please print out a copy of the R reference cards that summarizes most of the commonly used commands
## http://isites.harvard.edu/fs/docs/icb.topic239013.files/Using_R_and_Zelig/R-refcard.pdf

### Housekeeping

## Let's start and work through the example code.
## The pound signs on the left here are "commenting" code.
## All code that follows # on the same line is ignored.
## This leads to a first important lesseon: Use # intensively to comment your code!
## Spell out in comments what a particular line of code does and why.
## Commenting is good programming practice and will keep your
## code readable to yourself and others, including your graders!

## Let's begin and start R and input the following command
## at the R prompt*:

A <- 1

    # * Some of the many ways to do this:
    # 1) type it out at the prompt
    # 2) copy and paste into the prompt
    # (If within the built-in R editor:
    # 3) put your cursor at that line, go to the "Edit"
    #    drop-down menu and select "Run line or selection"
    # 4) put cursor at that line and press Ctrl + R

# Every programming language needs an assignment operator.
# <- is R's assignment operator. An assignment thus looks like:

#a <- b

# i.e., "put 'b' into object 'a'", or more precisely,
# "'a' will now refer to whatever is in 'b'."
# If "a" exists, current values are overwritten.
# If not, a new object "a" is created.

# Run these commands (you can run them all at once by
# highlighting them) and see if they make sense to you:
a <- 15
a
a <- a + 1
a

### Objects, Model, Types

# R is a (somewhat) object oriented programming language
# so: This essentially means three things.
# 1. Everything in R is an object.
# 2. Every object has a particular data type (vector, matrix, dataframe, list etc.).
# 3. Every object has a particular mode (numeric, character, logical, etc.).

# What does this mean?
# Well, above we definded object "a"
# Since there are no scalars in R, a is of data type "vector" which you can check by:
is.vector(a)

# So indeed, a is of type "vector". There are other data types such as matrices, lists, dataframes etc.
# but for now we will only deal with vectors. The vector is the most
# important data type in R. Vectors can be thought of as contiguous cells containing data.

# There are various ways to create vectors. For example:
a1 <- c(1,4,9,17)
a1
a2 <- 1:9
a2
a3 <- seq(.5, 1.8, by=.1)
a3
a4 <- rep("Hi", 8)
a4
a5 <- c(TRUE,FALSE,TRUE)
a5

## Now recall that idea about object oriented language. The last point was that:
## 3. Every object has a particular mode!!

## a1, a4, and a5 are all three objects of data type vector,
## but they have different modes (numeric, character, logical, etc).
## a1 for example contains numbers, so it is of mode numeric
mode(a1)
## a4 in contrast contains letters, so it is of mode character
mode(a4)
## a5 contains logical values TRUE (T) or FALSE (F), so it is of mode logical
mode(a5)

## Why are data types and modes important?
## One of the keys to object oriented programming is to understand
## that each R command or operation will only work with
## objects of a particular type. This sounds obvious, but it is a key concept!
## We say that commands (which are themselves objects) are associated with a particular
## type and mode.

## For example, let's look at R's summation command sum()
a1
sum(a1)
## The sum() command sums the elements of vector a1. This works because a1 is a numeric vector.
## sum() requires as input an object that is of mode numeric (or logical). This is why:
a4
sum(a4)
## Returns an error. sum() does not work with objects of mode character.
## If you get an error message in R, most of the time it will be because you are trying a command
## with an object of the wrong type. Here is anbother example:
 a1 + a4
## the "+" operator requires objects of mode numeric.
## So if you get an error in R, always think: What is the type of my object and is this a "legal"
## type for the particular operation I want to use on this object.

## Vector operations

# divide or divide by scalar
vec2 <- 1:9
vec2/2
vec2 * 2

# combine vectors to a matrix
mat1 <- cbind(vec2,vec2*2)
mat1
mat2 <- rbind(vec2,vec2*2)
mat2

# use data.frame() to combine vectors of different types
vec1 <- rep("Help",3)
vec1
vec2

# combine
mat1 <- data.frame(vec1,vec2)
mat1
# do not use cbind
mat2 <- cbind(vec1,vec2)
mat2

# data.frame also lets you name columns directly:
mat1 <- data.frame(Colnamed1 = vec1, Colnamed2 = vec2)
mat1

##  Matrix operations
mat1 <- matrix(c(1:4),2,2)
mat1
# element by element multiplication
mat1 * mat1
# matrix multiplication
mat1%*%mat1

### Help files

## Modes and types are also important because the are used to describe inputs and outputs from commands in the
## R help files. To learn about a command you don’t understand, type a question mark followed by the
# name of the command. For example, to see the help page describing the mean command type:
?mean

#If you don’t know the name of the command you are looking for, you’ll want to do a
#keyword search using help.search. For example, to find all commands that relate to
#taking the median, typing
help.search("median")

# definitly read A. Diamond's Guide to R help on the course website at:
# http://isites.harvard.edu/fs/docs/icb.topic239013.files/Using_R_and_Zelig/R-help.version1.pdf

### Commands and Libraries

# R commands are organized in libraries/packages.
# In R speak setx(Zelig) means that the function setx is in the Zelig package
# many commands aree in the base package that is automatically loaded at the start
# other libraries need to be loaded to use their commands
?setx
library(Zelig)
?setx

# to install a package use (your computer needs to be connected to the internet):
install.packages("car")

### Workspace
# all elements created in R are available in the workspace
# to see what is in the workspace use:
ls()
# to remove elements from the workspace use
rm(a1)
# remove all
rm(list=ls())

### Data Handling

## Ok, let's load some data to make this more fun. I have put a subset of the American National
# Election Survey on my website (it is also
# on the course website). You will need to use
# two files:

# -- The codebook, describing the data:
# http://people.fas.harvard.edu/~jhainm/nes.cbk

# -- And the data:
# http://people.fas.harvard.edu/~jhainm/nes.dat

# Have a look at both files. You should notice that
# the variables names are at the top of the data.

# Okay, now, let's read in the data to R.

nes <- read.table("http://people.fas.harvard.edu/~jhainm/nes.dat")

# So now you can see that data by simply
# typing "nes".
nes

# Note that R is not simply printing a copy
# of the table as it appeared on my website.
# Rather, the "read.table(fileName)" command
# does something a little like "Text to columns"
# in Excel -- it looks at a table and intelligently
# assigns the text to rows and columns.


## Indexing (Subsetting)

# Usually, we don't want to look at the whole dataset
# but only subsets. This leads to the important concept of indexing
# Objects are indexed by using object[]
# for one dimensional objects such as vectors we can simply use the element number
a1
# If we only want to look at the second element of the vector we do:
a1[2]

# Similarly, we use [rownumber,columnnumber] to index two dimensional data types.

# A dataframe has two dimensions: rows and columns. How many are there of each?
dim(nes)
# so we have 30 rows and 5 columns.

# to check the no of elements in a vectors use:
length(a1)
a1

# pick first element row, first column
nes[1,1]

# Pick first row (all columns):
nes[1,]

# pick first and third and  column, rows 2 to 10
nes[2:10,c(1,3)]

# This command tells R to show the first ten elemnts in the second column:
nes[1:10,2]

# subsetting for matrix objects works similarly

# Here are other useful commands to view subsets of the dataframe:
edit(nes)
# whole dataset in spreadsheet form
# like data editor in Stata

head(nes)
# first few rows

names(nes)
# column names

# get the col named "state"
nes$state

# equivalently:
nes[,1]  # first column.

#get first ten elements of that col
nes$state[1:10]

#-- rows of a dataframe
# which rows represent respondents from California?

# logical tests (== is equal to? != not equal to?)
nes$state == "California"
nes$state != "California"
nes$state == "California" | nes$state == "New York"
nes$state %in% c("California","New York", "Florida")

# list of indices
CArows <- which(nes$state == "California")
CArows

# let's see all data for those rows
# these three commands are equivalent:
nes[CArows, ]
nes[which(nes$state == "California"), ]
nes[nes$state == "California", ]

# let's do a cross-tab of one varibale
table(nes$state)

# cross-tab of two varaibles
table(nes$state,nes$gender)

# summarize the dataset
summary(nes)

# Here i s how to analyse subsets of the data:
mean(nes$envJobs)
# Do women or men care more about the environment in this sample?
mean(nes$envJobs[nes$gender =="Female"])
mean(nes$envJobs[nes$gender =="Male"])

# finally sort the dataset by gender and state
nes[order(nes$gender,nes$state),]

### Loops
vec2
for(i in vec2){
  print(vec2[i])
}

## another loop
# Usually, you want to store the output of a for loop in something
holder <- matrix(NA, nrow = 10, ncol = 1)
# this creates an empty matrix
holder

# Now we run a loop and store exp(i) in the rows of holder
for(i in 1:10){
  cat("i is now:",i," exp(i) is thus:",exp(i),"\n")
  holder[i,1] <- exp(i)
}
holder
summary(holder)

# avoid loops if you can use a matrix operation

#### FUNCTIONS

# Part A
# This is how we write functions

foo <- function(x) { # say what the function is used for
   # Write code inside function using x as an object within the function
  return(x) # Normally, a function does more than return its argument
}

# evaluate
foo(3) # Looks like pi

# anothe simply function
foo <- function(x){
 x <- x + 5
 return(x)
}

# evaluate
foo(3)

# more complex with multiple arguments
foo <- function(x, mu = 0, sigma2 = 1) { # returns the density of a normal distribution
  # Check to make sure the arguments are reasonable using the stopifnot() function
  stopifnot(is.numeric(x), is.numeric(mu), all(sigma2 >= 0))

  # Do whatever the function is designed to do
  density <- (1 / (sqrt(2 * pi * sigma2))) * exp(-((x - mu)^2) / (2 * sigma2))

  # Return whatever the function is designed to return
  return(density)
}

foo(x = 1 / 2 , mu = 2 , sigma2 = 3)
foo(x = 1 / 2 , mu = 0 , sigma2 = 3)

## Remember to work through some of the additional R ressources at home

## Quit R by typing q() at the R prompt, and I don't save my
## workspace.  I can replicate this session by opening R, opening my
## .R document, and ``cutting and pasting'' the commands or
## ``source()''-ing the .R file
