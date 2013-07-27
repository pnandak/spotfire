# Gov 2000
# Section I

# INTRODUCTION TO R
# Jens Hainmueller
# Some parts are based on a handout by Andy Eggers
# September 2007

## The goal for today is to introduce you to some useful concepts and commands in R.
## R is a free software environment for statistical computing and graphics that we will use for the class.
## Today we will examine just a small number of important concepts such as the idea of data types and modes,
## dataset handling, and a bit of controls structures such as loops.
## Needless to say, we will only be able to cover a very limited set of commands today and
## we strongly recommend you to work through the other R resources listed on the course website.
## In particular, Maindonald Guide to R, Chris Green's R Primer, Alexis Diamond's guide to R help,
## and the R reference card. Also please take a look at the R website at http://www.r-project.org/ which has
## the details on how to install R, who wrote R, etc. Chris Green's R primer also contains installation instructions.


## Let's start and work through the example code.
## The pound signs on the left here are
## "commenting" code.  
## All code that follows # on the same line is ignored.
## This enables you to write plain text without encountering syntax
## errors.

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


## Ok, let's load some data to make this more fun.

# I have put a subset of the American National
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

# Usually, we don't want to look at the whole dataset
# but only subsets. This leads to the important concept of indexing
# Objects are indexed by using object[]
# So recall our vector a1
a1
# If we only want to look at the second element of the vector we do:
a1[2]

# Similarly, we use [rownumber,columnnumber] to index two dimensional data types.
# As you have seen, the dataset nes has several rows and many columns and thus two dimensions.
# It is of data type dataframe.
is.data.frame(nes)

# A dataframe has two dimensions: rows and columns. How many are there of each?
dim(nes)
# so we have 30 rows and 5 columns.

# Naturally, this command tells R to show the first row of the object nes:
nes[1,]

# (you should see:
#       state votePres income gender envJobs
#  1 Illinois    Kerry  50000   Male       2

# This command tells R to show the first ten
# items in the second column:
nes[1:10,2]

#  [1] Illinois   California Tennessee  Louisiana  Washington California
# [7] Wisconsin  Alabama    New Jersey Colorado

# Here are other useful commands to view subsets of the dataframe:
edit(nes)
# whole dataset in spreadsheet form
# like data editor in Stata

head(nes)
# first few rows

names(nes)
# column names
# a very generally useful command.

#--  columns of a dataframe
nes$state

# equivalently:
nes[,1]  # first column.
nes$state[1:10]

#-- rows of a dataframe
# which rows represent respondents from California?

# logical test
nes$state == "California"
nes$state != "California"
nes$state == "California" | nes$state == "New York"
nes$state %in% c("California","New York", "Florida")

# all these TRUE and FALSES may not seem useful, but
# they are useful when used to index subsets of the data:

# list of indices
CArows <- which(nes$state == "California")
CArows

length(CArows) # what question does this answer?

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

# Here is how to analyse subsets of the data:
mean(nes$envJobs)
# Do women or men care more about the environment in this sample?
mean(nes$envJobs[nes$gender =="Female"])
mean(nes$envJobs[nes$gender =="Male"])

# finally sort the dataset by gender and state
nes[order(nes$gender,nes$state),]

## Before we close for today, let's quickly look at a few other topics

## vector operations
vec2 <- 1:9
vec2/2

## Much bulkier, but to illustrate a loop:
for(i in vec2){
  print(vec2[i]/2)
}

## Now, embed this in a function so I can redefine vec2 later.
my.halver <- function(x){
  for(i in 1:length(x)){
    print(x[i]/2)
  }
}

## Run my function
my.halver(2:20)

## Reminder:  much better is
2:20/2  ## Use vector operations instead of loops when you can.

## Quit R by typing q() at the R prompt, and I don't save my
## workspace.  I can replicate this session by opening R, opening my
## .R document, and ``cutting and pasting'' the commands or
## ``source()''-ing the .R file
