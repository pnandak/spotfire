#    ===Programming Basics===
 
#   ---OBJECTS & THEIR NAMES (1)---
#
# R is object oriented
#
# Everything: data, functions (procedures),
# models, etc. are objects
#
# Object names should begin with a letter and 
# can contain letters, numbers, underscores, “_”, 
# periods
#
# Case matters, e.g. myvar is not MyVar
#
# Some packages use uppercase letters 
# to differentiate e.g. “Save” from “save”

#   ---OBJECTS & THEIR NAMES (2)---
#
#
# Unlike SAS, the period in a name 
#   has no meaning, but we won’t use it
#
# As in any stat package, avoid creating names 
#   that may conflict, e.g. “mean” or “TRUE”.


# ---PROMPTS---
#
# ">" is the standard command prompt 
#
# "+" is the prompt when continuing on a new line
#
# If you see “+” prompt when not trying to 
# continue, look for a missing “)”
#
# To get rid of an unexpected “+” continuation 
# prompt
#   -Submit the missing piece of the command, or 
#   -Use Esc key (Windows) or CTRL-C on Mac or 
#    Linux



#   ---COMMANDS---
#
# Begin and end anywhere on a line
#
# Can contain additional blanks
#
# When continuing a command on a new line: 
# -Do not make the partial one executable 
# -Splitting after a comma works well
#
# R prompt is “>” or “+” for continuation
#
# End with ")" 
#   So if you see “+” prompt 
#   when not trying to continue, 
#   look for a missing “)”
#
# You can end a command with “;” but it's only 
# required for two commands on one line



#  ---SPACING EXAMPLE: THESE ARE EQUIVALENT---
setwd("myRworkshop")

mydata<-read.csv("mydata.csv")
summary(mydata)

mydata <- 
  read.csv ( "mydata.csv" )
summary (
  mydata
  )


#   ---MODIFYING DATA VS. ANALYZING IT---

# SAS: separate steps (data vs. proc)
#
# SPSS & Stata: separate statements 
#
# R: no separation
#
# This:
#
#     myLogX <- log(x)
#     lm( y ~ myLogX ) 
#
# is the same as this: 
#
#     lm( y ~ log(x) )

#   ---COMMANDS AND DATASETS---
#
# SAS, SPSS & Stata must analyze data
#   from a single data set
"
# R analyses can blend data sets
#   lm( myONE$y ~ myTWO$x )


#   ---SIMPLE CALCULATIONS---
#
# Enter any math to get answer
#
# The [1] is countign the output values

2+3

# ---CONTROLLING OUTPUT WIDTH---

# option() function controls settings
# Colon operator generates integers "from:to"
# index [numbers] count the values

1:40
options( width=20 )
1:40
options( width=50 )

# ---USING SYMBOLS---
#
# Assignment operator in SAS/SPSS/Stata is "="
#
# "<-" is the assignment operator
#
# "="  works, but it's best to avoid it

x <- 2
y <- 3
x+y
x*y

#    ---DATA STRUCTURES---

# SAS, SPSS, Stata have one main
# data structure: the dataset
#
# R is much more flexible with: 
#   -Vectors
#   -Factors 
#   -Matrices
#   -Arrays
#   -Data frames
#   -Lists

# ---VECTORS (1)---

# Our first variable
# "Which workshop did you take?"

workshop <- c(1,2,1,2,1,2,1,2)

# This c() function call combines 
# its values into a "vector".

# A vector is an object containing a single 
# set of values called its "elements"

# ---VECTORS (2)---
#
# Vectors are similar to variables, 
# but may exist outside a data set
#
# May also be a set of parameters to 
# control a procedure
#
# Operations on vectors 
#   -Happen to each element
#   -If one vector is shorter, 
#    its values are "recycled"
#    until the lengths match

workshop <- c(1,2,1,2,1,2,1,2)

print(workshop)
workshop

workshop+workshop # element by element
workshop + 10     # 10 is recycled
5*workshop

table(workshop)
summary(workshop) # oops!

# ---VECTOR ATTRIBUTES---
#
# SAS/SPSS/Stata say a variable’s "type" 
#   is numeric or character. 
#   R calls this the vector’s "mode".
#
# As in SAS/SPSS/Stata, even a single 
#   alphabetic character value will 
#   "coerce" this vector into character mode. 
#
# Unlike SAS/SPSS/Stata, vectors also have a "class" 
# of character or numeric. More later.
#
# The number of elements a vector contains, 
#   including missing values, is its "length".


# ---A CHARACTER VECTOR---
#
# A character vector
# "What is your gender?"
#
# Its character elements must be in quotes
#
# NA=Not Available, a missing value
#   Note the it is NOT in quotes, or 
#   it would be a valid value.

gender <- c("f","f","f",NA,"m","m","m","m")
print(gender)

table(gender)
summary(gender)


# ---MORE NUMERIC VECTORS---
#

q1 <- c(1,2,2,3,4,5,5,4)
q2 <- c(1,1,2,1,5,4,3,5)
q3 <- c(5,4,4,NA,2,5,4,5)
q4 <- c(1,1,3,3,4,5,4,5)

# ---R FUNCTIONS---

# What... 
#  SAS calls procedures and functions &
#  SPSS/Stata calls commands and functions &
#  ...R calls "functions", e.g. print(q1)

# When you use a function, you "call" it
#
# print() is the default function
#   so this function call:  print(q1) 
#   is usually the same as: q1
#
# Parameters/keywords are called "arguments"
#
# Arguments follow function name in parentheses
# ---FUNCTION OUTPUT---
#
#  Output is what the function call "returns"
#
#  Is sparse, e.g. counts without percents
#
#  Not in word processing tables, no tabs
#
#  Optimized for further analysis
#
#  Other functions can create neat tables 
#  to view in HTML, RTF, ODF, LaTeX 
#
#  If data contains NA (missing), output is often
#  NA unless you use an argument like 
#  na.rm=TRUE (more later)

# ---FACTORS (1) ---
#
# Categorical variables in R are called "factors"
#
# The mode of factors is ALWAYS numeric
#
# The class of factors is factor. So we begin to see 
# class is more specific than mode.



# ---FACTORS (2): STARTING WITH A VECTOR---

workshop <- c(1,2,1,2,1,2,1,2)
workshop
table(workshop)
mean(workshop)

# ---FACTORS (3) NOW AS A FACTOR---

# Categorical variables in R are called "factors"
# The mode of factors is ALWAYS numeric

workshop <- c(1,2,1,2,1,2,1,2)
workshop <- factor( workshop )
workshop

table(workshop)

mean(workshop) #generates warning now.


# ---FACTORS (4) ADDING VALUE LABELS---

workshop <- c(1,2,1,2,1,2,1,2)

workshop <- factor(
  workshop,
  levels=c(1,2),
  labels=c("R","SAS")
)

# ---FACTORS (5) ARGUMENTS---

# 1. The name of the vector to convert
#
# 2. Levels or values the data can have,
#    which need not be in the data.
#
# 3. Optionally, value labels for the levels. 
#    The labels match the levels in the order 
#    they both appear here, 
#    the order of the values in the data set 
#    is irrelevant. By default, R will use 
#    the values themselves as the labels.


# ---FACTORS (6) LABELED OUTPUT---

workshop
table(workshop)


# ---CHARACTER FACTORS (1) DEFAULT LABELS---

gender <- c("f","f","f",NA,"m","m","m","m")
gender <- factor(gender)
gender

# ---CHARACTER FACTORS (2) NICER LABELS---

gender <- c("f","f","f",NA,"m","m","m","m")
gender <- factor(
  gender,
  levels=c("m","f"),
  labels=c("Male","Female")
)

# ---CHARACTER FACTORS (3) LABEL IMPACT---

gender
table(gender)

# ---DATA FRAMES---

# Like a SAS/SPSS/Stata dataset: rectangular
#
# Variables are called variables, vectors, 
# columns, or "components" 
#
# Observations are called 
# rows, observations or cases
#
# It is a generalized matrix, one that can have 
# numeric and character mode vectors
#
# The mode of a data frame is list
#
# The class of a data frame is data.frame
# 
# Data frames are lists whose components must have 
# equal length (more soon)


# ---WHY DATA FRAMES?---
#
# You can do any analysis on vectors but...
#   -sorting vectors independently or...
#   -removing missing values independently...
#   -mixes what makes up a case. 
#    Whose data goes with who?
#    Covariance is destroyed!
#
# Data frames lock the values of cases 
# together for sorting, removing NAs, etc.

# ---OUR DATA SO FAR---

workshop <- c(1,2,1,2,1,2,1,2)
workshop <- factor(workshop,
   levels = c(1,2),
   labels = c("R","SAS") )
gender <- c("f","f","f",NA,"m","m","m","m")
gender <- factor(gender)
q1 <- c(1,2,2,3,4,5,5,4)
q2 <- c(1,1,2,1,5,4,3,5)
q3 <- c(5,4,4,NA,2,5,4,5)
q4 <- c(1,1,3,3,4,5,4,5)

# ---CREATING A DATA FRAME---

mydata <- data.frame(workshop,gender,q1,q2,q3,q4)
mydata

# ---DATA FRAME DETAILS---
#
# The data.frame() function will convert all character
# data to factors (even addresses!) You can block that
# with the argument "stringsAsFactors=FALSE".
#
# Variable names AND row names 
# are stored as "attributes"

names(mydata)
rownames(mydata)

# ---MATRICES---

# Like a dataset but all variables 
# must be of the same mode, 
# e.g. all numeric, or all character
#
# The class of a matrix is "matrix"
#
# More efficient for some types of analyses
#
# Actually one long vector stored with a 
# "dim" DIMension attribute. You can see or 
# change it using the dim() function. 

# ---CREATING A MATRIX FROM VECTORS---
#
# cbind() binds columns together

mymatrix <- cbind(q1, q2, q3, q4)
mymatrix
dim(mymatrix)

# ---CREATING A MATRIX FROM matrix()---

# Left as a comment so we keep
# version with names q1, q2...
#
# mymatrix <- matrix(
# c(1, 1, 5, 1,
#   2, 1, 4, 1,
#   2, 2, 4, 3,
#   3, 1, NA,3,
#   4, 5, 2, 4,
#   5, 4, 5, 5,
#   5, 3, 4, 4,
#   4, 5, 5, 5),
# nrow=8, ncol=4, byrow=TRUE)
# mymatrix

# ---CALLING FUNCTIONS ON MATRICES---

table(mymatrix)

mean(mymatrix, na.rm=TRUE)

cor(mymatrix, use="pairwise")


# ---ARRAYS---

# An array is a matrix that may have 
# more than two dimensions
# 
# In three dimensions, they are like matrices 
# stacked as pages in a book.
#
# Vectors and matrices are simple arrays
# Beyond our scope 

# ---LISTS---

# An object that can store any other type of 
#   objects, called its "components"
#
# Output from functions is often in list form, e.g. 
#   regression might have a vector of regression 
#   weights, a table of anova results, etc.
#
# Lists print "sideways" to make room for 
#   different types of components
#
# A list's mode is "list"
#
# A list's class can be "list", "lm",...
#
# Can contain sets of function arguments


# ---CREATING A LIST---

mylist <- list(workshop, gender,
  q1, q2, q3, q4, mymatrix)
mylist

# ---WHAT DO FUNCTIONS DO WITH A LIST?---

# Many functions don't work on a list that
# is not also a data frame...
#
# They must be "applied" (discussed later)

table(mylist)
Error: all arguments must have the same length

mean(mylist)
Warning: ...not numeric or logical: returning NA

# ---CREATING A LIST WITH NAMES---

mylist <- list(
  workshop=workshop,
  sex=gender, #note "new=old" form
  q1=q1,
  q2=q2,
  q3=q3,
  q4=q4,
  mymatrix=mymatrix
)
mylist


# ============TABLE OF MODES AND CLASSES=============
# OBJECT         MODE               CLASS
# ---------------------------------------------------
# Vector         numeric/character  numeric/character
# Factor         numeric            factor
# Matrix         numeric/character  matrix
# Data Frame     list               data.frame
# List           list               list
# List from lm() list               lm
# Table          numeric            table
# ----------------------------------------------------


# ===MANAGING YOUR FILES & WORKSPACE===

# ---INTRODUCTION---
#
# SAS, SPSS & Stata depend on
# operating system commands
# 
# SAS has a few seldom-used file 
# management procedures
#   PROC DATASETS
#   PROC CATALOG
#
# R is a complete environment, including 
# many operating system-like commands

# ---LISTING OBJECTS IN YOUR WORKSPACE---
#
# ls or object function 
# lists the objects in your workspace
#
# Does not list vectors within a data frame
#
# Can use pattern argument to search with 
# regular expressions

ls()
objects() #same as ls()
ls( pattern="q" )

# ---PRINTING OBJECTS---

print(mydata)
# or
mydata

# ---LISTING COMPONENT NAMES---

names(mydata)

# ---EXAMINING OBJECT strUCTURE---

str(mydata)

# ---EVEN FUNCTIONS HAVE STRUCTURE---

str(lm)

# ---DELETING OBJECTS---

# The rm() function deletes objects
#
# It accepts multiple objects without combining
# them with c(x,y), a rare ability in R
#
# If you create vectors, 
# then store them in a data frame, 
# delete the vectors to avoid confusion

# ---REMOVE THE Q VARIABLES---

	rm(q1,q2,q3,q4)

# or

myQs <- ls(pattern="q")
rm( list=myQs )

# ---REMOVE ALL OBJECTS---

rm( list=ls() )

ls()

# ---YOUR WORKING DIRECTORY (WD)---
#
# To find out where your files will go, enter:

getwd()

# You can change it with:

setwd("myRfolder") #or

setwd("C:/Users/Bob/Documents/myRfolder")


# ---SAVING YOUR WORK---

# One object at a time

save(mydata,mylist,mymatrix,
     file="myWorkspace.RData")

# Or save everything in workspace

save.image(file="myWorkspace.RData")


# ---QUITTING AND RESTARTING R---
#
# To quit R:

quit() #or just q()

# Later, after restarting R, 
# load data into your workspace with:

load("myWorkspace.RData")
ls()

# ---R "HELPS" AUTOMATE SAVING---
#
# When you quit R, it asks,
# "Save workspace image?"
#
# If you say yes, it will create ".RDdata" file 
#   in your working directory
#
# When you start R, it will load it automatically 
#   from the folder in which you start R.
#
# This is a major source of errors
# Do you want all your datasets named “.RData”?


# ---FUNCTIONS THAT EXAMINE OBJECTS---

FUNCTION           DISPLAYS
-------------      -------- 
print(mydata)      Displays all or some of an object, depending on its class 
head(mydata)       Prints the top of an object 
tail(mydata)       Prints the bottom of an object 
names(mydata)      Lists column names, if any 
rownames(mydata)   Lists row names, if any 
class(mydata)      Tells you its class (data frame, list…) 
attributes(mydata) Tells names, class, rownames 
mode(mydata)       Tells you its mode (character, numeric, logical…) 
str(mydata)        Displays object structure 
ls.str()           Displays structure of all objects in workspace 


# ========CONTROLLING R FUNCTIONS======== 

# ---CONTROLLING FUNCTIONS---

# SAS/SPSS/Stata use procedures/commands/functions
#   ( e.g. GLM), & sub-statements (CLASS).  
#   R has only functions
#
# When you use an R function, you "call" it
#
# Each statement has options/parameters/keywords 
# to control them. R calls these "arguments"
#
# Arguments follow function name in parentheses
# 
# Arguments are usually single objects: a numeric or 
# logical (TRUE/FALSE) value, a vector, a data frame
#
#
# SAS/SPSS/Stata modeling statements have formulas. 
#   R has these too
#
# R has a few more ways to control functions...


# ---CONTROLLING FUNCTIONS WITH ARGUMENTS---

help(mean)


# ---MEAN FUNCTION DEFAULT ARGUMENTS---

# X is a numeric data frame, vector...
#
# trim=0 means it will remove none of the 
# data unless you change that. To remove the 
# most extreme 5% of the data before getting 
# mean, use trim=0.05
#
# "na.rm=FALSE" means that missing values 
# (NA) will not be removed, resulting in 
# NA unless you use "na.rm=TRUE"
#
# "..." means any other arguments will be passed on


# ---ARGUMENT NAME OR POSITION?---

# You can name all the arguments
mean(x=q3, trim=.05, na.rm=TRUE)

# When named, position does not matter
mean(na.rm=TRUE, x=q3, trim=.05)

# In position, you can skip names
mean(q3, .05, TRUE)

# Most popular: name all but first
mean(q3, t=.05, na.rm=TRUE)

# Arguments and their values can be abbreviated
# but it is a very bad idea!

mean( q3, na=T )


# ---Warning!---
# What does this mean?

mean( q1, q2, q3 )

# These are un-named arguments, it’s saying:

mean( x=q1, trim=q3, na.rm=q3)

# Correct form is:

mean( data.frame(q1,q2,q3) ) # or:
mean(   list(q1,q2,q3)     )

# ---CONTROLLING FUNCTIONS WITH FORMULAS---
# (See slide #67 for table of common formulas)

# Modeling functions allow formulas as their 
# first argument

t.test(q1 ~ gender, data=mydata)

# But "data=" refers only to the 
# formula! So here, R ignores that argument:

t.test( q1[ (gender=="f") ],
        q1[ (gender=="m") ],
        data=mydata) # "data=" ignored!

# ---TABLE OF COMMON FORMULAS---

Simple Regression       y ~ x
Multiple Regression     y~. (uses all other vars as predictors)
Multiple Regression 
  with Interaction      y ~ x1 + x2 + x1:x2  or  y ~ x1*x2
Regression 
  without Intercept     y ~ -1 + x
One-way ANOVA           y ~ a
Two-way ANOVA 
  with interaction      y ~ a + b + a:b  or  y ~ a*b
Analysis of Covariance  y ~ a + x 
ANOVA with b 
  nested within a       y ~ b %in% A  or  y ~ a/b



# ---CONTROLLING FUNCTIONS WITH AN OBJECT'S CLASS---

# Each object has a class attribute
#
# Many R functions create the best output for each 
# type of variable. More formally, “generic functions 
# offer different methods (or output)  for different 
# classes of objects”
#
# The class() function tells you an object’s class
#
# methods(myFunction) tells you which methods 
#   a generic function has


# ---WORKSHOP WITH CLASS=NUMERIC---

workshop <- c( 1,2,1,2,1,2,1,2 )

class(workshop)

summary(workshop)


# ---WORKSHOP WITH CLASS=FACTOR---

workshop <- factor(workshop,
   levels=c(1,2),
   labels=c("R","SAS") )

class(workshop)

summary(workshop)


# ---METHODS FOR SUMMARY FUNCTION---

methods(summary)


# ---CONTROLLING FUNCTIONS WITH EXTRACTOR FUNCTIONS---
#
# SAS/SPSS: all output seen at once
#   ODS/OMS rarely used
#
# R (like Stata): write a little, see a little
# R calls these extractor functions

lm( q4 ~ q1+q2+q3, data=mydata )

myModel <- lm( q4 ~ q1+q2+q3, data=mydata )

summary(myModel)

anova(myModel)

plot(myModel)

predict( myModel, myNewData)


# ---HOW MUCH OUTPUT IS THERE?---
#
# Generic functions change their behavior 
# based on an object’s class
# 
# print() shows what its designer told it to 
# for that class of object!

print(myModel)

mode(myModel)
 
class(myModel)
 
names(myModel)

myModel$coefficients

# To print EVERYTHING, unclass() it:

print( unclass(myModel) )



# ===WRITING FUNCTIONS===

# ---WRITING FUNCTIONS---
#
# SAS/SPSS/Stata macros...
#   -Are used for simplicity, repeatability
#   -Are written in the macro language
#   -Look nothing like other procedures
#   -Are optional complexity that beginners avoid
#
# R functions...(like Stata ado files)
#   -Are used also for simplicity, repeatability
#   -Are written in R itself
#   -Look exactly like built-in procedures
#   -Required because arguments need single objects

# ---FUNCTION RULES---
#
# Write a function using the form
#   myFun<- function(x) {other 
#    functions}
#
# Functions "return" only a single object,
# the last one it creates
#
# That object can contain many results in 
# vector, matrix, data.frame or list form
#
# To see what any function contains, type its 
# name without the following parentheses

# ---A FUNCTION WITH VECTOR OUTPUT---

myvar <- c(1,2,3,4,5)

mystats <- function(x)
{
  mymean <- mean(x, na.rm=TRUE)
  mysd <- sd(x, na.rm=TRUE)
  c( mean=mymean, sd=mysd )
}
mystats(myvar)
myVector <- mystats(myvar)
myVector

# ---A FUNCTION WITH LIST OUTPUT---

mystats <- function(x)
{
  myinput <- x
  mymean <- mean(x, na.rm=TRUE)
  mysd <- sd(x, na.rm=TRUE)
  list(data=myinput, mean=mymean, sd=mysd)
}

mystats(myvar)
myStatlist <- mystats(myvar)
myStatlist

# ---SEE THE FUNCTION ITSELF---

mystats

# ===COMMENTS & LABELS===

# ---DOCUMENTING YOUR PROGRAMS---

# Comments go from "#" to the end of the line

# This comment is on its own line, between functions.

workshop <- c(1,2,1,2, #This comment is within the arguments.

1,2,1,2) #And this is at the end.


# R lacks comments like /*…*/ in SAS/SPSS/Stata
#
# Use a good editor to add/remove blocks of #'s


# ---Variable Labels---
#
# Not an official part of R
#
# Frank Harrell's Hmisc package adds them 
# using a label attribute
#
# Martin Elff's memisc package does too 
# but it's beyond our scope
#
# The labels they add are recognized only 
# by their own functions!

library("Hmisc")

label(mydata$q1) <- 
  "The instructor was well prepared."
label(mydata$q2) <- 
  "The instructor communicated well."
label(mydata$q3) <- 
  "The course materials were helpful."
label(mydata$q4) <- 
  "Overall, I found this workshop useful."

describe( mydata[ ,3:6] )
