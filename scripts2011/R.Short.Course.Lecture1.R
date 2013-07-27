# ---------------------------------------------------------------------
# Program: TW-IntroToR-20100210.R
#  Author: Matt Keller (with help from Steve Boker)
#    Date: Jan 4 12:02:11 EST 2011
#
# WELCOME TO THE WONDERFUL WORLD OF R!
#
# This is an introductory script for learning the very basics
#   of R.  This header is a comment.  Any text after a hash mark (#)
#   is ignored by R until the next carriage return.  So, if you
#   put a hash mark at the beginning of a line, the whole line
#   is ignored.
#
# This script is formatted for a text window that is 72 columns wide.
#
# As we go through this file, we encourage you to write your
#   own comments so that you can come back to this file and
#   remind yourself of what was happening. 
#
# We encourage you to make these notes in all of the scripts
#   in the workshop.  It will thus be easier to come back later
#   and modify them for your own use.
#
# ---------------------------------------------------------------------
# Revision History
#    -- Wed Feb 10 12:02:11 EST 2010
#      Created TW-IntroToR-20100210.R.
# Note: I always put a revision history in all of my scripts!
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Variables  (I always have a section here describing my variables)
# ---------------------------------------------------------------------
# 
# ---------------------------------------------------------------------


# ---------------------------------------------------------------------
# PART 1: USING R AS A CALCULATOR
# ---------------------------------------------------------------------

# Arithmetic with two numbers
2+2

2-4

2*3

8/3

# If you're like me, you don't remember order of operations
9+12*3

# So use parentheses to be explicit about what you want!
(9+12)*3

# 3 squared
3^2

# 9 to the 1/2 power
9^.5

# That is the same as what is returned by the square root function
sqrt(9)

# A more complicated bit of algebra
(sqrt(9)-6^2)/4


# ---------------------------------------------------------------------
# PART 2: ASSIGNMENT AND OBJECT CREATION
# ---------------------------------------------------------------------

# Let's make a variable called Fred
Fred <- 2+2

# Congrats! You've just made your first variable.
# We say, "Fred gets two plus two". The arrow (less than, dash)
# "assigns" 2+2 to Fred. You'll use the "gets arrow" all the time!

# Type Fred on a line by itself and press return and you find out
# what is contained in Fred
Fred

# Caps matter. There is no object "fred". Your first error message!
fred

# Error messages are our friends :) They give us tough love.

# These are all equivalent
Fred + 3
4 + 3
Fred+3
4 +      3

# white space doesn't     matter     between      things, 
# but it does matter within t h i n g s
F r e d

# One variable can be assigned to another.
a <- Fred

# Now 'a' is a copy of Fred.
a

# We read the next line as "A gets square root of 'a' times five"
A <- sqrt(a)*5
A

# This one is a little tricky.  We can re-assign A.
A <- A*a
A

# We can use scientific notation
x <- 1e3
x

# Here, we create a vector of length 13; notice what ":" does
x2 <- 1:13
x2

# The "*" does element-wise multiplication.
x2*3

# Since there are fewer elements in "3" than in "x2", "3" is 
#   repeated: it is multiplied by each element in "x2".  
# This makes scalar multiplication easy.

# Vectors can also be created by the "c()" function.
# "c" stands for "concatenate".
myvec <- c(8,13,2,1,6)
myvec


# ---------------------------------------------------------------------
# PART 3: LOGICALS
# ---------------------------------------------------------------------


# Remember our old friend Fred?
# The variable Fred contains the numeric value 4
Fred

# Is Fred _really_ a numeric value?
is.numeric(Fred)

# Does Fred equal four?
Fred == 4

# Does Fred equal five?
Fred == 5

# Is Fred greater than 3?
Fred > 3

# Is Fred less than 3?
Fred < 3

# Is Fred between 3 and 10?
Fred > 3 & Fred < 10

# Is Fred either greater than 5 or equal to 2?
Fred > 5 | Fred == 2

# Which elements of x2 are greater than 5?
x2 > 5

# Let's check by printing out x2
x2



# ---------------------------------------------------------------------
# PART 4: FUNCTIONS
# ---------------------------------------------------------------------

# R is a very *functional* language. 
# Most of what you do in R is use functions.
# We've already seen a couple of functions. 
# Let's look more in depth:

# sqrt() is a "function" and the "argument" is 16.
sqrt(16)

# Functions take arguments, do something, and then return something.
# Every time you see a word followed by parentheses,
# you are seeing a function.

# help() is a function that prints the help page for a function
help(sqrt)

# This does the same thing as help(sqrt)
?sqrt

# If you don't know the name of a function, but you know what it
# does, you can search for it using help.search()
help.search("skewness")

# This is the formal way to use functions.
# We can explicitly name the argument x.
sqrt(x=16)

# Absolute value
abs(-7)

# The function c() [concatenate] makes vectors
c(3,-2,-3,6,1)

# We can use functions as arguments to other functions! Fun!
abs(c(3,-2,-3,6,1))

# Some functions require no input.
# This prints to screen every object we've created so far
ls()

# This function creates a vector of 50 pseudo random numbers 
# drawn from a normal distribution with mean = 0 and sd = 1
?rnorm
rnorm(n=50, mean=0, sd=1.0)
rnorm(50,0,1)  #same thing! (although the exact numbers will differ!)

# Create *a different* vector of 50 values using rnorm
dat <- rnorm(50, mean=0, sd=1.0)
dat

# How long is dat?
length(dat)

# Who dat?
is.vector(dat)

# What is the mean of dat? Is it exactly 0? Why not?
mean(dat)

# What is the median of dat?
median(dat)

# What is the variance of dat?
var(dat)

# What is the standard deviation of dat?
sd(dat)

# Or, you could calculate standard deviation from the variance.
sqrt(var(dat))

range(dat)

min(dat)

max(dat)

round(dat, 3)

# Now, let's create a matrix
MAT <- matrix(1:50, nrow=10, ncol=5)  

# The matrix function is
?matrix

# Create *a different* matrix
MAT2 <- matrix(1:50, nrow=10, ncol=5, byrow=TRUE) 

# The dimensions of MAT are 5 rows, 2 columns
dim(MAT)             

# We can also "bind" vectors together to make matrices.
vec1 <- 1:5
vec2 <- vec1 * 2
MAT3 <- cbind(vec1, vec2)
MAT3
# The dimensions of MAT3 are 5 rows, 2 columns

MAT4 <- rbind(vec1, vec2)
MAT4
# The dimensions of MAT4 are 2 rows, 5 columns



# ---------------------------------------------------------------------
# Part 5: SUBSETTING & INDEXING
# ---------------------------------------------------------------------

x2

# This selects the third element of x2 by using an index.
x2[3]

# Select elements 3 through 5.
x2[3:5]

# Select elements 1, 6, 9, & 2 in that order.
x2[c(1,6,9,2)]

myvec

# Select the 2nd & 3rd elements of myvec.
myvec[2:3]

# You can select using logical vectors.
myvec[c(TRUE,TRUE,FALSE,FALSE,TRUE)]

# Here is another way to do the same thing!
my.select <- c(TRUE,TRUE,FALSE,FALSE,TRUE)  
myvec[my.select]

# We call my.select a "selection vector".
# A selection vector is usually the same length as the target vector.

# Select only elements of myvec that are greater than 5
my.select2 <- myvec > 5
myvec[my.select2]

# To index matrices, rows go first, then a comma, then columns
MAT[1:3,1:2]

# Putting nothing before or after the comma means ALL rows or columns.
MAT[2:3,]
MAT[,1:2]

# You can reorder rows or columns 
MAT[c(3,5,1),1:2]

# If you select a single row or column, you obtain a VECTOR
# Vectors are not matrices: no dimensions, just length
MAT[,1]

# Which elements of the 2nd column of MAT are over 8?
my.select3 <- MAT[,2] > 16

# Select just the rows where column 2 is over 8
MAT[my.select3,]

# As Darwin would say, selection is a powerful mechanism!



# ---------------------------------------------------------------------
# PROBLEM SET 1
#   Put your work directly into this script, below the q's
# ---------------------------------------------------------------------
#
# a) Create a vector of 100 normally distributed random variables (mean = 
#    0 & sd = 1). Assign it to "Y"
#
# b) Create another vector, "Z", of 100 normally distributed random
#    numbers with mean = 100 and the sd = 15. 
#    HINT: See the help function if you get stuck!
#
# c) Create another variable, "Sum.dist", that is the sum of Y and Z
#
# d) Put the vector "Sum.dist" into a matrix with 20 rows and 
#    5 columns.
#    Do so such that the numbers are put in BY ROW. 
#    Call the matrix "My.Mat"
#
# e) Get a new matrix that only has the rows of My.Mat where the 
#    first column of My.Mat is less than 100. (Note: everyone's
#    matrix will be different, but should have ~ 10 rows).
# ---------------------------------------------------------------------



# ---------------------------------------------------------------------
# PART 6: READING AND WRITING DATA ON THE DISK
# ---------------------------------------------------------------------

# First, we need to set the Working Directory for R.
# You can either set the Working Directory in the GUI or in a script.
#
# The Working Directory is where R will look if you read a file.
# The Working Directory is where R will write if you write a file.
# The Working Directory is where R can "save its state" at the
#   end of a session.
# 
# I recommend you create a new folder for each of your projects.
# That way you can save everything about a project together.
# Also, that way you can "save state" of R and it only applies to
#   one project.


# To display the Working Directory
getwd()

# If we wanted to change Working Directory on a PC, 
# we'd use something like this.
setwd("C:/NoExist")
# Note that this is a forward slash and not a backslash.

# Or on a Mac or Linux machine we would use something like
setwd("~/NoExist")
# where the tilde stands for your home directory.

# For this workshop session, we will set the Working Directory:
setwd("C:/XXXXXXX/XXXXXXXX/XXXXXXXX")

# To list all files in your working directory
list.files()         

# IMPORTANT POINT:
#  Do you see the file named ExampleData1.csv? 
#  If not, please try again and then raise your hand if you are 
#  still having trouble finding ExampleData1.csv

# To see all the arguments for read.csv() and other functions that
#  read in tables of data:
?read.csv

# In this session we will use "csv" data files because they can
#  be easily saved from programs like SPSS, Excel, etc.  There
#  are lots of options for reading in data files.  For instance,
help.search("spss")

# This tells us that there is an R package called "foreign" that has
#  a function called read.spss().  

# We can load that package by using the following line 
require(foreign)

# We will use read.csv() for this workshop.  
#  The csv format is the most general format for data files I know.  
#  And you can even open it up in a text editor.

# Now we will read our first data file.
ExampleData1 <- read.csv(file="ExampleData2.csv", header=TRUE)

# We have created a "data.frame" called ExampleData1.
is.data.frame(ExampleData1)

# I always ask for a summary() of a dataframe after reading it in.
summary(ExampleData1)

# summary() is like a sausage grinder.
# You can put almost anything into summary() and sausage will come out.
summary(x2)
summary(rnorm(1000, mean=2, sd=1))
summary(MAT3)

# But summary() doesn't tell me all the descriptive statistics
#   I want to know about ExampleData1

# Let's load a package called  "psych" with functions useful
#    in psychological measurement.
require(psych)

# Now let's get some better descriptive statistics about ExampleData1
describe(ExampleData1)



# ---------------------------------------------------------------------
# PART 7: CORRELATIONS, LINEAR REGRESSION, AND SELECTION
# ---------------------------------------------------------------------

# These data are in a wide format.
# That is to say, there is one twin pair per line.
# But both MZ and DZ twins are in the same file.

# Let's calculate an overall correlation matrix.
cor(ExampleData1)

# Hmm... maybe we don't want to see the first and second columns
# So, I will just select the 3rd through 6th columns.
cor(ExampleData1[,3:6])

# And I usually like to print correlations to only 3 decimal places.
round(cor(ExampleData1[,3:6]), 3)  #notice: here's a function within a function

# I would like to select only the MZ twins and only the DZ twins.
theMZs <- ExampleData1$Zygosity=="MZ"
theDZs <- ExampleData1$Zygosity=="DZ"

# That was our first use of the "$" operator.
# ExampleData1$Zygosity extracts the vector named "Zygosity" from
#   the data.frame named "ExampleData1".

# Now let's print the correlation matrix for the MZ twins.
round(cor(ExampleData1[theMZs,3:6]), 3)

# And the correlation matrix for the DZ twins.
round(cor(ExampleData1[theDZs,3:6]), 3)

# Next, we will run a univariate regression on the whole data.frame.
# We will predict Y1 from X1.
lmOut1 <- lm(Y1~X1, data=ExampleData1)
summary(lmOut1)

# Finally, we predict Y1 from X1 separately for the MZ & DZ twin 1.
lmOut2 <- lm(Y1~X1, data=ExampleData1[theMZs,])
summary(lmOut2)

lmOut3 <- lm(Y1~X1, data=ExampleData1[theDZs,])
summary(lmOut3)

# This is FAR from a complete analysis of these data.
# In order to go further, we need to take into account the
# relationship between twin 1 and twin 2 in each pair.


# ---------------------------------------------------------------------
# PROBLEM SET 2
#   Put your work directly into this script, below the q's
# ---------------------------------------------------------------------
#
# a) Read in the data in ExampleData2.csv
#
# b) Select all rows with TwinID equal to 1 and Zygosity equal to "MZ"
#
# c) Calculate a correlation matrix for those twins.
#
# d) Now calculate a correlation matrix for TwinID equals 2 and
#    Zygosity equal to "MZ".
#
# e) Try running a linear model with Y being predicted by X for
#    only rows with TwinID 2 and Zygosity "DZ".
#
# ---------------------------------------------------------------------








