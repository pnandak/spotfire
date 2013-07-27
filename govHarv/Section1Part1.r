### Gov 2001
### Section 1




### INTRODUCTION TO R
### Miya Woolfalk (Adopted from Jens Hainmueller)
### 2/5/08

# The goal for today is to introduce you to some of the most basic concepts and commands in R.
# R is a free software environment for statistical computing and graphics that we will use for the class.
# We will only be able to cover a very limited set of commands today and
# we STRONGLY recommend you to work through the other R resources listed on the 
# course website (http://isites.harvard.edu/k48701)under "Computing Documentation/Using R and Zelig".


##### This first part (first 20 min or so) will involve some basic stuff in R
##### If you tune out, be sure to tune back in for the last half (last 20 min or so)





### I. Installing R

# http://www.r-project.org/
# under "download", click "CRAN", select a mirror, choose a platform
# for windows users, select the "base" package, download and run the exe file






### II. Basics


## R Console: where you execute/run commands/programs


## R Editor: where you edit programs; to open: "File" -> "New Script"


## Commenting:

# The pound signs on the left are "commenting" code.
# All code that follows # on the same line is ignored.
# Use # intensively to comment your code!
# Spell out in comments what a particular line of code does and why.
# Commenting is good programming practice,
# making your code readable to yourself and others!


## Entering Commands: a few options

A <- 1

# 1) type it out at the prompt (">" in console)
# 2) copy and paste into the prompt
# 3) (within R Editor) put cursor at that line (or select code), 
	# 3a) "Edit" -> "Run line or selection"
	# 3b) left click -> "Run line or selection"
	# 3c) Ctrl + R


## Assignment operator

#Every programming language needs an assignment operator.
# <- is R's assignment operator. An assignment looks like:

# a <- b	# "put 'b' into object 'a'", or more precisely,
		# "'a' will now refer to whatever is in 'b'."
		# If "a" exists, current values are overwritten.
		# If not, a new object "a" is created.

# Run these commands and see if they make sense to you:
a <- 15
a
a <- a + 1
a


## Objects: Classes and Types

# R is a (somewhat) object oriented programming language
# This essentially means three things.
# 1. Everything in R is an object.
# 2. Every object has a particular data class (vector, matrix, dataframe, list etc.).
# 3. Every object has a particular type (numeric, character, logical, etc.).

# What does this mean?
# Above we definded object "a"
# Since there are no scalars in R, a is of data class "vector"
# You can check this by

is.vector(a)

# There are other data classes such as matrices, lists, dataframes etc.
# but for now we will only deal with vectors. 
# Vectors can be thought of as contiguous cells containing data.

# Creating Vectors: a few ways
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

# Recall: 3. Every object has a particular type
# a1, a2, a3, a4, and a5 are all objects of data class vector
# but they have different types (numeric, character, logical, etc).

# a1 contains numbers; it is of type numeric
mode(a1)
# a4 contains letters; it is of type character
mode(a4)
# a5 contains logical values TRUE or FALSE; it is of type logical
mode(a5)

# Why are data classes and types important?
# One of the keys to object oriented programming is to understand that 
# each R command or operation will only work with objects of a particular type. 
# This sounds obvious, but it is a key concept!
# We say that commands (which are themselves objects) are associated with a particular
# class and type.

# Example: R's summation command sum()
a1
sum(a1)	# sums the elements of vector a1. 
		# works because a1 is a numeric vector.
		# sum() requires as input an object of type numeric (or logical) 
a4
sum(a4)	# returns an error. 
		# sum() does not work with objects of type character.

# Note: Error messages in R: 
# a lot of the time they occur because you are trying a command
# with an object of the wrong class or type.  If you get an error in R, always first think about 
# the class or type of your objects and whether the operation is "legal" given your object class/type.








### III. Vector and Matrix operations


## Vectors: division and multiplication
vec2 <- 1:9
vec2/2
vec2 * 2


## Combine vectors to form a matrix

# vectors of same type; use cbind() or rbind()
mat1 <- cbind(vec2,vec2*2)	# "column bind" or "bind as columns"
mat1
mat2 <- rbind(vec2,vec2*2)	# "row bind" or "bind as rows"
mat2


##  Matrix operations

# creating a matrix from scratch
mat1 <- matrix(c(1:4),2,2)	# matrix(data, nrow, ncol)
mat1

# element by element (cell by cell) multiplication
mat1 * mat1

# matrix multiplication
mat1%*%mat1

# transposing a matrix 
t(mat1)

# inverting a matrix
solve(mat1)

# assigning column and row names
mat1
colnames(mat1) <- c("col1", "col2")
rownames(mat1) <- c("row1", "row2")
mat1









### IV. The Workspace and the Working Directory


## The Workspace
# All elements/objects created in R are available in the workspace

# see what is in the workspace 
ls()

# remove elements from the workspace 
rm(a1)

# remove all elements
rm(list=ls())
ls()


## The Working Directory
# This is the file path that R uses to load and save objects in the workspace

# to see the current working directory
getwd()

# to change the current working directory
setwd(dir="C:\\Documents and Settings\\Miya Woolfalk\\My Documents\\files\\harvard\\TF2001\\Section\\Section1")
getwd()

# Setting the working directory is extremely useful when
# you need to load objects from similar file locations
# and/or save objects/images in your workspace to specific locations.
# For example, say we have an object that we want to save as a data file in the working directory. 
# We can simply do the following without having to type out the full file path name:

A <- 1:9
save(A, file="A.RData")

rm(list=ls())
ls()








### V. Data Handling


## Loading Data

# How you load data into R depends on the format of your data

# Loading R objects
load(file="PS1_Data.RData") # Note: this is the data for PS1 availble on the course website.

# Data in table format (e.g. dat, csv, txt files)
# you can use read.table(), read.csv(), etc. functions

# Data in other formats, including S, SAS, SPSS, Stata, etc.
# you can use the read.S(), read.ssd(), read.xport(), read.spss(), read.dta(), etc. 
# These are functions in the "foreign" library/package.
# More on libraries and packages later.

# For information on how to use these commands, 
# see the section notes on "R help".
# These notes will help you to learn how to teach yourself 
# how to use new commands in R.


## Viewing data: Basics

# Again, to see what objects were loaded
ls()

# We see that the data loaded only contained one object called "data"

# View all of the data simply type "data" (the name of the object)
data

# View only the first/last few row of the data
head(data)
tail(data)

# View a summary of the data
summary(data)

# Viewing subsets of the data
# Objects are indexed in R using square brackets: e.g. objectname[]
a <- c(1,4,5,7,8) 	# For one dimensional objects, we simply use the element number
a[4]				# the fourth element in a
data[995,1]			# For two dimensional objects, we specify [rownumber,columnumber]
data[995,]			# view the 995 row (all columns)
data[995:998,c(1,2,4)]	# view the 995-998 rows and 1, 2 and 4 columns

# View dimensions of the data 
length(a)	# For one dimensional objects
dim(data)	# For two dimensional objects

# View the data in spreadsheet form
edit(data)

# Viewing data using column and row names
# Frequently, the best way to subset your data is using the column and row names.
# This way, if you add columns or rows to your data (e.g. new variables or observations), 
# your indexing will still correspond to the columns or rows you want.
# You index using column and row names like this:
colnames(data)	# to view column names
data$y		# to view the column named "y" (not for vectors or matrices!)
data[,"y"]		# to view the column named "y" 

# Viewing a cross-tab of one column/variable 
table(data$y)

# Viewing a cross-tab of two variables
table(data$y, data$x1)








### VI. Logical Tests

# Sometimes we are interested in data with only certain types of characteristics.

# For example, suppose we wanted to know which rows have negative y values
# And only wanted to see the data in those rows
data$y < 0
neg.y <- which(data$y < 0)	#to return the row numbers/indices
data[neg.y,] # or more efficiently: data[data$y<0,]

# Next, say we want to know what the mean of the negative y values is
mean(data$y[data$y<0])

# Other logical tests
# == is equal to
# != is not equal to
# & and
# | or
# <= less than or equal to
# >= greater than or equal to








### VII. Loops, Ifelse, Apply


## for loops
vec1 <- seq(0,5,.1)		# make a vector
for(i in 1:length(vec1)){	# for i in 1 to the length of vec1 (51)
					# or for each element in vec 1
  print(vec1[i])			# print the i-th element in vec1
}


## if
x <- rep(NA,length(vec1))
for (i in 1:length(vec1)) {
	if(vec1[i] < 2) x[i] <- 0
}
x


## if; else
x <- rep(NA,length(vec1))
for (i in 1:length(vec1)) {
	if(vec1[i] < 2) x[i] <- 0
	else x[i] <- 1
}
x


## ifelse
x <- rep(NA,length(vec1))
for (i in 1:length(vec1)) {
	ifelse(test=vec1[i] < 2, yes=x[i] <- 0, no=x[i] <- 1)
}
x


## apply
# applies a function to margins (e.g. columns or rows) of a matrix, dataframe, array, etc

# for example, say we want to get the 5% and 95% of the y, x1, x2 and x3 variables in "data"

# long way #1:
quantile(data$y, probs=c(0.05,0.95))
quantile(data$x1, probs=c(0.05,0.95))
# and so on...

# long way #2: with for loops
holder <- matrix(NA,2,4)
for (i in 1:ncol(data)){
	holder[,i] <- quantile(data[,i], probs=c(0.05,0.95))
}
holder

# the fast way
apply(X=data, MARGIN=2, FUN=quantile, probs=c(0.05,0.95)) # margin = rows or cols

# Note: there are many cases in which using apply() is more efficient than writing a for loop





### VIII. Commands, Libraries/Packages

# R commands are organized in libraries/packages.
# Many of the basic R commands come in the base()package.
# This is automatically loaded at the start of your R session.
# Other commands (via their library/package) need to be loaded 
# (and sometimes even installed) before they can be used in R.
# To install a package (your computer needs to be connected to the internet):
# install.packages("MASS") 	# select a mirror
library(MASS)

# In R speak mvrnorm(MASS) means that the function mvrnorm() is in the MASS package 
?mvrnorm	# This pulls up the help file on mean().  More on help in R later.






### IX. Quitting R

## Quit R by typing q() at the R prompt.
## If you are using an editor and have saved your code, 
## there is little need to save your workspace (unless you care about specific objects)
## You can always replicate your session by opening R, 
## opening your code in the editor and running the commands.

















