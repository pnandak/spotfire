# Section 1, Sept 3, 2008
# Introduction to R


#
## Preamble: The Script and the Console
#

# R works by typing code in the console (next to the red 'greater than'
# sign) or by typing on a script (like this one) and sending the lines of
# code to the console.

# Every line that begins with '#' is ignored by the console.
# So you can add comments to your code without generating syntax errors.


2 + 18


50821/6


21^4


# store an object for later retrieval


a <- 2 + 18
a


b <- 50821/6


# use the ls() command to see what objects are currently stored 
# in the R environment           # rm(list = ls())


ls()


#
## Section 1: Introductions (Make R Interact with Your Computer, the Web,...) 
#

# your working directory: the "folder" where R loads and saves data


getwd()


setwd("C:/Documents and Settings/Instructor/My Documents/Gov 2000") 


# R can load all kinds of data easily


# from my computer, saved in a previous R sessions as "*.RData"


load("C:/Documents and Settings/Instructor/My Documents/Gov 2000/sec1data.RData")
	# why is most of this information redundant?


load("sec1data.RData")   


ls()


nes


# from my computer, saved as a text document "*.txt"


nesTEXT <- read.table("nes.txt")


# from someone else's website, saved as a text document


nesWEB <- read.table("http://www.people.fas.harvard.edu/~blackwel/nes.dat")


# from the course website, saved as a comma-separated document "*.csv"


pakistan <- read.csv("http://isites.harvard.edu/fs/docs/icb.topic636824.files/Section%201/pakistan.csv")


# R likes to get more packages, collections of functions 
# and data which add to its functionality 


install.packages("foreign")
	# this must be done only once 


library(foreign)
     # this must be done every time you use the foreign library


# the foreign library permits you to load data formatted for 
# a variety of other statistical packages, e.g. Stata's .dta format


#
## Section 2: Objects: R's Nouns
#

# as noted above objects are defined using <- 
# these constructions are R's nominative sentences


scalar1 <- 2


R <- "fun"


truth.vec <- TRUE


# of course we would like to have longer objects, like vectors
# which we create with c(), for concatenate 


vec1 <- c(2,2,7,-1,4)


R <- c("fun", "compelling")


truth.vec <- c(TRUE, FALSE, FALSE, TRUE)


# R performs math on numeric operations


vec2 <- c(2,5,1,3,2)


vec1 + vec2


vec1 - vec2


3*vec2


vec2*3


# Tricks for creating vectors


vec3 <- 1:5


vec3 <- c(1:5, 7, 11)


vec4 <- c(vec1, vec2)


# Subsetting (use [] to pick out elements of an object)
# recall: vec1 is c(2,2,7,-1,4); vec4 is c(2,2,7,-1,4, 2,5,1,3,2)


vec1[1]


vec1[6]


vec1[-1]
	

vec1-1 # Warning!


vec4[c(5,7)]


vec4[5,7] # Warning!


vec4[c(5:7)]


vec4[5:7]


# You can also replace a particular element of a vector


vec1[3] <- 6


# finally you might wish to remind yourself what type of objects 
# you're dealing with...
# basic data types for vectors: numeric, character, logical

mode(vec1)
mode(R)
mode(truth.vec)


# object classes: vector, (factor), matrix, array, list, dataframe
class(a)  # returns the data type for vectors
class(R)
class(nes)


# ...and see what is contained within more complex object types
attributes(nes)


#
## Section 3: Functions: The Verbs of R
#

# R has many preprogrammed functions that manipulate objects.
# To use a function, you type the function name followed by the
# arguments in parentheses


a <- c(1,3,6,5,9,22)


b <- c(4,5,6,5,2,1)


sum(a)


sum(b)


sum(a,b)


mean(a)


length(a)


sort(a)


c <- c(4,5,6,5,2,1)


all.equal(b,c)


all.equal(a,b)


## you can store the output of a function, as a new object.


output <- length(a)


## These functions are useful for creating vectors


seq(from = 0, to = 5, by = .5) 


rep(10, 27)


## Modifying your functions with extra arguments (adverbs!)
bad.vec <- c(2, 7, -3, NA, 5, -5)
	# NA is R's symbol for data that is missing 


mean(bad.vec)


mean(bad.vec, na.rm = TRUE)


#
## Section 4: Matrices in R, a More Complex Object (Compound Nouns?)
#

# the matrix() function in R is one way to create a matrix

# to see the arguments matrix() takes, use


args(matrix)


# or help(matrix)


matrix(data = 1:12, nrow = 3, ncol = 4)


matrix(data = 1:12, nrow = 2, ncol = 6)


matrix(data = 1:12, nrow = 2, ncol = 6, byrow = TRUE)


# You can also create a matrix from vectors


my.vec <- c(4:8)
my.vec2 <- c(5:9)
my.vec3 <- c(1:5)


cbind(my.vec, my.vec2, my.vec3)


rbind(my.vec, my.vec2, my.vec3)


# Let's store the last matrix


mat <- rbind(my.vec, my.vec2, my.vec3)


rownames(mat)


colnames(mat) <- c("col1","col2","col3","col4","col5")


# We can extract particular elements of a matrix just like
# we could from a vector, though this time we have to specify
# two dimensions, the row and the column


mat[1,1]


mat[c(1,2), 1]


mat[,1]


mat[1,]


# finally we can check the data types and object class


mode(mat[,1])
	# all matrix cols are of one data type
	# dataframe may contain different data types

class(mat)


attributes(mat)


#
## Section 5: Writing Functions (Coin Your Own Verb)
#

# You can also write your own function in R

# This function will take three numbers as arguments;  it will add
# the first two and divide the sum by the third


my.function <- function(x,y,z){
  out <- (x + y)/z
  return(out)
}


## Now we call our function with 


my.function(x = 5, y = 10, z = 3)


my.function(5, 10, 3)


# General format...

# function.name <- function(inputs){
#   output <- do.somthing.to(inputs)
#   return(output)
# }


# Now let's see a function that returns the smallest element in a vector
# using the R commands we've seen so far


small <- function(vec){
  sorted <- sort(vec)
  out <- sorted[1]
  return(out)
}


# Here's a new vector we can use for a test


test <- c(2, 5, 4000, .1, 1)


small(test)











