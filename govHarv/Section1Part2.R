## Gov 2001
## Section 1

## More Advanced R Topics
## Patrick Lam


## Dataframes


## Dataframes are an important object class for our statistical analyses.
## All of our datasets must be of the dataframe class before they can be used
## by any of the canned regression functions.


## Let's load a sample dataframe from the Zelig library

library(Zelig)
data(turnout)
class(turnout)


## One important difference between matrices and dataframes is that dataframes
## can hold different types of vectors (integer, character, factor, etc.)

class(turnout$race)
class(turnout$age)


## If a matrix contains a character vector, it automatically turns everything into characters
## (it can only contain vectors of the same type)

as.matrix(turnout)


## If our data is a matrix, we can easily turn it into a dataframe for use in canned regression functions.

my.data <- matrix(c(rep(10,10)), nrow=5, ncol=5); my.data
my.data.frame <- as.data.frame(my.data); my.data.frame
class(my.data.frame)


##############################################

## Arrays


## An array is a (possibly) three dimensional object that stacks matrices.
## Think of layers of a cake where each layer is a matrix and all 
## the layers are of equal dimension.

## You can also do one or two dimensional arrays, but we would probably
## prefer to use vectors or matrices for that.


# Let's create an array that consists of 4 2x3 matrices stacked on 
# top of each other with all 0 values using the array() function.

my.array <- array(0, dim = c(2,3,4))
my.array

# The first argument of array() takes the data values.
# The second "dim" argument takes a vector that defines the dimensions.
# If we gave it a scalar, then it would be a 1 dimensional array.  If we 
# gave a vector of length 2, it would be a 2 dimensional array.  Here, we 
# have a vector of length 3, with the dimensions being c(row, column, height).


# The indices for an array work similarly to those of a matrix.  It always follows the 
# [row, column, height] order where height is the layers of the array from the top.


# Suppose we want the second columns of all the matrices to take on a value of 1:
my.array[,2,] <- 1 
my.array


# or make the whole second matrix into 5s
my.array[,,2] <- 5
my.array


# or make the 2nd row, 1st column of the 4th matrix into 123456
my.array[2,1,4] <- 123456
my.array
my.array[,,4]



######################################################

## Lists 


## A list is much more flexible than an array.  It can combine a LOT (more than 3) of different
## datatypes into one object.

# For example, we can combine a vector, a matrix, a dataframe, and an array into one list.

my.array
my.vector <- c(3,6,3,10,7,8,5,2); my.vector
my.matrix <- matrix(4, nrow = 3, ncol = 2); my.matrix
my.dataframe <- as.data.frame(my.array[,,4]); my.dataframe

my.list <- list(v = my.vector, m = my.matrix, d = my.dataframe, a = my.array)
my.list


# Note the arguments I gave to list().  I'm asking R to compile all those things into a list,
# and to the left of the equal sign, I'm giving each element in the list a name.  So my vector is 
# named "v", my matrix is named "m", etc.  This can be helpful for subsetting.  I don't really need the
# names for it to work though.  list(my.vector, my.matrix, my.dataframe, my.array) will work, only they
# don't have names, although I can name them afterward with the names() function.



# There are a couple ways to pull stuff out of a list:


# 1. If I had named the elements of the list, I can use the $ operator:

my.list$v
my.list$a


# 2. I can also use double brackets to pull up the ith element: my.list[[i]]

my.list[[2]]


# 3. I can do double brackets and "" if they are named:

my.list[["d"]]


# I can also pull out stuff from within the elements of the list.  


# first matrix of the array in my list:
my.list$a[,,1]
my.list[[4]][,,1]
my.list[["a"]][,,1]


# V2 of my dataframe within my array:
my.list$d$V2
my.list[[3]]$V2



#######################################

## Functions


## Functions are probably the most important concept to learn in R.

## A function is something that takes one or more inputs and 
## spits out one or more outputs.


# let's try an easy function which I call "divide"
# it takes two numbers (x and y) and divides them and returns the answer

divide <- function(x,y){
  out <- x/y
  return(out)
}

divide(x=3, y=2)
divide.object <- divide(x=6, y=3)
divide.object



## Let's try a harder function now.  I want to write a function that draws n random draws
## from a normal distribution with a given mean and sd and then plots those draws and its mean
## and prints and stores the mean and the certain quantiles in a list.


my.func <- function(n.draws=1000, mean, sd, probs=c(.025, .975)){
  draws <- rnorm(n.draws, mean=mean, sd=sd)
  quants <- quantile(draws, probs = probs)
  mean.draws <- mean(draws)
  
  plot(density(draws))
  abline(v=mean.draws, col="red")

  res <- list(mean=mean.draws, quantiles=quants)
  cat("The mean is", mean.draws, "\n")
  cat("The", probs[1], "quantile is", quants[1], "\n")
  cat("The", probs[2], "quantile is", quants[2], "\n")

  return(res)
}

test <- my.func(mean=0,sd=1)
test$mean
test$quantiles


# Note that I can specify default values in the arguments of my function (as I did for n.draws and probs)
# I can simply replace the defaults by specifying specific values for the arguments

test2 <- my.func(n.draws=10000, mean=5, sd=3, probs=c(.25,.75))


# Also, note that I avoided "hard-coding".  That is, I made as many things part of the function arguments
# as possible.  So instead of putting the mean and the sd inside the function, I made them into arguments
# so that I can change them without changing the function.  This is good practice and makes your function
# more flexible.


# Note that I used the cat() function to print out results and I also stored the results 
# using return().  The "\n" tells it to skip to the next line.



## Debugging functions

# I'll go over briefly how to debug functions.  The basic things to use are
# traceback(), debug(), and browser().



## How to Read a Help File

?lm









