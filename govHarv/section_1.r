## Section 1, Sept 17, 2008
## Introduction to R

## This worksheet will introduce you to the basic concepts and commands in R.




## R works by typing code in the console (next to the red 'greater than'
## sign) or by typing on a script (like this one) and sending the lines of
## code to the console.




## Every line that begins with '#' is ignored by the console.
## So you can add comments to your code without generating syntax errors.




## R can be used as a calculator

2+2


50821/6


21^4





## More usefully, you can store objects in R's workspace


x <- 50821/6

x

x*6

x <- 2

x



## To see what objects are in your workspace

ls()






## Other types of objects will be useful when working with data

## A vector is a collection of numeric objects

## To create a vector in R, 'concatenate' the objects with c()





vec <- c(1,2,3,4,5)

vec






## R knows vector operations


vec2 <- c(2,5,1,3,2)




vec + vec2




vec - vec2




6 * vec




vec * 6




vec * vec2







## Tricks for creating vectors



newvec <- 1:5




newvec <- c(1:5, 7, 11)




newvec <- c(newvec, newvec)







## You can extract a particular element of a vector using subseting



newvec[1]



newvec[6]



newvec[-1]



newvec-1



newvec[c(9,11)]



newvec[9,11]



newvec[c(2:6)]





## You can also replace a particular element of a vector


a <- c(2,2,2,2,2)


a[3] <- 6







##########################################################################
## Functions
##########################################################################

## R has many preprogrammed functions that manipulate objects
## To use a function, you type the function name followed by the
## arguments in parentheses


a <- c(1,3,6,5,9,22)

b <- c(4,5,6,5,2,1)




sum(a)

sum(b)

sum(a,b)




mean(a)




length(a)





length(newvec)




sort(a)


c <- c(4,5,6,5,2,1)

all.equal(b,c)

all.equal(a,b)




## you can store the output of a function

output <- length(a)





## These functions are useful for creating vectors

seq(from = 0, to = 5, by = .5) 





rep(10, 200)







########################################################################
## Matrices
########################################################################

## the matrix() function in R is one way to create a matrix

## to see the arguments matrix() takes, use

args(matrix)

## or help(matrix)


matrix(data = 1:12, nrow = 3, ncol = 4)


matrix(data = 1:12, nrow = 2, ncol = 6)


matrix(data = 1:12, nrow = 2, ncol = 6, byrow = TRUE)


## You can also create a matrix from vectors

my.vec <- c(4:8)
my.vec2 <- c(5:9)
my.vec3 <- c(1:5)

cbind(my.vec, my.vec2, my.vec3)


rbind(my.vec, my.vec2, my.vec3)


## Let's store the last matrix

mat <- rbind(my.vec, my.vec2, my.vec3)

rownames(mat)

## We can extract particular elements of a matrix just like
## we could from a vector, though this time we have to specify
## two dimensions, the row and the column

mat[1,1]


mat[c(1,2), 1]


mat[,1]


mat[1,]



#################################################################
## Writing Functions
#################################################################

## You can also write your own function in R

## This function will take three numbers as arguments;  it will add
## the first two and divide the sum by the third

my.function <- function(x,y,z){
  out <- (x + y)/z
  return(out)
}


## Now we call our function with 

my.function(x = 5, y = 10, z = 3)

my.function(5, 10, 3)




## Now let's see a function that returns the smallest element in a vector
## using the R commands we've seen so far

small <- function(vec){
  sorted <- sort(vec)
  out <- sorted[1]
  return(out)
}

## Here's a new vector we can use for a test

test <- c(2, 5, 4000, .1, 1)

small(test)


