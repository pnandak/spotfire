## Solutions to 'Section Prep' and 'Additional R Practice'

############################################################
## From 'Section Prep'
############################################################

## Entering data: enter two vectors and cbind() and rbind() them

vec1 <- c(2,4,6,8)
vec2 <- c(1,3,5,7)

cbind(vec1, vec2)
rbind(vec1, vec2)

## cbind() treats the two vectors as columns and binds them together
## along the columns;
## rbind() treats the two vectors as rows and binds them together along
## the rows

## Writing a mean function

my.mean <- function(x){
  out <- sum(x)/length(x)
  return(out)
}

## Now we can test the function on our vectors vec1 and vec2

my.mean(x = vec1)
my.mean(x = vec2)

######################################################################
## From 'Additional R Practice'
######################################################################

## 1) 

a <- c(2,3,2,6,7,4) #create a vector of length 6

mean(a) #find its mean

a[1:2] <- 20 #replace the first two elements with the number 20

a #take a look to make sure our replacement worked

mean(a) #find the new mean



## 2)

b <- c(4,4,4,8,8,9) #create a second vector of length 6

mat.ab <- cbind(a,b) #bind the two into a matrix

mat.ab <- mat.ab[-5,] #remove the fifth row of the matrix



## 3)

one <- c(2,1,7,11,9) #enter the vector

two <- c(1,11,2,7,9) #enter the vector

all.equal(one,two) #test for equivalence; the aren't (all.equal doesn't return 'true')

one.sorted <- sort(one) #sort vector one

two.sorted <- sort(two) #sort vector two

all.equal(one.sorted, two.sorted) #test for equivalence; returns 'true'





## 4) Write a function that adds the first and fourth arguments, 
##    divides that sum by the third argument, and multiplies that
##    quotient by the second argument.

## First I'll show a less efficient but easier to visualize solution

f.arithmetic <- function(a,b,c,d){
  total <- a + d
  quot <- total / c
  prod <- quot * b
  out <- prod
  return(out)
}

## Here's a more efficient function which does the same thing

f.arithmetic.2 <- function(a,b,c,d){
  out <- ((a + d) / c)*b
  return(out)
}


f.arithmetic(a = 2, b = 3, c = 4, d = 5) #works
f.arithmetic.2(a = 2, b = 3, c = 4, d = 5) #works


## 5) Write a function that takes a vector of at least three as its
##    argument and returns the third element of the vector multiplied
##    by the length of the vector

f.third <- function(x){ #x is a vector with length at least 3
  out <- x[3] * 3
  return(out)
}

test <- c(4,3,12,6,6,6,6,4,9)
f.third(x = test) #works

## 6)  Write a function that calculates the median of a vector.

my.median <- function(x){ #x is a vector with odd length
  sorted <- sort(x) #sort the vector
  medspot <- (length(x) + 1)/2 #find which spot marks the 'middle' of the vector
  out <- sorted[medspot] #pick the middle element out of the sorted vector
  return(out)
}
  
test2 <- c(4,4,4,2,2,6,6)
my.median(test2) #works

