## Gov 2000 Section
## November 19, 2008


## Matrix operations in R

## Remember the two ways we've seen to create matrices:

mat <- matrix(1:30, byrow = TRUE, nrow = 5)

row1 <- c(1:6)
row2 <- c(7:12)
row3 <- c(13:18)
row4 <- c(19:24)
row5 <- c(25:30)

mat2 <- rbind(row1, row2, row3, row4, row5)

## And some matrix-specific commands

nrow(mat)
ncol(mat)
dim(mat)
class(mat)

## So let's enter our working examples

A <- matrix(c(2,3,5,1,0,7), byrow = TRUE, nrow = 3)
B <- matrix(c(2,3,2,0,1,5,1,2), byrow = TRUE, nrow =2)
C <- matrix(c(1:7,10,12), byrow = TRUE, nrow = 3)
D <- c(4,10,-2)

#################################
## Matrix Operations
#################################

## Which of the above can we add together?

A + B

A + C

A + D

## BE AWARE: R takes liberties with vectors.  Here, matrix addition
## isn't defined for A+D, and yet R makes it work by adding the vector D
## to both columns of A.  Don't just rely on R's 'non-conformable' errors

A+A

2*A

A - 2*A


## Transpose of a matrix

t(A)
t(B)
t(C)
t(D)

## Note that t(D) becomes a row vector, demonstrating that R treats D
## as a column vector by default


## We can save any of these operations as an object; R expects that they will
## produce a matrix, so you don't need to create a blank matrix first
## (I say this because there has been some redundant coding on the
## problem sets)

my.transp <- t(A)

class(my.transp)


## Matrix multiplication

## Remember, matrix multiplication AB is defined to be the inner product of 
## the rows of A and the columns of B.  What must be true for two matrices
## to be multiply-able, a.k.a. 'conformable'?

dim(A)
dim(B)

A %*% B

B %*% A

t(B) %*% t(A)

A %*% A

A %*% t(A)

t(A) %*% A





## R is more flexible with vectors, and will transpose the first
## vector to make multiplication conformable

C %*% D

D %*% C

## But R respects dimensions if you've done something to change them
## (like take the transpose)

D %*% D

t(D) %*% D

t(D) %*% t(D)

# this one errors, because you've used t(), which set dimensions

t %*% t(D)

# same with this one





## Inverse of a matrix

solve(A)

## What's the problem here?

solve(C)

solve(t(A) %*% A)



## The identity matrix

ident <- matrix(c(1,0,0,0,1,0,0,0,1), byrow = TRUE, nrow = 3)

C %*% ident

t(C) %*% ident

ident %*% ident


C %*% solve(C)


####################################
## Now let's load some data
#####################################

load("ps6_data.RData")

head(Democ)

## In the expression for OLS coefficients, beta = ((X'X)^-1)X'y
## where X is our model matrix

## Whenever we want to estimate an intercept (as opposed to 
## forcing it to be 0), we must include a column of 1's in the 
## model matrix

## We can either create this matrix ourself

democ.mm.temp <- as.matrix(Democ)
democ.mm <- cbind(1, democ.mm.temp[,-1])

## And add the name of the column we just added

colnames(democ.mm)[1] <- "Intercept"


## Note that we could also have named the column as we built the matrix


democ.mm.temp <- as.matrix(Democ)
democ.mm <- cbind("Intercept" = 1, democ.mm.temp[,-1])


## Another way to construct the model matrix is to use model.matrix().
## This function takes an object as its argument; either the output
## of lm(), or a formula like you feed lm() in its first argument


my.mm <- model.matrix(Democracy ~ GDPPC + Literacy, data = Democ)


## And we need a vector of the dependent variables,

dep <- Democ$Democracy


## Finally, what to do about missing data?
## While there are more sophisticated techniques for handling
## missing observations, for now we will follow lm() by listwise-deleting
## missing observations

## There are no missing observations in the Democ dataset.  Let's 
## alter the dataset so that the GDPPC is coded as missing for 
## observations 10 and 195, and so Literacy is missing for observations 34
## through 37

## I'll make a copy of Democ so that the original will still be in 
## our working memory unaltered

new.democ <- Democ

## We can see the dimensions of the original data with
dim(new.democ)

## And the dimensions of the data after listwise deletion with

dim(na.omit(new.democ))

## So let's code some of our data as missing (In practice we would not 
## do this- we would have been given a datset which was already missing
## observations.)

new.democ[c(10, 195), "GDPPC"] <- NA
new.democ[34:37, "Literacy"] <- NA

## Now let's see the dimensions after listwise deletion:

dim(na.omit(new.democ))

## Let's save a dataset post-listwise deletion, one that has no 
## missing observations

democ.na.removed <- na.omit(new.democ)

dim(democ.na.removed)




























