##-----------------------------------------##
## Script for Session 7: Building Packages ##
##    John Fox                             ##
## Statistical Computing in R/S            ##
##    ICPSR Summer Program 2008            ##
##-----------------------------------------##

  # Functions for a simple matrix-algebra package
  
source(
  "http://socserv.socsci.mcmaster.ca/jfox/Courses/R-course/matrixDemos.R")
  
objects()

# some examples

A <- matrix(c(1,2,3,4,5,6,7,8,10), 3, 3) # a nonsingular matrix
A
RREF(A) # the reduced row-echelon form of A

b <- 1:3
RREF(A, b)  # solving the matrix equation Ax = b

RREF(A, diag(3)) # inverting A

B <- matrix(1:9, 3, 3) # a singular matrix
B
RREF(B)

RREF(B, b)

RREF(B, diag(3))

Ginv(A, fractions=TRUE)  # a generalized inverse of A = inverse of A
round(Ginv(A) %*% A, 6)  # check

Ginv(B, fractions=TRUE)  # a generalized inverse of B
B %*% Ginv(B) %*% B   # check

C <- matrix(c(1,2,3,2,5,6,3,6,10), 3, 3) # nonsingular, symmetric
C
cholesky(C)
cholesky(C) %*% t(cholesky(C))  # check

    # Create the package "skeleton"

objects()
remove(A, b, B, C)
objects()

setwd("c:/temp")

package.skeleton("matrixDemos")



    



