## Section 9: Basic Tips for Linear Algebra in R
## Clayton Nall and Jens Hainmueller
## Based on some material from Alison Post and Ryan Moore.

## MULTIPLE REGRESSION "by hand" in R

## The next problem set will contain one long problem where you'll be
## asked to do multiple regression by hand in R.  We're reaching the
## point where we'd like you to know what is happening in the lm
## function.  You'll find that multiple regression is more easily
## represented in matrix
## form.  While Adam will present this in lecture, we want to give you
## a head start, since we won't have a Thanksgiving Eve section.

## Remember that we represent multiple regression with k variables
## in the following form:
## Y_i = \beta_0 + \beta_{1}X_{1i} +\beta_{2}X_{2i}+ ...+
##\beta_{k}X_{ki}+ \epsilon_i

## Writing this out every time would get extremely burdensome.
## It's a lot easier to represent all of the Y_i in matrix form.
## Y = X \beta + \epsilon (all of these terms in bold to indicate mtx)

## See Fox pp. 212, we can think about OLS as an optimization problem
## requiring us to solve a system of equations where we
## set the derivative of the sum of squared residuals with respect to
## b equal to zero.  
## We express this sum of squared residuals conveniently using the
## inner product of our vector of residuals.

## We'll set up a system of equations in matrix form
## where the columns are our X's (augmented a column of 1's for
## the intercept).  Our goal will be to solve for the \hat{\beta}
## vector that minimizes e'e (the regression sum of squares).  By
## definition, this is the value of the \hat{beta} vector that will give us
## sum(residuals)=0.

## This is exactly what we've been doing with lm(), but now we're
## going to do it by hand.

## We will not walk through exactly what you need to do on the
## homework, but we will give you some basic exposure to the aspects
## of matrix algebra that will be most important for solving the
## problem set.

## BASIC DATA HANDLING

## Constructing matrices
## You've already seen cbind()
A<-cbind(1, seq(1,10), seq(3,8, length.out=10))
C<-c(rep(NA,5), rep(1,5))
B<-rnorm(10) ##10 draws from the unit normal
D<-cbind(B,A,C)
## Neat trick in R: you can include a constant in cbind()
## and R will treat it as a vector.

## What about filling in a matrix manually
## This is usually not the way we'll do it.
## Concatenate the B,A,C vectors into one vector
D<-matrix(c(B,A,C), nrow=10, ncol=5)

## What to do about the NAs?
D
## We almost always have missing data in real life, but we
## can't do linear regression on  missing values.  So do the following:
D.new<-na.omit(D)
summary(D.new)
dim(D.new)
## What happened here?
## If these were explanatory variables, what would this do
## to our inferences?  What fraction of our sample did we lose?  What
## fraction of our data frame actually had missingness?
sum(is.na(D))/length(D) ## can use length to indicate # of cells in mtx
nrow(D.new)/nrow(D)
## "Listwise Deletion is Evil," retitled
## "Analyzing Incomplete Political Science Data," King, Honaker, Joseph, Scheve (2001)
## Intuition from Gary: Swiss Cheese
## We cut a slice, then cut that into strips.
## We throw away any row of cheese that
## has a hole. How is this a good idea?
## It's not usually, but for this course we'll listwise delete.


## LINEAR ALGEBRA: A few key points for R implementation

## We don't expect you to have a solid understanding of
## linear algebra at this point.
## Practically speaking, we just need to remember a few points about
## how to do some elementary matrix manipulations.

## Vector and matrix operations

## Addition and subtraction of matrices straightforward.

## Multiplication: Commutative property does not apply
## to matrices.  BA not necessarily equal to AB.  

## Rule of thumb for multiplying matrices:
## (m by n) x (n by k) = (m by k)
## Worksheet problems 

## Transposing vectors and matrices
## Change an nx1 column vector to a 1xn row vector
## Or transpose an m x k matrix to a k x m matrix.
## R defaults to column vectors (even though you
## see it as a row)
x<-c(3,4)
t(x)
y<-t(x) #y=x' (y equals x prime)
## Which of these gives us the inner product x'x?
t(x)%*%x
x%*%t(x)
## Using just an asterisk will multiply each element.
x*x
## rule of thumb (m by n) x (n by k) =(m by k)
## This requires us to transpose matrices to do operations
## For example, suppose we want to multiply D by itself.
## First get rid of the NA's.

DD<-t(D)%*%D

## We may want to solve a system of linear equations.  Suppose
## we have such a system consisting of a set of observed values
## so that Xb =y.  We want to solve for the b vector, which is unknown.
## One way to think about this intuitively is doing all the row
## operations in Gauss-Jordan elimination by left-multiplying
## with the inverse matrix.


## Let's consider an example where we have 3 independent vectors: x1, x2, and x3: 

x1 <- matrix(c(1,3,5),3,1)  ## Another way to fill matrices.
x2 <- matrix(c(3,1,2),3,1)
x3 <- matrix(c(1,1,1),3,1)
X<-cbind(x1,x2,x3)

y <-  4*x1 + 3*x2 + x3
## And we know that y =Xb.  How do we solve for b?
##inv(X) * Xb = inv(X)*y

## First, let's make sure we can do it.  Graphically, we can think of
## the set of column vectors of X as spanning some space, either a
## plane or hyperplane. The vectors, which can be thought about as
## originating from zero, form, in the simplest example, a
## parallelogram.  If these two vectors were dependent, they wouldn't
## form one.
## The area of the parallelogram (or volume of a higher-dimensional
## solid) is given by the DETERMINANT of the matrix.
det(X)
## The determinant is nonzero, so the matrix is invertible.
## Aside: what if one of the columns were a linear combination of the
##others?
X[,3]<-2*X[,1]+1*X[,2]
det(X)
## Thinking about this issue in terms of linear regression, if two of
## our explanatory variables are perfectly linearly related, we won't
## be able to do regression.

## Next, left-multiply by the inverse.
## We use "solve" to get a matrix's inverse.
## Xb=y

## On the left-hand side:
solve(X)%*%X ##This is an identity matrix
             ##Off-diagonals: (+/-rounding error)
## On the right-hand side:
solve(X)%*%y ##Presto! returns the coefficients
             ##from our system of equations.
             $$

## As go through this week's reading, think
## about how you'd derive the following for a linear
## regression using "by hand" matrix operations in R.

beta.hat 
y.hat 
resid 
S.E. 
RegSS 
TSS 
R2
hat values?



