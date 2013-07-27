## Section 10/29/08

## Simple linear regression using lm(), review of sampling from 
## a matrix or dataframe, and an example of a function which returns
## a matrix



## We'll use some data in the car library 
## If you don't have the car library yet, install the package with

install.packages("car")

## Once the car package is installed, load the car library with 

library(car)




## We are going to use a dataset in car called Davis about 
## height and weight

head(Davis)






## In the simple OLS case (i.e. one independent variable) we can plot
## the dep. variable against the indep. variable to gather some 
## insight-by-inspection about how the variables relate

plot(x = Davis$weight, y = Davis$height)






## You know the analytic formulas for the values lm() returns,
## but lm() can save time (and lets you double check your results
## when we won't let you use lm() for your final answer on a problem set)

lm(height ~ weight, data = Davis)



## The '~' separates your dependent variable from the independent variables.










## You can store the output of lm() to access other useful functions
## and methods

out <- lm(height ~ weight, data = Davis)

out

summary(out)


## To see what we can extract from our lm object 'out', we use

names(out)

coefficients(out)

coefficients(out)[1]

coefficients(out)["(Intercept)"]

head(residuals(out))

## You can also perform some of the diagnostics we discussed in lecture
## very simply

plot(out,1) # non-linearity
plot(out,3) # non constant error variance 



## 'hatvalues' (which are a measure of influence) can be calculated with

hatvalues(out)

plot(x = Davis$weight, y = hatvalues(out))

abline(h = 4/nrow(Davis), col = "cadetblue")




#####################
##  Suppose we think the data in Davis describes an entire population
## and we want to simulate repeated sampling 

## Remember that taking a random draw from the dataset means
## taking a random draw of 'observations' or 'respondents'.

## When we draw an observation, we need to draw both the x and y value
## for that observation.



## Matt described how to take random samples from a Matrix or Dataframe
## a couple of weeks ago.

## A review:

## Each row of Davis is a different observation

## To take observation 1, we write

Davis[1,]

## To take a random draw of observations, we can first 
## draw a random sample of row numbers 
## and then  collect the corresponding observations into
## a dataset.

## One random sample of size 10 can be found with

index <- sample(1:nrow(Davis), size = 10, replace = TRUE)

one.sample <- Davis[c(index),]




#######################
## Functions that return matrices
#######################



## There is nothing special about returning a matrix with a function

## Here is a function (unrelated to this week's problem set)
## which shows the mechanics


my.function <- function(x,y){ #x and y are vectors
 
  mat <- matrix(data = NA, nrow = 2, ncol = 3)
 
  x.sum <- sum(x)
  mat[1,1] <- x.sum

  y.sum <- sum(y)
  mat[1,2] <- y.sum

  xy.sum <- sum(x.sum, y.sum)
  mat[1,3] <- xy.sum

  ## You can also fill whole rows (or columns) at once with a vector:

  mat[2,] <- c(mean(x), mean(y), mean(c(x,y)))

  colnames(mat) <- c("x calculations", "y calculations", "x and y calculations")
  rownames(mat) <- c("sum", "mean")

  return(mat)
}


my.function(x = c(1:10), y = c(23:40))


fun.out <- my.function(x = c(1:10), y = c(23:40))

fun.out[1,2]
fun.out["sum", "y calculations"]

