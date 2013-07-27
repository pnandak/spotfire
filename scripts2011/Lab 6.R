## Lab 6

## CSSS 508

## Programming in R

## Functions

## The logit function (the log odds) is often used in statistics.  Here it is:
logit = function(p) log(p/(1-p))

## Note that the range of logit is (0,1)

## Plot the logit function on the range 0,1

## The inverse of the logit function is the expit function.  That is, 
## logit(expit(x)) = x and expit(logit(p))=p.  Derive the expit function
## and program it in R.  Test that it is the inverse of the logit.


## Plot the expit function on the range -10, 10

## loops

## Write a loop to plot 11 normal curves, each with standard deviation 1, but 
## with means ranging from 0 to 10,
## overlaid on the same plot with range 5 to 15 on the x-axis.
## Set the color of each curve equal to the loop index so that the 
## curves are different colors.


## apply

## Create a matrix with 100 rows and 100 columns, containing
## 100 samples from a chi squared distribution with 4 degrees of freedom

## Use the apply() function to compute the 2.5% and 97.5% quantiles of 
## each row.  You will need the quantile() function.

## tapply

## load the chickwts data set with 
data(chickwts)

## look at the data
summary(chickwts)

## This data set contains chick weights (grams) by feed type.

## Make a box plot of weights by feed type.

## Use tapply() to compute the mean chick weight for each feed type.

