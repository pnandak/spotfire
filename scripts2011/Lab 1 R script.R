## CS&SS Intro to R
## Lab 1


## Working Directory
## What working directory are you in?

getwd()

## Change your working directory to your personal folder for R work for this course.
## Open a new script, name it "lab 1.R", and save it in your personal folder.
## Save the code you write for this lab in the script.

## Basic Arithmetic in R

## Compute the log of 50
log(50)

## Use the help system to find the exponential function
help.search("exponential")

 ## Now check out the help page for
	##  base::log       Logarithms and Exponentials

?log

## Find the value of e
exp(1)

## Compute e to the tenth power.
exp(10)


## Vectors
## Run the following code (type CRTL+R) to create an income variable
## (thousands of dollars)
income = c(18,20,20.5, 22,28,30,36,36,37,38.5, 45, 46, 55, 55.5,58,59, 115, 367)

## how many observations are in this data set?
length(income)

## what is the mean income?	
mean(income)

## what is the median income?
median(income)

## Create a variable for the log of the income.
log.inc = log(income) 

## Use help.search() to figure out how to make a histogram.
help.search("histogram")
## Now check out graphics::hist     Histograms
?hist

## Make a histogram of income
hist(income) 


## Make a histogram of log(income)
hist(log.inc)


## Use the help system to find out how to compute the tangent.
help.search("tangent")  ## doesn't work!  we need to be creative.  try:
 help.search("trigonometric")

## now check the help page for
## base::Trig      Trigonometric Functions
?Trig


## compute the tangent of 1.57.
tan(1.57)

## plot the tangent function from 0 to 1.5
?plot  ## see example at bottom of page: plot(sin, -pi, pi)
plot(tan,0,1.5)

## save your R script to your personal directory.

## Type ls() to view everything in your workspace.
ls()

## save your workspace to your personal directory.
getwd()
setwd("C://temp//gail")
save.image("R work lab 1.RData")