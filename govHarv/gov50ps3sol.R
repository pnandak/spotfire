
###############################

##  Gov 50  Problem Set 3

##  Sample code

###############################


##  Problem 3

qnorm(.99)
qnorm(.95)
qnorm(.75)
qnorm(1-(1-.9973)/2)

##  Problem 4

qnorm(.995)

c(4.1 - qnorm(.995)*18.2/sqrt(2284), 4.1 + qnorm(.995)*18.2/sqrt(2284))

## Problem 5

(1.96)^2/(.02)^2*.3*(1-.3)
(1.96)^2/(.02)^2*.5*(1-.5)

## Problem 6

pi.hat <- 284/603
nn <- 603

c(pi.hat - qnorm(.95)*sqrt(pi.hat*(1-pi.hat)/nn), pi.hat + qnorm(.95)*sqrt(pi.hat*(1-pi.hat)/nn))

##   Problem 7

#    Load the salary dataset
salary <- read.csv("C:/salary.csv")
ftr <- salary$FTR

#    Determine the number of observations
obs <- length(ftr)
obs

#    Calculate summary statistics
mean(ftr)
median(ftr)
min(ftr)
max(ftr)
##   Population standard deviation; you could also use the regular
##   sd() command
sqrt(1/(obs)* sum((ftr - mean(ftr))^2))


#    Plot a histogram of FTR; divide into 100 bins; add informative labels.
hist(ftr, breaks=100, xlab = "Pay rate in dollars", main = "Full-time pay rate for employees \n at the University of Michigan")  


##    Problem 7

#    Set the random number generator
#    (not necessary, but useful for replication)
set.seed(7398742)

#    Set the sample size
size <- 100

#   Draw one sample of observation numbers

idx <- sample(1:obs, size, replace=FALSE)
idx

#   You can then extract only the observations that you want and calculate
#   the relevant sample statistics:

sample.mean <- mean(ftr[idx])
sample.mean

sample.sd <- sd(ftr[idx])
sample.sd

sample.se <- sample.sd/sqrt(size)
sample.se

#    Calculate a 95% confidence interval

ci.lower <- sample.mean + qnorm(.025)*sample.se
ci.upper <- sample.mean + qnorm(.975)*sample.se
c(ci.lower, ci.upper)

