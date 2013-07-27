#  CSSS 508
#  Lab 4 - sampling

## Run the following code to create the ages and IDs of a
## population of size 10,591 from which you will draw a sample.

n=10591
id = 1:n
age = sample(1:100, n, replace=T, prob=c(rep(1,49), seq(1,.05,by = -.019)))
sex = sample(c("M","F"), n, replace=T)
race = sample(c("Black","White","Asian","Hispanic","Native Am"),
	 n, replace=T,prob=c(.05,.9,.02, .025,.005))
hist(age)

## Tabulate age, sex, and age by sex.

## Create a Boolean variable called "child", which is true when age<=18
## and false when age>18

## Combine these variables into a data frame and summarize it.


## Draw a random sample of size 100 from this population.

## Now draw a weighted sample of size 100, where children (18 and under)
## are twice as likely to be sampled as adults.
## Hint: first create a "probability weight" vector which is 2 for kids
## and 1 for adults.  Then send this as a parameter to the sample function
## Store the IDs of your sample in a vector.

## For your sample, tabulate child, sex, race, and child by race.


## Chi-squared distribution
## The chi-squared distribution is used for many statistical tests.
## The functions pchisq, qchisq, rchisq, and dchisq can be used for 
## computations.

# type ?rchisq.  What parameter(s) does this distribution take?


# Draw a sample of size 1000 from a chi-square distribution with 
# degrees of freedom = 2.

# Make a histogram of your sample.

# The Chi-square distribution is used to test whether one model fits
# better than another.  You compute the chi-squared test statistic 
# and compare it to the chi-squared distribution.  Supposed your
# test statistic is 6.27

# If X follows a chi-squared distribution with 3 degrees of freedom
# what is the probability that X is greater than 6.27?  This is the p-value.


# What test statistic value do you need for a p-value of 0.05 or less?

