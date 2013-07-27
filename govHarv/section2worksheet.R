## PROBABILITY SECTION
## Clayton Nall and Jens Hainmueller
## Thanks to past Gov 2000 TFs:
## Alison Post & Ryan T. Moore (discussion of distributions)
## Andy Eggers & Ben Goodrich (height distribution application and function writing)
## September 2007

## PDF's and CDF's:  Introduction, Including the Normal Distribution

## The Normal (Gaussian) Distribution

## PDF: Probability Density Function
## Illustration
x<-seq(from=-5, to=5, by=0.001)
plot(x,dnorm(x), type="l", col="blue", main="PDF for the Normal Distribution")

## - see the pdf on Fox97 p. 554
## - this is the function that generates curve with which we are familiar
## - notice that parameters are mu and sigma (mu = center or mean,
## sigma = standard deviation). 
## - Relationship between x and p(x), represented by the curve, will
## vary with these parameters (think about how p(x) will change as
## sigma gets larger)
## - this is a way of representing the overall population (here, of x)
## with which we are dealing 
## - area under curve always equals 1, regardless of mu and sigma 
## - y-axis values do not have straight-forward interpretation, as the
## integral of the pdf evaluated at any specific point is 0 (since
## p(x) is continuous.  Were we to integrate within a certain area, we
## would get a probability value, though.)
## - Examples of such integrals:
##    - \int[-1,1] \approx .68
##    - \int[-2,2] \approx .95
##    - \int[-3,3] \approx .997
## - the y-axis values can be thought of as "normalizing" the density
## - unit normal: mu = 0, s^2 = 1 : will be a building block for us

## - examples of superimposed normal distributions

x <- seq(from=-5, to=5, by=0.001)
plot(x,dnorm(x), type="l", col="green", main="PDF for the Normal, s=1,3")
##Recall: if Y=AX+B, mean(Y)=A*mean(X)+B, sd(Y)=A*sd(X)
##Y=3X
lines(x, dnorm(x,sd=3), type="l", col="red")
##Y=3X+2
lines(x, dnorm(x, m=2, sd=3), type="l", col="blue")
dev.off()

## CDF: Cumulative distribution function
## Illustration

par(mfrow=c(2,1))
x <- seq(from=-5, to=5, by=0.01)
plot(x,dnorm(x), type="l", main="PDF for the Normal Distribution")
plot(x,pnorm(x), type="l", main="CDF for the Normal Distribution")

## Note that y-axes not scaled the same.  The y-axis scale for the pdf
## gives the probability of X=x (if discrete) or acts as described
## above (if continuous).  The y-axis scale for the cdf always gives
## the P(X <= x) [i.e., height of CDF tells area under PDF to the left
## of X = x].

## In other words:  The cumulative distribution function yields the
## probability that a random variable takes on a value less than or
## equal to a particular value x.  We would obtain the same result if
## we were to calculate the area under the pdf up until the value x.

par(mfrow=c(1,1))

## IMPLEMENTATION IN R

## Normal PDF
x <- seq(from=-5, to=5, by=0.1, main="PDF: Unit Normal Distribution")
plot(x,dnorm(x), type="l")
dnorm(0)

## Normal CDF
x <- seq(from=-5, to=5, by=0.1)
plot(x,pnorm(x), type="l", main="CDF: Unit Normal Distribution")

## Illustrative calculations for the normal cdf
pnorm(0)
pnorm(1)
pnorm(-1)
1-pnorm(0)
##What kind of function would we use if we wanted to know, 
##say, what score would be necessary
##to score better than 99% of other SAT test takers? 



## QUANTILE FUNCTIONS

## The quantile function calculates, for an input q, the smallest x
## such that Pr(X <= x) > q.  It can be thought of as the inverse of
## the CDF.  (For the test score example, we want the score x such that Pr(SAT Score <= x) > q).

qnorm(.95)
pnorm(1.645)
pnorm(qnorm(.95))
qnorm(.95, mean=5)
qnorm(.5)
qnorm(0)

## RANDOM NUMBER GENERATION

## generates a set of pseudo-random numbers drawn from a given
## probability distribution.  "Pseudo-" because these are, in fact,
## deterministic functions, but they have very long periods.

set.seed(1234)
rnorm(10, mean=0, sd=2)
n <- 10 
rnorm(n, mean=0,sd=2) ##didn't set seed!

########################################################
# AN EMPIRICAL EXAMPLE USING NORMALLY DISTRIBUTED DATA #
########################################################

# Part A
# This is the R workspace that we will use for section
load(file = url("http://isites.harvard.edu/fs/docs/icb.topic98225.files/nhanes.RData"))

# It contains a dataframe called NHANES (National Health and Nutrition Examination Survey 2004)
# Let's examine it
summary(NHANES)

# Part B
# Unfortunately, height is measured in centimeters. Here's how make a variable in inches
NHANES$height.in <- NHANES$height.cm * .3937

# Now there is a fourth variable in NHANES called height.in
colnames(NHANES)

# Part C
# We will talk next week about the density of theoretical distributions (normal, F, etc.)
# Here we have data and want to see if it has an approximate normal distribution
# The following plots the what is called the kernel density, which is an *empirical* density.
plot(density(NHANES$height.in), xlab = "Height, in inches",
     ylab = "Kernel Density", col = "black", main = "Distribution of Height in the USA")
# Does the plot look like a normal distribution?
# Why not?
# What is the data generating process?

# How would we plot the density of female height?
plot(density(NHANES$height.in[NHANES$sex == "Female"]), xlab = "Height, in inches",
     ylab = "Kernel Density", col = "black", main = "Distribution of Female Height in the USA")
# Now, does the plot look like a normal distribution?
# What is the data generating process?
# What's the population of interest?

# How would we plot the density of fully grown (>=18) female height?
plot(density(NHANES$height.in[NHANES$sex == "Female" & NHANES$age >= 18]),
     xlab = "Height, in inches", ylab = "Kernel Density", col = "black",
     main = "Distribution of Women's Height in the USA", lty = "solid")
# Now, does the plot look like a normal distribution?

# Let's make objects for the mean and standard deviation of women's  height
women.mean <- mean(NHANES$height.in[NHANES$sex == "Female" & NHANES$age >= 18])
women.sd   <-   sd(NHANES$height.in[NHANES$sex == "Female" & NHANES$age >= 18])
# If the distribution of women's height were normal, then it will have this mean and sd

# How do we put the appropriate theoretical normal distribution on our plot for comparison?
ruler <- seq(from = 50, to = 75, by = .01)
head(ruler)
tail(ruler)
lines(x = ruler, y = dnorm(ruler, mean = women.mean, sd = women.sd),
      col = "green", lty = "dashed")

# Part D: Repeat for men >= 21
plot(density(NHANES$height.in[NHANES$sex == "Male" & NHANES$age >= 21]),
     xlab = "Height, in inches", ylab = "Kernel Density", col = "black", ylim = c(0,0.15),
     main = "Distribution of Men's Height in the USA", lty = "solid")

men.mean <- mean(NHANES$height.in[NHANES$sex == "Male" & NHANES$age >= 21])
men.sd   <-   sd(NHANES$height.in[NHANES$sex == "Male" & NHANES$age >= 21])

ruler <- seq(from = 55, to = 80, by = .01)
lines(x = ruler, y = dnorm(ruler, mean = men.mean, sd = men.sd), col = "blue",
      lty = "dotted")

# Part F: Compare to data from Match.com
lines(density(Match.com * 12), col = "red", lty = "dashed")
# What is wrong with this picture?

# Part G: Limit NHANES sample to young men
plot(density(NHANES$height.in[NHANES$sex == "Male" & NHANES$age >= 21 & NHANES$age <= 35]),
     xlab = "Height, in inches", ylab = "Kernel Density", col = "black", ylim = c(0,0.15),
     main = "Distribution of Young Men's Height in the USA", lty = "solid")

men.mean <- mean(NHANES$height.in[NHANES$sex == "Male" & NHANES$age >= 21 & NHANES$age <= 35])
men.sd   <-   sd(NHANES$height.in[NHANES$sex == "Male" & NHANES$age >= 21 & NHANES$age <= 35])

ruler <- seq(from = 55, to = 80, by = .01)
lines(x = ruler, y = dnorm(ruler, mean = men.mean, sd = men.sd), col = "blue",
      lty = "dotted")

lines(density(Match.com * 12), col = "red", lty = "dashed")

# Does it make sense to plot a normal density when reported Match.com heights are discrete?
library(MASS)
truehist(Match.com * 12)

#############################################
#### A BRIEF INTRODUCTION TO FUNCTIONS ######
#############################################

# Part A
# This is how we write functions

foo <- function(x) { # say what the function is used for
  # Check to make sure that the arguments are reasonable for this function
  # Write code inside function using x as an object within the function
  return(x) # Normally, a function does more than return its argument
}

foo(pi) # Looks like pi

# Better example: normal density
foo <- function(x, mu = 0, sigma2 = 1) { # returns the density of a normal distribution
  # Check to make sure the arguments are reasonable using the stopifnot() function
  stopifnot(is.numeric(x), is.numeric(mu), all(sigma2 >= 0))

  # Do whatever the function is designed to do
  density <- (1 / (sqrt(2 * pi * sigma2))) * exp(-((x - mu)^2) / (2 * sigma2))

  # Return whatever the function is designed to return
  return(density)
}

  foo(x = pi / 2) # .1161772
dnorm(x = pi / 2) # .1161772
  foo(x ="pi")    # error

# Part B
# We've seen how to draw from a given distribution (e.g., rnorm()).
# To draw randomly from a vector of numbers, we generally use the sample command
args(sample) # x is the vector of possibilities, size is the size of the sample to draw
             # replace is whether there should be sampling WITH replacement or not (the default)
             # prob is a vector that indicates how probable it is to draw each value of x
             # the default of prob is NULL, indicating equal probability for each x value

# Draw a sample of heights of size 1000 from Match.com using the defaults for replace and prob
set.seed(54321)
sample.1 <- sample(x = Match.com, size = 1000, replace=FALSE)

all(sample.1 %in% Match.com) # TRUE

# Do the same thing with replacement
sample.2 <- sample(x = Match.com, size = 1000, replace = TRUE)

summary(sample.1) # These two quantities are 
summary(sample.2) # very similar when the length of N (the population we're drawing from)
                  # is much larger than n, the size of the sample

# Part C
# Writing "for" loops
# Usually, you want to store the output of a for loop in something
holder <- matrix(NA, nrow = length(sample.1), ncol = 1)
# There are different ways to index a for loop, the most common is something like this
for(i in 1:length(sample.1)) { # This loops from 1 to whatever the length of sample.1 is
  holder[i,1] <- sample.1[i] > sample.2[i] ##we can do this because 
                                           ##length(sample.1)==length(sample.2)
}

summary(holder)


