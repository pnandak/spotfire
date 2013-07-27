############################################

#   Goverment 50
#   Section 2 - Probability Distributions
#   February 14, 2008
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################

#   Goals for today:

#   1) Using the probability distributions in R

#   2) Simulating sampling distributions

#   3) Some examples

############################################

#   1) Using the probability distributions in R

#   One of the strengths of R is its functionality for working with 
#   probability distributions.  There are predefined functions for many
#   important families of distributions, including all of the ones that we 
#   will need in this course.  For each family of distributions, there are
#   four functions; we will illustrate this using the normal distribution, 
#   which is the most important of the continuous distributions.

#   A) The normal distribution

#   The normal distribution is continuous and symmetric, and is governed by
#   two parameters: the mean, which determines where the distribution is 
#   centered, and the standard deviation, which determines how spread out the 
#   distribution is.  

#   The most important member of the set of normal distributions is the 
#   "standard normal" distribution, which has mean 0 and standard deviation 1.

#   We can calculate the value of the density of a normal distribution at any 
#   point by using the dnorm() function.  For a continuous function, the density 
#   function has higher values in areas of higher probability, but the height of
#   the density at a number does not represent the probability of obtaining that 
#   number:

dnorm(0, 0, 1)
dnorm(1, 0, 1)
dnorm(0, 0, .1)

#   Since probability is the area under the density curve, it is often useful 
#   to plot the density of a distribution that you are working with.  To do that 
#   in R, you first need to create a set of points at which to evaluate the density.
#   Then you can plot (using type="l") as an option, as follows:

xx <- -600:600/100
plot(xx, dnorm(xx, 0, 1), type="l")
lines(xx, dnorm(xx, 0, 2), col="red")
lines(xx, dnorm(xx, 1, 1), col="blue")
lines(xx, dnorm(xx, 1, 2), col="green")

#   One of the nice things about the normal is that the center and spread of the 
#   distribution are independent; you can shift one without shifting the other.
#   This isn't always true.

#   In lecture, we also talked about the cumulative distribution function for a 
#   random variable.  That is, for a point xx, what is the probability that the 
#   random variable is less than xx.  The CDF is an increasing function (why?) and
#   is bounded between 0 and 1.

#   You can calculate the CDF of a random variable in R:

pnorm(0, 0, 1)
pnorm(1, 0, 1)
pnorm(0, 0, .1)

#   You may also want to plot the CDF:

xx <- -600:600/100
plot(xx, pnorm(xx, 0, 1), type="l")
lines(xx, pnorm(xx, 0, 2), col="red")
lines(xx, pnorm(xx, 1, 1), col="blue")
lines(xx, pnorm(xx, 1, 2), col="green")

#   CDFs are most useful for calculating the probability that the RV falls within
#   some interval.  What is the probability that a draw from a standard normal
#   variable is between -1 and 1?   -2 and 2?

pnorm(1, 0, 1) - pnorm(-1, 0, 1)
pnorm(2, 0, 1) - pnorm(-2, 0, 1)

#   A third set of functions is the inverse of the CDF, or the quantile function.
#   This asks the question, "where do I have to be on the scale of the random 
#   variable so that xx % of the draws are smaller than that value?

qnorm(.975, 0, 1)
qnorm(.2, 0, 1)

#   Obviously, xx has to be between 0 and 1.
#   qnorm() and pnorm() are inverses of each other.

xx <- c(1,2,3,4,5)
qnorm(pnorm(xx, 0, 1))

#   Finally, the fourth function associated with the normal distribution is rnorm.
#   This function generates random draws from the normal distribution with paramenters
#   that you specify.  The first argument is the number of draws to take, the 
#   second is the mean, and the third is the standard deviation.

aa <- rnorm(1000, 0, 1)
hist(aa, freq=FALSE)
aa <- rnorm(10000, 0, 1)
hist(aa, freq=FALSE)
xx <- -600:600/100
lines(xx, dnorm(xx, 0, 1))

#
#   B) The uniform distribution

#   Another common continuous distribution is the uniform distribution, in which
#   the density is flat over some interval.  The functions in R follow the same 
#   form, but instead of setting the mean and variance, they set the lower and 
#   upper bounds of the uniform.

#   dunif() isn't very interesting, since it is the same for all values in the 
#   range.  We can still look at a few uniform distributions, however:

xx <- -600:600/100
plot(xx, dunif(xx, 0, 1), type="l", ylim=c(0, 2.5), xlim=c(-.5, 2.5))
lines(xx, dunif(xx, 0, 2), col="red")
lines(xx, dunif(xx, .5, 1), col="blue")
lines(xx, dunif(xx, .75, 2.25), col="green")

#   We can also look at the CDFs; these will all be straight lines with slope 
#   equal to 1/(b-a), where b and a define the interval.

plot(xx, punif(xx, 0, 1), type="l", ylim=c(0, 1), xlim=c(-.5, 2.5))
lines(xx, punif(xx, 0, 2), col="red")
lines(xx, punif(xx, .5, 1), col="blue")
lines(xx, punif(xx, .75, 2.25), col="green")

#   The uniform distribution is probably used most often to generate random
#   numbers:

hist(runif(100,0, 2), freq=FALSE)
hist(runif(1000,0, 2), freq=FALSE)
hist(runif(10000,0, 2), freq=FALSE)


#   C)  Two important (and related) discrete distributions: the Bernoulli 
#   and the binomial

#   When you have binary data (anything that can be thought of as zero or one),
#   it is usual to model that as a Bernoulli random variable.  This assumes that 
#   you get 1 with probability p and 0 with probability (1-p).  Imagine you did 
#   this n times; what would the distribution of the sum of those Bernoulli random
#   variables be?  You know that it can take on discrete values representing the 
#   number of successes: 0 sucesses, 1 sucess, 2 sucesses, etc., up through n 
#   sucesses.  A random variable with this distribution is known as a binomial 
#   RV with parameters n (the number of trials) and p (the probability of success).
#   Since a Bernoulli is a binomial with n=1 and p=p, there is only one set of 
#   functions in R for these two random variables.

#   Notice that, because these are discrete variables, dbinom gives you the 
#   probability mass function, not the density.

dbinom(0,size=1,p=.4)
dbinom(1,size=1,p=.4)
 
pbinom(0, size=1, p=.4)
pbinom(1, size=1, p=.4)

#   Again, this may be the most useful for generating Bernoulli random variables.
#   Let's say that I want 1000 Bernoulli RVs with probability 2/3:

yy <- rbinom(1000, size=1, p=2/3)
mean(yy)
hist(yy)



#   2) Simulating a sampling distribution

#   In the example from lecture, we had two independent uniformly distributed 
#   random variables:

#   X ~ Unif(0,100)
#   Y ~ Unif(0,100)

#   We wanted to find the sampling distribution of W = X - Y

#   How can we take one sample from the sampling distribution of W?

ww <- runif(1, 0, 100) - runif(1, 0, 100)
ww

#   To simulate the sampling distribution, we need to do this over 
#   and over again.  Fortunately, there is an easy way to do this 
#   in R using the replicate() function.

ww.sampling <- replicate(1000, runif(1, 0, 100) - runif(1, 0, 100))

#   This function takes two arguments.  The first argument is the number 
#   of times you want to sample from the sampling distribution.  The second 
#   argument is the statistic that you are trying to sample.  In this case,
#   it is the difference between two uniform random variables.

hist(ww.sampling, freq=F)

#   If we ask you to create a sampling distribution on the homework, here is 
#   a simple process to follow:
#   1) Write code that will give you one draw from the sampling distribution.
#      This will generally involve generating some random data and calculating
#      a statistic from it.
#   2) Wrap that data in the replicate function and decide how many draws that 
#      you want to take.  Save the output as an object.
#   3) Summarize the sampling distribution by calculating statistics or plotting
#      graphs of the samples you generated in (2)

#   As an example, what if we asked you to take 10000 draws from the sampling 
#   distribution for the median of 100 points drawn from a standard normal 
#   distribution?

#   1) Write code that will give you one draw from the sampling distribution.
#      This will generally involve generating some random data and calculating
#      a statistic from it.

median(rnorm(n=100, 0, 1))

#   2) Wrap that data in the replicate function and decide how many draws that 
#      you want to take.  Save the output as an object.

std.median <- replicate(10000, median(rnorm(n=100, 0, 1)))

#   3) Summarize the sampling distribution by calculating statistics or plotting
#      graphs of the samples you generated in (2)

hist(std.median, freq=F, xlim=c(-3,3))
lines(xx, dnorm(xx))
mean(std.median)


#   3) An example: maximum wind speeds (or why you shouldn't live in Florida)

#   In hurricanes, most of the damage due to wind is a function of the maximum 
#   wind speed that is experienced by an area, not by the length of the exposure.
#   So, it is useful to know what the highest wind speed is that you are likely
#   to get in a certain period of time.

#   Let's assume for the sake of argument that the daily maximum wind speed in 
#   a certain part of Florida is independent and identically distributed 
#   according to a exponential distribution
#   with parameter 1/10 (you don't need to know about the exponential distribution,
#   but it works well for this example).
#   First, some questions:
#   1)  What does the distribution of daily max wind speeds look like?

plot(0:2000/10, dexp(0:2000/10, 1/10), type = "l")

#   2)  Estimate the mean daily maximum wind speed:

mean(rexp(10000, 1/10))

#   OK, so the mean daily maximum wind speed doesn't seem so bad.  But what is 
#   the mean maximum wind speed over the course of a year?  Here is one realization:

years <- 1
xx <- max(rexp(365*years, 1/10))
xx

##  Now, what if we want the sampling distribution for the maximum wind speed during
##  a year?  We need to simulate lots and lots of years.

sims <- 1000
years <- 1
sample.max1 <- replicate(sims, max(rexp(365*years, 1/10)))

hist(sample.max1)
mean(sample.max1)

#   So that's still not so bad; what is the probability that you will experience
#   hurricance force winds over the course of a year?

mean(sample.max1 >= 75)

#   So, appox. a 1 in 5 chance of getting a hurricane.  Not great, but not terrible.
#   Most people don't live in Florida for just a year.  What about 10 years?

years <- 10
sims <- 1000
sample.max10 <- replicate(sims,  max(rexp(365*years, 1/10)))
mean(sample.max10)

mean(sample.max10 >= 75)

#   How about 100 years? (This may take a while)

years <- 100
sims <- 1000
sample.max100 <- replicate(sims,  max(rexp(365*years, 1/10)))
mean(sample.max100)

mean(sample.max100 >= 75)
mean(sample.max100 >= 130)





