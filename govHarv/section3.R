############################################

#   Goverment 50
#   Section 3 - Estimation
#   February 21, 2008
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################


#   Goals for today:

#   1) Sampling from real data

#   2) Confidence intervals

#   3) Biasedness and Consistency - an example

############################################



##  1)  Sampling from data

#   So far we have spend a lot of time drawing random numbers from 
#   probability distributions with known properties in order to simulate 
#   the sampling distribution of some estimator.  Now, we want to turn to 
#   sampling from real data with an unknown distribution.  The following 
#   dataset consists of the monthly number of sunspots for the past 200 or 
#   so years.

data(sunspots)
hist(sunspots)

obs <- length(sunspots)
obs

#   If we define this as the population, we can calculate the population 
#   mean and variance:

mean.sunspots <- sum(sunspots)/obs
var.sunspots <- 1/(obs)* sum((sunspots - mean.sunspots)^2)

#   But, what if we could only look at a random sample of the sunspot data
#   say, of size 100?

size <- 100

#   The easiest way to sample from a dataset is to draw the index numbers
#   that you are going to use:

sample.sunspots <- sample(sunspots, size, replace=FALSE)

#   You can then extract only the observations that you want and calculate
#   the relevant sample statistics:

sample.mean <- mean(sample.sunspots)
sample.var <- var(sample.sunspots)
sample.se <- sqrt(sample.var/size)


#   Now, let's calculate the sampling distributions for the sample mean and 
#   variance.  To do that, I'm going to write a simple function that 
#   calculates both the mean and variance and reports it as a vector:

meanandvar <- function(x){c(mean(x), var(x))}

#   This lets me draw multiple samples and calculate both statistics on the
#   same sample:

draws <- replicate(1000, meanandvar(sample(sunspots, size, replace=FALSE)))

draw.mean <- draws[1,]
draw.var <- draws[2,]

#   Plot the sampling distributions

hist(draw.mean)
abline(v=mean.sunspots)

hist(draw.var)
abline(v=var.sunspots)

#   Estimate the bias

mean(draw.mean) 
mean(draw.mean) - mean.sunspots
mean(sqrt(draw.var)) - sqrt(var.sunspots)


## Confidence intervals

#  With the sample mean and standard deviation, it is is simple to 
#  calculate confidence intervals for the estimate of the mean.  If we want
#  a 95\% confidence interval, it is the sample mean + or - qnorm((1 - .95)/2) 
#  times the standard error.

ci.lower <- sample.mean + qnorm(.025)*sample.se
ci.upper <- sample.mean + qnorm(.975)*sample.se
c(ci.lower, ci.upper)

#  Remember that it is the interval that is random, not the true value of a 
#  parameter.  So, on average 95\% of 95\% confidence intervals will cover
#  the true value.  It is NOT correct to say that there is a 95% chance that 
#  the true value is in the interval; it either is or it isn't.  This is one
#  of the most common mistakes in interpreting statistical results.

#  We can calculate a bunch of CIs and check their coverage probability.
#  The following code creates the intervals:

draw.se <- sqrt(draw.var/size)
ci.lower <- draw.mean + qnorm(.025)*draw.se
ci.upper <- draw.mean + qnorm(.975)*draw.se
draw.ci <- cbind(ci.lower, ci.upper)

#  To check the coverage, we can see whether the lower bound is lower than the 
#  true value and the upper bound is higher than the true value:

coverage <- ci.lower < mean.sunspots & ci.upper > mean.sunspots
mean(coverage)

#  Here it is pretty good.  We aren't usually concerned if more intervals cover
#  the true value (this is called being conservative), but we would be very concerned
#  if a lot fewer intervals are covering the true value than we expect.


plot(0, 0, xlim= c(30, 70), ylim= c(0,101), type="n")
segments(x0=draw.ci[1:100,1], y0=1:100, x1=draw.ci[1:100,2], y1=1:100)
abline(v=mean.sunspots)

#  We can also calculate different confidence intervals.  Say we want 90% CIs:

ci.lower.90 <- draw.mean + qnorm(.05)*draw.se
ci.upper.90 <- draw.mean + qnorm(.95)*draw.se
draw.ci.90 <- cbind(ci.lower, ci.upper)

coverage.90 <- ci.lower.90 < mean.sunspots & ci.upper.90 > mean.sunspots
mean(coverage.90)



##   3) Bias and consistency

#    Sometimes it is very difficult to find an unbiased estimate for some 
#    quantity of interest.  Suppose we want to estimate the maximum number of 
#    sunspots in a month by sampling from the population:

max.sunspots <- max(sunspots)
max.sunspots

#    The obvious estimator for this quantity is the maximum of the sample.  
#    Unfortunately, this estimator is biased.  Think about why that might be.

size <- 36
draws.max.36 <- replicate(1000,max(sample(sunspots, size, replace=FALSE)))

mean(draws.max.36)
hist(draws.max.36, , xlim=c(100,260))
abline(v=max.sunspots)

#    But, is the sample max a consistent estimator of the population max?  That is
#    as we increase the sample size, does the sampling distribution converge on the 
#    true value of the parameter?

#    Samples of size 100

size <- 100
draws.max.100 <- replicate(1000,max(sample(sunspots, size, replace=FALSE)))

mean(draws.max.100)
hist(draws.max.100,  xlim=c(100,260))
abline(v=max.sunspots)

#    Samples of size 1000

size <- 1000
draws.max.1000 <- replicate(1000,max(sample(sunspots, size, replace=FALSE)))

mean(draws.max.1000)
hist(draws.max.1000, xlim=c(100,260))
abline(v=max.sunspots)

#    Samples of size (obs - 1); this is leave-one-out sampling

size <- obs - 1
draws.max.obs <- replicate(1000,max(sample(sunspots, size, replace=FALSE)))


mean(draws.max.obs)
hist(draws.max.obs, xlim=c(100,260))
abline(v=max.sunspots)


##  We can tile the histograms onto a single plotting device using the
#   par(mfrow=c(2,2)) command.

par(mfrow=c(2,2))
hist(draws.max.36, xlim=c(100,260))
abline(v=max.sunspots)
hist(draws.max.100, xlim=c(100,260))
abline(v=max.sunspots)
hist(draws.max.1000, xlim=c(100,260))
abline(v=max.sunspots)
hist(draws.max.obs, xlim=c(100,260))
abline(v=max.sunspots)

#   To reset the plotting device, use the following:

par(mfrow=c(1,1))

#   Or just close the plotting window.
