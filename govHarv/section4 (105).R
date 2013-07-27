############################################

#   Goverment 1001
#   Section 4 - More Confidence Intervals
#   March 1, 2007
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################


#   Goals for today:

#   1) Review of probability distributions

#   2) Confidence interval for the median

#   3) Bootstrapped confidence intervals

#   4) Writing functions in R

############################################


#   1) Review of probability distributions

#   How do we calculate the probability that X is larger than 
#   some number when we know the distribution of X?

#   How do we calculate the probability that |X| is larger than some 
#   number when we know the distribution of X?

#   How do we calculate the z-score that we use in our confidence interval?


#   2) Confidence interval for the median

#   We are going to use the same subspots data that we used last week.

data(sunspots)

#   But instead of using the whole dataset, we are going to randomly pick 
#   a subset of size 225 and call that our "observed data":

idx <- sample(1:length(sunspots), size = 225, replace=FALSE)
sunspots.subset <- sunspots[idx]
sunspots.subset

#   The population median in sunspots is 

median(sunspots)

#   but the sample median is 

median(sunspots.subset)

#   These are pretty close; but how close?  Let's say we want a 95% confidence
#   interval for our estimate, so that 95% of the intervals that we generate
#   in this way will cover the true value.  The book gives a formula for this.

#   First, we have to order the observations from smallest to largest:

sunspots.subset <- sort(sunspots.subset)
sunspots.subset

#   Then, we need to find the ordered observation corresponding to the 
#   ((n + 1)/2 - sqrt(n))th observation and the ((n + 1)/2 + sqrt(n))th
#   observation.

nObs <- length(sunspots.subset)
lower.subscript <- floor((nObs + 1)/2 - sqrt(nObs))
upper.subscript <- ceiling((nObs + 1)/2 + sqrt(nObs))

sunspots.subset[lower.subscript]
sunspots.subset[upper.subscript]

#   So, the confidence interval that we estimate from our data is 

c(sunspots.subset[lower.subscript], sunspots.subset[upper.subscript])

#   Which in this case does contain the true population median.

#   3) Bootstrapped confidence intervals

#   We've talked about analytic formulas for confidence intervals for the
#   mean, for proportions, and for the median, but it is a pain to have to 
#   remember the formula for each quantity of interest, and there may be no 
#   good formula to use.

#   An alternative that we can often use is bootstrapping in order to create
#   confidence intervals.  Bootstrapping only requires the data that we have, 
#   not the entire population as we used last week to calculate the sampling
#   distribution of various statistics.  What we do in bootstrapping is 
#   to sample WITH REPLACEMENT from the data that we observe a whole bunch of 
#   times in order to construct fake datasets.  We then calculate our statistic
#   of interest in each of the fake datasets and use those to calculate the 
#   confidence interval.

#   As an example, we will calculate a bootstrapped confidence interval for the 
#   mean using the subset of the sunspot data that we constructed in the previous
#   section:

#   This defines the number of bootstrap samples to take:

bootSamples <- 1000 

#   This defines how many observations we want in each sample; it is
#   equal to the number of observations in our dataset:

nObs <- length(sunspots.subset)

#   This creates the holder for our bootstrapped means

draws.mean <- numeric()

#   And now we have the for loop

for(ii in 1:bootSamples){
idx <- sample(1:nObs, size=nObs, replace=TRUE)
draws.mean[ii] <- mean(sunspots.subset[idx])
}

#   So, how do we construct the confidence interval?  We take the (alpha/2)th
#   and (1 - alpha/2)th quantiles of our bootstrapped samples:

quantile(draws.mean, prob=c(.025, .975))

#   4) Writing functions in R

#   One of the nice features of R is that you can write your own functions
#   to automate tasks that you have to do over and over in different contexts.
#   
#   In order to write a function, you need to use the function() function (cue 
#   bad Seseme Street-style joke).  The function() function takes as arguments 
#   dummy objects that correspond to the things that you will want to put in
#   to the function when you actually use it.

function(x){
print("hi!")
}

#   In order to use the function, you need to save it as an object, just like 
#   you would a vector of numbers or a dataset.  You can call it whatever you
#   want, but avoid the names of other functions in R.

hi.function <- function(x){
print("hi!")
}

hi.function(1)
hi.function("goodbye")

#   So, hi.function() is a pretty stupid function; no matter what we put into 
#   it, it always does the same thing.

#   Let's say that we want to create a function that calculates the inter-quartile
#   range.  Remember that the IQR is a measure of the dispersion of some variable.
#   First, we calculate, the upper and lower quartiles:

quantile(sunspots, p=.75)
quantile(sunspots, p=.25)

#   The interquartile range is just the difference between these two numbers:

quantile(sunspots, p=.75) - quantile(sunspots, p=.25)

#   Now, let's put this in a function:

iqr <- function(x){
quantile(x, p=.75) - quantile(x, p=.25)
}

#   What did we do?  We defined a function iqr that takes one argument, x.
#   Then, we calculated the 75th and 25th percentile for the vector x and took ]
#   the difference between them.  So, now if we use

iqr(sunspots)

#   We get the same answer as we got before. But, now we can use our iqr() 
#   function on any numeric variable.

#   Let's calculate a bootstrapped confidence interval for the iqr, using our 
#   sample of the sunspots data.

bootSamples <- 1000 

#   This defines how many observations we want in each sample; it is
#   equal to the number of observations in our dataset:

nObs <- length(sunspots.subset)

#   This creates the holder for our bootstrapped means

draws.iqr <- numeric()

#   And now we have the for loop

for(ii in 1:bootSamples){
idx <- sample(1:nObs, size=nObs, replace=TRUE)
draws.iqr[ii] <- iqr(sunspots.subset[idx])
}

#   So, how do we construct the confidence interval?  We take the (alpha/2)th
#   and (1 - alpha/2)th quantiles of our bootstrapped samples:

quantile(draws.iqr, prob=c(.025, .975))


