
##  Problem 3

#   Begin by loading the salary data back into your workspace:

salary <- read.csv("C:/200607.csv")
ftr <- salary$FTR

#   I want to be able to recreate my results, so I set the 
#   random number generator:

set.seed(5413426)


#   Now draw a random sample of 1000 observations from the FTR
#   variable:

idx <- sample(1:length(ftr), size = 1000, replace=FALSE)
ftr.subset <- ftr[idx]

#   Calculate summary descriptive statistics:

mean(ftr.subset)
median(ftr.subset)
min(ftr.subset)
max(ftr.subset)
sd(ftr.subset)
length(ftr.subset)

sample.stats <- c(mean(ftr.subset), median(ftr.subset), min(ftr.subset), max(ftr.subset), sd(ftr.subset), length(ftr.subset))
pop.stats <- c(mean(ftr), median(ftr), min(ftr), max(ftr), sqrt((length(ftr)-1)/length(ftr))*sd(ftr), length(ftr))

cbind(sample.stats, pop.stats)


#   To calculate a 95\% confidence interval using our standard approximation, 
#   we first need to calculate the sample mean and standard deviation

sample.mean <- mean(ftr.subset)
sample.sd <- sd(ftr.subset)

#   Strictly speaking, we should use a t reference distribution to construct
#   our confidence intervals, since we have to estimate the standard deviation.
#   A t with 999 degrees of freedom is so close to standard normal that
#   it really doesn't matter:

qnorm(.975, 0, 1)
qt(.975, df=999)

ci.lower <- sample.mean + qt(.025, 999)*sample.sd/sqrt(1000)
ci.upper <- sample.mean + qt(.975, 999)*sample.sd/sqrt(1000)
c(ci.lower, ci.upper)


##  Problem 5


#   This defines the number of bootstrap samples to take; I'm going
#   to take a lot because I don't want too much simulation error:

bootSamples <- 10000 

#   This defines how many observations we want in each sample; it is
#   equal to the number of observations in our dataset:

nObs <- length(ftr.subset)

#   This creates the holder for our bootstrapped means

draws.mean <- numeric()

#   And now we have the for loop

for(ii in 1:bootSamples){
idx <- sample(1:nObs, size=nObs, replace=TRUE)
draws.mean[ii] <- mean(ftr.subset[idx])
}

#   So, how do we construct the confidence interval?  We take the (alpha/2)th
#   and (1 - alpha/2)th quantiles of our bootstrapped samples:

quantile(draws.mean, prob=c(.025, .975))

abs(quantile(draws.mean, prob=c(.025, .975)) - mean(ftr.subset))

##  Problem 6

#   Now, we want to construct a 95% confidence interval for the median.  In our
#   sample, the sample median is 

median(ftr.subset)


#   First, we have to order the observations from smallest to largest:

ftr.ordered.subset <- sort(ftr.subset)

#   Then, we need to find the ordered observation corresponding to the 
#   ((n + 1)/2 - sqrt(n))th observation and the ((n + 1)/2 + sqrt(n))th
#   observation.

nObs <- length(ftr.subset)
lower.subscript <- floor((nObs + 1)/2 - sqrt(nObs))
upper.subscript <- ceiling((nObs + 1)/2 + sqrt(nObs))

ftr.ordered.subset[lower.subscript]
ftr.ordered.subset[upper.subscript]

#   So, the confidence interval that we estimate from our data is 

c(ftr.ordered.subset[lower.subscript], ftr.ordered.subset[upper.subscript])

#   Is it symmetric?

abs(c(ftr.ordered.subset[lower.subscript], ftr.ordered.subset[upper.subscript]) - median(ftr.subset))

#   Which in this case does contain the true population median.


#   Problem 6

#   Now we want to construct a bootstrapped confidence interval for the median.
#   We can re-use almost all of the code from problem 4, except we calculate the
#   median in each of the bootstrapped samples rather than the mean.


bootSamples <- 10000 

#   This defines how many observations we want in each sample; it is
#   equal to the number of observations in our dataset:

nObs <- length(ftr.subset)

#   This creates the holder for our bootstrapped means

draws.median <- numeric()

#   And now we have the for loop

for(ii in 1:bootSamples){
idx <- sample(1:nObs, size=nObs, replace=TRUE)
draws.median[ii] <- median(ftr.subset[idx])
}

#   So, how do we construct the confidence interval?  We take the (alpha/2)th
#   and (1 - alpha/2)th quantiles of our bootstrapped samples:

quantile(draws.median, prob=c(.025, .975))



##   Problem 8

#    Now, we need to write a function that will calculate the 90-10 ratio.
#    This is almost the same as the example from the section notes, except 
#    we calculate the 10th and 90th percentiles and divide rather than subtract:


rat9010 <- function(x){
out <- quantile(x, p=.90)/quantile(x, p=.10)
return(out)
}

#    The population 90-10 ratio:

rat9010(ftr)

#    The 90-10 ratio in our sample:

rat9010(ftr.subset)


##   Problem 9

#   Now, let's calculate a bootstrapped confidence interval for the 90-10 ratio, using our 
#   sample of the ftr data.

bootSamples <- 10000 

#   This defines how many observations we want in each sample; it is
#   equal to the number of observations in our dataset:

nObs <- length(ftr.subset)

#   This creates the holder for our bootstrapped 90-10 ratios:

draws.9010 <- numeric()

#   And now we have the for loop

for(ii in 1:bootSamples){
idx <- sample(1:nObs, size=nObs, replace=TRUE)
draws.9010[ii] <- rat9010(ftr.subset[idx])
}

#   So, how do we construct the confidence interval?  We take the (alpha/2)th
#   and (1 - alpha/2)th quantiles of our bootstrapped samples:

quantile(draws.9010, prob=c(.025, .975))

#   Is it symmetric?

abs(quantile(draws.9010, prob=c(.025, .975)) - rat9010(ftr.subset))
