
#
# Fun with the normal distribution and hints for Home Work 3 Question 3.
#

#
# The normal cumulative distribution function is pnorm().  It takes three arguments:
#
# 1) "q": the quantile of interest.  pnorm() will return the
#         cumulative density up to "q"
# 2) "mean" the mean of the normal distribution (by default this is zero)
# 3) "sd": the standard deviation (which is the square root of the
#          variance) of the normal distribution


#how much of the standard normal density is contained in q=1.96?
pnorm(1.96,mean=0,sd=1)

#how much of the standard normal density is contained in q=-1.96?
pnorm(-1.96,mean=0,sd=1)

# The quantile function returns the quantile of the normal
# distribution which corresponds to the probability requested.  The
# function is named "qnorm()" and takes three arguments:
#
# 1) "p" the probability of interest
# 2) "mean" the mean of the normal distribution (by default this is zero)
# 3) "sd": the standard deviation (which is the square root of the
#          variance) of the normal distribution

#what quantile of the standard normal corresponds to the probability of .975?
qnorm(.975,mean=0,sd=1)


#what quantile of the standard normal corresponds to the probability of .95?
qnorm(.95,mean=0,sd=1)


#
# This example is from Question 3 from Home Work #3
#

#what quantile of the normal with mean=0.05 and variance=26
#corresponds to the probability of .975?
qnorm(.975,mean=0.05, sd=sqrt(26))

#what quantile of the normal with mean=0.05 and variance=26
#corresponds to the probability of .025?
qnorm(.025,mean=0.05, sd=sqrt(26))

#what quantile of the normal with mean=0.05 and variance=26
#corresponds to the probability of .95?
qnorm(.95,mean=0.05, sd=sqrt(26))

#what quantile of the normal with mean=0.05 and variance=26
#corresponds to the probability of .05?
qnorm(.05,mean=0.05, sd=sqrt(26))


#
#60% of the normal density with mean=0.05 and variance=26 out to be
#within the following two points:
#

#what quantile of the normal with mean=0.05 and variance=26
#corresponds to the probability of .8?
qnorm(.8,mean=0.05, sd=sqrt(26))

#what quantile of the normal with mean=0.05 and variance=26
#corresponds to the probability of .2?
qnorm(.2,mean=0.05, sd=sqrt(26))


#
#What proportion of the observed change in approval series is
#actually within these two points?
#

#
#obtain the data
#
data.approval <- read.table(file="http://www.courses.fas.harvard.edu/~gov1000/Data/approval.asc",header=T)

#the change in approval variable as requested in Question 3
change.approval  <- data.approval$approval-data.approval$lagApproval

#as before, let's just look at it's histogram
hist(change.approval)

#let's check its mean and variance.  Both of these are close to the
#values presented in Question #3
mean(change.approval)
var(change.approval)

#how many observations do we have:
observations  <- length(change.approval)
cat("number of observations:",observations,"\n")

#how many of the observation are above or equal to qnorm(.8,mean=0.05, sd=sqrt(26))
above  <- sum(change.approval >= qnorm(.8,mean=0.05, sd=sqrt(26)))
cat("number above:", above,"\n")
above.proportion  <- above/observations;
cat("proportion above:", above.proportion,"\n")

#how many of the observation are below or equal to qnorm(.2,mean=0.05, sd=sqrt(26))
below  <- sum(change.approval <= qnorm(.2,mean=0.05, sd=sqrt(26)))
cat("number below:", below,"\n")
below.proportion  <- below/observations;
cat("proportion below:", below.proportion,"\n")
