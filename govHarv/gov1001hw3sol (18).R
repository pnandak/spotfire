
##########################################

#    Sample code for HW 3

##########################################

##   Problem 1

#    a)
qnorm(.99)

#    b)
qnorm(.95)

#    c) 
qnorm(.75)

#    d)
qnorm(.99865)


##    Problem 2

mean.p2 <- 20.3
sd.p2 <- 18.2
n.p2 <- 1415
se.p2 <- sd.p2/sqrt(n.p2)

ci.lower <- mean.p2 + qnorm(.005)*se.p2
ci.upper <- mean.p2 + qnorm(.995)*se.p2
c(ci.lower, ci.upper)

##    Problem 3

#     If pi equals 0.1:
(.1)*(.9)*(1.96/.02)^2

#     If pi equals 0.5:
(.5)*(.5)*(1.96/.02)^2


##   Problem 6

#    Load the salary dataset
salary <- read.csv("C:/200607.csv")
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
size <- 36

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



#   Calculate the sampling distributions for the sample mean and 
#   variance:

draw.mean <- numeric()
draw.sd <- numeric()

for(ii in 1:1000){
idx <- sample(1:obs, size, replace=FALSE)
draw.mean[ii] <- mean(ftr[idx])
draw.sd[ii] <- sd(ftr[idx])
}

#   Plot the sampling distributions

hist(draw.mean, main="Sampling distribution for mean, n=36", xlab="Dollars", freq=FALSE)
abline(v=mean(ftr))

hist(draw.sd, main="Sampling distribution for sd, n=36", xlab="Dollars",freq=FALSE)
abline(v=sd(ftr))


##   Estimate the mean squared error

estimation.error <- draw.mean - mean(ftr)
hist(estimation.error)

#    Then we square the estimation error:

squared.error <- estimation.error^2
hist(squared.error)

#    And finally, we take the mean of the squared errors

mse <- mean(squared.error)
mse

#    Of course, we can do this all in one step:

mse.sd <- mean((draw.sd - sd(ftr))^2)
mse.sd
