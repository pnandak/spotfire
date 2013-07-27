
##########################

##  Gov 50 PS 2

##  Sample code

##########################


## Problem 4

se5 <- .5/sqrt(5)
se100 <- .5/sqrt(100)
se1000 <- .5/sqrt(1000)
se10000 <- .5/sqrt(10000)

c(.5 - 2*se5, .5 + 2*se5)
c(.5 - 2*se100, .5 + 2*se100)
c(.5 - 2*se1000, .5 + 2*se1000)
c(.5 - 2*se10000, .5 + 2*se10000)

## Part b

means5 <- replicate(1000, mean(rbinom(5, 1, .5)))
means100 <- replicate(1000, mean(rbinom(100, 1, .5)))
means1000 <- replicate(1000, mean(rbinom(1000, 1, .5)))
means10000 <- replicate(1000, mean(rbinom(10000, 1, .5)))

mean(means5 > .5 - 2*se5 & means5 < .5 + 2*se5)
mean(means100 > .5 - 2*se100 & means100 < .5 + 2*se100)
mean(means1000 > .5 - 2*se1000 & means1000 < .5 + 2*se1000)
mean(means10000 > .5 - 2*se10000 & means10000 < .5 + 2*se10000)

## Part c

par(mfrow=c(2,2))
hist(means5, xlim=c(0,1), main="Sampling distribution for pi.hat, n=5", xlab=expression(hat(pi)))
hist(means100, xlim=c(0,1), main="Sampling distribution for pi.hat, n=100", xlab=expression(hat(pi)))
hist(means1000, xlim=c(0,1), main="Sampling distribution for pi.hat, n=1000", xlab=expression(hat(pi)))
hist(means10000, xlim=c(0,1), main="Sampling distribution for pi.hat, n=10000", xlab=expression(hat(pi)))



## Problem 5

1 - pnorm(2.5)
1 - pnorm(42, 37, 6)
dbinom(10, 10, .75)
qunif((1 - .43), 0, 2)
pnorm(6,3, 2) - pnorm(4, 3, 2)