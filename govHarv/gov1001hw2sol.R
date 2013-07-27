
##  This command allows me to get the same simulation results each time I run the whole code; it resets
##  the random number generator in R.

set.seed(98347512)

##  Problem 4

##  You don't have to do this problem in R, but you need the standard errors for the next problem

se.10 <- .5/sqrt(10)
se.100 <- .5/sqrt(100)
se.1000 <- .5/sqrt(1000)
se.10000 <- .5/sqrt(10000)


## Problem 5

## Once you have the basic looping structure written, you just have to 
## change the size of the sample that you are drawing in each simulation

## n=10

sims <- 1000
nn <- 10
draws.10 <- rep(0, sims)
for(ii in 1:sims){
xx <- rbinom(nn, size=1, p=.5)
draws.10[ii] <- mean(xx)
}

## n=100

sims <- 1000
nn <- 100
draws.100 <- rep(0, sims)
for(ii in 1:sims){
xx <- rbinom(nn, size=1, p=.5)
draws.100[ii] <- mean(xx)
}

## n=1000

sims <- 1000
nn <- 1000
draws.1000 <- rep(0, sims)
for(ii in 1:sims){
xx <- rbinom(nn, size=1, p=.5)
draws.1000[ii] <- mean(xx)
}

## n=10000

sims <- 1000
nn <- 10000
draws.10000 <- rep(0, sims)
for(ii in 1:sims){
xx <- rbinom(nn, size=1, p=.5)
draws.10000[ii] <- mean(xx)
}

##  Now, we can calculate how many draws fell within two standard errors
##  of the population mean.

mean( draws.10 > (.5 - 2*se.10) & draws.10 < (.5 + 2*se.10))

mean( draws.100 > (.5 - 2*se.100) & draws.100 < (.5 + 2*se.100))

mean( draws.1000 > (.5 - 2*se.1000) & draws.1000 < (.5 + 2*se.1000))

mean( draws.10000 > (.5 - 2*se.10000) & draws.10000 < (.5 + 2*se.10000))

## Problem 6

##  It is probably easier to compare if all of the graphs are on the 
##  same plot:

par(mfrow=c(2,2))
hist(draws.10, xlim=c(0,1), xlab="Sample mean", main="Sampling distribution of the mean, \n n=10")
hist(draws.100, xlim=c(0,1), xlab="Sample mean", main="Sampling distribution of the mean, \n n=100")
hist(draws.1000, xlim=c(0,1), xlab="Sample mean", main="Sampling distribution of the mean, \n n=1000")
hist(draws.10000, xlim=c(0,1), xlab="Sample mean", main="Sampling distribution of the mean, \n n=10000")
par(mfrow=c(1,1))




## Problem 7

## (a)

1 - pnorm(3, mean=0, sd=1)

## (b)

1 - pnorm(42, mean=35, sd=6)

## (c)

dbinom(10, 10, .8)

## (d) 

punif(.9, 0, 1)

## (e)

1 - pchisq(6.5, 2)


## Problem 8

1 - pf(1^2, 1, 3)
pt(-1*1, 3) + (1 - pt(1,3))

1 - pf(2^2, 1, 3)
pt(-1*2, 3) + (1 - pt(2,3))

1 - pf(3^2, 1, 3)
pt(-1*3, 3) + (1 - pt(3,3))

1 - pf(4^2, 1, 3)
pt(-1*4, 3) + (1 - pt(4,3))

1 - pf(5^2, 1, 3)
pt(-1*5, 3) + (1 - pt(5,3))

## A quicker way to do the same thing:

cc <- 1:5
1 - pf(cc^2, 1, 3)
pt(-1*cc, 3) + (1 - pt(cc,3))