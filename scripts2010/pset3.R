##########################################################################
## Problem Set 3 Solutions
## October 13, 2009
## Gov 2000
## prepared by maya sen
##########################################################################

load("subprime.RData")
    ## loads data

## To get a feel for the data
head(subprime)
dim(subprime)
summary(subprime)

## also opening up the xtables library
library(xtable)

##################################
## Problem One	              ##
##################################

## see answers provided in the solution key.

##################################
## Problem Two	              ##
##################################


#######
## A ##
#######

truth <- mean(subprime$high.rate)
truth
hist(subprime$high.rate, xlab = "", ylab = "", main = "subpriime lending")

#######
## B ##
#######

set.seed(12345)
n.obs <- nrow(subprime)
    ## takes the number of rows
N <- 250
    ## takes a sample of 250

rand.rows <- sample(x=1:n.obs, size=N, replace=FALSE)
    ## samples 2500 rows
rand.rows

## Now that we have the random rows, we can grab those random rows
## from the actual data and put it into  a new matrix:
mysamp <- subprime[rand.rows,]
    ## subsets the data according to the
    ## randomly selected rows

mean(mysamp$high.rate)

mu.b <- mean(mysamp$high.rate)
sd.b <-sd(mysamp$high.rate)

mu.b
sd.b

## we'll just use the formulas for confidence intervals

mu.b + 1.96*sd.b/sqrt(N)
mu.b - 1.96*sd.b/sqrt(N)

## what's the z value for CI_50? for CI_99? We can use qnorm

z50 <- qnorm((1-.5)/2, lower.tail=FALSE)
z50

z99 <- qnorm((1-.99)/2, lower.tail=FALSE)
z99

mu.b + z50*sd.b/sqrt(N)
mu.b - z50*sd.b/sqrt(N)

mu.b + z99*sd.b/sqrt(N)
mu.b - z99*sd.b/sqrt(N)

#######
## C ##
#######

## To calculate the margin of error:
1.96*sd.b/sqrt(N)

#######
## D ##
#######

## We're going to pretend we have 10,000 independent researchers out in
## the field 250 polling home owners. Each will and calculate the mean
## and sd and 95% CI and then report back

set.seed(12345)
N <- 250
researchers <- 10000
samp.means <- vector(length=length(researchers))

samp.sds   <- vector(length=length(researchers))

for (i in 1:researchers) {

  samp.rows <- sample(x=1:n.obs, size=N, replace=FALSE)
  samp <- subprime[samp.rows,]

  samp.means[i] <- mean(samp$high.rate)
  samp.sds[i]   <-   sd(samp$high.rate)
}

ci.upper <- samp.means + 1.96*samp.sds/sqrt(N)
ci.lower <- samp.means - 1.96*samp.sds/sqrt(N)

## And then to plot the sampling distribution

## pdf(file= "myPlot.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(density(samp.means), main = "density, subprime lending rate")
## dev.off()

#######
## E ##
#######

## To create a nice-looking plot:

set <- cbind(samp.means, samp.sds, ci.upper, ci.lower)
set <- set[sample(nrow(set),100),]
    ## subsets data

ruler <- seq(1,100,1)
## pdf(file= "myPlot2.pdf", width = 5, height = 5, family = "Helvetica", pointsize = 10)
plot(x=set[ruler], y=ruler, pch=19, col="orange", xlim=c(0,.50),
     xlab="subprime lending rate", ylab="confidence intervals")
segments(x0=ci.lower[ruler], x1=ci.upper[ruler], y0=ruler, y1=ruler, col="orange")
abline(v=mean(subprime$high.rate))
## dev.off()


## And finally to calculate the proportion of confidence
## intervals that contain the true subprime lending rate

mean(truth >= set[,4] & truth <= set[,3])

##################################
## Problem Three	              ##
##################################


#######
## A ##
#######

set.seed(12345)
n.obs <- nrow(subprime)
    ## takes the number of rows
N <- 100
    ## takes a sample of 100

rand.rows <- sample(x=1:n.obs, size=N, replace=FALSE)
    ## samples 100 rows
rand.rows

mysamp <- subprime[rand.rows,]
	## subsets data 


## now to create a nice looking table:
prob3 <- matrix(data = NA, nrow = 2, ncol = 2)
colnames(prob3) <- c("Average Amount", "standard dev")
rownames(prob3) <- c("men", "women")

prob3[1,1] <- mean(mysamp$loan.amount[mysamp$woman == 0])
prob3[1,2] <- sd(mysamp$loan.amount[mysamp$woman == 0])
prob3[2,1] <- mean(mysamp$loan.amount[mysamp$woman == 1])
prob3[2,2] <- sd(mysamp$loan.amount[mysamp$woman == 1])

xtable(prob3)

#######
## B ##
#######

## to derive the test statistic

diff <- mean(mysamp$loan.amount[mysamp$woman == 0]) - mean(mysamp$loan.amount[mysamp$woman == 1])
n.m <- length(mysamp$loan.amount[mysamp$woman == 0])
n.w <- length(mysamp$loan.amount[mysamp$woman == 1])

sd.m <- sd(mysamp$loan.amount[mysamp$woman == 0])
sd.w <- sd(mysamp$loan.amount[mysamp$woman == 1])

denom <- sqrt(sd.m^2/n.m + sd.w^2/n.w)

test <- (diff - 0)/denom
test

#####
# b #
#####

qnorm(1-.05/2)
qnorm(.05/2)

qnorm(1-.01/2)
qnorm(.01/2)

#####
# c #
#####

ruler <- seq(-4,4,.1)

##pdf(file= "problem3.pdf", width = 5, height = 5, family = "Helvetica",
pointsize = 10)
plot(ruler,(dnorm(ruler)), type = "l", xlab = "", ylab = "",
	main = "Rejection Regions Based on a One-Sided Hypothesis Test")
legend(x = "topleft", legend = c("Alpha = .05", "Alpha = .01", "Test Stat"), lwd = 2, col = c("darkgreen", "purple", "black"), bty = "n")
abline(v = test)
abline(v = c(qnorm(1-.05/2),qnorm(.05/2)), col = "darkgreen")
abline(v = c(qnorm(1-.01/2),qnorm(.01/2)), col = "purple")
##dev.off()

