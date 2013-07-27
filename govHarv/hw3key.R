##
## hw3key.R - answer key for ps3
## gov2000 - 15 Oct 08
## m.blackwell
##

###############
## Problem 1 ##
###############

load("fulton.RData")
head(fulton)
names(fulton)
summary(fulton)

#####
# a #
#####

mean(fulton$turnout)

#####
# b #
#####

set.seed(12345)

n.registered <- nrow(fulton)
sample.size <- 100

## first we randomly draw people (row numbers)

polled <- sample(x = 1:n.registered, size = sample.size, replace = FALSE)

## then we grab those rows of whom we polled

mypoll <- fulton[polled,]



samp.trate <- mean(mypoll$turnout)
samp.sd    <- sd(mypoll$turnout)



upper95 <- samp.trate + 1.96*samp.sd/sqrt(sample.size)
lower95 <- samp.trate - 1.96*samp.sd/sqrt(sample.size)

## what's the z value for CI_99? We can use qnorm

z99 <- qnorm((1-.99)/2, lower.tail=FALSE)
z99

upper99 <- samp.trate + z99*samp.sd/sqrt(sample.size)
lower99 <- samp.trate - z99*samp.sd/sqrt(sample.size)

## we could also write a function to do this:
  
conf.intervals <- function(means, sds, n, level) {
  z <- qnorm((1-level)/2, lower.tail=FALSE)
  upper <- means + z*sds/sqrt(n)
  lower <- means - z*sds/sqrt(n)
  return(list(lower=lower, upper=upper))
}

conf.intervals(samp.trate, samp.sd, sample.size, .95)
conf.intervals(samp.trate, samp.sd, sample.size, .99)


#####
# c #
#####

b.poll <- mypoll[mypoll$black==1,]
n.b <- nrow(b.poll)

xbar.b <- mean(b.poll$turnout)
shat.b <- sd(b.poll$turnout)

xbar.b - 1.96*shat.b/sqrt(n.b)
xbar.b + 1.96*shat.b/sqrt(n.b)


nb.poll <- mypoll[mypoll$black==0,]
n.nb <- nrow(nb.poll)

xbar.nb <- mean(nb.poll$turnout)
shat.nb <- sd(nb.poll$turnout)

xbar.nb - 1.96*shat.nb/sqrt(n.nb)
xbar.nb + 1.96*shat.nb/sqrt(n.nb)


#####
# e #
#####


sims <- 10000
n <- 100

rates <- vector(length=sims)
sds   <- vector(length=sims)

for (i in 1:sims) {
  poll <- sample(1:nrow(fulton), size=n, replace=FALSE)
  rates[i] <- mean(fulton$turnout[poll])
  sds[i]   <-   sd(fulton$turnout[poll])
}

cis <- conf.intervals(rates, sds, n, .95)

hist(rates, col="chocolate", border="white", xlab="turnout rates", main="")

#####
# f #
#####


mean((cis$lower < mean(fulton$turnout)) & (cis$upper > mean(fulton$turnout)))

ruler <- 1:100

plot(x=rates[ruler], y=ruler, pch=19, col="orange", xlim=c(0,1),
     xlab="turnout rates", ylab="confidence intervals")
segments(x0=cis$lower[ruler], x1=cis$upper[ruler], y0=ruler, y1=ruler, col="orange")
abline(v=mean(fulton$turnout))

#####
# g #
#####

qnorm(.01)
qnorm(.96)


asym.lowers <- rates +qnorm(.01)*sds/sqrt(n)
asym.uppers <- rates +qnorm(.96)*sds/sqrt(n)

mean((asym.lowers < mean(fulton$turnout)) & (asym.uppers > mean(fulton$turnout)))

plot(x=rates[ruler], y=ruler, pch=19, col="orange", xlim=c(0,1),
     xlab="turnout rates", ylab="confidence intervals")
segments(x0=asym.lowers[ruler], x1=asym.uppers[ruler], y0=ruler, y1=ruler, col="orange")
abline(v=mean(fulton$turnout))

###############
## Problem 2 ##
###############

#####
# a #
#####

set.seed(12345)
n <- 500

tosample <- fulton$turnout[sample(1:nrow(fulton), size=n, replace=FALSE)]

xbar <- mean(tosample)
sehat <- sd(tosample)/sqrt(n)

t.stat <- (xbar-.5)/sehat
t.stat

#####
# b #
#####

qnorm(.05)
qnorm(.01)

#####
# c #
#####

pnorm(t.stat)

###############
## Problem 3 ##
###############

## setting up the problem. 

null.b <- .15
null.w <- .75

xbar.b <- .25
xbar.w <- .68

sd.b <- .177
sd.w <- .22

n <- 50000

## test statistics for black and white

t.b <- (xbar.b-null.b)/(sd.b/sqrt(n))
t.w <- (xbar.w-null.w)/(sd.w/sqrt(n))

t.b
t.w

## p-values

2*(1-pnorm(abs(t.b)))
2*(1-pnorm(abs(t.w)))
