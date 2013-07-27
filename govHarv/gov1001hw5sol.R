
##  Problem 2(a)

pi.hat <- .53
pi.null <- .5
nObs <- 832

#   Remember that, to construct the test statistic, we want to use
#   the value of pi implied by our test statistic.

z.stat <- (pi.hat - pi.null)/sqrt(pi.null*(1-pi.null)/nObs)
z.stat

##  Problem 2(b)
#   We want the two-sided p-value:

p.val <- 1 - pnorm(z.stat) + pnorm(-z.stat)
p.val

##  Problem 2(c)
#   Remember that we need to use the estimated probability , not our
#   null hypothesis, when we calculate the standard error:

ci.lower <- pi.hat + qnorm(.025)*sqrt(pi.hat*(1-pi.hat)/nObs)
ci.upper <- pi.hat + qnorm(.975)*sqrt(pi.hat*(1-pi.hat)/nObs)
c(ci.lower, ci.upper)

##  Problem 2(d)

#   Now, we repeat the analysis using just the Black subsample:

pi.hat <- .66
pi.null <- .5
nObs <- 299

z.stat <- (pi.hat - pi.null)/sqrt(pi.null*(1-pi.null)/nObs)
z.stat

p.val <- 1 - pnorm(z.stat) + pnorm(-z.stat)
p.val

ci.lower <- pi.hat + qnorm(.025)*sqrt(pi.hat*(1-pi.hat)/nObs)
ci.upper <- pi.hat + qnorm(.975)*sqrt(pi.hat*(1-pi.hat)/nObs)
c(ci.lower, ci.upper)

##  Problem 3

#   In order to conduct a power analysis, the first thing that we 
#   need to do is to calculate the critical value for the test when
#   the null hypothesis is true.   In this case, we are testing the 
#   null against the implied alternative that pi > 0.6, so we want
#   a one-sided test.

pi.null <- .6
nObs <- 100
var.null <- pi.null*(1 - pi.null)
se.null <- sqrt(var.null/nObs)
se.null

#   pi.null and se.null give us the mean and standard deviation of the
#   sampling distribution, which is approximately normal with a large 
#   enough sample size.

crit.val <- qnorm(.95, pi.null, se.null)
crit.val

##  Note: it is also possible to construct the critical value
#   this way, analogous to a confidence interval, except you
#   don't need any data:

crit.val2 <- pi.null + qnorm(.95, 0, 1)*se.null
crit.val2

#   Now, to determine the power, we need to construct the sampling
#   distribution for the alternative values of pi, and find the 
#   probability of obtaining a draw larger than the critical value:


pi.alt <- .65 
var.alt <- pi.alt*(1-pi.alt)
se.alt <- sqrt(var.alt/nObs)
se.alt

pnorm(crit.val, pi.alt, se.alt, lower.tail=TRUE)


pi.alt <- .7 
var.alt <- pi.alt*(1-pi.alt)
se.alt <- sqrt(var.alt/nObs)
se.alt

pnorm(crit.val, pi.alt, se.alt, lower.tail=TRUE)


pi.alt <- .75 
var.alt <- pi.alt*(1-pi.alt)
se.alt <- sqrt(var.alt/nObs)
se.alt

pnorm(crit.val, pi.alt, se.alt, lower.tail=TRUE)

#  Now, we repeat the analysis with a larger sample size:

pi.null <- .6
nObs <- 1000
var.null <- pi.null*(1 - pi.null)
se.null <- sqrt(var.null/nObs)
se.null

#  Get the new critical value:

crit.val <- qnorm(.95, pi.null, se.null)
crit.val


pi.alt <- .65 
var.alt <- pi.alt*(1-pi.alt)
se.alt <- sqrt(var.alt/nObs)
se.alt

pnorm(crit.val, pi.alt, se.alt, lower.tail=TRUE)


pi.alt <- .7 
var.alt <- pi.alt*(1-pi.alt)
se.alt <- sqrt(var.alt/nObs)
se.alt

pnorm(crit.val, pi.alt, se.alt, lower.tail=TRUE)


pi.alt <- .75 
var.alt <- pi.alt*(1-pi.alt)
se.alt <- sqrt(var.alt/nObs)
se.alt

pnorm(crit.val, pi.alt, se.alt, lower.tail=TRUE)


#  Problem 5

#  Part (g)

#  p-value if z = 1.58
pnorm(-1.58) + 1 - pnorm(1.58)

#  z statistic that yields 0.057

qnorm(1 - .057/2)