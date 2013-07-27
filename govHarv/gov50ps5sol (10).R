
##################################

##  Gov 50 - Problem Set 5

##  Sample code

#################################


##  Problem 1(a)

pi.hat <- .53
pi.null <- .5
nObs <- 832

#   Remember that, to construct the test statistic, we want to use
#   the value of pi implied by our test statistic.

z.stat <- (pi.hat - pi.null)/sqrt(pi.null*(1-pi.null)/nObs)
z.stat

##  Problem 1(b)
#   We want the two-sided p-value:

p.val <- 1 - pnorm(z.stat) + pnorm(-z.stat)
p.val

##  Problem 1(c)
#   Remember that we need to use the estimated probability , not our
#   null hypothesis, when we calculate the standard error:

ci.lower <- pi.hat + qnorm(.025)*sqrt(pi.hat*(1-pi.hat)/nObs)
ci.upper <- pi.hat + qnorm(.975)*sqrt(pi.hat*(1-pi.hat)/nObs)
c(ci.lower, ci.upper)

##  Problem 1(d)

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

#  Part (e)

#  p-value if z = 1.58
pnorm(-1.58) + 1 - pnorm(1.58)

#  z statistic that yields 0.057

qnorm(1 - .057/2)


##  Problem 4

#  Load in the dataset and take the specified random samples:

salary <- read.csv("H:/200607.csv")
flint <- subset(salary, CAMPUS=="UM_FLINT") 
annarbor <- subset(salary, CAMPUS=="UM_ANN-ARBOR") 
annarbor.idx <- sample(1:nrow(annarbor), 1000, replace=FALSE) 
flint.idx <- sample(1:nrow(flint), 200, replace=FALSE) 
annarbor.subset <- annarbor[annarbor.idx,] 
flint.subset <- flint[flint.idx,]
#    Find the critical value for the test; we use the standard normal reference distribution because the sample
#    size is large:
qnorm(.99) 
#    It is pretty clear that the variances are not equal in the two groups, so we will not assume equal variances:
sd(annarbor.subset$FTR)
sd(flint.subset$FTR)

#  We can use the t.test function to obtain the test statistic.
t.test(annarbor.subset$FTR, flint.subset$FTR, var.equal=FALSE) 


##  Problem 5

pi.nc <- 218/508
nn.nc <- 508
pi.pa <- 248/506
nn.pa <- 506

##  The critical value for the test (ie, the boundary of the
##  rejection region) can be calculated without the data:

qnorm(.01)

##  Now we calculate the test statistic:

point.est <- pi.nc - pi.pa
point.est

stderr <- sqrt(pi.nc*(1-pi.nc)/nn.nc + pi.pa*(1-pi.pa)/nn.pa)

test.stat <- point.est/stderr
test.stat




##  Problem 6

##  Original data

satverb <- c(730, 550, 510, 570, 620, 500, 630, 480, 480, 620, 750, 720)

sat.mean <- mean(satverb)
sat.se <- sd(satverb)/sqrt(length(satverb))
test.stat <- (sat.mean - 500)/sat.se
test.stat

1 - pt(test.stat, 11)

##  Recentered data

sat.recenter <- satverb - 500
recenter.mean <- mean(sat.recenter)
recenter.se <- sd(sat.recenter)/sqrt(length(sat.recenter))
recenter.test.stat <- recenter.mean/recenter.se
recenter.test.stat

1 - pt(recenter.test.stat, 11)
