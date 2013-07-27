##
## hw4key.R - answer key for ps4
## gov2000 - 03 Nov 08
## m.blackwell
##


###############
## Problem 1 ##
###############

#####
# e #
#####

(.26 - .23) - 1.96 * sqrt((.11^2 + .13^2)/70)
(.26 - .23) + 1.96 * sqrt((.11^2 + .13^2)/70)


###############
## Problem 2 ##
###############

load("fulton.RData")

#####
# a #
#####

precincts$maj.black <- precincts$black > .5

mean(precincts$turnout[precincts$maj.black==1])
mean(precincts$turnout[precincts$maj.black==0])

sd(precincts$turnout)

plot(density(precincts$turnout[precincts$maj.black==1]), col="orange",
     lty=1, main="", xlab="Turnout Rate", axes=F, ylab="")
axis(side=1)
lines(density(precincts$turnout[precincts$maj.black==0]), col="red",
     lty=2)
legend(x="topleft", legend=c("majority black", "majority non-black"),
       col=c("orange","red"), lty=c(1,2), bty="n")

#####
# b #
#####

scatter.smooth(x=precincts$black, y=precincts$turnout, pch=19,
               xlab="Percent Black in precinct",
               ylab="Turnout rate")


#####
# c #
#####


scatter.smooth(x=precincts$age, y=precincts$turnout, pch=19,
               xlab="Average age in precinct",
               ylab="Turnout rate")


###############
## Problem 3 ##
###############


load("hibbs.RData")

#####
# a #
#####

ols <- function(y,x) {
  beta1 <- cov(x,y)/var(x)
  beta0 <- mean(y) - beta1*mean(x)
  return(list(intercept=beta0,slope=beta1))
}

## OR, more explcitly:


ols2 <- function(y,x) {
  beta1 <- sum((x-mean(x))*(y-mean(x)))/sum((x-mean(x))^2)
  beta0 <- mean(y) - beta1*mean(x)
  return(list(intercept=beta0,slope=beta1))
}

#####
# b #
#####

ols(y=hibbs$inflation, x=hibbs$percleft)
ols2(y=hibbs$inflation, x=hibbs$percleft)

## these two give the same answer.


unemp.mod <- ols(y=hibbs$unemployment, x=hibbs$percleft)

plot(y=hibbs$unemployment, x=hibbs$percleft, pch=19, col="orange",
     xlab="Percent Leftist Government",
     ylab="Unemployment")
abline(a=unemp.mod$intercept, b=unemp.mod$slope, col="chocolate")
identify(y=hibbs$unemployment, x=hibbs$percleft, labels=hibbs$country)

#####
# c #
#####

inf.mod <- ols(y=hibbs$inflation, x=hibbs$percleft)

plot(y=hibbs$inflation, x=hibbs$percleft, pch=19, col="orange",
     xlab="Percent Leftist Government",
     ylab="Inflation")
abline(a=inf.mod$intercept, b=inf.mod$slope, col="chocolate")
identify(y=hibbs$inflation, x=hibbs$percleft, labels=hibbs$country)

#####
# d #
#####

r.sq <- function(x,y) {
  beta1 <- cov(x,y)/var(x)
  beta0 <- mean(y) - beta1*mean(x)  
  yhat <- beta0 + beta1*x
  rss <- sum((y-yhat)^2)
  tss <- sum((y-mean(y))^2)

  r.squared <- 1- (rss/tss)
  return(r.squared)
}



r.sq(y=hibbs$unemployment, x=hibbs$percleft)
r.sq(y=hibbs$inflation, x=hibbs$percleft)
