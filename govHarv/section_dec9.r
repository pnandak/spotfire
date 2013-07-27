## Gov 2000 section, December 9, 2008
## A.K.A. last but not least

## Diagnostics and corrections

## Let's load some data about credit cards
## This data file ends in '.dta', indicating it's a stata file
## To load it into R, we need to install and load the foreign library

## I've already installed it, so I can use

library(foreign)

cc <- read.dta("ccarddata.dta")

head(cc)

summary(cc)

## -Nice, no NAs
## -People ages 20-55
## -Everyone spent at least some money with credit cards
## -37.5 % own their homes
## -Household income in thousands: we weren't told this, but it must
## be monthly income.


## Another way to take an introductory glance at the data is with 
## scatter plots.  A function in the car library plots scatter plots
## of all bivariate combinations of variables at once

## (This is better used as an in-R diagnostic, not as something
## to include in a problem set writeup).

library(car)

scatterplot.matrix( ~ credit.card.expend + cc$age + income + own.rent, data=cc)


## -Mostly look at the first column of this.
## -Bivariate plots don't hurt as a place to start, but doesn't look
## like too much to work with here.
## -Pretty clear that credit card spending not normally distributed,
## for what that's worth.
## Income something we might want to keep an eye on.



## Now try running a regression of credit card expenditures on age,
## income, and home ownership.

lm.cc <- lm(credit.card.expend ~ income + own.rent + age, data=cc)
summary(lm.cc)

## LINEARITY

## Let's look at the component residual plots

cr.plots(lm.cc)


## Select one of the 4 options or 0 to exit selection mode

## Partial residual E_i^(j) = E_i + B_j X_{ij}
## Note that this differs from added-variable plots,
## where we're also controlling for the linear influence component,
## but we're graphing the residuals of credit card expenditures
## against X_j, not against the residuals of X_j~X_{-j}.



## To compare added variable to component plus residual,

par(mfrow=c(1,2))
cr.plots(lm.cc)
2
0 ## to exit
av.plots(lm.cc)
2
0 ## to exit


## The plots don't look too bad, but the log-curve shape of the
## income residuals suggests that we need to go UP the chain of powers

## Important notes on power transformations:
## -Only apply power transformations to positive values.  Odd power
## transformations will preserve order but even transformations will
## not.
## -Careful when trying to log values.  If X=0, log(X) undefined.
## A common trick is to add 1 to X: log(X+1)

## In this case, squaring the income variable seems reasonable.

cc$incsquared <- cc$income^2

## Now rerun with squared income term.

lm.cc2<-lm(credit.card.expend ~ income + incsquared + own.rent + age, data=cc)
summary(lm.cc2)

cr.plots(lm.cc2)

## Linearity doesn't look so bad now.  


## GAM plots

library(mgcv)

g.cc <- gam(credit.card.expend~s(income)+s(age)+own.rent, data=cc) 

## s() lets the function choose a smooth functional form that
## minimizes deviations from the surface without becoming too wiggly
## (a penalty for the variance-bias tradeoff is written into the function)

summary(g.cc)

## What do these numbers mean?

## edf: equivalent degrees of freedom. When edf=number of independent
## variables, linear relationship captures effect. When edf > k, smooth
## surface deviates from linear.  Think about n>k  as having to use
## an additional variable to describe a smooth regression surface.
## (This gam eats up two DF estimating the smooth curve for income.
## This indicates that our power transformation is about right.)

g.cc.2<-gam(credit.card.expend~s(income)+s(incsquared)+s(age)+own.rent, data=cc)

## F value and associated p-value: probability that variable would
## have this strong of an effect under the null hypothesis that there
## is no relationship (treat with skepticism, better off graphing
## confidence bands)

## GCV: generalized cross-validation score, ability to predict out of
## sample.  We can compare these across models and try to minimize
## this value.

## However, it makes more sense to plot the gam objects to see the
## partial relationships for each variable.

plot.gam(g.cc)

## age gets a nice quadratic fit.

plot.gam(g.cc.2)

## All 3 of the terms get linear fits when we include the squared
## term.




## HETEROSKEDASTICITY

library(mgcv)

## Let's look at a spread-location plot of the studentized residuals
## from our regression.

scatter.smooth(fitted(lm.cc2), sqrt(abs(rstudent(lm.cc2))), col="red")
scatter.smooth(fitted(lm.cc2), sqrt(abs(residuals(lm.cc2))),
               col="red")

scatter.smooth(cc$income, sqrt(abs(rstudent(lm.cc2))), col="red")

scatter.smooth(cc$incsquared, sqrt(abs(rstudent(lm.cc2))), col="red")

## There is some heteroskedasticity here, especially with respect to
## income.

## White standard errors

## Recall variance covariance matrices

vcov(lm.cc2)


se.hc3<-sqrt(diag(hccm(lm.cc2))) ## default=hc3 method

se.hc0<-sqrt(diag(hccm(lm.cc2, type="hc0"))) ## formula in book
se.hc1<-sqrt(diag(hccm(lm.cc2, type="hc1"))) ## small samp correction
se.hc2<-sqrt(diag(hccm(lm.cc2, type="hc2"))) ## small samp correction
se.std<-sqrt(diag(vcov(lm.cc2))) ## small samp correction

## Let's calculate some test statistics

t.std<-coefficients(lm.cc2)/se.std

sqrt(diag(hccm(lm.cc2, type="hc0"))) ## default=hc3 method
hccm(lm.cc2) ## default=hc3 method
     



## Weighted Least Squares

## Suppose you have good prior reason to believe
## that the error variance is proportional to log(income).
## In WLS, we'll weight observations to give more weight
## to observations that we believe have small error variance.

## The easy way: just add the weights in lm()

lm.cc2.wt<-lm(lm.cc2$call, weights=1/log(income), data=cc)

summary(lm.cc2.wt)

t.stat.unwt<-coefficients(lm.cc2)/sqrt(diag(vcov(lm.cc2)))
t.stat.wt<-coefficients(lm.cc2.wt)/sqrt(diag(vcov(lm.cc2.wt)))
plot(t.stat.wt~t.stat.unwt, xlim=c(-2.5,2.5), ylim=c(-2.5, 2.5))
abline(0,1)
abline(h=c(-1.96,1.96), lty=2)
abline(v=c(-1.96,1.96), lty=2)



## This seems to be a clear case where the chosen weights are leading to
## larger standard errors on the income-squared term.


## NORMALITY

library(car)

## We've already discussed how to compare quantiles to compare 
## two distributions and test an assumption of normality, for example.
## Before we used the functions qqnorm() and qqplot().
## We can also check the normality of residuals assumption with
## a form of the qq plot in the car library.
## Here we are comparing the studentized residuals to the t distribution

qq.plot(lm.cc2)


## UNUSUAL OBSERVATIONS

## Recall the previous section in which we looked for unusual 
## observations.

## hatvalues() will be useful

hats <- hatvalues(lm.cc2)



## ROBUST REGRESSION

install.packages("robustbase")
library(robustbase)


lm.rob <- lmrob(credit.card.expend~income+age+own.rent, data=cc) 

## INDEPENDENCE

## Fixed effects models

## These are fairly straightforward: set a dummy variable for each
## cluster in our data. This is equivalent to setting a separate
## intercept for each cluster.  As such, we are forcing ourselves to
## draw inferences only about the population of individuals in the cluster

## This makes the most sense when our clusters cover the entire
## population of interest, e.g.
## -individuals w/in countries
## -countries w/in continents
## -individuals w/in states
## -individuals w/in regions

## Cases where this approach may be more limiting and will force
## us to limit our study to a narrower population.
## -students within schools (unless we have a sample of ALL schools)
## -children within households (unless we have all households covering
## our population of interest)

## Let's look at an example using the Robey data.
data(Robey)

## First, let's see if there's a big difference in the residuals
## the four regions in our study

lm.rob<-lm(tfr~contraceptors, data=Robey)
par(mfrow=c(1,2))

plot(residuals(lm.rob)~Robey$region, ylim=c(-1.5,1.5))

## There are slight differences across the three regions.  In
## Asia, especially, there may be some regional confounder that we
## could use if we had more data.  But we can still use the regional
## dummy variable.  

lm.rob.fix<-lm(tfr~contraceptors+region, data=Robey)
plot(residuals(lm.rob.fix)~Robey$region, , ylim=c(-1.5, 1.5))

## Another way of viewing these data

plot(tfr~contraceptors, pch="", data=Robey)
abline(coefficients(lm(tfr~contraceptors, data=Robey)))
points(tfr~contraceptors, data=Robey[Robey$region=="Asia",], pch=5)
points(tfr~contraceptors, data=Robey[Robey$region=="Latin.Amer",],
       pch=19)
points(tfr~contraceptors, data=Robey[Robey$region=="Near.East",], pch=4)
points(tfr~contraceptors, data=Robey[Robey$region=="Africa",], pch=3)
legend(15,3, legend=c("Latin.Amer", "Asia","Near.East", "Africa"), pch=c(19,5,4,3))






library(mgcv)