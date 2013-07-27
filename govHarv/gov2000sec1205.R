library(foreign)
library(car)
library(mgcv) ## for gam plots
dir()
setwd("~/Gov2000TF")
cc<-read.dta("ccarddata.dta")

## Initial diagnostics
## Consider this a "getting to know you" moment before making the move
## on your data.
summary(cc)
## -Nice, no NAs
## -People ages 20-55
## -Everyone spent at least some money with credit cards
## -37.5 % own their homes
## -Household income in thousands: we weren't told this, but it must
## be monthly income.

scatterplot.matrix(~credit.card.expend+age+income+own.rent, data=cc)
## -Mostly look at the first column of this.
## -Bivariate plots don't hurt as a place to start, but doesn't look
## like too much to work with here.
## -Pretty clear that credit card spending not normally distributed,
## for what that's worth.
## Income something we might want to keep an eye on.


## Now try running a regression of credit card expenditures on age,
## income, and home ownership.

lm.cc<-lm(credit.card.expend~income+own.rent+age, data=cc)
summary(lm.cc)
cr.plots(lm.cc)

## How do we interpret these plots?

## Formally:
## Partial residual E_i^(j) = E_i + B_j X_{ij}
## Note that this differs from added-variable plots,
## where we're also controlling for the linear influence component,
## but we're graphing the residuals of credit card expenditures
## against X_j, not against the residuals of X_j~X_{-j}.

av.plots(lm.cc)
par(mfrow=c(1,2))
cr.plots(lm.cc)
2
0 ## to exit
av.plots(lm.cc)
2
0 ## to exit

## Heuristically: think about taking a bunch of transparent cross-sections
## of the regression hyperplane with respect to variable X_j,
## then overlaying all of them so they each center on the linear component.
## (This lets us do a higher-dimensional form

## The plots don't look totally wacky, but the log-curve shape of the
## income residuals suggests that we need to go UP the chain of powers
## (See the Tukey/Mosteller "bulge" plot for a nice heuristic on
## this.)

## Important notes on power transformations:
## -Only apply power transformations to positive values.  Odd power
## transformations will preserve order but even transformations will
## not.
## -Careful when trying to log values.  If X=0, log(X) undefined.
## A common trick is to add 1 to X: log(X+1)

## In this case, squaring the income variable seems reasonable.

cc$incsquared<-cc$income^2

## Now rerun with squared income term.
lm.cc2<-lm(credit.card.expend~income+incsquared+own.rent+age, data=cc)
summary(lm.cc2)

cr.plots(lm.cc2)
## Linearity doesn't look so bad now.  But remember: we assume that
## controlling for the linear component of the other variables is
## okay.

## GAM plots
g.cc<-gam(credit.card.expend~s(income)+s(age)+own.rent, data=cc)
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

## Let's look at a spread-location plot of the studentized residuals
## from our regression.

scatter.smooth(fitted(lm.cc2), sqrt(abs(rstudent(lm.cc2))), col="red")
scatter.smooth(fitted(lm.cc2), sqrt(abs(residuals(lm.cc2))),
               col="red")
scatter.smooth(cc$income, sqrt(abs(rstudent(lm.cc2))), col="red")
scatter.smooth(cc$income, sqrt(abs(rstudent(lm.cc2))), col="red")
scatter.smooth(cc$incsquared, sqrt(abs(rstudent(lm.cc2))), col="red")

## There is some heteroskedasticity here, especially with respect to
## income.

## White standard errors
se.hc3<-sqrt(diag(hccm(lm.cc2))) ## default=hc3 method
se.hc0<-sqrt(diag(hccm(lm.cc2, type="hc0"))) ## formula in book
se.hc1<-sqrt(diag(hccm(lm.cc2, type="hc1"))) ## small samp correction
se.hc2<-sqrt(diag(hccm(lm.cc2, type="hc2"))) ## small samp correction
se.std<-sqrt(diag(vcov(lm.cc2))) ## small samp correction
t.std<-coefficients(lm.cc2)/se.std
t.hc3<-coefficients(lm.cc2)/se.hc3
t.hc2<-coefficients(lm.cc2)/se.hc2
t.hc1<-coefficients(lm.cc2)/se.hc1
t.hc0<-coefficients(lm.cc2)/se.hc0

plot(t.std, pch=19)
points(t.hc3, pch=5)
points(t.hc2, pch=4)
points(t.hc1, pch=3)
points(t.hc0, pch=2)
legend(locator(), legend=c("Regular", "hc0", "hc1", "hc2", "hc3"),
       pch=c(19, 2,3,4,5))
## Calculate and display t statistics for the 4 methods
## Very small differences here, even with only 72 observations.
## But if we have a big attachment to hypothesis testing, these
## inflation factors could make a difference.
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

## This seems to be a clear case where the chosen weights are leading
## larger standard errors on the income-squared term.

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
lm.rob.fix<-lm(tfr~contraceptors+region, data=Robey, ylim=c(-1.5, 1.5))
plot(residuals(lm.rob.fix)~Robey$region)

## Another way of viewing these data

plot(tfr~contraceptors, pch="", data=Robey)
abline(coefficients(lm(tfr~contraceptors, data=Robey)))
points(tfr~contraceptors, data=Robey[Robey$region=="Asia",], pch=5)
points(tfr~contraceptors, data=Robey[Robey$region=="Latin.Amer",],
       pch=19)
points(tfr~contraceptors, data=Robey[Robey$region=="Near.East",], pch=4)
points(tfr~contraceptors, data=Robey[Robey$region=="Africa",], pch=3)
legend(15,3, legend=c("Latin.Amer", "Asia","Near.East", "Africa"), pch=c(19,5,4,3))


