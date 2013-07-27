## NAIVE NON-PARAMETRIC REGRESSION, LOCAL AVERAGING, and
## LOCALLY-WEIGHTED REGRESSION IN R
## Alison Post and Ryan T. Moore
## Updated by Clayton Nall 17 October 2007


##Thanks to Justin Grimmer for the code adapted for this example.

##(See OrgHours.pdf)
##We've displayed a graph that does a moving local average over some
##underlying data.  In this case, let's say that these are from
##monthly surveys where people reported the number hours they spent
##in civic groups each month, minus the time they reported spending
##in entertainment activities (e.g, which, depending on the period,
##could have included listening to radio, watching sporting events, playing cards, watching TV).  We
##then take the unweighted local average within a sliding six-year
##window.  This moving local average appears in the figure.
##Tell us a convincing story about these data.


##
## CONTEXT: Regression analysis describes the distribution of a
## dependent variable as a function of one or more independent
## variables.  In other words, it is a way of estimating Pr(Y|x1,
## x2,...).  Linear regression models start from particular
## assumptions about Pr(Y|X), such as constant V(Y|X) and E(Y|X) is a
## linear function of the X's (See Fox, p. 17).  Below we explore
## regression techniques that do not rest upon such assumptions.

## 1.  NAIVE NON-PARAMETRIC REGRESSION

## This is the simplest procedure Fox describes.  One simply
##  - orders the data according to the x values,
##  - divides the overall range of x into a number of disjoint intervals,
##  - takes the average y value within each interval, and
##  - connects these points.
set.seed(12345)
## Example:
library(car) ## load car library
data(Robey) ## load Robey data from within car
Robey
help(Robey)

## First, an illustration of how R uses objects and how order() works
## "tfr" is the name of one column in the Robey dataframe

tfr  ## Error: object "tfr" not found
Robey$tfr  ## show column "tfr" without attaching
attach(Robey) ## attach data frame so we can call columns directly
tfr
tfr <- 1:10
tfr
detach(Robey)
attach(Robey)
tfr  ## R still uses the vector tfr, not the Robey$tfr variable.
     ## Moral of the story:  be careful attaching.  If you've created
     ## objects that use the name of variables in an attach()'ed
     ## dataframe, you may not be using what you think you are...

## For every attach(), there should be a detach().

rm(tfr)
tfr
order(tfr)  ## order() permutes the indices of tfr to reflect the values.
            ## The 23rd element of tfr is the smallest, e.g.
## order() can also handle multiple vectors of given length
order(contraceptors, region, tfr)  ## use these indices as row numbers

Robey.ordered.1 <- Robey[order(contraceptors,region,tfr),] 
Robey.ordered.1[1:10,]
## data frame sorted first according to the contraceptors, then by
## region, then by total fertility rate, bringing along the row names

Robey.ordered.2 <- Robey[order(contraceptors,tfr, region),] 
Robey.ordered.2[1:10,]

## Outlying observations can exert a strong influence upon our y
## averages for each interval.  Use the identify() function to
## identify the rows containing outliers we spot in our scatterplot.

plot(contraceptors, tfr)
identify(contraceptors,tfr, row.names(Robey))

## to terminate the process, click with a different mouse button
## (right-click in PC or Unix, not sure with a single button mouse) 

## You can label the local averages you calculate using the points()
## command.  The points command follows a plot() command, just like
## the lines() command.

## Ex:
plot(contraceptors,tfr, col="blue")
xav<- c(20,40) ## E.g., fills vector with specified values
yav<- c(6, 4.5)  ## E.g.
points(xav, yav, col="red") ## plots points with vectors of x and y values

## One could write a function to calculate non-overlapping local
## averages (a.k.a. fixed bin averaging).  It might start out something like

loc.avg <- function(xvar, yvar, m){
  y.means <- array() ## intialize storage
  o <- order(xvar)  ## order of x values
  y.ord <- yvar[o]  ## reorder y's according to x's
  ## etc....
  ## this is a homework exercise...
}

source("gov2000sec4.secret.R")  ## here I source my function, loc.avg
## You should write your own for Fox Problem 2.2

## An example with 2 vectors of 14 observations each
x1 <- 10:-3
y1 <- c(30:35,1:8)
x1
y1 
loc.avg(x1, y1, 1, 7)
loc.avg(x1, y1, 2, 3)
loc.avg(contraceptors,tfr,1,5) ##this uses observations w/ 5 lowest x values


## 2.  LOCAL AVERAGING

## x intervals can be overlapping.  Here are two ways of doing this:

##  1) defining intervals based on a fixed number of data values that
##  are the nearest m neighbors

##  2) defining intervals based on physical proximity to x values
##  evenly-spaced throughout the range of x

## ksmooth() is a pre-defined function that allows us to calculate
## local averages according to method 2 (set interval, not m).

## ksmooth() with the "box" option:
## - the researcher defines an interval length (bandwidth) w.r.t.
## x axis units.
## - key:  as bandwidth increases, interval length increases
##    (think about bias and variance here...)
## - ksmooth() divides the x axis into evenly spaced units
## - it then identifies the x observations falling within the interval
## centered at each of these evenly spaced x values.
## - it then calculates y averages based on the y observations
## associated with the x values in each interval

##Problem: what happens if you have few observations at the extremes?

## Example:
data(Sahlins)
attach(Sahlins)
plot(consumers,acres)
lines(ksmooth(consumers,acres,"box",bandwidth=.5), col="blue")
points(ksmooth(consumers,acres,"box",bandwidth=.5), col="red")
plot(consumers, acres)
lines(ksmooth(consumers,acres,"box",bandwidth=.5), col="blue")
lines(ksmooth(consumers,acres,"box",bandwidth=1), col="red")


## 3.  WEIGHTED LOCAL AVERAGING

## - in the previous example, all observations within each interval
## were weighted equally when calculating the E(Y|X).

## - the resulting curve is choppy; also, estimates may not be great because
## weighting all members of interval equally

## - Below is a way to smooth this local average: using non-uniform
## weight functions.

## - ksmooth() can use a non-uniform distribution to weight the
## y-observations within an x-interval: the normal distribution.  Note
## that the weighting is w.r.t. x values, here.


##Now with a normal smoothing kernel
plot(consumers,acres)
lines(ksmooth(consumers,acres,kernel="normal",bandwidth=.5))

## -Fox also discusses the possibility of weighting outlying y values
## (i.e., observations with larger residuals) less than other
## observations using the bisquare weight function.  One could write
## an R procedure to do this in the context of local averaging.


## 4.  LOCALLY-WEIGHTED REGRESSION

## This is an analog to locally-weighted averaging.  As Cleveland
## discusses, rather than calculating one regression line for an
## entire data set, one can calculate regression estimates for
## overlapping sets of x values (i.e., for all x observations
## falling within the range [x_{i-5},x_{i+5}].  In other words,
## using information from the neighboring points, one can calculate
## the intercept and slope of a line at each x observation along
## the x axis.  Individual observations are weighted according to
## their distance from the given x point, just as in weighted local
## averaging. This method captures changes in the slope with
## changes in X as well as changes in the expectation of Y.

## In R, lowess() plots a curve consisting of a connected series of
## points estimated from locally-weighted regression.  This function
## weights (w.r.t. x) observations according to the tricube weight
## function described by Fox and Cleveland (in different
## contexts). The "f" parameter defines the fraction of the x values
## (nearest neighbors) that should be included in the weighted least
## squares estimation of y-hat for each x value.

## While Cleveland discusses advantages of locally quadratic fitting
## in certain cases, lowess() simply performs locally linear fitting.
## (higher-order polynomials can also be used, but generally aren't desirable)

## lowess() vs. loess(): Basically the same, but different parameters
## and defaults.  We'll use lowess().

## EXAMPLE:
plot(consumers,acres)
lines(lowess(consumers,acres,f=1/3,iter=0),col="green")


## 5.  LOCALLY-WEIGHTED ROBUST REGRESSION

## The lowess() function also allows us to weight observations
## associated with extreme y values less when we calculate local
## regression estimates.  (Normal least squares allows outliers to
## exert a strong effect upon the slope of our regression line, since
## OLS minimizes squared error.) As we increase the number of
## "robustifying iterations," outliers exert less influence upon the
## regression line slope. (see Cleveland p. 97, p. 110 onwards for
## more detail... weight related to residual/(6*med(epsilon)

##EXAMPLE:


plot(consumers,acres)
lines(lowess(consumers,acres,f=1/3,iter=0),col="green")
lines(lowess(consumers,acres,f=1/3,iter=6),col="blue")
lines(lowess(consumers,acres,f=1/3,iter=20),col="blue")
##While this demonstrates the values of robust lowess regression,
##it will be up to you to explore how changing the bandwidth of the
##lowess smoothing affects the results.


##Various weighting functions used for smoothing and robust
##nonparametric regression (see Fox Ch. 2 for a discussion of the
##bisquare and tricube).

xx<-seq(from=-3, to=3, by=0.001)
##Define and test bisquare function

bisquare <- function(zz){
  ifelse(abs(zz)>1, 0, (1-(zz)^2)^2)
  }
bisquare(xx)

##Define and test tricube function
tricube <- function(zz){
  ifelse(abs(zz)>1, 0, ((1-(abs(zz))^3)^3))
  }
tricube(xx)

##Plot dnorm(xx), the tricube, and bisquare functions
plot(xx, dnorm(xx), col="green", xlab="x",
     ylab="f(x)", type="l", main="Examples of Three Weighting
Functions", xlim=c(-3, 3), ylim=c(0, 1), lwd=2)
lines(xx, tricube(xx), col="blue", lwd=2)
lines(xx, bisquare(xx), col="red", lwd=2)
legend(-2.5, 0.8, legend=c("Normal", "Bisquare", "Tricube"), col=c("green",
                                                    "red", "blue"), lwd=c(2,2,2))
