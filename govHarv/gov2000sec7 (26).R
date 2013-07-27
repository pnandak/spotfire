## Multiple Regression
## Clayton Nall and Jens Hainmueller
## with some material borrowed from Alison Post and Ryan Moore
## October 31, 2007

## REVIEW OF PROBLEM SETS 3 AND 4

##t-distributions with a low number of degrees of freedom.

##The Cauchy distribution
n<-1000
mean.cauchy<-rep(NA, n)
mean.norm<-rep(NA,n)
for(i in 1:n){
  dd<-rt(10000, df=1)
  ee<-rnorm(10000) 
  mean.cauchy[i]<-mean(dd)
  mean.norm[i]<-mean(ee)
}
par(mfrow=c(1,2))
plot(mean.cauchy, ylim=c(-1,1))
plot(mean.norm, ylim=c(-1,1))
## Does the Central Limit Theorem apply if we're drawing values from
## a Cauchy?
## Key take-away point: it's a "pathological case" of the t.  Useful
## for testing how
## well hypothesis tests perform in the presence of heavy-tailed
## distributions.  See the NIST Engineering Stats Handbook (online)
## for a good discussion of this distribution.


##Interpreting coefficients
##Based on what you learned about regression coefficients in lecture
##on Monday, answer the following questions:

##INTERPRETATION EXERCISE

library(car)
data(SLID)
summary(lm(wages ~ education + age, data=SLID))

## What are the estimated hourly wages for an Ontarian who
##   - is zero years old and has no education?
##   - is zero years old and has 10 years of formal education?
## What is the estimated change in hourly wages for an Ontarian who
##   - goes to college for four years?
##   - goes to college for four years and graduates?
##   - wakes up one day having gone to college for four years and graduated?
##   - ages from 30 to 40?

## Things to think about (we'll discuss later):
## What does the R^2 (.25) tell us? When is it useful, when not?
## How should we interpret the standard error of the regression?



## CALCULATING A LEAST SQUARES LINE IN R: 

##Calculating a least squares line in R

##BASIC MECHANICS
lm.out <-  lm(tfr~contraceptors+region, data=Robey) 
attributes(lm.out) ## reveals the various types of output from the function
summary(lm.out) ##This treats region as a factor.  Think about this as
                ##a dummy for each region.  The lowest alphabetical
                ##region is omitted (i.e., the base condition is "Africa")
##What do you notice about the R-squared here?  Is this meaningful?


## There are other functions that allow us to look at a larger set of
## models.  Zelig (Imai, King, Lau) has lm() as an option.  Zelig is a
## wrapper program for a range of models.
library(Zelig)
z.out <-zelig(tfr~contraceptors +region, data=Robey, model="ls")
attributes(z.out)

## Extracting coefficients is the same in simple and multiple
## regression.
beta <- coefficients(lm.out) ## puts our intercept and slope into a vector
beta2 <- lm.out$coefficients ## alternative syntax, same operation
beta

##What are the residuals of this regression?
eps <-residuals(lm.out)
eps2 <-lm.out$residuals
eps2
## What does the value for Yemen mean?
##tfr=# of children per childbearing-age woman

##What are hat values?
##Let's look at these in the simple regresion
lm.simp<-lm(tfr~contraceptors, data=Robey)
hats<-hatvalues(lm.simp)
plot(Robey$contraceptors, hats) ## Demonstrates that hat-value a
                                ## summary of relevant x distance
sum(hats)
length(lm.simp$coefficients)/nrow(Robey) ##mean hat value
## Hat values measure the relative distance of a point from the "centroid"
## of the observed X cloud.  The average hat value for multivariate X
## is hbar=(k+1)/n, where "k is the number of regressors in the model"
## (Fox 271).

##On Problem Set 5 we expect you to demonstrate that you know
##the difference between high-leverage points and outliers.
##Hat values might be a useful diagnostic tool.

## MEASURES OF MODEL FIT: 

## Fox mentions two standard measures of model fit:
## a) The Standard Error of the Regression
## b) R^2

## ***The Standard Error of the Regression*** 

## The standard error of the regression is the standard deviation of
## our error terms.

## sd(error) = sqrt(sum(e^2)/ n-2)

## Since our residuals are measured in the same units as our y values,
## the standard error of the regression is directly comparable with
## the standard deviation of our original y values.  (Hint: think
## about this for Problem 1.)For example, if
## the standard error of the regression is 2 and the standard
## deviation of our y values is 1000, then the standard deviation of
## our residuals is very small in relative terms, and our model is
## performing quite well.

## -->note: Fox encourages thinking about the standard error of the
## residuals as the "average" residual.  It's not the literal average,
## however.  The average residual (which is perhaps best thought of as
## sum(abs(res))/N is different than the sd(res).

## This means that when interpreting the standard error of the
## regression, it is often helpful to know the range and distribution
## of the dependent variable.  (Note: this point carries over to the
## interpretation of our coefficients -- the intercept has little
## meaning on its own, for example, when the independent variables
## never take on the value of zero.)

summary(lm.out)  ## Residual standard error: 0.561
sqrt(sum((lm.out$resid)^2)/lm.out$df)


## ***R^2***

## Regression ANOVA: Decompose TotalSS into RegressionSS + ResidualSS
## TSS = RegSS + ResidSS

## Fox diagram, p. 93

## R^2 is defined as RegSS/TSS.

## R^2 = RegSS/TSS = sum(Yhat-Ybar)^2/ sum(Y_i - Ybar)^2

## R^2 is often described as the fraction of the variation in Y that
## is "accounted for" or "explained" by our model. ([Variation between
## model & mean] / [total observed variation in our dependent
## variable]).  

## R^2 will take on a value between 0 and 1, larger numbers indicating
## that the RegSS is a larger proportion of the TSS ("model accounts
## for a larger fraction of the variation").  R^2 is unitless.  It can
## useful for comparing the relative fit of different models with the
## same dependent variable, but not models with different dependent
## variables.  With different dependent variables, overall variation
## in Y's differ.  CAUTION: HIGHER R^2 is NOT "BETTER".  Build the
## best model you can, based in substantive theory and sound
## statistical practice.  Don't specification-search (i.e., add or
## remove lots of variables) to "improve" R^2.
## Higher R^2 is not a better model.  It may be a worse model.  High
## R^2 may overfit the data.  High R^2 is not a better theory.

## Fox stresses that R^2 isn't an ideal measure of model fit or
## utility.  R^2 is in Gov2000 so that you can be an informed consumer
## of work that reports it.  We will revisit some of the problems with
## and limitations of R^2 when we cover multiple regression.

## -->In R, one can obtain the R^2 value from the regression summary
## table as well.

summary(lm.out)

## IV.  STATISTICAL INFERENCE AND SIMULATION FOR BIVARIATE REGRESSION 

## Our estimator is a function of the observed data.  Our estimates,
## \hat{beta}_0 and \hat{beta}_1, are the output of that function.
## So, HOW DO OUR ESTIMATES "HAVE A DISTRIBUTION"?

## a) DISTRIBUTION OF OUR COEFFICIENT ESTIMATES:

## In many statistical models, we can assume that the relationship
## between our explanatory and dependent variables has two
## components (for more info, read King, Unifying Political Methodology):
## *a systematic component: in LS, the conditional expectation E(Y|X)
## given X. 
## *a stochastic component: a.k.a. the "error" or "random" component.

## When conducting least squares estimation, we assume that our
## observed y values have a stochastic component -- that is, they
## represent a random sample from a distribution.  Each y_i is a
## random variable that (under the strict OLS model) follows a normal
## distribution.  Since our beta-hat values constitute weighted
## averages of our y values, the estimator beta-hat will also follow a
## normal distribution. (A linear combination of independent normally
## distributed random variables is normally distributed.)

## In other words, were we to re-calculate our regression line with a different
## set of y values drawn from the same underlying population, we would
## obtain a slightly different set of coefficients each time.  In
## sufficiently large samples, each coefficient estimate will be
## approximately normally distributed.

## So, because our estimator follows a distribution, we can speak of the
## variance and standard error of the estimator.  (In fact, we can
## think about the coefficients beta being distributed multivariate normal.)

## -***write equation for Var(betahat) on board, show that both are a
## function of the standard error of the residuals, but that location
## and spread of x values matter

##############################################
## An example of simulation based on a "true" population model
##############################################

## Remember the simulation practice we had in the univariate case:
## We assumed a true population model in which we knew the
## population mean and variance.  We then drew samples using these
## true population parameters as our arguments.  For example, if you
## assume the true population values for X are distributed unit
## normal, you draw your simulations from a unit normal.

x.samp<-rnorm(100, mean=0, sd=1)

## Let's try this for a true population model.
## We assume a linear model with
## Y=2X+5
## sigma = 4 (What does this mean?)

## We can choose X and fix it (go back a couple lectures).
## Hard to think about this in terms of observational studies.
## Easier to think about in terms of experimental design (e.g.,
## if we reran an experiment a bunch of times, each time with new
## subjects with the same observed X characteristics). 

x.obs<-seq(0,10, by=0.1)
length(x.obs)
y.sys<-  ##For any given set of x values, we can start by generating
         ##a systematic component.
y.resid<- ##What distribution will we draw from

## Now how do we generate our simulated values drawn from the true
## population model?
y.sim<-   ##our simulated values

lm.sim<- ## Now, using the sample you drew, run the regression.
         ## Extract the coefficients.
## Repeat the above process many times to simulate the sampling distribution
## of your coefficients.  

## Let's try a simple example related to the Robey data
## Suppose we know the true population model for the
## Robey data. (In a real-life example, we'd never know the true
##  population model.)

## True model for the simple case.
## tfr=6.9-0.06X+\epsilon
## Standard error of the regression: 0.56

## Define the parameters of the model
beta0<-6.9
beta1<--0.06
se.reg<-0.56
##First draw X values from 0 to 50 percent
x.sim1<-seq(0,50, length.out=100)
y.sys1<-beta0+beta1*x.sim1
y.resid1<-rnorm(length(x.sim1), mean=0, sd=se.reg)
y.sim1<-y.sys1+y.resid1
## What will the following line give us?
plot(y.sim1-y.sys1)
lm.sim.1<-lm(y.sim1~x.sim1)

##Next draw X values from 0 to 100 percent contraceptors
x.sim2<-seq(0,100, length.out=100)
y.sys2<-beta0+beta1*x.sim2
y.resid2<-rnorm(length(x.sim2), mean=0, sd=se.reg)
y.sim2<-y.sys2+y.resid2

lm.sim.2<-lm(y.sim2~x.sim2)

##Here's a good way to present your graphs from a simulation like
##this.
plot(y.sim1~x.sim1, cex=.6, pch=19, xlim=c(0,100), ylim=c(0,7),
     xlab="Contraceptors", ylab="Fertility Rate")
points(y.sim2~x.sim2, cex=.6, pch=5)
abline(beta0, beta1, col="black")
abline(lm.sim.1$coefficients, col="red")
abline(lm.sim.2$coefficients, col="blue")
legend(10, 2, legend=c("True Regression Line", "0<contraceptors<50", "0<contraceptors<100"),col=c("black", "red", "blue"), lty=c(1,1,1))

##These look really similar, but look at the R-squared values.
summary(lm.sim.1)
summary(lm.sim.2)
##If we're drawing our sample from the same theoretical model,
##why should this be the case?

##Finally, let's have a look at added-variable plots
## We'll use an example with more continuous variables.
## Added-variable plots are
## We are controlling for the estimated linear component
## of the other explanatory variables
data(Chirot)
##Chirot and Ragin (1971)
## intensity: intensity of peasant rebellion
## commerce: commercialization
## tradition: traditionalism
## midpeasant: strength of "middle peasantry"
## inequality: inequality of land tenure

lm.ch<-lm(intensity~commerce+tradition+midpeasant, data=Chirot)
summary(lm.ch)
av.plots(lm.ch, ask=F)##this spares us the menu options

## What does each of these plots represent?
## Walk through the steps.
