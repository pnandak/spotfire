##
## section 10 - multiple testing
## gov2000 - 03 Dec 08
## mb
##


#################################
## 1) Working with lm() output ##
#################################

## There is a lot packed into output from the lm function.
## It is often fruitful to write functions that use lm output
## to do some task that we need.

## Let's load the
## Duncan data from the car library for a running example.
## This dataset looks at the prestige of jobs based on their
## income and education levels.

library(car)
data(Duncan)

my.mod <- lm(prestige ~ education + income, data=Duncan)
names(my.mod)
names(summary(my.mod))

## there are a number of useful functions that extract what we
## want from the lm output.

## The beta hats:
coef(my.mod)

## The variance-covariance matrix of the beta-hats:
vcov(my.mod)

## The model matrix [1,X]:
head(model.matrix(my.mod))

## the Confidence intervals at the desired level:
confint(my.mod, level=.9)

###############
## 2) F test ##
###############

## Let's start off with a quick demonstration of what the F
## distribution looks like for some parameters:

curve(df(x=x,df1=5,df2=200), from=0, to=15, col="chocolate", lwd=2)

## We can use the same pXXXX functions from the normal and t
## with the F:

pf(q=2, df1=5, df2=200)
pf(q=2, df1=5, df2=200, lower.tail=FALSE)

## We can also use the qXXXX functions to find the point where
## x% of the distribution is higher or lower:

qf(p=0.95, df1=5, df2=200)
## or
qf(p=0.05,  df1=5, df2=200, lower.tail=FALSE)

curve(df(x=x,df1=5,df2=200), from=0, to=15, col="chocolate", lwd=2)
abline(v=qf(p=0.95, df1=5, df2=200), col="chocolate", lwd=2)


## We can do F-tests in R a few different ways.


## A special case of the F-test that is often used is the "omnibus" test
## of all of the variables in the regression. R (and most other
## software packages) gives you this in the regression output:

lm.out <- lm(prestige ~ education + income, data=Duncan)
summary(lm.out)

## We can plot this test-statistic against its null distribution
## and we can see that it is very unlikely.

curve(df(x, 2, 42), from=0, to=110, col="chocolate")
abline(v=summary(lm.out)$fstatistic[1], col="chocolate")


## anova()
## we can also use the anova function to test the hypothesis
## a group of variables is jointly significant. First we need to
## run two models the full (or unrestricted model) and the restricted
## model that does not include the variables.

## Full model:
prestige.mod.1 <- lm(prestige ~ income + education + type,data=Duncan)

## Restricted model:
prestige.mod.0 <- lm(prestige ~ income + education, data=Duncan)


## 'type' is a dummy variable in this case.
anova(prestige.mod.0, prestige.mod.1) ## notice order of arguments



## linear.hypothesis() in the car library

## linear.hypothesis() permits a broad set of restrictions.  maybe you can
## use this to test your code this week.

lm.out <- lm(prestige ~ income + education, data=Duncan)
linear.hypothesis(lm.out,matrix(c(0,1,1),1,3, byrow=T), rhs=c(1))
summary(lm.out)

## rhs=c(1) is our c vector.  The matrix() input is a our hypothesis
## matrix (in this case a vector).  The p-value is relatively high;
## our results suggest that under our null, a discrepancy like the one
## we calculated would appear as a result of chance not infrequently.


############################
## 3) Confidence Ellipses ##
############################


## confidence.ellipses are hard to compute on your own and are impossible
## with more than two coefficients, but they can be useful. in the car
## library there is a function called, unsurprisingly, confidence.ellipse():


lm.out <- lm(prestige ~ income + education + type, data=Duncan)
confidence.ellipse(lm.out, which.coef=c("income","education"), col="red")

## however, the defaults for this arent't since we often cannot see (0,0),
## which is an important point:

confidence.ellipse(lm.out, which.coef=c("income","education"), col="red",
                    xlim=c(-.1,1), ylim=c(-.1,1))


## we can add an additional point on the graph using the points() function

points(x=0, y=0, col="blue", pch=19, cex=1.5)

## cex=1.5 stands for "character expansion" which makes the point bigger.

## we also might want to add lines for the confidence intervals for each
## of the two coefficients. when you do this, make sure you are plotting
## them in the right direction. in this case the CI for income should be
## vertical and the CI for education should be horizontal.