
## section 9 - multiple testing (f-test)
## gov 2k
## nov 12, 2009
## section notes prepared by msen


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

## so you can see everything
model.matrix(my.mod)
Duncan


#######################
## 2) F distribution ##
#######################

## Let's start off with a quick demonstration of what the F
## distribution looks like for some parameters:

curve(df(x=x,df1=5,df2=200), from=0, to=15, col="chocolate", lwd=2)

## We can use the same pXXXX functions from the normal and t
## with the F:

pf(q=2, df1=5, df2=200)
pf(q=2, df1=5, df2=200, lower.tail=FALSE)
	## if lower.tail = T, probabilities are P[X <= x], otherwise, P[X > x]

## We can also use the qXXXX functions to find the point where
## x% of the distribution is higher or lower:

qf(p=0.95, df1=5, df2=200)
## or
qf(p=0.05,  df1=5, df2=200, lower.tail=FALSE)

curve(df(x=x,df1=5,df2=200), from=0, to=15, col="chocolate", lwd=2)
abline(v=qf(p=0.95, df1=5, df2=200), lwd=2)

## We can do F-tests in R a few different ways.

#######################
## 2) F Omnibus test ##
#######################

## A special case of the F-test that is often used is the "omnibus" test
## of all of the variables in the regression. R (and most other
## software packages) gives you this in the regression output:

lm.out <- lm(prestige ~ education + income, data=Duncan)
summary(lm.out)

## We can plot this test-statistic against its null distribution
## and we can see that it is very unlikely.

curve(df(x, 2, 42), from=0, to=110, col="chocolate")
abline(v=summary(lm.out)$fstatistic[1])
	## extracting the omnibus f stat from the lm output

## note that df1 = 2 (two restrictions)
## and df2 = n-(k+1) -- > 45 - 2 - 1

## We can also calculate the F-statistic "manually."
## We know from lecture that 

# F test stat = (RSSnull - RSSfull)/q _divided_ by RSSfull/n-(k+1)

lm.null <- lm(prestige ~ 1, data = Duncan)
	## note that this is basically taking the mean of x
lm.full <- lm(prestige ~ education + income, data = Duncan)
	## and this is the fullmodel

RSSnull <- sum(lm.null$residuals^2)
RSSfull <- sum(lm.full$residuals^2)

df1 <- length(lm.full$coef) - 1
df2 <- nrow(model.matrix(lm.full)) - (length(lm.full$coef) -1 + 1)

((RSSnull - RSSfull)/df1)/(RSSfull/df2)
summary(lm.full)$fstatistic[1]

## Ta da!

#########################################
## 2) F General Linear Hypothesis Test ##
#########################################

## general linear hypothesis tests are more complicated.
## You'll be writing a function that does this in your pset.

## let's take a break to discuss

## One function that will help is 
## linear.hypothesis() in the car library

## linear.hypothesis() permits a broad set of restrictions.  
## hint: you can
## use this to test your code this week.

lm.out <- lm(prestige ~ income + education, data=Duncan)
linear.hypothesis(lm.out, matrix(c(0,1,0,0,0,1),2,3, byrow=T), rhs=c(0,0))
summary(lm.out)

## rhs=c(0,0) is our c vector.  
## The matrix() input is a our hypothesis
## matrix.

############################
## 3) Confidence Ellipses ##
############################

## confidence.ellipses are hard to compute on your own and are impossible
## with more than two coefficients, but they can be useful. in the car
## library there is a function called, unsurprisingly, confidence.ellipse():

lm.out <- lm(prestige ~ income + education + type, data=Duncan)
confidence.ellipse(lm.out, which.coef=c("income","education"), col="red")

## however, we often cannot see (0,0),
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