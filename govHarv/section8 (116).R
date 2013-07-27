############################################

#   Government 50
#   Section 8 - Introduction to linear regression
#                 
#   April 10, 2008
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################

#   Goals for today:

#   1) Interpreting regression output

#   2) Testing hypotheses about regression coefficients

#   3) Diagnostics

#   4) Interpreting transformed variables

#   5) Fitting multiple regressions

############################################



#   1) Interpreting regression output

#   We are review fitting and interpreting simple regression output; 
#   we'll use the Florida data in the car library:

library(car)
data(Florida)
attach(Florida)

#   Let's start by regressing Gore's votes on Bush's votes:

plot(GORE ~ BUSH)
lmout <- lm(GORE ~ BUSH)
summary(lmout)

#   What does each of these columns mean?

#   As a side note, there are a lot of things that you can get out of the
#   model object:

names(lmout)

#   The fitted values and residuals are often useful.

#   We can also form confidence intervals in the usual way.  So, a 95% confidence
#   interval for the slope is going to be the point estimate + or - t(alpha/2) times
#   the standard error:

c(1.198 - qt(.975, 65)*.06657 ,1.198 + qt(.975, 65)*.06657 )

confint(lmout)
confint(lmout, level=.99)


#   2) Testing hypotheses

#   OK, so what if we want to test a hypothesis about either the slope or the
#   intercept?  If our hypothesis is that the value is zero, then we can
#   just read it off of the summary output:

summary(lmout)

#   The t-values that are reported are the t-values under the null hypothesis
#   that the parameter is equal to zero.  The same is true for the p-value, so
#   you can just compare the p-value to the level that you chose for the test and
#   go from there.

#   But what if you want to conduct a hypothesis test for something other than 
#   the null that the parameter equals 0?  What if you wanted to test the 
#   hypothesis that the slope coefficient on Bush is exactly 1? There are two ways:

#   The first is probably the easiest.  If you remember the duality between
#   confidence intervals and hypothesis tests, you can just look at the CI and see
#   if it contains the null value specified in your hypothesis.  

#   The other option is to actually calculate the test statistic and the p-value u
#   under your null.  So, just like before, the test statistic is beta_hat - beta_null
#   divided by se(beta_hat)

(1.198 - 1)/.06657

#   Using that, we can either compare it to the critical value to see if it falls in 
#   the rejection region:

qt(.975, 65)

#   Or we can calculate the p-value under the null:

(1 - pt(2.9743, 65))*2

#   This will give us the right result: we reject the null.  Of course, all of this 
#   depends on the model being right.

#   3) Diagnostics

#   There are basically two types of diagnostics that we will want to use
#   when we are fitting a simple linear regression.  One set of diagnostics 
#   attempts to identify violations of the basic regression assumptions: 
#   linearity, constant error variance, normality, and conditional independence.
#   

#   The easiest thing to check is linearity; just plot it!
#   One tool that can be useful is the scatter.smooth() function.  It works
#   just like plot(), but it adds a moving average to the graph.

#   We are going to keep looking at the Gore ~ Bush data.

scatter.smooth(GORE ~ BUSH)

#   We can also add the least squares line to the plot; in general, if the moving
#   average and the least squares line are "close", then the linearity assumption is
#   usually pretty reasonable.

lmout <- lm(GORE ~ BUSH)
abline(lmout)

#   So, it looks like there might be a bit of a problem.  Let's also look at the 
#   constant variance assumption:

plot(lmout, which=3)

#   This is the spread-location plot that we saw in class.  There is pretty clearly
#   a problem with this data.  The spread-location plot is one of the diagnostics
#   that you get if you plot an lm object:

plot(lmout)

#   Given the pretty obvious violation of the constant error variance assumption, 
#   checking normality doesn't make much sense because the errors appear to come
#   from different distributions.  So, when we look at the qq-plot, it looks pretty 
#   bad:

plot(lmout, which=2)
qq.plot(lmout$resid)

#   So, what can we do?  One option would be to change the definition of the variables.
#   Instead of looking at the raw vote counts, we could look at the share of the
#   vote for each candidate:

GorePct <- GORE/Total
BushPct <- BUSH/Total

lmout2 <- lm(GorePct ~ BushPct)

#   Remember, though, that this changes the question!

scatter.smooth(GorePct ~ BushPct)

plot(lmout2, which=3)

plot(lmout2, which=2)

#   Another option is to use robust standard errors to deal with the non-constant 
#   error variance.  This uses the hccm() function, but you need to take the square 
#   root of the diagonal of the hccm output (don't worry too much about the details):

sqrt(diag(hccm(lmout)))
cbind(summary(lmout)$coef[,1:2], sqrt(diag(hccm(lmout))))


#   The other set of regression diagostics tries to identify points that are "ususual" 
#   in some way.  Going back to our original model of GORE ~ BUSH, we can start by
#   looking at leverage points.  These are points that are far away from the
#   center of the X data.

#   One metric for measuring leverage is the hat value.  This is just a measure of 
#   distance.  You can use the hatvalues() function in the car library:

hatvalues(lmout)
plot(BUSH, hatvalues(lmout))
abline(h=4/length(BUSH))

plot(BushPct, hatvalues(lmout2))
abline(h=4/length(BushPct))

#   So in this data, the counties with very large numbers of Bush votes have high 
#   influence.  

#   What about outliers?  The standard way to look at these is to look at studentized 
#   residuals.  These follow a t-distribution if the model assumptions are correct.

rstudent(lmout)
plot(fitted(lmout), rstudent(lmout))

#   In general, we'd expect about 95% of the studentized residuals to fall within
#   plus or minus two.  So we've got some problems here as well.

#   We can use the identify() function to pick out the problematic data points:
#   The first argument(s) to identify are the x and y axis of the data:

identify(fitted(lmout), rstudent(lmout))

#   This brings up a different cursor (called the brush) which you can use
#   to click near points and call up their observation numbers.  You can also
#   give other labels to use, such as row names or an ID variable:

plot(fitted(lmout), rstudent(lmout))
identify(fitted(lmout), rstudent(lmout), labels=rownames(Florida))


#  4) Interpreting transformed variables:

#  We know how to descriptively interpret the results from 
#  untransformed linear regression:

library(car)
attach(Florida)
plot(BUSH ~ GORE)
lm(BUSH ~ GORE)

#  So, for this data, if two counties differ in the number of Gore 
#  votes by 1, we would expect them to differ in the number of Bush 
#  votes by 0.695.

#  Now, what if we take the square root of both variables?

plot(sqrt(BUSH) ~ sqrt(GORE))
lm(sqrt(BUSH) ~ sqrt(GORE))

#  How do we interpret the coefficient on sqrt(GORE)?  You can do the 
#  same thing, but now you have to remember that the units have changed:

#  If two counties differ in the square root of Gore 
#  votes by 1, we would expect them to differ in the 
#  square root of Bush votes by 0.848.

#  This isn't very intuitive, but there isn't much we can do with the 
#  square root transformation.

#  The same isn't true for the log transformation, however.

plot(log(BUSH) ~ log(GORE))
lm(log(BUSH) ~ log(GORE))

#  We can do the same thing as before, saying that if two counties 
#  differ in the log of Gore votes by 1, then we would expect them
#  to differ in the log of Bush votes by 0.887.

#  But, there are a couple of more intuitive ways to interpret these 
#  results.  If both the dependent and independent variables are logged,
#  then we can interpret the slope coefficient as the relationship between
#  a percentage unit change in X and a percentage unit change in Y.

#  So, if the coefficient on log(GORE) is 0.8869, then a 1% change 
#  in Gore votes is associated with a 0.89% change in Bush votes.

#  Note that this makes sense for small percentage changes in X; 
#  You probably wouldn't want to interpret a 1000% change in X as leading 
#  to a 887% change in Y. 

#  A more precise way to describe the relationship between the two variables 
#  is to think about the mathematics of the model

#  log(BUSH) = alpha + beta*log(GORE)
#  exp(log(BUSH)) = exp(alpha + beta*log(GORE))
#  BUSH = exp(alpha)*exp(log(GORE^beta))
#  BUSH = exp(alpha)*GORE^beta

#  So, if you multiply the number of Gore votes by a constant c, then 
#  the predicted number of Bush votes is multiplied by c^beta.
#  In other words, if beta = 2, and you double Gore votes, then you 
#  multiply the number of Bush votes by 2^2, or 4.

## 5) Fitting multiple regression

#  Now that we've spent a lot of time looking at simple regressions
#  we can move on to fitting multiple regression.  For this example, 
#  we will use the Anscombe dataset on income and demograpics in US
#  states.


data(Anscombe)
attach(Anscombe)
head(Anscombe)

#  Let's look first at bivariate plots of income on education and 
#  income on the proportion of young residents:

plot(income ~ education)
plot(income ~ young)
identify(income ~ young, labels=rownames(Anscombe))

#  Now we will run a regression of income on young and education:

lmout <- lm(income ~ young + education)
summary(lmout)

#  How do we interpret the coefficients?

#  We can get some insight by looking at plots of subsets
#  of the the data for income and education defined by 
#  groups of young

par(mfrow=c(2,2))
plot(income[young > 380] ~ education[young > 380], ylim=c(2000,4500), xlim=c(100,400))
plot(income[young <= 380 & young > 360] ~ education[young <= 380 & young > 360], ylim=c(2000,4500), xlim=c(100,400))
plot(income[young <= 360 & young > 340] ~ education[young <= 360 & young > 340], ylim=c(2000,4500), xlim=c(100,400))
plot(income[young <= 340] ~ education[young <= 340], ylim=c(2000,4500), xlim=c(100,400))


#  How do we form confidence intervals for each estimate?

confint(lmout)

#  How do we interpret these confidence intervals?
