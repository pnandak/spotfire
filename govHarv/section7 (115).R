############################################

#   Goverment 1001
#   Section 6 - Introduction to linear regression
#                 
#   March 22, 2007
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################

#   Goals for today:

#   1) Interpreting regression output

#   2) Testing hypotheses about regression coefficients

#   3) Diagnostics

############################################



#   1) Interpreting regression output

#   We are going to look at the same example that we looked at last week; 
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


#   The other set of regression diagostics tries to identify points that are "ususual" 
#   in some way.  Going back to our original model of GORE ~ BUSH, we can start by
#   looking at leverage points.  These are points that are far away from the
#   center of the X data.

#   One metric for measuring leverage is the hat value.  This is just a measure of 
#   distance.  In simple regression (with one X variable) the hat value is 

#   1/n + (x_i - mean(x))^2/sum((x_i - mean(x))^2)

#   or you can use the hatvalues() function

hatvalues(lmout)
plot(BUSH, hatvalues(lmout))

#   So in this data, the counties with very large numbers of Bush votes have high 
#   influence.  

#   What about outliers?  The standard way to look at these is to look at studentized 
#   residuals.  We don't need to worry too much about where these come from at this 
#   point; just think of them as being distributed (close to) normal if the model 
#   assumptions are correct.

rstudent(lmout)
plot(fitted(lmout), rstudent(lmout))

#   In general, we'd expect about 95% of the studentized residuals to fall within
#   plus or minus two.  So we've got some problems here as well.

#   Finally, observations that are unusual in both dimensions exert a lot of 
#   influence on the regression line.  One measure of this is Cook's D, which 
#   is a function of the residuals and the hat value.  Big Cook's D implies an 
#   influential observation:

cooks.distance(lmout)
