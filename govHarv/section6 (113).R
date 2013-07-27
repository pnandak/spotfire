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

#   1) Visualizing two-dimensional data

#   2) Fitting linear regressions

#   3) Relationship between regression, t-test

############################################




#   1) Visualizing two-dimensional data

#   The first thing to do when taking a look at bivariate data is to actually 
#   look at the data.  We are interested in modelling the relationship between 
#   two variables as a line, so we had better make sure that this isn't 
#   completely crazy!

#   Let's look at the Florida 2000 data again:

library(car)
data(Florida)
attach(Florida)

#   We'll plot Bush's votes on the x-axis and Gore's votes on the y-axis:

#   The plot() function draws a scatterplot for us:

plot(BUSH, GORE)

#   Does this look like a linear relationship?
#   What about this?

plot(log(BUSH), log(GORE))

#   This looks a whole lot more like a line.  We'll talk more about why this 
#   works in this case, but for right now, we'll work with the log of Bush 
#   votes and the log of Gore votes.

#   Another way to create the scatterplot is to use a formula.  This is a
#   piece of syntax of the form Y ~ X.  We'll see this again:


plot(log(GORE) ~ log(BUSH))

#   We can also look at a contour plot like the ones that we saw in lecture 
#   this morning.  These contours are being estimated from the data:

#   The kde2d function is doing something like fitting a two-dimensional 
#   histogram to this data:

kdout <- kde2d(log(BUSH), log(GORE))

#   Let's look at a contour plot:

contour(kdout)
points(log(BUSH), log(GORE))

#   We can also look at a perspective plot:

persp(kdout)
persp(kdout, theta = -45, phi=20)
persp(kdout, theta = 45, phi=20)

#   So, this looks pretty linear.  But what is the "best" line?

#   2) Fitting linear regressions


#   We are going to use the least-squares criterion to pick the "best" line
#   for this data.  We do this using the lm() function.  The first argument
#   to the function is the formula, of the form Y ~ X.  When we are fitting
#   this model, we say that we are regressing Y on X:

lm(log(GORE) ~ log(BUSH))

#   How do we interpret the intercept?  How about the slope?

#   We often want to save the output of fitting our line so that we can 
#   use it again.

lmout <- lm(log(GORE) ~ log(BUSH))

#   Let's add the line to our plot:

abline(lmout)

#   We can also get a summary of our fitted line, which has a lot more 
#   information:

summary(lmout)

#   We are going to spend a lot of time talking about what all of these
#   numbers mean, but for now, you just need to know that all of the 
#   numbers in the table have the same interpretation as they did in the 
#   univariate case.

#   So, the standard error of each coefficient is a measure of the 
#   variability of that coefficient.  We can use these to construct 
#   confidence intervals

#   The t-value is the value of the test statistic under the null hypothesis
#   that the coefficient equals zero.  You get it by dividing the coefficient
#   estimate by the standard error.

#   The Pr(>|t|) is the two-sided p-value under the null hypothesis that 
#   the coefficient equals zero.

#   Let's take a look at some more data:

data(anscombe)

anscombe
lm(y1 ~ x1, data=anscombe)
lm(y2 ~ x2, data=anscombe)
lm(y3 ~ x3, data=anscombe)
lm(y4 ~ x4, data=anscombe)

plot(y1 ~ x1, data=anscombe)
abline(lm(y1 ~ x1, data=anscombe))
plot(y2 ~ x2, data=anscombe)
abline(lm(y1 ~ x1, data=anscombe))
plot(y3 ~ x3, data=anscombe)
abline(lm(y1 ~ x1, data=anscombe))
plot(y4 ~ x4, data=anscombe)
abline(lm(y1 ~ x1, data=anscombe))

#   3) Relationship between regression, t-test

#   So far, we've looked at linear regression where both the outcome (Y)
#   variable and the predictor (X) variable are continuous.  But what if
#   the X variable is binary, taking on the values zero or one?

salary <- read.csv("C:/200607.csv")
flint <- subset(salary, CAMPUS=="UM_FLINT")
annarbor <- subset(salary, CAMPUS=="UM_ANN-ARBOR")

annarbor.idx <- sample(1:nrow(annarbor), 1000, replace=FALSE)
flint.idx <- sample(1:nrow(flint), 200, replace=FALSE)

annarbor.subset <- annarbor[annarbor.idx,]
flint.subset <- flint[flint.idx,]

t.test(annarbor.subset$FTR, flint.subset$FTR, var.equal=FALSE)
t.test(annarbor.subset$FTR, flint.subset$FTR, var.equal=TRUE)

#   The rbind command joins the two subsets into a single dataframe

subset <- rbind(annarbor.subset, flint.subset)
dim(subset)

#   This line creates an indicator variable equal to 1 for Ann Arbor, 
#   0 for Flint

subset$aa <- as.numeric(subset$CAMPUS=="UM_ANN-ARBOR")

#   Now, we plot the data:

plot(subset$FTR~subset$aa, xlab="Campus", ylab="Full-time rate")

#   Find the least-squares fit:

lmout <- lm(FTR ~ aa, data=subset)
lmout
summary(lmout)

#   See anything familiar?

abline(lmout)

