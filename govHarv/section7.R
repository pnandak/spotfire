############################################

#   Goverment 50
#   Section 7 - Introduction to linear regression
#                 
#   April 3, 2008
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################

#   Goals for today:

#   1) Visualizing two-dimensional data

#   2) Fitting linear regressions

#   3) Relationship between regression, t-test

#   4) Loading datasets

############################################




#   1) Visualizing two-dimensional data

#   The first thing to do when taking a look at bivariate data is to actually 
#   look at the data.  We are interested in modelling the relationship between 
#   two variables as a line, so we had better make sure that this isn't 
#   completely crazy!

#   We'll start by replicating another result from the Epstein-Mershon paper 
#   on the preferences of supreme court justices.  This data is in the file
#   EpstMershData.txt and we'll use the read.table() function to load it in R.

scotus <- read.table("C:/datasets/EpstMershData.txt", header=T)
attach(scotus)

#   We looked at the relationship between the Segal-Cover scores and civil
#   liberties votes in lecture.  Now, let's look at judicial power votes.

#   We'll plot Segal-Cover scores on the x-axis and the proportion of
#   liberal votes in judicial power cases on the y-axis:

#   The plot() function draws a scatterplot for us:

plot(SCscore, Judlib)

#   Does this look like a linear relationship?

#   Another way to create the scatterplot is to use a formula.  This is a
#   piece of syntax of the form Y ~ X.  We'll see this again:

plot(Judlib ~ SCscore)

#   We can also look at a contour plot like the ones that we saw in lecture 
#   this morning.  These contours are being estimated from the data:


#   2) Fitting linear regressions

#   We are going to use the least-squares criterion to pick the "best" line
#   for this data.  We do this using the lm() function.  The first argument
#   to the function is the formula, of the form Y ~ X.  When we are fitting
#   this model, we say that we are regressing Y on X:

lm(Judlib ~ SCscore)

#   How do we interpret the intercept?  How about the slope?

#   We often want to save the output of fitting our line so that we can 
#   use it again.

lmout <- lm(Judlib ~ SCscore)

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
#   estimate by zero

#   The Pr(>|t|) is the two-sided p-value under the null hypothesis that 
#   the coefficient equals zero.

#   If we want to construct a confidence interval, we can either do it by hand:

9.631 - qt(.975, 25)*6.857
9.631 + qt(.975, 25)*6.857

#   Or we can use the confint() function:

confint(lmout)

#
#   Let's take a look at some more data:
#

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

salary <- read.csv("C:/salary.csv")
flint <- subset(salary, CAMPUS=="UM_FLINT")
annarbor <- subset(salary, CAMPUS=="UM_ANN-ARBOR")

annarbor.idx <- sample(1:nrow(annarbor), 1000, replace=FALSE)
flint.idx <- sample(1:nrow(flint), 200, replace=FALSE)

annarbor.subset <- annarbor[annarbor.idx,]
flint.subset <- flint[flint.idx,]

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


##  4)  Loading datasets

#   For your replication poster, you'll need to download a dataset
#   and be able to load it into R.  There are various kinds of 
#   datasets that R can read.  If the data is a simple text file
#   you can use read.table() and read.csv().  If it is in another
#   format, you may want to use the functions in the foreign library:

library(foreign)

#   SPSS files generally end in .sps.

?read.spss

#   Stata files end in .dta

?read.dta

#   Other file formats R can read:

library(help=foreign)
