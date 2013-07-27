############################################

#   Goverment 50
#   Section 4 - Comparing Means
#   February 28, 2008
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################


#   Goals for today:

#   1) Loading datasets

#   2) Small-sample confidence intervals

#   3) Comparing means

############################################


#   1)  Loading datasets in R

#   For the rest of the course, we usually will want to 
#   analyze data that has been collected by other people.
#   It is a pain to enter all of this data into R by hand,
#   so we need to find a way to load it and use it.

#   We already saw one way of doing this in last week's
#   homework.  If the data is saved as a CSV (comma-separated
#   value) file, you can use the read.csv() function:

salary <- read.csv("C:/datasets/salary.csv", header=T)

#   In a CSV file, the data is written as text.  Each line 
#   represents an observation and variables are separated
#   by commas:
#   
#   CAMPUS,DEPT,FTR
#   UM_ANN-ARBOR,Pathology Department,4615.38
#   UM_ANN-ARBOR,Prosthodontics,4800

head(salary)

#   You can create CSV files in Excel by changing the format
#   when you save a worksheet.  Another common format
#   is Tab-Delimted Text, where each variable is separated
#   by tabs rather than spaces:

#   CAMPUS	DEPT	FTR
#   UM_ANN-ARBOR	Pathology Department	4615.38
#   UM_ANN-ARBOR	Prosthodontics	4800

salary2 <- read.delim("C:/datasets/salary.txt", header=T)
head(salary2)

#   With more complicated datasets, it may be easier to use
#   data saved in SPSS or Stata format.  To use these files
#   we need functions in the foreign library

library(foreign)

#   The function for SPSS files (which end in .sps) is:

read.spss("C:/datasets/salary.sps")

#   And the function for Stata files (which end in .dta) is:

read.dta("C:/datasets/salary.dta")


#   2)  Constructing small-sample intervals

#   We saw in lecture this week that we cannot use the standard
#   formula for large-sample confidence intervals when we
#   have a small number of observations; the normal 
#   approximation just isn't good enough.

#   If we know that the data is approximately normal, however
#   we can construct confidence intervals with the correct 
#   coverage by replacing the quantiles of the standard
#   normal distribution with the quantiles of the appropriate
#   t distribution.

#   Say we have the following 6 SAT scores:

sat <- c(690, 560, 550, 670, 690, 450)

#   We calculate the mean and standard deviation the same way:

mean.sat <- mean(sat)
mean.sat

sd.sat <- sd(sat)
sd.sat

#   And the standard error is still s/sqrt(n):

se.sat <- sd.sat/sqrt(length(sat))
se.sat

#   Remember that the form of a (1-alpha)% CI is

#   mean - t_(alpha/2)*se , mean - t_(alpha/2)*se 

#   Where does t_(alpha/2) come from?

#   The t-distribution: this distribution also has only one parameter, which is
#   also known as the degrees of freedom.  It can take on values over the whole
#   real number line, and is symmetric.  

xx <- -600:600/100
plot(xx, dt(xx, 1), type="l", ylim= c(0,.5), xlim=c(-6,6))
lines(xx, dt(xx, 2), col="red")
lines(xx, dt(xx, 5), col="blue")
lines(xx, dt(xx, 10), col="green")

#   To get the value of t_(alpha/2), we can use the qt() function.
#   For a 95% CI, we want the 97.5 quantile for a t distribution 
#   with n-1 degrees of freedom

qt(.975, length(sat)-1)

mean.sat - qt(.975, length(sat)-1)*se.sat
mean.sat + qt(.975, length(sat)-1)*se.sat

#   If we want a different CI, we change the argument to qt():

mean.sat - qt(.995, length(sat)-1)*se.sat
mean.sat + qt(.995, length(sat)-1)*se.sat


#   3) Comparing two means


#   We are often more interested in comparing the means in two
#   populations or at two points in time than we are in estimating
#   the mean of a single population.

#   Assume we took two surveys of Democratic voters.
#   In the first survey, 295 out of 600 supported Clinton.  
#   In the second survey, 227 out of 500 supported Clinton.
#   What is a point estimate and CI for the difference in 
#   proportions?

#   The point estimate for a difference in means (or proportions) 
#   is just the difference of the means of the two groups:

mean.first <- 295/600
mean.second <- 227/500

diff.means <- mean.second - mean.first

#   To construct a confidence interval, we need to know
#   the standard error of the difference in means.
#   Start by calculating the sample variances
#   for each group.

#   In this case, since they are proportions, we 
#   plug into the formula for proportions:

var.first <- mean.first*(1 - mean.first)
var.second <- mean.second*(1 - mean.second)

#   (If this was continuous data, we would just use 
#   the var() function to estimate the variance)

#   The variance of the estimate is just
#   the sum of each variance divided by its sample size:

diff.samp.var <- var.first/600 + var.second/500
diff.samp.var

#   To get the standard error, we just take the square root:

diff.se <- sqrt(diff.samp.var)

#   A large sample confidence interval is just 

diff.means - qnorm(.975)*diff.se
diff.means + qnorm(.975)*diff.se

#   Comparing dependent data

#   When observations are measured on the same units
#   we usually need to assume that there is some dependence 
#   between them.

#   We are going to look at a dataset with average SAT 
#   scores by state from 2003 and 2003.

#   The question of interest:  what is the population
#   difference between SAT Math and SAT Verbal scores in 2004?

statesat <- read.csv("C:/datasets/statesat.csv", header=T)

#   What if we assume that the Math and Verbal scores are
#   independent?

mean.math <- mean(statesat$Math04)
sd.math <- sd(statesat$Math04)

mean.verb <- mean(statesat$Verb04)
sd.verb <- sd(statesat$Verb04)

#   What is our point estimate?

mean.math - mean.verb

#   These are very close.  What is the standard error (under
#   the incorrect assumption of independence)?

wrong.se.diff <- sqrt(var(statesat$Math04)/length(statesat$Math04) 
     + var(statesat$Verb04)/length(statesat$Verb04))
wrong.se.diff

#   So, our (incorrect) CI would be 

mean.math - mean.verb - qnorm(.975)*wrong.se.diff
mean.math - mean.verb + qnorm(.975)*wrong.se.diff

#   Now, what is the right way?
#   We can take the differences between scores within each state

difference <- statesat$Math04 - statesat$Verb04

mean(difference)
sd(difference)

#   And form the confidence interval:

mean(difference) - qnorm(.975)*sd(difference)/sqrt(length(difference))
mean(difference) + qnorm(.975)*sd(difference)/sqrt(length(difference))

##  A canned function in R for doing this:

#   We can use the t.test function to generate confidence intervals.
#   It has several options:
#     paired = TRUE or FALSE (is data in pairs or independent)
#     var.equal = TRUE or FALSE (is variance assumed equal)
#     conf.level = (confidence level desired)

#   Here is the wrong result, without accounting for dependence

t.test(statesat$Math04, statesat$Verb04, paired=FALSE, var.equal = FALSE)

#   Here is the right result, accounting for the paired data:

t.test(statesat$Math04, statesat$Verb04, paired=TRUE)

#   Note that these are a little wider than the ones we calculated above
#   since they use t quantiles rather than standard normal quantiles

qnorm(.975)
qt(.975, 50)
qt(.975, 100)