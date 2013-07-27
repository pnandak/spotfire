############################################

#   Goverment 50
#   Section 5 - Hypothesis testing and
#                 comparison of means
#   March 6, 2008
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################


#   Goals for today:

#   1) Setting up a hypothesis test

#   2) Calculating test statistics and p-values

#   3) Comparing two means

############################################


#   Goals for today:

#   1) Setting up a hypothesis test

##  To set up a hypothesis test, you don't need to know anything about the 
##  data that you are going to observe, other than possibly how the data was
##  collected and what kind of variable you have.  In fact, if you really 
##  buy into the hypothesis testing framework, you really need to set up your 
##  hypothesis test before you ever get a chance to look at the data.

##  The first thing that you need to do is to identify the hypothesis that 
##  you want to test.  These tests are often a claim about the true value
##  of some parameter, such as the mean or proportion in a population.  
##  It is also possible to specify different types of null hypotheses, which 
##  we will get to later in the term.

##  You need to specify a null hypothesis, which we'll call H_0, and an 
##  alternative hypothesis, H_A.  You don't have to ensure that every possible 
##  outcome is contained in either your null or your alternative hypothesis, but 
##  they do have to be disjoint; it cannot be possible for both H_0 and H_A to 
##  be true simultaneously.

##  So, for example:

##  H_0:  mu = 5  (mu equals 5)
##  H_A:  mu != 5  (mu does not equal 5)

##  H_0:  mu = 5  (mu equals 5)
##  H_A:  mu > 5  (mu greater than 5)

##  H_0:  mu >= 5  (mu greater than or equal to 5)
##  H_A:  mu < 5  (mu less than 5)

##  Once you have specified your null and alternative hypotheses, you need to 
##  make two more decisions.  The first is what test statistic to use.  You can 
##  use just about anything as a test statistic, so long as you know what the 
##  sampling distribution of the test statistic is *when the null is true*.  
##  Statisticians have spent a lot of time figuring out which test statistics 
##  make sense to use, and we are going to follow their advice.  The most common
##  test statistic is the t-statistic, which is

##  ybar - mu_0 / std.error(ybar)

##  The other thing that you have to decide is what level you are going to test
##  at.  You have to decide how often you are willing to reject the null hypothesis
##  when it is in fact true.  For this whole setup to work, you need to pick the 
##  level (which we call alpha) before you look at the data.

#   2) Calculating test statistics and p-values

##  Once we have our data, we can then estimate the mean and standard deviation 
##  from our sample and use them to construct a test statistic.  If we are testing
##  a hypothesis about the mean of a continuous variable, our test statistic is

##  ybar - mu_0 / std.error(ybar)

##  Let's take a look at this in the sunspots data, this time treating the data
##  as a sample from a larger population:

data(sunspots)
sample.mean <- mean(sunspots)
sample.sd <- sd(sunspots)
n <- length(sunspots)
mu_0 <- 50

t.stat <- (sample.mean - mu_0)/(sample.sd/sqrt(n))
t.stat

##  Once you have a t-statistic, what do you do with it?  We want to know how 
##  unusual this value of the t-statistic would be if the null hypothesis is 
##  true, where "unusual" is defined by the alternative hypothesis.  We use the 
##  distribution function for our test statistic in order to calculate this 
##  probability.   Since this sample is so large, we can use the normal 
##  distribution to calculate probabilities:

##  H_A : mu not equal to 51.25

p.value <- pnorm(-t.stat, 0, 1) + 1 - pnorm(t.stat, 0, 1)
p.value

##  H_A : mu greater than 51.25

p.value <- 1 - pnorm(t.stat, 0, 1)
p.value

##  H_A:  mu less than 51.25

p.value <- pnorm(t.stat, 0, 1)
p.value

##  We can use the t.test function when we are testing a hypothesis about a 
##  continuous variable:

t.test(sunspots, mu=mu_0, alternative = "two.sided")
t.test(sunspots, mu=mu_0, alternative = "greater")
t.test(sunspots, mu=mu_0, alternative = "less")

##  For proportions, the approach is slightly different because you don't have 
##  to estimate the variance of the data; the variance is a function of the 
##  null hypothesis.

aa <- rbinom(100, 1, .6)

##  Let's test the hypothesis that pi = .5 against the alternative that pi is not
##  equal to 0.5:

z.stat <- (mean(aa) - 0.5)/sqrt(0.5*(1 - 0.5)/100)
z.stat
pnorm(-z.stat) + 1 - pnorm(z.stat)

##  Either way, we compare our p-value to the level that we set beforehand for
##  the test.  If p-value < alpha, then we reject the null hypothesis.  If 
##  p-value > alpha, we fail to reject the null.

#   3) Comparing two means

##  We can use the t.test function to test hypotheses about the difference 
##  of the means in two samples when the samples are independent:

##  To demonstrate, we'll start by creating two vectors of data drawn from 
##  normal distributions with different means but the same variance

xx1 <- rnorm(10, mean = 4, sd = 5)
xx2 <- rnorm(20, mean = 5, sd = 5)

##  We can then test the null hypothesis that the difference is exactly zero.
##  If our alternative is that the difference is not zero (but could be 
##  positive or negative), then we use the alternative="two-sided" argument.
##  Since we know that the variances are equal in this case, the right thing 
##  to do is to set var.equal=TRUE:

t.test(xx1, xx2, alternative = "two.sided", var.equal = TRUE)

##  We can also see what happens when we don't assume equal variances:

t.test(xx1, xx2, alternative = "two.sided", var.equal = FALSE)

##  Or, we could test a one-sided hypothesis:

t.test(xx1, xx2, alternative = "less", var.equal = TRUE)

t.test(xx1, xx2, alternative = "greater", var.equal = TRUE)

##  In all of these, the null hypothesis is based around the true difference
##  in means being equal to zero.  If instead, our null hypothesis is that the
##  difference in means is equal to 1 (which is true in this case), with the
##  alternative that the difference is not equal to 1, we would use the following
##  syntax:

t.test(xx1, xx2, mu = 1, alternative = "two.sided", var.equal = TRUE)

