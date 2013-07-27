############################################

#   Goverment 1001
#   Section 2 - Probability Distributions
#   February 15, 2007
#   
#   Prof: Adam Glynn
#   TF: Mike Kellermann

############################################


#   Goals for today:

#   1) Using the probability distributions in R

#   2) Writing for loops

#   3) Some other useful functions

#   4) Some examples

############################################

#   1) Using the probability distributions in R

#   One of the strengths of R is its functionality for working with 
#   probability distributions.  There are predefined functions for many
#   important families of distributions, including all of the ones that we 
#   will need in this course.  For each family of distributions, there are
#   four functions; we will illustrate this using the normal distribution, 
#   which is the most important of the continuous distributions.

#   A) The normal distribution

#   The normal distribution is continuous and symmetric, and is governed by
#   two parameters: the mean, which determines where the distribution is 
#   centered, and the standard deviation, which determines how spread out the 
#   distribution is.  

#   The most important member of the set of normal distributions is the 
#   "standard normal" distribution, which has mean 0 and standard deviation 1.

#   We can calculate the value of the density of a normal distribution at any 
#   point by using the dnorm() function.  For a continuous function, the density 
#   function has higher values in areas of higher probability, but the height of
#   the density at a number does not represent the probability of obtaining that 
#   number:

dnorm(0, 0, 1)
dnorm(1, 0, 1)
dnorm(0, 0, .1)

#   Since probability is the area under the density curve, it is often useful 
#   to plot the density of a distribution that you are working with.  To do that 
#   in R, you first need to create a set of points at which to evaluate the density.
#   Then you can plot (using type="l") as an option, as follows:

xx <- -600:600/100
plot(xx, dnorm(xx, 0, 1), type="l")
lines(xx, dnorm(xx, 0, 2), col="red")
lines(xx, dnorm(xx, 1, 1), col="blue")
lines(xx, dnorm(xx, 1, 2), col="green")

#   One of the nice things about the normal is that the center and spread of the 
#   distribution are independent; you can shift one without shifting the other.
#   This isn't always true.

#   In lecture, we also talked about the cumulative distribution function for a 
#   random variable.  That is, for a point xx, what is the probability that the 
#   random variable is less than xx.  The CDF is an increasing function (why?) and
#   is bounded between 0 and 1.

#   You can calculate the CDF of a random variable in R:

pnorm(0, 0, 1)
pnorm(1, 0, 1)
pnorm(0, 0, .1)

#   You may also want to plot the CDF:

xx <- -600:600/100
plot(xx, pnorm(xx, 0, 1), type="l")
lines(xx, pnorm(xx, 0, 2), col="red")
lines(xx, pnorm(xx, 1, 1), col="blue")
lines(xx, pnorm(xx, 1, 2), col="green")

#   CDFs are most useful for calculating the probability that the RV falls within
#   some interval.  What is the probability that a draw from a standard normal
#   variable is between -1 and 1?   -2 and 2?

pnorm(1, 0, 1) - pnorm(-1, 0, 1)
pnorm(2, 0, 1) - pnorm(-2, 0, 1)

#   A third set of functions is the inverse of the CDF, or the quantile function.
#   This asks the question, "where do I have to be on the scale of the random 
#   variable so that xx % of the draws are smaller than that value?

qnorm(.975, 0, 1)
qnorm(.2, 0, 1)

#   Obviously, xx has to be between 0 and 1.
#   qnorm() and pnorm() are inverses of each other.

xx <- c(1,2,3,4,5)
qnorm(pnorm(xx, 0, 1))

#   Finally, the fourth function associated with the normal distribution is rnorm.
#   This function generates random draws from the normal distribution with paramenters
#   that you specify.  The first argument is the number of draws to take, the 
#   second is the mean, and the third is the standard deviation.

aa <- rnorm(1000, 0, 1)
hist(aa, freq=FALSE)
aa <- rnorm(10000, 0, 1)
hist(aa, freq=FALSE)
xx <- -600:600/100
lines(xx, dnorm(xx, 0, 1))

#
#   B) The uniform distribution

#   Another common continuous distribution is the uniform distribution, in which
#   the density is flat over some interval.  The functions in R follow the same 
#   form, but instead of setting the mean and variance, they set the lower and 
#   upper bounds of the uniform.

#   dunif() isn't very interesting, since it is the same for all values in the 
#   range.  We can still look at a few uniform distributions, however:

xx <- -600:600/100
plot(xx, dunif(xx, 0, 1), type="l", ylim=c(0, 2.5), xlim=c(-.5, 2.5))
lines(xx, dunif(xx, 0, 2), col="red")
lines(xx, dunif(xx, .5, 1), col="blue")
lines(xx, dunif(xx, .75, 2.25), col="green")

#   We can also look at the CDFs; these will all be straight lines with slope 
#   equal to 1/(b-a), where b and a define the interval.

plot(xx, punif(xx, 0, 1), type="l", ylim=c(0, 1), xlim=c(-.5, 2.5))
lines(xx, punif(xx, 0, 2), col="red")
lines(xx, punif(xx, .5, 1), col="blue")
lines(xx, punif(xx, .75, 2.25), col="green")

#   The uniform distribution is probably used most often to generate random
#   numbers:

hist(runif(100,0, 2), freq=FALSE)
hist(runif(1000,0, 2), freq=FALSE)
hist(runif(10000,0, 2), freq=FALSE)


#   Other continuous distributions that we will talk about in class:

#   C) The Chi-square distribution: this distribution has only one parameter
#   which is known as the degrees of freedom.  It can only take on positive values 

xx <- 0:600/100
plot(xx, dchisq(xx, 1), type="l", ylim= c(0,2), xlim=c(0,6))
lines(xx, dchisq(xx, 2), col="red")
lines(xx, dchisq(xx, 3), col="blue")
lines(xx, dchisq(xx, 4), col="green")

#   D) The t-distribution: this distribution also has only one parameter, which is
#   also known as the degrees of freedom.  It can take on values over the whole
#   real number line, and is symmetric.  In fact, it looks a bit like the normal 
#   distribution (which we'll talk about more in class).

xx <- -600:600/100
plot(xx, dt(xx, 1), type="l", ylim= c(0,.5), xlim=c(-6,6))
lines(xx, dt(xx, 2), col="red")
lines(xx, dt(xx, 5), col="blue")
lines(xx, dt(xx, 10), col="green")

#   E) The F-distribution:  this distribution has two parameters.  It only takes on 
#   positive values.

xx <- 0:600/100
plot(xx, df(xx, 1,3), type="l", ylim= c(0,1), xlim=c(0,6))
lines(xx, df(xx, 2,2), col="red")
lines(xx, df(xx, 5,20), col="blue")
lines(xx, df(xx, 10,2), col="green")

#   F)  Two important (and related) discrete distributions: the Bernoulli 
#   and the binomial

#   When you have binary data (anything that can be thought of as zero or one),
#   it is usual to model that as a Bernoulli random variable.  This assumes that 
#   you get 1 with probability p and 0 with probability (1-p).  Imagine you did 
#   this n times; what would the distribution of the sum of those Bernoulli random
#   variables be?  You know that it can take on discrete values representing the 
#   number of successes: 0 sucesses, 1 sucess, 2 sucesses, etc., up through n 
#   sucesses.  A random variable with this distribution is known as a binomial 
#   RV with parameters n (the number of trials) and p (the probability of success).
#   Since a Bernoulli is a binomial with n=1 and p=p, there is only one set of 
#   functions in R for these two random variables.

#   Notice that, because these are discrete variables, dbinom gives you the 
#   probability mass function, not the density.

dbinom(0,size=1,p=.4)
dbinom(1,size=1,p=.4)
 
pbinom(0, size=1, p=.4)
pbinom(1, size=1, p=.4)

#   Again, this may be the most useful for generating Bernoulli random variables.
#   Let's say that I want 1000 Bernoulli RVs with probability 2/3:

yy <- rbinom(1000, size=1, p=2/3)
mean(yy)
hist(yy)


#   2)  Writing for loops

#   We often want R to do the same (or similar) things over and over again.  
#   Having to sit there and resubmit a command over and over again is tiresome, 
#   as many of you probably found out on the first problem set.  R lets you 
#   automate many of these repetitive tasks using for loops.

#   You start a for loop by using the for() command.  In it, you tell R
#   how many times to loop through the code that you put in brackets immediately
#   after the command.

for(ii in 1:10){
}

#   There, we just did a for loop!  Not a very exciting for loop, but it still 
#   works.  Let's see something a bit more interesting.

for(ii in 1:10){
ii
}

#   Why didn't anything happen?  If I type

ii

#   at the command propmt, it prints a value.  It turns out that R does not print
#   output to the console inside a for loop unless you tell it to.  Try this:

for(ii in 1:10){
print(ii)
}

#   That's better.  At each iteration, it prints out the value of ii, which is our
#   indicator for the loop.  Now, what if I try this?  How many times will I go 
#   through the loop?  What will print out?

for(ii in c(2,4,5,7)){
print(ii)
}

#   Again, while printing is fun, we usually want to save the output that is generated
#   in our loop.  A couple of rules of thumb:
#      1) You can create objects within the loop, but those objects will be recreated
#         each time you cycle through the loop.


for(ii in c(2,4,5,7)){
dd <- ii
print(dd)
}

#   	 2) Therefore, if you want to save output from each iteration of the loop,
#         you need to create a variable or variables outside of the loop.  You also
#         need to assign the output from each iteration of the loop to a different 
#         spot.  The traditional way of doing this is the indexing operator.

dd <- rep(0, 4)
for(ii in c(2,4,5,7)){
dd[ii] <- ii^2
print(dd)
}
dd

#  Why is it behaving like this?  What if I just want to square the four numbers and 
#  get a vector of length 4?

dd <- rep(0, 4)
ee <- c(2,4,5,7)
for(ii in 1:length(ee)){
dd[ii] <- ee[ii]^2
print(dd)
}
dd

#   So, what about something more practical?  Draw 1000 samples from a standard normal,
#   calculate the standard deviation, and repeat 1000 times.

nn <- 1000
samp <- 1000
draws <- rep(0, nn)
for(ii in 1:nn){
xx <- rnorm(samp)
draws[ii] <- sd(xx)
}

mean(draws) 
hist(draws)


#   3) Some other useful functions

#   We have talked about different types of data objects in R: numeric 
#   vectors, character vectors, factors, and data frames.  Another type 
#   of object that is often useful is a matrix.  In R, a matrix is a 
#   two-dimensional data structure that looks like a data frame (but isn't).
#   The main difference between the two is that all of the columns in the 
#   matrix must be the same type (that is, all numeric or all character or ...),
#   whereas a data frame can have columns of different types.  We'll mostly
#   use data frames, but you should know how to create matrices.

matrix(1, nrow = 3, ncol = 4)
matrix(1, nrow = 4, ncol = 3)
matrix(rnorm(1000), nrow=100, ncol=10)
aa <- matrix(rnorm(1000), nrow=100, ncol=10)

#   Another thing that we often want to do is to use the same function separately 
#   on every column of a matrix or data frame.  We can use the apply() function
#   to do this.  Say (hypothetically) that we want to take the standard deviation
#   of each of the variables in the Florida dataset.

library(car)
data(Florida)
apply(Florida, 2, sd)

#   The apply function takes three arguments.  The first is the data frame or 
#   matrix that you want to work on.  The second is a number that tells R whether
#   to apply the function across rows or columns (1 for rows, 2 for columns). The
#   third is the name of the function that you want to apply to the rows or columns.
#   So, how would you calculate the mean of each of the variables in the Florida
#   dataset? 




#   4) An example: maximum wind speeds (or why you shouldn't live in Florida)

#   In hurricanes, most of the damage due to wind is a function of the maximum 
#   wind speed that is experienced by an area, not by the length of the exposure.
#   So, it is useful to know what the highest wind speed is that you are likely
#   to get in a certain period of time.

#   Let's assume for the sake of argument that the daily maximum wind speed in 
#   a certain part of Florida is independent and identically distributed 
#   according to a exponential distribution
#   with parameter 1/10 (you don't need to know about the exponential distribution,
#   but it works well for this example).
#   First, some questions:
#   1)  What does the distribution of daily max wind speeds look like?

plot(0:2000/10, dexp(0:2000/10, 1/10), type = "l")

#   2)  Estimate the mean daily maximum wind speed:

mean(rexp(10000, 1/10))

#   OK, so the mean daily maximum wind speed doesn't seem so bad.  But what is 
#   the mean maximum wind speed over the course of a year?

years <- 1
draws <- rep(0, 1000)
for(ii in 1:1000){
xx <- rexp(365*years, 1/10)
draws[ii] <- max(xx)
}
mean(draws)

#   So that's still not so bad; what is the probability that you will experience
#   hurricance force winds over the course of a year?

mean(draws >= 75)

#   So, appox. a 1 in 5 chance of getting a hurricane.  Not great, but not terrible.
#   Most people don't live in Florida for just a year.  What about 10 years?

years <- 10
draws <- rep(0, 1000)
for(ii in 1:1000){
xx <- rexp(365*years, 1/10)
draws[ii] <- max(xx)
}
mean(draws)

mean(draws >= 75)

#   How about 100 years? (This may take a while)

years <- 100
draws <- rep(0, 1000)

for(ii in 1:1000){
xx <- rexp(365*years, 1/10)
draws[ii] <- max(xx)
}

mean(draws)

mean(draws >= 75)

mean(draws >= 130)





