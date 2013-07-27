# plotting.R provides some simple examples of how to plot functions in R
#
# Kevin Quinn
# Dept. of Political Science and CSSS
# quinn@stat.washington.edu
#

# let's put 4 graphs on 1 page
par(mfrow=c(2,2))


# plot the cosine function on [0,10]
x <- 0:100/10
plot(x, cos(x), type="l")

# plot exp(x) on [0,10]
plot(x,exp(x), type="l")

# plot the beta(3,4) density function on [0,1]
x <- 0:100/100
plot(x, dbeta(x,3,4), type="l")

# plot a normal(0,4) density on [-10,10]
x <- -100:100/10
plot(x, dnorm(x,0,2), type="l")
