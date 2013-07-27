# POISSON DISTRIBUTION WITH MU=2
#
# How to obtain a sequence from 0 to 10:
0:10
#
# Calculate p(y) for y=0,1,...,10 when mu=2
# (dpois() is the Poisson probability function or "density"):
dpois(0:10, 2)
#
# Make the output prettier:
round(dpois(0:10, 2), 3)
#
# ppois() is the cumulative distribution function, P(Y <= y), for
# the Poisson distribution.  Find P(Y <= 6) when mu=2:
ppois(6, 2)
#
# Using ppois() is faster than adding up the probabilities, which could
# be done by using the sum() function (this should yield the same
# value as the last calculation):
sum(dpois(0:6, 2))
#
# Find P(Y>6) when mu=2:
1 - ppois(6, 2)
#
# cbind() (column bind) takes vectors of equal length and makes them
# into matrices with the vectors as columns (guess what rbind does).
# Make a table of the first 11 Poisson probs and cumulative probs when
# mu=2:
cbind(0:10, dpois(0:10,2), ppois(0:10,2))
#
# Make the output prettier:
round(cbind(0:10, dpois(0:10,2), ppois(0:10,2)), 3)
#
# Plot the probabilities (type="h" makes this particular type of plot
# with the vertical lines):
plot(0:10,dpois(0:10,2),type="h")
#
# Put some labels onthe axes and give the plot a title:
plot(0:10,dpois(0:10,2),type="h",xlab="y",ylab="p(y)",main="Poisson Distribution (mu=2)")

