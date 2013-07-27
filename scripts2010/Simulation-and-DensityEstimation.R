rm(list=ls())


###########################################################################
# Random sampling functions.

# Sample 32 values from a standard normal distribution.
sampled.values <- rnorm(32)

# If we want to set the mean and standard deviation we can use

sampled.values <- rnorm(32,mean=15.7,sd=3.2)


# R has built-in functions for sampling from many different distributions.
# Try the following help commands.
?rgamma

?rbeta

###########################################################################
# Now let's try an experiment.

# First load some data from the MASS package
library(MASS)
data(cats)

# Enter the command.
str(cats)

# Now plot the data.
library(lattice)
xyplot(Hwt ~ Bwt | Sex,data=cats,type=c('g','p','r'))

# Make a linear model for just the male cats.
lm.cats <- lm(Hwt ~ Bwt,data=cats,subset=Sex=='M')

# Alternatively we could have extracted part of the data set.
male.cats <- subset(cats,Sex=='M')
lm.cats <- lm(Hwt ~ Bwt,data=male.cats)


# and look at the summary.
summary(lm.cats)

# To predict values from the linear model

new.data <- data.frame(Bwt=seq(2,4,length=10000))
predicted.values <-predict(lm.cats, new = new.data, interval="prediction")


# Plot the results along with conifence intervals.

plot(new.data$Bwt,predicted.values[,'fit'],xlab='Bwt',ylab='Hwt',type='l',col='blue')
lines(new.data$Bwt,predicted.values[,'lwr'],xlab='Bwt',ylab='Hwt',col='red')
lines(new.data$Bwt,predicted.values[,'upr'],xlab='Bwt',ylab='Hwt',col='red')

# Now add the data to the plot.

points(male.cats$Bwt,male.cats$Hwt)


# But what about simulating new values?

simulated.residuals <- rnorm(nrow(new.data),mean=0,sd=sd(residuals(lm.cats)))
simulated.values <- predicted.values[,'fit'] + simulated.residuals
points(new.data$Bwt,simulated.values,col='red',pch='.')

############################################################################
# Density Estimation.
#
# Let's take the residuals from our model.
x <- residuals(lm.cats)

# We can construct a an approximate density for x as

density.estimate <- density(x)

# Try looking at the structure of the density.estimate object.

str(density.estimate)

# Compare to the histogram.

par(mfrow=c(1,2))  # This puts both plots side by side.
hist(x,col='plum')
plot(density.estimate)
par(mfrow=c(1,1)) # This resets out plots to be one by one.

# Both density estimates and histograms have extra parameters.  The histogram
# has a parameter for the number of bins (and the placement of the breaks for 
# the bins).  The density estimate has a bandwidth parameter that serves much
# the same function.  Let's try to change both.

par(mfrow=c(1,2))
hist(x,col='plum',breaks=8)
hist(x,col='plum',breaks=5)
par(mfrow=c(1,1))

 
density.estimate.2 <- density(x,bw=0.5)
density.estimate.3 <- density(x,bw=1.0)

par(mfrow=c(1,2))
plot(density.estimate.2,col='blue',lwd=2,main="Bandwidth 0.5")
plot(density.estimate.3,col='blue',lwd=2,main="Bandwidth 1.0")
par(mfrow=c(1,1))


# One more plot ... and some practice using the legend function.
plot(density.estimate.2$x,density.estimate.2$y,col='blue',type='l',lwd=2)
lines(density.estimate.3$x,density.estimate.3$y,col='red',lwd=2)
legend(2.5,0.25,legend=c("bandwidth 0.5","bandwidth 1.0"), col=c("blue","red"),
       lty=1,lwd=2)


