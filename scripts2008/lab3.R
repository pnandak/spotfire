# QERM514 Lab #3 (4/12/07)
# The purpose of this lab is to cover:
# - case statistics
# - Fit linear model using subset of the data.
# - using the boxcox function to determine an appropriate transformation
#   of the response variable
#   

####################################################################
setwd("c://qerm514")
# Download the woodbeam data (from Hoaglin & Welsch) and read it into 
#  a dataframe object.
woodbeam.df<-read.table("woodbeam.dat",header=T)
attach(woodbeam.df)

pairs(woodbeam.df)


# Now, let's do some work with the leverages and the hat matrix
# There are many useful features and functions enabled by the call
library(MASS)
help(package="MASS")

# in particular, all of the examples from Venables and Ripley (2002)
#  are now available, including many of their own functions.  


# The model here a regression of strength as a function of specfic 
#  gravity and moisture.  So, create the linear model:
woodbeam.lm<-lm(strength ~ specgrav + moisture, data=woodbeam.df)

# manually create the entire hat matrix
X<-model.matrix(woodbeam.lm)
woodbeam.hat<-X%*%solve(t(X)%*%X)%*%t(X)

# and just extract the diagonal elements
diag(woodbeam.hat)

# Unfortuately, this is a lot of work!  There are two other ways to get
#  this data. 

# 1) The first (this is how it is done in the notes) is to create a 
#  least squares fit object.  Note that if the X data is multidimensional
#  you need to pass a vector of the X data.  
woodbeam.ls<-lsfit(woodbeam.df[,(1:2)], woodbeam.df$strength)
# and look at the diagnostic information from this object
ls.diag(woodbeam.ls)
# This is the best way to get all of the diagnostic information

# 2) Since we've already created the linear model, we can use those
#  results and the lm.influence() command to extract an abbreviated  
#  set of results.  lm.influence() is specially designed to examine each X point's infleuence 
#   on the linear model.
lm.influence(woodbeam.lm)
lm.influence(woodbeam.lm)$hat
# See the help for lm.influence for more information

# Also, the MASS library contains commands for extracting both the 
#  standardized and studentized (deleted) residuals.  Compare these 
#  results with the above results from ls.diag()
studres(woodbeam.lm)
stdres(woodbeam.lm)
hat(X)

# Again, the only advantage to using these (i think) is that you don't
#  need the extra step of creating the lsfit() object.  There are no
#  built in commands to compute either the dfits or cooks residuals, 
#  directly from the lm object, but... Cook's distance and other useful 
#  information can be obtained from:
par(mfrow=c(2,2))
plot(woodbeam.lm)
# See the help for plot.lm for explanations of each of the pages of the
#  resulting plot.  

####################################################################

# Since data point 1 has big influence on the fitted linear model, we might be intersted to fit a model 
# without this point.
# an easy way to fit a model excluding a subset of the data (in the 
# example below excluding data point #1) use:
woodbeam.lm3<-lm(strength~specgrav+moisture, data=woodbeam.df, subset=-1)
# To exclude multiple points use: subset=-c(1, 4)


####################################################################
# Now, let's play with the boxcox function to transform the RESPONSE 

#  To see the available help and function syntax type:
?boxcox

# First simulate some data by yourself.   
b0<-2
b1<-1	
lambda<-.5			
	
x<-rnorm(40, mean=5, sd=2.5)
# note, to make the boxcox transformation
# work consistently, the range of x's needs to be somewhat large!
e<-rnorm(40, mean=0, sd=1)
#we want to simulate y such that y^lamda (NOT y itself)has a linear relationship with x
# i.e.  y^lamda=b0+b1*x+e

y<-(b0+b1*x+e)^(1/lambda)
my.df<-data.frame(x=x,y=y)
my.df
# First plot the data, create the linear model and plot the residuals
par(mfrow=c(1,2))
plot(my.df$x, my.df$y)
my.lm<-lm(y~x, data=my.df)
abline(my.lm)
plot(my.lm$fitted, my.lm$resid)
abline(h=0)

# Can we transform y to fit a linear model?
# We can, and you know that answer is to use y^lamda.
# So, let's try whether the boxcox transformation can find the lamda for you:

win.graph()		# opens a new window for plotting
boxcox(y~x, data=my.df, lambda = seq(-1, 4, .25), plotit=T)

# So, the maximum occurs (roughly) where lambda ~= .5  (Note, there  
#  may be some amount of variation in these results...)

# Plot the transformed response data vs. the predictor data,  
#  make a new linear model and look at the new residuals...
win.graph()		# opens a new window for plotting
par(mfrow=c(1,2))
plot(my.df$x, my.df$y^0.5)
my.lm2<-lm(y^0.5~x, data=my.df)
abline(my.lm2)
plot(my.lm2$fitted, my.lm2$resid)
abline(h=0)

# Play with other values of lambda to see what happens!  Note: if you 
#  want to experiment with lambda=0, you need to explicitly log transform 
#  the data

####################################################################

