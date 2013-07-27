


# first we'll need to load the car library

> library("car")

Attaching package `car':


	The following object(s) are masked from package:base :

	 dfbetas rstudent 

# we'll also want to load the modreg library so that we can use the 
# kmsooth() function

> library(modreg)


# let's look at the Duncan occupational prestige data
# first we load the data 
> data(Duncan)


# let's take a quick look at the first 3 rows to make sure the 
# dataset is what we think it is

> Duncan[1:3,]
           type income education prestige
accountant prof     62        86       82
pilot      prof     72        76       83
architect  prof     75        92       90

# ok, looks fine. Let's look at the relationship between education and 
# prestige. First we'll just draw a scatterplot of prestige on education:

> plot(Duncan$education, Duncan$prestige)

# looks like a positive, basically linear relationship between education 
# (measured as percent of males in occupation in 1950 who were
# high-school graduates) and prestige (measured as percent of raters
# in NORC study rating occupation as excellent or good in prestige). 
#
# What can we say about the conditional expectation of prestige given
# education (E[prestige|education])? Let's look at some nonparametric 
# estimates of this conditional expectation function. 
#
# First we'll look at a simple running mean smoother. 
# 

> lines(ksmooth(Duncan$education, Duncan$prestige, kernel="box",
  bandwidth=20), col="red") 

# this red line is an estimate of E[prestige|education]. If we set the
# bandwidth parameter higher we will get a smoother curve as more
# data is falling withing each window. For instance:

> lines(ksmooth(Duncan$education, Duncan$prestige, kernel="box",
  bandwidth=40), col="red", lty=2) 

# estimates a running mean with a wider bandwidth and plots it with a
# dashed red line. Note it is still not very smooth. This is often a
# problem with simple local averaging methods such as the method above
# that weight each observation in the window equally. A better way to
# proceed is to weight observations by their distance from the point
# of interest. The lowess function provides one way to do this:


> lines(lowess(Duncan$education, Duncan$prestige, f=.25, iter=0),
  col="blue") 

# the f parameter is the span and acts similarly to the bandwidth
# parameter to ksmooth(). Setting span closer to 1.0 will result in a
# smoother curve:


> lines(lowess(Duncan$education, Duncan$prestige, f=.5, iter=0),
  col="blue", lty=2) 

# the iter parameter controls how many "robustifying" iterations are 
# performed to downweight vertical outliers. Setting this parameter to
# a value around 3-5 will usually be enough to produce reasonably
# robust estimates

> lines(lowess(Duncan$education, Duncan$prestige, f=.5, iter=5),
  col="green")


