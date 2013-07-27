R : Copyright 2001, The R Development Core Team
Version 1.4.0  (2001-12-19)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type `license()' or `licence()' for distribution details.

R is a collaborative project with many contributors.
Type `contributors()' for more information.

Type `demo()' for some demos, `help()' for on-line help, or
`help.start()' for a HTML browser interface to help.
Type `q()' to quit R.

# let's create a variable x that is a random sample of size 500
# from a standard normal distribution and plot the histogram of x

> x <- rnorm(500)
> hist(x)

# now let's create a variable y that is equal to exp(x) plus a normal
# random variable with mean 2.5 and variance 3 and plot y on x

> y <- exp(x) + rnorm(500, 2.5, sqrt(3))
> plot(x,y)

# we can add a loess smooth (see the Cleveland book) to this plot
# using the loess.smooth() function in the modreg package. To use the 
# loess.smooth() function we first load the modreg library

> library(modreg)

# to see what functions are in the modreg package we can type

> library(help=modreg)

modreg          Modern Regression: Smoothing and Local Methods

Description:

Package: modreg
Version: 1.4.0
Priority: base
Title: Modern Regression: Smoothing and Local Methods
Author: Brian Ripley.
Maintainer: R Core Team 
Description: Modern regression: smoothing and local methods
License: GPL Version 2 or later.

Index:

ksmooth                  Kernel Regression Smoother
loess                    Local Polynomial Regression Fitting
loess.control            Set Parameters for Loess
loess.smooth             Simple Smoothing via Loess
plot.ppr                 Plot Ridge Functions for Projection Pursuit
                           Regression Fit
ppr                      Projection Pursuit Regression
predict.loess            Predict Loess Curve or Surface
predict.smooth.spline    Predict from Smoothing Spline Fit
scatter.smooth           Scatter Plot with Smooth Curve Fitted by Loess
smooth.spline            Fit a Smoothing Spline
supsmu                   Friedmans's SuperSmoother


# OK, to get help on the loess.smooth() function we could type

> ?loess.smooth

scatter.smooth            package:modreg            R Documentation

Scatter Plot with Smooth Curve Fitted by Loess

Description:

     Plot and add a smooth curve computed by `loess' to a scatter plot.

Usage:

     scatter.smooth(x, y, span = 2/3, degree = 1,
         family = c("symmetric", "gaussian"),
         xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
         ylim = range(y, prediction$y), evaluation = 50, ...)
     loess.smooth(x, y, span = 2/3, degree = 1,
         family = c("symmetric", "gaussian"), evaluation=50, ...)

Arguments:

       x: x coordinates for scatter plot.

       y: y coordinates for scatter plot.

    span: smoothness parameter for `loess'.

  degree: degree of local polynomial used.

  family: if `"gaussian"' fitting is by least-squares, and if
          `family="symmetric"' a re-descending M estimator is used.

    xlab: label for x axis.

    ylab: label for y axis.

    ylim: the y limits of the plot.

evaluation: number of points at which to evaluate the smooth curve.

     ...: graphical parameters.

Details:

     `loess.smooth' is an auxiliary function.

Value:
       x: x coordinates for scatter plot.

       y: y coordinates for scatter plot.

    span: smoothness parameter for `loess'.

  degree: degree of local polynomial used.

  family: if `"gaussian"' fitting is by least-squares, and if
          `family="symmetric"' a re-descending M estimator is used.

    xlab: label for x axis.

    ylab: label for y axis.

    ylim: the y limits of the plot.

evaluation: number of points at which to evaluate the smooth curve.

     ...: graphical parameters.

Details:

     `loess.smooth' is an auxiliary function.

Value:

     None.

Author(s):

     B.D. Ripley

See Also:

     `loess'

Examples:

     data(cars)
     attach(cars)
     scatter.smooth(speed, dist)
     detach()


# OK, so let's use this on our simulated data, since we already have
# scatterplot displayed we can use the lines() function to add a line 
# corresponding to the loess smooth


> lines(loess.smooth(x,y))
Warning message: 
k-d tree limited by memory. ncmax= 500 

# We'll ignore the warning message for now. To get a thicker line that
# is red we could type

> lines(loess.smooth(x,y), lwd=4, col="red")
Warning message: 
k-d tree limited by memory. ncmax= 500

# We can also change the span to reduce the amount of smoothing

> lines(loess.smooth(x,y, span=.3), lwd=4, col="blue")
Warning message: 
k-d tree limited by memory. ncmax= 500

# Let's work with a real dataset-- the Swiss cantons data. First we
# have to load the dataframe

> data(swiss)

# let's look at the first 3 rows of the dataframe to see what
# variables are in the dataframe

> swiss[1:3,]
             Fertility Agriculture Examination Education Catholic
Courtelary        80.2        17.0          15        12     9.96
Delemont          83.1        45.1           6         9    84.84
Franches-Mnt      92.5        39.7           5         5    93.40
             Infant.Mortality
Courtelary               22.2
Delemont                 22.2
Franches-Mnt             20.2


# now let's plot Fertility on Education

> plot(swiss$Education, swiss$Fertility)

# we could also do accomplish the same thing by first attaching the 
# swiss dataframe and omitting the name of the dataframe in the call
# to the plot() function. 

> attach(swiss)
> plot(Education, Fertility)

# let's add a loess smooth

> plot(Education, Fertility)
> lines(loess.smooth(Education, Fertility), lwd=2, col="red")

# we can also use R much like a pocket calculator. for instance if we
# want to calculate the natural logarithm of a number (say 3.42)
# we can do this by typing

> log(3.42)
[1] 1.229641

# similarly we can raise the number e to some power with the exp()
# function

> exp(1.229641)
[1] 3.420002

# to add a bunch of numbers we could type

> 238.4 + 8243 + 37.2 + 832.8 + 237
[1] 9588.4

# you get the picture. 

# R also has facilities for evaluating a number of probability
# (density) functions, distribution functions, and quantile functions,
# and for drawing pseudo-random samples from a number of standard 
# probability distributions.
# 
# to take a pseudo-random sample of size 300 from a binomial 
# distribution with parameters n=10 and pi=.85 and assign it to a 
# variable z we could type

> z <- rbinom(300, 10, .85)

# to evaluate the binomial probability function with parameters 
# n=10 and pi = .85 at the point 4 we could type

> dbinom(4, 10, .85)
[1] 0.001248655

# evaluating the distribution function of the same distribution at the
# point 4 gives us

> pbinom(4, 10, .85)
[1] 0.001383235

# Section 8 of the "An Introducation to R" available at CRAN 
# http://cran.r-project.org/ provides a wealth of information about 
# about probability distributions in R

