########################################### hi.
## section 5 notes
## october 15, 2009
## gov2k
## prepared by msen :)

## on the agenda:

## Simple linear regression using lm(), 
## sampling for OLS estimates (marginal and joint),
## confidence and preditive bands, 
## detecting outliers and violations of OLS assumptions

########################################### Example 1

## We'll use some data in the car library 
## If you don't have the car library yet, install the package with

## install.packages("car")

## Once the car package is installed, load the car library with 

library(car)

## we'll play around with the Burt dataset within the car library

## This data frame contains the following columns:
## IQbio IQ of twin raised by biological parents
## IQfoster IQ of twin raised by foster parents
## class A factor with levels (note: out of order): high; low; medium.
## (Let's ignore this last one for now)

head(Burt)
	## to get a feed for the data

## In the simple OLS case (i.e. one independent variable) we can plot
## the dep. variable against the indep. variable to gather some 
## insight-by-inspection about how the variables relate

plot(Burt$IQbio, Burt$IQfoster, xlab = "IQ, Raised by Biological Family",
	ylab = "IQ, Raised by Foster Family", pch = 19)

## You know the analytic formulas for the values lm() returns,
## but lm() can save time (and lets you double check your results
## when we won't let you use lm() for your final answer on a problem set)

## The '~' separates your dependent variable from the independent variables.

## You can store the output of lm() to access other useful functions
## and methods

my.lm <- lm(Burt$IQfoster ~ Burt$IQbio)

my.lm

summary(my.lm)

## pause
## let's examine the following on the board
fost <- c(97, 85, 115)
bio <- c(95,83,108)

plot(bio, fost)
lm(fost~bio)	## this is what we calculate by hand

## returning to our example from above:
## To see what we can extract from our lm object 'my.lm', we use

names(my.lm)

coefficients(my.lm)
	## so the equation for the line is burt$IQfoster = 13.21 + 0.86*burt$IQbio

coefficients(my.lm)[1]

coefficients(my.lm)["(Intercept)"]

## let's take a look at the fitted values

names(my.lm)
my.lm$fitted.values

## let's take a look at the residuals, 
## which R calculates as the actual values minus fitted values.

my.lm$residuals

## you can also sum the square of the residuals

sum(my.lm$residuals^2)

## and extrat the R-sq value:

summary(my.lm)
## note -- this is a very high R^2 value, which led people to
## attack Burt and say he was fabricating his data!

## let's plot our regression line onto the plot

plot(Burt$IQbio, Burt$IQfoster, xlab = "IQ, Raised by Biological Family",
	ylab = "IQ, Raised by Foster Family")
abline(my.lm, col = "darkred")

## Let's add the residual lines onto the plot

plot(Burt$IQbio, Burt$IQfoster, xlab = "IQ, Raised by Biological Family",
	ylab = "IQ, Raised by Foster Family", pch = 19)
abline(my.lm, col = "darkred")
segments(x0=Burt$IQbio, x1=Burt$IQbio, y0=Burt$IQfoster, y1=my.lm$fitted, col="red")

########################################### Sampling

##  This came up in lecture and we will go over this on the pset

## let's generate some data and pretend that it's the true population.
## we can do this with the rnorm function

x <- rnorm(5000, mean = 7, sd = 1.56)
	## just some normally distributed data
y <- 12-.4*x + rnorm(5000, mean = 0, sd = 1)
	## we're establishing here a linear relationship,
	## so that y = 12 + .4x + some residual value

## to check out the histograms separately
par(mfrow = c(1,2))
hist(x, col = "cyan")
hist(y, col = "pink")
	## you know they are normal because??

## plotting them together:
par(mfrow = c(1,1))
plot(x, y)
	## you see they are roughly linear

fake.lm <- lm(y~x)
fake.lm

plot(x,y)
abline(fake.lm, col = "coral")
	## and if you really want, you can add the residuals
	## segments(x0=x, x1=x, y0=y, y1=fake.lm$fitted, col="cadetblue")

## let's sample from this the way we did in the univariate statistics part of the course:

data <- data.frame(x,y)
size <- 500

my.samp <- data[sample(nrow(data),size),]
lm(my.samp$y ~ my.samp$x)

## so we can set up a for loop to do this thousands of times,
## and store the info

sims <- 1000
size <- 500
holder <- matrix(data = NA, ncol = 2, nrow = sims)
colnames(holder) <- c("intercept", "slope")
for(i in 1:sims){
	my.samp <- data[sample(nrow(data),size),]
	samp.lm <- lm(my.samp$y ~ my.samp$x)
	holder[i,1] <- samp.lm$coefficients[1]
	holder[i,2] <- samp.lm$coefficients[2]
}
	## this could take a while depending
	## on the number of simulations you're performing!

## and now we can plot the marginal distribution
## of the slope and intercept:

par(mfrow = c(1,2))
plot(density(holder[,1]), main = "intercept")
abline(v = mean(holder[,1]))
plot(density(holder[,2]), main = "slope")
abline(v = mean(holder[,2]))

## two questions:
##	1. why do they look normal? (and why does that make sense?)
##	2. what's the mean of these two distributions? (and why does that make sense?)

## let's look at the joint sampling distribution
par(mfrow = c(1,1))
plot(holder[,1], holder[,2], xlab = "intercept", ylab = "slope",
	main = "Joint Sampling Distribution of Slope and Intercept")

## and also
plot(x,y, main = "Sampling Distribution of Regression Lines")
segments(x0=0, x1=100, y0=holder[,1], y1=(holder[,1] + 100*holder[,2]), col="red")

## another way of doing the same plot
plot(x,y, main = "Sampling Distribution of Regression Lines")

for (i in 1:nrow(holder)) {
 	abline(a=holder[i,1], b=holder[i,2], col = "purple")
}

## you can also used the predict (predict.lm) function to draw up the
## confidence bands and predictive bands that Adam did in lecture:

ruler <- data.frame(x = c(1:12))
predict(lm(y ~ x))
predict(lm(y ~ x), ruler, se.fit = TRUE)
	## setting things up

pred.pred <- predict(lm(y ~ x), ruler, interval="prediction")
pred.conf <- predict(lm(y ~ x), ruler, interval="confidence")
	## calculating the values

## and now to creat plots like the ones Adam has:

## for the predictive bands
matplot(ruler, pred.pred, type="l")
	## matplot will the columns of one matrix against the columns of another
points(x,y)
	## if you want to overlay the observations

## for the confidence bands
matplot(ruler, pred.conf, type="l")
points(x,y)

########################################### Diagnostics

## You can also perform some of the diagnostics we discussed in lecture
## very simply.

## Let's do this with the Leinhardt data, also
## from the car library

## This data frame contains the following columns: 
## income: Per-capita income in U. S. dollars. 
## infant: Infant-mortality rate per 1000 live births. 
## region: A factor with levels: Africa; Americas; Asia, Asia and Oceania; Europe. 
## oil: Oil-exporting country. A factor with levels: no, yes. 
	## we will disregard the last two vars

summary(Leinhardt)
head(Leinhardt)

Leinhardt <- na.omit(Leinhardt)

income <- Leinhardt$income
infant <- Leinhardt$infant

plot(income, infant)
	## linear?

my.lm <- lm(infant ~ income)
	## fitting a linear regression model

plot(income, infant)
abline(my.lm, col = "darkred")
	## plotting the two together

## let's do some diagnostics

######################## leverage

## The "hat" values are a measure of whether 
## the observation is far from the center of the distribution 
## of the explanatory variable (i.e., has leverage). 
## Large hat values are an indication that the observation is potentially influential. 

## to calculate them, we can use the "hatvalues"
## function from the car library

hatvalues(my.lm)

## more useful is to plot these to get a sense 
## of what's going on

plot(income, hatvalues(my.lm))
abline(h = 4/length(income), col = "darkred")
	## looks like there might be some problems
identify(income, hatvalues(my.lm), tolerance = "10")

######################## influence points

stud.res <- rstudent(my.lm) 
	#calculates studentized residuals

plot(hatvalues(my.lm), rstudent(my.lm))
	## plots hat values against studentized residuals
abline(h = 2, col = "darkred")
abline(v = 4/length(income), col = "darkred")

######################## non-linearity
plot(income, infant)
plot(my.lm,1) 	## here, you are looking for the red line to be relatively flat

######################## non-constant error variance

plot(my.lm,3) ## Again you want the line to be more or less flat across the plot
		  ## if it's sloped, then we might have non-constant error variance

######################## non-normality

## we can look at a histogram of the residuals
hist(my.lm$residuals) 	## ugh

## or, better yet, we can look at a QQ plot of the residuals
## following the lecture notes, we 
##	1. extract the residuals from the lm output
##	2. sort them
##	3. normalize them
##	4. plot them in a qqplot against a standard normal

my.lm <- lm(income ~ infant)
residuals <- sort(my.lm$residuals)
residuals <- (residuals - mean(residuals))/sd(residuals)
theoretical <- rnorm(length(residuals))

qqplot(theoretical, residuals)
	## ouch.


########################################### FNS that return matrixes

## There is nothing special about returning a matrix with a function

## Here is a function (unrelated to this week's problem set)
## which shows the mechanics

my.function <- function(x,y){ #x and y are vectors
 
  mat <- matrix(data = NA, nrow = 2, ncol = 3)
 
  x.sum <- sum(x)
  mat[1,1] <- x.sum

  y.sum <- sum(y)
  mat[1,2] <- y.sum

  xy.sum <- sum(x.sum, y.sum)
  mat[1,3] <- xy.sum

  ## You can also fill whole rows (or columns) at once with a vector:

  mat[2,] <- c(mean(x), mean(y), mean(c(x,y)))

  colnames(mat) <- c("x calculations", "y calculations", "x and y calculations")
  rownames(mat) <- c("sum", "mean")

  return(mat)
}


my.function(x = c(1:10), y = c(23:40))


fun.out <- my.function(x = c(1:10), y = c(23:40))

fun.out[1,2]
fun.out["sum", "y calculations"]


