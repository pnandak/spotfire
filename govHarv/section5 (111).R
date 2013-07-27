##
## section5.R
## 22 Oct 08
## mb.
##


## Today we'll be going over some plotting commands. Namely, scatterplots.
## Scatterplots are a very informative way to display the relationship between
## two variables. 

## First we'll use a dataset by Donno and Russett in a 2004 paper
## entitled "Islam, Authoritiarianism, and Female Empowerment: What
## Are the Linkages?" which contains the average Polity score of a country
## over 1991-2000 along with the average of its neighbors' Polity scores
## over the same period. We also have the log od GDP per capita from 1990.

dr <- read.table(url("http://www.people.fas.harvard.edu/~blackwel/donnoRussett.dat"))

head(dr)

###########################
## Scatterplot Smoothing ##
###########################


## Adam covered a bunch of scatterplot smoothers. Basically, all of these
## try to summarize the relationship between X and Y. A convenient function
## in R is the scatter.smooth() function, which plots a scatterplot along
## a loess smooth on top of it. You can control the degree of smoothing with
## the "span" argument. the lower it is, the loess fits the data more closely
## in the sample, but might be bad out of sample (lower bias, higher variance). 

args(scatter.smooth)

scatter.smooth(x=dr$polity, y=dr$lgdp)

## loess.smooth() acts exactly like density() but for two variables, so we
## can add it to an existing plot with lines().

lines(loess.smooth(x=dr$polity, y=dr$lgdp, span = 10), col="red")
lines(loess.smooth(x=dr$polity, y=dr$lgdp, span = .25), col="green")






##############
## Identify ##
##############

## we're going to learn how to put observation labels on scatter plots!

## To illustrate this point (and focus on how regression is a predictive
## model), we'll use a fun dataset, which contains the vote shares for each
## candidate in presidential election since 1892 along with the change
## in real disposible income the year before the election and the turnout
## in the election. 

elec <- read.csv(url("http://www.people.fas.harvard.edu/~blackwel/rdi.csv"))

elec

plot(x=elec$rdichange, y=elec$Incvote, pch=19, col="orange",
     xlab="Percent Change in Real Dispossble Income in year before election",
     ylab="Percent of Presidential Two-Party Vote Share for Incumbent Party",
     main="Economic Election Predictions")



## First we'll add a regression line. To do this, we'll use the abline()
## command. If instead of h or v, we give it an "a" and "b" argument,
## it will draw a line with y=a+bx. That is, "a" will be the intercept
## and "b" will be the slope

## coef(lm()) is getting the coefficients for this regression. We'll
## go over this more in the coming weeks. note that you can't use this
## function for problem set 4. 

coefs <- coef(lm(Incvote ~ rdichange, data=elec))
coefs


abline(a=coefs[1], b=coefs[2], col="orange")

##
## Let's use this regression to predict a certain perhaps relevant
## election. I've included the rdichange for last year in the dataset.
## Let's use that to make a prediction about the vote share we expect
## mccain to get

coefs[1] + coefs[2]*elec$rdichange[elec$year==2008]

elec$Incvote[elec$year==2008] <- coefs[1] + coefs[2]*elec$rdichange[elec$year==2008]

points(x=elec$rdichange[elec$year==2008],
       y=elec$Incvote[elec$year==2008], col="red3", pch=19)

## Wow that plot looks fun, incumbents do better when the economy does well.
## It would be more fun if we knew which dots referred to which elections.
## If we have a vector of labels that is the same length as the coordinate pairs
## we can use the identify() function


identify(x=elec$rdichange, y=elec$Incvote, labels=elec$year)

## The side of the point that you click on determines where the label will fall.



#######################################
## Function Writing (la tercera vez) ##
#######################################

## often we need to return more than one number from our functions.

ci95.interval <- function(x) {
  n <- length(x)
  xbar <- mean(x, na.rm=TRUE)
  se <- sd(x, na.rm=TRUE)/sqrt(n)

  upper <- xbar + 1.96*se
  lower <- xbar - 1.96*se

  return(list(upper = upper, lower = lower))
}

out <- ci95.interval(x=elec$turnout)

out[1]
out[2]

out$upper
out$lower

#####################################
## Row and Column Means (R tricks) ##
#####################################

## Say we were interested in comparing a couple of distributions
## so we take 1000 draws from each (uniform, normal and chi^2).

## now we'll create a matrix by column-binding the draw together
matt <- cbind(runif(1000),rnorm(1000),rchisq(1000, df=20))

head(matt)

## we could go and grab the means of each column separately:

mean(matt[,1])
mean(matt[,2])
mean(matt[,3])

## or we could use a fancy R trick:

colMeans(matt)

## this is useful for testing some condition on multiple columns at once

colMeans(matt < 0.5)

## This implies a quick way to do simulations:

n <- 100
sims <- 1000

## how many total draws do i need for this?

draws <- n*sims

## so! let's get that many draws and have R fill the matrix

sim.shelf <- matrix( rnorm(draws, mean=0, sd=1) , nrow=sims, ncol=n)


## now, we can get the mean of each row (which is one simulation):

rowMeans(sim.shelf)

## only two real lines of code! awesome!
