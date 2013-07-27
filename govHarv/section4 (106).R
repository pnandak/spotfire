## Section 4 R notes
## mb 8 Oct 08.

## Remember to set the working directory when you start
## a session of R. Something like:
##   c:/workland/gov2000/hw3/
## is good because it's organized and will be easier to
## find later.

## First we'll load the data that's in our working dir##
load("fulton.RData")


## Whenever you load up a dataset, but sure to play around
## with it for a bit. You should get to know how big it is,
## what the columns are and whatnot.

## get the column names of the data:
names(precincts)

## get the first six rows of the data:
head(precincts)  

## get the dimensions of the data:
dim(precincts)

## This data is similar to the HW data for this week, which
## has data for each registered voter in Fulton county. This
## data (precincts) is data aggregated to the precinct level.
## So this measures the turnout rate, percent Black, percent
## Female, mean age, turnout in the democratic primary,
## turnout in the republican primary, is the precinct in Atlanta,
## and location dummies for the polling stations.

## Our goal in section today will be to estimate the percent
## Black in Fulton county. To do this, we'll take the mean of
## percent Black in each of the precincts.

mean(precincts$black)

## Sampling from a dataset
## remember that we can use indexing to get certain rows
## of the data. If I wanted to get just rows 10, 3 and 200
## in that order, I would say...

precincts[c(10,3,200),]

## What if we want to do a random sample of the datasets?
##We'll use the "sample" command from previous weeks to give
##us a random sample of the row numbers.
set.seed(12345)
n.precincts <- nrow(precincts)
N <- 40

rand.rows <- sample(x=1:n.precincts, size=N, replace=FALSE)
rand.rows

## Now that we have the random rows, we can grab those random rows
## from the actual data and put it into  a new matrix:
mypoll <- precincts[rand.rows,]

## note that this poll has the same variable
head(mypoll)

## let's calcuate the mean, standard deviation and 95% CI for this sample

mu.b <- mean(mypoll$black)
sd.b <-sd(mypoll$black)

mu.b
sd.b

## we'll just use the formulas for confidence intervals

mu.b + 1.96*sd.b/sqrt(N)
mu.b - 1.96*sd.b/sqrt(N)


## We're going to pretend we have 20 independent researchers out in
## the field polling precincts in Fulton. Each of the 10 will
## poll 40 precincts and then calculate the mean and sd and 95% CI.

set.seed(12345)
N <- 40
pollsters <- 20
poll.means <- vector(length=10)
poll.sds   <- vector(length=10)


for (i in 1:pollsters) {
  poll.rows <- sample(x=1:n.precincts, size=N, replace=FALSE)
  poll <- precincts[poll.rows,]

  poll.means[i] <- mean(poll$black)
  poll.sds[i]   <-   sd(poll$black)
}

## Note that we didn't save the CI in each iteration. That's because
## we can create the CIs outside the loop much more quickly


ci.upper <- poll.means + 1.96*poll.sds/sqrt(N)
ci.lower <- poll.means - 1.96*poll.sds/sqrt(N)



## Let's plot the sampling distribution of the means

plot(density(poll.means))


## let's plot the sampling density of the CIs (draw on board)
## this is tricky, but we'll get through it. First, we need to create
## a ruler that will denote the horizontal locations of the CIs

ruler <- 1:pollsters

## Now, let's open up a plotting window and plot the means first,
## I'm using "pch=19" to get solid dots and xlim=c(0,1) because this
## is a proportion so I know things can't be bigger than that. 

plot(x=poll.means, y=ruler, pch=19, col="orange", xlim=c(0,1))

## The next plotting command is "segments" which will draw lines
## on the plot. We'll use this to plot the CIs. Note that the
## arguments are segments(x0,x1,y0,y1...), where
##     (x0,y0) is the beginning point of the line
##     (x1,y1) is the ending point of the line.
## Let's do a test: 

segments(x0=.4,y0=3,
         x1=.8,y1=7, col="orange")

## Well that doesn't look too good, let's start over:
plot(x=poll.means, y=ruler, pch=19, col="orange", xlim=c(0,1))

## for each x0,x1,y0,y1, segments can take vectors, which draw the lines
## using the values with the same index. So we have:

segments(x0 = ci.lower, x1 = ci.upper, y0 = ruler, y1 = ruler, col = "orange")


## In addition, let's draw a vertical line at the true mean. we'll use the
## abline(v=) command.

abline(v=mean(precincts$black), col="red")



## Logical Statements (part duex)

## let's say we have a logical statement -- for instance
## for precincts that are majority black.
precincts$black > .5
length(precincts$black[precincts$black > .5])/length(precincts$black)

mean(precincts$black > .5)

## we might want to see how many of the CIs contain the true mean
mean((ci.upper > mean(precincts$black) & (ci.lower < mean(precincts$black))))
