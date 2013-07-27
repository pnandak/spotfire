## Section 3 R Notes
## Gov2000 Section Notes
## September 24, 2009
## Maya Sen
## Confidence Intervals and Hypothesis Testing
## Thanks to Matt Blackwell, last year's TF for data and code

#####################################
## converting to a standard normal ##
#####################################

## a key concept at work in everything we talked about this week is 
## that you can transform any normal back to a standard normal,
## which has a mean of 0 and a standard deviation of 1.

## see explanation on the board?

## by substracting the mean and dividing by the standard deviation:

samp <- rnorm(1000, mean = 69.1, sd = 2.9)	## men's heights in the US
hist(samp)							

transformed.samp <- (samp - 69.1)/2.9		## substracting mean, divindg by sd
hist(transformed.samp)

## putting those on the same line
par(mfrow = c(1,2))
hist(samp)
hist(transformed.samp)

#################################
## Let's play around with data ##
#################################

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

## This data is election data from Georgia. This
## data (precincts) is data aggregated to the precinct level.
## So this measures the turnout rate, percent Black, percent
## Female, mean age, turnout in the democratic primary,
## turnout in the republican primary, is the precinct in Atlanta,
## and location dummies for the polling stations.

## Our goal in section today will be to estimate the percent
## Black in Fulton county. 

## this is what that variable looks like:

plot(density(precincts$black))
	## anything weird about this??

###########################
## Calculating True Mean ##
###########################

##To do this, we'll take the mean of
## percent Black in each of the precincts.

## Note: a really quick and easy way to calculate proportions
## is to use the mean function:

true.mean <- mean(precincts$black)
true.mean
## this is the TRUE population

## we can also use the mean function to calculate other 
## statistics, even those involving binary variables

mean(precincts$school)
	## proportion of precincts where people
	## vote at a school
mean(precincts$church)
	## at a church
mean(precincts$firest)
	## at a fire station

##############
## Sampling ##
##############

## remember that we can use indexing to get certain rows
## of the data. If I wanted to get just rows 10, 3 and 200
## in that order, I would say...

precincts[c(10,3,200),]

## What if we want to do a random sample of the datasets?
##We'll use the "sample" command from previous weeks to give
##us a random sample of the row numbers.

## First, ALWAYS check if you have to set the seed!

set.seed(12345)
	## setting seed to 12345

## Now...
n.precincts <- nrow(precincts)
	## number of rows
N <- 40
	## sample size

rand.rows <- sample(n.precincts, size=N, replace=FALSE)
rand.rows

## Now that we have the random rows, we can grab those random rows
## from the actual data and put it into  a new matrix:

mypoll <- precincts[rand.rows,]

## or we can put everything on one line

mypoll <- precincts[sample(nrow(precincts), size = N, replace = FALSE),]

## and we can treat this as a nice little subset of the data
summary(mypoll)
head(mypoll)
dim(mypoll)

####################
## Conf Intervals ##
####################

## recall the formula for the 95% CI
## let's calcuate the mean, standard deviation and 95% CI for this sample

mu.b <- mean(mypoll$black)
sd.b <-sd(mypoll$black)

mu.b
sd.b

## we'll just use the formulas for confidence intervals

mu.b + 1.96*sd.b/sqrt(N)
mu.b - 1.96*sd.b/sqrt(N)

## where does the 1.96 come from?
		## (draw on board.)

par(mfrow = c(1,1))
ruler <- seq(-4,4,.01)
plot(ruler, dnorm(ruler), type = "l", xlab = "standard normal")

(1-.95)/2 ## because this is two-tailed
qnorm(1 - .025)
qnorm(0 + .025)

ruler <- seq(-4,4,.01)
plot(ruler, dnorm(ruler), type = "l", xlab = "standard normal")
abline(v = 1.96)
abline(v = -1.96)

## KEY QUESTIONS:
# 1. WHAT HAPPENS TO CONFIDENCE INTERVAL WHEN N GOES UP?
# (RELATED: WHAT HAPPENS WHEN N<32)?
# 2. WHAT HAPPENS TO CONFIDENCE INTERVAL WHEN STANDARD DEV
#	GOES UP?
# 3. WHAT HAPPENS TO CONFLIDENCE INTERVAL WHEN CONFIDENCE LEVEL GOES UP?

## once the quantities are observed, nothing is random!
## explain.

###########################
## Example from the news ##
###########################

## WorldPublicOpinion.org (WPO) conducted the poll of 
## 1,003 Iranians across Iran between Aug. 27 and Sept. 10, 2009. 
## 642 said they had confidencein the president
## 211 said they did not have confidence in the president
## (150 said they had no opinion; let's ignore these)

## calculate 95% conf interval of support
## among those with an opinion

iran <- matrix(data = NA, nrow = (642+211), ncol = 1)
iran[1:642] <- 1
iran[643:length(iran)] <- 0

mean(iran)
sd(iran)

mean(iran) + 1.96*(sd(iran)/sqrt(length(iran)))
mean(iran) - 1.96*(sd(iran)/sqrt(length(iran)))

## margin of error

## and margin of error

#########################################
## Conf Intervals w/ repeated sampling ##
#########################################

## We're going to pretend we have 20 independent researchers out in
## the field polling precincts in Fulton. Each of the 10 will
## poll 40 precincts and then calculate the mean and sd and 95% CI.

set.seed(12345)		## again, remember seed setting!
N <- 40			## sample size
pollsters <- 20		## number of posters
poll.means <- matrix(data = NA, ncol = 1, nrow = 10)
poll.sds   <- matrix(data = NA, ncol = 1, nrow = 10)
				## holder matrixes

for (i in 1:pollsters) {

  poll.surveyed <- precincts[sample(nrow(precincts), size=N, replace=FALSE),]

  poll.means[i] <- mean(poll.surveyed$black)
  poll.sds[i]   <-   sd(poll.surveyed$black)

}

## Note that we didn't save the CI in each iteration. That's because
## we can create the CIs outside the loop much more quickly

ci.upper <- poll.means + 1.96*poll.sds/sqrt(N)
ci.lower <- poll.means - 1.96*poll.sds/sqrt(N)

## Let's plot the sampling distribution of the means

plot(density(poll.means))

## let's plot the sampling density of the CIs...
## this is tricky, but we'll get through it. First, we want to get comfortable
## with the "plot" command

x <- c(1:10)			## just 1-10
y <- rnorm(length(x))		## draws from a standard normal
x
y

plot(x,y)
plot(x,y, type = "l")		## lines
plot(x,y, type = "b")		## both lines and dots
plot(x,y, type = "s")		## stair steps

## back to the task at hand...
## we need to create
## a ruler that will denote the horizontal locations of the CIs

ruler <- 1:pollsters

## Now, let's open up a plotting window and plot the means first,
## I'm using "pch=19" to get solid dots and xlim=c(0,1) because this
## is a proportion so I know things can't be bigger than that. 

plot(x=poll.means, y=ruler, pch=19, col="orange", xlim=c(0,1))
	## all "ruler" is at this point is an index indicating
	## which poster reported which poll.mean

## The next plotting command is "segments" which will draw lines
## on the plot. We'll use this to plot the CIs. Note that the
## arguments are segments(x0,x1,y0,y1...), where
##     (x0,y0) is the beginning point of the line
##     (x1,y1) is the ending point of the line.
## Let's do a test: 

segments(x0=.2,y0=3,
         x1=.8,y1=20, col="orange")

		## the starting point here was (.2, 3)
		## and then ending point was (.8, 20)

## But that doesn't look too good, let's start over:

plot(x=poll.means, y=ruler, pch=19, col="orange", xlim=c(0,1))

## for each x0,x1,y0,y1, segments can take vectors, which draw the lines
## using the values with the same index. So we have:

segments(x0 = ci.lower, x1 = ci.upper, y0 = ruler, y1 = ruler, col = "orange")

## In addition, let's draw a vertical line at the true mean. we'll use the
## abline(v=) command.

abline(v=mean(precincts$black), col="red")

## we might want to see how many of the CIs contain the true mean

mean((ci.upper > mean(precincts$black) & (ci.lower < mean(precincts$black))))

########################
## hypothesis testing ##
########################

## ok, let's say a fancy pollster thinks that there's
## no way that the turnout rate was more than 35%

## Let's use hypothesis testing to test his conclusion

## STEP 1: take a sample
	## why? otherwise, you just know the truth

hypo.samp <- precincts[sample(nrow(precincts), 200),]

## Step 2: take the sample mean (e.g., the estimator you are interested in)
##  and sample standard deviation

samp.mean <- mean(hypo.samp$turnout)
samp.sd <- sd(hypo.samp$turnout)

## Step 3: set up a test statistic
	## it can be a lot of things, but generally people
	## normalize the estimate to the standard normal and then
	## check to see how unusual (or unusual) that test statistic would be

test.stat <- (samp.mean - .35)/(samp.sd/sqrt(N))

## step 4: calculate rejection regions

## so for alpha = 0.05 and 0.01:

qnorm(1 - .025)	## for alpha = 0.05 (why??)
qnorm(0 + .025)	## for alpha = 0.05 

qnorm(1 - .005)	## for alpha = 0.01 (why??)
qnorm(1 - .005)	## for alpha = 0.01 

## let's plot these against a standard normal to see what they look like

ruler <- seq(-6,6,.01)
plot(ruler, dnorm(ruler), type = "l", xlab = "standard normal")
abline(v = c(qnorm(1 - .025), qnorm(0 + .025)), col = "red")
abline(v = c(qnorm(1 - .005), qnorm(0 + .005)), col = "blue")
abline(v = test.stat)
legend(x = "topleft", legend = c("alpha = .05", "alpha = .01", "test stat"), lwd = 2, col =
c("red", "blue", "black"))

#cool!


