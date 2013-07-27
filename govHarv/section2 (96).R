##
## Section 2 - Probability and Random Variables
## mb. 24 sept 08
##
## NB: past gov2000 TFs deserve shoutouts as this
##     code is heavily borrowed (read: stolen) from
##     their work. (alison post, ryan moore, andy eggers,
##     ben goodrich, jens hainmueller, clayton nall)


###########################
## 1) Logical statements ##
###########################


## Last week we talked about subsetting, where we can
## for items in a vector or a matrix by their position.
## We made the analogy that this was like asking for 
## beer at a bar by the tap number. Or like saying "I'd like
## the 5th book on the 2nd shelf of your bookshelf.

## Now we will learn more about how to find certain items in
## our vectors and matrices by what they are, not their position.
## So, we will be asking for "Miller High Life" instead of "tap 2"
## or "The Great Gatsby" instead of "shelf 2, book 5."  

## Let's start with some simple scalars

x <- 5

x == 5  ## here we're asking R, "does x equal 5?"

## These become hugely important in what are called "if" statements

if (x == 5) {
  print("number five is alive")
} else {
  print("  :(  ")
}

## Let's translate this really quickly (NB: R is a language, 
## so be sure to be retranslating into your native language 
## constantly). We're saying "if x is equal to 5, print the 
## sentence "number five is alive." Otherwise (else), print 
## a sad face emoticon."

## a few more to get some intuition:

TRUE == TRUE
TRUE == FALSE
5 == 5.0
"hello" == "hello "

## Great, fun, looks like we have scalars down. Let's move on
## to vectors. 

x <- c(1,1,1,3,3,3,1,1,1)

## what's going to happen when we use the statements from before?

x == 1
x == 3
x == 2

## Note that the question we are asking R is different once
## we change "x": now we ask "where does x equal 1?" and 
## R gives us a TRUE/FALSE vector (a "logical" vector) that is
## the same size as x, with a TRUE where it equals 1 and a FALSE
## where it doesn't.


## to make it slightly clearer, i'll print them together.
## data.frame collects vectors together into one data.frame
data.frame(x, x == 1)
data.frame(x, x == 3)
data.frame(x, x == 2)


## Now we can use these logical statement to pull out
## parts of the vector that we want.

x[x==1]



## This becomes incredibly more fun with real data.
## read.table() reads in a data file and outputs a 
## dataframe. 

nes <- read.table("http://www.people.fas.harvard.edu/~blackwel/nes.dat")


head(nes)    ## show the top of the data 
nrow(nes)    ## get the number of rows of nes
ncol(nes)    ## get the number of columns of nes              
names(nes)   ## get the variable names of nes
summary(nes) ## summarize the variables                               

## nes is a dataframe, which is how R stores datasets.
## we can access the individual variables by using the dollar 
## sign:

gender <- nes$gender
gender

## Remember how to grab certain rows:

nes[c(5,6,7,15,30),]

## And certain columns:

nes[, c(1,2,3)]
nes[,c("gender","income")]

## Now, we probably will want to subset the dataset by values of
## certain variables. Perhaps we would like two datasets: one
## for males and one for females.

nes.male   <- nes[nes$gender == "Male",]
nes.female <- nes[nes$gender == "Female",]  ## or nes$gender != "Male"

nes.male
nes.female

mean(nes.male$income)
mean(nes.female$income)

## we can also construct complex logical statements with 
## "and" (&) and "or" (|)

## rich men
nes[(nes$income >= 50000) & (nes$gender == "Male"),]

## people with incomes in [25000,50000]
nes[(nes$income <= 50000) & (nes$income >= 25000),]

## people with incomes in [25000,50000]
nes[(nes$income < 50000) & (nes$income > 25000),]

## people less than 250000 or greater than 50000
nes[(nes$income >= 50000) | (nes$income <= 25000),]

######################
## 2) distributions ##
######################


## Let's say that I'm a lazy movie reviewer and I give out three kinds of
## reviews: negative, neutral and positive. Since I'm lazy, I assign
## reviews at random with the following probabilities:
##   negative - 30%
##   neutral  - 50%
##   positive - 20%
## We can get random draws of this with the sample function. type help(sample)
## for descriptions of all the arguments.

sample(c("negative","neutral","positive"), size=5, replace=TRUE,
prob=c(.3,.5,.2))


## We might want to take the verbal story and turn it into a math story.
## To that end, we can define the following random variable:
##
##      -1 if negative;
##  X =  0 if neutral; 
##       1 if positive;
##
## This is exactly the same as before, except replace words with math 
sample(c(-1,0,1), size=5, replace=TRUE, prob=c(.3,.5,.2))


## create simulations of this random variable
## by drawing randomly from its distribution

set.seed(12345)
reviews <- sample(c(-1,0,1), size=10000, replace=TRUE, prob=c(.3,.5,.2))

## investigate the simulations
mean(reviews)  ## the mean of the random draws
var(reviews)   ## the variance of the random draws
hist(reviews, col = "wheat")  ## a histogram of the draws 
                              ## ("col" changes the coloro of the histogram)


## We can write an R function for the PMF of this
## distribution. 

pmovie <- function(x) {
  if (x == -1) {
    out <- .3
  }
  if (x == 0) {
    out <- .5
  }
  if (x == 1) {
    out <- .2
  } else {
    out <- 0
  }
  return(out)
}

## the above function doesn't work for vectors, but the following one
## takes advantage of logical statements and their 0/1 status

pmovie <- function(x) {
  out <- (x == -1)*.3 + (x == 0)*.5 + (x == 1)*.2
  return(out)
}


################################
## 3) the Normal distribution ##
################################


## the Normal distribution is invaluable to statistics. as
## we will see later in the course, we will often use the 
## Normal distribution as an approximation of some unknown 
## distribution. This means that we think that the probability 
## statements about the Normal are very close to the  probability
## statements about the unknown distribution.

plot(dnorm, mean = 1, sd = 4, from = -4, to = 4, col = "blue", 
     main = "PDF of the Standard Normal")

## - area under curve always equals 1, regardless of mu and sigma
## - y-axis values do not have straight-forward interpretation, as the
## integral of the pdf evaluated at any specific point is 0 (since
## p(x) is continuous.  Were we to integrate within a certain area, we
## would get a probability value, though.)
## - Examples of such integrals:
##    - \int[-1,1] \approx .68
##    - \int[-2,2] \approx .95
##    - \int[-3,3] \approx .997
## - the y-axis values can be thought of as "normalizing" the density
## - unit normal: mu = 0, s^2 = 1 : will be a building block for us


## often we want to know the probability of being in some interval
## for a normal random variable. 
## Remember: if you use pnorm(q,...)
## lower.tail=TRUE:  probability of (-infinity, q)
## lower.tail=FALSE: probability of (q, infinity)
## YOU SHOULD DRAW PICTURES!!!

pnorm(-2, mean = 0, sd = 1, lower.tail = TRUE)
pnorm(-2, mean = 0, sd = 1, lower.tail = FALSE)

## what about 0?
pnorm(0, mean = 0, sd = 1, lower.tail = TRUE)
pnorm(0, mean = 0, sd = 1, lower.tail = FALSE)

## how can we get the probability of being between 2 numbers?

pnorm(0) - pnorm(-2)


## RANDOM NUMBER GENERATION

## generates a set of pseudo-random numbers drawn from a given
## probability distribution.  "Pseudo-" because these are, in fact,
## deterministic functions, but they don't often repeat.

set.seed(12345)
rnorm(10, mean=0, sd=2)


## Comparing Distributions

## We often want to know if the Normal approximation is
## correct. Let's look back at the NES data.

hist(nes$income, col = "wheat", main = "Hisogram of Income")

## so, again "col" changes the color and "main" changes the title.
## you should play with these to get a feel for what happens.



## instead of a histogram, sometimes density plots are useful.
## they are smooth representations of histogram.

plot(density(nes$income), col = "violet", main = "Desnity Plot of Income")

## let's do an experiment: generate some Normal random draws
## that have the same mean and same standard deviation as
## income variable and compare them to the actual income variable.

fakeincome <- rnorm(100000, mean=mean(nes$income), sd=sd(nes$income))


## now that we have the "fake" normal approximation, we can plot the two
## densities on top of one another. to do this, we first plot income
## again like before:

plot(density(nes$income), col = "violet",
     main = "Desnity Plot of Income and its Normal Approximation")
     
## now we'll call a function called "lines" that will draw lines on 
## on the graph we've already plotted.
lines(density(fakeincome), col = "blue")


## some notes about plotting: there are two kinds of graphing commands,
## high-level and low-level. when you call a high-level command, such as
## plot(), R will bring up a new plot window. Once a plot window is open,
## low-level commands can add stuff to the open plot window. lines() is an 
## example of a low-level command. if we were to use the following commands:

plot(density(nes$income), col = "violet",
     main = "Desnity Plot of Income and its Normal Approximation")
plot(density(fakeincome), col = "blue")

## you'll see that the second command will close the first window and 
## plot the second over it. 



