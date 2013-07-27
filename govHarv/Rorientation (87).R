########################################################
## Gov 2001, Spring 2011
## R Orientation Session
## (i.e., Section 0)
## Brandon Stewart (adapted from Maya Sen and many previous years of TF's)
########################################################

## To download R, please go to http://www.r-project.org/
## and follow the directions. You can also access R through
## any of the computers in the HMDC lab and also some of the FAS
## computers (for example, in the science center).

## R works by 
## 1. typing code in the console (next to the red 'greater than'
## sign) 
## 2. OR by typing on a script (like this one) and sending the lines of
## code to the console.

## You can use the R Editor (the R GUI, pronounced "gooey")
## or you can use from a variety of free R Editors, like Tinn R or Notepad++. 
## Unless you are working on very simple calculations,
## you should always save your code as a script using a ".R" file extension.

## Every line that begins with '#' is ignored by the console.
## So you can add comments to your code without generating syntax errors.
## In fact, your code should be heavily commented so that you can understand
## what you've done if you had to come back to it later.

## Note: You should make your R code neat tidy by taking
## advantage of spacing, hitting return, and the "#" sign.

########################################################
## R help!
########################################################

## get used to using the "help" command; it's your friend.

?mean

## or 

help(mean)

## Remember also the Gov 2001 list! There's a steep learning
## curve but it will pay off! :)

########################################################
## R as a calculator
########################################################

## R can be used to run a wide variety of basic arithmetic commands

2 + 18

50821/6

21^4

34*.01

(900+1)/(45*.09)


## You can store an object for later retrieval


a <- 2 + 18
a


b <- 50821/6
b

a + b


## use the ls() command to see what objects are currently stored 
## in the R environment        


ls()

## You can use the following command to clear your workspace

rm(list = ls())


########################################################
## Making R interact with your computer and the WWW
########################################################

## Your working directory is the "folder" where R loads and saves data

getwd()

## To change the working directory, use "setwd"

setwd("C:/Documents and Settings/My Documents") 

## You can also do this manually by clicking on the 
## R console, going to "File," and then clicking "Change dir"

## R can load all kinds of data easily

## 1. From computer, saved in a previous R sessions as "*.RData"

load("astrology.RData")
ls()
astrology


## 2. From my computer, saved as a text document "*.txt"

lalonde <- read.table("lalonde.txt")
lalonde

## 3. From someone else's website, saved as a text document

fish <- read.csv("http://dl.dropbox.com/u/12848660/fish.csv")
fish

########################################################
## Getting a feel for the data
########################################################

## There are a number of really useful commands that will
## quickly give you a feel for any data:

summary(fish) ## summary statistics

names(fish) ## column (generally variable) names

dim(fish)   ## the dimensions of the dataset

head(fish)  ## first few observations

########################################################
## R packages
########################################################

## People write software for R all the time.
## These are called "packages" and the one we'll use most often is 
## Zelig (Imai et al). To install a package:

install.packages("Zelig")
	## this must be done only once 

library(Zelig)
     ## this must be done every time you use the Zelig package

## Other useful packages are
## foreign -- permits you to load data formatted for other software
## xtables -- helps you write up tables in LaTeX code
## car -- has many datasets and linear regresison functions
## more packages are at http://cran.r-project.org/web/packages/

## As you get more familiar with R, you will use more packages!
## A great website to learn about cutting-edge stuff that
## people are writing is http://www.r-bloggers.com/

########################################################
## Working with objects
########################################################

## R can store objects, and you can call these up later.
## As noted above objects are defined using "<-" 

scalar1 <- 2
scalar1

R <- "fun"
R

truth.vec <- TRUE
truth.vec

## Note: Don't name your objects things like "mean"
## or "sum" or "7" since those are things that R already
## has pre-packaged.

# You can make longer things, like vectors,
# which we create with c(), for concatenate 

vec1 <- c(2,2,7,-1,4)

R <- c("Gov2001","is","fun")

## R performs math on numeric objects

vec2 <- c(2,5,1,3,2)

vec1 + vec2

vec1 - vec2

3*vec2

vec2*3

## Tricks for creating vectors

vec3 <- 1:5
vec3

vec3 <- c(1:5, 7, 11)
vec3

vec4 <- c(vec1, vec2)
vec4

## Subsetting (use [] to pick out elements of an object)
## recall: vec1 is c(2,2,7,-1,4); vec4 is c(2,2,7,-1,4, 2,5,1,3,2)

vec1[1]

vec1[6]

vec1[-1]
	
vec4[c(5,7)]

vec4[c(5:7)]

## You can also replace a particular element of a vector

vec1[3] <- 6

########################################################
## Basic R functions
########################################################

# R has many preprogrammed functions that manipulate objects.
# To use a function, you type the function name followed by the
# arguments in parentheses


a <- c(1,3,6,5,9,22)


b <- c(4,5,6,5,2,1)


sum(a) ## sum of all elements of a

sum(b)

sum(a,b)

max(a) ## maximum element in a

min(a) ## minimum element in a

mean(a) ## mean of a

length(a) ## number of elements in a,
	    ## useful for when you need to calculate the sample size, n

sort(a) ## sorts a from lowest to highest

## you can store the output of a function, as a new object.

output <- length(a)

## These functions are useful for creating vectors

seq(from = 0, to = 5, by = .5) 
	## creates a sequence of numbers

rep(10, 27) 
	## repeats the number "10" 27 times.

## to learn the arguments for a particular 
## function, use the help commands:

?sort

sort(a)
sort(a, decreasing = TRUE)

########################################################
## Matrixes in R
########################################################

## the matrix() function in R is one way to create a matrix

matrix(data = 1:12, nrow = 3, ncol = 4)

matrix(data = 1:12, nrow = 2, ncol = 6)

matrix(data = 1:12, nrow = 2, ncol = 6, byrow = TRUE)

## You can also create a matrix from vectors
## using the cbind and rbind commands

my.vec <- c(4:8)
my.vec2 <- c(5:9)
my.vec3 <- c(1:5)


cbind(my.vec, my.vec2, my.vec3)
rbind(my.vec, my.vec2, my.vec3)

## Let's store the last matrix

mat <- rbind(my.vec, my.vec2, my.vec3)

## You can give your matrix colums and rows names

rownames(mat)

colnames(mat) <- c("col1","col2","col3","col4","col5")

## We can extract particular elements of a matrix just like
## we could from a vector, though this time we have to specify
## two dimensions, the row and the column

mat[1,1]

mat[,1]

mat[1,]

## We can also do various kinds of matrix manipulations
## element by element (cell by cell) multiplication
mat * mat

## Transposing a matrix 
t(mat)

## Matrix multiplication
mat %*% t(mat)

## Note that order matters!
t(mat) %*% mat #different answer!
mat %*% mat #doesn't work!

# inverting a matrix
mat1 <- matrix(rnorm(9, 0, 3), nrow=3)
solve(mat1)



#################################################################
## Writing Functions
#################################################################

## You can write your own functions in R using the "function" command
## Functions can be really complicated or really simple!

## here's the general idea:

## my.function <- function(x,y,z){
##					## tells R that this is a function
##   	out <- crazy function stuff
##					## the meat of the function (you usually "tab" this)
##  return(out)
##					## returns the output of the function
## }
						## closes the function up


## This function will take three numbers as arguments;  it will add
## the first two and divide the sum by the third

my.function <- function(x,y,z){
  out <- (x + y)/z
  return(out)
}


## Now we call our function with 

my.function(x = 5, y = 10, z = 3)

my.function(5, 10, 3)

## Now let's see a function that returns the smallest element in a vector
## using the R commands we've seen so far

small <- function(vec){
  sorted <- sort(vec)
  out <- sorted[1]
  return(out)
}

## Here's a new vector we can use for a test

test <- c(2, 5, 4000, .1, 1)

small(test)

## By the time you are done with this course, you will be writing
## more complicated functions. But the structure is the same.

#################################################################
## Writing For-Loops
#################################################################

## A for loop is a way of repeating a process a vast 
## number of times.  For each iteration of the loop
## R keeps an index number stored which can be handy.

## here is a simple example where we first define a 
## storage matrix to hold the output of our for loop

holder <- c()

for(i in 1:100){
  draw <- rnorm(1, mean = 0, sd = 1) 
		## here we are drawing one observation from a normal distribution
		## with mean zero and standard dev 1
  holder[i] <- draw
}

## and then to get a visual representation of this, we can
## use something like the hist command:

hist(holder)

## let's write a more flexible for loop that will let you 
## change the number if simulations quickly

sims <- 10000

holder <- c()

for(i in 1:sims){
  draw <- rnorm(1, mean = 0, sd = 1) 
  holder[i] <- draw
}

hist(holder)

## you can also use the plot command (using the density function)

plot(density(holder))


#################################################################
## Logical Tests
#################################################################

## We know about subsetting where we ask
## for items in a vector or a matrix by their position.
## We can make the analogy to saying, "I'd like
## the 5th book on the 2nd shelf of your bookshelf.

## Now we will learn more about how to find certain items in
## our vectors and matrices by what they are, not their position.

## Let's start with some simple scalars

x <- 5

x == 5  ## here we're asking R, "does x equal 5?"

## These become hugely important in what are called "if" statements

if (x == 5) {
  print("number five is alive")
} else {
  print("  :(  ")
}

## Let's translate this really quickly 
## We're saying "if x is equal to 5, print the 
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

# Suppose we wanted to know which rows have negative y values
# And only wanted to see the data in those rows
fish$income < 3
which(fish$income < 3)	#to return the row numbers/indices


# Logical tests
# == is equal to
# != is not equal to
# & and
# | or
# <= less than or equal to
# >= greater than or equal to


##########################################################################
## Replicating (some of) the Fish Paper
##########################################################################
##We load the data in.  It is a csv file so we indicate that a comma is the separating
##value.  It also has row headers, so we specify that as well.
data <- fish
##We can then look at the first few rows with the head() command
head(data)
##We see that there are extra row numbers, so we remove those
data <- data[,-1]

##Let's look at the data
summary(data)

##Some plots of the data
hist(data$fhrev)
plot(data$muslim, data$fhrev)
plot(data$income, data$fhrev)

##Let's manually look at the means
summary(data$fhrev[data$muslim==1])
summary(data$fhrev[data$muslim==0])

##What if we look at a bivariate regression.  Use the lm() function.
##We can save the model like any other function 
model.0 <- lm(fhrev ~ muslim, data=data)
model.0
summary(model.0)

##We can easily add covariates.
model.5 <- lm(fhrev ~ muslim + income, data=data)
model.5 
summary(model.5)

##We can also visualize the results.
names(model.5)
model.5$coefficients
plot(data$income, data$fhrev)
abline(model.5$coefficients[1], model.5$coefficients[3])

#################################################################
## Sampling and Random Numbers
#################################################################


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


#################################################################
## PMFs
#################################################################
##Programming the PMF as a function
##Note: Why is this not a proper PMF?

pd <- function(x) { log10(1 + 1/x) }


##Using R to solve for the expectation
sum <- 0
for (i in 1:9) {
  sum <- sum + pd(i)*i
}

#Plotting the PMF
plot(x=1:9, y=pd(1:9), 
       main="PMF of First Digits Under Benford's Law",
       xlab="First Digit", ylab="Probability Under Benford's Law",
       pch=19)
axis(1, at=1:9)
segments(1:9, rep(0,9), 1:9, pd(1:9)) 


#################################################################
## More resources
#################################################################

## There are loads more resources for R:
## http://cran.r-project.org/doc/contrib/usingR.pdf (the Maindoland intro, very good)
## http://www.r-bloggers.com/ (the R bloggers website -- awesome applications!)
## the gov 2001 list!!!
