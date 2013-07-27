########################################################
## Gov 2001, Spring 2011
## Section 1
## Brandon Stewart (adapted from Patrick Lam, Jens Hainmueller)
########################################################

## First a quick R refresher

######################################################
## Dataframes
######################################################

## Dataframes are an important object class for our statistical analyses.
## All of our datasets must be of the dataframe class before they can be used
## by any of the canned regression functions.


## Let's load a sample dataframe from the Zelig library

library(Zelig)
data(turnout)
class(turnout)
summary(turnout)

## One important difference between matrices and dataframes is that dataframes
## can hold different types of vectors (integer, character, factor, etc.)
class(turnout$race)
class(turnout$age)


## If a matrix contains a character vector, it automatically turns everything into characters
## (it can only contain vectors of the same type)
as.matrix(turnout)


## If our data is a matrix, we can easily turn it into a dataframe for use in canned regression functions.

my.data <- matrix(c(rep(10,10)), nrow=5, ncol=5); my.data
my.data.frame <- as.data.frame(my.data); my.data.frame
class(my.data.frame)



######################################################
## Lists 
######################################################

## A list is a more flexible than a matrix. 

# For example, we can combine a vector, a matrix and a dataframe

my.vector <- c(3,6,3,10,7,8,5,2); my.vector
my.matrix <- matrix(4, nrow = 3, ncol = 2); my.matrix
my.dataframe <- as.data.frame(my.matrix); my.dataframe

my.list <- list(v = my.vector, m = my.matrix, d = my.dataframe)
my.list


# Note the arguments I gave to list().  
#I'm asking R to compile all those things into a list, and to the left of the equal sign, I'm giving each element in the list a name.  So my vector is named "v", my matrix is named "m", etc.  This can be helpful for subsetting.  I don't really need the names for it to work though.  list(my.vector, my.matrix, my.dataframe) will work, only they don't have names. We can name them afterward with the names() function.



# There are a couple ways to pull stuff out of a list:


# 1. If I had named the elements of the list, I can use the $ operator:

my.list$v
my.list$m


# 2. I can also use double brackets to pull up the ith element: my.list[[i]]

my.list[[2]]


# 3. I can do double brackets and "" if they are named:

my.list[["d"]]


#######################################
## Functions
#######################################

## Functions are probably the most important concept to learn in R.

## A function is something that takes one or more inputs and 
## spits out one or more outputs.


# let's try an easy function which I call "divide"
# it takes two numbers (x and y) and divides them and returns the answer

divide <- function(x,y){
  out <- x/y
  return(out)
}

divide(x=3, y=2)
divide.object <- divide(x=6, y=3)
divide.object



## Let's try a harder function now.  I want to write a function that draws n random draws
## from a normal distribution with a given mean and sd and then plots those draws and its mean
## and prints and stores the mean and the certain quantiles in a list.

my.func <- function(n.draws=1000, mean, sd, probs=c(.025, .975)){
  draws <- rnorm(n.draws, mean=mean, sd=sd)
  quants <- quantile(draws, probs = probs)
  mean.draws <- mean(draws)
  
  plot(density(draws))
  abline(v=mean.draws, col="red")

  res <- list(mean=mean.draws, quantiles=quants)
  cat("The mean is", mean.draws, "\n")
  cat("The", probs[1], "quantile is", quants[1], "\n")
  cat("The", probs[2], "quantile is", quants[2], "\n")

  return(res)
}

test <- my.func(mean=0,sd=1)
test$mean
test$quantiles


## Note that I can specify default values in the arguments of my function (as I did for n.draws and probs)
## I can simply replace the defaults by specifying specific values for the arguments

test2 <- my.func(n.draws=10000, mean=5, sd=3, probs=c(.25,.75))


# Also, note that I avoided "hard-coding".  That is, I made as many things part of the function arguments as possible.  So instead of putting the mean and the sd inside the function, I made them into arguments so that I can change them without changing the function.  This is good practice and makes your function more flexible.


# Note that I used the cat() function to print out results and I also stored the results 
# using return().  The "\n" tells it to skip to the next line.


##We won't cover this now, but here is an example of a really hard function that you can try to decipher.
##Bell numbers are a function in combinatorial mathematics that calculates the number of partitions of a set with n members. They start very small but quickly grow very large.

bell.numbers <- function(num) {  
  ##Bell Numbers only take certain values, so we perform error checking
  if(class(num)!="numeric") {return("Please enter a positive integer.")} else
  if(num%%1!=0) {return("Number must be an integer.")} else
  if(num<1) {return("The number must be positive.")} else
  
  ##We seed the function here
  i <- 2 
  bell <- c(1,1,2)
  ##The while loop employs a recursive(-esque) function to calculate the bell number
  while (i < num) {
    binomials <- choose((i), (0:(i)))
	bell[i+2] <- sum(binomials*bell)
	i <- i+1
  }
  ##We return a list with both the individual number requested and the vector leading up to it.
  return(list(bell=bell[num+1], allbell=bell))
}

bell.numbers(.5)
bell.numbers("1")
bell.numbers(-1)
bell.numbers(10)


############################
## How to write a simulation
############################

## Suppose there are 7 balls in an urn, 3 red and 4 blue

## We can construct our urn a few ways
urn <- c("red", "red", "red", "blue", "blue", "blue", "blue")
urn <- c(rep("red", 3), rep("blue", 4))
urn <- c(rep(1, 3), rep(0, 4))

## For now let's use an urn with numbers representing color

## Some basic operations we can perform on our urn are
length(urn)
unique(urn)
length(unique(urn))
duplicated(urn)
urn[duplicated(urn)]
sum(duplicated(urn))
urn == 1
sum(urn == 1)
sum(urn)
sum(urn == 0)
urn[urn==1]
table(urn)

## We can draw balls from our urn with 
sample(urn, 3)
sample(urn, 10) #this won't work!  Sample uses replace=FALSE by default.
sample(urn, 10, replace = TRUE)

## To make our work replicable, we can set the state of the random number generator
set.seed(01238)

## A single sample isn't very interesting.  Suppose we're interested in the
## number of red balls we're likely to have in a draw of 4 balls
## without replacement

sample(urn, 4, replace = FALSE)

sims <- 1000
mat <- matrix(data = NA, nrow = sims, ncol = 4)
for(i in 1:sims){
	mat[i,] <- sample(urn, 4, replace = FALSE)
}
 
## We need to sum each row of our matrix and average the sums.  
## We can either use a for loop,
vec <- c()
for (i in 1:nrow(mat)){
 	vec[i] <- sum(mat[i,])
}
mean(vec)

## Or the apply() command which applies a function to each row or column of a matrix
mean(apply(X = mat, MARGIN = 1, FUN = sum))

## If we wanted to know the total number of reds drawn first, second, third or fourth, 
apply(X = mat, MARGIN = 2, FUN = sum)

## And their probabilities,
apply(X = mat, MARGIN = 2, FUN = sum)/sims

## Let's look at apply() in greater detail.  Apply works with any function
apply(X = mat, MARGIN = 2, FUN = mean)

## What if we want to know both the sum and the mean of the columns?
apply(X = mat, MARGIN = 2, FUN = c(mean, sum)) #doesn't work

## Instead, we can write
do.both <- function(x){
  out <- c(mean(x), sum(x))
  return(out)
}

both <- apply(X = mat, MARGIN = 2, FUN = do.both)
rownames(both)<- c("mean", "sum")
both

## Now suppose we want to sample from a matrix.  
## Let's fill a matrix with random draws of size 5 from a normal distribution

mat <- matrix(data = NA, nrow = 100, ncol = 5)
for (i in 1:nrow(mat)){
	mat[i,] <- rnorm(ncol(mat))
}

## To sample a new matrix with 50 rows, we use

index <- sample(1:100, 50, replace = TRUE)
matnew <- mat[index,]	

##This gave one sampled dataset

##Suppose we want to know the average median for each column.  We can use a for loop:
sims <- 100
holder <- matrix(data = NA, nrow = sims, ncol = ncol(mat))
for (i in 1:sims){
	index <- sample(1:100, 50, replace = TRUE)
	matnew <- mat[index,]
	holder[i,] <- apply(matnew, 2, median)
}
apply(holder, 2, mean)
	


## To print more explanatory output, you can use cat()

for(i in 1:ncol(holder)){
 cat("sum of column",i," is equal to",sum(holder[,i]),"\n")
}

###########################################################################

# putting it all together: a monte carlo study to simulate the
# sampling distribution of two estimators
# we consider the following two estimators:

# 1. the sample total: sum(x)
# 2. the mean of the 7th and 8th sample element: mean(x[c(7,8)])


# we simulate the sampling distribution of these estimators as follows,
# 1. we draw M random samples of size n from from the population
# 2. in each sample, we compute the estimate with the estimator
# 3. the estimates over M samples represent the sampling distribution of the estimator
# 4. we can compare the parameters of the sampling distribution to the true population parameters

# set the sample size and the number of simulations
M <- 10000
n <- 15

# define the population
population <- rnorm(5000,mean=0,sd=1)

# visualize the population
hist(population,xlab="population x")

# here are the population mean and sd
true.mean <- mean(population)
true.sd <- sd(population)

# now we define an empty store matrix where we will store the M samples
store <- matrix(NA,M,n) # on row for each sample of size n
head(store)

# now we draw M samples from the population and stick them
# in the rows of our store matrix
for(i in 1:M){
  store[i,] <- sample(population,n,replace=FALSE)
}

# define the two estimators:

# the sum: (could also just use sum() of course)
our.sum.estimator <- function(x){
 out <- sum(x)
 return(out)
}

# the mean of the 7th and 8th element
our.mean.7.8.estimator <- function(x){
  out <- mean(x[c(7,8)])
  return(out)
}

# now we need to apply each of the estimators to each of the
# samples and store the estimates

# the cumbersome way:
store.est <- matrix(NA,M,2) # build a store matrix for the estimates
colnames(store.est) <- c("sum(x)","mean(x[c(7,8)])")

for(i in 1:M){ # loop through samples
 x <- store[i,] # pull out sample
 store.est[i,1] <- our.sum.estimator(x)      # apply sum estimator to sample and store
 store.est[i,2] <- our.mean.7.8.estimator(x) # apply sum estimator to sample and store
 cat("Estimators applied to Sample No",i,"\n")
}

# look at the results
head(store.est)
summary(store.est)
# how do we interpret the results?
true.mean; true.sd

par(mfrow=c(2,1))
hist(store.est[,"sum(x)"],xlab="sum(x) hat")
abline(v=true.mean,col="red") # here we superimpose the true population mean
hist(store.est[,"mean(x[c(7,8)])"],xlab="mean(x[c(7,8)]) hat")
abline(v=true.mean,col="red") # here we superimpose the true population mean






