## Section Worksheet
## Feb 14, 2008

## How to write a simulation

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

## We can draw balls from our urn with 

sample(urn, 3)
sample(urn, 10)
sample(urn, 10, replace = TRUE)

## To make our work replicable, we can determine the state of the random number generator
set.seed(12345)

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


# we simulate the sampling distribution of these estimators just as
# described in the lecture slides
# 1. we draw M random samples of size n from
# from the population
# 2. in each sample, we compute the estimate with the estimator
# 3. the estimates over M samples represent the sampling distribution
# of the estimator
# 4. we can compare the parameters of the sampling distribution to the true
# population parameters

# set the sample size and the number of simulations
M <- 10000
n <- 15

# define the population
population <- rnorm(5000,mean=0,sd=1)

# visualize the population
library(MASS)
truehist(population,xlab="population x")

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

# so this is what these samples look like
# you can interupt this run anytime
for(i in 1:100){
plot(density(store[i,]))
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
truehist(store.est[,"sum(x)"],xlab="sum(x) hat")
abline(v=true.mean,col="red") # here we superimpose the true population mean
truehist(store.est[,"mean(x[c(7,8)])"],xlab="mean(x[c(7,8)]) hat")
abline(v=true.mean,col="red") # here we superimpose the true population mean

dev.off()
