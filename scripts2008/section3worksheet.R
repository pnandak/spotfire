## sdtion worksheet number 3
## Jens Hainmueller October 02, 2007

## R commands that are important (and also help with the homework)


# plot multiple figures on one screen
# and adding vertical and horizontal lines to plots and text
# dynamicaly creating plot titles using paste
# this code illustrates it all:
par(mfrow=c(2,2)) # sets up a 2 by 2 plot window
for(i in 1:4){
plot(rnorm(1000),main=paste("Plot No",i)) # use paste to number the plots in title
abline(v=500,col="red",lwd=2) # add a vertical line
abline(h=0,col="green",lwd=2) # add a horizontal line
text(700,-2.5, "Hello World",col="blue")
}
dev.off() # turns it off

# here is how paste works
i <- "very well"
paste("How are you?",i,"\n")

# cat also does the job
cat("How are you?",i,"\n")

# Functions: Functions are just commands
# that you write on your own
# they take inputs (so called arguments) and transform them
# into outputs (so called return values)

# Basic setup
func.name <- function(x){
out <- x
return(out)
}

# evaluate
func.name(x=10)
# we just get x back

# now estimators can be implemented as functions
# since estimators also take some input (data)
# and return some output (the estimate)

# consider the deletion.3 estimator discussed in the teaching problem
# what does it do? in words:
# 1. takes as input a vector x
# 2. sorts x
# 3. deletes the second highest value
# 4. compute the standart deviation
# 5  return the standart deviation

# sorting is done by sort(x)
x1 <- 9:1;x1
sort(x1) # sorts the vector

# a simple implementation (we assume no ties for now)

deletion.estimator.3a <-
  function(x){
 x <- sort(x) # stors the vector in increasing order
              # so that now last value of sort will be the maximum
              # the second to last will thus be the value we want to delete
              # there are 100 ways do to this.
              # for example by creating an indexing vector
 n <- length(x) # the number of elements in x
 index.vector <- c(1:(n-2),n) # this will contain all the numbers
                              # refering to the positions in x expect the second to last one
                              # so if x = 1,2,3,4
                              # index vector = 1,2,4
              # now use the subset vector to reduce x
 x.reduc <- x[index.vector]
 # now we compute the standart devaition of x and return it
 out <- sd(x.reduc)
 return(out)
}

# evaluate
x1 <- c(1,2,3,4,4)
deletion.estimator.3a(x=x1)

# here is a more elegent version that uses the fact that
# we can eleminate an element from a vector by indexing
# the position to eliminate with a minus sign
x1
x1[1:4]
x1[-2] # eliminates the second element
x1[-c(1,4)] #eleminates the first and the fourth element

# using this fact the estimator can be implemented as
deletion.estimator.3b <-
  function(x){
 out <- sd(sort(x)[-(length(x)-1)]) # here is the money: we dynmally grab n as length(x)
 return(out)                       # and enter -(n-1) as the subset index to the sorted vector
}

# Evaluate
deletion.estimator.3b(x1)

# now also get rid of the canned sd() function
deletion.estimator.3c <-
  function(x){
  x.reduc <- sort(x)[-(length(x)-1)] # get the reduced x vector
  n.reduc <- length(x.reduc)
  # here is most transparant way of computing the sd that i could come up with
  x.bar <- mean(x.reduc)   # get he mean
  squared.deviations <- (x.reduc - x.bar)^2 # compute the squared deviations
  sum.squared.deviations <- sum(squared.deviations) # sum them
  x.var <- sum.squared.deviations * 1 / (n.reduc - 1)   # scale by 1/(n-1)
  out <- sqrt(x.var)
  return(out)
}

# evaluate
deletion.estimator.3c(x1)

# evidently, we can code this more elegantly (and we could probably reduce further)
deletion.estimator.3d <-
  function(x){
  out <- sqrt(1/(length(x) - 2) *
         sum((sort(x)[-(length(x)-1)] - mean(sort(x)[-(length(x)-1)]))^2))
  return(out)
}

# evaluate
deletion.estimator.3d(x1)

# here is modified version of the function that also deals with ties
# as it will always delete all the elements that are equal to the
# second highest element
# so x1 = 1,2,3,3,4,4 -> x.reduc = 1,2,4,4
deletion.estimator.3e <-
  function(x){
  if(sd(x)==0){stop("No variation in x")}
  x.reduc <- x[
               x != x[
                      min(
                          which(sort(x1)==max(x1))
                          )-1
                     ]
              ] # examine this line closley at home
  out <- sd(x.reduc)
  return(out)
}

# evaluate
deletion.estimator.3e(x1)

# in the homework there are no ties!! we sample without replacement so no reason to worry


# apply() - a key command that applies
# a function to each row or column of a matrix

# examine the following matrix
store <- matrix(c(1:8),2,4) ; store

# how can we compute the sum for each row of the matrix
# we can use a for loop
# we let i go from 1 to 2 as there are two rows
# and in each iteration we compute the sum of the ith row
# and print it on the screen
# here is the code
for(i in 1:nrow(store)){
 cat("sum of row",i," is equal to",sum(store[i,]),"\n")
}

# similiary we could compute the sums over the columns of the matrix
for(i in 1:ncol(store)){
 cat("sum of column",i," is equal to",sum(store[,i]),"\n")
}

# so in the for loops we apply the function sum() iterativley for
# each row or column.

# an alternative way to apply a function like sum to all of the
# rows or columns at the same time is the apply function
# here is how it works
# get the sum for each row
# the MARGIN 1 means that the function (here: sum) is
# applied over the rows
apply(X=store,MARGIN=1,FUN=sum)

# get the sum for each column
# the MARGIN 2 means that the function (here: sum) is
apply(X=store,MARGIN=2,FUN=sum)

# this works for all functions that take a numeric vector
# here we take the mean over rows and columns
apply(X=store,MARGIN=1,FUN=mean)
apply(X=store,MARGIN=2,FUN=mean)

# let's take it to the next step and apply three functions
# to the vectors of a matrix at the same time
three.at.once <- function(x){
  out <- c(mean(x),sum(x),sd(x))
  return(out)
}

# take a look very at the following line of code
# rerun it many times until your absolutly clear
# on what happens here
# if you don't understand it afer rerunning many times
# you can come to office hours and i will explain it again

# we pass our function to apply
# so that three.at.once() will be applied to
# each column of the store matrix
Z <- apply(X=store,MARGIN=2,FUN=three.at.once)
Z
rownames(Z) <- c("Mean","Sum","Sd")
Z

# so for each column we get back a three element vector
# one with the column mean, one with the colum sum
# and one with the column sd

# we can do the same operation over the rows
Z <- apply(X=store,MARGIN=1,FUN=three.at.once)
Z
rownames(Z) <- c("Mean","Sum","Sd")
Z
# we only get two columns elements back here, because
# store have two rows


# putting it all together: a monte carlo study to simulate the
# sampling sidtribution of two estimators
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


# what is going on here? bias, efficiency
# would this work if population is nonnormal?

# now back to apply: instead of using a for loop we can  use apply
# to apply the estimators to each row in the store matrix

# apply the our.sum.estimator to each sample
sampling.dist1 <- apply(store,1,our.sum.estimator)
# apply the our.mean.7.8.estimator to each sample
sampling.dist2 <- apply(store,1,our.mean.7.8.estimator)

# and we get the same pictures:
par(mfrow=c(2,1))
truehist(sampling.dist1,xlab="sum(x) hat")
abline(v=true.mean,col="red",lwd=2)
truehist(sampling.dist2,xlab="mean(x[c(7,8)]) hat")
abline(v=true.mean,col="red",lwd=2)
dev.off()

# one last time: here is how to do it all at once.
two.estimators.at.once <- function(x){
out <- c(our.sum.estimator(x),our.mean.7.8.estimator(x))
}

# this applies both estimators at the same time:
sampling.dists <- apply(store,1,two.estimators.at.once)
row.names(sampling.dists) <- c("sum(x) hat","mean(x[c(7,8)]) hat")

# short code for plotting
for(i in row.names(sampling.dists)){
truehist(sampling.dists[i,],xlab=paste(i))
abline(v=true.mean,col="red",lwd=2)
}

# MSE? think up the pseudo code and implement step by step









