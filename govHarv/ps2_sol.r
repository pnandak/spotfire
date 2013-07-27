## Gov 2000
## PS 2 Solutions

##############################################################
## Problem 2
##############################################################

## First we load the data with 



load("pop_data.RData")

## To look at a density of the population data, we can plot

## [the pdf() and devoff() commands are one way to save graphics
## as pdfs at the command line]

pdf(file = "ps2_plot1.pdf")

plot(density(pop), main = "Density of pop compared to a normal density")

## It doesn't look normal.  For comparison, we can overlay a normal
## density with mean equal to the mean of pop, sd equal to sd of pop

ruler <- seq(from = mean(pop) - 3*sd(pop), to = mean(pop) + 3*sd(pop), by = .1)
heights <- dnorm(ruler, mean = mean(pop), sd = sd(pop))

lines(x = ruler, y = heights, lty = "dashed", col = "red")

dev.off()

## Now we want to repeatedly sample from pop. To expedite parts b-d, I
## will write a function which takes sample size as an argument
## so that each time I want to collect means from repeated samples of pop, 
## I can simply rerun the function.


samp.dens <- function(sims = 1000, size){# note that by putting something
                                         # after the equal sign for sims
                                         # when writing
                                         # the function, I am setting the 
                                         # default value of sims
  holder <- c()  # create an empty vector
  set.seed(12345) # note I set the seed outside of the for loop
  
  for (i in 1:sims){
    draw <- sample(pop, size = size, replace = TRUE)
    holder[i] <- mean(draw)
  }
  return(holder)
}

## Now I can run the function with size = 2, leaving the default sims = 1000

two <- samp.dens(size = 2)

pdf(file = "ps2_plot2.pdf")

plot(density(two), main = "Sample Size = 2", ylim = c(0, .051))
ruler <- seq(from = mean(two) - 3*sd(two), to = mean(two) + 3*sd(two), by = .01)
heights <- dnorm(ruler, mean = mean(two), sd = sd(two))
lines(x = ruler, y = heights, lty = "dashed", col = "red")

dev.off()

## Repeating for sample size 5,

five <- samp.dens(size = 5)

pdf(file = "ps2_plot3.pdf")

plot(density(five), main = "Sample Size = 5", ylim = c(0, .081))
ruler <- seq(from = mean(five) - 3*sd(five), to = mean(five) + 3*sd(five), by = .01)
heights <- dnorm(ruler, mean = mean(five), sd = sd(five))
lines(x = ruler, y = heights, lty = "dashed", col = "red")

dev.off()

## And for sample size 50,

fifty <- samp.dens(size = 50)

pdf(file = "ps2_plot4.pdf")

plot(density(fifty), main = "Sample Size = 50", ylim = c(0, .26))
ruler <- seq(from = mean(fifty) - 3*sd(fifty), to = mean(fifty) + 3*sd(fifty), by = .01)
heights <- dnorm(ruler, mean = mean(fifty), sd = sd(fifty))
lines(x = ruler, y = heights, lty = "dashed", col = "red")

dev.off()

######################################################################
## Problem 3
######################################################################

## First we load the data

load("housedems06.RData")

## Treating candidates' receipts as the population, the population mean
## and variance are
mean(house$receipts)
var(house$receipts)

## Now a function for the sample mean is

my.mean <- function(x){ #x is a vector
  out <- sum(x)/length(x)
  return(out)
}

## The sample median is

my.median <- function(x){ #x is a vector
  ordered <- sort(x)
  index <- trunc(.5*(length(x)+1))
  index.2 <- length(x) + 1 - index
  out <- .5*(ordered[index] + ordered[index.2])
  return(out)
}

## The deletion estimator is

my.deletion <- function(x){ #x is a vector
  ordered <- sort(x)
  remaining <- ordered[-c(1,2,length(x)-1, length(x))]
  out <- sum(remaining)/length(remaining)
  return(out)
}
  

## Once again, I'll write a function to repeatedly sample to make part d 
## easier.  The function will use all three estimators and store the 
## estimates in a matrix.

samp.receipts <- function(sims = 1000, size){ 
  holder <- matrix(data = NA, nrow = sims, ncol = 3)
  colnames(holder) <- c("mean", "median", "deletion")

  for(i in 1:sims){
    draw <- sample(house$receipts, size = size, replace = TRUE)
    holder[i,1] <- my.mean(draw)
    holder[i,2] <- my.median(draw)
    holder[i,3] <- my.deletion(draw)
  }

  return(holder)
}

r.hundred <- samp.receipts(size = 100)

## Now I plot the sampling distributions of the three estimators

pdf(file = "ps2_plot5.pdf")

par(mfrow= c(1,3))
plot(density(r.hundred[,"mean"]), main = "Samp. Dens. of Mean", xlim = c(700, 1800))
abline(v = mean(house$receipts), col = "green")
abline(v = mean(r.hundred[,"mean"]), col = "blue", lty = "dashed")


plot(density(r.hundred[,"median"]), main = "Samp. Dens. of Median", xlim = c(700, 1800))
abline(v = mean(house$receipts), col = "green")
abline(v = mean(r.hundred[,"median"]), col = "blue", lty = "dashed")

plot(density(r.hundred[,"deletion"]), main = "Samp. Dens. of Deletion", xlim = c(700, 1800))
abline(v = mean(house$receipts), col = "green")
abline(v = mean(r.hundred[,"deletion"]), col = "blue", lty = "dashed")

dev.off()

## Here I calculate the means and standard deviations for the three sampling 
## distributions for n = 100,

mean(r.hundred[,"mean"])
sd(r.hundred[,"mean"])
mean(r.hundred[,"median"])
sd(r.hundred[,"median"])
mean(r.hundred[,"deletion"])
sd(r.hundred[,"deletion"])

## And for n = 50

r.fifty <- samp.receipts(size = 50)

mean(r.fifty[,"mean"])
sd(r.fifty[,"mean"])
mean(r.fifty[,"median"])
sd(r.fifty[,"median"])
mean(r.fifty[,"deletion"])
sd(r.fifty[,"deletion"])

## And for n = 5

r.five <- samp.receipts(size = 5)

mean(r.five[,"mean"])
sd(r.five[,"mean"])
mean(r.five[,"median"])
sd(r.five[,"median"])
mean(r.five[,"deletion"])
sd(r.five[,"deletion"])
