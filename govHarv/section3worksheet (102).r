## Gov 2000 Section 3

## Sampling, For-loops, Graphs and Tables















## Suppose we want to draw ten numbers from a vector

set.seed(12345)

vec <- c(1:100)
ten <- sample(x = vec, size = 10, replace = TRUE);ten


## We could also draw ten numbers, one at a time, and store them
## in a vector.  The process of repeating a task over and over is
## ideal for the use of a 'for loop'. 


holder <- vector(length = 10)

for (i in 1:10){
  draw <- sample(x = vec, size = 1, replace = TRUE)
  holder[i] <- draw
}


## Your code will be more flexible if instead of hard-coding '10',
## you use a variable defined once at the beginning of the code

sims <- 10

holder <- vector(length = sims)

for (i in 1:sims){
  draw <- sample(x = vec, size = 1, replace = TRUE)
  holder[i] <- draw
}







## You can also simplify the code inside the loop by writing

sims <- 10

holder <- vector(length = sims)

for (i in 1:sims){
  holder[i] <- sample(x = vec, size = 1, replace = TRUE)
}







## Now suppose we wanted to draw 10 pairs of numbers instead of 
## 10 individual numbers

## Instead of a vector to hold our draws, we need a...














## matrix.  Here is a transparent way to write the loop:

sims <- 10

store <- matrix(data = NA, nrow = sims, ncol = 2)

for (i in 1:sims){
  store[i,1] <- sample(x = vec, size = 1, replace = TRUE)
  store[i,2] <- sample(x = vec, size = 1, replace = TRUE)
}

## A more efficient loop which does the same thing is:



sims <- 10

store <- matrix(data = NA, nrow = sims, ncol = 2)

for (i in 1:sims){
  store[i,] <- sample(x = vec, size = 2, replace = TRUE)
}





## Now suppose we want to draw 20 elements of vec, record the median of the 
## 20 elements, and do this 1000 times.

sims <- 1000
size <- 20
med.holder <- c() #this is another way to create a holder vector

for(i in 1:sims){
  draw <- sample(x = vec, size = size, replace = TRUE)
  med <- median(draw)
  med.holder[i] <- med
}

head(med.holder)


## Now suppose that we still want to repeatedly sample 20 elements, but
## we want to record the average of the largest and smallest elements

## If we don't know an R function to do this for us, we can
## write our own function and use it in the for loop




my.midpoint <- function(x){ #x is a vector
  ordered <- sort(x)
  smallest <- x[1]
  largest <- x[length(x)]
  out <- (smallest + largest)/2
  return(out)
}


sims <- 1000
size <- 20

mid.holder <- c()

for(i in 1:sims){
  draw <- sample(x = vec, size = size, replace = TRUE)
  mid <- my.midpoint(draw)
  mid.holder[i] <- mid
}


## So now we have 1000 midpoints.  What can we do with them?

###########################################################################
## Plotting in R
############################################################################

## You have already seen histograms

hist(mid.holder)

hist(mid.holder, breaks = 30)

## plot() is the generic plotting function in R.  It can be used 
## to plot a vector of y values against a vector of x values

some.values <- 11:50
other.values <- 1:40

plot(x = some.values, y = other.values)

## plot() can also take a single argument in some special cases, 
## like the density() function

plot(density(mid.holder))

## R has very flexible graphical parameters, far too many to learn in 
## one day.  Here are some highlights:

plot(x = some.values, y = other.values)

plot(x = some.values, y = other.values, col = "red")

plot(x = some.values, y = other.values, col = "blue", pch = 2)

plot(x = some.values, y = other.values, col = "blue", type = "l")

plot(x = some.values, y = other.values, col = "blue", type = "l", 
  lty = "dashed")

plot(x = some.values, y = other.values, col = "blue", type = "l", lwd = 5)

plot(x = some.values, y = other.values, col = "blue", type = "l", lwd = 5,
  main = "A Lovely Plot")

plot(x = some.values, y = other.values, col = "blue", type = "l", lwd = 5,
  main = "A Lovely Plot", xlab = "Age of participants", 
  ylab = "Pairs of shoes owned")

plot(x = some.values, y = other.values, col = "blue", type = "l", lwd = 5,
  main = "A Lovely Plot", xlab = "Age of participants", 
  ylab = "Pairs of shoes owned", xlim = c(-10,100), ylim = c(0,60))



## Often you will want to compare two plots by overlaying them.
## To do this, plot one using a traditional plotting command which calls
## a canvas (such as plot() )


plot(x = some.values, y = other.values, col = "blue", type = "l", lwd = 5,
  main = "A Lovely Plot", xlab = "Age of participants", 
  ylab = "Pairs of shoes owned", xlim = c(-10,100), ylim = c(0,60))

## And overlay as many plots as you like using lines() instead of plot()

lines(x = some.values, y = 2*other.values, col = "hotpink", lwd = 2)

lines(x = some.values - 5, y = .5*other.values, col = "cadetblue", lwd = 2)






## abline() is a useful plotting function

plot(density(mid.holder), lwd = 2, col = "purple")

abline(v = mean(mid.holder), col = "orange")


## Suppose we want to plot a density of the normal centered at the 
## mean of mid.holder with standard deviation equal to the 
## standard deviation of mid.holder


## We could sample from a normal with these specifications and 
## plot the density

norm.draws <- rnorm(10000, mean = mean(mid.holder), sd = sd(mid.holder))
plot(density(norm.draws))

## To overlay this on the previous plot, we simply use lines()
## instead of plot() for the second density plotted



plot(density(mid.holder), lwd = 2, col = "purple")
abline(v = mean(mid.holder), col = "orange")

lines(density(norm.draws))



## A more precise way to plot a normal density is to use another of the
## _norm() functions, this time dnorm(), which evaluates the density
## function at values you feed the function

## The idea is you will evaluate the value of the normal density
## at lots of different x values, then you will plot the ordered pairs
## (x, density(x)).  

## Here is one way to generate lots of different x values

start <- -3 * sd(mid.holder) + mean(mid.holder)
end <- 3 * sd(mid.holder) + mean(mid.holder)

ruler <- seq(from = start, to = end, by = .01)

dens.values <- dnorm(ruler, mean = mean(mid.holder), sd = sd(mid.holder))

plot(x = ruler, y = dens.values, type  = "l")

## So we could overlay this with


plot(density(mid.holder), lwd = 2, col = "purple")
abline(v = mean(mid.holder), col = "orange")
lines(x = ruler, y = dens.values, type  = "l")




