## 2001 section 4
## February 28
## Jens Hainmueller and Jenn Larson

## 1) Reparameterization
## 2) Log likelihoods
## 3) Multiple plots on a graph

## To maximize with constraints on the parameters, it is best to reparameterize
## the parameters so the maximization acts as if it is unconstrained.

## If your goal is to maximize this function,
fun <- function(par){
	x <- par[1]
	out <- 3*log(x)-(x-4)^2
	return(out)
}


##You will need to constrain your search to positive values.  To see this,
## take a look

a <- seq(from = -16, to = 16, by = .01)
holder <- c()
for (i in 1:length(a)){
	holder[i] <- fun(a[i])
}

plot(a, holder)

##We get warnings because natural log is undefined at negative values and evaluates
## to negative infinity at 0.  This should hint that when we optimize, the algorithm
## will give errors if it tries nonpositive values:

opt <- optim(par = 0, fn = fun, method = "BFGS", control = list(fnscale = -1))

opt <- optim(par = 3, fn = fun, method = "BFGS", control = list(fnscale = -1))

points(opt$par, fun(opt$par), pch = "X", col = "red")

##Yep, starting value 0 gives errors, though here starting values > 0 work.  
##We will rarely be even this lucky.  Instead, we should make sure that any guess
## optim makes will be converted to a positive value before it is evaluated in fun:

## To do this, we REPARAMETERIZE

fun.rep <- function(par){
	x <- exp(par[1])
	out <- 3*log(x)-(x-4)^2
	return(out)
}

opt.rep <- optim(par = 0, fn = fun.rep, method = "BFGS", control = list(fnscale = -1))
opt.rep$par  #Maximum

## Why is this maximum wrong?:







## Because when optim guessed par = 1.469, our function used exp(1.469).  
## So the maximum of the function we're interested in is at exp(1.469)

exp(opt.rep$par)


## What if a paramter is constrained to [0,1]?









## A useful map from the real line to [0,1] is any cdf
pnorm()
pt()
pbinom()
ppois()

## A cdf gives the probability of a value <= the given value
pnorm(3)
## you can also change the distribution, but for our purposes, a standard normal would suffice
pnorm(3, mean = 4, sd = 2)


#############################################################################################
## Implementing a log-likelihood in R 
#############################################################################################

## The Poisson:
ll.pois <- function(par, y = y.pois){
	lambda <- par[1]
	out <- sum(log(lambda^y)) - (length(y) * lambda)
	return(out)
}


## Make some data distributed Poisson with rate 3
set.seed(12345)
y.pois <- rpois(1000, lambda = 3)

## What should we expect as our maximum likelihood estimate?
sum(y.pois)/1000

## The parameter of interest (lambda) is > 0.  

## Let's take a look at our log-likelihood

a <- seq(from = -16, to = 16, by = .01)
holder <- c()
for (i in 1:length(a)){
	holder[i] <- ll.pois(a[i])
}
plot(a, holder)


## The warnings suggest that optim may struggle:
opt <- optim(par = 3.5, fn = ll.pois, y = y.pois, method = "BFGS", control = list(fnscale = -1))

## Yep, here, even with a positive starting value, we get warnings

## We could use constrained optimization

op <- optimize( f = ll.pois, interval = c(1, 20), maximum = TRUE)

## But in general we want to avoid constrained optimization, so we reparameterize

ll.pois.rep <- function(par, y = y.pois){
	lambda <- exp(par[1])
	out <- sum(log(lambda^y)) - (length(y) * lambda)
	return(out)
}

opt <- optim(par =5, fn = ll.pois.rep, y = y.pois, method = "BFGS", control = list(fnscale = -1))
opt$par
exp(opt$par)

## What else could we do?

ll.pois.rep <- function(par, y = y.pois){
	lambda <- par[1]^2
	out <- sum(log(lambda^y)) - (length(y) * lambda)
	return(out)
}

opt <- optim(par =5, fn = ll.pois.rep, y = y.pois, method = "BFGS", control = list(fnscale = -1))
opt$par
opt$par^2

## What could be a problem with this parameterization (hint: what happens to a guess of 0?)


## Relative values: can we return(out - 20) ?

ll.pois.rep <- function(par, y = y.pois){
	lambda <- exp(par[1])
	out <- sum(log(lambda^y)) - (length(y) * lambda)
	return(out-20)
}


opt <- optim(par =5, fn = ll.pois.rep, y = y.pois, method = "BFGS", control = list(fnscale = -1))
opt$par
exp(opt$par)

## Yes, our likelihood can be vertically shifted any way we like.  We are only interested
## in its relative output: which point achieves the maximum, regardless of what the maximum 
## function value is

##########################################################################################
## Multiple Plots on a Graph
##########################################################################################

## The first thing you plot must call a canvas.  So you need to use something like plot()

## You can add to a plot with 
lines()
points()
abline()

## Consider our earlier plot of the log-likelihood

plot(a, holder, type = "l", lwd = 2)
abline(v = exp(opt$par), col = "skyblue")


## Suppose we wanted to plot our reparameterized log-likelihood on top


a <- seq(from = -16, to = 16, by = .01)
holder.rep <- c()
for (i in 1:length(a)){
	holder.rep[i] <- ll.pois.rep(a[i])
}

lines(a, holder.rep, col = "blue", lty = "dashed")

abline(v = opt$par, col = "hotpink", lty = "dashed")

## See the help file for plot() and the parameters 'par' for many many ways to spend an 
## afternoon tweaking a plot

## Adding a legend:

legend(x = "topleft", col = c("black", "skyblue", "blue", "hotpink"), lty = c(1,1,2,2), 
	legend = c("LL of Poisson", "MLE", "Rep LL of Poissin", "Rep MLE"))



