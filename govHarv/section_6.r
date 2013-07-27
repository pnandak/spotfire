## Section Code
## March 13, 2008
## Delta Method and Quantities of Interest
## Jens Hainmueller and Jenn Larson

#############################################################################
## The Delta Method
#############################################################################

## A quick way to write the log likelihood for a stylized normal (sd = 1) is

log.norm<- function(par, Y){
	mu<- par[1]
	out.norm<- dnorm(Y, mean=mu, sd=1, log=T)
	out<-sum(out.norm)
	return(out)
}

## Now we'll make some data
set.seed(12345)
Y<- rnorm(1000, mean=0.75, sd=2)

opt<- optim(-1, log.norm, Y=Y, control=list(fnscale=-1),method="BFGS", hessian=T)
opt$par

## An analytic calculation of our variance:
an.var<- 1/length(Y)

## We canuse the hessian to calculate the varirance numerically:
num.var<- diag(solve(-opt$hessian))


##now, suppose that we want to know the distribution of the mean squared

## We could simulate:

sim <- rnorm(1000, opt$par, sqrt(num.var))
sim.squared <- sim^2



plot(density(sim.squared))
mean.ci<- mean(sim.squared)
sd.sim<- sd(sim.squared)
sim.CI<- quantile(sim.squared, c(0.025, 0.975))

## Now we can get the same results analytically using...
## The Delta Method

mean.out <- opt$par^2; 
var.out <- 4*opt$par^2 * num.var

## constructing the 95 percent confidence interval
delta.CI <- c(mean.out - sqrt(var.out)*1.96, mean.out + sqrt(var.out)*1.96)

sim.CI
delta.CI

##############################################################################
## Including Covariates
##############################################################################

## The normal log likelihood with mean X*beta is

ll.normal <- function(par, X, Y) {
  n <- length(Y)
  beta <- par[1:ncol(X)] #assuming we've added a column of 1's
  #reparameterize to keep sigma^2>0
  gamma <- par[(ncol(X)+1)] # [2]
  sigma2 <- exp(gamma)
  -0.5 * (n * log(sigma2) + sum((Y - X %*% beta)^2 / sigma2))
}

## Now we'll make some data to illustrate/test the following methods

x1 <- rchisq(1000, df = 3)
x2 <- rnorm(1000, mean = 10, sd = 3)
x3 <- rbinom(1000, size = 30, prob = .7)
error <- rnorm(1000, mean = 0, sd = 2)
Y <- 6 * x1 + .5 * x2 + 3 * x3 + error

## Since we can see behind the scenes at the data generating process,
## we know roughly what to expect as our MLE's

## What should we expect to be the MLE's for the coefficients to x1, x2 and x3?



## Yep, 6, .5 and 3

## And what about the MLE for sigma squared?





## Yep, 4 (2^2)



## So we can make a covariate matrix, remembering to include a column of 1's

X <- cbind(1, x1, x2, x3)
head(X)

## Now obtaining MLE's using optim
## (Remember we have 5 parameters here: an intercept, three slopes, and sigma^2)

opt <- optim(c(0,0,0,0,0), ll.normal, X = X, Y = Y, method = "BFGS", control = list(fnscale = -1), hessian = TRUE)

## So our MLEs of the parameters is
opt$par

## Don't forget that we reparameterized sigma^2
exp(opt$par[5])

## Here's our Hessian
opt$hessian

solve(-opt$hessian)
## The variances are on the diagonal, so our standard errors are

sqrt(diag(solve(-opt$hessian)))

## Remember, we reparameterized sigma squared, so its variance is incorrect

## How can we find the correct variance?







## Yep, either numerically using simulation or analytically using the delta method


## We'll extract the incorrect estimate of the variance of sigma squared
bad.sq <- diag(solve(-opt$hessian))[5]

## The corrected variance using the delta method is

bad.sq * (exp(opt$par[5]))^2


## The corrected variance can also be found with simulation
## (This was called the Zelig method in the last section)

## Draw m simulated sets of parameters from a multivariate normal approximation
## to the likelihood

library(mvtnorm)
args(rmvnorm)

draws <- rmvnorm(1000, mean = opt$par, sigma = solve(-opt$hessian))


## Here are our simulated sigma^2's
draws.sq <- draws[,5]

## Remembering that we reparameterized 
draws.sq.rep <- exp(draws.sq)

## So our corrected variance of sigma squared
var(draws.sq.rep)

###########################################################################
## Simulating Quantities of Interest
############################################################################

library(Zelig)

## Let's make some new data

X.new <- mvrnorm(1000, c(0,0,0,0), Sigma=diag(1, 4))
mu <- 1 + 2*X.new[,1] + 0.5*X.new[,2] -0.7*X.new[,3] + 3*X.new[,4] 
Y.new <- c()

for(i in 1:length(mu)){
Y.new[i]<- rnorm(1, mean=mu[i], sd=3)
}

X.new <- cbind(1, X.new)

ll.normal(c(1,2,3,4,5,-100), X = X.new, Y= Y.new)

##Now maximizing the likellihood in optim

ll <- optim(c(0,0,0,0,0,0), ll.normal, X=X.new, Y=Y.new, method="BFGS", control=list(fnscale=-1), hessian=T)

##and comparing with lm

data.out<- as.data.frame(cbind(Y.new,X.new))
test.run<- lm(Y.new~ X.new[,2] + X.new[,3] + X.new[,4] + X.new[,5], data=data.out)

##Now we can generate estimates of our uncertainty as follows:
se.coeff.mle<- sqrt(diag(solve(-ll$hessian)))[1:5]
se.coeff.ls<- sqrt(diag(vcov(test.run)))

##luckily they are essentially the same, up to differences in tolerance in optim. 


## Now let's change the data so we can use a logit model
## (the dependent variable must be binary)

##Simulating from a Bernoulli distribution
##First, changing our dependent variable

Y.2 <- c()
for(i in 1:length(mu)){
Y.2[i]<- rbinom(1, prob=(1/(1+ exp(-mu[i]))), size=1)
}

## We can estimate the logit coefficients with zelig

new.data <- cbind(Y.2, X.new)
colnames(new.data) <- c("y", "1", "x1", "x2", "x3", "x4")
wer<- zelig(y~x1 + x2 + x3 + x4 , data=as.data.frame(new.data), model="logit")

##now, let's simulate some quantities of interest.  What if we move from 
##0 to 1 on x4, holding all other's constant at their mean?

X <- new.data[,2:6]
c.1<- c(apply(X, 2, mean)[-5], 0)
c.2<- c(apply(X, 2, mean)[-5], 1)

##now we want to generate some simulations
var.covar<- vcov(wer)##getting the variance covariance matrix
beta.draws<- mvrnorm(1000, mu=wer$coeff, Sigma=var.covar)

##now we use these to generate our two mu's
mu.1<- beta.draws%*%c.1
mu.2<- beta.draws%*%c.2
##now we transform to get probabilities
prob.1<- 1/(1 + exp(-mu.1))
prob.2<- 1/(1 + exp(-mu.2))

##and then we can take a first difference
first.diff<- prob.2- prob.1
##how can we present these results
##(1) density plots

plot(density(prob.1), col="red", lwd=2, xlim=c(0.7, 1))
lines(density(prob.2), col="blue", lwd=2)
plot(density(first.diff), col="magenta", lwd=2)

##equivalently we can use Zelig to do the simulations
##here is the code

X <- X.new
Y <- Y.2

colnames(X)<- c("int", "var1", "var2", "var3", "var4")
my.data<- cbind(Y,X)
my.data<- as.data.frame(my.data)
z.out<- zelig(Y~var1 + var2 + var3 + var4 , data=my.data, model="logit")
x.out<- setx(z.out, var4=0)
x1.out<- setx(z.out, var4=1)
s.out<- sim(z.out, x=x.out, x1=x1.out)

##you can access the code using s.out$qi$fd
s.out$qi$fd ##vector of first differences

##we can then plot the density of these first differences
lines(density(s.out$qi$fd), col="green3", lwd=2)

##we can also generate the predicted values
##(we'll do this outside of zelig for the problem set)

sims<- 1000
betas<- mvrnorm(sims, mu=z.out$coef, Sigma=vcov(z.out))
mu<- X%*%c.1
pi<- 1/(1 + exp(-mu))
store<- c()
##now we need to generate 
for(i in 1:sims){
draws<- rbinom(20, size=1, prob=pi[i])
store[i]<- mean(draws)
}
plot(density(store), col="red")
lines(density(pi), col="blue")


