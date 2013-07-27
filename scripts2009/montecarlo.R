payoff <- function(path)
{
	exp(-rate*bigt)*max(stock*exp(path[len])-strike,0)
}

maxopt <- function(path)
{
	exp(-rate*bigt)*max(stock*exp(max(path))-strike,0)
}

rate <- .0
sigma <- .2
bigt <- 1
stock <- 100
strike <- 130
nsim <- 10000
len <- 100

delt <- bigt/len
drift <- rate-sigma^2/2

# Brute force Monte Carlo

eps <- matrix(rnorm(len*nsim,mean=0,sd=1),ncol=nsim)
spath <- apply(eps*sigma*sqrt(delt)+drift*delt,2,cumsum)

spay <- apply(spath,2,payoff)
mean(spay)
sd(spay)/sqrt(nsim)

# Antithetic Variate

peps <- matrix(rnorm(len*nsim/2,mean=0,sd=1),ncol=nsim/2)
eps <- cbind(peps,-peps)
spath <- apply(eps*sigma*sqrt(delt)+drift*delt,2,cumsum)

spay <- apply(spath,2,payoff)
mean(spay)
sd(spay)/sqrt(nsim)

# Importance Sampling

mu <- .4
theta <- mu/sigma*sqrt(delt)

eps <- matrix(rnorm(len*nsim,mean=theta,sd=1),ncol=nsim)
dens <- exp(-colSums(eps)*theta+theta^2*len/2)
spath <- apply(eps*sigma*sqrt(delt)+drift*delt,2,cumsum)

spay <- apply(spath,2,payoff)*dens
isval = mean(spay)
isval
sd(spay)/sqrt(nsim)

# Control Variate

eps <- matrix(rnorm(len*nsim,mean=0,sd=1),ncol=nsim)
spath <- apply(eps*sigma*sqrt(delt)+drift*delt,2,cumsum)

spay <- apply(spath,2,payoff)
apay <- apply(spath,2,asian)
mean(apay)
sd(apay)/sqrt(nsim)
cf = coef(lm(apay ~ spay))
cf[1]+cf[2]*isval

#df = data.frame(x= rep(isval,nsim))
#nd = predict(lm(apay ~ spay),df)
#mean(nd)
#sd(nd)/sqrt(nsim)
