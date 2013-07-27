## Gov 2001 Assignment 7 Solutions

## 1.2

poisson.ll <- function(par, y, X){
  xb <- X %*% par
  ll <- sum(y * xb - exp(xb))
  return(ll)
}


## 2.2

zip.ll <- function(par, y, X, Z, d){
  betas <- par[1:ncol(X)]
  gammas <- par[(ncol(X)+1):length(par)]
  p <- 1/(1+exp(-Z %*% gammas))
  lambda <- exp(X %*% betas)
  ll.0 <- log(p + (1-p) * exp(-lambda))
  ll.1 <- log(1-p) + y*log(lambda) - lambda
  ll <- sum(d*ll.0 + (1-d)*ll.1)
  return(ll)
}


## 2.3

library(pscl)
data(bioChemists)

# data cleaning
bioChemists$fem <- ifelse(bioChemists$fem == "Women", 1, 0)
bioChemists$mar <- ifelse(bioChemists$mar == "Married", 1, 0)


X <- cbind(1, bioChemists$mar, bioChemists$fem, bioChemists$kid5, bioChemists$phd, bioChemists$ment)
Z <- cbind(1, bioChemists$mar)
y <- bioChemists$art
d <- ifelse(bioChemists$art == 0, 1, 0)

zip.opt <- optim(par=c(0,0,0,0,0,0,0,0), fn=zip.ll, y=y, X=X, Z=Z, d=d,
method="BFGS", control=list(fnscale=-1), hessian=T)
zip.coefs <- zip.opt$par
zip.vcov <- solve(-zip.opt$hessian)
zip.se <- sqrt(diag(zip.vcov))

summary(zeroinfl(art~mar+fem+kid5+phd+ment|1+mar, data=bioChemists))


## 2.4

# making counterfactual x and z vectors
x <- c(1, 0, median(bioChemists$fem), median(bioChemists$kid5), 
median(bioChemists$phd), median(bioChemists$ment))
x1 <- x
x1[2] <- 1
z <- c(1, 0)
z1 <- c(1,1)

# simulating ev's and fd's
set.seed(12345)
simpar <- mvrnorm(1000, mu=zip.coefs, Sigma=zip.vcov)
xbeta <- simpar[,1:length(x)] %*% as.matrix(x)
x1beta <- simpar[,1:length(x)] %*% as.matrix(x1)
zgamma <- simpar[,(length(x)+1):ncol(simpar)] %*% as.matrix(z) 
z1gamma <- simpar[,(length(x)+1):ncol(simpar)] %*% as.matrix(z1) 
ev.low <- exp(xbeta) * (1 - 1/(1 + exp(-zgamma)))
ev.high <- exp(x1beta) * (1 - 1/(1 + exp(-z1gamma)))
fd <- ev.high - ev.low
mean(fd)
sd(fd)


## 3.1

# create the two hypothetical individuals by taking their covariates 
# and dropping the intercept.  also create the matrix of all individuals.
cf <- rbind(x, x1)[,-1]
all.data <- X[,-1]

# are they in the convex hull?
library(WhatIf)
ch.1 <- whatif(data=all.data, cfact=cf)
ch.1$in.hull


## 3.2

# switch everybody's marital status
cf.matrix <- all.data
cf.matrix[,1] <- 1 - cf.matrix[,1]

# what percent of these cfs are in the convex hull?
ch.2 <- whatif(data=all.data, cfact=cf.matrix)
mean(ch.2$in.hull)





