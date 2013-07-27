rm(list = ls())

## 1

# 1.1
# analytic approach 
a <- seq(54, 49, -1)
prod((a-8)/a) 

#doublecheck
(choose(8, 0)*choose(46,6))/choose(54,6)

#triplecheck
dhyper(0, m = 8, n = 54-8, k = 6)

# R function
caucus.poll <- function(n.draws = 100, n = 54, m = 8, k = 6){
  pop <- c(rep(0,n-m), rep(1, m))
  mccainiacs <- c()
  for(i in 1:n.draws){
    samp <- sample(x = pop, size = k, replace = FALSE)
    mccainiacs[i] <- sum(samp) < 1
  }
return(mean(mccainiacs))   
}

#quadruplecheck
set.seed(12345)
caucus.poll(n.draws = 10000, n = 54, m = 8, k = 6)


# 1.2
set.seed(12345)
mccainiacs <- caucus.poll(n.draws = 10000, k = 13)
mean(mccainiacs)

caucus.poll <- function(k = 6, n.draws = 100, n = 54, m = 8){
  pop <- c(rep(0,n-m), rep(1, m))
  mccainiacs <- c()
  for(i in 1:n.draws){
    samp <- sample(x = pop, size = k, replace = FALSE)
    mccainiacs[i] <- sum(samp) < 1
  }
return(mean(mccainiacs))   
}

a.seq <- 1:20
probs <- sapply(a.seq, caucus.poll, n.draws = 10000, n = 54, m = 8)
cbind(a.seq, probs)

## 2 

# 2.2
install.packages("VGAM")
library(VGAM)

set.seed(12345)
draws <- rpareto(1000000, shape = 10, location = 9)
mean(draws)

# 2.3 
mean(draws^2 - mean(draws)^2)
var(draws)
mean(sqrt(1 + cos(draws)))

## 3

# 3.1: write OLS solution with matrix algebra

OLS <- function(y,X){  #y is the dependent variable vector, X is the covariate marix
   X <- cbind(1,X)                            #add a column of 1's for the intercept
   colnames(X)[1] <- c("Intercept")           #label the first column 'intercept'
   betas <- solve(t(X) %*% X) %*% t(X) %*% y  #compute the betas with matrix operations
   fitted <- X %*% betas                      #estimate the fitted values
   resid <- fitted - y                        #calculate the residuals
   sigma.sq <- c(t(resid) %*% resid / (nrow(X) - ncol(X)))  #calculate sigma squared
   ses.betas <- sqrt(diag(solve(t(X)%*%X)*sigma.sq))
   output <- list(betas = betas, sigma.sq = sigma.sq, ses.betas = ses.betas)    
   	# return list of betas, sigma^2, ses
   return(output)   
}

# 3.3 
setwd("C:/Documents and Settings/Iain Osgood/My Documents/Iain's Stuff/Gov 2001/PS1")
covs <- as.matrix(read.csv("covs.csv"))

set.seed(12345)

n <- 1000

betas <- c(4, 2, -3, 1)
covs.aug <- cbind(1, covs)
means <- covs.aug%*%betas

y.sim <- rnorm(n, means, 3)

reg <- OLS(y.sim, covs)
betas <- reg$betas
sigma.sq <- reg$sigma.sq
ols.ses <- reg$ses.betas

summary(lm(y.sim ~ covs))

# 3.4 use nonparametric bootstrap to estimate the standard errors 
set.seed(12345)
B <- 1000

betas.bs <- matrix(data = NA, ncol = 4, nrow = B)
for(i in 1:B){
  rows <- sample(1:nrow(covs), replace = TRUE)
  betas.bs[i,] <- OLS(y.sim[rows], covs[rows,])$betas
}

bs.ses <- apply(betas.bs, 2, sd)

table.data <- cbind(betas, ols.ses, bs.ses)

library(xtable)
xtable(table.data, digits = 4)





