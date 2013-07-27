# This is a quick example program that performs a simple Monte Carlo
# experiment to illustrate the sampling distribution of t-statistics
# from a simple linear regression model.
#
# The number of beta coefficients (including an intercept) is hardcoded
# to be 3.
#
# Kevin Quinn
# 2/3/2003
#

library(lattice)


M <- 1000         # number of Monte Carlo iterations
N <- 20           # sample size

t.store <- matrix(NA, M, 3)  # matrix to store the beta estimates

# the following are the true values of beta that generate the data
beta0.true <- 0     # the intercept
beta1.true <- 0     # the coefficient on x1
beta2.true <- 0     # the coefficient on x2
sigma2     <- 25     # variance of the disturbances

# the Monte Carlo experiment begins here
for (iter in 1:M){

  # sample the data from the "population"
  x1 <- runif(N)
  x2 <- rnorm(N, mean=x1)
  epsilon <- rnorm(N, mean=0, sd=sqrt(sigma2))
  y  <- beta0.true + beta1.true*x1 + beta2.true*x2 + epsilon

  # find the least squares estimates given our sample data
  lm.out <- lm(y~x1+x2)

  # put the least squares estimates into the storage matrix
  t.store[iter,] <- summary(lm.out)$coefficients[,3]
  
  # print iteration to the screen
  if (iter %% 50 == 0){
    cat("Monte Carlo iteration = ", iter, "\n")
  }
  
}

t0 <- t.store[,1]
t1 <- t.store[,2]
t2 <- t.store[,3]

par(mfrow=c(3,2))
hist(t0, nclass=15, col=5)
qqplot(qt(ppoints(t0), df=(N-3)), t0)
abline(0,1)

hist(t1, nclass=15, col=5)
qqplot(qt(ppoints(t1), df=(N-3)), t1)
abline(0,1)

hist(t2, nclass=15, col=5)
qqplot(qt(ppoints(t2), df=(N-3)), t2)
abline(0,1)

