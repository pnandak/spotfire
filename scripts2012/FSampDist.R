# This is a quick example program that performs a simple Monte Carlo
# experiment to illustrate the sampling distribution of the F-statistic
# from a simple linear regression model.
#
# The number of beta coefficients (including an intercept) is hardcoded
# to be 5.
#
# Kevin Quinn
# 2/3/2003
#

library(lattice)


M <- 1000         # number of Monte Carlo iterations
N <- 50           # sample size

F.store <- matrix(NA, M, 1)  # matrix to store the beta estimates

# the following are the true values of beta that generate the data
beta0.true <- 0     # the intercept
beta1.true <- 0     # the coefficient on x1
beta2.true <- 0     # the coefficient on x2
beta3.true <- 0     # the coefficient on x3
beta4.true <- 0     # the coefficient on x4
sigma2     <- 25     # variance of the disturbances

# the Monte Carlo experiment begins here
for (iter in 1:M){

  # sample the data from the "population"
  x1 <- runif(N)
  x2 <- rnorm(N, mean=x1)
  x3 <- rnorm(N, mean=x2)
  x4 <- rexp(N)
  epsilon <- rnorm(N, mean=0, sd=sqrt(sigma2))
  y  <- beta0.true + beta1.true*x1 + beta2.true*x2 +
    beta3.true*x3 + beta4.true*x4 + epsilon

  # find the least squares estimates given our sample data
  lm.out <- lm(y~x1+x2+x3+x4)

  # put the least squares estimates into the storage matrix
  F.store[iter,] <- summary(lm.out)$fstatistic[1]
  
  # print iteration to the screen
  if (iter %% 50 == 0){
    cat("Monte Carlo iteration = ", iter, "\n")
  }
  
}

par(pty="s")
hist(F.store, nclass=25, col=5, probability=TRUE)
lines(1:150/10, df(1:150/10, df1=4, df2=45), lwd=2, col="red")


