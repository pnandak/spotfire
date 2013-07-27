# This is a quick example program that performs a simple Monte Carlo
# experiment to illustrate the sampling distribution of the OLS
# regression coefficients.
#
# The number of beta coefficients (including an intercept) is hardcoded
# to be 3.
#
# Kevin Quinn
# 2/3/2003
#

M <- 1000         # number of Monte Carlo iterations
N <- 20           # sample size

beta.store <- matrix(NA, M, 3)  # matrix to store the beta estimates
beta.standardized.store <- matrix(NA, M, 3) # matrix to store standardized
                                            # beta estimates

# the following are the true values of beta that generate the data
beta0.true <- 25     # the intercept
beta1.true <- 75     # the coefficient on x1
beta2.true <- -5     # the coefficient on x2
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
  beta.hat <- coef(lm.out)

  # put the least squares estimates into the storage matrix
  beta.store[iter,] <- beta.hat

  # standardize and store the least squares estimates
  X <- cbind(1,x1,x2)
  beta.var <- sigma2 * solve(crossprod(X))
  beta.sd  <- sqrt(diag(beta.var))
  beta.standardized.store[iter,] <- (beta.hat -
    c(beta0.true, beta1.true, beta2.true) )/beta.sd
  
  
  # print iteration to the screen
  if (iter %% 50 == 0){
    cat("Monte Carlo iteration = ", iter, "\n")
  }
  
}


cat("\n\n#############################################################\n")
cat(    " The least squares estimator is unbiased:\n")
cat(    "    True beta0 = ", beta0.true, "   mean of estimated beta0 = ",
    mean(beta.store[,1]), "\n")
cat(    "    True beta1 = ", beta1.true, "   mean of estimated beta1 = ",
    mean(beta.store[,2]), "\n")
cat(    "    True beta2 = ", beta2.true, "   mean of estimated beta2 = ",
    mean(beta.store[,3]), "\n")
cat("\n")
cat(" Conditional on sigma2 the betas are normally distributed:\n")
par(mfrow=c(3,2))
hist(beta.standardized.store[,1], nclass=15, col=5)
qqnorm(beta.standardized.store[,1])
qqline(beta.standardized.store[,1])
hist(beta.standardized.store[,2], nclass=15, col=5)
qqnorm(beta.standardized.store[,2])
qqline(beta.standardized.store[,2])
hist(beta.standardized.store[,3], nclass=15, col=5)
qqnorm(beta.standardized.store[,3])
qqline(beta.standardized.store[,3])
cat("#############################################################\n")
