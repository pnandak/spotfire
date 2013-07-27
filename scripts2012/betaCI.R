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
coverage.indic <- matrix(NA, M, 3)   # matrix of T/F values indicating
                                     # whether betahat fell within the
                                     # 95% confidence interval


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

  # calculate 95% CI and check coverage
  lm.sum <- summary(lm.out)
  coef.table <- coefficients(lm.sum)
  lower.025 <- coef.table[,1] + qt(0.025, N-3)*coef.table[,2]
  upper.975 <- coef.table[,1] + qt(0.975, N-3)*coef.table[,2]
  coverage.indic[iter,1] <- ((beta0.true > lower.025[1])
                             & (beta0.true < upper.975[1]))
  coverage.indic[iter,2] <- ((beta1.true > lower.025[2])
                             & (beta1.true < upper.975[2]))
  coverage.indic[iter,3] <- ((beta2.true > lower.025[3])
                             & (beta2.true < upper.975[3]))
  
  
  
  # print iteration to the screen
  if (iter %% 50 == 0){
    cat("Monte Carlo iteration = ", iter, "\n")
  }
  
}

cat("\n\n\n")
cat("########################################################\n")
cat("   Number of Monte Carlo Replications = ", M,"\n\n")
cat("   Nominal Coverage Probabilities\n")
print(rep(0.95,3))
cat("\n")
cat("   Monte Carlo Coverage Probabilities\n")
print(apply(coverage.indic, 2, mean))
cat("########################################################\n")
cat("\n\n")
