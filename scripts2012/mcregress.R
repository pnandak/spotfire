# mcregress.R performs a simple Monte Carlo experiment with the standard
# linear model
#
# Kevin Quinn
# Dept. of Political Science and CSSS
# University of Washington
# quinn@stat.washington.edu
#
# 1/14/2002
#

n <- 50   # number of observations
k <- 6    # number of covariates
m <- 5000 # number of Monte Carlo replications

X <- matrix(rnorm(n*(k-1)), n, k-1)  # matrix of ind. vars. (k-1) columns
                                     # b/c the intercept is included
                                     # automatically

bigX <- cbind(1, X)                  # the X matrix with a vector of ones

betahat.mat <- matrix(NA, k, m)      # matrix to hold the MLEs from each
                                     # of the m Monte Carlo replications

coverage.indic <- matrix(NA, k, m)   # matrix of T/F values indicating
                                     # whether betahat fell within the
                                     # 95% confidence interval

beta.true <- matrix(1, k, 1)         # the true value of beta

# conduct m Monte Carlo replications
for (i in 1:m){
  y <- bigX %*% beta.true + rnorm(n, 0, .5)
  lm.out <- lm(y~X)
  beta <- coefficients(lm.out)
  betahat.mat[,i] <- beta
  lm.sum <- summary(lm.out)
  coef.table <- coefficients(lm.sum)
  lower.025 <- coef.table[,1] + qt(0.025, n-k)*coef.table[,2]
  upper.975 <- coef.table[,1] + qt(0.975, n-k)*coef.table[,2]
  coverage.indic[,i] <- (beta.true > lower.025) & (beta.true < upper.975)
}



# plot formatting 
plot.rowsize <- ceiling(sqrt(k)) + 1
plot.colsize <- round(sqrt(k))
par(mfrow=c(plot.rowsize, plot.colsize))

# draw the plots
for (i in 1:k){
  hist(betahat.mat[i,], nclass=30, col=5, xlab=paste("betahat ", i),main="")
  abline(v=beta.true[i], lwd=3, col="red")
}

cat("\n\n\n")
cat("########################################################\n")
cat("   Number of Monte Carlo Replications = ", m,"\n\n")
cat("   Nominal Coverage Probabilities\n")
print(rep(0.95,k))
cat("\n")
cat("   Monte Carlo Coverage Probabilities\n")
print(apply(coverage.indic, 1, mean))
cat("########################################################\n")
cat("\n\n")
