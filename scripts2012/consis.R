M <- 1000                  # number of Monte Carlo simulations
pi.store <- matrix(NA,M,4) # storage matrix for the MLEs
true.pi <- 0.6      # true value of pi that generates Y ~ Bernoulli(true.pi)


N.vec <- c(10, 50, 500, 10000)
# loop over sample size
for (j in 1:4){
  N <- N.vec[j]
  # The Monte Carlo simulations
  for (i in 1:M){
    y <- rbinom(N,1,true.pi) # take a sample from the distribution of Y
    pi.hat <- sum(y)/N       # calculate the MLE
    pi.store[i,j] <- pi.hat  # store the MLE from this Monte Carlo sample
  }
}


par(mfrow=c(2,2))
for (i in 1:4){
  main.string = paste("N =", N.vec[i])
  hist(pi.store[,i], col=5, xlim=c(0,1), main=main.string)
  abline(v=true.pi, col="red", lwd=3)
}

