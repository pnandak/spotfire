# gibbsnorm() implements a simple gibbs sampler in order to sample from a
#             bivariate normal distribution.
#
# This program is for pedagogical purposes only.
#
# Inputs:     meanX    mean of X
#
#             meanY    mean of Y
#
#             varX     variance of X
#
#             varY     variance of Y
#
#             rho      corr(X,Y)
#
#             burnin   number of initial iterations to discard
#
#             gibbs    number of iterations to save
#
#             initX    initial value of X
#
#             initY    initial value of Y
#
#
# Output:     storage.matrix  gibbs x 2 matrix holding the draws of (X,Y)
#                             (the draws are ordered by iteration number)
#
#             in addition, a some simple plots are sent to the
#             graphics device
#
#
#
# Kevin Quinn
# Assistant Professor
# Dept. of Political Science and CSSS
# Box 354322
# University of Washington
# Seattle, WA  98195-4322
# quinn@stat.washington.edu
# 
# 1/23/2001 
# Last updated 1/23/2001
#


gibbsnorm <- function(meanX=0, meanY=0, varX=1, varY=1, rho=0, burnin=500, gibbs=1000, initX=0, initY=0){

  tot.iter <- burnin + gibbs
  covXY <- rho*sqrt(varX)*sqrt(varY)
  storage.matrix <- matrix(NA,gibbs,2)
  move.matrix    <- matrix(NA,tot.iter*2+1,2)
  X <- initX
  Y <- initY
  count <- 1
  move.matrix[count,] <- c(X,Y)
  count <- count + 1
  
  for (i in 1:tot.iter){

    # sample X|Y ~ N(mu, tau^2) where
    #              mu = meanX - (covXY/varY)*meanY + (covXY/varY)*Y
    #              tau^2 = varX*(1 - rho^2)
    mu   <- meanX - (covXY/varY)*meanY + (covXY/varY)*Y 
    tau  <- sqrt(varX*(1 - rho^2))
    X    <- rnorm(1, mean=mu, sd=tau)

    move.matrix[count,] <- c(X,Y)
    count <- count + 1
    
    # sample Y|X ~ N(mu, tau^2) where
    #              mu = meanY - (covXY/varX)*meanX + (covXY/varX)*X
    #              tau^2 = varY*(1 - rho^2)
    mu   <- meanY - (covXY/varX)*meanX + (covXY/varX)*X
    tau  <- sqrt(varY*(1 - rho^2))
    Y    <- rnorm(1, mean=mu, sd=tau)

    move.matrix[count,] <- c(X,Y)
    count <- count + 1

    # store X and Y
    if (i > burnin){
      storage.matrix[i-burnin,1] <- X
      storage.matrix[i-burnin,2] <- Y
    }
        
  }

  cat("\n\n\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ RESULTS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
  cat("true mean of X =      ", meanX, "\n")
  cat("estimated mean of X = ", mean(storage.matrix[,1]), "\n\n")
  cat("true (0.025, 0.975) quantiles of X      =  (",
      qnorm(0.025, meanX, sqrt(varX)), ", ",
      qnorm(0.975, meanX, sqrt(varX)), ")\n")
  cat("estimated (0.025, 0.975) quantiles of X =  (",
      quantile(storage.matrix[,1], 0.025), ", ",
      quantile(storage.matrix[,1], 0.975), ")\n\n\n")
  
  cat("true mean of Y =      ", meanY, "\n")  
  cat("estimated mean of Y = ", mean(storage.matrix[,2]), "\n\n")
  cat("true (0.025, 0.975) quantiles of Y      =  (",
      qnorm(0.025, meanY, sqrt(varY)), ", ",
      qnorm(0.975, meanY, sqrt(varY)), ")\n")
  cat("estimated (0.025, 0.975) quantiles of Y =  (",
      quantile(storage.matrix[,2], 0.025), ", ",
      quantile(storage.matrix[,2], 0.975), ")\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
  par(mfrow=c(3,2))
  plot(move.matrix, type="n", xlab="X", ylab="Y", main="Move History")
  lines(move.matrix)
  plot(storage.matrix, xlab="X", ylab="Y")
  hist(storage.matrix[,1], nclass=30, xlab="X", main="Histogram of X")
  hist(storage.matrix[,2], nclass=30, xlab="Y", main="Histogram of Y")
  plot(1:gibbs, storage.matrix[,1], type="l", xlab="iteration", ylab="X")
  plot(1:gibbs, storage.matrix[,2], type="l", xlab="iteration", ylab="Y")
  
  return(storage.matrix)

}




