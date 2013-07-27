# optimExamp2.R provides a brief example of the optim function in R
#
# Kevin Quinn
# Dept. of Political Science and CSSS
# quinn@stat.washington.edu
#

y <- c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1)

loglike <- function(theta){
  loglike <- dbinom(y, 1, theta, log=TRUE)
  loglike <- sum(loglike)
  return(loglike)
}


theta.start <- .65

optim.out <- optim(theta.start, fn=loglike, method="BFGS",
                   control=list(fnscale=-1, trace=1, REPORT=1))

