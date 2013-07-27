y <- c(1, 1, 1, 0, 1, 0, 1, 1, 0, 0)

loglike1 <- function(p){
  loglike <- dbinom(y, 1, p, log=TRUE)
  loglike <- sum(loglike)
  return(loglike)
}

loglike2 <- function(theta){
  p <- exp(theta)/(1+exp(theta))
  loglike <- dbinom(y, 1, p, log=TRUE)
  loglike <- sum(loglike)
  return(loglike)
}



p.start <- 0.5
optim1.out <- optim(p.start, fn=loglike1, method="BFGS",
                   control=list(fnscale=-1))
p1.max <- optim1.out$par
theta1.max <- log(p1.max/(1-p1.max))




theta.start <- -2
optim2.out <- optim(theta.start, fn=loglike2, method="BFGS",
                    control=list(fnscale=-1))
theta2.max <- optim2.out$par
p2.max <- exp(theta2.max)/(1+exp(theta2.max))

cat("\n\np1.max = ", p1.max, "        theta1.max = ", theta1.max, "\n")
cat("p2.max = ", p2.max, "  theta2.max = ", theta2.max, "\n\n\n")

