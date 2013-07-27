# optimExamp1.R provides a brief example of the optim function in R
#
# Kevin Quinn
# Dept. of Political Science and CSSS
# quinn@stat.washington.edu
#

myfun <- function(theta){
  f <- -1 * (theta - 3.4)^2
  return(f)
}

myfund1 <- function(theta){
  d1 <- -2*theta + 2*3.4
  return(d1)
}


theta.start <- -30

optim.out <- optim(theta.start, fn=myfun, gr=myfund1, method="BFGS",
                   control=list(fnscale=-1, trace=1, REPORT=1))

