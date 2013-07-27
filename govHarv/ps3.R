setwd("C:/Documents and Settings/Iain Osgood/My Documents/Iain's Stuff/Gov 2001/PS3")

# 1
data <- c(-1,0,1)

ll.sty.norm <- function(beta, data){
  loglike <- sum(-.5*(data-beta)^2)
  return(loglike)
}

opt <- optim(par = .3, ll.sty.norm, data = data, control = list(fnscale = -1), method = "BFGS", hessian = TRUE)
opt$par
opt$hessian

a.seq <- seq(-2, 2, by = .001)

plot(a.seq, sapply(a.seq, ll.sty.norm, data = data), type = "l")

taylor.norm <- -1 + -1.5*a.seq^2

plot(a.seq, taylor.norm, type = "l")
points(a.seq, sapply(a.seq, ll.sty.norm, data = data)+.1, type = "l", lwd = 3)


# 2 
data <- c(6,5,3,5,6)

ll.binom <- function(pi, data, n = 10){
  log.like <- sum(data*log(pi) + (n-data)*log(1-pi))
  return(log.like)
}

opt.binom <- optim(par = .2, ll.binom, data = data, n = 10, control = list(fnscale = -1), method = "BFGS", hessian = TRUE)

pdf(file = "22.pdf", width = 4, height = 4, family = "Helvetica", pointsize = 10)
a.seq <- seq(from = .001, to = .999, by = .001) 
taylor.binom <- opt.binom$value + opt.binom$hessian/2*(a.seq-.5)^2
plot(a.seq, taylor.binom, type = "l", lwd = 2, col = "darkorchid3", 
     xlab = "pi", ylab = "likelihood")
points(a.seq, sapply(a.seq, ll.binom, data = data, n = 10), type = "l", lwd = 2, col = "goldenrod2")
legend(x = "topleft", legend = c("log-Like", "Approx."), lty = c(1,1),
       lwd = c(2,2), col = c("goldenrod2", "darkorchid3"), cex = .7)
dev.off()

# to see how the quadratic approximation improves, try repeating the data
# note that I standardized the likelihood ratio (i.e. the log-likelihood difference
# by always limiting the range to 50
data <- rep(c(6,5,3,5,6), 3)

ll.binom <- function(pi, data, n = 10){
  log.like <- sum(data*log(pi) + (n-data)*log(1-pi))
  return(log.like)
}

opt.binom <- optim(par = .2, ll.binom, data = data, n = 10, control = list(fnscale = -1), method = "BFGS", hessian = TRUE)

a.seq <- seq(from = .001, to = .999, by = .001) 
taylor.binom <- opt.binom$value + opt.binom$hessian/2*(a.seq-.5)^2
plot(a.seq, taylor.binom, type = "l", lwd = 2, col = "darkorchid3", 
     xlab = "pi", ylab = "likelihood", ylim = c(max(taylor.binom) - 50,max(taylor.binom)))
points(a.seq, sapply(a.seq, ll.binom, data = data, n = 10), type = "l", lwd = 2, col = "goldenrod2")
legend(x = "topleft", legend = c("log-Like","Approx."), lty = c(1,1),
       lwd = c(2,2), col = c("darkorchid3","goldenrod2"), cex = .7)



# 3
mvn <- function(xy){
  x <- xy[1]
  y <- xy[2]
  z <- exp(-.5*((x-2)^2+(y-1)^2))
  return(z)
}

# install.packages("lattice")
library(lattice)

y <- x <- seq(-5,5, by = .1)
grid <- expand.grid(x,y)
names(grid) <- c("x","y")
grid$z <- apply(grid, 1, mvn)

wireframe(z ~ x + y, data = grid, shade = TRUE,
          # aspect = c(61/87, 0.4),
          light.source = c(10,0,10))

optim(par = c(0,1), fn = mvn, method="BFGS",hessian=T,control=list(fnscale=-1))
optim(par = c(5,5), fn = mvn, method="BFGS",hessian=T,control=list(fnscale=-1))

optim(par = c(5,5), fn = mvn, method="SANN",hessian=T,control=list(fnscale=-1, trace = 1))
optim(par = c(5,5), fn = mvn, method="Nelder-Mead",hessian=T,control=list(fnscale=-1, trace = 1))



l.mvn <- function(xy){
  x <- xy[1]
  y <- xy[2]
  z <- -.5*((x-2)^2+(y-1)^2)
  return(z)
}

x <- seq(-5,5, by = .1)
y <- seq(-5,5, by = .1)
grid <- expand.grid(x,y)
names(grid) <- c("x","y")
grid$z <- apply(grid, 1, l.mvn)

wireframe(z ~ x + y, data = grid, shade = TRUE,
          # aspect = c(61/87, 0.4),
          light.source = c(10,0,10))

optim(par = c(0,1), fn = l.mvn, method="BFGS",hessian=T,control=list(fnscale=-1))
optim(par = c(5,5), fn = l.mvn, method="BFGS",hessian=T,control=list(fnscale=-1))





