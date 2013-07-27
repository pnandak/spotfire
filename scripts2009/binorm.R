# DATA MINING AND INFORMATION SYSTEMS
# MULTINORMAL DISTRIBUTION MARCH, 27TH 2009 
###############################################################################
#BIVARIATE NORMAL DENSITY FUNCTION
binormf <- function(x,y,mx,sx,my,sy,rho) 1/(2*pi*sx*sy*sqrt(1-rho^2))*
            exp(-(((x-mx)/sx)^2+((y-my)/sy)^2-2*rho*(x-mx)/sx*(y-my)/sy)/(2*(1-rho^2)))
f1 <- binormf(1,-1,0,1,0,1,0.5)
f1
#MULTIVARIATE NORMAL DENSITY FUNCTION
multinormf <- function(x,mu,sigma) {
              p <- length(mu)
              d <- det(sigma)
              mah <- (x-mu)%*%solve(sigma)%*%(x-mu)
              f <- exp(-mah/2)/sqrt(((2*pi)^p)*d)
              return(f) 
}
x < c(1,-1)
mu <- c(0,0)
sigma <- matrix(c(1,0.5,0.5,1),2,2)
multinormf(c(1,-1),mu,sigma)
#SIMULATION OF MULTIVARIATE NORMAL SAMPLES
library(MASS)
mu <- c(0.25,0.6,2.5)                                #mean vector
sigma <- matrix(c(0.073,0.034,0.243,                 #covariance matrix
                  0.034,0.063,0.225,
                  0.243,0.225,3.24),3,3)
camp <- mvrnorm(n=50,mu,sigma,empirical=TRUE)
###############################################################################
# CONTOUR PLOT
# SETTING PARAMETERS
mx <- 0;my <- 0 #means
sx <- 1;sy <- 1 #sd
rho <- 0.6 #correlation
cov <- matrix(c(sx^2,rho*sx*sy,rho*sx*sy,sy^2),2,2,byrow=TRUE) #cov matrix
# GRID IN XY PLANE
x <- seq(-3, 3, length= 50)
y <- x
# BIVARIATE NORMAL DENSITY FUNCTION
f <- function(x,y) 1/(2*pi*sqrt(det(cov)))*exp(-(((x-mx)/sx)^2+((y-my)/sy)^2-
 2*rho*((x-mx)/sx)*((y-my)/sy))/(2*(1-rho^2)))
z <- outer(x, y, f)
# CONTOUR PLOT
contour(x, y, z, nlevels=6, col = "black", lty = "solid",
 xlab = "X", ylab = "Y",
 main=paste("Normal Density Cont. (r = ",rho,")"),asp=1)
points(mx, my,type="p",pch="*",col="red",cex=1.5)
abline(a=0,b=1,lty="dashed",col="red")
abline(a=0,b=-1,lty="dashed",col="red")
abline(a=0,b=rho,lty="dashed",col="green")
# PERSPECTIVE PLOT
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
 ltheta = 120, shade = 0.75, ticktype = "detailed",
 xlab = "X", ylab = "Y", zlab = "Density",
 main=paste("Bivariate Normal Density (r = ",rho,")"))
# USEFUL TO COMPARE DIFFERENT SET OF PARAMETERS
layout(matrix(1:4,2,2,byrow=TRUE))
layout(matrix(1,1,1))

