
#Simulate the "Hockey Stick" Function
x <- seq(1,100, by=1)

before <- function(x) ifelse (x<60, 60-x,0)
after <- function(x) ifelse (x<60,0, x-60)

X <- cbind(before(x), after(x))

y <- 1 + 1*before(x) + 1*after(x) + rnorm(100)

ols <- lm(y ~ x)

#Figure 3.1
plot(x,y, bty="l", xlab="X", ylab="Y", bty='l')
abline(ols)

mod.1 <- lm(y ~ x)
mod.2 <- lm(y ~ before(x) + after(x))

y.plot <-mod.2$coef[1]+mod.2$coef[2]*before(x)+mod.2$coef[3]*after(x)

#Figure 3.2
plot(x, y, type = "p", xlab="X", ylab="Y", bty='l')
abline(v=60, lwd=1)
lines(x,y.plot, lty=1, lwd=1)

#Now A Handrolled Cubic Spline
#Read In The Data
library(foreign)

#Set the Working Directory
setwd()
jacob <- read.dta("jacob.dta")
attach(jacob)

#Rescale Ind. Variable
perot <-  perotvote- min(perotvote)
perot <- perot/max(perot)

#Write a Function R(x,z) to For the Cubic Spline Basis
rk <- function(x,z){
  ((z-0.5)^2-1/12)*((x-0.5)^2-1/12)/4-
  ((abs(x-z)-0.5)^4-(abs(x-z)-0.5)^2/2 + 7/240)/24
  }
  
#Model Matrix For Spline Regression
spl.X <- function(x, xk){
   # set up model matrix for cubic penalized regression spline
   q <- length(xk)+2 # number of parameters
   n <- length(x) #amount of data
   X <- matrix(1,n,q) #initialized model matrix with constant in first column
   X[,2] <- x #set second column to x
   X[,3:q] <- outer(x,xk,FUN=rk) # add in remaining to R(x,xk)
   X
   }

#Now Choose Number of Knots - Here 4 Knots
xk <- 1:4/5
#Generate Model Matrix
X <- spl.X(perot,xk)

#Fit Regression Model n- Be sure to Remove the Constant
mod.1 <- lm(chal.vote~X-1)
#X values for prediction
xp <- 0:100/100
#Prediction Matrix
Xp <- spl.X(xp,xk)

#Figure 3.3
plot(perot, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot", bty="l")
points(perot, chal.vote, pch=".", cex=1.75)
lines(xp,Xp%*%coef(mod.1))

#Vary Knots - 3 Knots 
xk.2 <- 1:3/4
X.2 <- spl.X(perot,xk.2)
#8 Knots
xk.3 <- 1:8/9
X.3 <- spl.X(perot,xk.3)

mod.2 <- lm(chal.vote~X.2-1)
mod.3 <- lm(chal.vote~X.3-1)

#Prediction Matrices
Xp.2 <- spl.X(xp,xk.2)
Xp.3 <- spl.X(xp,xk.3)

#Plot (Not in Book)
plot(perot, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot", bty="l")
points(perot, chal.vote, pch=".", cex=1.75)
lines(xp,Xp.2%*%coef(mod.2))
lines(xp,Xp.3%*%coef(mod.3), lty=2)

legend(0.0,50, c("3 Knots","8 Knots"), cex=c(.8,.8), lty=c(1,2), bty="n")



