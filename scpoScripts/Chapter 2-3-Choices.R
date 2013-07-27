n     <- 50
niter <- 50   #Estimation is repeated and mean and variance of estimators is calculated
oversmooth  <- matrix(0, nrow=niter, ncol=n)
undersmooth <- matrix(0, nrow=niter, ncol=n)
goodsmooth  <- matrix(0, nrow=niter, ncol=n)
biasoversmooth   <- 0*(1:n)
biasundersmooth  <- 0*(1:n)
biasgoodsmooth   <- 0*(1:n)
sdoversmooth     <- 0*(1:n)
sdundersmooth    <- 0*(1:n)
sdgoodsmooth     <- 0*(1:n)


for (iter in 1:niter) {
  x    <-   seq (1/n, 1, 1/n)
  f    <-   x*cos(4*3.1415927*x)
  e    <-   rnorm(n)
  y    <-   f + .3*e
  fhat <-   lowess(x, y,f=.3)
  goodsmooth[iter,] <- fhat$y
  fhat <-   lowess(x, y,f=.00001)
  undersmooth[iter,] <- fhat$y
  fhat <-   lowess(x, y,f=.6)
  oversmooth[iter,] <- fhat$y                             }


for (i in 1:n)   {
biasoversmooth [i] <- mean(oversmooth [,i])
sdoversmooth   [i] <- var (oversmooth [,i]) ^.5
biasundersmooth[i] <- mean(undersmooth[,i])
sdundersmooth  [i] <- var (undersmooth[,i]) ^.5
biasgoodsmooth [i] <- mean(goodsmooth [,i])
sdgoodsmooth   [i] <- var (goodsmooth [,i]) ^.5            }

#Things to try: legend, markers for span as well, 
postscript("figure2.9.eps", width=5.75, height=7.25, horizontal=FALSE, onefile=FALSE, paper="special")

par(mfrow=c(3,1))
#---------------Undersmoothing----------------

plot (x, f ,pch=" ",cex=.5, ylab = "y", bty="l")
lines(x, f ,                             lty=2,lwd=1)
lines(x, biasundersmooth ,               lty=1,lwd=1)
lines(x, biasundersmooth+2*sdundersmooth,lty=4,lwd=1)
lines(x, biasundersmooth-2*sdundersmooth,lty=4,lwd=1)
text(.15,.8,"Span < 0.01", cex=1.25)
abline(v=c(.4,.41))
legend(0.62, 1.0, c("True Function", "Lowess Fit"), lty=c(2,1), bty="n")


#----------------Good Fit------------------

plot (x, f ,pch=" ",cex=.5, ylab = "y", bty="l")
lines(x, f ,                             lty=2,lwd=1)
lines(x, biasgoodsmooth ,                lty=1,lwd=1)
lines(x, biasgoodsmooth+2*sdgoodsmooth,  lty=4,lwd=1)
lines(x, biasgoodsmooth-2*sdgoodsmooth,  lty=4,lwd=1)
text(.25,.8,"Span = 0.30", cex=1.25)
abline(v=c(.4,.7))


#----------------Oversmoothing------------------

plot (x, f ,pch=" ",cex=.5, ylab = "y", bty="l")
lines(x, f ,                           lty=2,lwd=1)
lines(x, biasoversmooth ,              lty=1,lwd=1)
lines(x, biasoversmooth+2*sdoversmooth,lty=4,lwd=1)
lines(x, biasoversmooth-2*sdoversmooth,lty=4,lwd=1)
text(.35,.8,"Span = 0.60", cex=1.25)
abline(v=c(.2,.8))

#Bookeeping - Remove All Objects
rm(list = ls())

#---------Different Spans-------------------------
#Returning to the Congressional Voting Example

#Set Working Directory Here
setwd()

#Read the data
library(foreign)
jacob <- read.dta("jacob.dta")
attach(jacob)


#####################################################################
#A Variety of Span Choices
#Figure 2.10
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(lowess(perotvote, chal.vote, f = 0.1))
lines(lowess(perotvote, chal.vote, f = 0.2), lty=2)
lines(lowess(perotvote, chal.vote, f = 0.4), lty=3)
lines(lowess(perotvote, chal.vote, f = 0.6), lty=4)
lines(lowess(perotvote, chal.vote, f = 0.8), lty=5)

#More Refined Selection
#Figure 2.11
par(mfrow=c(3,1))
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main="Span: 0.50")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(lowess(perotvote, chal.vote, f = 0.50))

plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main="Span: 0.40")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(lowess(perotvote, chal.vote, f = 0.40))

plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main="Span: 0.30")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(lowess(perotvote, chal.vote, f = 0.30))



#############################################################################
#Adjustment of Polynomial Degree
library(locfit)

#Figure 2.12
par(mfrow = c(3,1))
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main="Local Linear")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(locfit(chal.vote~perotvote, alpha=0.5, deg=1), lwd=1)

plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main="Local Quadratic")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(locfit(chal.vote~perotvote, alpha=0.5, deg=2), lwd=1)

plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main="Local Cubic")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(locfit(chal.vote~perotvote, alpha=0.5, deg=3), lwd=1)
dev.off()

#Overfit Example
#Figure 2.13
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main="Span: 0.20")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(lowess(perotvote, chal.vote, f = 0.20))



