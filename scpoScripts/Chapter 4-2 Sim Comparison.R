
library(splines)
library(mgcv)
library(SemiPar)

#Comparison Plots
set.seed(83713)

trans <- function(x) {cos(4*pi*x^3)^2}
x <- seq(0,1,by=.001)
y <-trans(x)+ .06*rnorm(1001)

#Figure 4.4
par(mfrow = c(1,2))
#Natural Cubic B-spline - Must adjust Degrees of Freedom
matplot(x, cbind(y, trans(x)), 
   pch=".", type="pl", lty=1, col =1,
   xlab="X", ylab="Y", main="Natural Cubic B-spline", bty= "l")
sp1<-lm(y~ns(x, df=12))
lines(x,sp1$fit, lwd=2)

#Smoothing Spline - No Adjustment necessary
matplot(x, cbind(y, trans(x)), 
   pch=".", type="pl", lty=1, col=1,
   xlab="X", ylab="Y", main="Smoothing Splines", bty= "l")
fit <- spm(y ~ f(x))
lines(fit, se=FALSE, lwd=2)

#Plotting polynomial fit - Red Line Represents True Relationship
matplot(x, cbind(y, trans(x)), 
   pch=".", type="pl", lty=1, col=1,
   xlab="X", ylab="Y", main="True Fit", bty= "l")

#Black Line Represents Estimated Smoothed Fit

#Lowess- Must adjust span
matplot(x, cbind(y, trans(x)), 
   pch=".", type="pl", lty=1, col=1,
   xlab="X", ylab="Y", main="Lowess", bty= "l")
lines(lowess(x,y, f = 0.05), lwd=2)



#Calculate MSE
y.hat <- lowess(x,y, f = 0.05)
mse.lowess <- mean((y-y.hat$y)^2)
mse.cubic <- mean((y-sp1$fit)^2)
mse.smooth <- mean((y-fit$fit$fitted)^2)

mse.lowess
mse.cubic
mse.smooth

#Cubic Spline Selection Via AIC
sp1<-lm(y~ns(x, df=10))
AIC(sp1)
sp1<-lm(y~ns(x, df=11))
AIC(sp1)
sp1<-lm(y~ns(x, df=12))
AIC(sp1)
sp1<-lm(y~ns(x, df=13))
AIC(sp1)
sp1<-lm(y~ns(x, df=14))
AIC(sp1)
sp1<-lm(y~ns(x, df=15))
AIC(sp1)
sp1<-lm(y~ns(x, df=16))
AIC(sp1)
sp1<-lm(y~ns(x, df=17))
AIC(sp1)
sp1<-lm(y~ns(x, df=18))
AIC(sp1)
sp1<-lm(y~ns(x, df=19))
AIC(sp1)
sp1<-lm(y~ns(x, df=20))
AIC(sp1)


######################################################3

#A Less Obvious Example

trans<-function(x) {cos(4*exp(x))^4}
x<-seq(0,1,by=.001)
y<- trans(x) + rnorm(1001)

#Settling on this plot.  Point is lots of variation no other 
#criteria to evaluate them other than an eyeball test.

library(SemiPar)
#Figure 4.5
matplot(x, cbind(y, trans(x)), 
   pch=".", type="pl", lty=1, lwd=1, col =1,
   xlab="X", ylab="Y", main="", bty = "l")

#Compare Lowess and Auto Fits
#Figure 4.6
par(mfrow = c(2,1))  
plot(x,y, pch=".", main = "Lowess - 6 Different Spans", cex=0.85, bty="l")
lines(lowess(x,y, f = 0.1), lwd=1, lty=6)
lines(lowess(x,y, f = 0.2), lwd=1, lty=2)
lines(lowess(x,y, f = 0.3), lwd=1, lty=3)
lines(lowess(x,y, f = 0.4), lwd=1, lty=4)
lines(lowess(x,y, f = 0.5), lwd=1, lty=5)
lines(lowess(x,y, f = 0.6), lwd=1, lty=1)
fit <- spm(y ~ f(x))

plot(x,y, pch=".", main = "Spline - Automatic Smoothing", cex=0.85, bty="l")
lines(fit, se=FALSE, lwd=1)

#Figure 4.7
plot(x,y, pch="", bty='l')   
fit <- spm(y ~ f(x))
lines(fit, se=FALSE, lwd=1)
lines(lowess(x,y, f = 0.1), lty=2, lwd=2)
points(x, y, pch=".", cex=1.25)
legend(0.1, 4, c("Smoothing Spline", "Lowess, Span = 0.1"), cex=.85, lty=c(1,2), bty="n", y.intersp = 1.2)

