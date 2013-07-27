
library(SemiPar)
library(splines)

#Comparison Plots
#Simulate A Very Wavy Functional Form
trans<-function(x) {sin(2*pi*x^2)^3}
x<-seq(0,2,by=.01)
y<-trans(x)+.2*rnorm(201)

plot(x,y, pch=1, type="o", lty=1, xlab="X", ylab="Y")

#Plotting polynomial fit - Red Line Represents True Relationship
matplot(x, cbind(y, trans(x)), 
   pch=1, type="pl", lty=1,
   xlab="X", ylab="Y")
   loess <- loess(y ~ x, span=0.1)
   
   #Loess- Must adjust span to almost minimum
xhatloess <- x
yhatloess <- fitted(loess)

#Figure 3.11
#Black Line Represents Estimated Smoothed Fit
par(mfrow = c(2,2))

#Loess
matplot(x, cbind(y, trans(x)), 
    type="pl", lty=2, col=1, pch="",
   xlab="X", ylab="Y", main = "Loess", bty = "l")
lines(xhatloess, yhatloess, lwd=1)

#Cubic B-spline 
matplot(x, cbind(y, trans(x)), 
   pch="", type="pl", lty=2, col=1,
   xlab="X", ylab="Y", main = "Natual Cubic B-Spline", bty = "l")
sp1<-lm(y~ns(x, df=15))
lines(x,sp1$fit, lwd=1)

#Lowess
matplot(x, cbind(y, trans(x)), 
   pch="", type="pl", lty=2, col=1,
   xlab="X", ylab="Y", main="Lowess", bty = "l")
lines(lowess(x,y, f = 0.05), lwd=1)

#Smoothing Spline 
matplot(x, cbind(y, trans(x)), 
   pch="", type="pl", lty=2, col=1,
   xlab="X", ylab="Y", main="Smoothing Spline", bty = "l")
fit <- spm(y ~ f(x))
lines(fit, se=FALSE, lwd=1)