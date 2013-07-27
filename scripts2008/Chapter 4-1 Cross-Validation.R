
library(foreign)

#Set the Working Directory
setwd()
#Read the data
jacob <- read.dta("jacob.dta")
attach(jacob)

#Using LOCFIT for GCV Smoothing
library(locfit)

#Set a variety of span parameters
alpha <- seq(0.2,0.8, by=0.05)

#Plot the GCV scores
#Figure 4.1
plot(gcvplot(chal.vote~perotvote, data=jacob, alpha=alpha), type="o", bty = "l")

#Fit recommended model
fit <- locfit(chal.vote~perotvote,  data=jacob, alpha=0.2)

#Figure 4.2
plot(fit, ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", ylim=c(10,60))
points(perotvote, chal.vote, pch=".", cex=1.75)

#Now Do Splines
library(pspline)

#Plot and Add Fit
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main = "Cubic B-spline")
points(perotvote, chal.vote, pch=".", cex=1.75)
#GCV Smoothing Selection
lines(sm.spline(perotvote, chal.vote, cv=FALSE))


#Compare Likelihood To GCV
library(mgcv)
library(SemiPar)

sm.1 <- gam(chal.vote ~ s(perotvote, bs="cr"))
smspline.fit <- spm(chal.vote ~ f(perotvote))

#Figure 4.3
par(mfrow=c(1,2))
plot(sm.1, rug=FALSE, se=FALSE, ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", residual=TRUE, shift=33.88, bty="l", main="GCV Smoothing")
#points(perotvote, chal.vote, pch=".", cex=1.75)

plot(smspline.fit, se=FALSE, lwd=1, rug=FALSE, ylab="Challengers' Vote Share (%)", xlab= "Vote for Perot (%)", ylim=c(10,60), main="Likelihood Smoothing")
points(perotvote, chal.vote, pch=".", cex=1.75)

