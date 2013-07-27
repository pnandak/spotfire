

library(foreign)
#Set the Working Directory
setwd()
#Read the data
jacob <- read.dta("jacob.dta")
attach(jacob)
library(pspline)

#Vary Penalty Term to Control Smoothing
#Figure 3.6
par(mfrow = c(2,2))
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main = "Degrees of Freedom = 2", cex.main = .95)
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(sm.spline(perotvote, chal.vote, df=2), col=1)

plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main = "Degrees of Freedom = 4", cex.main = .95)
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(sm.spline(perotvote, chal.vote, df=4), col=1)

plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main = "Degrees of Freedom = 8", cex.main = .95)
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(sm.spline(perotvote, chal.vote, df=8), col=1)

plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main = "Degrees of Freedom = 12", cex.main = .95)
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(sm.spline(perotvote, chal.vote, df=12), col=1)

#Vary Knots Hold DF Constant
library(splines)

#Figure 3.7
par(mfrow = c(1,2))
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main = "4 Knots", cex.main = .95)
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(smooth.spline(perotvote, chal.vote, df=5, nknots=4), col=1)


plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main = "16 Knots", cex.main = .95)
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(smooth.spline(perotvote, chal.vote, df=5, nknots=16), col=1)

#Now Compare to Natural Splines and Lowess
#Re-estimate Natural Spline Model

mod.nspline <- lm(chal.vote~ns(perotvote, df=4, intercept=TRUE), data=jacob)
perot <- seq(min(perotvote), max(perotvote), length=312)
sfit <- predict(mod.nspline, data.frame(perotvote=perot))

plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l") 
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(sm.spline(perotvote, chal.vote, df=4))
lines(perot, sfit, lwd=1, lty=2)
lines(lowess(perotvote, chal.vote, f=0.4), lty=3)

legend(5,60, c("Smoothing Spline", "Natural Spline", "Lowess"), lty=c(1,2,3), bty="n")
