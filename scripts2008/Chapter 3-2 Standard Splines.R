
#Read in the Data
library(foreign)

#Set the Working Directory
setwd()
#Read the data
jacob <- read.dta("jacob.dta")
attach(jacob)

library(splines)

#Perform Spline Regression
mod.bspline <- lm(chal.vote~bs(perotvote, df=4, intercept=TRUE), data=jacob)
mod.nspline <- lm(chal.vote~ns(perotvote, df=4, intercept=TRUE), data=jacob)

perot <- seq(min(perotvote), max(perotvote), length=312)

sfit.1 <- predict(mod.bspline, data.frame(perotvote=perot))
sfit.2 <- predict(mod.nspline, data.frame(perotvote=perot))

#Figure 3.4
par(mfrow = c(1,2))
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main = "Cubic B-spline")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(perot, sfit.1, lwd=1, col=1)

plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l",  main="Natural Spline")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(perot, sfit.2, lwd=1, col=1)

#Knot Selection For Natural Spline via AIC
mod.nspline.2 <- lm(chal.vote~ns(perotvote, df=2, intercept=TRUE), data=jacob)

mod.nspline.3 <- lm(chal.vote~ns(perotvote, df=3, intercept=TRUE), data=jacob)

mod.nspline.4 <- lm(chal.vote~ns(perotvote, df=4, intercept=TRUE), data=jacob)

mod.nspline.5 <- lm(chal.vote~ns(perotvote, df=5, intercept=TRUE), data=jacob)

mod.nspline.6 <- lm(chal.vote~ns(perotvote, df=6, intercept=TRUE), data=jacob)

mod.nspline.7 <- lm(chal.vote~ns(perotvote, df=7, intercept=TRUE), data=jacob)

mod.nspline.8 <- lm(chal.vote~ns(perotvote, df=8, intercept=TRUE), data=jacob)

mod.nspline.9 <- lm(chal.vote~ns(perotvote, df=9, intercept=TRUE), data=jacob)

#Check AIC for Each Model
AIC(mod.nspline.2)
AIC(mod.nspline.3)
AIC(mod.nspline.4)
AIC(mod.nspline.5)
AIC(mod.nspline.6)
AIC(mod.nspline.7)
AIC(mod.nspline.8)
AIC(mod.nspline.9)

#Plot Four Fits with Lowest AIC
sfit.4 <- predict(mod.nspline.4, data.frame(perotvote=perot))
sfit.5 <- predict(mod.nspline.5, data.frame(perotvote=perot))
sfit.6 <- predict(mod.nspline.6, data.frame(perotvote=perot))
sfit.9 <- predict(mod.nspline.9, data.frame(perotvote=perot))

#Figure 3.5 
par(mfrow = c(2,2))
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main = "4 Knots")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(perot, sfit.4, lwd=1, col=1)

plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main = "5 Knots")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(perot, sfit.5, lwd=1, col=1)

plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main = "6 Knots")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(perot, sfit.6, lwd=1, col=1)

plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l", main = "9 Knots")
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(perot, sfit.9, lwd=1, col=1)







