library(foreign)

#Set the Working Directory
setwd()
#Read the data
jacob <- read.dta("jacob.dta")
attach(jacob)

#Inference For Splines
#First For Natural Splines
#Re-estimate Natural Spline Model
library(splines)
mod.nspline <- lm(chal.vote~ns(perotvote, df=4, intercept=TRUE), data=jacob)
perot <- seq(min(perotvote), max(perotvote), length=312)
sfit <- predict(mod.nspline, inteval="confidence", se.fit=TRUE, data.frame(perotvote=perot))

#Figure 3.8
plot(perotvote, chal.vote, type="n", ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", bty="l") 
points(perotvote, chal.vote, pch=".", cex=1.75)
lines(perot, sfit$fit, lwd=1)
lines(perot, sfit$fit + 1.96*sfit$se.fit, lty=2, lwd=1)
lines(perot, sfit$fit - 1.96*sfit$se.fit, lty=2, lwd=1)

#Bias Corrected Confidence Intervals For Smoothing Spline 
library(mgcv)
sm.1 <- gam(chal.vote ~ s(perotvote, bs="cr", k=4, fx=TRUE))

plot(sm.1, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", residual=TRUE, shift=33.88, bty="l")

#Figure 3.9
#Overlay Natural Spline Fit and Confidence Intervals
plot(sm.1, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", residual=FALSE, shift=33.88, bty="l")
lines(perot, sfit$fit + 1.96*sfit$se.fit, lty=3, lwd=1)
lines(perot, sfit$fit - 1.96*sfit$se.fit, lty=3, lwd=1)

legend(5,50, c("Pointwise Bands", "Bias Adjusted Bands"), , lty=c(3, 2), bty="n")

#Test For Significant Effect
#Null Model
ols <- lm(chal.vote ~ 1)

#Use Automated R F-test Function Anova
anova(ols, mod.nspline)
anova(ols, sm.1)

#Test For Significant Nonlinearity
ols <- lm(chal.vote ~ perotvote)
anova(ols, mod.nspline)
anova(ols, sm.1)

#Test Against Log Fit
perot.log <- log(perotvote)
log <- lm(chal.vote ~ perot.log)
#anova(log, mod.nspline)
anova(log, sm.1)

#Test Against Quadratic Fit
perot.sqd <- perotvote^2
quad <- lm(chal.vote ~ perotvote + perot.sqd)
#anova(quad, mod.nspline)
anova(quad, sm.1)

#Derivative Plot
library(SemiPar)

#Figure 3.10
smspline.fit <- spm(chal.vote ~ f(perotvote))
plot(smspline.fit, drv=1, se.lwd=1, lwd=1, shade=FALSE, rug=FALSE, ylab = "Change In Challenger Vote Share", xlab= "Vote for Perot (%)")



