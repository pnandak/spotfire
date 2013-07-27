
library(foreign)

#Set the Working Directory
#Read the data
cong <- read.dta("jacob.dta")
attach(cong)

library(mgcv)

gam.1 <- gam(chal.vote ~ s(perotvote, bs="cr") + s(checks.raw, bs="cr"), data=cong)
summary(gam.1)

#Figure 5.1
par(mfrow = c(1,2))
plot(gam.1, select=1, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", residual=FALSE, bty="l", shift=36.6672)
points(perotvote, chal.vote, pch=".", cex=1.75)

plot(gam.1, select=2, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", xlab="Number of Overdrafts", residual=FALSE, bty="l", shift=36.6672)
points(checks.raw, chal.vote, pch=".", cex=2)

#Manual Smoothing Parameter Selection
gam.2 <- gam(chal.vote ~ s(perotvote, bs="cr") + s(checks.raw, k=4, fx=TRUE, bs="cr"), data=cong)

#Diagnostics
gam.check(gam.1)

#Statistical inference
#OLS Model
ols.1 <- gam(chal.vote ~ perotvote + checks.raw, data=cong)

#Degrees of freedom for GAM
gam.1

#Chi sqaured test
1 - pchisq(deviance(ols.1) - deviance(gam.1), 7.49)

#Automatic Test
anova(ols.1, gam.1, test="Chisq")

#A Second Test
#Test against log transformation
gam.3 <- gam(chal.vote ~ s(perotvote, bs="cr") + logchecks1, data=cong)

#Compare Fits
anova(gam.3, gam.1, test="Chisq")

#Interactions
detach(cong)
cong$nonmarg <- as.numeric(cong$marginal==0)
attach(cong)

gam.4 <- gam(chal.vote ~ s(perotvote, by=marginal) + s(perotvote, by=nonmarg) + s(checks.raw) + marginal, data=cong)
summary(gam.4)

par(mfrow = c(1,2))
plot(gam.4, select=1, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", residual=FALSE, bty="l", shift=36.6672, main="Marginal Districts")
points(perotvote, chal.vote, pch=".", cex=1.75)

plot(gam.4, select=2, rug=FALSE, se=TRUE, ylab="Challengers' Vote Share (%)", xlab="Vote for Perot (%)", residual=FALSE, bty="l", shift=36.6672, main="Nonmarginal Districts")
points(perotvote, chal.vote, pch=".", cex=1.75)

#Interaction Between Smoothed Terms
gam.5 <- gam(chal.vote ~ s(checks.raw, chal.spend, bs="tp"), data=cong)
summary(gam.5)

ols.2 <- gam(chal.vote ~ checks.raw + chal.spend + checks.raw:chal.spend, data=cong)

vis.gam(gam.5, theta=325, se=FALSE, xlab="Number of Overdrafts", ylab="Challenger Spending", color="bw", plot.type="persp", zlim=range(seq(20,60, by=10)), type="response", ticktype="detailed")

anova(ols.2, gam.5, test="Chisq")


