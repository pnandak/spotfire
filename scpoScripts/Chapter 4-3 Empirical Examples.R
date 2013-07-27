
library(foreign)
library(MASS)

#Set the Working Directory
setwd()
#Read the data
cong <- read.dta("jacob.dta")
attach(cong)

library(mgcv)

#Estimate Four Different Fits
gam.1 <- gam(chal.vote ~ s(perotvote), data=cong)
gam.2 <- gam(chal.vote ~ s(perotvote, k=3, fx=TRUE), data=cong)
gam.3 <- gam(chal.vote ~ s(perotvote, k=4, fx=TRUE), data=cong)
gam.4 <- gam(chal.vote ~ s(perotvote, k=5, fx=TRUE), data=cong)

#Figure 4.8

par(mfrow=c(2,2))
plot(gam.1, rug=FALSE, se=FALSE, ylab="Challengers' Vote Share (%)", xlab="Number of Overdrafts", residual=TRUE, shift=33.88, bty="l", main="Automatic Selection")

plot(gam.2, rug=FALSE, se=FALSE, ylab="Challengers' Vote Share (%)", xlab="Number of Overdrafts", residual=TRUE, shift=33.88, bty="l", main="DF = 2")

plot(gam.3, rug=FALSE, se=FALSE, ylab="Challengers' Vote Share (%)", xlab="Number of Overdrafts", residual=TRUE, shift=33.88, bty="l", main="DF = 3")

plot(gam.4, rug=FALSE, se=FALSE, ylab="Challengers' Vote Share (%)", xlab="Number of Overdrafts", residual=TRUE, shift=33.88, bty="l", main="DF = 4")


#Beck, Katz, and Tucker Example - Warning Very Slow to Estimate
war <- read.dta("PHdata.dta")

#Cubic Splines - Warning These Models Takes A Long Time To Estimate
war.cubic <- glm(dispute ~ nudem + nugrow + allies + contig + nucapab + trade + ns(year, df=3), data = war, family=binomial)
                   
war.smooth <- gam(dispute ~ nudem + nugrow + allies + contig + nucapab + trade + s(year, bs="cr"), data = war, family=binomial) 

#Test Statistic
-2*(logLik(war.cubic) - logLik(war.smooth))
#P-value
1 - pchisq(deviance(war.cubic) - deviance(war.smooth), 4.29)


library(foreign)

#Read the data
p.beach <- read.dta("palmbeach.dta")
p.beach <- na.omit(p.beach)
attach(p.beach)

library(pspline)

plot(overvotes, algore, pch = ".", xlab="Number of Overvotes", ylab="Votes for Al Gore", bty="l")
lines(sm.spline(overvotes, algore, df=4), lty=1)
lines(sm.spline(overvotes, algore, df=5), lty=1)
lines(sm.spline(overvotes, algore, df=6), lty=2)
lines(sm.spline(overvotes, algore, cv=TRUE), lty=3)

#Note cv=FALSE implies the use of GCV
#Figure 4.9
par(mfrow = c(3,1)) 
plot(overvotes, algore, type="n", xlab="Number of Overvotes", ylab="Votes for Al Gore", bty="l", main = "DF = 5")
points(overvotes, algore, pch=".", cex=1.25)
lines(sm.spline(overvotes, algore, df=5), lty=1)

plot(overvotes, algore, type="n", xlab="Number of Overvotes", ylab="Votes for Al Gore", bty="l", main = "DF = 3")
points(overvotes, algore, pch=".", cex=1.25)
lines(sm.spline(overvotes, algore, df=3), lty=1)

plot(overvotes, algore, type="n", xlab="Number of Overvotes", ylab="Votes for Al Gore", bty="l", main = "Automated")
points(overvotes, algore, pch=".", cex=1.25)
lines(sm.spline(overvotes, algore, cv=TRUE), lty=1)
dev.off()


#Compare to Log
gam.1 <- gam(algore ~ log(overvotes+1), data=p.beach)
gam.2 <- gam(algore ~ s(overvotes, bs="cr"), data=p.beach)

anova(gam.1, gam.2, test='Chisq')

#Deforestation Example
forest <- read.dta("forest.dta")

ols <- lm(deforest ~ dem + openc + wardum + rgdpl + rgdplsq + popdense, data=forest)

plot(ols)

library(pspline)

#Figure 4.10
par(mfrow=c(3,1))
plot(ols$fitted, stdres(ols), type="n", xlab="Fitted Values", ylab="Standardized Residuals", bty="l", main = "DF = 3")
points(ols$fitted, stdres(ols), pch="o", cex=0.75) 
abline(h=0, lty=2)
lines(sm.spline(ols$fitted, stdres(ols), df=3), lty=1)

plot(ols$fitted, stdres(ols), type="n", xlab="Fitted Values", ylab="Standardized Residuals", bty="l", main = "DF = 4")
points(ols$fitted, stdres(ols), pch="o", cex=0.75) 
abline(h=0, lty=2)
lines(sm.spline(ols$fitted, stdres(ols), df=4), lty=1)

plot(ols$fitted, stdres(ols), type="n", xlab="Fitted Values", ylab="Standardized Residuals", bty="l",, main = "Automated")
points(ols$fitted, stdres(ols), pch="o", cex=0.75) 
abline(h=0, lty=2)
lines(sm.spline(ols$fitted, stdres(ols), cv=FALSE), lty=1)


