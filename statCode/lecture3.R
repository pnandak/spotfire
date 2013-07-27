library(RWinEdt)
library(MASS)
options(width=75)

## Diagnostic example

bodyfat <- read.table('bodyfat.txt',header=T)

attach(bodyfat)

postscript("../bodyfatdata.eps", horiz=F, width=10, height=6.5)
par(mfrow=c(2,3), mar=c(4,4,1,1) + 0.1, pty="m")

plot(Tricep, Bodyfat, type="n")
text(Tricep, Bodyfat, 1:20)
plot(Thigh, Bodyfat, type="n")
text(Thigh, Bodyfat, 1:20)
plot(Midarm, Bodyfat, type="n")
text(Midarm, Bodyfat, 1:20)
plot(Tricep, Thigh, type="n")
text(Tricep, Thigh, 1:20)
plot(Tricep, Midarm, type="n")
text(Tricep, Midarm, 1:20)
plot(Thigh, Midarm, type="n")
text(Thigh, Midarm, 1:20)
dev.off()

print(cor(bodyfat),4)

bodyfat2.lm <- lm(Bodyfat ~ Tricep + Thigh, data=bodyfat)

summary(bodyfat2.lm)

influence.measures(bodyfat2.lm)

postscript("../bodyfatres2.eps", horiz=F, width=10, height=6.5)
par(mfrow=c(2,2), mar=c(4,4,1,1) + 0.1, pty="m")
plot(Tricep, resid(bodyfat2.lm), ylab="Raw Residuals",
  xlab="Tricep", type="n")
text(Tricep, resid(bodyfat2.lm), 1:20)
plot(Thigh, resid(bodyfat2.lm), ylab="Raw Residuals",
  xlab="Tricep", type="n")
text(Thigh, resid(bodyfat2.lm), 1:20)
plot(Tricep, rstudent(bodyfat2.lm), ylab="Externally Studentized Residuals",
  xlab="Tricep", type="n")
text(Tricep, rstudent(bodyfat2.lm), 1:20)
plot(Thigh, rstudent(bodyfat2.lm), ylab="Externally Studentized Residuals",
  xlab="Tricep", type="n")
text(Thigh, rstudent(bodyfat2.lm), 1:20)
dev.off()


dfbeta2 <- dfbetas(bodyfat2.lm)

postscript("../bodyfatdiag2.eps", horiz=F, width=10, height=6.5)
par(mfrow=c(2,3), mar=c(4,4,1,1) + 0.1, pty="m")
plot(1:20, dfbeta2[,1], xlab="Observation No.", ylab="DFBETAS(Intercept)")
abline(h=0, lty=2)
plot(1:20, dfbeta2[,2], xlab="Observation No.", ylab="DFBETAS(Tricep)")
abline(h=0, lty=2)
plot(1:20, dfbeta2[,3], xlab="Observation No.", ylab="DFBETAS(Thigh)")
abline(h=0, lty=2)
plot(1:20, dffits(bodyfat2.lm), xlab="Observation No.", ylab="DFFITS")
abline(h=0, lty=2)
plot(1:20, cooks.distance(bodyfat2.lm), xlab="Observation No.", ylab="Cook's D")
plot(1:20, hatvalues(bodyfat2.lm), xlab="Observation No.", ylab="Leverages")
dev.off()

weight <- rep(1,20)
weight[3] <- 0
bodyfatdroptest.lm <- lm(Bodyfat ~ Tricep + Thigh, data=bodyfat, weight=weight)
summary(bodyfatdroptest.lm)

## Weighted least squares example

learning <- read.table("learning.txt", header=T)
attach(learning)

learning.lm <- lm(Cost ~ Responses, data= learning)

postscript("../learning.eps", horiz=F, width=9, height=3.5)
par(mfrow=c(1,2), mar=c(4,4,1,1) + 0.1, pty="m")

plot(Cost ~ Responses, data=learning)
abline(learning.lm, col=2)
plot(Responses, resid(learning.lm), ylab="Residuals")
dev.off()

var.lm <- lm(abs(resid(learning.lm)) ~ Responses)
summary(var.lm)

learning.w.lm <- lm(Cost ~ Responses, data= learning, weight=1/(Responses^2))

summary(learning.lm)
anova(learning.lm)
summary(learning.w.lm)
anova(learning.w.lm)

postscript("../learningweight.eps", horiz=F, width=7, height=4.5)
par(mfrow=c(1,1), mar=c(4,4,1,1) + 0.1, pty="m")

plot(Cost ~ Responses, data=learning)
abline(learning.lm, col=2)
abline(learning.w.lm, col=4)
legend(min(Responses),max(Cost), c("LS", "Weighted LS"), lty=1, col=c(2,4))
dev.off()

postscript("../learningweightres.eps", horiz=F, width=9, height=4.5)
par(mfrow=c(1,2), mar=c(5,4,4,1) + 0.1, pty="m")

plot(Responses, resid(learning.w.lm), ylab="Residuals")
plot(Responses, resid(learning.w.lm)/Responses, 
  ylab=expression(paste("Residuals *", sqrt("Weight"))))
title("Weighted Regression Residuals", outer=T , line=-2)
dev.off()
