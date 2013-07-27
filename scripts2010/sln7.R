gasdata <- read.csv("UKGasConsumption.csv")
plot(gasdata$gas, xlab="year", ylab="gas consumption",
     type="l", col=4, xaxt="n", lwd=2)
axis(1, at=(0:26)*12, labels=1960:1986)
# looks like we need a log transform!
# We'll also need periodic and linear trends.

QR <- 5:108
cos4 <- cos(QR*pi/2)
sin4 <- sin(QR*pi/2)
quarter <- as.factor(gasdata$quarter[QR])

loggas <- log(gasdata$gas[QR])
loggaslast <- log(gasdata$gas[QR-4])
logpop<- log(gasdata$pop[QR])
loggdp <- log(gasdata$gdp[QR])

newdata <- data.frame(sin4=sin4, cos4=cos4, QR=QR, loggdp=loggdp, logpop=logpop, loggaslast=loggaslast, quarter=quarter)

# First, lets try a no-covariate models
reg1 <- lm(loggas ~ QR +  sin4 + cos4 + loggaslast)
summary(reg1)
# Now, consider adding the other covariates...
reg2 <- lm(loggas ~ QR +  sin4 + cos4 + loggaslast + logpop + loggdp)
summary(reg2)
# doesn't look like we need QR; keep the covars for now due to multicolinearity
reg3 <- lm(loggas ~ sin4 + cos4 + loggaslast + logpop + loggdp)
summary(reg3)
# logGDP can go
reg4 <- lm(loggas ~ sin4 + cos4 + logpop + loggaslast)
summary(reg4)
# What about quarterly effecst rather than sin and cos?
reg5 <- lm(loggas ~ logpop + loggaslast + quarter)
summary(reg5)

# Model probabilities
n <- length(QR)
BIC <- cbind(extractAIC(reg1, k=log(n)),
             extractAIC(reg2, k=log(n)),
             extractAIC(reg3, k=log(n)),
             extractAIC(reg4, k=log(n)),
             extractAIC(reg5, k=log(n)))
# Model probabilities
print(eBIC <- exp(-.5*(BIC[2,]-min(BIC[2,]))))
round(probs <- eBIC/sum(eBIC), 2)
# We're pretty sure that reg4 is best,
# but the noncovar model is also possible!
# Perhaps there is a better covariate that we're missing...
# Anyways, here is the model, predictions, and residuals
par(mfrow=c(1,3))
plot(loggas, xlab="year", ylab="log gas consumption", type="l", col=4, lty=2, xaxt="n", lwd=2)
axis(1, at=(0:26)*4, labels=1960:1986)
pred <- predict(reg4,  newdata)
lines(pred, col=2, lwd=2)
legend("topleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))
plot(reg4$resid, xlab="year", ylab="residual", type="l", col=4, main="residuals in time",  xaxt="n", lwd=2)
axis(1, at=(0:26)*4, labels=1960:1986)
acf(reg4$resid, lwd=2)
# The model interpretation: There is mean reversion on an annual basis
# and an annual periodic oscillation.  Gas consumption increases about
# 3% for a 1% increase in population
#
# You can also see the gas price shock!
# Bonus: Since the shock behavior is effectively an outlier,
# we could remove these points by adding a dummy variable for each
# spring/summer (driving season) quarter of 1970 & 1971.  This is
# beyond what I was looking for, but it cleans up the problem:
shock1 <- QR==43
shock2 <- QR==44
shock3 <- QR==47
shock4 <- QR==48
newdata$shock1 <- shock1
newdata$shock2 <- shock2
newdata$shock3 <- shock3
newdata$shock4 <- shock4

reg6 <- lm(loggas ~ logpop + loggaslast + sin4 + cos4 + shock1 + shock2 + shock3 + shock4)
summary(reg6)
par(mfrow=c(1,3))
plot(loggas, xlab="year", ylab="log gas consumption", type="l", col=4, lty=2, xaxt="n", lwd=2)
axis(1, at=(0:26)*4, labels=1960:1986)
pred <- predict(reg6,  newdata)
lines(pred, col=2, lwd=2)
legend("topleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))
plot(reg6$resid, xlab="year", ylab="residual", type="l", col=4, main="residuals in time",  xaxt="n", lwd=2)
axis(1, at=(0:26)*4, labels=1960:1986)
acf(reg6$resid, lwd=2)
