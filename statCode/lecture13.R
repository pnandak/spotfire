library(RWinEdt)
library(MASS)
library(epitools)
options(width=75)

dev.resid <- function(model,y) {

n <- apply(y,1,sum)
fits <- predict(model, type="response")
O <- y[,1]
E <- fits * n
resids <- sign(O-E)* sqrt(2*(O * log(O/E) + (n-O) * log((n-O)/(n-E))))
resids
}

pear.resid <- function(model,y) {

n <- apply(y,1,sum)
fits <- predict(model, type="response")
O <- y[,1]
E <- fits * n
resids <- (O-E) / sqrt(n*fits*(1-fits))
resids
}

# Fastener failure

fasten.load <- seq(2500, 4300, by=200)
fasten.n <- c(50, 70, 100, 60, 40, 85, 90, 50, 80, 65)
fasten.fail <- c(10, 17, 30, 21, 18, 43, 54, 33, 60, 51)
fasten.prop <- fasten.fail/fasten.n
fasten <- data.frame(load=fasten.load, n=fasten.n, 
    fail=fasten.fail, hold=fasten.n-fasten.fail, prop=fasten.prop)
fasten.res <- cbind(fasten.fail,fasten.n-fasten.fail)
fasten.rev <- cbind(fasten.n-fasten.fail,fasten.fail)

fasten.logit.glm <- glm(fasten.res ~ fasten.load, family=binomial())
   
summary(fasten.logit.glm)

fasten.resid <- resid(fasten.logit.glm)
fasten.fit <- predict(fasten.logit.glm, type="response")
fasten.exp <- fasten.fit * fasten.n
fasten.dev <- dev.resid(fasten.logit.glm,fasten.res)
fasten.pear <- pear.resid(fasten.logit.glm,fasten.res)

summary(fasten.resid)


postscript("../fasten.data.eps", width=9, height=5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(fasten.load, fasten.prop, xlab="Load (psi)", ylab=expression(hat(pi)), 
  main="Fasteners - Sample Proportions", ylim=c(0,1), pch=16)
plot(fasten.load, logit(fasten.prop), xlab="Load (psi)", ylab=expression(logit(hat(pi))), 
  main="Fasteners - Empirical Logits",  pch=16)
dev.off()

postscript("../fasten.data2.eps", width=9, height=5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(fasten.load, logit(fasten.prop), xlab="Load (psi)", ylab=expression(logit(hat(pi))), 
  main="Fasteners - Empirical Logits", pch=16)
plot(fasten.load, qnorm(fasten.prop), xlab="Load (psi)", ylab=expression({Phi^-1} (hat(pi))), 
  main="Fasteners - Empirical Normits",  pch=16)
dev.off()

postscript("../fasten.data3.eps", width=9, height=5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(fasten.load, logit(fasten.prop), xlab="Load (psi)", ylab=expression(logit(hat(pi))), 
  main="Fasteners - Empirical Logits", pch=16)
plot(fasten.load, qcauchy(fasten.prop), xlab="Load (psi)", ylab=expression({F^-1} (hat(pi))), 
  main="Fasteners - Empirical Cauchits",  pch=16)
dev.off()


postscript("../fasten.resid.eps", width=9, height=5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(fasten.load, fasten.dev, xlab="Load (psi)", ylab="Deviance Residual", 
  main="Fasteners - Deviance Residuals",  pch=16)
plot(fasten.load, fasten.pear, xlab="Load (psi)", ylab="Pearson Residual",
  main="Fasteners - Pearson Residuals",  pch=16)
dev.off()


# Pop bottle deposits

deposit.glm <- glm(returned ~ deposit, family=binomial())

returned.prop <- returned[,1]/apply(returned,1,sum)

deposit.fit <- predict(deposit.glm, type="response")
deposit.dev <- resid(deposit.glm)
deposit.pear <- pear.resid(deposit.glm, returned)

postscript("../deposit.data.eps", width=9, height=5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(deposit, returned.prop, xlab="Deposit", ylab=expression(hat(pi)), 
  main="Returned - Sample Proportions", ylim=c(0,1),  pch=16)
plot(deposit, logit(returned.prop), xlab="Deposit", ylab=expression(logit(hat(pi))), 
  main="Returned - Empirical Logits",  pch=16)
dev.off()

postscript("../deposit.resid.eps", width=9, height=5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(deposit, deposit.dev, xlab="Deposit", ylab="Deviance Residual", 
  main="Returned - Deviance Residuals",  pch=16)
plot(deposit, deposit.pear, xlab="Deposit", ylab="Pearson Residual", 
  main="Returned - Pearson Residuals",  pch=16)
dev.off()

# simulated residual plots

n <- 20
m <- 50
x <- 1:n
xt <- x-n/2
eta <- -2 + x/5 + xt^2/80
psim <- exp(eta)/(1+exp(eta))
ysim <- rbinom(n,rep(m,n),psim)
ymat <- cbind(ysim,m-ysim)
ysim.glm <- glm(ymat ~ x, family=binomial())
ysim.dev <- resid(ysim.glm)
ysim.pear <- pear.resid(ysim.glm,ymat)

postscript("../simdata.data.eps", width=9, height=5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(x, ysim/m, xlab="x", ylab=expression(hat(pi)), 
  main="Sample Proportions", ylim=c(0,1),  pch=16)
plot(x, logit(ysim/m), xlab="x", ylab=expression(logit(hat(pi))), 
  main="Empirical Logits",  pch=16)
dev.off()

postscript("../simdata.resid.eps", width=9, height=5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(x, ysim.dev, xlab="x", ylab="Deviance Residual", 
  main="Deviance Residuals",  pch=16)
plot(x, ysim.pear, xlab="x", ylab="Pearson Residual", 
  main="Pearson Residuals",  pch=16)
dev.off()

ysim2.glm <- glm(ymat ~ x + I(x^2), family=binomial())
summary(ysim2.glm)
ysim2.dev <- resid(ysim2.glm)
ysim2.pear <- pear.resid(ysim2.glm,ymat)

truth.lm <- lm(eta ~ x + I(x^2))
summary(truth.lm)

postscript("../simdata.resid2.eps", width=9, height=5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(x, ysim2.dev, xlab="x", ylab="Deviance Residual", 
  main="Quadratic - Deviance Residuals",  pch=16)
plot(x, ysim2.pear, xlab="x", ylab="Pearson Residual", 
  main="Quadratic - Pearson Residuals",  pch=16)
dev.off()

-2 * sum(dbinom(ysim,m,ysim/m,log=T))
-2 * sum(dbinom(ysim,m,fitted(ysim.glm),log=T))
-2 * sum(dbinom(ysim,m,fitted(glm(ymat ~ 1, family=binomial())),log=T))
-2 * sum(dbinom(ysim,m,sum(ysim)/(n*m),log=T))

summary(fasten.logit.glm)
pchisq(deviance(fasten.logit.glm), df.residual(fasten.logit.glm), lower.tail=F)

summary(deposit.glm)
pchisq(deviance(deposit.glm), df.residual(deposit.glm), lower.tail=F)

summary(ysim.glm)
pchisq(deviance(ysim.glm), df.residual(ysim.glm), lower.tail=F)

summary(ysim2.glm)
pchisq(deviance(ysim2.glm), df.residual(ysim2.glm), lower.tail=F)

anova(ysim.glm, test="Chisq")
anova(ysim2.glm, test="Chisq")

deviance(fasten.logit.glm)
sum(fasten.pear^2)

deviance(deposit.glm)
sum(deposit.pear^2)

deviance(ysim.glm)
sum(ysim.pear^2)

deviance(ysim2.glm)
sum(ysim2.pear^2)
