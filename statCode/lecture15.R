library(RWinEdt)
library(MASS)
library(epitools)
library(lattice)
options(width=75)

puffin <- read.table('puffin.txt',header=T)

puffin.glm <- glm(nesting ~ grass + soil + angle + distance, data=puffin,
  family=poisson())

  
summary(puffin.glm)
anova(puffin.glm)

betahat <- coef(puffin.glm)[5]
se <- sqrt(vcov(puffin.glm)[5,5])
cibeta <- c(betahat-qnorm(0.975)*se, betahat+qnorm(0.975)*se)
betahat
cibeta
exp(betahat)
exp(cibeta)

dev.resid <- resid(puffin.glm)
pear.resid <- resid(puffin.glm, type="pearson")

puffin2.glm <- glm(nesting ~ angle + distance, data=puffin,
  family=poisson())
  
anova(puffin2.glm, puffin.glm, test="Chisq")

puffin3.glm <- glm(nesting ~ soil + angle + distance, data=puffin,
  family=poisson())
  
anova(puffin3.glm, puffin.glm, test="Chisq")
summary(puffin.glm)

attach(puffin)

postscript("../puffin.resid1.eps", width=9, height=6, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(2,3))

plot(fitted(puffin.glm), dev.resid, xlab="Fitted Nests", 
  ylab="Deviance Residuals")
plot(grass, dev.resid, xlab="Grass Cover", 
  ylab="Deviance Residuals")
plot(soil, dev.resid, xlab="Soil", 
  ylab="Deviance Residuals")

plot(fitted(puffin.glm), pear.resid, xlab="Fitted Nests", 
  ylab="Pearson Residuals")
plot(grass, pear.resid, xlab="Grass Cover", 
  ylab="Pearson Residuals")
plot(soil, pear.resid, xlab="Soil", 
  ylab="Pearson Residuals")

dev.off()

postscript("../puffin.resid2.eps", width=9, height=6, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(2,2))

plot(angle, dev.resid, xlab="Angle of Slope", 
  ylab="Deviance Residuals")
plot(distance, dev.resid, xlab="Distance from Cliff Edge", 
  ylab="Deviance Residuals")

plot(angle, pear.resid, xlab="Angle of Slope", 
  ylab="Pearson Residuals")
plot(distance, pear.resid, xlab="Distance from Cliff Edge", 
  ylab="Pearson Residuals")

dev.off()

summary(puffin.glm)

pchisq(deviance(puffin.glm), df.residual(puffin.glm), lower.tail=F)

pchisq(sum(pear.resid^2), df.residual(puffin.glm), lower.tail=F)


cbind(puffin,pear.resid, dev.resid, fitted(puffin.glm))[24,]
cbind(puffin,pear.resid, dev.resid, fitted(puffin.glm))[5,]

puffin.inf <- influence.measures(puffin.glm)

puffin.dffits <- dffits(puffin.glm)
puffin.dfbetas <- dfbetas(puffin.glm)
dfbetamax <- apply(abs(puffin.dfbetas),1,max)
puffin.cooksd <- cooks.distance(puffin.glm)
puffin.hat <- hatvalues(puffin.glm)

puffin.hat[puffin.hat > 3*5/38]
puffin.dffits[puffin.dffits > 3*sqrt(5/33)]
puffin.cooksd[puffin.cooksd > qf(0.5,5,33)]
puffin.dfbetas[dfbetamax > 1,]
puffin.dfbetas[dfbetamax = max(dfbetamax),]


wave <- read.table('wavedamage.txt',header=T)
wave2 <- wave[wave$Service > 0,]

wave.glm <- glm(Damage ~ Type + Construct + Operation, offset=log(Service),
  data=wave2, family=poisson())

summary(wave.glm)

wave.t.glm <- glm(Damage ~ Construct + Operation, offset=log(Service),
  data=wave2, family=poisson())
wave.c.glm <- glm(Damage ~ Type + Operation, offset=log(Service),
  data=wave2, family=poisson())
wave.o.glm <- glm(Damage ~ Type + Construct, offset=log(Service),
  data=wave2, family=poisson())
  
anova(wave.t.glm, wave.glm, test="Chisq")
anova(wave.c.glm, wave.glm, test="Chisq")
anova(wave.o.glm, wave.glm, test="Chisq")


postscript("../wave.eps", width=9, height=3.75, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(1,3))

boxplot(1000*Damage/Service ~ Type, data=wave2, xlab="Ship Type", 
  ylab="Damage Rates (per 1000 months)")
boxplot(1000*Damage/Service ~ Construct, data=wave2, xlab="Construction Year", 
  ylab="Damage Rates (per 1000 months)")
boxplot(1000*Damage/Service ~ Operation, data=wave2, xlab="Operation Period", 
  ylab="Damage Rates (per 1000 months)")

dev.off()

attach(wave2)

postscript("../wave.resid.eps", width=9, height=6, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(2,2))

plot(fitted(wave.glm),resid(wave.glm), xlab="Fitted Damage Incidents",
  ylab="Deviance Residual")
plot(Type, resid(wave.glm), xlab="Ship Type", 
  ylab="Deviance Residual")
plot(Construct, resid(wave.glm), xlab="Construction Year", 
  ylab="Deviance Residual")
plot(Operation, resid(wave.glm), xlab="Operation Period", 
  ylab="Deviance Residual")

dev.off()

influence.measures(wave.glm)

wave.dffits <- dffits(wave.glm)
wave.dfbetas <- dfbetas(wave.glm)
dfbetamax <- apply(abs(wave.dfbetas),1,max)
wave.cooksd <- cooks.distance(wave.glm)
wave.hat <- hatvalues(wave.glm)

wave.hat[wave.hat > 3*9/34]
wave.dffits[wave.dffits > 3*sqrt(9/25)]
wave.cooksd[wave.cooksd > qf(0.5,9,25)]
wave.dfbetas[dfbetamax > 1,]
wave.dfbetas[dfbetamax = max(dfbetamax),]


wave.qglm <- glm(Damage ~ Type + Construct + Operation, offset=log(Service),
  data=wave2, family=quasipoisson())

summary(wave.qglm)
