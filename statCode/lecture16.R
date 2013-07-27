library(RWinEdt)
library(MASS)
library(epitools)
library(lattice)
options(width=75)

orobanche <- read.table("orobanche.txt", header=T)

attach(orobanche)

expcond <- Species:Extract

orobanche.glm <- glm(cbind(Germinate,n-Germinate) ~ Species + Extract + Species:Extract,
   data=orobanche, family=binomial())

fits<-unique(fitted(orobanche.glm))
   
summary(orobanche.glm)

pchisq(deviance(orobanche.glm), df.residual(orobanche.glm), lower.tail=F)
sum(resid(orobanche.glm,type="pearson")^2)
pchisq(sum(resid(orobanche.glm,type="pearson")^2), df.residual(orobanche.glm), lower.tail=F)

postscript("../orobanche.eps", width=5, height=4, horiz=F)
par(mar=c(4,4,1,2) + 0.1, pty="m")
par(mfrow=c(1,1))

plot(Germinate/n ~ as.numeric(expcond), data=orobanche, 
  xlab="Experimental Condition", ylab="Proportion Germinated",xaxt="n")
axis(1,c(1:4),c("O73/Bean", "O73/Cucumber", "O75/Bean", "O75/Cucumber"))
points(c(3,4,1,2), fits, pch=16, col=2)
dev.off()

postscript("../orobanche.resid.eps", width=6, height=4, horiz=F)
par(mar=c(4,4,1,2) + 0.1, pty="m")
par(mfrow=c(1,1))

plot(resid(orobanche.glm) ~ as.numeric(expcond),  
  xlab="Experimental Condition", ylab="Deviance Residual",xaxt="n")
axis(1,1:4,c("O73/Bean", "O73/Cucumber", "O75/Bean", "O75/Cucumber"))
dev.off()

orobanche.qglm <- glm(cbind(Germinate,n-Germinate) ~ Species + Extract + Species:Extract,
   data=orobanche, family=quasibinomial())
   
summary(orobanche.qglm)

anova(orobanche.qglm, test="Chisq")
anova(orobanche.qglm, test="F")

orobanche.null.qglm <- glm(cbind(Germinate,n-Germinate) ~ 1,
   data=orobanche, family=quasibinomial())
anova(orobanche.null.qglm, orobanche.qglm, test="F")

orobanche.main.qglm <- glm(cbind(Germinate,n-Germinate) ~ Species + Extract,
   data=orobanche, family=quasibinomial())
summary(orobanche.main.qglm)

influence.measures(orobanche.main.qglm)

orobanche.main.glm <- glm(cbind(Germinate,n-Germinate) ~ Species + Extract,
   data=orobanche, family=binomial())
summary(orobanche.main.glm)



wave.glm <- glm(Damage ~ Type + Construct + Operation, offset=log(Service),
  data=wave2, family=poisson())
summary(wave.glm)

pchisq(deviance(wave.glm), df.residual(wave.glm), lower.tail=F)

wave2.glm <- glm(Damage ~ Type + Construct + Operation + Type:Construct, offset=log(Service),
  data=wave2, family=poisson())
summary(wave2.glm)
anova(wave2.glm,test="Chisq")


attach(wave2)

postscript("../wave2.resid.eps", width=9, height=6, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(2,2))

plot(fitted(wave2.glm),resid(wave2.glm), xlab="Fitted Damage Incidents",
  ylab="Deviance Residual")
plot(Type, resid(wave2.glm), xlab="Ship Type", 
  ylab="Deviance Residual")
plot(Construct, resid(wave2.glm), xlab="Construction Year", 
  ylab="Deviance Residual")
plot(Operation, resid(wave2.glm), xlab="Operation Period", 
  ylab="Deviance Residual")

dev.off()



wave.qglm <- glm(Damage ~ Type + Construct + Operation, offset=log(Service),
  data=wave2, family=quasipoisson())
summary(wave.qglm)

wave2.qglm <- glm(Damage ~ Type + Construct + Operation + Type:Construct, offset=log(Service),
  data=wave2, family=quasipoisson())
summary(wave2.qglm)
anova(wave2.qglm, test="F")


wave.t.qglm <- glm(Damage ~ Construct + Operation, offset=log(Service),
  data=wave2, family=quasipoisson())
wave.c.qglm <- glm(Damage ~ Type + Operation, offset=log(Service),
  data=wave2, family=quasipoisson())
wave.o.qglm <- glm(Damage ~ Type + Construct, offset=log(Service),
  data=wave2, family=quasipoisson())
  
anova(wave.t.qglm, wave.qglm, test="F")
anova(wave.c.qglm, wave.qglm, test="F")
anova(wave.o.qglm, wave.qglm, test="F")
