library(RWinEdt)
options(width=75)

puffin <- read.table('puffin.txt',header=T)

summary(puffin)

postscript("../puffin.eps", horiz=F, width=8, height=6.5)
plot(puffin)
dev.off()

print(cor(puffin), digits=2)


puffin.lm <- lm(nesting ~ grass + soil + angle + distance, data=puffin)

summary(puffin.lm)

anova(puffin.lm)

postscript("../puffinfitres.eps", horiz=F, width=5, height=4)
plot(fitted(puffin.lm), resid(puffin.lm), xlab="Fitted Values",
  ylab="Residuals")
dev.off()


attach(puffin)
postscript("../puffinres.eps", horiz=F, width=8, height=6.5)
par(mfrow=c(2,2), oma=c(0,0,1.5,0))
plot(grass, resid(puffin.lm), xlab="Grass", ylab="Residuals")
plot(soil, resid(puffin.lm), xlab="Soil", ylab="Residuals")
plot(angle, resid(puffin.lm), xlab="Angle", ylab="Residuals")
plot(distance, resid(puffin.lm), xlab="Distance", ylab="Residuals")
mtext(side=3, line=0, cex=1.5, outer=T, "Residuals vs Predictors")
dev.off()

plot(puffin.lm)


newdata <- data.frame(grass=c(50,95), soil=c(35,25), angle=c(20,5), 
   distance=c(15,60))
predict(puffin.lm,newdata)

puffin.glm <- glm(nesting ~ grass + soil + angle + distance, data=puffin,
  family=poisson())

  
summary(puffin.glm)
anova(puffin.glm)

postscript("../puffinfitglmres.eps", horiz=F, width=5, height=4)
plot(fitted(puffin.glm), resid(puffin.glm), xlab="Fitted Values",
  ylab="Deviance Residuals")
dev.off()

attach(puffin)
postscript("../puffinglmres.eps", horiz=F, width=8, height=6.5)
par(mfrow=c(2,2), oma=c(0,0,1.5,0))
plot(grass, resid(puffin.glm), xlab="Grass", ylab="Deviance Residuals")
plot(soil, resid(puffin.glm), xlab="Soil", ylab="Deviance Residuals")
plot(angle, resid(puffin.glm), xlab="Angle", ylab="Deviance Residuals")
plot(distance, resid(puffin.glm), xlab="Distance", ylab="Deviance Residuals")
mtext(side=3, line=0, cex=1.5, outer=T, "Deviance Residuals vs Predictors")
dev.off()

predict(puffin.glm, newdata)
predict(puffin.glm, newdata, type="response")



puffin.qglm <- glm(nesting ~ grass + soil + angle + distance, data=puffin,
  family=quasipoisson())
  
summary(puffin.qglm)
anova(puffin.qglm)
predict(puffin.qglm, newdata)
predict(puffin.qglm, newdata, type="response")
