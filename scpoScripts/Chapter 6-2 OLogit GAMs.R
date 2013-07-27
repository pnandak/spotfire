
#Set the Working Directory
setwd()

library(foreign)
couples <- read.dta("couples.dta")
attach(couples)

library(VGAM)

#Without Squared Term
glm.1 <- vglm(violence ~ chabting + minority +  fagunion + misolatn + ecndisad + alcdrug + duryrs + econ.alc + disagmnt + comstyle, cumulative (parallel=T), data=couples)

summary(glm.1)

#With Squared Term
glm.2 <- vglm(violence ~ chabting + minority +  fagunion + misolatn + ecndisad + alcdrug + duryrs + durat.sqd + econ.alc + disagmnt + comstyle, cumulative (parallel=T), data=couples)

summary(glm.2)

#Full GAM
gam.1 <- vgam(violence ~ chabting + minority +  s(fagunion) + s(misolatn) + s(ecndisad) + alcdrug + s(duryrs) + s(econ.alc) + s(disagmnt) + s(comstyle), cumulative (parallel=T), data=couples)

summary(gam.1)

#Reduced GAM
gam.2 <- vgam(as.numeric(violence) ~ chabting + minority + fagunion + misolatn + ecndisad + alcdrug + s(duryrs) + econ.alc + disagmnt + comstyle, cumulative (parallel=T), data=couples)

summary(gam.2)

#Figure 6.3
plot(gam.2, which.term=7, se=TRUE, rug=FALSE, ylab="Propensity For Violence (Linear Predictor)", xlab="Relationship Duration (Mean Centered)", bty="l")

1-pchisq(deviance(glm.1) - deviance(gam.2), 3.0)

1-pchisq(deviance(glm.2) - deviance(gam.2), 3.0)

#Extracting Model Results from vgam function
coef(gam.2)
sqrt(diag(vcov(gam.2)))
t.stat <- coef(gam.2)/sqrt(diag(vcov(gam.2)))
t.stat

#Plot On Probability Scale
logit <- function(xb){
    1/(1+exp(-xb))
    }
    
dur.sim <- seq(1, 63, by = 1)

p.data <- data.frame(expand.grid(list(chabting = 0, minority=0, fagunion =0, misolatn=-.19, ecndisad=0, alcdrug=1, duryrs=dur.sim,  econ.alc=0, disagmnt=2, comstyle=2)))

predict.fit <- predict.vglm(gam.2, newdata=p.data, se=TRUE)

prY0 <- logit(coef(gam.2)[1] - predict.fit$fitted.values[,1])
se <- predict.fit$se.fit[,1]

#Figure 6.4
plot(dur.sim, prY0, type= "n", ylim=c(0,1), xlab="Relationship Duration", ylab = "Probability of Physical Violence")
lines(dur.sim, prY0)
lines(dur.sim, prY0-se, lty=2)
lines(dur.sim, prY0+se, lty=2)

#Figure 6.5
plot(density(duryrs+20),  main="", xlab="Relationship Duration")



