library(RWinEdt)
library(MASS)
library(epitools)
options(width=75)

fasten.load <- seq(2500, 4300, by=200)
fasten.n <- c(50, 70, 100, 60, 40, 85, 90, 50, 80, 65)
fasten.fail <- c(10, 17, 30, 21, 18, 43, 54, 33, 60, 51)
fasten.prop <- fasten.fail/fasten.n
fasten <- data.frame(load=fasten.load, n=fasten.n, 
    fail=fasten.fail, hold=fasten.n-fasten.fail, prop=fasten.prop)
fasten.res <- cbind(fasten.fail,fasten.n-fasten.fail)
fasten.rev <- cbind(fasten.n-fasten.fail,fasten.fail)

fasten.logit.glm <- glm(fasten.res ~ fasten.load, family=binomial())
fasten.probit.glm <- glm(fasten.res ~ fasten.load, 
   family=binomial(link="probit"))
fasten.cloglog.glm <- glm(fasten.res ~ fasten.load, 
   family=binomial(link="cloglog"))
fasten.logit.rev.glm <- glm(fasten.rev ~ fasten.load, family=binomial())
   
summary(fasten.logit.glm)
summary(fasten.probit.glm)
summary(fasten.cloglog.glm)
summary(fasten.logit.rev.glm)

fasten.pred.levels <- data.frame(fasten.load=2500:4300)
fasten.logit.pred <- predict(fasten.logit.glm, fasten.pred.levels, type="response")
fasten.probit.pred <- predict(fasten.probit.glm, fasten.pred.levels, type="response")
fasten.cloglog.pred <- predict(fasten.cloglog.glm, fasten.pred.levels, type="response")

postscript("../fasten.comp.eps", width=8, height=6, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")

plot(fasten.load, fasten.prop, xlab="Load (psi)", 
  ylab="Proportion Failing", ylim=c(0,1), pch=16)
lines(2500:4300, fasten.logit.pred,col=2)
lines(2500:4300, fasten.probit.pred,col=3)
lines(2500:4300, fasten.cloglog.pred,col=4)
legend(2500,1, c("Logit","Probit","Complementary Log-Log"), col=2:4,
  lty=1)
dev.off()

birthwt.logit.glm <- glm(low ~ age, data=birthwt, family=binomial(link="logit"))
birthwt.probit.glm <- glm(low ~ age, data=birthwt, family=binomial(link="probit"))
birthwt.cloglog.glm <- glm(low ~ age, data=birthwt, family=binomial(link="cloglog"))
summary(birthwt.logit.glm)
summary(birthwt.probit.glm)
summary(birthwt.cloglog.glm)

pred.ages <- seq(14,45,by=0.1)
pred.levels <- data.frame(age=pred.ages)
birthwt.logit.pred <- predict(birthwt.logit.glm, pred.levels, 
   type="response")
birthwt.probit.pred <- predict(birthwt.probit.glm, pred.levels, 
   type="response")
birthwt.cloglog.pred <- predict(birthwt.cloglog.glm, pred.levels, 
   type="response")

postscript("../birthwt.comp.eps", width=8, height=6, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")

plot(low ~ age, data=birthwt, xlab="Maternal Age", 
  ylab="Low Birth Weight", ylim=c(-0.05, 1.05))
symbols(colnames(birthtable), rep(1,ages), circles=sqrt(birthtable[2,]*0.05),
  add=T, inches=F, fg=2, bg=2)
symbols(colnames(birthtable), rep(0,ages), circles=sqrt(birthtable[1,]*0.05),
  add=T, inches=F, fg=4, bg=4)
lines(pred.ages, birthwt.logit.pred,col=2)
lines(pred.ages, birthwt.probit.pred,col=3)
lines(pred.ages, birthwt.cloglog.pred,col=4)
legend(45,0.9, c("Logit","Probit","Complementary Log-Log"), col=2:4,
  lty=1, xjust=1)

dev.off()


logitpts <- (-69:69)/10
probpts <- plogis(logitpts)
normpts <- qnorm(probpts)
cauchitpts <- qcauchy(probpts)
cloglogpts <- log(-log(1-probpts))

axisprobs <- c(0.001, 0.01, 0.1, 0.2, 0.5, 0.8, 0.9, 0.99, 0.999)

postscript("../linkcomp.eps", width=10, height=4.75, horiz=F)
par(mar=c(4,4,2,1) + 0.1, pty="m")
plot(logitpts,logitpts,type="l", xlab="Logit Scale", ylab=expression(g(pi)))
lines(logitpts, normpts, col=2)
# lines(logitpts, cauchitpts, col=3)
lines(logitpts, cloglogpts, col=4)
axis(3,qlogis(axisprobs),axisprobs)
# axis(4,qlogis(axisprobs),axisprobs)
legend(-7,7, c("Logit","Probit","Complementary Log-Log"), col=c(1,2,4),
  lty=1)
dev.off()

options(contrasts = c("contr.treatment", "contr.poly"))
birthwtall.glm <- glm(low ~ ., binomial, bwt)
summary(birthwtall.glm)

birthwtnull.glm <- glm(low ~ 1, family=binomial, data=bwt)
birthwt.add.step <- stepAIC(birthwtnull.glm, direction="forward",
  scope=list(upper= ~ age + lwt + race + smoke + ptd + ht + ui + ftv,
             lower= ~ 1))
             
birthwtall.glm <- glm(low ~ ., binomial, bwt)
birthwt.drop.step <- stepAIC(birthwtall.glm, direction="backward",
  scope=list(upper= ~ age + lwt + race + smoke + ptd + ht + ui + ftv,
             lower= ~ 1))

birthwtnull.glm <- glm(low ~ 1, family=binomial, data=bwt)
birthwt.step.step <- stepAIC(birthwtnull.glm, direction="both",
  scope=list(upper= ~ age + lwt + race + smoke + ptd + ht + ui + ftv,
             lower= ~ 1))
             
birthwtall.glm <- glm(low ~ ., binomial, bwt)
birthwt.step2.step <- stepAIC(birthwtall.glm, direction="both",
  scope=list(upper= ~ age + lwt + race + smoke + ptd + ht + ui + ftv,
             lower= ~ 1))

birthwtnull.glm <- glm(low ~ 1, family=binomial, data=bwt)
birthwt.BIC.step <- stepAIC(birthwtnull.glm, direction="both",
  scope=list(upper= ~ age + lwt + race + smoke + ptd + ht + ui + ftv,
             lower= ~ 1), k=log(189))

birthwt.BIC2.step <- stepAIC(birthwtall.glm, direction="both",
  scope=list(upper= ~ age + lwt + race + smoke + ptd + ht + ui + ftv,
             lower= ~ 1), k=log(189))
             
anova(birthwt.add.step)
anova(birthwt.drop.step)
anova(birthwt.step.step)
anova(birthwt.step2.step)
anova(birthwt.BIC.step)
anova(birthwt.BIC2.step)
