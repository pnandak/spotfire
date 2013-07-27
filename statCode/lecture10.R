library(RWinEdt)
library(MASS)
library(epitools)
options(width=75)




birthwt.glm <- glm(low ~ age, data=birthwt, family=binomial)
summary(birthwt.glm)
anova(birthwt.glm, test="Chisq")
vcov(birthwt.glm)

pred.ages1 <- c(15, 25, 35, 45)
pred.levels1 <- data.frame(age=pred.ages1)
birthwt.lpred1 <- predict(birthwt.glm, pred.levels1,
   type="link", se.fit=T)
birthwt.lpred1
birthwt.rpred1 <- predict(birthwt.glm, pred.levels1,
   type="response", se.fit=T)
birthwt.rpred1
glmlink.ci(birthwt.glm, pred.levels1)
exp(glmlink.ci(birthwt.glm, pred.levels1))
good.ci <- glmpred.ci(birthwt.glm, pred.levels1)
bad.ci <- glmpred.ci.bad(birthwt.glm, pred.levels1)
cbind(good.ci, bad.ci)

pred.ages <- seq(14,45,by=0.1)
pred.levels <- data.frame(age=pred.ages)
birthwt.lpred <- predict(birthwt.glm, pred.levels, type="link",se.fit=T)
birthwt.rpred <- predict(birthwt.glm, pred.levels, 
   type="response",se.fit=T)


glmlink.ci <- function(glm.object, pred.levels, conflevel=0.95) {
  alpha <- (1-conflevel) / 2
  critval <- qnorm(alpha, lower.tail=F)
  pred.object <- predict(glm.object, pred.levels, type="link", se.fit=T)
  low <- pred.object$fit - critval * pred.object$se.fit
  upp <- pred.object$fit + critval * pred.object$se.fit
  ci <- cbind(lower=low, upper=upp)
  row.names(ci) <- pred.levels[,1]
  ci
}

glmpred.ci <- function(glm.object, pred.levels, conflevel=0.95) {
  alpha <- (1-conflevel) / 2
  critval <- qnorm(alpha, lower.tail=F)
  pred.object <- predict(glm.object, pred.levels, type="link", se.fit=T)
  low <- pred.object$fit - critval * pred.object$se.fit
  lower <- family(glm.object)$linkinv(low)
  upp <- pred.object$fit + critval * pred.object$se.fit
  upper <- family(glm.object)$linkinv(upp)
  ci <- cbind(lower=lower, upper=upper)
  row.names(ci) <- pred.levels[,1]
  ci
}

glmpred.ci.bad <- function(glm.object, pred.levels, conflevel=0.95) {
  alpha <- (1-conflevel) / 2
  critval <- qnorm(alpha, lower.tail=F)
  pred.object <- predict(glm.object, pred.levels, type="response", se.fit=T)
  low <- pred.object$fit - critval * pred.object$se.fit
  upp <- pred.object$fit + critval * pred.object$se.fit
  ci <- cbind(lower=low, upper=upp)
  row.names(ci) <- pred.levels[,1]
  ci
}

birthwt.linkci <- glmlink.ci(birthwt.glm, pred.levels)
birthwt.predci <- glmpred.ci(birthwt.glm, pred.levels)
birthwt.predci.bad <- glmpred.ci.bad(birthwt.glm, pred.levels)

postscript("../birthwtlinkci.eps", width=8, height=5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")

plot(pred.ages, birthwt.lpred$fit, xlab="Maternal Age", 
  ylab="Log Odds of Low Birth Weight",
  main="95% Pointwise CI of Log Odds", type="l", 
  ylim=c(min(birthwt.linkci[,1]), max(birthwt.linkci[,2])))
lines(pred.ages, birthwt.linkci[,"lower"], lty = 2)
lines(pred.ages, birthwt.linkci[,"upper"], lty = 2)
dev.off()

postscript("../birthwtoddsci.eps", width=8, height=4.5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")

plot(pred.ages, exp(birthwt.lpred$fit), xlab="Maternal Age", 
  ylab="Odds of Low Birth Weight",
  main="95% Pointwise CI of Odds", type="l", 
  ylim=c(exp(min(birthwt.linkci[,1])), exp(max(birthwt.linkci[,2]))))
lines(pred.ages, exp(birthwt.linkci[,"lower"]), lty = 2)
lines(pred.ages, exp(birthwt.linkci[,"upper"]), lty = 2)
dev.off()

  
postscript("../birthwtpredci.eps", width=8, height=6, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")

plot(low ~ age, data=birthwt, xlab="Maternal Age", 
  ylab="Low Birth Weight", main="95% Pointwise CI of Probability",
  ylim=c(-0.05, 1.05))
symbols(colnames(birthtable), rep(1,ages), circles=sqrt(birthtable[2,]*0.05),
  add=T, inches=F, fg=2, bg=2)
symbols(colnames(birthtable), rep(0,ages), circles=sqrt(birthtable[1,]*0.05),
  add=T, inches=F, fg=4, bg=4)
lines(pred.ages, birthwt.rpred$fit)
lines(pred.ages, birthwt.predci[,"lower"], lty = 2)
lines(pred.ages, birthwt.predci[,"upper"], lty = 2)
dev.off()

postscript("../birthwtpredcibad.eps", width=8, height=5.5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")

plot(low ~ age, data=birthwt, xlab="Maternal Age", 
  ylab="Low Birth Weight", main="95% Pointwise CI of Probability",
  ylim=c(-0.05, 1.05))
symbols(colnames(birthtable), rep(1,ages), circles=sqrt(birthtable[2,]*0.05),
  add=T, inches=F, fg=2, bg=2)
symbols(colnames(birthtable), rep(0,ages), circles=sqrt(birthtable[1,]*0.05),
  add=T, inches=F, fg=4, bg=4)
lines(pred.ages, birthwt.rpred$fit)
lines(pred.ages, birthwt.predci[,"lower"], lty = 2, col=4)
lines(pred.ages, birthwt.predci[,"upper"], lty = 2, col=4)
lines(pred.ages, birthwt.predci.bad[,"lower"], lty = 2, col=2)
lines(pred.ages, birthwt.predci.bad[,"upper"], lty = 2, col=2)
legend(45,1,c("Asymmetric CI","Symmetric CI"),col=c(4,2),
    lty=2, lwd=1.5,xjust=1)
dev.off()

birthwt.coef <- coef(birthwt.glm)
birthwt.coef[2]
birthwt.se <- sqrt(diag(vcov(birthwt.glm)))
birthwt.se[2]
est <- (15 - 25) * birthwt.coef[2]
est
se <- abs((15 - 25) * birthwt.se[2])
se
logodds.ci <- c(est - qnorm(0.025, lower.tail=F)*se , 
                est + qnorm(0.025, lower.tail=F)*se)
logodds.ci
odds.ci <- exp(logodds.ci)
odds.ci



# non-linearity of logit transform

eta <- (-400:400)/100
odds <- exp(eta)
prob <- odds / (1 + odds)
slope <- 1/2^2

postscript("../logitlin.eps", width=5, height=4, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(1,1))
  
plot(eta, prob, xlab=expression(eta), ylab=expression(pi), type="l")
abline(a=0.5, b=slope, lty=2)  
abline(h=c(0.2,0.8), lty=3)
dev.off()

# Model comparison

attach(birthwt)
     race <- factor(race, labels = c("white", "black", "other"))
     ptd <- factor(ptl > 0)
     ftv <- factor(ftv)
     levels(ftv)[-(1:2)] <- "2+"
     bwt <- data.frame(low = factor(low), age, lwt, race,
         smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
detach("birthwt")
options(contrasts = c("contr.treatment", "contr.poly"))
birthwtall.glm <- glm(low ~ ., binomial, bwt)
summary(birthwtall.glm)

birthwt2.glm <- update(birthwtall.glm, . ~ . - age - lwt)
summary(birthwt2.glm)
anova(birthwt2.glm, birthwtall.glm, test="Chisq")

birthwt3.glm <- update(birthwtall.glm, . ~ . - lwt)
summary(birthwt3.glm)
anova(birthwt3.glm, birthwtall.glm, test="Chisq")

birthwt4.glm <- update(birthwt3.glm, . ~ . + lwt)
summary(birthwt4.glm)
anova(birthwt4.glm, test="Chisq")

birthwt.al.glm <- glm(low ~ age + lwt, family=binomial, data=bwt)
birthwt.la.glm <- glm(low ~ lwt + age, family=binomial, data=bwt)
anova(birthwt.al.glm, test="Chisq")
anova(birthwt.la.glm, test="Chisq")

bodyfat.full.lm <- lm(Bodyfat ~ Tricep + Thigh, data=bodyfat)
bodyfat.red.lm <- lm(Bodyfat ~ 1, data=bodyfat)
bodyfat.full.glm <- glm(Bodyfat ~ Tricep + Thigh, data=bodyfat,
  family=gaussian)
bodyfat.red.glm <- glm(Bodyfat ~ 1, data=bodyfat,
  family=gaussian)
  
anova(bodyfat.red.lm, bodyfat.full.lm, test="F")
anova(bodyfat.red.glm, bodyfat.full.glm, test="Chisq")
anova(bodyfat.red.glm, bodyfat.full.glm, test="F")
