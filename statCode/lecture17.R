library(RWinEdt)
library(MASS)
library(epitools)
library(lattice)
options(width=75)

# Quine example

Quine <- quine
levels(Quine$Eth) <- c("Aboriginal", "Non-Aboriginal")
levels(Quine$Sex) <- c("Female", "Male")
levels(Quine$Age) <- c("Primary", "First form", "Second form", "Third form")
levels(Quine$Lrn) <- c("Average learner", "Slow learner")


trellis.device("postscript", file="../quinebox.eps", width=9, height=6
  ,horiz=F, col=T)
  
bwplot(Age ~ Days | Sex*Lrn*Eth, data=Quine, layout=c(4,2))

dev.off()

trellis.device("postscript", file="../quineboxdef.eps", width=9, height=6
  ,horiz=F)
bwplot(Days ~ Age | Sex*Lrn*Eth, data=Quine, layout=c(4,2))
dev.off()

quine.qglm <- glm(Days ~ .^4, family = quasipoisson(), data = quine)
summary(quine.qglm)

postscript("../quine.qresid.eps", width=9, height=6, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(2,3))

plot(fitted(quine.qglm),resid(quine.qglm), xlab="Fitted Days Absent",
  ylab="Deviance Residual")
plot(fitted(quine.qglm),resid(quine.qglm,type="pearson"), xlab="Fitted Days Absent",
  ylab="Pearson Residual")
plot(Quine$Eth,resid(quine.qglm), xlab="Ethnic Group",
  ylab="Deviance Residual")
plot(Quine$Sex,resid(quine.qglm), xlab="Gender",
  ylab="Deviance Residual")
plot(Quine$Age,resid(quine.qglm), xlab="Education Level",
  ylab="Deviance Residual")
plot(Quine$Lrn,resid(quine.qglm), xlab="Learning Ability",
  ylab="Deviance Residual")

dev.off()

quine2.qglm <- update(quine.qglm, . ~ Sex/(Age + Eth*Lrn))
summary(quine2.qglm)

postscript("../quine2.qresid.eps", width=9, height=6, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(1,1))

plot(fitted(quine2.qglm),resid(quine2.qglm), xlab="Fitted Days Absent",
  ylab="Deviance Residual")

dev.off()


anova(quine2.qglm, quine.qglm, test="F")

quine.glm <- glm(Days ~ .^4, family = negative.binomial(2), data = quine)
summary(quine.glm)

postscript("../quine.resid.eps", width=9, height=6, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(2,3))

plot(fitted(quine.glm),resid(quine.glm), xlab="Fitted Days Absent",
  ylab="Deviance Residual")
  plot(fitted(quine.glm),resid(quine.glm,type="pearson"), xlab="Fitted Days Absent",
  ylab="Pearson Residual")
plot(Quine$Eth,resid(quine.glm), xlab="Ethnic Group",
  ylab="Deviance Residual")
plot(Quine$Sex,resid(quine.glm), xlab="Gender",
  ylab="Deviance Residual")
plot(Quine$Age,resid(quine.glm), xlab="Education Level",
  ylab="Deviance Residual")
plot(Quine$Lrn,resid(quine.glm), xlab="Learning Ability",
  ylab="Deviance Residual")

dev.off()


quine2.glm <- update(quine.glm, . ~ Sex/(Age + Eth*Lrn))
summary(quine2.glm)

postscript("../quine2.resid.eps", width=9, height=6, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(1,1))

plot(fitted(quine2.glm),resid(quine2.glm), xlab="Fitted Days Absent",
  ylab="Deviance Residual")

dev.off()


anova(quine2.glm, quine.glm, test="Chisq")


quine.nb <- glm.nb(Days ~ .^4, data = quine)
summary(quine.nb)
c(theta = quine.nb$theta, SE = quine.nb$SE)
anova(quine.nb)

quine2.nb <- glm.nb(Days ~ Lrn/(Age + Eth + Sex)^2, data=quine)

anova(quine2.nb, quine.nb)
anova(quine2a.nb, quine.nb)

postscript("../quine.nbresid.eps", width=9, height=6, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(2,3))

plot(fitted(quine2.nb),resid(quine2.nb), xlab="Fitted Days Absent",
  ylab="Deviance Residual")
  plot(fitted(quine2.nb),resid(quine2.nb,type="pearson"), xlab="Fitted Days Absent",
  ylab="Pearson Residual")
plot(Quine$Eth,resid(quine2.nb), xlab="Ethnic Group",
  ylab="Deviance Residual")
plot(Quine$Sex,resid(quine2.nb), xlab="Gender",
  ylab="Deviance Residual")
plot(Quine$Age,resid(quine2.nb), xlab="Education Level",
  ylab="Deviance Residual")
plot(Quine$Lrn,resid(quine2.nb), xlab="Learning Ability",
  ylab="Deviance Residual")

dev.off()

quine2a.nb <- stepAIC(quine.nb)

quine3.nb <- update(quine2.nb, . ~ . - Eth:Age:Lrn - Sex:Age:Lrn)
quine3a.nb <- update(quine2a.nb, . ~ . - Eth:Age:Lrn - Sex:Age:Lrn)

anova(quine2.nb, quine3.nb, test="Chisq")
anova(quine2a.nb, quine3a.nb, test="Chisq")


# Business major

n <- c(68, 56, 91, 40, 5, 6, 61, 59)
major <- c("Accounting","Accounting","Administration","Administration",
   "Economics","Economics","Finance","Finance")
gender <- rep(c("Female","Male"),4)

business.tab <- matrix(n, ncol=2, byrow=T, 
  dimnames=list(major=unique(major), gender=c("Female","Male")))

business <- data.frame(n=n, major=major, gender=gender)

business.ind <- glm(n ~ major + gender, family=poisson(), data=business)

summary(business.ind)
anova(business.ind, test="Chisq")

business.int <- glm(n ~ major * gender, family=poisson(), data=business)

summary(business.ind)
anova(business.ind, test="Chisq")
pchisq(deviance(business.ind),df.residual(business.ind),lower.tail=F)

business.tab
chisq.test(business.tab)

summary(business.int)

anova(business.ind, business.int, test="Chisq")
