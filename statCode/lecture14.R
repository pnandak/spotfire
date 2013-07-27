library(RWinEdt)
library(MASS)
library(epitools)
library(lattice)
options(width=75)


# Fastener failure

fasten.load <- seq(2500, 4300, by=200)
fasten.n <- c(50, 70, 100, 60, 40, 85, 90, 50, 80, 65)
fasten.fail <- c(10, 17, 30, 21, 18, 43, 54, 33, 60, 51)
fasten.hold <- fasten.n - fasten.fail
fasten.prop <- fasten.fail/fasten.n
fasten <- data.frame(load=fasten.load, n=fasten.n, 
    fail=fasten.fail, hold=fasten.n-fasten.fail, prop=fasten.prop)
fasten.res <- cbind(fasten.fail,fasten.n-fasten.fail)
fasten.rev <- cbind(fasten.n-fasten.fail,fasten.fail)
fasten.bern <- data.frame(
  load = c(rep(fasten.load,fasten.hold),rep(fasten.load,fasten.fail)),
  response = c(rep(0,sum(fasten.hold)),rep(1,sum(fasten.fail)))
  )

fasten.logit.glm <- glm(fasten.res ~ fasten.load, family=binomial())

fasten.bern.glm <- glm(response ~ load, data=fasten.bern, family=binomial())
  
summary(fasten.logit.glm)
summary(fasten.bern.glm)
anova(fasten.logit.glm, test="Chisq")
anova(fasten.bern.glm, test="Chisq")


hosmer.lemeshow <- function(glmobject, nclass = 10, pen = 2) {
  response <- glmobject$y
  fits <- fitted(glmobject, response=T)
  levs <- co.intervals(fits, number=nclass, overlap=0)
  group <- rep(0,length(fits))
  for (i in 1:nclass) {
    group[fits >= levs[i,1] & fits < levs[i,2]] <- i }
  Obs <- rep(0, nclass)
  Exp <- Obs
  n <- Obs
  for(i in 1:nclass) {
    Obs[i] <- sum(response[group==i])
    Exp[i] <- sum(fits[group==i])
    n[i] <- sum(group == i)
  }
  hl <- sum((Obs - Exp)^2/(Exp * (1 - Exp/n)),na.rm=T)
  pvalue <- pchisq(hl, nclass-pen, lower.tail=F)
  list(hl=hl, pvalue=pvalue, df=nclass-pen,
       Obs=Obs, Exp=Exp, n=n, intervals=levs)
}  
   
birthwt.age.glm <- glm(low ~ age, family=binomial, data=bwt)
birthwt.age2.glm <- glm(low ~ age + I(age^2), family=binomial, data=bwt)

birthwt.hl.glm <- glm(low ~ ptd + age + ht + lwt + ui,
    family=binomial, data=bwt)

anova(birthwt.age.glm, birthwt.age2.glm, test="Chisq")
anova(birthwt.age.glm, birthwt.hl.glm, test="Chisq")


birthwt.hl10 <- hosmer.lemeshow(birthwt.hl.glm, nclass=10)

birthwt.hl10$hl
birthwt.hl10$pvalue

grange <- 6:14
hltable <- matrix(0, nrow=2, ncol<- length(grange))
for (g in 1:length(grange)) {
  temp <- hosmer.lemeshow(birthwt.hl.glm, nclass=grange[g])
  hltable[1,g] <- temp$hl
  hltable[2,g] <- temp$pvalue
}
print(rbind(grange,hltable),digits=2)


birthwt.hl6 <- hosmer.lemeshow(birthwt.hl.glm, nclass=6)
birthwt.hl20 <- hosmer.lemeshow(birthwt.hl.glm, nclass=20)

hosmer.lemeshow(birthwt.age.glm)

hosmer.lemeshow(birthwt.age.glm, 20)

# influence measures

birthwt.lm <- lm(low ~ ptd + age + ht + lwt + ui, data=bwt)
influence.measures(birthwt.hl.glm)
birthwt.dffits <- dffits(birthwt.hl.glm)
birthwt.dfbetas <- dfbetas(birthwt.hl.glm)
dfbetamax <- apply(abs(birthwt.dfbetas),1,max)
birthwt.cooksd <- cooks.distance(birthwt.hl.glm)
birthwt.hat <- hatvalues(birthwt.hl.glm)

birthwt.hat[birthwt.hat > 3*6/189]
birthwt.dffits[birthwt.dffits > 3*sqrt(6/183)]
birthwt.cooksd[birthwt.cooksd > qf(0.5,6,183)]
birthwt.dfbetas[dfbetamax > 1,]
birthwt.dfbetas[dfbetamax = max(dfbetamax),]

birthwt.age.lm <- lm(low ~ age, data=bwt)
hatvalues(birthwt.age.lm)
hatvalues(birthwt.age.glm)

influence.measures(birthwt.age.glm)


deposit.glm <- glm(returned ~ deposit, family=binomial())
deposit.drop.glm <- glm(returned ~ deposit, family=binomial(), 
  weight=c(1,1,1,0,1,1))
dep.levels <- data.frame(deposit=(0:300)/10)


postscript("../deposit.infl.eps", width=9, height=2.75, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(deposit, returned.prop, xlab="Deposit", ylab=expression(hat(pi)), 
   ylim=c(0,1),  pch=16)
lines((0:300)/10, predict(deposit.drop.glm, dep.levels, type="response"), col=4)
lines((0:300)/10, predict(deposit.glm, dep.levels, type="response"), col=2)


plot(deposit, deposit.dev, xlab="Deposit", ylab="Deviance Residual", 
   pch=16)
dev.off()

returned.prop <- returned[,1]/apply(returned,1,sum)
deposit.lm <- glm(returned.prop ~ deposit)

influence.measures(deposit.glm)
rstandard(deposit.glm)
rstudent(deposit.glm)
resid(deposit.glm)

influence.measures(deposit.lm)

influence.measures(fasten.logit.glm)

fasten.drop.glm <- glm(fasten.res ~ fasten.load, family=binomial(),
  weight=c(rep(1,8),0,1))
fast.levels <- data.frame(fasten.load = 2500:4300)


postscript("../fasten.infl.eps", width=9, height=5, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(fasten.load, fasten.prop, xlab="Load (psi)", ylab=expression(hat(pi)), 
  ylim=c(0,1), pch=16)
lines(2500:4300, predict(fasten.drop.glm, fast.levels, type="response"), col=4)
lines(2500:4300, predict(fasten.logit.glm, fast.levels, type="response"), col=2)

plot(fasten.load, fasten.dev, xlab="Load (psi)", ylab="Deviance Residual", 
  pch=16)
dev.off()


# glm contrast coding

belief <- read.table("belief.txt",header=T)
attach(belief)
favor <- round(M * PERCENT/100)
favor
against <- M - favor
against

context <- rep(0:1,4)
mode <- c(0,0,0,0,1,1,1,1)

temp.glm <- glm(cbind(favor,against) ~ CONTEXT * MODE, family=binomial)
summary(temp.glm)

temp2.glm <- glm(cbind(favor,against) ~ CONTEXT + MODE + CONTEXT : MODE, family=binomial)
summary(temp2.glm)

temp3.glm <- glm(cbind(favor,against) ~ context * mode, family=binomial)
summary(temp3.glm)

temp4.glm <- glm(cbind(favor,against) ~ context + mode + context : mode, family=binomial)
summary(temp4.glm)

temp5.glm <- glm(cbind(favor,against) ~ CONTEXT + MODE, family=binomial)
summary(temp5.glm)

temp6.glm <- glm(cbind(favor,against) ~ context + mode, family=binomial)
summary(temp6.glm)

anova(temp2.glm, test="Chisq")
anova(temp3.glm, test="Chisq")

options(contrasts=c("contr.treatment","contr.poly"))
options(contrasts=c("contr.sum","contr.poly"))
options(contrasts=c("contr.helmert","contr.poly"))

temp5.glm <- glm(cbind(favor,against) ~ as.factor(context) * as.factor(mode), family=binomial)
summary(temp5.glm)


temp.lm <- lm(PERCENT ~ CONTEXT * MODE)
summary(temp.lm)

temp2.lm <- lm(PERCENT ~ context * mode)
summary(temp2.lm)
