library(RWinEdt)
library(MASS)
library(epitools)
options(width=75)

return.glm <- glm(returned ~ deposit, family=binomial)
return.glm

birthtable <- table(birthwt$low,birthwt$age)
ages <- dim(birthtable)[2]
birthtable[birthtable == 0] <- NA

postscript("../birthwt.eps", width=8, height=4, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")

plot(low ~ age, data=birthwt, xlab="Maternal Age", 
  ylab="Low Birth Weight", ylim=c(-0.05, 1.05))
symbols(colnames(birthtable), rep(1,ages), circles=sqrt(birthtable[2,]*0.05),
  add=T, inches=F, fg=2, bg=2)
symbols(colnames(birthtable), rep(0,ages), circles=sqrt(birthtable[1,]*0.05),
  add=T, inches=F, fg=4, bg=4)
dev.off()

birthwt.lm <- lm(low ~ age, data=birthwt)
summary(birthwt.lm)
anova(birthwt.lm)


birthwt.glm <- glm(low ~ age, data=birthwt, family=binomial)
summary(birthwt.glm)
anova(birthwt.glm, test="Chisq")
vcov(birthwt.glm)

betahat <- coef(birthwt.glm)
betahat
# vcov(birthwt.glm) contains the variance/covariance matrix of betahat
se.betahat <- sqrt(diag(vcov(birthwt.glm)))  
se.betahat
me.betahat <- qnorm(0.975) * se.betahat
ci.betahat <- cbind(Lower=betahat - me.betahat, Upper=betahat + me.betahat)
ci.betahat

pred.ages <- seq(14,45,by=0.1)
pred.levels <- data.frame(age=pred.ages)
birthwt.pred <- predict(birthwt.glm, pred.levels, type="response")

postscript("../birthwtfit.eps", width=8, height=3.75, horiz=F)
par(mar=c(4,4,1,1) + 0.1, pty="m")

plot(low ~ age, data=birthwt, xlab="Maternal Age", 
  ylab="Low Birth Weight", ylim=c(-0.05, 1.05))
symbols(colnames(birthtable), rep(1,ages), circles=sqrt(birthtable[2,]*0.05),
  add=T, inches=F, fg=2, bg=2)
symbols(colnames(birthtable), rep(0,ages), circles=sqrt(birthtable[1,]*0.05),
  add=T, inches=F, fg=4, bg=4)
lines(pred.ages, birthwt.pred)
text(40,0.5,expression(hat(pi)(age)),cex=1.25)
arrows(40,0.43,35,birthwt.pred[211],length=0.1)
dev.off()

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
birthwt.step <- stepAIC(birthwtall.glm)
birthwt.step$anova


# changes in odds and probability

eta <- (-500:500)/100
odds <- exp(eta)
prob <- odds / (1 + odds)

etaj <- c(0,1,2,3)
oddsj <- exp(etaj)
probj <- oddsj / (1 + oddsj)

postscript("../deltafit.eps", width=10, height=6.5, horiz=F)
par(mar=c(4,4,4,1) + 0.1, pty="m")
par(mfrow=c(1,2))

plot(eta[101:901],odds[101:901], main="Odds", xlab=expression(eta),
  ylab=expression(omega), type="l")
segments(etaj[1],oddsj[1],etaj[2],oddsj[1])
segments(etaj[2],oddsj[1],etaj[2],oddsj[2])
segments(etaj[3],oddsj[3],etaj[4],oddsj[3])
segments(etaj[4],oddsj[3],etaj[4],oddsj[4])

text(-2.5,5,"Factor of 2.718")
arrows(rep(-1,2),c(3.7,6.3),c(1,3),c(1.8,13),length=0.1)
text(-3.75,50,expression(omega == e^eta), adj=0, cex=1.25)
text(2.1,5,expression(Delta[eta] == 1),adj=0)
  
plot(eta, prob, main="Probability", xlab=expression(eta),
  ylab=expression(pi), type="l")
segments(etaj[1],probj[1],etaj[2],probj[1])
segments(etaj[2],probj[1],etaj[2],probj[2])
segments(etaj[3],probj[3],etaj[4],probj[3])
segments(etaj[4],probj[3],etaj[4],probj[4])
text(-4.75,0.6,"Changes by 0.231",adj=0)

text(4.75,0.3,"Changes by 0.072",adj=1)
arrows(4,0.33,3,0.9,length=0.1)
text(-4.75,0.9,expression(pi == frac(e^eta,1+e^eta)), adj=0, cex=1.25)
text(0.1,0.45,expression(Delta[eta] == 1),adj=0)
  
dev.off()
