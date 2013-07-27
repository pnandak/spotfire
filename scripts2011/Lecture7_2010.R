###################################################
### chunk number 1: ss1
###################################################
library(pspline)
jacob <- read.dta("jacob.dta")
perot <- with(jacob, perotvote - min(perotvote))
perot <- perot/max(perot)
with(jacob, plot(perot, chal_vote, cex=.5))
with(jacob, lines(sm.spline(perot, chal_vote, spar=0), lwd=3, col="red"))


###################################################
### chunk number 2: ss2
###################################################
with(jacob, plot(perot, chal_vote, cex=.5))
with(jacob, lines(sm.spline(perot, chal_vote, spar=0.1), lwd=3, col="red"))


###################################################
### chunk number 3: ss3
###################################################
with(jacob, plot(perot, chal_vote, cex=.5))
with(jacob, lines(sm.spline(perot, chal_vote, spar=1), lwd=3, col="red"))


###################################################
### chunk number 4: ss4
###################################################
with(jacob, plot(perot, chal_vote, cex=.5))
with(jacob, lines(sm.spline(perot, chal_vote, spar=10), lwd=3, col="red"))


###################################################
### chunk number 5: ss1a
###################################################
with(jacob, plot(perot, chal_vote, cex=.5))
with(jacob, lines(sm.spline(perot, chal_vote, df=2), lwd=3, col="red"))


###################################################
### chunk number 6: ss2a
###################################################
with(jacob, plot(perot, chal_vote, cex=.5))
with(jacob, lines(sm.spline(perot, chal_vote, df=5), lwd=3, col="red"))


###################################################
### chunk number 7: ss3a
###################################################
with(jacob, plot(perot, chal_vote, cex=.5))
with(jacob, lines(sm.spline(perot, chal_vote, df=10), lwd=3, col="red"))


###################################################
### chunk number 8: ss4a
###################################################
with(jacob, plot(perot, chal_vote, cex=.5))
with(jacob, lines(sm.spline(perot, chal_vote, df=20), lwd=3, col="red"))


###################################################
### chunk number 9: prestgam
###################################################
prestige.gam <- gam(prestige ~ s(income) + s(education), data=Prestige)
summary(prestige.gam)


###################################################
### chunk number 10: visgam1
###################################################
vis.gam(prestige.gam, theta=-40)


###################################################
### chunk number 11: visgam2
###################################################
vis.gam(prestige.gam, se=2, theta=-20)


###################################################
### chunk number 12: twofigs1
###################################################
ed.seq <- with(Prestige, seq(min(education), max(education), 
	length=25))
ed.pred <- predict(prestige.gam, newdata=data.frame(
	education=ed.seq, income=mean(Prestige[["income"]])), 
	se.fit=T)
lower <- with(ed.pred, fit - 2*se.fit)
upper <- with(ed.pred, fit + 2*se.fit)
plot(ed.seq, ed.pred[["fit"]], ylim=range(c(lower,upper)), 
	xlab="Education", ylab="Predicted Prestige", type="l")
lines(ed.seq, lower, lty=2)
lines(ed.seq, upper, lty=2)
with(Prestige, rug(education))


###################################################
### chunk number 13: twofigs2
###################################################
inc.seq <- with(Prestige, seq(min(income), max(income), 
	length=25))
inc.pred <- predict(prestige.gam, 
	newdata=data.frame(education=mean(Prestige[["education"]]), 
	income= inc.seq), se.fit=T)
lower <- with(inc.pred, fit - 2*se.fit)
upper <- with(inc.pred, fit + 2*se.fit)
plot(inc.seq, inc.pred[["fit"]], ylim=range(c(lower,upper)), 
	xlab="Income", ylab="Predicted Prestige", type="l")
lines(inc.seq, lower, lty=2)
lines(inc.seq, upper, lty=2)
with(Prestige, rug(income))


###################################################
### chunk number 14: lintest
###################################################
library(xtable)
prestige.ols<-gam(prestige~income+education, data=Prestige)
xtable(anova(prestige.ols, prestige.gam, test="F"))


###################################################
### chunk number 15: gamcheck
###################################################
gam.check(prestige.gam)


###################################################
### chunk number 16: gaminter
###################################################
type.bc<-with(Prestige, as.numeric(type=="bc"))
type.prof<-with(Prestige, as.numeric(type=="prof"))
type.wc<-with(Prestige, as.numeric(type=="wc"))
inter.gam<-gam(prestige~type+s(income,by=type.bc)
        +s(income,by=type.prof)+s(income,by=type.wc), 
		data=Prestige)
summary(inter.gam)


###################################################
### chunk number 17: bcinc
###################################################
plot.gam(inter.gam, select=1)
title("Blue Collar")


###################################################
### chunk number 18: profinc
###################################################
plot.gam(inter.gam, select=2)
title("Professional")


###################################################
### chunk number 19: wcinc
###################################################
plot.gam(inter.gam, select=3)
title("White Collar")


###################################################
### chunk number 20: demogam
###################################################
weakliem <- read.table("weakliem.txt")
nondemo <- with(weakliem, as.numeric(democrat == 0))
demo.gam<-gam(secpay~democrat + s(gini,by=democrat)
        +s(gini,by=nondemo), data=weakliem)


###################################################
### chunk number 21: sumdemogam
###################################################
summary(demo.gam)


###################################################
### chunk number 22: dgfig1
###################################################
plot(demo.gam, select=1)
title("Inequality in Democracies")


###################################################
### chunk number 23: dgfig2
###################################################
plot(demo.gam, select=2)
title("Inequality in Non-democracies")


###################################################
### chunk number 24: demolin
###################################################
demo.lm<-gam(secpay~democrat*gini, data=weakliem)
xtable(anova.gam(demo.lm, demo.gam))


###################################################
### chunk number 25: anes
###################################################
nes <- read.dta("anes1992.dta")
mod <- gam(votedem ~ s(pid, k=7) + s(age) + female + educ + 
	black + retnat + south, family=binomial, data=nes)


###################################################
### chunk number 26: modsum
###################################################
summary(mod)


###################################################
### chunk number 27: pid
###################################################
plot(mod, select=1)
title("Effect of Party ID")


###################################################
### chunk number 28: age
###################################################
plot(mod, select=2)
title("Effect of Age")


###################################################
### chunk number 29: agetest
###################################################
moda <- gam(votedem ~ s(pid, k=7, fx=T) + s(age) + female + educ + 
	black + retnat + south, family=binomial, data=nes)
agelin <- gam(votedem ~ s(pid, k=7, fx=T) + age + female + educ + 
	black + retnat + south, family=binomial, data=nes)
xtable(anova(moda, agelin, test='Chisq'))


###################################################
### chunk number 30: pidlintest
###################################################
mod2 <- gam(votedem ~ s(pid, k=7, fx=F) + age + female + educ + 
	black + retnat + south, family=binomial, data=nes)
piddum <- 	gam(votedem ~ as.factor(pid) + age + female + educ + 
		black + retnat + south, family=binomial, data=nes)
xtable(anova(mod2, piddum, test='Chisq'))


###################################################
### chunk number 31: pidpreds
###################################################
newdata <- with(nes, data.frame(pid = 1:7, age = mean(age, na.rm=T), 
	female = 0, educ = mean(educ, na.rm=T), black = 0, 
	retnat = factor(2, levels=1:3, labels=levels(retnat)), south = 0))
preds <- predict(mod2, newdata=newdata, se.fit=T, type="link")
lower <- plogis(with(preds, fit - 2*se.fit))
upper <- plogis(with(preds, fit + 2*se.fit))
plot(plogis(preds[["fit"]]) ~ newdata[["pid"]], type="l", xlab="Party ID", 
	ylab="Pr(Vote Democrat)", ylim=c(min(lower), max(upper)))
lines(newdata[["pid"]], lower, lty=2)
lines(newdata[["pid"]], upper, lty=2)


