###################################################
### chunk number 1: makenonlin
###################################################
library(apsrtable)
set.seed(123)
x <- rep(1:5, 100)
x <- x[order(x)]

y <- 2 + x + rnorm(500,0,2)
x <- as.factor(x)

mod <- lm(y ~ x)
apsrtable(mod, model.names="")


###################################################
### chunk number 2: glht1
###################################################
L <- matrix(c(
0, 2, -1, 0, 0, 
0, 3, 0, -1, 0, 
0, 4, 0, 0, -1), ncol=5, byrow=T)
c <- rep(0, 3)

e <- matrix(residuals(mod), ncol=1)
s2e <- (t(e)%*% e)/with(mod, df.residual)
b <- coef(mod)
X <- model.matrix(mod)

F0 <- (t(L%*%b-c) %*% solve(L%*%solve(t(X) %*% X)%*% 
    t(L))%*%(L%*%b - c))/(3*s2e)

cat("F = ",F0, "\n", sep="")
cat("Pr(>F) = ", 1-pf(F0, 3, with(mod, df.residual)), "\n", sep="")


###################################################
### chunk number 3: nlsimfig
###################################################
plot(seq(1,5), c(0, b[2:5]), pch=19, type="o", xlab="x", ylab="predicted")
abline(a=-1, b=1, lty=2)


###################################################
### chunk number 4: anovatest
###################################################
library(xtable)
restricted.mod <- lm(y ~ as.numeric(x))
unrestricted.mod <- lm(y ~ x)
xtable(anova(restricted.mod, unrestricted.mod, test="F"))


###################################################
### chunk number 5: glmlin
###################################################
anes <- read.dta("anes1992.dta")
anes[["pidfac"]] <- as.factor(anes[["pid"]])
unrestricted.mod <- glm(votedem ~ retnat + pidfac + age + male + educ + 
	black + south, data=anes, family=binomial)
restricted.mod <- glm(votedem ~ retnat + pid + age + male + educ + 
	black + south, data=anes, family=binomial)


###################################################
### chunk number 6: glmout
###################################################
apsrtable(unrestricted.mod, restricted.mod, model.names="", Sweave=T)


###################################################
### chunk number 7: plotglm1
###################################################
unres.eff <- effect("pidfac", unrestricted.mod)
res.eff <- effect("pid", restricted.mod, default.levels=7)
plot(c(1,7), c(0,1), type="n", xlab="Party ID", ylab="Pr(Vote Dem)")
polygon(x=c(1:7,7:1,1), y=plogis(c(unres.eff[["lower"]], 		
	rev(unres.eff[["upper"]]), unres.eff[["lower"]][1])), 
	col=rgb(0,1,0,.25,1), border=NA)
polygon(x=c(1:7,7:1,1), y=plogis(c(res.eff[["lower"]], 		
	rev(res.eff[["upper"]]), res.eff[["lower"]][1])), 
	col=rgb(0,0,1,.25,1), border=NA)
lines(c(1:7), plogis(res.eff[["fit"]]), lty=1, col="blue", lwd=2)
lines(c(1:7), plogis(unres.eff[["fit"]]), lty=1, col="green", lwd=2)

legend("topright", c("Linear","Non-linear"), fill=c(rgb(0,0,1,.25,1),
	rgb(0,1,0,.25,1)), inset=.01)


###################################################
### chunk number 8: loess
###################################################
dat <- read.table("weakliem.txt", header=T)
out.loess <- loess(secpay ~ gini, data=dat, span=.75, degree=1,
    family="symmetric")
plot(secpay ~ gini, data=dat)
lines(out.loess$fitted[order(dat$gini)] ~
    dat$gini[order(dat$gini)])
out.loess <- loess(secpay ~ gini, data=dat, span=.75, degree=2,
    family="symmetric")
lines(out.loess$fitted[order(dat$gini)] ~
    dat$gini[order(dat$gini)], lty=2)
legend("topright", c("Degree = 1", "Degree = 2"),
    lty=c(1,2), inset=.01)


###################################################
### chunk number 9: cpr1
###################################################
data(Prestige)
Prestige.model<-lm(prestige ~ income + education + 
	women, data=Prestige)
library(car)
cr.plot(Prestige.model, "income")


###################################################
### chunk number 10: cpr2
###################################################
cr.plot(Prestige.model, "education")


###################################################
### chunk number 11: cpr3
###################################################
cr.plot(Prestige.model, "women")


###################################################
### chunk number 12: orthpoly
###################################################
mod <- lm(prestige ~ poly(income, 3) +
    poly(education, 2), data=Prestige)
apsrtable(mod, model.names="", Sweave=T)


###################################################
### chunk number 13: polyeff1
###################################################
library(effects)
plot(effect("poly(income, 3)", mod,
    default.levels=100, se=T))


###################################################
### chunk number 14: polyeff2
###################################################
plot(effect("poly(education, 2)", mod,
    default.levels=100, se=T))


###################################################
### chunk number 15: ornbox
###################################################
data(Ornstein)
mod <- lm(interlocks ~ box.cox.var(interlocks+1) + assets + 
	sector + nation, data=Ornstein)
summary(mod)


###################################################
### chunk number 16: ornboxfig
###################################################
av.plots(mod,'box.cox.var(interlocks + 1)',
    col='black', identify.points=F)


###################################################
### chunk number 17: boxtid
###################################################
box.tidwell(prestige ~ income + education,
    ~poly(women, 2), data=Prestige)


###################################################
### chunk number 18: avinc
###################################################
newinc <- with(Prestige, log(income))
newed <- with(Prestige, education^2)
mod <- lm(prestige ~ newinc + newed + poly(women, 2), data=Prestige)
av.plots(mod,"newinc", identify.points=F, col="black")


###################################################
### chunk number 19: aved
###################################################
av.plots(mod,"newed", identify.points=F, col="black")


###################################################
### chunk number 20: incdiag1
###################################################
inc.mod1 <- lm(prestige ~ log(income) + education + 
    women, data=Prestige)
apsrtable(inc.mod1, model.names="", Sweave=T)


###################################################
### chunk number 21: incdiag2
###################################################
box.tidwell(prestige ~ income, ~ education + women, data=Prestige)


###################################################
### chunk number 22: incfix
###################################################
newinc <- with(Prestige, income^0.08073)
inc.mod2 <- lm(prestige ~ newinc + education +
	women, data=Prestige)
apsrtable(inc.mod2, model.names="", Sweave=T)


###################################################
### chunk number 23: eddiag1
###################################################
ed.mod1 <- lm(prestige ~ newinc + 
	poly(education, 3) + women, data=Prestige)
apsrtable(ed.mod1, model.names="", Sweave=T)


###################################################
### chunk number 24: edfig
###################################################
plot(effect("poly(education, 3)", ed.mod1))


###################################################
### chunk number 25: wompostinc
###################################################
cr.plot(inc.mod2,"women", col="black")


###################################################
### chunk number 26: wom2
###################################################
wom.mod1 <- lm(prestige ~ newinc + education + 
	poly(women, 2), data=Prestige)
apsrtable(wom.mod1, model.names="", Sweave=T)


###################################################
### chunk number 27: womeff
###################################################
plot(effect("poly(women, 2)", wom.mod1))


###################################################
### chunk number 28: boxinc2
###################################################
box.tidwell(prestige ~ income, ~education + 
    poly(women, 2), data=Prestige)


###################################################
### chunk number 29: finalmod
###################################################
final.mod <- lm(prestige ~ log(income) + education + 
	poly(women, 2), data=Prestige)
apsrtable(final.mod, model.names="", Sweave=T)


###################################################
### chunk number 30: effincfinal
###################################################
plot(effect("log(income)", final.mod))


###################################################
### chunk number 31: effwomfinal
###################################################
plot(effect("poly(women, 2)", final.mod))


