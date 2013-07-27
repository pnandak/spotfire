###################################################
### chunk number 1: Ftestlpr
###################################################
library(car)
data(Prestige)

mod <- lm(prestige ~ income, data=Prestige)

trans.mod <- lm(prestige ~ log(income), data=Prestige)
loess.mod <- loess(prestige ~ income, data=Prestige, span=.5,
    family="symmetric")

rss0 <- with(Prestige, sum((prestige - fitted(trans.mod))^(2)))
rss1 <- with(loess.mod, sum((y - fitted)^2))
F0 <- with(loess.mod, ((rss0-rss1)/(trace.hat-length(coef(trans.mod))))/
    (rss1/(n-trace.hat)))
cat("F = ", round(F0, 2), "\n", sep="")
pval <- with(loess.mod, pf(F0, (trace.hat-length(coef(trans.mod))),
    (n-trace.hat), lower.tail=F))
cat("Pr( > F) = ", round(pval, 2), "\n", sep="")


###################################################
### chunk number 2: paranonpara
###################################################
inc.seq <- with(Prestige, seq(from=min(income), to=max(income), length=1000))
para.pred <- predict(trans.mod, 
	newdata=data.frame(income=inc.seq), se.fit=T)
nonpara.pred <- predict(loess.mod, 
	newdata=data.frame(income=inc.seq), se=T)

with(para.pred, plot(fit ~ inc.seq, type="l", xlab="Income",
 	ylab="Predicted Prestige", ylim=range(Prestige[["prestige"]])))
with(nonpara.pred, lines(fit ~ inc.seq, lty=2))
with(Prestige, points(prestige ~ income, pch=".", cex=3))
legend("bottomright", c("Parametric Fit", "Nonparametric Fit"), lty=c(1,2), 
    inset=.01)


###################################################
### chunk number 3: paranonparaci
###################################################
poly.x <- c(inc.seq, inc.seq[1000:1], inc.seq[1])
para.lower <- with(para.pred, fit - 2*se.fit)
para.upper <- with(para.pred, fit + 2*se.fit)
nonpara.lower <- with(nonpara.pred, fit - 2*se.fit)
nonpara.upper <- with(nonpara.pred, fit + 2*se.fit)

para.poly.y <- c(para.lower, para.upper[1000:1], para.lower[1])
nonpara.poly.y <- c(nonpara.lower, nonpara.upper[1000:1], nonpara.lower[1])

with(para.pred, plot(fit ~ inc.seq, type="n", xlab="Income", 
	ylab="Predicted Prestige", ylim=c(min(c(para.lower, nonpara.lower)), 
	max(c(para.upper, nonpara.upper)))))
polygon(poly.x, para.poly.y, col="gray65", border=NA)
polygon(poly.x, nonpara.poly.y, col=rgb(0,0,1,.25), border=NA)
with(para.pred, lines(fit ~ inc.seq, lty=1))
with(nonpara.pred, lines(fit ~ inc.seq, lty=2))
legend("bottomright", c("Parametric Fit", "Nonparametric Fit"), lty=c(1,2), 
    inset=.01)


###################################################
### chunk number 4: pnpcode1
###################################################
inc.seq <- with(Prestige, seq(from=min(income), to=max(income), length=1000))
para.pred <- predict(trans.mod, 
	newdata=data.frame(income=inc.seq), se.fit=T)
nonpara.pred <- predict(loess.mod, 
	newdata=data.frame(income=inc.seq), se=T)

with(para.pred, plot(fit ~ inc.seq, type="l", xlab="Income",
 	ylab="Predicted Prestige", ylim=range(Prestige[["prestige"]])))
with(nonpara.pred, lines(fit ~ inc.seq, lty=2))
with(Prestige, points(prestige ~ income, pch=".", cex=3))
legend("bottomright", c("Parametric Fit", "Nonparametric Fit"), lty=c(1,2), 
    inset=.01)


###################################################
### chunk number 5: pnpcode2
###################################################
poly.x <- c(inc.seq, inc.seq[1000:1], inc.seq[1])
para.lower <- with(para.pred, fit - 2*se.fit)
para.upper <- with(para.pred, fit + 2*se.fit)
nonpara.lower <- with(nonpara.pred, fit - 2*se.fit)
nonpara.upper <- with(nonpara.pred, fit + 2*se.fit)

para.poly.y <- c(para.lower, para.upper[1000:1], para.lower[1])
nonpara.poly.y <- c(nonpara.lower, nonpara.upper[1000:1], nonpara.lower[1])

with(para.pred, plot(fit ~ inc.seq, type="n", xlab="Income", 
	ylab="Predicted Prestige", ylim=c(min(c(para.lower, nonpara.lower)), 
	max(c(para.upper, nonpara.upper)))))
polygon(poly.x, para.poly.y, col="gray65", border=NA)
polygon(poly.x, nonpara.poly.y, col=rgb(0,0,1,.25), border=NA)
with(para.pred, lines(fit ~ inc.seq, lty=1))
with(nonpara.pred, lines(fit ~ inc.seq, lty=2))
legend("bottomright", c("Parametric Fit", "Nonparametric Fit"), lty=c(1,2), 
    inset=.01)


###################################################
### chunk number 6: simplespline
###################################################
set.seed(15)
x <- seq(1,100, by=1)

before <- function(x) ifelse (x<60, 60-x,0)
after <- function(x) ifelse (x<60,0, x-60)

X <- cbind(before(x), after(x))
y <- 1 + 1*before(x) + 1*after(x) + rnorm(100, 0, 5)
plot(y~x)


###################################################
### chunk number 7: polyfig
###################################################
x.s <- seq(from=0, to=100, by=1)
poly.mod <- lm(y ~ poly(x,2))
poly.pred <- predict(poly.mod, newdata=data.frame(x=x.s))
plot(x,y)
lines(poly.pred ~ x.s)
lpr <- loess(y ~ x)
lpr.pred <- predict(lpr, newdata= data.frame(x=x.s))
lines(x.s, lpr.pred, lty=2)
legend("topright", c(paste("Polynomial, df=", with(poly.mod, rank), 	
	sep=""), paste("LPR, df=", with(lpr, round(trace.hat, 2)), sep="")), 
	lty=c(1,2), inset=.01)


###################################################
### chunk number 8: dumint
###################################################
d <- as.numeric(x>60)
mod <- lm(y ~ x*d)

x.s <- seq(from=0, to=100, by=1)
d.s <- as.numeric(x.s>60)

pred <- predict(mod, newdata=data.frame(x=x.s, d=d.s))

plot(pred ~ x.s, type="l")


###################################################
### chunk number 9: spline1
###################################################
x1p <- ifelse(x > 60, x-60, 0)

mod1 <- lm(y ~ x + x1p)
x1p.s <- ifelse(x.s > 60, x.s-60, 0)
pred1 <- predict(mod1, newdata=data.frame(x=x.s, x1p=x1p.s))

plot(pred1 ~ x.s, type="l")


###################################################
### chunk number 10: compspline
###################################################
library(foreign)
library(splines)
jacob <- read.dta("jacob.dta")
perot <- with(jacob, perotvote - min(perotvote))
perot <- perot/max(perot)

xk <- seq(.2,.8, by=.2)
mod.ns <- lm(chal_vote ~ ns(perot, knots=xk), data=jacob)
mod.bs <- lm(chal_vote ~ bs(perot, knots=xk), data=jacob)
xp <- seq(0,1,by=.01)
pred.ns <- predict(mod.ns, newdata=data.frame(perot=xp))
pred.bs <- predict(mod.bs, newdata=data.frame(perot=xp))

with(jacob, plot(perot, chal_vote, pch=".", cex=1.75, ylab="Challengers' Vote Share (%)", xlab="Vote for Perot", bty="l"))
lines(pred.ns ~ xp, lty=2, col="red", lwd=3)
lines(pred.bs ~ xp, lty=3, col="blue", lwd=3)

legend("bottomright", c("Natural Spline","B-Spline"), lty=c(2,3), 
    lwd=c(3,3), col=c("red","blue"),inset=.01)


###################################################
### chunk number 11: aicbic
###################################################
mod.bs2 <- lm(chal_vote ~ bs(perot, knots=seq(0,1, length=4)[-c(1,4)]), 	
	data=jacob)
mod.bs3 <- lm(chal_vote ~ bs(perot, knots=seq(0,1, length=5)[-c(1,5)]), 
	data=jacob)
mod.bs4 <- lm(chal_vote ~ bs(perot, knots=seq(0,1, length=6)[-c(1,6)]),	
	data=jacob)
mod.bs5 <- lm(chal_vote ~ bs(perot, knots=seq(0,1, length=7)[-c(1,7)]), 
	data=jacob)
mod.bs6 <- lm(chal_vote ~ bs(perot, knots=seq(0,1, length=8)[-c(1,8)]), 	
	data=jacob)
mod.bs7 <- lm(chal_vote ~ bs(perot, knots=seq(0,1, length=9)[-c(1,9)]), 
	data=jacob)
mod.bs8 <- lm(chal_vote ~ bs(perot, knots=seq(0,1, length=10)[-c(1,10)]), 	
	data=jacob)
mod.bs9 <- lm(chal_vote ~ bs(perot, knots=seq(0,1, length=11)[-c(1,11)]), 
	data=jacob)

mods.aic <- c(AIC(mod.bs2),AIC(mod.bs3),AIC(mod.bs4),AIC(mod.bs5),
AIC(mod.bs6),AIC(mod.bs7),AIC(mod.bs8),AIC(mod.bs9))
library(stats4)
mods.bic <- c(BIC(mod.bs2),BIC(mod.bs3),BIC(mod.bs4),BIC(mod.bs5),
BIC(mod.bs6),BIC(mod.bs7),BIC(mod.bs8),BIC(mod.bs9))
tab <- cbind(mods.aic, mods.bic)
colnames(tab) <- c("AIC", "BIC")
rownames(tab) <- 2:9
library(xtable)
xtable(tab)


###################################################
### chunk number 12: aicbicfig
###################################################
par(mar=c(5,4,4,4))
plot(2:9, mods.aic, xlab="# Knots", ylab="AIC", type="l")
par(new=T)
plot(2:9, mods.bic, axes=F, xlab="", ylab="", main="", type="l", lty=2)
axis(4)
mtext("BIC", 4, 3)


###################################################
### chunk number 13: predbs1
###################################################
preds <- list()
preds[[1]] <- predict(mod.bs2, newdata=data.frame(perot=xp))
preds[[2]] <- predict(mod.bs3, newdata=data.frame(perot=xp))
preds[[3]] <- predict(mod.bs4, newdata=data.frame(perot=xp))
preds[[4]] <- predict(mod.bs5, newdata=data.frame(perot=xp))
preds[[5]] <- predict(mod.bs6, newdata=data.frame(perot=xp))
preds[[6]] <- predict(mod.bs7, newdata=data.frame(perot=xp))
preds[[7]] <- predict(mod.bs8, newdata=data.frame(perot=xp))
preds[[8]] <- predict(mod.bs9, newdata=data.frame(perot=xp))

with(jacob, plot(perot, chal_vote, pch=".", cex=1.75, ylab="Challengers' 
	Vote Share (%)", xlab="Vote for Perot"))
text(with(par(), mean(usr[1:2])), with(par(), mean(usr[3:4])), 
	"2", cex=10, col="gray75")
lines(xp, preds[[1]])


###################################################
### chunk number 14: predbs2
###################################################
with(jacob, plot(perot, chal_vote, pch=".", cex=1.75, ylab="Challengers' 
	Vote Share (%)", xlab="Vote for Perot"))
text(with(par(), mean(usr[1:2])), with(par(), mean(usr[3:4])), 
	"3", cex=10, col="gray75")
lines(xp, preds[[2]])


###################################################
### chunk number 15: predbs3
###################################################
with(jacob, plot(perot, chal_vote, pch=".", cex=1.75, ylab="Challengers' 
	Vote Share (%)", xlab="Vote for Perot"))
text(with(par(), mean(usr[1:2])), with(par(), mean(usr[3:4])), 
	"4", cex=10, col="gray75")
lines(xp, preds[[3]])


###################################################
### chunk number 16: predbs4
###################################################
with(jacob, plot(perot, chal_vote, pch=".", cex=1.75, ylab="Challengers' 
	Vote Share (%)", xlab="Vote for Perot"))
text(with(par(), mean(usr[1:2])), with(par(), mean(usr[3:4])), 
	"5", cex=10, col="gray75")

lines(xp, preds[[4]])


###################################################
### chunk number 17: predbs5
###################################################
with(jacob, plot(perot, chal_vote, pch=".", cex=1.75, ylab="Challengers' 
	Vote Share (%)", xlab="Vote for Perot"))
text(with(par(), mean(usr[1:2])), with(par(), mean(usr[3:4])), 
	"6", cex=10, col="gray75")
lines(xp, preds[[5]])


###################################################
### chunk number 18: predbs6
###################################################
with(jacob, plot(perot, chal_vote, pch=".", cex=1.75, ylab="Challengers' 
	Vote Share (%)", xlab="Vote for Perot"))
text(with(par(), mean(usr[1:2])), with(par(), mean(usr[3:4])), 
	"7", cex=10, col="gray75")
lines(xp, preds[[6]])


###################################################
### chunk number 19: predbs7
###################################################
with(jacob, plot(perot, chal_vote, pch=".", cex=1.75, ylab="Challengers' 
	Vote Share (%)", xlab="Vote for Perot"))
text(with(par(), mean(usr[1:2])), with(par(), mean(usr[3:4])), 
	"8", cex=10, col="gray75")
lines(xp, preds[[7]])


###################################################
### chunk number 20: predbs8
###################################################
with(jacob, plot(perot, chal_vote, pch=".", cex=1.75, ylab="Challengers' 
	Vote Share (%)", xlab="Vote for Perot"))
text(with(par(), mean(usr[1:2])), with(par(), mean(usr[3:4])), 
	"9", cex=10, col="gray75")
lines(xp, preds[[8]])


