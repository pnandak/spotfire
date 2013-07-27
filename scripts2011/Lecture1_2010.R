#######################
## Regression III    ##
## Lecture 1         ##
## Summer 2010       ##
##                   ##
## author:           ##
## Dave Armtrong     ## 
## UW-Milwaukee      ##
## armstrod@uwm.edu  ##
#######################

###################################################
### chunk number 1: ICV1
###################################################
library(apsrtable)
setwd("~/desktop/ICPSR_slides/Lecture 1/")
wvs<-read.table('Weakliem.txt', header=T) 
with(wvs, plot(secpay ~ gini, cex=1.5, xlab="GINI", ylab="Attitudes toward Inequality (mean)"))
outs <- which(rownames(wvs)%in% c("Slovakia","CzechRepublic","Chile"))
with(wvs, text(gini[outs], secpay[outs], rownames(wvs)[outs], pos=c(2,4,4)))
abline(lm(secpay ~ gini, data=wvs), lty=2, col="red", lwd=1.5)
with(wvs, lines(lowess(gini, secpay, f=.5), col="red", lwd=1.5))
legend("topright", c("Linear Model","LOWESS"), lty=c(2,1), col=c("red","red"), xjust=1, inset=.01)


###################################################
### chunk number 2: fig1
###################################################
library(apsrtable)
setwd("~/desktop/ICPSR_slides/Lecture 1/")
wvs<-read.table('Weakliem.txt', header=T) 
with(wvs, plot(secpay ~ gini, cex=1.5, xlab="GINI", ylab="Attitudes toward Inequality (mean)"))
outs <- which(rownames(wvs)%in% c("Slovakia","CzechRepublic","Chile"))
with(wvs, text(gini[outs], secpay[outs], rownames(wvs)[outs], pos=c(2,4,4)))
abline(lm(secpay ~ gini, data=wvs), lty=2, col="red", lwd=1.5)
with(wvs, lines(lowess(gini, secpay, f=.5), col="red", lwd=1.5))
legend("topright", c("Linear Model","LOWESS"), lty=c(2,1), col=c("red","red"), xjust=1, inset=.01)


###################################################
### chunk number 3: ICVmod1
###################################################
mod <- lm(secpay ~ gini, data=wvs)
apsrtable(mod, model.names="", Sweave=TRUE)


###################################################
### chunk number 4: ICV2
###################################################
with(wvs, plot(secpay ~ gini, cex=1.5, xlab="GINI", ylab="Attitudes toward Inequality (mean)", type="n"))
with(wvs, points(secpay[democrat == 0] ~ gini[democrat == 0], col="red", pch=1, cex=1.5))
with(wvs, points(secpay[democrat == 1] ~ gini[democrat == 1], col="blue", pch=2, cex=1))
with(wvs, text(gini[outs], secpay[outs], rownames(wvs)[outs], pos=c(2,4,4)))
abline(lm(secpay ~ gini, data=wvs, subset=democrat==0), col="red", lty=2, lwd=1.5)
abline(lm(secpay ~ gini, data=wvs, subset=democrat==1), col="blue", lty=2, lwd=1.5)
legend("topright", c("Non-Democracy","Democracy"), pch=c(1,2), col=c("red","blue"), xjust=1, inset=.01)


###################################################
### chunk number 5: fig2
###################################################
with(wvs, plot(secpay ~ gini, cex=1.5, xlab="GINI", ylab="Attitudes toward Inequality (mean)", type="n"))
with(wvs, points(secpay[democrat == 0] ~ gini[democrat == 0], col="red", pch=1, cex=1.5))
with(wvs, points(secpay[democrat == 1] ~ gini[democrat == 1], col="blue", pch=2, cex=1))
with(wvs, text(gini[outs], secpay[outs], rownames(wvs)[outs], pos=c(2,4,4)))
abline(lm(secpay ~ gini, data=wvs, subset=democrat==0), col="red", lty=2, lwd=1.5)
abline(lm(secpay ~ gini, data=wvs, subset=democrat==1), col="blue", lty=2, lwd=1.5)
legend("topright", c("Non-Democracy","Democracy"), pch=c(1,2), col=c("red","blue"), xjust=1, inset=.01)


###################################################
### chunk number 6: ICVmod2
###################################################
mod2 <- lm(secpay ~ gini*democrat, data=wvs)
apsrtable(mod2, model.names="", Sweave=TRUE)


###################################################
### chunk number 7: ICV3
###################################################
with(wvs[-which(rownames(wvs) %in% c("CzechRepublic","Slovakia")),], plot(secpay ~ gini, cex=1.5, xlab="GINI", ylab="Attitudes toward Inequality (mean)", type="n"))
with(wvs[-which(rownames(wvs) %in% c("CzechRepublic","Slovakia")),], points(secpay[democrat == 0] ~ gini[democrat == 0], col="red", pch=1, cex=1.5))
with(wvs[-which(rownames(wvs) %in% c("CzechRepublic","Slovakia")),], points(secpay[democrat == 1] ~ gini[democrat == 1], col="blue", pch=2, cex=1))
abline(lm(secpay ~ gini, data=wvs[-which(rownames(wvs) %in% c("CzechRepublic","Slovakia")),], subset=democrat==0), col="red", lty=2, lwd=1.5)
abline(lm(secpay ~ gini, data=wvs[-which(rownames(wvs) %in% c("CzechRepublic","Slovakia")),], subset=democrat==1), col="blue", lty=2, lwd=1.5)
legend("topright", c("Non-Democracy","Democracy"), pch=c(1,2), col=c("red","blue"), xjust=1, inset=.01)


###################################################
### chunk number 8: fig3
###################################################
with(wvs[-which(rownames(wvs) %in% c("CzechRepublic","Slovakia")),], plot(secpay ~ gini, cex=1.5, xlab="GINI", ylab="Attitudes toward Inequality (mean)", type="n"))
with(wvs[-which(rownames(wvs) %in% c("CzechRepublic","Slovakia")),], points(secpay[democrat == 0] ~ gini[democrat == 0], col="red", pch=1, cex=1.5))
with(wvs[-which(rownames(wvs) %in% c("CzechRepublic","Slovakia")),], points(secpay[democrat == 1] ~ gini[democrat == 1], col="blue", pch=2, cex=1))
abline(lm(secpay ~ gini, data=wvs[-which(rownames(wvs) %in% c("CzechRepublic","Slovakia")),], subset=democrat==0), col="red", lty=2, lwd=1.5)
abline(lm(secpay ~ gini, data=wvs[-which(rownames(wvs) %in% c("CzechRepublic","Slovakia")),], subset=democrat==1), col="blue", lty=2, lwd=1.5)
legend("topright", c("Non-Democracy","Democracy"), pch=c(1,2), col=c("red","blue"), xjust=1, inset=.01)


###################################################
### chunk number 9: ICVmod3
###################################################
mod3 <- lm(secpay ~ gini*democrat, data=wvs[-which(rownames(wvs) %in% c("CzechRepublic","Slovakia")),])
apsrtable(mod3, model.names="", Sweave=TRUE)


###################################################
### chunk number 10: nl1
###################################################
set.seed(123)
y <- seq(from=1,to=100, length=100)
x <- log(y) + rnorm(100, 0, 0.4)
plot(x ~ y, axes=F, xlab="x", ylab="y", cex=1.5)
box()
lines(y, log(y))


###################################################
### chunk number 11: nonlin1
###################################################
set.seed(123)
y <- seq(from=1,to=100, length=100)
x <- log(y) + rnorm(100, 0, 0.4)
plot(x ~ y, axes=F, xlab="x", ylab="y", cex=1.5)
box()
lines(y, log(y))


###################################################
### chunk number 12: nl2
###################################################
set.seed(123)
x <- seq(from=-100,to=100, length=100)
y <- x + 2*x^2 + 3*x^3 
plot(y + rnorm(100, 0, 250000) ~ x, axes=F, xlab="x", ylab="y", cex=1.5)
box()
lines(y ~ x, lwd=2)


###################################################
### chunk number 13: nonlin2
###################################################
set.seed(123)
x <- seq(from=-100,to=100, length=100)
y <- x + 2*x^2 + 3*x^3 
plot(y + rnorm(100, 0, 250000) ~ x, axes=F, xlab="x", ylab="y", cex=1.5)
box()
lines(y ~ x, lwd=2)


###################################################
### chunk number 14: nl3
###################################################
set.seed(123)
x <- seq(from=-100,to=100, length=100)
y <- 20 + 50*x + x^2
plot(y+rnorm(100, 0, 1000) ~ x, axes=F, xlab="x", ylab="y", cex=1.5)
box()
lines(y ~ x, lwd=2)


###################################################
### chunk number 15: nonlin3
###################################################
set.seed(123)
x <- seq(from=-100,to=100, length=100)
y <- 20 + 50*x + x^2
plot(y+rnorm(100, 0, 1000) ~ x, axes=F, xlab="x", ylab="y", cex=1.5)
box()
lines(y ~ x, lwd=2)


###################################################
### chunk number 16: regspline
###################################################
plot(c(0,10), c(0, 10), type="n", axes=F, xlab="", ylab="")
segments(0,0,0,10)
segments(0,0,10,0)
text(0.25, 9,"Election", pos=4, cex=2)
text(9, 9,"Election", pos=2, cex=2)
segments(9,8.5,6,4, lwd=2)
segments(0,8.5,6,4, lwd=2)
mtext(expression(X[0]), 1, at=6, cex=1.5)
mtext("Public Support", 2, at=5, cex=1.5)
mtext("Time X", 1, at=9.5, cex=1.5)
segments(6,1, 6,4, lty=3)
text(6,.5,"Campaign Begins", cex=1.5)
mtext("0", 1, at=0, line=0, cex=1.25)
text(6,3.8,"knot", pos=4, cex=1.25)


###################################################
### chunk number 17: splines1
###################################################
plot(c(0,10), c(0, 10), type="n", axes=F, xlab="", ylab="")
segments(0,0,0,10)
segments(0,0,10,0)
text(0.25, 9,"Election", pos=4, cex=2)
text(9, 9,"Election", pos=2, cex=2)
segments(9,8.5,6,4, lwd=2)
segments(0,8.5,6,4, lwd=2)
mtext(expression(X[0]), 1, at=6, cex=1.5)
mtext("Public Support", 2, at=5, cex=1.5)
mtext("Time X", 1, at=9.5, cex=1.5)
segments(6,1, 6,4, lty=3)
text(6,.5,"Campaign Begins", cex=1.5)
mtext("0", 1, at=0, line=0, cex=1.25)
text(6,3.8,"knot", pos=4, cex=1.25)


###################################################
### chunk number 18: lowfig
###################################################
prestige <- read.table("prestige.txt", header=T)
with(prestige, plot(prestige ~ income, cex=1.5, xlab ="Pineo-Porter Prestige Score", ylab="Average Income"))
with(prestige, lines(lowess(income, prestige, f=.7), col="red"))
with(prestige, lines(lowess(income, prestige, f=.2), col="blue"))
abline(lm(prestige ~ income, data=prestige))
legend("bottomright", c("Linear","Lowess (f=.7)","Lowess (f=.2)"), lty=c(1,2,3), 
    lwd=c(1.5,1.5,1.5), col=c("black","red","blue"), inset=.01)


###################################################
### chunk number 19: lowess1
###################################################
prestige <- read.table("prestige.txt", header=T)
with(prestige, plot(prestige ~ income, cex=1.5, xlab ="Pineo-Porter Prestige Score", ylab="Average Income"))
with(prestige, lines(lowess(income, prestige, f=.7), col="red"))
with(prestige, lines(lowess(income, prestige, f=.2), col="blue"))
abline(lm(prestige ~ income, data=prestige))
legend("bottomright", c("Linear","Lowess (f=.7)","Lowess (f=.2)"), lty=c(1,2,3), 
    lwd=c(1.5,1.5,1.5), col=c("black","red","blue"), inset=.01)


###################################################
### chunk number 20: smsp1
###################################################
plot(wvs$secpay ~ wvs$gini, cex=1.5, xlab="Gini", ylab="Secpay")
lmod1 <- lm(secpay ~ gini, data=wvs)
abline(lmod1)
lines(lowess(wvs$gini, wvs$secpay, f=.8), lwd=2, col="red")
mod1 <- with(wvs, smooth.spline(x=gini, y=secpay, df=5))
pred.seq <- wvs$gini[order(wvs$gini)]
mod1.fit <- predict(mod1, newdata=data.frame(gini=pred.seq))
lines(mod1.fit$y ~ mod1.fit$x, lwd=2, col="green")
legend("topright", c("Linear","Lowess","Smoothing Spline"), 
    col=c("black","red","green"), lty=c(1,1,1), xjust=1, lwd=c(2,2,2), inset=.01)


###################################################
### chunk number 21: lmsmoothlowess1
###################################################
plot(wvs$secpay ~ wvs$gini, cex=1.5, xlab="Gini", ylab="Secpay")
lmod1 <- lm(secpay ~ gini, data=wvs)
abline(lmod1)
lines(lowess(wvs$gini, wvs$secpay, f=.8), lwd=2, col="red")
mod1 <- with(wvs, smooth.spline(x=gini, y=secpay, df=5))
pred.seq <- wvs$gini[order(wvs$gini)]
mod1.fit <- predict(mod1, newdata=data.frame(gini=pred.seq))
lines(mod1.fit$y ~ mod1.fit$x, lwd=2, col="green")
legend("topright", c("Linear","Lowess","Smoothing Spline"), 
    col=c("black","red","green"), lty=c(1,1,1), xjust=1, lwd=c(2,2,2), inset=.01)


###################################################
### chunk number 22: multnp
###################################################
Model.lowess<-loess(secpay~gini+gdp, data=wvs, span=.8, degree=1)
gini2<-seq(min(wvs$gini), max(wvs$gini), len=15)
gdp2<-seq(min(wvs$gdp), max(wvs$gdp), len=15)
data<-expand.grid(gini=gini2, gdp=gdp2)
secpay.fitted<-matrix(predict(Model.lowess, data), 15, 15)
persp(gini2, gdp2, secpay.fitted, theta=35, ph=20, ticktype='detailed',
  xlab='Gini Coefficient', ylab='GDP', zlab='Attitudes', shade=.5)


###################################################
### chunk number 23: multnpreg1
###################################################
Model.lowess<-loess(secpay~gini+gdp, data=wvs, span=.8, degree=1)
gini2<-seq(min(wvs$gini), max(wvs$gini), len=15)
gdp2<-seq(min(wvs$gdp), max(wvs$gdp), len=15)
data<-expand.grid(gini=gini2, gdp=gdp2)
secpay.fitted<-matrix(predict(Model.lowess, data), 15, 15)
persp(gini2, gdp2, secpay.fitted, theta=35, ph=20, ticktype='detailed',
  xlab='Gini Coefficient', ylab='GDP', zlab='Attitudes', shade=.5)


###################################################
### chunk number 24: gdpf
###################################################
mod1 <- gam(secpay ~ s(gini) + s(gdp), data=wvs)
gdp.dat <- with(wvs, data.frame(gdp=seq(from=min(gdp), to=max(gdp), length=25), gini=mean(gini, na.rm=T)))
pred.gdp <- predict(mod1, gdp.dat, se.fit=T)
plot.gdp <- cbind(with(gdp.dat, data.frame(gdp=gdp)), with(pred.gdp, data.frame(est=fit, lower=fit-2*se.fit, upper=fit+2*se.fit)))

with(plot.gdp, plot(gdp,est, type="l", ylim=c(min(lower), max(upper)), xlab="GDP", ylab="Predicted Secpay"))
with(plot.gdp, lines(gdp, lower, lty=2))
with(plot.gdp, lines(gdp, upper, lty=2))


###################################################
### chunk number 25: ginif
###################################################
gini.dat <- with(wvs, data.frame(gini=seq(from=min(gini), to=max(gini), length=25), gdp=mean(gdp, na.rm=T)))
pred.gini <- predict(mod1, gini.dat, se.fit=T)
plot.gini <- cbind(with(gini.dat, data.frame(gini=gini)), with(pred.gini, data.frame(est=fit, lower=fit-2*se.fit, upper=fit+2*se.fit)))

with(plot.gini, plot(gini,est, type="l", ylim=c(min(lower), max(upper)), xlab="GINI", ylab="Predicted Secpay"))
with(plot.gini, lines(gini, lower, lty=2))
with(plot.gini, lines(gini, upper, lty=2))


###################################################
### chunk number 26: gdpfig
###################################################
mod1 <- gam(secpay ~ s(gini) + s(gdp), data=wvs)
gdp.dat <- with(wvs, data.frame(gdp=seq(from=min(gdp), to=max(gdp), length=25), gini=mean(gini, na.rm=T)))
pred.gdp <- predict(mod1, gdp.dat, se.fit=T)
plot.gdp <- cbind(with(gdp.dat, data.frame(gdp=gdp)), with(pred.gdp, data.frame(est=fit, lower=fit-2*se.fit, upper=fit+2*se.fit)))

with(plot.gdp, plot(gdp,est, type="l", ylim=c(min(lower), max(upper)), xlab="GDP", ylab="Predicted Secpay"))
with(plot.gdp, lines(gdp, lower, lty=2))
with(plot.gdp, lines(gdp, upper, lty=2))


###################################################
### chunk number 27: ginifig
###################################################
gini.dat <- with(wvs, data.frame(gini=seq(from=min(gini), to=max(gini), length=25), gdp=mean(gdp, na.rm=T)))
pred.gini <- predict(mod1, gini.dat, se.fit=T)
plot.gini <- cbind(with(gini.dat, data.frame(gini=gini)), with(pred.gini, data.frame(est=fit, lower=fit-2*se.fit, upper=fit+2*se.fit)))

with(plot.gini, plot(gini,est, type="l", ylim=c(min(lower), max(upper)), xlab="GINI", ylab="Predicted Secpay"))
with(plot.gini, lines(gini, lower, lty=2))
with(plot.gini, lines(gini, upper, lty=2))


###################################################
### chunk number 28: outmod
###################################################
mod1 <- lm(secpay ~ gini + gdp, data=wvs)


###################################################
### chunk number 29: inflplot1
###################################################
library(car)
influencePlot(mod1, identify="auto")


###################################################
### chunk number 30: davisinf
###################################################
data(Davis)
davis.mod <-lm(repwt~weight, data=Davis) 
with(Davis, plot(repwt ~ weight, xlab="Weight", ylab="Reported Weight"))
abline(davis.mod)


###################################################
### chunk number 31: davisnoinf
###################################################
davis.mod1 <-lm(repwt~weight, data=Davis, subset = -12) 
with(Davis[-12,], plot(repwt ~ weight, xlab="Weight", ylab="Reported Weight"))
abline(davis.mod1)


###################################################
### chunk number 32: davisinffig
###################################################
data(Davis)
davis.mod <-lm(repwt~weight, data=Davis) 
with(Davis, plot(repwt ~ weight, xlab="Weight", ylab="Reported Weight"))
abline(davis.mod)


###################################################
### chunk number 33: Davismod1
###################################################
apsrtable(davis.mod, model.names="", Sweave=TRUE)


###################################################
### chunk number 34: davisnoinffig
###################################################
davis.mod1 <-lm(repwt~weight, data=Davis, subset = -12) 
with(Davis[-12,], plot(repwt ~ weight, xlab="Weight", ylab="Reported Weight"))
abline(davis.mod1)


###################################################
### chunk number 35: Davismod1a
###################################################
apsrtable(davis.mod1, model.names="", Sweave=TRUE)


###################################################
### chunk number 36: huber1
###################################################
require(MASS, quietly=T)
huber.mod <- rlm(repwt~weight, method="MM", data=Davis)
plot(Davis$repwt ~ Davis$weight, xlab="Weight", ylab="Reported Weight")
abline(davis.mod, lwd=2)
abline(huber.mod, col="red", lty=2, lwd=2)
legend("topleft", 
c("OLS","Huber"), col=c("black","red"), lty=c(1,2), inset=.01)


###################################################
### chunk number 37: florida1
###################################################
data(Florida)
mod1 <- lm(BUCHANAN ~ GORE, data=Florida)
mod2 <- lm(BUCHANAN ~ GORE, data=Florida, subset=-which(rownames(Florida) =="PALM.BEACH"))
mod3 <- lm(BUCHANAN ~ GORE, data=Florida, subset=-which(rownames(Florida) %in% c("DADE", "BROWARD")))
mod4 <- lm(BUCHANAN ~ GORE, data=Florida, subset=-which(rownames(Florida) %in% c("PALM.BEACH","DADE", "BROWARD")))
gpb <- which(rownames(Florida) =="PALM.BEACH")
gd <- which(rownames(Florida) %in% c("DADE"))
gb <-  which(rownames(Florida) %in% c("BROWARD"))
plot(BUCHANAN ~ GORE, data=Florida)
with(Florida, text(GORE[gpb], BUCHANAN[gpb], rownames(Florida)[gpb], pos=1))
with(Florida, text(GORE[gb], BUCHANAN[gb], rownames(Florida)[gb], pos=2))
with(Florida, text(GORE[gd], BUCHANAN[gd], rownames(Florida)[gd], pos=1))

# Add the linear model results with different colors and line-types
abline(mod1)
abline(mod2, col="red", lty=2)
abline(mod3, col="blue", lty=3)
abline(mod4, col="green", lty=4)

# Add a legend in the upper-left corner
legend("topleft", c("OLS", "No Palm Beach", "No Dade or Broward", "No PB, Dade or Broward"), 
    col=c("black", "red", "blue", "green"), lty=c(1,2,3,4), inset=.01)


###################################################
### chunk number 38: attdata
###################################################
library(car) # Loads the car "car" package
data(Prestige) # Brings the "Prestige" data to the workspace
attach(Prestige) # attaches the "Prestige" data


###################################################
### chunk number 39: adddata
###################################################
Dept <- c("Soc", "Pol", "Psych", "Econ")
Year <- c(1,2,3,4)
Dataset <- data.frame(cbind(Dept, Year))
Dataset


###################################################
### chunk number 40: read1
###################################################
library(foreign)
GSSdat <- read.spss("gss91.sav", use.value.labels=T, to.data.frame=T)


###################################################
### chunk number 41: read2
###################################################
library(foreign)
GSSdat <- read.dta("gss91.dta", convert.factors=T)


###################################################
### chunk number 42: respec
###################################################
VarA <- c(1,2,3,2,3,4,4,2,3,2,1,1,2,3,2)
VarB <- as.factor(VarA)
VarA
VarB


###################################################
### chunk number 43: respec2
###################################################
VarC <- ordered(VarA, levels=c("1", "2", "3", "4"))
VarC


###################################################
### chunk number 44: recode1
###################################################
library(car)
VarD <- recode(VarA, "1:2=2; 3:4=1")
table(VarD, VarA)


###################################################
### chunk number 45: recode2
###################################################
VarE <- recode(VarA, "1:2='Low'; 3:4='High'")
table(VarE, VarA)


###################################################
### chunk number 46: graphex
###################################################
data(Florida)
with(Florida, plot(BUCHANAN, BUSH, xlab="Buchanan",ylab="Bush"))

with(Florida[which(rownames(Florida) %in% c("DADE", "PALM.BEACH")), ], 
	text(BUCHANAN, BUSH, rownames(Florida[which(rownames(Florida) %in% 
	c("DADE", "PALM.BEACH")), ]), pos=c(1,2)))
	
abline(lm(BUSH~BUCHANAN, data=Florida),
    col="green", lwd=2)
abline(lm(BUSH ~ BUCHANAN, data=Florida,
    subset =BUCHANAN < 3000), col="blue",
    lty=2, lwd=2)
title("Florida votes by county")


###################################################
### chunk number 47: graphexfig
###################################################
data(Florida)
with(Florida, plot(BUCHANAN, BUSH, xlab="Buchanan",ylab="Bush"))

with(Florida[which(rownames(Florida) %in% c("DADE", "PALM.BEACH")), ], 
	text(BUCHANAN, BUSH, rownames(Florida[which(rownames(Florida) %in% 
	c("DADE", "PALM.BEACH")), ]), pos=c(1,2)))
	
abline(lm(BUSH~BUCHANAN, data=Florida),
    col="green", lwd=2)
abline(lm(BUSH ~ BUCHANAN, data=Florida,
    subset =BUCHANAN < 3000), col="blue",
    lty=2, lwd=2)
title("Florida votes by county")


