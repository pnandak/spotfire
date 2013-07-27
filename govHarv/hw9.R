##Clayton Nall
##Homework Assignment 9
##December 3, 2007
##nall@fas.harvard.edu

##Load libraries
library(car)
library(MASS)
##PROBLEM 1


##Download NES data
setwd("~/Gov2000TF")
load("ANES2000.RData")
library(car)
library(MASS)
TI<-read.csv("Treisman98TI.csv", header=T)

ti.out<-lm(TI98~commonlaw+britcolony+noncolony+pctprot+elf+FMMexports93,
          data=TI)
pdf(file="hw9slplot.pdf")
plot(sqrt(abs(studres(ti.out)))~fitted(ti.out), xlab="Fitted Values",
     ylab="Sqrt(Abs. Studentized Residuals)")
lines(lowess(abs(studres(ti.out))~fitted(ti.out)))
dev.off()

##Here we write a function for calculating the White SEs
white.se <-  function(lmobj, weighted=FALSE){
  X<-model.matrix(lmobj)
  n<-nrow(X)
  df.resid<-df.residual(lmobj)
  white.var<-
  solve(t(X)%*%X)%*%
  t(X)%*%
  diag((resid(lmobj))^2)%*%
  X%*%
  solve(t(X)%*%X)
  hc0.se<-sqrt(diag(white.var))
  hc1.se<-sqrt(n/df.resid)*hc0.se
  return(list(hc0.se, hc1.se))
}

white.se(ti.out) ##OLS
##Replicates fine using "hc1" method.


##Now--weighted least squares with White SEs.
##First get coefficients for WLS, disregarding SEs.
ti.wtd<-lm(ti.out$model, data=TI, weights=1/TI98variance)
summary(ti.wtd)

##Now create the design matrix.
D<-data.frame(TI98=response(ti.out), model.matrix(ti.out))
TI98.weights<-1/TI$TI98variance[as.numeric(rownames(D))]

X<-model.matrix(ti.wtd)
y<-response(ti.wtd)
##Take the sqrt of the weights when we solve using the method from
##lecture slides.
X.wtd<-X*sqrt(weights(ti.wtd))
##Why do we need to take the square root of the weights?
y.wtd<-y*sqrt(weights(ti.wtd))

lm.wt<-lm(y.wtd~0+X.wtd)
white.se(lm.wt)
white.se(ti.wtd) ## This replicates the results in Treisman
                 ## using the "hc1" method


## Problem 2
data(Prestige)
pdf(file="fox12.9.pdf")
par(mfrow=c(2,3))
lm.pres<-lm(prestige~income+education+women, data=Prestige)

lm.pres.trans<-lm(prestige~log(income)+education+women, data=Prestige)
cr.plots(lm.pres)
2
3
4
0
cr.plots(lm.pres.trans)
2
3
4
0
dev.off()

Prestige$profincome<-as.numeric(Prestige$type=="prof")*Prestige$income
Prestige$wcincome<-as.numeric(Prestige$type=="wc")*Prestige$income

Prestige$profeducation<-as.numeric(Prestige$type=="prof")*Prestige$education
Prestige$wceducation<-as.numeric(Prestige$type=="wc")*Prestige$education

Prestige$profwomen<-as.numeric(Prestige$type=="prof")*Prestige$women
Prestige$wcwomen<-as.numeric(Prestige$type=="wc")*Prestige$women


lm.alt<-lm(prestige~type+income+education+profincome+wcincome+profeducation+wceducation+profwomen+wcwomen,
           data=Prestige)

plot(prestige~income, pch="", data=Prestige)
points(prestige~income, data=Prestige[Prestige$type=="bc",], pch=19, cex=1.2)
points(prestige~income, data=Prestige[Prestige$type=="wc",], pch=5, cex=1.2)
points(prestige~income, data=Prestige[Prestige$type=="prof",], pch=10,
       cex=1.2)
abline(lm(prestige~income, data=Prestige[Prestige$type=="prof",]),
       lty=3, lwd=2)
abline(lm(prestige~income, data=Prestige[Prestige$type=="bc",]),
       lty=1, lwd=2)
abline(lm(prestige~income, data=Prestige[Prestige$type=="wc",]),
       lty=2, lwd=2)
legend(locator(), legend=c("Blue Collar", "White Collar",
                    "Professional"), pch=c(19,5,10), lty=c(1,2,3),
       lwd=c(2,2,2), cex=c(1.2,1.2,1.2))

dev.print(device=pdf, file="typeincome.pdf")

##Problem3
##Chile
data(Chile)
lm.chil<-lm(statusquo~age+education+income, data=Chile)
lm.fix<-lm(statusquo~population+age+education+income+region,
           data=Chile)
pdf("residualboxplots.pdf")

dev.off()
Chile$region[rownames(model.frame(lm.chil))]
pdf("residbox.pdf")
par(mfrow=c(1,2), las=2)
plot(residuals(lm.chil)~Chile$region[as.numeric(rownames(model.frame(lm.chil)))],
     xlab="Region", ylab="Residuals", ylim=c(-2,2), main="No Fixed
Effects", names=c("Central", "Metro", "North", "South", "Santiago"))
plot(residuals(lm.fix)~Chile$region[as.numeric(rownames(model.frame(lm.fix)))],
     xlab="Region", ylab="Residuals", ylim=c(-2,2),main="Fixed
Effects", names=c("Central", "Metro", "North", "South", "Santiago"))
dev.off()

##Problem 4

data(SLID)
library(mgcv)
lm.slid<-lm(wages~age+education+sex, data=SLID)
G<-gam(wages~s(age)+s(education)+sex, data=SLID)
summary(G)
par(pty="s")
pdf(file="gamplots.pdf")
plot.gam(G, pages=1, pty="s", rug=TRUE)
dev.off()

## Looks like we'll try cubing education and squaring age.
SLID$edcube<-SLID$education^3
SLID$edsq<-SLID$education^2
SLID$agesq<-SLID$age^2
lm.trans<-lm(wages~agesq+age+edcube+edsq+education+sex, data=SLID)
summary(lm.trans)
##Use CR plots to test for linearity
pdf(file="CRplotstransform.pdf")
cr.plots(lm.trans, one.page=TRUE)
2
3
4
5
6
0
dev.off()

## Using GAM plots might be even better.
## Be sure to include the spline on both the higher and lower-order
## terms
g.trans<-gam(wages~s(agesq)+s(age)+s(edcube)+s(edsq)+s(education)+sex,
             data=SLID)
pdf("gamtransform.pdf")
plot.gam(g.trans, pages=1, rug=TRUE)
dev.off()
