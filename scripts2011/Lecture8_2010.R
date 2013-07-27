###################################################
### chunk number 1: outfig1
###################################################
weakliem2 <- read.table("weakliem2.txt")
outs <- which(rownames(weakliem2) %in% c("CzechRepublic", "Slovakia"))
plot(secpay ~ gini, data=weakliem2, main="Non-Democracies")
abline(lm(secpay ~ gini, data=weakliem2))
abline(lm(secpay ~ gini, data=weakliem2, subset=-outs), lty=2, col="red")
with(weakliem2, text(gini[outs], secpay[outs], 
	rownames(weakliem2)[outs], pos=4))
legend("topright", c("All Obs", "No Outliers"), 
	lty=c(1,2), col=c("black","red"), inset=.01)


###################################################
### chunk number 2: outmods1
###################################################
library(apsrtable)
mod1 <- lm(secpay ~ gini, data=weakliem2)
mod2 <- lm(secpay ~ gini, data=weakliem2, subset=-outs)
apsrtable(mod1, mod2, Sweave=T, model.names=c("All Obs", "No Outliers"), digits=4)


###################################################
### chunk number 3: davisout
###################################################
library(car)
data(Davis)
plot(weight ~ height, data=Davis)
with(Davis, text(height[12], weight[12], "12", pos=1))
abline(lm(weight ~ height, data=Davis), 
	lty=1, col=1, lwd=2)
abline(lm(weight ~ height, data=Davis, subset=-12), 
	lty=2, col=2, lwd=2)
legend("topright", lty=c(1,2), col=c(1,2), 
	legend=c("All Cases", "Outlier Excluded"), inset=.01)


###################################################
### chunk number 4: davismods
###################################################
mod1 <- lm(weight ~ height, data=Davis)
mod2 <- lm(weight ~ height, data=Davis, subset=-12)
apsrtable(mod1, mod2, Sweave=T, 
	model.names=c("All Obs", "No Outliers"), digits=2)


###################################################
### chunk number 5: simdata1
###################################################
set.seed(123)
x<-c(rnorm(1000,mean=4,sd=1))
x1<-c(x,55)
y<-c(x,5)
range(x1)
range (y)


###################################################
### chunk number 6: simtab
###################################################
mod1 <- lm(y ~ x1)
apsrtable(mod1, model.names="", Sweave=T)


###################################################
### chunk number 7: simplot
###################################################
plot(x1,y)
abline(lm(y ~ x1))


###################################################
### chunk number 8: ineqrevis
###################################################
mod3 <- lm(secpay ~ gini + gdp, data=weakliem2)
apsrtable(mod3, Sweave=T, model.names="")


###################################################
### chunk number 9: hatvals1
###################################################
plot(hatvalues(mod3), xlim=c(0,27), 
    main="Hat Values for Inequality model")
abline(h=c(2,3)*3/nrow(weakliem2), lty=2)
text(x=c(3,5,7,8,26), 
	 y=hatvalues(mod3)[c(3,5,7,8,26)], 
	 rownames(weakliem2)[c(3,5,7,8,26)], 
	 pos=1)


###################################################
### chunk number 10: outtest
###################################################
mod3 <- lm(secpay~gini + gdp, data=weakliem2)
outlier.test(mod3)


###################################################
### chunk number 11: qqp
###################################################
qq.plot(mod3, simulate=T, labels=F)


###################################################
### chunk number 12: dfbeta
###################################################
dfb <- as.data.frame(dfbetas(mod3))
with(dfb, plot(gini, gdp, xlab="DFBeta for GINI", 
	ylab="DFBeta for GDP"))
abline(h=c(-2/sqrt(26), 2/sqrt(26)), 
	v=c(-2/sqrt(26), 2/sqrt(26)), lty=2)


###################################################
### chunk number 13: iddfb
###################################################
cutoff <- 2/sqrt(26)
big <- with(dfb, which(abs(gini) > cutoff | 
	abs(gdp) > cutoff))
dfb[big, ]


###################################################
### chunk number 14: cookd
###################################################
mod3.cook <- cookd(mod3)
plot(cookd(mod3))
cutoff <- with(mod3, 4/df.residual)
abline(h=cutoff, lty=2)
text(which(mod3.cook > cutoff), mod3.cook[which(mod3.cook > cutoff)],
    names(mod3.cook[which(mod3.cook > cutoff)]), pos=c(4,4,2))


###################################################
### chunk number 15: inflplot
###################################################
influencePlot(mod3, identify="auto")


###################################################
### chunk number 16: av1
###################################################
av.plots(mod3, "gini")


###################################################
### chunk number 17: av2
###################################################
av.plots(mod3, "gdp")


