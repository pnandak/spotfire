###################################################
### chunk number 1: A eval=FALSE
###################################################
## #line 64 "L5-1"
## solar.radiation <- c(11.1, 10.6 ,  6.3 ,  8.8 ,  10.7 ,  11.2 ,  8.9 ,  12.2)


###################################################
### chunk number 2: B eval=FALSE
###################################################
## #line 69 "L5-1"
## myFn<-function(x) {
## 	results<-c(mean=mean(x), median=median(x), var=var(x))
## 	print(results)
## 	}
## myFn(solar.radiation)
## summary(solar.radiation)


###################################################
### chunk number 3: C eval=FALSE
###################################################
## #line 82 "L5-1"
## sr10 <- solar.radiation + 10
## myFn(sr10)


###################################################
### chunk number 4: D eval=FALSE
###################################################
## #line 88 "L5-1"
## par(mfrow=c(2,2))
## 
## myFn<-function(x, myTitle=NULL, myColor="red") {
## 	results<-c(mean=mean(x), median=median(x), var=var(x))
## 	print(results)
## 	hist(x,  main=myTitle, col=myColor)
## 	}
## 
## myFn(solar.radiation, myTitle="solar.radiation", myColor="magenta")
## myFn(sr10, myTitle="sr10", myColor="gold")


###################################################
### chunk number 5: E eval=FALSE
###################################################
## #line 103 "L5-1"
## myFn<-function(x, myTitle=NULL,yLab=NULL, myColor="red") {
## 	results<-c(mean=mean(x), median=median(x), var=var(x))
## 	print(results)
## 	hist(x,  main=myTitle, col=myColor)
## 	plot(x,  main=myTitle, col=myColor, ylab=yLab)
## 	}
## 
## myFn(solar.radiation, myTitle="solar radiation data", myColor="magenta", yLab="solar radiation observed")


###################################################
### chunk number 6: F eval=FALSE
###################################################
## #line 116 "L5-1"
## date <- 1:8 
## 
## myFn<-function(x, y, myTitle=NULL,yLab=NULL,xLab=NULL, myColor="red", abline=TRUE) {
## 	results<-c(mean=mean(x), median=median(x), var=var(x))
## 	print(results)
## 	hist(x,  main=myTitle, col=myColor)
## 	plot(y, x,  main=myTitle, col=myColor, ylab=yLab, xlab=xLab)
## 	lmObj <- lm(x ~ y)
## 	abline(lmObj, lwd=3, col="green", lty=2)
## 	}
## 
## myFn(solar.radiation, date,  myTitle="solar radiation data", yLab="solar radiation observed", xLab="date",myColor="magenta")


###################################################
### chunk number 7: lmcats
###################################################
#line 160 "L5-1"
library(MASS)
## dataset cats
str(cats)
cats.lmB <- lm(Hwt~Bwt, data=cats)
cats.lmS <- lm(Hwt~Sex, data=cats)
cats.lmBS <- lm(Hwt~Bwt + Sex, data=cats)


###################################################
### chunk number 8: crossval
###################################################
#line 181 "L5-1"
nfold <- 10
## nr is the number of observations
nr <- nrow(mtcars)
## nfold is the number of folds in the cross-validation
if(nfold > 1) k <- floor(nr/nfold) else {
	k <- 1
	nfold <- nr
}
smpl <- sample(nr)
mse.big <- mse.small <- NULL
    
for (i in 1:nfold) {
        if (i == nfold) s.ix <- smpl[c(((i - 1) * k + 1):nr)] else s.ix <- smpl[c(((i - 1) * k + 1):(i * k))]
	## fit the model
	mm.big <- lm(mpg ~ ., data=mtcars[-s.ix, , drop=FALSE])
	mm.small <- lm(mpg ~ wt, data=mtcars[-s.ix, , drop=FALSE]) 
	## assess the performance of the model
	pp.big <- predict(object=mm.big, newdata=mtcars[s.ix, !is.element(colnames(mtcars), "mpg")])
	pp.small <- predict(object=mm.small, newdata=mtcars[s.ix, !is.element(colnames(mtcars), "mpg")])
	## compute mean squared error (MSE)
	mse.big <- c(mse.big, sqrt(mean((mtcars[s.ix, "mpg"] - pp.big)^2)))
	mse.small <- c(mse.small, sqrt(mean((mtcars[s.ix, "mpg"] - pp.small)^2)))
}
names(mse.big) <- names(mse.small) <- paste("fold", 1:nfold, sep=".")

## compare the performance of the big and small models using a Wilcoxon Rank Sum test
wilcox.test(mse.big, mse.small, paired=TRUE, alternative="less")


###################################################
### chunk number 9: glm
###################################################
#line 253 "L5-1"
library(vcd)
str(Arthritis)


###################################################
### chunk number 10: glm1
###################################################
#line 260 "L5-1"
## transform the health status in a binary variable
Arthritis2 <- Arthritis
Arthritis2[ ,"Improved"] <- ifelse(Arthritis[ ,"Improved"] == "None", 0, 1)
Arthritis2 <- Arthritis2[sample(1:nrow(Arthritis2)), ]
logitm <- glm(Improved ~ Age + Sex + Treatment,
                  data = Arthritis2, family = binomial(link=logit))


###################################################
### chunk number 11: glm1summary
###################################################
#line 271 "L5-1"
summary(logitm)


###################################################
### chunk number 12: logitm2 eval=FALSE
###################################################
## #line 277 "L5-1"
## logitm2 <- glm(Improved ~ Treatment,
##                   data = Arthritis2, family = binomial(link=logit))
## summary(logitm2)
## summary(logitm2)$aic


###################################################
### chunk number 13: glmPlotRes
###################################################
#line 288 "L5-1"
opar <- par(mfrow=c(2,2))
for (i in c('deviance', 'working', 'pearson', 'response'))
 plot(resid(logitm, type=i), ylab=i, pch=19, col="red")
mtext('Different types of residuals',line=-2, outer=T, cex=1.2)
par(opar)


###################################################
### chunk number 14: logitmpredictnew
###################################################
#line 297 "L5-1"
predict(logitm, newdata=data.frame(Treatment="Placebo", Sex="Female", Age=32), type="response")
predict(logitm, newdata=data.frame(Treatment="Treated", Sex="Male", Age=70), type="response")


###################################################
### chunk number 15: logitmfitted eval=FALSE
###################################################
## #line 304 "L5-1"
## boxplot(logitm$fitted ~ Arthritis2[ ,"Improved"], main="Response vs fitted value", xlab="Response ('Improved')", ylab="Prediction (probability of improved health)")


###################################################
### chunk number 16: logitmfitted eval=FALSE
###################################################
## #line 310 "L5-1"
## boxplot(predict(logitm, newdata=Arthritis2, type="response") ~ Arthritis2[ ,"Improved"], main="Response vs fitted value", xlab="Response ('Improved')", ylab="Prediction (probability of improved health)")


###################################################
### chunk number 17: loadlibrary
###################################################
#line 319 "L5-1"
search()
library(survival)


###################################################
### chunk number 18: surv
###################################################
#line 326 "L5-1"
data(leukemia)
head(leukemia)
help(Surv)
mysurv <- Surv(leukemia$time, leukemia$status)
head(mysurv)


###################################################
### chunk number 19: survival
###################################################
#line 358 "L5-1"
leuk.km <- survfit(Surv(time, status) ~ x, data=leukemia)
plot(leuk.km, lty=1, col=c("darkblue","darkred"))
legend(100, 1, legend=c('Maintain', 'Non-main'), lty=1:2, col=c("darkblue","darkred"))


###################################################
### chunk number 20: Survival_conf
###################################################
#line 365 "L5-1"
leuk.km2 <- survfit(Surv(time, status) ~ x,
                   data=leukemia,
                   conf.type='log-log')
summary(leuk.km2)
plot(leuk.km2, mark.time=FALSE, conf.int=TRUE, lty=1, col=c("darkblue","darkred"))
legend(100, 1, legend=c('Maintain', 'Non-main'), lty=1:2, col=c("darkblue","darkred"))


###################################################
### chunk number 21: survivalTest
###################################################
#line 375 "L5-1"
survdiff(Surv(time, status) ~ x,
                   data=leukemia)


###################################################
### chunk number 22: survivalkm eval=FALSE
###################################################
## #line 382 "L5-1"
## colon.km <- survfit(Surv(time, status) ~ rx, data=colon)
## plot(colon.km, lty=1, col=c("darkblue", "darkgreen", "darkred"))
## legend(2500, 1, legend=c("Obs", "Lev", "Lev+5FU"), lty=1, col=c("darkblue", "darkgreen", "darkred"))
## ## logrank test
## survdiff(formula = Surv(time, status) ~ rx, data = colon)


###################################################
### chunk number 23: CoxProp
###################################################
#line 394 "L5-1"
leuk.ph <- coxph(Surv(time, status) ~ x, data=leukemia)
summary(leuk.ph)


###################################################
### chunk number 24: survivalcoxph eval=FALSE
###################################################
## #line 403 "L5-1"
## summary(coxph(Surv(time, status) ~ ., data=colon))


###################################################
### chunk number 25: image eval=FALSE
###################################################
## #line 426 "L5-1"
## save.image("L5.RData")
## savehistory("L5.Rhistory")
## dir()
## dir(pattern="RData")
## load("L5.RData")
## loadhistory("L5.Rhistory")


###################################################
### chunk number 26: system.time
###################################################
#line 532 "L5-1"
df<-matrix(rnorm(5000000), ncol=20000)
system.time(apply(df,1,mean))
system.time(rowMeans(df))


###################################################
### chunk number 27: pkgseleton eval=FALSE
###################################################
## #line 556 "L5-1"
## package.skeleton(name="myFirstRPackage")


###################################################
### chunk number 28: Rprofile eval=FALSE
###################################################
## #line 576 "L5-1"
## sessionInfo()
## search()
## q()


###################################################
### chunk number 29: Rprofile
###################################################
#line 598 "L5-1"
sessionInfo()
search()


###################################################
### chunk number 30: installed
###################################################
#line 618 "L5-1"
mypkg<-installed.packages()
dim(mypkg)
summary(mypkg)


###################################################
### chunk number 31: pkg eval=FALSE
###################################################
## #line 629 "L5-1"
## packageStatus()


###################################################
### chunk number 32: pkgmac
###################################################
#line 633 "L5-1"
packageStatus(repositories="http://software.rc.fas.harvard.edu/mirrors/R/bin/macosx/leopard/contrib/2.12")


###################################################
### chunk number 33: class
###################################################
#line 698 "L5-1"
a<- c(1:10)
class(a)
is.data.frame(a)
is.matrix(a)
is.character(a)
a<-as.factor(a)
class(a)
is.character(a)
is.factor(a)
class(as.character(a))


###################################################
### chunk number 34: grep
###################################################
#line 721 "L5-1"
grep("A", LETTERS)
grep("A", LETTERS, value=TRUE)


###################################################
### chunk number 35: sub
###################################################
#line 728 "L5-1"
sub("B", "A", LETTERS)


###################################################
### chunk number 36: strsplit
###################################################
#line 734 "L5-1"
a<-date()
strsplit(a, " ")
strsplit(a, "J")
b<-strsplit(a, "11")
class(b)
b<-unlist(b)
class(b)


###################################################
### chunk number 37: strsplit
###################################################
#line 746 "L5-1"
a<-"aedin@jimmy.harvard.edu"
strsplit(a, "\\.")


###################################################
### chunk number 38: NA
###################################################
#line 754 "L5-1"
a<-c(1:10, NA, NA)
a<-c(1:10, NA, NA)
summary(a)
mean(a)


