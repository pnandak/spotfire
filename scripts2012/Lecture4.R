### R code from vignette source 'C:/Users/aedin/Dropbox/Talks/Bio503/winter2012/L4/Lecture4.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: dev.copy png (eval = FALSE)
###################################################
## plot(1:10, col="red", pch=19)
## dev.copy(png, file="test.png")
## dev.off()


###################################################
### code chunk number 2: dev.copy pdf (eval = FALSE)
###################################################
## plot(1:10, col="red", pch=19)
## dev.copy(pdf, file="test.pdf")
## dev.off()


###################################################
### code chunk number 3: Create pdf (eval = FALSE)
###################################################
## pdf(file="myplot.pdf")
## plot(1:10, col="blue", xlab="X axis", ylab="Y axis")
## dev.off()  


###################################################
### code chunk number 4: scatterplot3d
###################################################
require(scatterplot3d)
data(trees)
trees[1:2,]
s3d <- scatterplot3d(trees, type="h", highlight.3d=TRUE, 
                    angle=55, scale.y=0.7, pch=16, 
                    main="Example of scatterplot3d plot: Tree Data")

# Now adding some points to the "scatterplot3d"
s3d$points3d(seq(10,20,2), seq(85,60,-5), 
            seq(60,10,-10),  col="blue", 
            type="h", pch=16)        

# Now adding a regression plane to the "scatterplot3d"
attach(trees)
my.lm <- lm(Volume ~ Girth + Height)
s3d$plane3d(my.lm)
detach(trees)


###################################################
### code chunk number 5: venn (eval = FALSE)
###################################################
## install.package(gtools)
## install.package(gplots)


###################################################
### code chunk number 6: venn
###################################################
library(gplots)
n=20
for (i in 3:5) {
    myList<-lapply(1:i, sample, x=LETTERS, size=n)
    print(paste("Plotting venn of", length(myList), "sets"))
    venn(myList)
   }


###################################################
### code chunk number 7: Example_t.test
###################################################
data(ChickWeight)
ChickWeight[1:2,]
t.test(ChickWeight[,1], mu=100)


###################################################
### code chunk number 8: Example_two_sample_t.test
###################################################
t.test(ChickWeight$weight[ChickWeight$Diet=="1"], 
      ChickWeight$weight[ChickWeight$Diet=="2"])
t.test(weight~Diet, data=ChickWeight, subset=Diet%in%c("1","2"))


###################################################
### code chunk number 9: Pairwise t
###################################################
pairwise.t.test(ChickWeight$weight, ChickWeight$Diet, p.adjust.method="bonferroni")


###################################################
### code chunk number 10: Example lm
###################################################
lmDiet<-lm(weight~Diet, data=ChickWeight)
lmDiet
summary(lmDiet)
anova(lmDiet)


###################################################
### code chunk number 11: aov
###################################################
aov(weight~Diet, data=ChickWeight)


###################################################
### code chunk number 12: More lm
###################################################
lmDiet<-lm(weight~Diet+Time, data=ChickWeight)
summary(lmDiet)
anova(lmDiet)


###################################################
### code chunk number 13: contable
###################################################
## load library
library(vcd)

## load data
attach(Arthritis)

## check the variables of interest
is.factor(Arthritis$Treatment)
print(levels(Arthritis$Treatment))
is.factor(Arthritis$Improved)
print(levels(Arthritis$Improved))

## build the contingency table
tab <- table(Arthritis$Treatment, Arthritis$Improved)
print(tab)

## compute statistics
res <- assocstats(tab)
print(res)
detach(Arthritis)


###################################################
### code chunk number 14: stres
###################################################
str(res)


###################################################
### code chunk number 15: assocstatsres
###################################################
## Pearson chi squared test
print(res$chisq_tests[2, ])

## Cramer's V statistic
print(res$cramer)


###################################################
### code chunk number 16: kappa
###################################################
## two random classification
set.seed(12345)
c1 <- sample(0:1, 100, replace=TRUE)
c2 <- sample(0:1, 100, replace=TRUE)
tab <- table("C1"=c1, "C2"=c2)
Kappa(x=tab, weights=matrix(rep(1,4),ncol=2))


###################################################
### code chunk number 17: agreement
###################################################
agr <- sum(diag(tab)) / sum(tab)
cat(sprintf("Agreement: %.2g%%\n",agr))


###################################################
### code chunk number 18: Example Linear Regression
###################################################
library(MASS)
help("cats")
str(cats)
cats.lmB <- lm(Hwt~Bwt, data=cats)
cats.lmS <- lm(Hwt~Sex, data=cats)
cats.lmBS <- lm(Hwt~Bwt + Sex, data=cats)
cats.lmBxS <- lm(Hwt~Bwt*Sex, data=cats)
cats.lmB2 <- lm(Hwt~Bwt + I(Bwt^2), data=cats)


###################################################
### code chunk number 19: contrasts
###################################################
contr.treatment(n=3,base=2)
contr.sum(n=3)


###################################################
### code chunk number 20: regrorigin
###################################################
# What is the difference in output?
lm(Hwt~Sex, data=cats)
lm(Hwt~Sex-1, data=cats)


###################################################
### code chunk number 21: model.matrix (eval = FALSE)
###################################################
## mod1<-lm(Hwt~Sex, data=cats)
## model.matrix(mod1)
## mod1<-lm(Hwt~Sex-1, data=cats)
## model.matrix(mod1)


###################################################
### code chunk number 22: ExtractorFunctions
###################################################
attach(cats)
cats.lmBS <- lm(Hwt ~ Bwt + Sex, data=cats)
coef(cats.lmBS)
fit.catsBS <- fitted(cats.lmBS)


###################################################
### code chunk number 23: Extractor Functions:Sex Specific Fit
###################################################
cats.lmBxS <- lm(Hwt~Bwt*Sex, data=cats)
fit.catsBxS <- fitted(cats.lmBxS)


###################################################
### code chunk number 24: plotModel
###################################################
plot(Bwt, Hwt)
lines(Bwt, fit.catsBS, col='green', lwd=2) # OR
#abline(cats.lmBS, col='green', lwd=2)
lines(Bwt[Sex=='F'],fit.catsBxS[Sex=='F'], col='red')
lines(Bwt[Sex=='M'],fit.catsBxS[Sex=='M'], col='blue')
legend(x=2, y=20, legend=c("LMBS","lmBxS.female","lmBxS.male"), col=c("green","red","blue"), lwd=c(2,1,1))


###################################################
### code chunk number 25: Extractor Functions:Prediction
###################################################
predict(cats.lmBxS, data.frame(Bwt=seq(2,5,1), Sex='M'))
detach(cats)


###################################################
### code chunk number 26: Redidual Variance
###################################################
var.catsB <- deviance(cats.lmB)/df.residual(cats.lmB)  # direct calculation
var.catsB <- summary(cats.lmB)$sigma^2


###################################################
### code chunk number 27: fit (eval = FALSE)
###################################################
## attach(cats)
## plot(Bwt, Hwt, main="Model fit")
## abline(cats.lmB, col='green', lwd=2)


###################################################
### code chunk number 28: hist (eval = FALSE)
###################################################
## hist(resid(cats.lmB), main="Residual histogram")


###################################################
### code chunk number 29: residuals v fitted (eval = FALSE)
###################################################
## plot(fitted(cats.lmB), resid(cats.lmB), main="Residuals vs. fitted values")
## lines(lowess(fitted(cats.lmB),resid(cats.lmB)), col='red')
## abline(h=0)


###################################################
### code chunk number 30: qq-plot (eval = FALSE)
###################################################
## qqnorm(resid(cats.lmB))
## qqline(resid(cats.lmB))
## detach()


###################################################
### code chunk number 31: dofitPlot
###################################################
attach(cats)
par(mfrow=c(2,2))
plot(Bwt, Hwt, main="Model fit")
abline(cats.lmB, col='green', lwd=2)

hist(resid(cats.lmB), main="Residual histogram")

plot(fitted(cats.lmB), resid(cats.lmB), main="Residuals vs. fitted values")
lines(lowess(fitted(cats.lmB),resid(cats.lmB)), col='red')
abline(h=0)

qqnorm(resid(cats.lmB))
qqline(resid(cats.lmB))
detach()


###################################################
### code chunk number 32: plotResultslm
###################################################
par(mfrow=c(2,2))
plot(cats.lmB, which=1:4, id.n=5)


###################################################
### code chunk number 33: anova
###################################################
anova(cats.lmB, cats.lmBS)
anova(cats.lmB, cats.lmBxS)
anova(cats.lmB, cats.lmBS, cats.lmBxS)


###################################################
### code chunk number 34: mtcars example
###################################################
help("mtcars")
cars.lm <- lm(mpg ~ hp + wt, data=mtcars)
cars.lm2 <- update(cars.lm, . ~ . + disp)
#cars.lms <- update(cars.lm2, sqrt(.) ~ .)


###################################################
### code chunk number 35: mtcars anova
###################################################
#anova(cars.lm, cars.lm2, cars.lms)
anova(cars.lm, cars.lm2)


###################################################
### code chunk number 36: crossval
###################################################
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
### code chunk number 37: chicks lm
###################################################
attach(ChickWeight)
time.wgt <- tapply(weight,Time, var)
time.wgt.rep <- as.numeric(time.wgt[match(Time,as.numeric(names(time.wgt)))])
detach(2)
Chick.anl <- data.frame(ChickWeight, time.wgt.rep=time.wgt.rep)
chick.lm.wgt <- lm(weight~Time,
                  data=Chick.anl,
                  weight=1/time.wgt.rep)
chick.lm.T0 <- lm(weight~Time,
                  data=Chick.anl,
                  subset=(Time==0))


###################################################
### code chunk number 38: glm
###################################################
summary(esoph)


###################################################
### code chunk number 39: glm1
###################################################
eso.age <- glm(cbind(ncases, ncontrols) ~ agegp,
                  data = esoph, family = binomial())


###################################################
### code chunk number 40: glm-oneway (eval = FALSE)
###################################################
## eso.base <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp + alcgp,
##                   data = esoph, family = binomial())


###################################################
### code chunk number 41: glm2 (eval = FALSE)
###################################################
## eso.base <- update(eso.age, .~ . + tobgp + alcgp)
## 
## eso.TA <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp*alcgp,
##                   data = esoph, family = binomial())
## eso.2way <- glm(cbind(ncases, ncontrols) ~ (agegp + tobgp + alcgp)^2,
##                   data = esoph, family = binomial())


###################################################
### code chunk number 42: glm Step
###################################################
eso.base <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp + alcgp,
                  data = esoph, family = binomial())
eso.stp <- stepAIC(eso.age,
          scope=list(upper= ~ agegp + tobgp + alcgp, lower = ~ 1),
          test='Chisq')


###################################################
### code chunk number 43: glmPlotfitted
###################################################
attach(esoph)
eso.pred.age <- predict.glm(eso.base,
                 data.frame(agegp=agegp,
                            tobgp=rep('30+', 88),
                            alcgp=rep('40-79', 88)),
                           type="response")
plot(agegp, eso.pred.age)
detach(esoph)


###################################################
### code chunk number 44: glmPlotRes
###################################################
opar <- par(mfrow=c(2,2))
for (i in c('deviance', 'working', 'pearson', 'response'))
 plot(resid(eso.base, type=i), ylab=i, pch=19, col="red")
mtext('Different types of residuals',line=-2, outer=T, cex=1.2)
par(opar)


###################################################
### code chunk number 45: glmPoisson (eval = FALSE)
###################################################
## ins.pois <- glm(Claims ~ District + Group + Age + offset(log(Holders)),
##         data = Insurance, family = poisson)
## summary(ins.pois)


###################################################
### code chunk number 46: loadlibrary
###################################################
search()
library(survival)


###################################################
### code chunk number 47: surv
###################################################
data(leukemia)
head(leukemia)
?Surv
mysurv <- Surv(leukemia$time, leukemia$status)
head(mysurv)


###################################################
### code chunk number 48: survival
###################################################
leuk.km <- survfit(Surv(time, status) ~ x, data=leukemia)
plot(leuk.km, lty=1, col=c("darkblue","darkred"))
legend(100, 1, legend=c('Maintain', 'Non-main'), lty=1:2, col=c("darkblue","darkred"))


###################################################
### code chunk number 49: Survival_conf
###################################################
leuk.km2 <- survfit(Surv(time, status) ~ x,
                   data=leukemia,
                   conf.type='log-log')
summary(leuk.km2)
plot(leuk.km2, mark.time=FALSE, conf.int=TRUE, lty=1, col=c("darkblue","darkred"))
legend(100, 1, legend=c('Maintain', 'Non-main'), lty=1:2, col=c("darkblue","darkred"))


###################################################
### code chunk number 50: survivalTest
###################################################
survdiff(Surv(time, status) ~ x,
                   data=leukemia)


###################################################
### code chunk number 51: CoxProp
###################################################
leuk.ph <- coxph(Surv(time, status) ~ x, data=leukemia)
summary(leuk.ph)
#plot(leuk.km2, mark.time=F, lty=1:2)
#lines(survfit(leuk.ph), lty=1:2, lwd=2)


