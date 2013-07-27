###################################################
### chunk number 1: dev.copy png eval=FALSE
###################################################
## #line 94 "Lecture4_models"
## plot(1:10, col="red", pch=19)
## dev.copy(png, file="test.png")
## dev.off()


###################################################
### chunk number 2: dev.copy pdf eval=FALSE
###################################################
## #line 99 "Lecture4_models"
## plot(1:10, col="red", pch=19)
## dev.copy(pdf, file="test.pdf")
## dev.off()


###################################################
### chunk number 3: Create pdf eval=FALSE
###################################################
## #line 108 "Lecture4_models"
## pdf(file="myplot.pdf")
## plot(1:10, col="blue", xlab="X axis", ylab="Y axis")
## dev.off()  


###################################################
### chunk number 4: scatterplot3d
###################################################
#line 126 "Lecture4_models"
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
### chunk number 5: Example_t.test
###################################################
#line 199 "Lecture4_models"
data(ChickWeight)
ChickWeight[1:2,]
t.test(ChickWeight[,1], mu=100)


###################################################
### chunk number 6: Example_two_sample_t.test
###################################################
#line 207 "Lecture4_models"
t.test(ChickWeight$weight[ChickWeight$Diet=="1"], 
      ChickWeight$weight[ChickWeight$Diet=="2"])
t.test(weight~Diet, data=ChickWeight, subset=Diet%in%c("1","2"))


###################################################
### chunk number 7: Pairwise t
###################################################
#line 215 "Lecture4_models"
pairwise.t.test(ChickWeight$weight, ChickWeight$Diet, p.adjust.method="bonferroni")


###################################################
### chunk number 8: Example lm
###################################################
#line 222 "Lecture4_models"
lmDiet<-lm(weight~Diet, data=ChickWeight)
lmDiet
summary(lmDiet)
anova(lmDiet)


###################################################
### chunk number 9: aov
###################################################
#line 231 "Lecture4_models"
aov(weight~Diet, data=ChickWeight)


###################################################
### chunk number 10: More lm
###################################################
#line 237 "Lecture4_models"
lmDiet<-lm(weight~Diet+Time, data=ChickWeight)
summary(lmDiet)
anova(lmDiet)


###################################################
### chunk number 11: contable
###################################################
#line 264 "Lecture4_models"
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
### chunk number 12: stres
###################################################
#line 289 "Lecture4_models"
str(res)


###################################################
### chunk number 13: assocstatsres
###################################################
#line 295 "Lecture4_models"
## Pearson chi squared test
print(res$chisq_tests[2, ])

## Cramer's V statistic
print(res$cramer)


###################################################
### chunk number 14: kappa
###################################################
#line 318 "Lecture4_models"
## two random classification
set.seed(12345)
c1 <- sample(0:1, 100, replace=TRUE)
c2 <- sample(0:1, 100, replace=TRUE)
tab <- table("C1"=c1, "C2"=c2)
Kappa(x=tab, weights=matrix(rep(1,4),ncol=2))


###################################################
### chunk number 15: agreement
###################################################
#line 329 "Lecture4_models"
agr <- sum(diag(tab)) / sum(tab)
cat(sprintf("Agreement: %.2g%%\n",agr))


###################################################
### chunk number 16: Example Linear Regression
###################################################
#line 451 "Lecture4_models"
library(MASS)
help("cats")
str(cats)
cats.lmB <- lm(Hwt~Bwt, data=cats)
cats.lmS <- lm(Hwt~Sex, data=cats)
cats.lmBS <- lm(Hwt~Bwt + Sex, data=cats)
cats.lmBxS <- lm(Hwt~Bwt*Sex, data=cats)
cats.lmB2 <- lm(Hwt~Bwt + I(Bwt^2), data=cats)


###################################################
### chunk number 17: contrasts
###################################################
#line 492 "Lecture4_models"
contr.treatment(n=3,base=2)
contr.sum(n=3)


###################################################
### chunk number 18: regrorigin
###################################################
#line 502 "Lecture4_models"
# What is the difference in output?
lm(Hwt~Sex, data=cats)
lm(Hwt~Sex-1, data=cats)


###################################################
### chunk number 19: model.matrix eval=FALSE
###################################################
## #line 510 "Lecture4_models"
## mod1<-lm(Hwt~Sex, data=cats)
## model.matrix(mod1)
## mod1<-lm(Hwt~Sex-1, data=cats)
## model.matrix(mod1)


###################################################
### chunk number 20: ExtractorFunctions
###################################################
#line 545 "Lecture4_models"
attach(cats)
cats.lmBS <- lm(Hwt ~ Bwt + Sex, data=cats)
coef(cats.lmBS)
fit.catsBS <- fitted(cats.lmBS)


###################################################
### chunk number 21: Extractor Functions:Sex Specific Fit
###################################################
#line 553 "Lecture4_models"
cats.lmBxS <- lm(Hwt~Bwt*Sex, data=cats)
fit.catsBxS <- fitted(cats.lmBxS)


###################################################
### chunk number 22: plotModel
###################################################
#line 559 "Lecture4_models"
plot(Bwt, Hwt)
lines(Bwt, fit.catsBS, col='green', lwd=2) # OR
#abline(cats.lmBS, col='green', lwd=2)
lines(Bwt[Sex=='F'],fit.catsBxS[Sex=='F'], col='red')
lines(Bwt[Sex=='M'],fit.catsBxS[Sex=='M'], col='blue')
legend(x=2, y=20, legend=c("LMBS","lmBxS.female","lmBxS.male"), col=c("green","red","blue"), lwd=c(2,1,1))


###################################################
### chunk number 23: Extractor Functions:Prediction
###################################################
#line 569 "Lecture4_models"
predict(cats.lmBxS, data.frame(Bwt=seq(2,5,1), Sex='M'))
detach(cats)


###################################################
### chunk number 24: Redidual Variance
###################################################
#line 583 "Lecture4_models"
var.catsB <- deviance(cats.lmB)/df.residual(cats.lmB)  # direct calculation
var.catsB <- summary(cats.lmB)$sigma^2


###################################################
### chunk number 25: fit eval=FALSE
###################################################
## #line 628 "Lecture4_models"
## attach(cats)
## plot(Bwt, Hwt, main="Model fit")
## abline(cats.lmB, col='green', lwd=2)


###################################################
### chunk number 26: hist eval=FALSE
###################################################
## #line 635 "Lecture4_models"
## hist(resid(cats.lmB), main="Residual histogram")


###################################################
### chunk number 27: residuals v fitted eval=FALSE
###################################################
## #line 640 "Lecture4_models"
## plot(fitted(cats.lmB), resid(cats.lmB), main="Residuals vs. fitted values")
## lines(lowess(fitted(cats.lmB),resid(cats.lmB)), col='red')
## abline(h=0)


###################################################
### chunk number 28: qq-plot eval=FALSE
###################################################
## #line 647 "Lecture4_models"
## qqnorm(resid(cats.lmB))
## qqline(resid(cats.lmB))
## detach()


###################################################
### chunk number 29: dofitPlot
###################################################
#line 653 "Lecture4_models"
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
### chunk number 30: plotResultslm
###################################################
#line 671 "Lecture4_models"
par(mfrow=c(2,2))
plot(cats.lmB, which=1:4, id.n=5)


###################################################
### chunk number 31: anova
###################################################
#line 687 "Lecture4_models"
anova(cats.lmB, cats.lmBS)
anova(cats.lmB, cats.lmBxS)
anova(cats.lmB, cats.lmBS, cats.lmBxS)


###################################################
### chunk number 32: mtcars example
###################################################
#line 704 "Lecture4_models"
help("mtcars")
cars.lm <- lm(mpg ~ hp + wt, data=mtcars)
cars.lm2 <- update(cars.lm, . ~ . + disp)
#cars.lms <- update(cars.lm2, sqrt(.) ~ .)


###################################################
### chunk number 33: mtcars anova
###################################################
#line 712 "Lecture4_models"
#anova(cars.lm, cars.lm2, cars.lms)
anova(cars.lm, cars.lm2)


###################################################
### chunk number 34: crossval
###################################################
#line 779 "Lecture4_models"
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
### chunk number 35: chicks lm
###################################################
#line 848 "Lecture4_models"
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
### chunk number 36: glm
###################################################
#line 930 "Lecture4_models"
summary(esoph)


###################################################
### chunk number 37: glm1
###################################################
#line 935 "Lecture4_models"
eso.age <- glm(cbind(ncases, ncontrols) ~ agegp,
                  data = esoph, family = binomial())


###################################################
### chunk number 38: glm-oneway eval=FALSE
###################################################
## #line 940 "Lecture4_models"
## eso.base <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp + alcgp,
##                   data = esoph, family = binomial())


###################################################
### chunk number 39: glm2 eval=FALSE
###################################################
## #line 946 "Lecture4_models"
## eso.base <- update(eso.age, .~ . + tobgp + alcgp)
## 
## eso.TA <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp*alcgp,
##                   data = esoph, family = binomial())
## eso.2way <- glm(cbind(ncases, ncontrols) ~ (agegp + tobgp + alcgp)^2,
##                   data = esoph, family = binomial())


###################################################
### chunk number 40: glm Step
###################################################
#line 956 "Lecture4_models"
eso.base <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp + alcgp,
                  data = esoph, family = binomial())
eso.stp <- stepAIC(eso.age,
          scope=list(upper= ~ agegp + tobgp + alcgp, lower = ~ 1),
          test='Chisq')


###################################################
### chunk number 41: glmPlotfitted
###################################################
#line 967 "Lecture4_models"
attach(esoph)
eso.pred.age <- predict.glm(eso.base,
                 data.frame(agegp=agegp,
                            tobgp=rep('30+', 88),
                            alcgp=rep('40-79', 88)),
                           type="response")
plot(agegp, eso.pred.age)
detach(esoph)


###################################################
### chunk number 42: glmPlotRes
###################################################
#line 982 "Lecture4_models"
opar <- par(mfrow=c(2,2))
for (i in c('deviance', 'working', 'pearson', 'response'))
 plot(resid(eso.base, type=i), ylab=i, pch=19, col="red")
mtext('Different types of residuals',line=-2, outer=T, cex=1.2)
par(opar)


###################################################
### chunk number 43: glmPoisson eval=FALSE
###################################################
## #line 998 "Lecture4_models"
## ins.pois <- glm(Claims ~ District + Group + Age + offset(log(Holders)),
##         data = Insurance, family = poisson)
## summary(ins.pois)


###################################################
### chunk number 44: loadlibrary
###################################################
#line 1030 "Lecture4_models"
search()
library(survival)


###################################################
### chunk number 45: surv
###################################################
#line 1037 "Lecture4_models"
data(leukemia)
head(leukemia)
?Surv
mysurv <- Surv(leukemia$time, leukemia$status)
head(mysurv)


###################################################
### chunk number 46: survival
###################################################
#line 1069 "Lecture4_models"
leuk.km <- survfit(Surv(time, status) ~ x, data=leukemia)
plot(leuk.km, lty=1, col=c("darkblue","darkred"))
legend(100, 1, legend=c('Maintain', 'Non-main'), lty=1:2, col=c("darkblue","darkred"))


###################################################
### chunk number 47: Survival_conf
###################################################
#line 1076 "Lecture4_models"
leuk.km2 <- survfit(Surv(time, status) ~ x,
                   data=leukemia,
                   conf.type='log-log')
summary(leuk.km2)
plot(leuk.km2, mark.time=FALSE, conf.int=TRUE, lty=1, col=c("darkblue","darkred"))
legend(100, 1, legend=c('Maintain', 'Non-main'), lty=1:2, col=c("darkblue","darkred"))


###################################################
### chunk number 48: survivalTest
###################################################
#line 1086 "Lecture4_models"
survdiff(Surv(time, status) ~ x,
                   data=leukemia)


###################################################
### chunk number 49: CoxProp
###################################################
#line 1133 "Lecture4_models"
leuk.ph <- coxph(Surv(time, status) ~ x, data=leukemia)
summary(leuk.ph)
#plot(leuk.km2, mark.time=F, lty=1:2)
#lines(survfit(leuk.ph), lty=1:2, lwd=2)


