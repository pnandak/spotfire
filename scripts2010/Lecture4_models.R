###################################################
### chunk number 1: dev.copy png eval=FALSE
###################################################
## plot(1:10, col="red", pch=19)
## dev.copy(png, file="test.png")
## dev.off()


###################################################
### chunk number 2: dev.copy pdf eval=FALSE
###################################################
## plot(1:10, col="red", pch=19)
## dev.copy(pdf, file="test.pdf")
## dev.off()


###################################################
### chunk number 3: Create pdf eval=FALSE
###################################################
## pdf(file="myplot.pdf")
## plot(1:10, col="blue", xlab="X axis", ylab="Y axis")
## dev.off()  


###################################################
### chunk number 4: scatterplot3d
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
### chunk number 5: Example_t.test
###################################################
data(ChickWeight)
ChickWeight[1:2,]
t.test(ChickWeight[,1], mu=100)


###################################################
### chunk number 6: Example_two_sample_t.test
###################################################
t.test(ChickWeight$weight[ChickWeight$Diet=="1"], 
       ChickWeight$weight[ChickWeight$Diet=="2"])
t.test(weight~Diet, data=ChickWeight, subset=Diet%in%c("1","2"))


###################################################
### chunk number 7: Pairwise t
###################################################
pairwise.t.test(ChickWeight$weight, ChickWeight$Diet)


###################################################
### chunk number 8: Example lm
###################################################
lmDiet<-lm(weight~Diet, data=ChickWeight)
lmDiet
summary(lmDiet)
anova(lmDiet)


###################################################
### chunk number 9: aov
###################################################
aov(weight~Diet, data=ChickWeight)


###################################################
### chunk number 10: More lm
###################################################
lmDiet<-lm(weight~Diet+Time, data=ChickWeight)
summary(lmDiet)
anova(lmDiet)


###################################################
### chunk number 11: Example Linear Regression
###################################################
library(MASS)
help("cats")
cats.lmB <- lm(Hwt~Bwt, data=cats)
cats.lmS <- lm(Hwt~Sex, data=cats)
cats.lmBS <- lm(Hwt~Bwt + Sex, data=cats)
cats.lmBxS <- lm(Hwt~Bwt*Sex, data=cats)
cats.lmB2 <- lm(Hwt~Bwt + I(Bwt^2), data=cats)


###################################################
### chunk number 12: contrasts
###################################################
contr.treatment(3,base=2)
contr.sum(3)
# What is the difference in output?
lm(Hwt~Sex, data=cats)
lm(Hwt~Sex-1, data=cats)


###################################################
### chunk number 13: model.matrix
###################################################
mod1<-lm(Hwt~Sex, data=cats)
model.matrix(mod1)
mod1<-lm(Hwt~Sex-1, data=cats)
model.matrix(mod1)


###################################################
### chunk number 14: Extractor Functions
###################################################
summary(cats.lmB)
coef(cats.lmBxS)
fit.catsB <- fitted(cats.lmB)


###################################################
### chunk number 15: Extractor Functions:Sex Specific Fit
###################################################
fit.catsBxS <- fitted(cats.lmBxS)


###################################################
### chunk number 16: plotModel
###################################################
attach(cats)
plot(Bwt, Hwt)
lines(Bwt, fit.catsB) # OR
abline(cats.lmB, col='green', lwd=2)
lines(Bwt[Sex=='F'],fit.catsBxS[Sex=='F'], col='red')
lines(Bwt[Sex=='M'],fit.catsBxS[Sex=='M'], col='blue')


###################################################
### chunk number 17: Extractor Functions:Prediction
###################################################
predict(cats.lmBxS, data.frame(Bwt=seq(2,5,1), Sex='M'))
detach(cats)


###################################################
### chunk number 18: Redidual Variance
###################################################
var.catsB <- deviance(cats.lmB)/df.residual(cats.lmB)  # direct calculation
var.catsB <- summary(cats.lmB)$sigma^2


###################################################
### chunk number 19: fit eval=FALSE
###################################################
## attach(cats)
## plot(Bwt, Hwt)
## abline(cats.lmB, col='green', lwd=2)


###################################################
### chunk number 20: hist eval=FALSE
###################################################
## hist(resid(cats.lmB))


###################################################
### chunk number 21: residuals v fitted eval=FALSE
###################################################
## plot(fitted(cats.lmB), resid(cats.lmB))
## lines(lowess(fitted(cats.lmB),resid(cats.lmB)), col='red')
## abline(h=0)


###################################################
### chunk number 22: qq-plot eval=FALSE
###################################################
## qqnorm(resid(cats.lmB))
## qqline(resid(cats.lmB))
## detach()


###################################################
### chunk number 23: dofitPlot
###################################################
attach(cats)
par(mfrow=c(2,2))
plot(Bwt, Hwt)
abline(cats.lmB, col='green', lwd=2)
hist(resid(cats.lmB))

plot(fitted(cats.lmB), resid(cats.lmB))
lines(lowess(fitted(cats.lmB),resid(cats.lmB)), col='red')
abline(h=0)

qqnorm(resid(cats.lmB))
qqline(resid(cats.lmB))
detach()


###################################################
### chunk number 24: plotResultslm
###################################################
par(mfrow=c(2,2))
plot(cats.lmB, which=1:4, id.n=5)


###################################################
### chunk number 25: anova
###################################################
anova(cats.lmB, cats.lmBS)
anova(cats.lmB, cats.lmBxS)
anova(cats.lmB, cats.lmBS, cats.lmBxS)


###################################################
### chunk number 26: mtcars example
###################################################
help("mtcars")
cars.lm <- lm(mpg ~ hp + wt, data=mtcars)
cars.lm2 <- update(cars.lm, . ~ . + disp)
cars.lms <- update(cars.lm2, sqrt(.) ~ .)


###################################################
### chunk number 27: mtcars anova
###################################################
anova(cars.lm, cars.lm2, cars.lms)


###################################################
### chunk number 28: chicks lm
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
### chunk number 29: glm
###################################################
summary(esoph)


###################################################
### chunk number 30: glm1
###################################################
eso.age <- glm(cbind(ncases, ncontrols) ~ agegp,
                   data = esoph, family = binomial())


###################################################
### chunk number 31: glm-oneway
###################################################
eso.base <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp + alcgp,
                   data = esoph, family = binomial())


###################################################
### chunk number 32: glm2
###################################################
eso.base <- update(eso.age, .~ . + tobgp + alcgp)

eso.TA <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp*alcgp,
                   data = esoph, family = binomial())
eso.2way <- glm(cbind(ncases, ncontrols) ~ (agegp + tobgp + alcgp)^2,
                   data = esoph, family = binomial())


###################################################
### chunk number 33: glm Step
###################################################
eso.stp <- stepAIC(eso.age,
           scope=list(upper= ~(agegp + tobgp + alcgp)^2, lower = ~ 1),
           test='Chisq')


###################################################
### chunk number 34: glmPlotfitted
###################################################
attach(esoph)
eso.pred.age <- predict.glm(eso.base,
                  data.frame(agegp=agegp,
                             tobgp=rep('30+', 88),
                             alcgp=rep('40-79', 88)),
                            type="response")
plot(agegp, eso.pred.age)
detach()


###################################################
### chunk number 35: glmPlotRes
###################################################
opar <- par(mfrow=c(2,2))
for (i in c('deviance', 'working', 'pearson', 'response'))
  plot(resid(eso.base, type=i), ylab=i, pch=19, col="red")
mtext('Different types of residuals',line=-2, outer=T, cex=1.2)
par(opar)


###################################################
### chunk number 36: glmPoisson
###################################################
ins.pois <- glm(Claims ~ District + Group + Age + offset(log(Holders)),
         data = Insurance, family = poisson)
summary(ins.pois)


###################################################
### chunk number 37: loadlibrary
###################################################
search()
library(survival)


###################################################
### chunk number 38: survival
###################################################
data(leukemia)
leukemia[1:2,]
leuk.km <- survfit(Surv(time, status) ~ x, data=leukemia)
plot(leuk.km, lty=1:2, col=c("blue","red"))


###################################################
### chunk number 39: Survival_conf
###################################################
leuk.km2 <- survfit(Surv(time, status) ~ x,
                    data=leukemia,
                    conf.type='log-log')
summary(leuk.km2)
plot(leuk.km2, mark.time=F, conf.int=T, lty=1:2, col=c("blue","red"))
legend(100, 1, legend=c('Maintain', 'Non-main'), lty=1:2, col=c("blue","red"))


###################################################
### chunk number 40: survivalTest
###################################################
survdiff(Surv(time, status) ~ x,
                    data=leukemia)


###################################################
### chunk number 41: CoxProp
###################################################
leuk.ph <- coxph(Surv(time, status) ~ x, data=leukemia)
summary(leuk.ph)
plot(leuk.km2, mark.time=F, lty=1:2)
lines(survfit(leuk.ph), lty=1:2, lwd=2)


