# Swiss Fertility Example - Multiple Linear Regression

data(swiss)
names(swiss)
plot(swiss)
summary(swiss)

hist(swiss$Infant.Mortality)
qqnorm(swiss$Infant.Mortality)
qqline(swiss$Infant.Mortality)

plot(swiss$Infant.Mortality~swiss$Fertility, main="IMR by Fertility in Switzerland", xlab="Fertility Rate", ylab="Infant Mortality Rate", xlim=c(30, 100), ylim=c(10,30))
abline(lm(swiss$Infant.Mortality~swiss$Fertility))

fert1<-lm(Infant.Mortality ~ Fertility + Education + Agriculture, data=swiss)
summary(fert1)
anova(fert1)

swiss$cathcat <- ifelse(swiss$Catholic > 60, c(1), c(0))
swiss$cathfact<- ifelse(swiss$Catholic > 60, c("PrimCath"), c("PrimOther"))

fert2<-lm(Infant.Mortality ~ cathcat, data=swiss)
summary(fert2)
fert3<-lm(Infant.Mortality ~ cathfact, data=swiss)
summary(fert3)

IMR_other<-swiss$Infant.Mortality[swiss$cathcat==0]
FR_other<-swiss$Fertility[swiss$cathcat==0]
IMR_cath<-swiss$Infant.Mortality[swiss$cathcat==1]
FR_cath<-swiss$Fertility[swiss$cathcat==1]
plot(IMR_other~FR_other, type="p", pch=20, col="darkred",ylim=c(10,30),xlim=c(30,100), ylab="Infant Mortality Rate", xlab="Fertility Rate")
points(FR_cath, IMR_cath, pch=22, col="darkblue")
abline(lm(IMR_other~FR_other), col="darkred")
abline(lm(IMR_cath~FR_cath), col="darkblue")
legend(30, 30, c("Other", "Catholic"), pch=c(20, 22), cex=.8, col=c("darkred", "darkblue"))

fert4<-lm(Infant.Mortality~Fertility + Catholic + Fertility:cathcat, data=swiss)
summary(fert4)

fert5<-lm(Infant.Mortality~Fertility + Education, data=swiss)
summary(fert5)

mean(fert5$residuals)
hist(fert5$residuals, xlab="Residuals", main="Histogram of residuals")
qqnorm(fert5$residuals, main="Normal Probability Plot", pch=19)
qqline(fert5$residuals)
plot(swiss$Fertility, fert5$residuals, main="Residuals vs. Predictor", xlab="Fertility Rate", ylab="Residuals", pch=19)
abline(h=0)
plot(fert5$fitted.values, fert5$residuals, main="Residuals vs. Fitted", xlab="Fitted values", ylab="Residuals", pch=19)
abline(h=0)
plot(fert5$residuals, main="Residuals", ylab="Residuals", pch=19)
abline(h=0) 
cd<-cooks.distance(fert5)
plot(cd, ylab="Cook's Distance")
abline(h=qf(c(.2,.5),2,44))

opar<-par(mfrow=c(2,2))
plot(fert5, which=1:4)

anova(fert1,fert5)

# Titanic Passenger Survival Example - Logistic Regression

titan<-read.table("D:/RShortcourse/titanicpassengers.txt",sep=",",header=T, na=".")
titan2 <- na.omit(titan)

log1<-glm(titan2$Survived~titan2$Age,family=binomial("logit"))
summary(log1)
plot(titan2$Age~fitted(glm(titan2$Survived~titan2$Age,binomial)), xlab="Age", ylab="P(Survival)", pch=15)
plot(titan2$Age~fitted(log1), xlab="Age", ylab="P(Survival)", pch=15)

titan$agecat4 <- ifelse(titan$Age>35, c(1),c(0))
titan$agecat3 <- ifelse(titan$Age>=18 & titan$Age<=35, c(1),c(0))
titan$agecat2 <- ifelse(titan$Age>=6 & titan$Age<=17, c(1),c(0))
titan$agecat1 <- ifelse(titan$Age<6, c(1),c(0))
titan2 <- na.omit(titan)

log2<-glm(titan2$Survived~titan2$agecat1 + titan2$agecat2 + titan2$agecat3, binomial)
summary(log2)
exp(cbind(OR=coef(log2),confint(log2)))

log3<-glm(titan$Survived~titan$Sex + titan$PClass + titan$Sex:titan$PClass,binomial)
summary(log3)
exp(cbind(OR=coef(log3),confint(log3)))
anova(log3,test="Chisq")

install.packages("arm")
library(arm)
x<-predict(log3)
y<-resid(log3)
binnedplot(x,y)

# Cebu Fertility Example - Poisson Regression

ceb<-read.table("D:/RShortcourse/ceb.dat",header=T)

ceb1<-glm(y ~ educ + res, offset=log(n), family = "poisson", data=ceb)
summary(ceb1)
ceb1$deviance/ceb1$df.residual
pchisq(2646.5, 64, lower=F)

ceb1<-glm(y~educ, family="poisson", offset=log(n), data=ceb)
ceb2<-glm(y~educ+res, family="poisson", offset=log(n), data=ceb)
1-pchisq(deviance(ceb1)-deviance(ceb2), df.residual(ceb1)-df.residual(ceb2))

ceb2<-glm(y~educ+res, family="quasipoisson", offset=log(n), data=ceb)
summary(ceb2)

install.packages("MASS")
library(MASS)
ceb.nb <- glm.nb(y~educ+res+offset(log(n)), data=ceb)
summary(ceb.nb)
ceb.nb$deviance/ceb.nb$df.residual

install.packages("pscl")
library(pscl)
ceb.zip <- zeroinfl(y~educ+res, offset=log(n), data=ceb)




