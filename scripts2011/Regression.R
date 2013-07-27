# Multiple linear regression example

boston<-read.csv("F:/RShortcourse/boston.csv",header=T)
names(boston)

hist(MEDV)
qqnorm(MEDV)
qqline(MEDV)

boston$LMEDV<-log(boston$MEDV)

bost<-lm(LMEDV ~ RM + LSTAT + CRIM + ZN + CHAS + DIS, data=boston)
summary(bost)

mean(bost$residuals)
hist(bost$residuals, xlab="residuals", main="Histogram of residuals")
qqnorm(bost$residuals, main="Normal Probability Plot", pch=19)
qqline(bost$residuals)
plot(boston$LSTAT, bost$residuals, main="Residuals vs. Predictor", xlab=“% in Lower Status", ylab="Residuals", pch=19)
abline(h=0)
plot(bost$fitted.values, bost$residuals, main="Residuals vs. Fitted", xlab="Fitted values", ylab="Residuals", pch=19)
abline(h=0)
plot(bost$residuals, main="Residuals", ylab="Residuals", pch=19)
abline(h=0) 
cd=cooks.distance(bost)
plot(cd, ylab="Cook's Distance")
abline(h=qf(c(.2,.5),6, 499))
ic=(1:506)[cd>qf(c(.2,.5), 6,499)]
text(ic,cd[ic], as.character(boston$OBS [ic]),adj=c(1,1))

step(lm(LMEDV~1),LMEDV~LSTAT+RM+CRIM+INDUS+ZN+CHAS+DIS,direction="forward")

# Logistic regression example

bd<-read.csv("F:/RShortcourse/bd.csv",header=T)

bdlog<-glm(bd$casegrp~bd$MAGE,family=binomial("logit"))
bdlog<-glm(bd$casegrp~bd$MAGE,binomial)
summary(bdlog)

plot(MAGE~fitted(glm(casegrp~MAGE,binomial)), xlab=“Maternal Age”, ylab=“P(Birth Defect)”, pch=15)

bd$magecat3 <- ifelse(bd$MAGE>25, c(1),c(0))
bd$magecat2 <- ifelse(bd$MAGE>=20 & bd$MAGE<=25, c(1),c(0))
bd$magecat1 <- ifelse(bd$MAGE<20, c(1),c(0))

bdlog2<-glm(casegrp~magecat1+magecat2,binomial,data=bd)
summary(bdlog2)

exp(cbind(OR=coef(bdlog2),confint(bdlog2)))

bdlog3<-glm(casegrp~magecat1+magecat2+bthparity2+smoke,binomial,data=bd)
anova(bdlog3,test="Chisq")

install.packages("arm")
library(arm)
x<-predict(bdlog)
y<-resid(bdlog)
binnedplot(x,y)

# Poisson regression example

ceb<-read.table("F:/RShortcourse/ceb.dat",header=T)

ceb1<-glm(y ~ educ + res, offset=log(n), family = "poisson", data = ceb)
summary(ceb1)

ceb1<-glm(y~educ, family=“poisson", offset=log(n), data= ceb)
ceb2<-glm(y~educ+res, family=“poisson", offset=log(n), data= ceb)

1-pchisq(deviance(ceb1)-deviance(ceb2), df.residual(ceb1)-df.residual(ceb2))

ceb2<-glm(y~educ+res, family="quasipoisson", offset=log(n), data=ceb)

install.packages(“MASS”)
library(MASS)
ceb.nb <- glm.nb(y~educ+res+offset(log(n)), data= ceb)

install.packages(“pscl”)
library(pscl)
ceb.zip <- zeroinfl(y~educ+res, offset=log(n), data= ceb)



