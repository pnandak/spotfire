## Code for Tutorial 4 

# Problem 1 

# reading in the seal data
cfseal <- read.table("cfseal.txt", header=T, sep="\t")

# 1A
qqnorm(cfseal$heart, main="QQ Plot of Heart Variable", col="red", pch=20) ; qqline(cfseal$heart, lwd=3)
qqnorm(log(cfseal$heart), main="QQ Plot of Log(Heart)", col="red", pch=20) ; qqline(log(cfseal$heart), lwd=3)

# 1B
lmAll <- lm(log(heart) ~ ., data=cfseal)
summary(lmAll)

# 1C
lmBK <- stepAIC(lmAll)
summary(lmBK)


# 1D
lmOne <- lm(log(heart) ~ weight.log, data=cfseal)

# 1E
lmCon <- 
lmSub <- lm(log(heart) ~ weight.log + lung.log, data=cfseal)
summary(lmSub)

# 1F
anova(lmAll, lmBK)

# Problem 2 

# 2A
anesthetic <- read.table("anesthetic.txt", header=T, sep="\t")

# 2B
anes.logit <- glm(nomove ~ conc, family=binomial(link=logit), data=anesthetic)
summary(anes.logit) 

# 2C
fitted.values(anes.logit)

# 2D 
p <- fitted.values(anes.logit) 
lg <- log(p/(1-p))
anes.logit$linear.predictors

# 2E
plot(anesthetic$conc, anes.logit$linear.predictors, xlab="Concentration", ylab="Log(Odds of Moving)", main="Logistic Regression Model")
abline(coefficients(anes.logit), lwd=3, col="magenta")

# Problem 3

# 3A
bwt <- read.table("birthwt.txt", header=T, sep="\t")

# 3B
race <- factor(race, labels=c("white", "black", "other"))

table(ptl)
ptd <- factor(ptl > 0)
ftv <- factor(ftv)
table(ftv)
levels(ftv)[-(1:2)] <- "2+"

# 3C
bwt <- data.frame(low = factor(low), age, lwt, race, smoke = factor(smoke), ptd, ht = factor(ht), ui = factor(ui), ftv)

# 3D
birthwt.glm <- glm(low ~., family=binomial, data=bwt)
summary(birthwt.glm)
step.out <- stepAIC(birthwt.glm, direction="backward")
summary(step.out)

# 3E
pred.out <- predict(birthwt.glm)

class.pred <- as.numeric(pred.out > 0)
table(prediction=class.pred, truth=low)

# Problem 4

# 4A
schooldat <- read.csv("schooldata.csv", head=T)
attach(schooldat)

# 4B
male <- factor(schooldat$male)
male <- factor(male, labels=c("female", "male"))
schooldat <- data.frame(male, math, langarts, daysabs) 
plm <- glm(daysabs ~ ., family=poisson, data=schooldat)
summary(plm)

# 4C
dropterm(plm, test="Chisq")

# 4D
step.mod <- stepAIC(plm, direction="backward")
summary(step.mod)

# Problem 5 

# 5A
aml <- read.table("aml.txt", sep="\t", header=T)

# 5B
fit1 <- survfit(Surv(time, status==1), data=aml)
summary(fit1)
plot(fit1)

fit.byx <- survfit(Surv(time, status==1)~x, data=aml)
summary(fit.byx)
plot(fit.byx)

# 5C
survdiff(Surv(time, status==1)~x, data=aml)

# 5D
coxfit1 <- coxph(Surv(time, status==1)~x, data=aml)
summary(coxfit1)

# cumulative baseline hazard estimator
basehaz(coxph(Surv(time, status==1)~x, data=aml))
basehaz(coxph(Surv(time, status==1)~strata(x), data=aml))

# 5E
plot(survfit(coxph(Surv(time, status==1)~strata(x), data=aml)))
