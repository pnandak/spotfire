
#### Income Data ####

census <- read.csv("census2000.csv")
# only include folks working more than 500 hours AND earning more than $5000 AND less than age 60
workers <- (census$hours > 500)&(census$income > 5000)&(census$age < 60) 
log.WR <- log(census$income/census$hours)[workers]
age <- census$age[workers]
age2 <- age^2
sex <- census$sex[workers]
edu <- census$education[workers]
 # Use relevel to make "White" and "Married" the intercept
race <- relevel(census$race[workers], "White") 
marital <- relevel(census$marital[workers], "Married")

summary(fullreg <- lm(log.WR ~ age*sex + age2*sex + age*edu + age*race + age*marital) ) #too big
morethanHS <-  (edu=="4.assoc")|(edu=="5.bachs")|(edu=="6.mstr")|(edu=="7.profdef")|(edu=="8.phd") 

summary(reduced1 <- lm(log.WR ~ age*sex + age2*sex + age*morethanHS + age*race + marital) ) #too small
anova(reduced1, fullreg)  

summary(reduced2 <- lm(log.WR ~ age*sex + age2*sex + age*edu + age*race + marital) ) #just right!
anova(reduced2, fullreg)

n <- sum(workers)
# AIC
extractAIC(fullreg)
extractAIC(reduced1)
extractAIC(reduced2)
# BIC
BIC <- cbind(extractAIC(fullreg, k=log(n)),
                    extractAIC(reduced1, k=log(n)),
                    extractAIC(reduced2, k=log(n)) )
# Model probabilities
print(eBIC <- exp(-.5*(BIC[2,]-min(BIC[2,]))))
round(probs <- eBIC/sum(eBIC), 2)
# Forward Stepwise Regression
null <- lm(log.WR ~ age*sex + age2*sex)
fwdreg <- step(null, k=log(n), direction="forward", scope=~.+age*edu+age*race+age*marital)



##### Crime Data ####### this example is a good guide for your homework

crime <- read.csv("CommunityCrime.csv")
logCR <- log(crime$ViolentCR)
covars <- crime[,-(1:2)] # data frame of the covariates
train <- 1:1500

initreg <- lm(logCR[train] ~ 1, data=covars[train,])
# build a regression model with the BIC
regBIC <- step(initreg, scope=formula(terms(logCR[train] ~ ., data=covars[train,])), direction="forward", k=log(1500))
# again with the BIC, but searching all variables AND interactions
regBIC2 <- step(initreg, scope=formula(terms(logCR[train] ~ . + .^2, data=covars[train,])), direction="forward", k=log(1500))
# or the full model
regFull <- lm(logCR[train] ~ ., data=covars[train,])
# then with the AIC (just for curiosity, you don't need to do this one on your assignment)
regAIC <- step(initreg, scope=formula(terms(logCR[train] ~ ., data=covars[train,])), direction="forward")      

### Calculate the probabilities for each
BICandP <- cbind(regBIC1 = extractAIC(regBIC, k=log(1500)),
             regBIC2 = extractAIC(regBIC2, k=log(1500)),
             regFull  = extractAIC(regFull, k=log(1500)),
             regAIC  = extractAIC(regAIC, k=log(1500)))
bic <- BICandP[2,]
ebic <- exp(-.5*(bic-min(bic)))
round(probs <- ebic/sum(ebic),4)

### Plot the fits
par(mfrow=c(1,4))
plot(regBIC$fitted, logCR[train], pch=20, col=grey(.3), xlab="fitted", ylab="logCR",
     main=paste("BIC fit: p =",length(regBIC$coef)))
lines(regBIC$fitted, regBIC$fitted, lty=2, col=2)
plot(regBIC2$fitted, logCR[train], pch=20, col=grey(.3), xlab="fitted", ylab="logCR",
     main=paste("BIC2 fit: p =",length(regBIC2$coef)))
lines(regBIC2$fitted, regBIC2$fitted, lty=2, col=2)
plot(regFull$fitted, logCR[train], pch=20, col=grey(.3), xlab="fitted", ylab="logCR",
     main=paste("Full model: p =",length(regFull$coef)))
lines(regFull$fitted, regFull$fitted, lty=2, col=2)
plot(regAIC$fitted, logCR[train], pch=20, col=grey(.3), xlab="fitted", ylab="logCR",
     main=paste("AIC fit: p =",length(regAIC$coef)))
lines(regAIC$fitted, regAIC$fitted, lty=2, col=2)

### Get the MSE's on left out data
errorBIC <- predict(regBIC, newdata=covars[-train,])-logCR[-train]
errorBIC2 <- predict(regBIC2, newdata=covars[-train,])-logCR[-train]
errorFull <- predict(regFull, newdata=covars[-train,])-logCR[-train]
errorAIC <- predict(regAIC, newdata=covars[-train,])-logCR[-train]

mean(errorBIC^2)
mean(errorBIC2^2)
mean(errorFull^2)
mean(errorAIC^2)

