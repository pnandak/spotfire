## LPM, logit, and probit, PS 206 Class 2 

PPICdat2 <- read.table("PPIC_class1.txt", header=TRUE)

PPICdat <- as.data.frame(PPICdat2)
attach(PPICdat)  

## linear probability model

model1 <- lm(votemail ~ gender+age+youngkid+strongpart, data=PPICdat)
summary(model1)

## plot data and regression line

plot(age, votemail, xlab="Age", ylab="Vote By Mail", xlim=c(0,100), ylim=c(-0.1,1.1), mgp=c(2,.5,0))
curve(coef(model1)[1] + coef(model1)[3]*x, add=TRUE)

## to make one problem more obvious

plot(age, votemail, xlab="Age", ylab="Vote By Mail", xlim=c(-100,200), ylim=c(-0.1,1.1), mgp=c(2,.5,0))
curve(coef(model1)[1] + coef(model1)[3]*x, add=TRUE)

## More problems

e <- model1$residuals
e2 <- e^2
plot(age, e2)

#install.packages("sandwich")
library(sandwich)
#install.packages("lmtest")
library(lmtest)

bptest(model1)

coeftest(model1, vcov=sandwich)

## logit 

logitmodel <- glm(votemail ~ gender+age+youngkid+strongpart, family=binomial(link="logit"), data=PPICdat)
summary(logitmodel)


## Plot predicted probabilities ##

logitmodel2 <- glm(votemail ~ age, family=binomial(link="logit"), data=PPICdat)
summary(logitmodel2)

plot(age, votemail, xlab="Age", ylab="Vote By Mail", xlim=c(0,100), ylim=c(-0.1,1.1), mgp=c(2,.5,0))
curve(exp(coef(logitmodel2)[1] + coef(logitmodel2)[2]*x)/(1 + exp(coef(logitmodel2)[1] + coef(logitmodel2)[2]*x)), add=TRUE)

## expand range of x to better demonstrate probability curve

plot(age, votemail, xlab="Age", ylab="Vote By Mail", xlim=c(-100,300), ylim=c(-0.1,1.1), mgp=c(2,.5,0))
curve(exp(coef(logitmodel2)[1] + coef(logitmodel2)[2]*x)/(1 + exp(coef(logitmodel2)[1] + coef(logitmodel2)[2]*x)), add=TRUE)

## probit

probitmodel <- glm(votemail ~ gender+age+youngkid+strongpart, family=binomial(link="probit"), data=PPICdat)
summary(probitmodel)

## Plot predicted probabilities on exaggerated range

probitmodel2 <- glm(votemail ~ age, family=binomial(link="probit"), data=PPICdat)
summary(probitmodel2)

plot(age, votemail, xlab="Age", ylab="Vote By Mail", xlim=c(-100,300), ylim=c(-0.1,1.1), mgp=c(2,.5,0))
curve(pnorm(coef(probitmodel2)[1] + coef(probitmodel2)[2]*x), add=TRUE)

## comparison of logit and probit

predprobs <- cbind(logitmodel$fitted.values, probitmodel$fitted.values)
predprobs

plot(age, votemail, xlab="Age", ylab="Vote By Mail", xlim=c(0,100), ylim=c(-0.1,1.1), mgp=c(2,.5,0))
curve(exp(coef(logitmodel)[1] + coef(logitmodel)[3]*x)/(1 + exp(coef(logitmodel)[1] + coef(logitmodel)[3]*x)), add=TRUE)
curve(pnorm(coef(probitmodel)[1] + coef(probitmodel)[3]*x), col="red", lty="dashed", add=TRUE)

## Again with exaggerated x range 

plot(age, votemail, xlab="Age", ylab="Vote By Mail", xlim=c(-100,300), ylim=c(-0.1,1.1), mgp=c(2,.5,0))
curve(exp(coef(logitmodel)[1] + coef(logitmodel)[3]*x)/(1 + exp(coef(logitmodel)[1] + coef(logitmodel)[3]*x)), add=TRUE)
curve(pnorm(coef(probitmodel)[1] + coef(probitmodel)[3]*x), col="red", lty="dashed", add=TRUE)

##
## practice estimating and interpreting other models, generating plots
##