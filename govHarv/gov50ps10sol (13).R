

##  Problem 1

##  Loading the data and fitting the regression

library(car)
data(Leinhardt)
lmout <- lm(log(infant) ~ log(income) + oil + log(income):oil, data=Leinhardt)
summary(lmout)

##  Plotting the data and the prediction equations

attach(Leinhardt)
plot(log(infant) ~ log(income), pch=as.numeric(oil))
abline(7.42765, -0.569)
abline(7.42765 + -5.37469, -0.569 + 0.981)

##  Examining the added variable plots

par(mfrow=c(2,2))
av.plot(lmout, "(Intercept)")
av.plot(lmout, "log(income)")
av.plot(lmout, "oilyes")
av.plot(lmout, "log(income):oilyes")


##  Problem 2

##  Loading the data and fitting the model

oring <- read.csv("C:/datasets/oring.csv")
glmout <- glm(damage ~ temp, data=oring, family=binomial)
summary(glmout)

##  Generating predicted probabilities

newdata <- data.frame(temp=20:90)
predict(glmout, newdata, type="response")
plot(oring$temp, oring$damage, xlim=c(20, 90), xlab="temperature", ylab="damage")
lines(20:90, predict(glmout, newdata, type="response"), lwd=2)

##  Predicted probability when temp=31

predict(glmout, newdata=data.frame(temp=31), type="response")

##  Odds ratio for 1 degree difference in temperature

exp(-0.2322)

##  Problem 3

##  Part (a)

exp(-2.4 + .02*30 + 0.08*16 + .2*1)/(1 + exp(-2.4 + .02*30 + 0.08*16 + .2*1))

exp(-2.4 + .02*30 + 0.08*16)/(1 + exp(-2.4 + .02*30 + 0.08*16))

##  Part (b)

.421/(1-.421)

.373/(.627)

.727/.595

##  Part (c)

exp(.08)

##  Part (d)

exp(-2.4 + .02*30 + 0.08*11 + .2*1)/(1 + exp(-2.4 + .02*30 + 0.08*11 + .2*1))

exp(-2.4 + .02*30 + 0.08*12 + .2*1)/(1 + exp(-2.4 + .02*30 + 0.08*12 + .2*1))

##  Part (e)

exp(-2.4 + .02*30 + 0.08*11)/(1 + exp(-2.4 + .02*30 + 0.08*11))

exp(-2.4 + .02*30 + 0.08*12)/(1 + exp(-2.4 + .02*30 + 0.08*12))

