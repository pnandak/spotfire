## Linear Regression, PS 206 Class 1 (Part 2)

PPICdat2 <- read.table("PPIC_class1.txt", header=TRUE)

PPICdat <- as.data.frame(PPICdat2)
attach(PPICdat)  

## linear regression

model1 <- lm(votemail ~ gender+age+youngkid+strongpart, data=PPICdat)
summary(model1)

## plot data and regression line

plot(age, votemail, xlab="Age", ylab="Vote By Mail", xlim=c(0,100), ylim=c(-0.1,1.1), mgp=c(2,.5,0))
curve(coef(model1)[1] + coef(model1)[3]*x, add=TRUE)

## to make one problem more obvious

plot(age, votemail, xlab="Age", ylab="Vote By Mail", xlim=c(-100,200), ylim=c(-0.1,1.1), mgp=c(2,.5,0))
curve(coef(model1)[1] + coef(model1)[3]*x, add=TRUE)

## Tests for heteroskedasticity and robust standard errors

e <- model1$residuals
e2 <- e^2
plot(age, e2)

#install.packages("sandwich")
library(sandwich)
#install.packages("lmtest")
library(lmtest)

bptest(model1)

coeftest(model1, vcov=sandwich)
