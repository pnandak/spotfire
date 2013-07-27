# Code for Lecture 4

library(ISwR) 
library(help=ISwR)

# 
data(tlc)
class(tlc)
help(tlc)
attach(tlc)
age

#
lmObject <- lm(tlc ~ height, data=tlc)
class(lmObject)
summary(lmObject)

#
TLC <- tlc[,4]
plot(height, TLC)
abline(lmObject)

# 
fitted(lmObject)
plot(height, TLC) 
abline(lmObject)
points(height, fitted(lmObject), pch=20, col="red")
resid(lmObject)

#
plot(height, TLC)
abline(lmObject)
segments(height, fitted(lmObject), height, TLC) 
plot(height, resid(lmObject))
qqplot(resid(lmObject))

#
predict(lmObject)
predict(lmObject, interval="confidence")

#
pp <- predict(lmObject, interval="confidence")
plot(height, pp[,1], type="l", ylim=range(pp, height))
lines(height, pp[,2], col="red", lwd=3)
lines(height, pp[,3], col="blue", lwd=3)

#
range(height)
new <- data.frame(height = seq(from=120, to=200, by=2))
pp.new <- predict(lmObject, new, interval="confidence")

#
plot(new$height, pp.new[,1], type="l")
lines(new$height, pp.new[,2], col="red", lwd=2)
lines(new$height, pp.new[,3], col="blue", lwd=2)

#
help(matlines)
plot(new$height, pp.pred[,1], ylim=range(pp.pred, pp.conf))
matlines(new$height, pp.pred[,-1], type=c("l", "l"), lwd=c(3,3), 
	col=c("red", "blue"), lty=c(1,1))

matlines(new$height, pp.conf[,-1], type=c("l", "l"), lwd=c(3,3),
	col=c("red", "blue"), lty=c(2,2))

# 
library(MASS)
data(ChickWeight)
help(ChickWeight)
attach(ChickWeight)
levels(Data)

# 
anova(lm(weight ~ Diet + Chick, data=ChickWeight))

anova(lm(weight ~ Diet, data=ChickWeight))

