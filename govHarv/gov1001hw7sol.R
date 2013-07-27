

library(car)
data(Florida)
out1 <- lm(GORE ~ Total, data=Florida)
summary(out1)
confint(out1)

out2 <- lm(I(GORE/1000) ~ Total, data=Florida)
summary(out2)
confint(out2)

out3 <- lm(GORE ~ I(Total/1000), data=Florida)
summary(out3)
confint(out3)

out4 <- lm(I(GORE/1000) ~ I(Total/1000), data=Florida)
summary(out4)
confint(out4)


##  Problem 3

##  (a): n= 11

set.seed(7963412)

XX <- seq(0,1,by=.1)
intercept <- numeric()
slope <- numeric()

for(i in 1:1000){
YY <- 2 + .5*XX + rt(11, 3)
lmout <- lm(YY ~ XX)
intercept[i] <- lmout$coef[1]
slope[i] <- lmout$coef[2]
}

par(mfrow=c(1, 2))
hist(intercept, main="Sampling distribution \n for intercept \n n=11", freq=FALSE)
hist(slope, main="Sampling distribution \n for slope \n n=11", freq=FALSE)
qq.plot(intercept, main="Sampling distribution \n for intercept \n n=11")
qq.plot(slope, main="Sampling distribution \n for slope \n n=11")


set.seed(341287)

XX <- seq(0,1,by=.01)
intercept <- numeric()
slope <- numeric()

for(i in 1:1000){
YY <- 2 + .5*XX + rt(101, 3)
lmout <- lm(YY ~ XX)
intercept[i] <- lmout$coef[1]
slope[i] <- lmout$coef[2]
}

par(mfrow=c(1, 2))
hist(intercept, main="Sampling distribution \n for intercept \n n=101", freq=FALSE)
hist(slope, main="Sampling distribution \n for slope \n n=101", freq=FALSE)
qq.plot(intercept, main="Sampling distribution \n for intercept \n n=101")
qq.plot(slope, main="Sampling distribution \n for slope \n n=101")


data(Robey)
attach(Robey)

lmout <- (lm(tfr ~ contraceptors))
summary(lmout)
confint(lmout)

scatter.smooth(tfr ~ contraceptors, main = "Total Fertility Rate vs.\nPercent using contraception")

abline(lmout)

plot(lmout, which=3, main = "SL Plot for TFR ~ Contraceptors")
qq.plot(lmout, main = "QQ Plot for TFR ~ Contraceptors" )

boxplot(residuals(lmout) ~ region, main = "Residuals by region, TFR ~ Contraceptors" )


lmout <- (lm(BUCHANAN ~ GORE, data=Florida))
summary(lmout)
confint(lmout)


plot(Florida$GORE, hatvalues(lmout), main ="Leverage for Buchanan ~ Gore")
identify(Florida$GORE, hatvalues(lmout), labels=rownames(Florida))

plot(Florida$GORE, rstudent(lmout), main ="Outlyingness for Buchanan ~ Gore")
identify(Florida$GORE, rstudent(lmout), labels=rownames(Florida))
abline(h=c(2,-2))

plot(Florida$GORE, cooks.distance(lmout), main ="Influence for Buchanan ~ Gore")
identify(Florida$GORE, cooks.distance(lmout), labels=rownames(Florida))


plot(hatvalues(lmout), rstudent(lmout), type="n")
cook <- sqrt(cooks.distance(lmout))
points(hatvalues(lmout), rstudent(lmout), cex=10*cook/max(cook))
abline(h=c(-2,2))
