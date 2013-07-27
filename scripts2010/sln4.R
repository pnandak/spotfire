####### 1.1 Residuals #######

## Here is how the transforms.csv data were generated:
#  X1 <- runif(200)
#  Y1 <- exp(-2*X1 + rnorm(200, sd=.4))
#  X2 <- runif(200, -3,3)
#  Y2 <- 3 + 2*X2 - .75*X2^2 + rnorm(200)
#  X3 <- exp(rnorm(200, mean=0))
#  Y3 <- 3 - 5*log(X3) + rnorm(200)
#  X4 <- exp(rnorm(mean=0, 200))
#  Y4 <- 10*X4^{2}*exp(rnorm(200))
## 

attach(D <- read.csv("transforms.csv"))
lm1 <- lm(Y1 ~ X1)
lm2 <- lm(Y2 ~ X2)
lm3 <- lm(Y3 ~ X3)
lm4 <- lm(Y4 ~ X4)

par(mfrow=c(3,4))  #you might have to make the plot window big to fit everything
plot(lm1$fitted, rstudent(lm1), col=1,
     xlab="Fitted Values", ylab="Studentized Residuals", pch=20, main="I")
plot(lm2$fitted, rstudent(lm2), col=2,
     xlab="Fitted Values", ylab="Studentized Residuals", pch=20, main="II")
plot(lm3$fitted, rstudent(lm3), col=3,
     xlab="Fitted Values", ylab="Studentized Residuals", pch=20, main="III")
plot(lm4$fitted, rstudent(lm4), col=4,
     xlab="Fitted Values", ylab="Studentized Residuals", pch=20, main="IV")

qqnorm(rstudent(lm1), pch=20, col=1, main="" )
abline(a=0,b=1,lty=2)
qqnorm(rstudent(lm2), pch=20, col=2, main="" )
abline(a=0,b=1,lty=2)
qqnorm(rstudent(lm3), pch=20, col=3, main="" )
abline(a=0,b=1,lty=2)
qqnorm(rstudent(lm4), pch=20, col=4, main="" )
abline(a=0,b=1,lty=2)

hist( rstudent(lm1), col=1, xlab="Studentized Residuals", main="", border=8)
hist( rstudent(lm2), col=2, xlab="Studentized Residuals", main="")
hist( rstudent(lm3), col=3, xlab="Studentized Residuals", main="")
hist( rstudent(lm4), col=4, xlab="Studentized Residuals", main="")

### the fixes are as follows:
logY1 <- log(Y1)
X2square <- X2^2
logX3 <- log(X3)
logX4 <- log(X4)
logY4 <- log(Y4)

### re-run the regressions and residual plots to show this worked

lm1 <- lm(logY1 ~ X1)
lm2 <- lm(Y2 ~ X2 + X2square)
lm3 <- lm(Y3 ~ logX3)
lm4 <- lm(logY4 ~ logX4)

par(mfrow=c(3,4))  #you might have to make the plot window big to fit everything
plot(lm1$fitted, rstudent(lm1), col=1,
     xlab="Fitted Values", ylab="Studentized Residuals", pch=20, main="I")
plot(lm2$fitted, rstudent(lm2), col=2,
     xlab="Fitted Values", ylab="Studentized Residuals", pch=20, main="II")
plot(lm3$fitted, rstudent(lm3), col=3,
     xlab="Fitted Values", ylab="Studentized Residuals", pch=20, main="III")
plot(lm4$fitted, rstudent(lm4), col=4,
     xlab="Fitted Values", ylab="Studentized Residuals", pch=20, main="IV")

qqnorm(rstudent(lm1), pch=20, col=1, main="" )
abline(a=0,b=1,lty=2)
qqnorm(rstudent(lm2), pch=20, col=2, main="" )
abline(a=0,b=1,lty=2)
qqnorm(rstudent(lm3), pch=20, col=3, main="" )
abline(a=0,b=1,lty=2)
qqnorm(rstudent(lm4), pch=20, col=4, main="" )
abline(a=0,b=1,lty=2)

hist( rstudent(lm1), col=1, xlab="Studentized Residuals", main="", border=8)
hist( rstudent(lm2), col=2, xlab="Studentized Residuals", main="")
hist( rstudent(lm3), col=3, xlab="Studentized Residuals", main="")
hist( rstudent(lm4), col=4, xlab="Studentized Residuals", main="")

######## 1.2 Cheese #########

attach(cheese <- read.csv("cheese.csv"))

## (i)
anova(lm(log(vol) ~ disp))
par(mfrow=c(1,2))
plot(as.factor(disp), log(vol), col=7, ylab="log sales volume", xlab="display")
plot(as.factor(disp), log(price), col=7, ylab="log price", xlab="display")
# There is a clear effect of display on volume (p-value for test
# of zero difference between group means is <2.2e-16). However,
# the boxplots show that display ad campaigns tend to be associated
# with lower prices. Hence, we cannot know if the increased volume is
# caused by the advertizements or by price cuts.

## (ii)
par(mfrow=c(1,1))
colors = c(4,2)
plot(log(price), log(vol), pch=20, cex=.5, col=colors[disp+1] )
legend("bottomleft", legend=c("No Display", "Ad Display"), fill=colors)
# We see that deep price cuts are accompanied by ad displays.
# However, it is hard to see if there are different slopes by display status
summary(reg0 <- lm(log(vol[disp==0]) ~ log(price[disp==0])) )
summary(reg1 <- lm(log(vol[disp==1]) ~ log(price[disp==1])) )
diff <- -1.28564 + 0.88982  # b1 for display minus b1 for no display
sdiff <- sqrt(0.07511^2 + 0.08801^2)  # standard error of the difference
diff/sdiff # t-statistic (is normal, since df is huge).
###
# p-value = p(Z < diff/sdiff) = .0003 is tiny, so we reject the null hypothesis
# of equal price elasticities. Interesting, it seems that the disp=1 data shows a
# steeper price elasticity.  My guess at an explanation is that the display
# adds are incuding customers who are not normally Borden Sliced Cheese fans
# to take a look at the product, and that these consumers are more price
# sensitive than those who by the brand regularly.
###
# add the LS lines to the plot:
abline(reg0, col=colors[1])
abline(reg1, col=colors[2])
###
# As a bit of foreshadowing, here is how to do this test with a single regression:
# the coefficient on the "interaction term" log(price):disp is our 'diff' above
# and its t-value is the same as our diff/sdiff (up to rounding error)
summary( lm(log(vol) ~ log(price)*disp) )
