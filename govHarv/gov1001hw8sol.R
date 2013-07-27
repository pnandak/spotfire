
##  Solution Code, HW8

##  Problem 3

vote <- read.csv("H:/election00.csv")
head(vote)

attach(vote)
lmout1 <- lm(dempct ~ gorepct)
summary(lmout1)
confint(lmout1)

par(mfrow=c(1,2))
plot(dempct ~ gorepct, main = "Democratic congressional vote vs.\n presidential vote, 2000", ylab = "Congressional vote (%)", xlab = "Gore vote (%)")
abline(lmout1)


plot(dempct ~ gorepct, main = "Democratic congressional vote vs.\n presidential vote, 2000", ylab = "Congressional vote (%)", xlab = "Gore vote (%)")
points(dempct[party00==1] ~ gorepct[party00==1], pch=16)
abline(lmout1)
legend(x=55, y=25, c("Dem incumbent", "Rep incumbent"), pch=c(1, 16))

lmout2 <- lm(dempct ~ gorepct + party00)
summary(lmout2)
confint(lmout2)

plot(dempct ~ gorepct, main = "Democratic congressional vote vs.\n presidential vote, 2000", ylab = "Congressional vote (%)", xlab = "Gore vote (%)")
points(dempct[party00==1] ~ gorepct[party00==1], pch=16)
abline(lmout2)
abline(lmout2$coef[1] + lmout2$coef[3], lmout2$coef[2])
legend(x=55, y=25, c("Dem incumbent", "Rep incumbent"), pch=c(1, 16))

library(car)
par(mfrow=c(1,2))
av.plots(lmout2)

plot(lmout2, 3, main = "SL plot for constant error variance")
qq.plot(lmout2, main = "QQ-plot for normality")

simdata <- read.csv("H:/simdata.csv")
attach(simdata)

lmoutY1X1 <- lm(Y1 ~ X1)
summary(lmoutY1X1)
confint(lmoutY1X1)

lmoutY1Z1 <- lm(Y1 ~ Z1)
summary(lmoutY1Z1)
confint(lmoutY1Z1)

lmoutY1X1Z1 <- lm(Y1 ~ X1 + Z1)
summary(lmoutY1X1Z1)
confint(lmoutY1X1Z1)

lmoutZ1X1 <- lm(Z1 ~ X1)

plot(Y1 ~ Z1, main="Scatterplot of Y1 on Z1", ylim=c(-10, 15))
abline(lmoutY1Z1)
plot(residuals(lmoutY1X1) ~ residuals(lmoutZ1X1),  ylim=c(-10, 15),main="Added Variable plot of Y1 on Z1")
abline(lm(residuals(lmoutY1X1) ~ residuals(lmoutZ1X1)))


lmoutY2X2 <- lm(Y2 ~ X2)
summary(lmoutY2X2)
confint(lmoutY2X2)

lmoutY2Z2 <- lm(Y2 ~ Z2)
summary(lmoutY2Z2)
confint(lmoutY2Z2)

lmoutY2X2Z2 <- lm(Y2 ~ X2 + Z2)
summary(lmoutY2X2Z2)
confint(lmoutY2X2Z2)

lmoutZ2X2 <- lm(Z2 ~ X2)

plot(Y2 ~ Z2, main="Scatterplot of Y2 on Z2", ylim=c(-10, 15))
abline(lmoutY2Z2)
plot(residuals(lmoutY2X2) ~ residuals(lmoutZ2X2),  ylim=c(-10, 15),main="Added Variable plot of Y2 on Z2")
abline(lm(residuals(lmoutY2X2) ~ residuals(lmoutZ2X2)))

plot(Z1 ~ X1, main= "Scatterplot of Z on X, first dataset")
plot(Z2 ~ X2, main= "Scatterplot of Z on X, second dataset")