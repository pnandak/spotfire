###  Problem Set 8

##   Sample code

##   Problem 1

library(car)
sat <- read.csv("C:/datasets/statesat.csv", header=T)
summary(sat)
attach(sat)

##  Part (a)

lmout <- lm(Math04 ~ Participation)
summary(lmout)

##  Part (b)

pdf("hw8fig1a.pdf")
par(mfrow=c(2,2))
scatter.smooth(Math04 ~ Participation)
plot(lmout, 1)
plot(lmout, 3)
plot(lmout, 2)
dev.off()

##  Part (c)

lmout2 <- lm(Math04 ~ log(Participation))
summary(lmout2)

##  Part (d)

pdf("hw8fig1b.pdf")
par(mfrow=c(2,2))
scatter.smooth(Math04 ~ log(Participation))
plot(lmout2, 1)
plot(lmout2, 3)
plot(lmout2, 2)
dev.off()


###  Problem 2

vote <- read.csv("C:/datasets/election00.csv")
head(vote)

##  Part (a-b)

attach(vote)
lmout1 <- lm(dempct ~ gorepct)
summary(lmout1)
confint(lmout1)

##  Part (c)

pdf("hw8fig2a.pdf")
plot(dempct ~ gorepct, main = "Democratic congressional vote vs.\n presidential vote, 2000", ylab = "Congressional vote (%)", xlab = "Gore vote (%)")
abline(lmout1)
dev.off()

##  Part (d)

pdf("hw8fig2b.pdf")
plot(dempct ~ gorepct, main = "Democratic congressional vote vs.\n presidential vote, 2000", ylab = "Congressional vote (%)", xlab = "Gore vote (%)")
points(dempct[party00==1] ~ gorepct[party00==1], pch=16)
abline(lmout1)
legend(x=55, y=25, c("Dem incumbent", "Rep incumbent"), pch=c(1, 16))
dev.off()

##  Part (e)

lmout2 <- lm(dempct ~ gorepct + party00)
summary(lmout2)
confint(lmout2)

pdf("hw8fig2c.pdf")
plot(dempct ~ gorepct, main = "Democratic congressional vote vs.\n presidential vote, 2000", ylab = "Congressional vote (%)", xlab = "Gore vote (%)")
points(dempct[party00==1] ~ gorepct[party00==1], pch=16)
abline(lmout2)
abline(lmout2$coef[1] + lmout2$coef[3], lmout2$coef[2])
legend(x=55, y=25, c("Dem incumbent", "Rep incumbent"), pch=c(1, 16))
dev.off()


###   Problem 3






