
###############################################
## Day 2: Example: Hubble's law
###############################################
hub<-read.table("hubble.txt",header=TRUE)
summary(hub)
plot(velocity~distance,hub)
lm1<-lm(velocity~distance,data=hub)
summary(lm1)
par(mfrow=c(2,2))
plot(lm1) ## OK

lm2<-lm(velocity~distance-1,hub)
lm2<-update(lm1,~.-1)
summary(lm2)
par(mfrow=c(2,2))
plot(lm2)

## Looking at prediction interval and conf interval:
par(mfrow=c(1,1))
abline(lm2,col=3,lwd=3)
pred.data <- data.frame(distance=seq(0,max(hub$distance),length=100))
pred.int<-predict(lm2,int="prediction",newdata=pred.data)
plot(velocity~distance,hub, ylim=range(hub$velocity, pred.int, na.rm=T))
pred.x <- pred.data$distance
matlines(pred.x, pred.int, lty=c(1,2,2), col="black")
conf.int<-predict(lm2, int="confidence", newdata=pred.data)
matlines(pred.x, conf.int, lty=c(1,2,2), col="red")
