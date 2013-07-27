
###############################################
## Day 2: Example: Nuclear
###############################################
nuc<-read.table("nuclear.txt",header=TRUE)
summary(nuc)
pairs(nuc)
lm1<-lm(MWatts~Cost,nuc)
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)
par(mfrow=c(1,1))
plot(nuc$Cost,nuc$MWatts)
abline(lm1$coef)

lm2<-lm(Cost~Date,nuc)
summary(lm2)
par(mfrow=c(2,2))
plot(lm2)


lm3<-lm(MWatts~Date,nuc)
summary(lm3)
par(mfrow=c(2,2))
plot(lm3)

lm4<-lm(Cost~.,nuc)
summary(lm4)

lm4<-lm(MWatts~.,nuc)
summary(lm4)


