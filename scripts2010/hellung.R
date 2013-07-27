###############################################
## Day 5: hellung
###############################################
hel<-read.table("hellung.txt",header=TRUE,row.names=1)
summary(hel)
hel$glucose<-factor(hel$glucose,labels=c("Yes","No"))
summary(hel)
pairs(hel)
hel$lconc<-log(hel$conc)
pairs(hel)
lm1<-lm(diameter~lconc*glucose,hel)
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)
lm2<-lm(diameter~lconc+glucose,hel)
summary(lm2)
plot(lm2)
lm3<-lm(sqrt(diameter)~lconc+glucose,hel)
summary(lm3)
plot(lm3)

par(mfrow=c(1,1))
plot(hel$lconc,hel$diameter,col=as.numeric(hel$glucose),pch=19)
coef(lm2)
abline(a=coef(lm2)[1],b=coef(lm2)[2],lwd=2)
abline(a=sum(coef(lm2)[-2]),b=coef(lm2)[2],lwd=2,col=2)

