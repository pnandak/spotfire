d.tips<-matrix(scan("../data/tips.dat"),ncol=8,byrow=T)
dimnames(d.tips)<-list(NULL,scan("../data/tips.col",what=character()))

#COMPUTE TIP RATE
tiprate<-d.tips[,3]/d.tips[,2]
d.tips<-cbind(d.tips,tiprate)
dimnames(d.tips.full)[[2]][9]<-"Tip Rate"

#SUMMARY STATISTICS
apply(d.tips[,-1],2,mean)
cor(d.tips[,-1])

apply(d.tips[,c(4,5,7)],2,sum)
table(d.tips[,6])
table(d.tips[,8])
table(d.tips[,6],d.tips[,7])
table(d.tips[,4],d.tips[,5])
mean(tiprate[d.tips[,4]==0])
mean(tiprate[d.tips[,4]==1])
mean(tiprate[d.tips[,5]==0])
mean(tiprate[d.tips[,5]==1])
mean(tiprate[d.tips[,5]==0&d.tips[,4]==0])
mean(tiprate[d.tips[,5]==0&d.tips[,4]==1])
mean(tiprate[d.tips[,5]==1&d.tips[,4]==0])
mean(tiprate[d.tips[,5]==1&d.tips[,4]==1])

mean(tiprate[d.tips[,8]==1])
mean(tiprate[d.tips[,8]==2])
mean(tiprate[d.tips[,8]==3])
mean(tiprate[d.tips[,8]==4])
mean(tiprate[d.tips[,8]==5])
mean(tiprate[d.tips[,8]==6])

#HISTOGRAMS OF TIPS
par(mfrow=c(3,1))
hist(d.tips[,3],breaks=c(0:10),col=2,xlab="Tips",main="Breaks at $1",cex=1)
hist(d.tips[,3],breaks=c(0:20)/2,col=2,xlab="Tips",main="Breaks at 50c",cex=1)
hist(d.tips[,3],breaks=c(0:30)/3,col=2,xlab="Tips",main="Breaks at 33c",cex=1)
hist(d.tips[,3],breaks=c(0:40)/4,col=2,xlab="Tips",main="Breaks at 25c",cex=1)
hist(d.tips[,3],breaks=c(0:50)/5,col=2,xlab="Tips",main="Breaks at 20c",cex=1)
hist(d.tips[,3],breaks=c(0:100)/10,col=2,xlab="Tips",main="Breaks at 10c",cex=1)
#HISTOGRAMS OF TIP RATE
par(mfrow=c(1,3))
hist(tiprate,xlab="Tip Rate",breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,
  0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0),col=2,
  main="Tip Rate")
hist(tiprate,xlab="Tip Rate",30,xlim=c(0,0.35),col=2,main="Tip Rate (<0.35)")
hist(log(tiprate),20,xlab="log(Tip Rate)",col=2,main="log(Tip Rate)")

# OUTLIERS
d.tips[tiprate>0.35,]

par(mfrow=c(1,1))
hist(tiprate,breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8),xlim=c(0,0.8),xlab="Tip Rate",col=2)
hist(tiprate,breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8),xlim=c(0,0.35),xlab="Tip Rate",col=2)
x<-hist(tiprate,breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8),xlim=c(0,0.35),xlab="Tip Rate",plot=F)
x$counts
[1]  1 26 82 96 29  7  1  0  1  0  0  0  0  0  1  0

par(mfrow=c(2,1))
hist(tiprate[d.tips[,5]==0],breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8),xlim=c(0,0.35),xlab="Tip Rate")
title("Non-Smoker")
x<-hist(tiprate[d.tips[,5]==0],breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8),xlim=c(0,0.35),xlab="Tip Rate",plot=F)
x$counts
[1]  0  9 59 60 20  3  0  0  0  0  0  0  0  0  0  0
hist(tiprate[d.tips[,5]==1],breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8),xlim=c(0,0.35),xlab="Tip Rate")
title("Smoker")
x<-hist(tiprate[d.tips[,5]==1],breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8),xlim=c(0,0.35),xlab="Tip Rate",plot=F)
x$counts
[1]  1 17 23 36  9  4  1  0  1  0  0  0  0  0  1  0

#SCATTERPLOTS
par(mfrow=c(1,1))
par(pty="s")
plot(d.tips[,2],d.tips[,3],pch=16,xlab="Total Bill",ylab="Total Tip",xlim=c(0,55),ylim=c(1,10))
cor(d.tips[,2],d.tips[,3])
title("Correlation=0.68")
par(mfrow=c(1,2), pty="s")
plot(d.tips[,2],d.tips[,3],pch=16,xlab="Total Bill",ylab="Total Tip",xlim=c(0,55),ylim=c(1,10))
d.tips.lsfit<-lsfit(d.tips[,2],d.tips[,3])
abline(d.tips.lsfit$coef)
plot(d.tips[,2],d.tips.lsfit$residuals,xlab="Total Bill",ylab="Residuals")
plot(d.tips[,2],d.tips[,3],pch=16,xlab="Total Bill",ylab="Total Tip",xlim=c(0,55),ylim=c(1,10))
d.tips.lsfit<-lsfit(d.tips[,2],d.tips[,3],intercept=F)
abline(d.tips.lsfit$coef)
plot(d.tips[,2],d.tips.lsfit$residuals,xlab="Total Bill",ylab="Residuals")
par(mfrow=c(1,2), pty="s")
plot(d.tips[d.tips[,4]==0,2],d.tips[d.tips[,4]==0,3],
  pch=16,xlab="Total Bill",ylab="Total Tip",xlim=c(0,55),ylim=c(1,10))
title("Males")
cor(d.tips[d.tips[,4]==0,2],d.tips[d.tips[,4]==0,3])
text(5,9,"r=0.67")
plot(d.tips[d.tips[,4]==1,2],d.tips[d.tips[,4]==1,3],
  pch=16,xlab="Total Bill",ylab="Total Tip",xlim=c(0,55),ylim=c(1,10))
title("Females")
cor(d.tips[d.tips[,4]==1,2],d.tips[d.tips[,4]==1,3])
text(5,9,"r=0.68")
	
par(mfrow=c(2,2), pty="s",lty=1)
plot(d.tips[d.tips[,4]==0&d.tips[,5]==0,2],d.tips[d.tips[,4]==0&d.tips[,5]==0,3],
  pch=16,xlab="Total Bill",ylab="Total Tip",xlim=c(0,55),ylim=c(1,10))
title("Male Non-smokers")
cor(d.tips[d.tips[,4]==0&d.tips[,5]==0,2],d.tips[d.tips[,4]==0&d.tips[,5]==0,3])
[1] 0.8185412
text(5,9,"r=0.82")
d.tips.coef<-lsfit(d.tips[d.tips[,4]==0&d.tips[,5]==0,2],d.tips[d.tips[,4]==0&d.tips[,5]==0,3])$coef
abline(d.tips.coef)
d.tips.coef
Intercept         X
 0.3481945 0.1397188
d.tips.residss<-mean(lsfit(d.tips[d.tips[,4]==0&d.tips[,5]==0,2],d.tips[d.tips[,4]==0&d.tips[,5]==0,3])$residuals^2)
d.tips.residss
[1] 0.7246293
par(lty=2)
d.tips.coef<-lsfit(d.tips[d.tips[,4]==0&d.tips[,5]==0,2],d.tips[d.tips[,4]==0&d.tips[,5]==0,3],intercept=F)$coef
abline(d.tips.coef)
d.tips.coef
      X 
 0.1544732

par(lty=1)
plot(d.tips[d.tips[,4]==1&d.tips[,5]==0,2],d.tips[d.tips[,4]==1&d.tips[,5]==0,3],
  pch=16,xlab="Total Bill",ylab="Total Tip",xlim=c(0,55),ylim=c(1,10))
title("Female Non-smokers")
cor(d.tips[d.tips[,4]==1&d.tips[,5]==0,2],d.tips[d.tips[,4]==1&d.tips[,5]==0,3])
[1] 0.8280662
text(5,9,"r=0.83")
d.tips.coef<-lsfit(d.tips[d.tips[,4]==1&d.tips[,5]==0,2],d.tips[d.tips[,4]==1&d.tips[,5]==0,3])$coef
abline(d.tips.coef)
d.tips.coef
Intercept         X 
 0.4517193 0.1282395
d.tips.residss<-mean(lsfit(d.tips[d.tips[,4]==1&d.tips[,5]==0,2],d.tips[d.tips[,4]==1&d.tips[,5]==0,3])$residuals^2)
d.tips.residss
[1] 0.3928085

par(lty=2)
d.tips.coef<-lsfit(d.tips[d.tips[,4]==1&d.tips[,5]==0,2],d.tips[d.tips[,4]==1&d.tips[,5]==0,3],intercept=F)$coef
abline(d.tips.coef)
d.tips.coef
     X 
 0.149767

par(lty=1)
plot(d.tips[d.tips[,4]==0&d.tips[,5]==1,2],d.tips[d.tips[,4]==0&d.tips[,5]==1,3],
  pch=16,xlab="Total Bill",ylab="Total Tip",xlim=c(0,55),ylim=c(1,10))
title("Male Smokers")
cor(d.tips[d.tips[,4]==0&d.tips[,5]==1,2],d.tips[d.tips[,4]==0&d.tips[,5]==1,3])
[1] 0.4820766
text(5,9,"r=0.48")
d.tips.coef<-lsfit(d.tips[d.tips[,4]==0&d.tips[,5]==1,2],d.tips[d.tips[,4]==0&d.tips[,5]==1,3])$coef
abline(d.tips.coef)
d.tips.coef
Intercept          X 
  1.425279 0.07296046
d.tips.residss<-mean(lsfit(d.tips[d.tips[,4]==0&d.tips[,5]==1,2],d.tips[d.tips[,4]==0&d.tips[,5]==1,3])$residuals^2)
d.tips.residss
[1] 1.698591

par(lty=2)
d.tips.coef<-lsfit(d.tips[d.tips[,4]==0&d.tips[,5]==1,2],d.tips[d.tips[,4]==0&d.tips[,5]==1,3],intercept=F)$coef
abline(d.tips.coef)
d.tips.coef
       X 
 0.1265028

par(lty=1)	
plot(d.tips[d.tips[,4]==1&d.tips[,5]==1,2],d.tips[d.tips[,4]==1&d.tips[,5]==1,3],
  pch=16,xlab="Total Bill",ylab="Total Tip",xlim=c(0,55),ylim=c(1,10))
title("Female Smokers")
cor(d.tips[d.tips[,4]==1&d.tips[,5]==1,2],d.tips[d.tips[,4]==1&d.tips[,5]==1,3])
[1] 0.5157922
text(5,9,"r=0.52")
d.tips.coef<-lsfit(d.tips[d.tips[,4]==1&d.tips[,5]==1,2],d.tips[d.tips[,4]==1&d.tips[,5]==1,3])$coef
abline(d.tips.coef)
d.tips.coef
Intercept          X 
  1.700568 0.06847009
d.tips.residss<-mean(lsfit(d.tips[d.tips[,4]==1&d.tips[,5]==1,2],d.tips[d.tips[,4]==1&d.tips[,5]==1,3])$residuals^2)
d.tips.residss
[1] 1.059174
par(lty=2)
d.tips.coef<-lsfit(d.tips[d.tips[,4]==1&d.tips[,5]==1,2],d.tips[d.tips[,4]==1&d.tips[,5]==1,3],intercept=F)$coef
abline(d.tips.coef)
d.tips.coef
      X 
 0.14394
	
cor(d.tips[d.tips[,5]==0,2],d.tips[d.tips[,5]==0,3]) #non-smoker
[1] 0.8221828
cor(d.tips[d.tips[,5]==1,2],d.tips[d.tips[,5]==1,3]) #smokers
[1] 0.4882179

par(mfrow=c(2,2),pty="c")
plot(d.tips[,6],d.tips[,2]/d.tips[,3],ylim=c(0,30),xlim=c(3,6),pch=16,xlab="Day",
  ylab="Tip Rate")
plot(d.tips[,7],d.tips[,2]/d.tips[,3],ylim=c(0,30),xlim=c(0,1),pch=16,
  xlab="Day/Even",ylab="Tip Rate")
plot(d.tips[,8],d.tips[,2]/d.tips[,3],ylim=c(0,30),xlim=c(1,6),pch=16,
  xlab="Day/Even",ylab="Tip Rate")

par(mfrow=c(1,1),pty="s")
plot(d.tips[d.tips[,4]==0,2],d.tips[d.tips[,4]==0,3],
  pch=16,xlab="Total Bill",ylab="Total Tip",xlim=c(0,55),ylim=c(1,10))
points(d.tips[d.tips[,4]==1,2],d.tips[d.tips[,4]==1,3],pch=3)
legend(5,10,pch=c(16,3),legend=c("Males","Females"))

#REGRESSION
thurs<-rep(0,244)
for (i in 1:244)
  if (d.tips[i,6] == 3)
    thurs[i]<-1
fri<-rep(0,244)
for (i in 1:244)
  if (d.tips[i,6] == 4)
    fri[i]<-1
sat<-rep(0,244)
for (i in 1:244)
  if (d.tips[i,6] == 5)
    sat[i]<-1
d.tips.full<-cbind(d.tips,thurs,fri,sat)
dimnames(d.tips.full)[[2]][10]<-"Thurs"
dimnames(d.tips.full)[[2]][11]<-"Fri"
dimnames(d.tips.full)[[2]][12]<-"Sat"
tips.fit1<-glm(d.tips.full[,9]~d.tips.full[,c(4,5,7,8,10,11,12)])
summary(tips.fit1)
glm(d.tips.full[,9]~d.tips.full[,8])
glm(d.tips.full[,9]~d.tips.full[,8]+d.tips.full[,12])

tips.fit2<-glm(d.tips.full[d.tips.full[,9]<0.35,9]~
  d.tips.full[d.tips.full[,9]<0.35,c(4,5,7,8,10,11,12)])
summary(tips.fit2)
glm(d.tips.full[d.tips.full[,9]<0.35,9]~d.tips.full[d.tips.full[,9]<0.35,8])

par(mfrow=c(1,2))
plot(d.tips.full[,8],d.tips.full[,9],xlab="Size of Party",ylab="Tip Rate",
  pch=16)
abline(c(0.215,-0.0096))

par(mfrow=c(1,1))
plot(d.tips.full[d.tips.full[,9]<0.35,8],d.tips.full[d.tips.full[,9]<0.35,9],
xlab="Size of Party",ylab="Tip Rate",pch=16)
abline(c(0.1844,-0.0092))
par(lty=2)
abline(c(0.1757,-0.0071))
par(lty=1)
legend(4.5,0.30,lty=c(1,2),legend=c("All","No extremes"))

tips.fit3<-glm(log(d.tips.full[,9])~d.tips.full[,c(4,5,7,8,10,11,12)])
summary(tips.fit3)

tips.fit4<-glm(d.tips.full[d.tips.full[,9]<0.35,9]~
  d.tips.full[d.tips.full[,9]<0.35,4]*d.tips.full[d.tips.full[,9]<0.35,5]+
  d.tips.full[d.tips.full[,9]<0.35,8])
summary(tips.fit4)

# MOSAIC PLOTS
source("mosaic-label.R")# load mosaic plot code into R

y<-table(d.tips[,6],d.tips[,4])
dimnames(y)[[1]]<-c("Thurs","Fri","Sat","Sun")
dimnames(y)[[2]]<-c("Male","Female")
mosaic.plot(y)

y<-table(d.tips[,5],d.tips[,4])
dimnames(y)[[1]]<-c("No Smoke","Smoke")
dimnames(y)[[2]]<-c("Male","Female")
mosaic.plot(y)

y<-table(d.tips[,6], d.tips[,7])
dimnames(y)[[1]]<-c("Thurs","Fri","Sat","Sun")
dimnames(y)[[2]]<-c("Day","Evening")
mosaic.plot(y)

y<-table(d.tips[,8],d.tips[,6],d.tips[,4])
dimnames(y)[[2]]<-c("Thurs","Fri","Sat","Sun")
dimnames(y)[[3]]<-c("Male","Female")
dimnames(y)[[1]]<-c("1","2","3","4","5","6")
mosaic.plot(y)

