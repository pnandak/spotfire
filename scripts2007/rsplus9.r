pdf(file="c:\\allwork\\rsplus\\chap1figs.pdf")
timber<-source("c:\\allwork\\rsplus\\chap9timber.dat")$value
#
x<-c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.2,1.4,1.6,1.8)
slippage<-rep(x,8)
loads<-as.vector(t(timber))
specimen<-rep(1:8,rep(15,8))
#
timber.dat<-data.frame(specimen,slippage,as.vector(t(timber)))
#
attach(timber.dat)
#
library(nlme)
#random intercept model
timber.lme<-lme(loads~slippage,random=~1|specimen,data=timber.dat,method="ML")
#
#random intercept and slope model
timber.lme1<-lme(loads~slippage,random=~slippage|specimen,data=timber.dat,method="ML")
anova(timber.lme,timber.lme1)
summary(timber.lme1)
#
predictions<-matrix(predict(timber.lme1),ncol=15,byrow=T)
predictions
par(mfrow=c(1,2))
matplot(x,t(timber),type="l",col=1,xlab="Slippage",ylab="Load",lty=1,ylim=c(0,25))
title("(a)")
matplot(x,t(predictions),type="l",col=1,xlab="Slippage",ylab="Load",lty=1,ylim=c(0,25))
title("(b)")
#
#
timber.lme2<-lme(loads~slippage+I(slippage*slippage),
random=~slippage|specimen,data=timber.dat,method="ML")
anova(timber.lme1,timber.lme2)
#
predictions<-matrix(predict(timber.lme2),ncol=15,byrow=T)
predictions
par(mfrow=c(1,2))
matplot(x,t(timber),type="l",col=1,xlab="Slippage",ylab="Load",lty=1,ylim=c(0,25))
title("(a)")
matplot(x,t(predictions),type="l",col=1,xlab="Slippage",ylab="Load",lty=1,ylim=c(0,25))
title("(b)")
#
#
plasma<-source("c:\\allwork\\rsplus\\chap9plasma.dat")$value 
# 
# 
par(mfrow=c(1,2)) 
matplot(matrix(c(0.0,0.5,1.0,1.5,2.0,3.0,4.0,5.0),ncol=1), 
t(plasma[1:13,]),type="l",col=1,lty=1, 
xlab="Time (hours after oral glucose challenge)",ylab="Plasma inorganic phosphate",ylim=c(1,7)) 
title("Control") 
matplot(matrix(c(0.0,0.5,1.0,1.5,2.0,3.0,4.0,5.0),ncol=1), 
t(plasma[14:33,]),type="l",col=1,lty=1, 
xlab="Time (hours after glucose challenge)",ylab="Plasma inorganic phosphate",ylim=c(1,7)) 
title("Obese") 
# 
# 
pairs(plasma[1:13,]) 
pairs(plasma[14:33,]) 
# 
# 
# 
group<-rep(c(0,1),c(104,160)) 
# 
time<-c(0.0,0.5,1.0,1.5,2.0,3.0,4.0,5.0) 
time<-rep(time,33) 
# 
subject<-rep(1:33,rep(8,33)) 
plasma.dat<-cbind(subject,time,group,as.vector(t(plasma))) 
dimnames(plasma.dat)<-list(NULL,c("Subject","Time","Group","Plasma")) 
# 
plasma.df<-data.frame(plasma.dat) 
plasma.df$Group<-factor(plasma.df$Group,levels=c(0,1),labels=c("Control","Obese")) 
# 
attach(plasma.df) 
# 
plasma.lme1<-lme(Plasma~Time+I(Time*Time)+Group,random=~Time|Subject, 
data=plasma.df,method="ML") 
summary(plasma.lme1) 
# 
#multiple regression model-independence model
summary(lm(Plasma~Time+I(Time*Time)+Group,data=plasma.df)) 
# 
# 
predictions<-matrix(predict(plasma.lme1),ncol=8,byrow=T) 
par(mfrow=c(1,2)) 
matplot(matrix(c(0.0,0.5,1,1.5,2,3,4,5),ncol=1), 
t(predictions[1:13,]),type="l",lty=1,col=1, 
xlab="Time (hours after glucose challenge)",ylab="Plasma inorganic phosphate",ylim=c(1,7)) 
title("Control") 
matplot(matrix(c(0.0,0.5,1,1.5,2,3,4,5),ncol=1), 
t(predictions[14:33,]),type="l",lty=1,col=1, 
xlab="Time (hours after glucose challenge)",ylab="Plasma inorganic phosphate",ylim=c(1,7)) 
title("Obese") 
# 
plasma.lme2<-lme(Plasma~Time*Group+I(Time*Time),random=~Time|Subject, 
data=plasma.df,method="ML") 
anova(plasma.lme1,plasma.lme2) 
# 
summary(plasma.lme2) 
#
predictions<-matrix(predict(plasma.lme2),ncol=8,byrow=T) 
par(mfrow=c(1,2)) 
matplot(matrix(c(0.0,0.5,1,1.5,2,3,4,5),ncol=1), 
t(predictions[1:13,]),type="l",lty=1,col=1, 
xlab="Time (hours after glucose challenge)",ylab="Plasma inorganic phosphate",ylim=c(1,7)) 
title("Control") 
matplot(matrix(c(0.0,0.5,1,1.5,2,3,4,5),ncol=1), 
t(predictions[14:33,]),type="l",lty=1,col=1, 
xlab="Time (hours after glucose challenge)",ylab="Plasma inorganic phosphate",ylim=c(1,7)) 
title("Obese") 

# 
res.int<-random.effects(plasma.lme2)[,1] 
res.int 
res.slope<-random.effects(plasma.lme2)[,2] 
par(mfrow=c(1,3)) 
qqnorm(res.int,ylab="Estimated random intercepts",main="Random intercepts") 
qqnorm(res.slope,ylab="Estimated random slopes",main="Random slopes") 
resids<-resid(plasma.lme2) 
qqnorm(resids,ylab="Estimated residuals",main="Residuals") 
# 
# 
btb.data<-source("c:\\allwork\\rsplus\\chap9btb.dat")$value 
# 
attach(btb.data) 
par(mfrow=c(2,1)) 
boxplot(btb.data[Treatment=="TAU",4],btb.data[Treatment=="TAU",5],btb.data[Treatment=="TAU",6], 
btb.data[Treatment=="TAU",7],btb.data[Treatment=="TAU",8],names=c("BDIpre","BDI2m","BDI4m","BDI6m", 
"BDI8m"),ylab="BDI",xlab="Visit",col=1) 
title("TAU") 
boxplot(btb.data[Treatment=="BtheB",4],btb.data[Treatment=="BtheB",5],btb.data[Treatment=="BtheB",6], 
btb.data[Treatment=="BtheB",7],btb.data[Treatment=="BtheB",8],names=c("BDIpre","BDI2m","BDI4m","BDI6m", 
"BDI8m"),ylab="BDI",xlab="Visit",col=1) 
title("BtheB") 
# 
# 
# 
# 
n<-length(btb.data[,1]) 
# 
BDI<-as.vector(t(btb.data[,c(5,6,7,8)])) 
# 
treat<-rep(btb.data[,3],rep(4,n)) 
subject<-rep(1:n,rep(4,n)) 
preBDI<-rep(btb.data[,4],rep(4,n)) 
drug<-rep(btb.data[,1],rep(4,n)) 
length<-rep(btb.data[,2],rep(4,n)) 
time<-rep(c(2,4,6,8),n) 
# 
# 
btb.bdi<-data.frame(subject,treat,drug,length,preBDI,time,BDI) 
# 
attach(btb.bdi) 
# 
# 
btbbdi.fit1<-lme(BDI~preBDI+time+treat+drug+length,method="ML", 
random=~1|subject,data=btb.bdi,na.action=na.omit) 
# 
btbbdi.fit2<-lme(BDI~newpre+occasn+newtreat,method="ML", 
random=~time|subject,data=btb.bdi,na.action=na.omit) 
anova(btbbdi.fit2,btbbdi.fit1) 
# 
summary(btbbdi.fit1) 
# 





