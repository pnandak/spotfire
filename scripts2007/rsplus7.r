Tibet<-source("c:\\allwork\\rsplus\\chap7tibetskull.dat")$value
#
attach(Tibet)
#
m1<-apply(Tibet[Type==1,-6],2,mean)
m2<-apply(Tibet[Type==2,-6],2,mean)
l1<-length(Type[Type==1])
l2<-length(Type[Type==2])
x1<-Tibet[Type==1,-6]
x2<-Tibet[Type==2,-6]
S123<-((l1-1)*var(x1)+(l2-1)*var(x2))/(l1+l2-2)
T2<-t(m1-m2)%*%solve(S123)%*%(m1-m2)
#
Fstat<-(l1+l2-5-1)*T2/(l1+l2-2)*5
pvalue<-1-pf(Fstat,5,26)
#
Fstat
pvalue
#
m1<-apply(Tibet[Type==1,-6],2,mean)
m2<-apply(Tibet[Type==2,-6],2,mean)
l1<-length(Type[Type==1])
l2<-length(Type[Type==2])
x1<-Tibet[Type==1,-6]
x2<-Tibet[Type==2,-6]
S123<-((l1-1)*var(x1)+(l2-1)*var(x2))/(l1+l2-2)
a<-solve(S123)%*%(m1-m2)
z12<-(m1%*%a+m2%*%a)/2
z1<-m1%*%a
z2<-m2%*%a
z12
z1
z2
#
a
z12
#

#
library(MASS)
dis<-lda(Type~Length+Breadth+Height+Fheight+Fbreadth,data=Tibet,prior=c(0.5,0.5))
#
#
newdata<-rbind(c(171,140.5,127.0,69.5,137.0),c(179.0,132.0,140.0,72.0,138.5))
colnames(newdata)<-colnames(Tibet)
#
newdata<-data.frame(newdata)
predict(dis,newdata=newdata)
#
#
group<-predict(dis,method="plug-in")$class
#
table(group,Type)
#
#
#
#
skulls<-source("c:\\allwork\\rsplus\\chap5skulls.dat")$value
#
attach(skulls)
skulls.manova<-manova(cbind(MB,BH,BL,NH)~EPOCH)
summary(skulls.manova,test="Pillai")
summary(skulls.manova,test="Wilks")
summary(skulls.manova,test="Hotelling")
summary(skulls.manova,test="Roy")
#
chisplot(residuals(skulls.manova))
#
#
#
dsfs1<-c(0.13,-0.04,-0.15,0.08)%*%t(skulls[,-1])
dsfs2<-c(0.04,0.21,-0.068,-0.08)%*%t(skulls[,-1])
m1<-c(mean(dsfs1[1:30]),mean(dsfs1[31:60]),mean(dsfs1[61:90]),mean(dsfs1[91:120]),mean(dsfs1[121:150]))
m2<-c(mean(dsfs2[1:30]),mean(dsfs2[31:60]),mean(dsfs2[61:90]),mean(dsfs2[91:120]),mean(dsfs2[121:150]))
plot(m1,m2,type="n",xlab="CV1",ylab="CV2",xlim=c(0.5,3))
text(m1,m2,labels=c("c4000BC","c3300BC","c1850BC","c200BC","cAD150"))
   






