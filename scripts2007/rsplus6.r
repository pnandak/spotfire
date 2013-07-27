life<-source("c:\\allwork\\rsplus\\chap4lifeexp.dat")$value
#
#
par(mfrow=c(1,3))
plclust(hclust(dist(life),method="single"),labels=row.names(life),ylab="Distance")
title("(a) Single linkage")
plclust(hclust(dist(life),method="complete"),labels=row.names(life),ylab="Distance")
title("(b) Complete linkage")
plclust(hclust(dist(life),method="average"),labels=row.names(life),ylab="Distance")
title("(c) Average linkage")
#
four<-cutree(hclust(dist(life),method="complete"),h=21)
#
#
country.clus<-lapply(1:5,function(nc) row.names(life)[four==nc])
country.mean<-lapply(1:5,function(nc) apply(life[four==nc,],2,mean))
country.mean
country.clus
#
dev.off()
#
pairs(life,panel=function(x,y) text(x,y,four))
#
#
pottery.data<-source("c:\\allwork\\rsplus\\chap6pottery.data")$value
#
rge<-apply(pottery.data,2,max)-apply(pottery.data,2,min)
pottery.dat<-sweep(pottery.data,2,rge,FUN="/")
#
n<-length(pottery.dat[,1])
wss1<-(n-1)*sum(apply(pottery.dat,2,var))
wss<-numeric(0)
for(i in 2:6) {
	   W<-sum(kmeans(pottery.dat,i)$withinss)
	   wss<-c(wss,W)
}
#
wss<-c(wss1,wss)
plot(1:6,wss,type="l",xlab="Number of groups",ylab="Within groups sum of squares",lwd=2)
#
pottery.kmean<-kmeans(pottery.dat,3)
pottery.kmean
lapply(1:3,function(nc) apply(pottery.data[pottery.kmean$cluster==nc,],2,mean))
#
#
kiln<-c(rep(1,21),rep(2,12),rep(3,2),rep(4,5),rep(5,5))
#
table(kiln,pottery.kmean$cluster)
#
planet.dat<-source("c:\\allwork\\rsplus\\chap6planet.dat")$value

attach(planet.dat)
#
library(mclust)
planet.clus<-Mclust(planet.dat)
#
plot(planet.clus,planet.dat)
#enter 1
#
plot(planet.clus,planet.dat)
#enter 2
#
#enter 0
#
planet.clus$mu  




