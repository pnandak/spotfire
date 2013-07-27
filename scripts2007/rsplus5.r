airline.dist<-source("c:\\allwork\\rsplus\\chap5airdist.dat")$value
#
airline.mds<-cmdscale(as.matrix(airline.dist),k=9,eig=T)
airline.mds$eig
#
sum(abs(airline.mds$eig[1:2]))/sum(abs(airline.mds$eig))
sum(airline.mds$eig[1:2]^2)/sum(airline.mds$eig^2)
#
par(pty="s")
plot(-airline.mds$points[,1],airline.mds$points[,2],type="n",xlab="Coordinate 1",ylab="Coordinate 2",
xlim=c(-2000,1500),ylim=c(-2000,1500))
text(-airline.mds$points[,1],airline.mds$points[,2],labels=row.names(airline.dist))
#
#
sex<-matrix(c(21,21,14,13,8,8,9,6,8,2,2,3,4,10,10),ncol=5,byrow=TRUE)
#
ncol<-5
nrow<-3
n<-sum(sex)
rtot<-apply(sex,1,sum)
ctot<-apply(sex,2,sum)
#
xrtot<-cbind(rtot,rtot,rtot,rtot,rtot)
xctot<-rbind(ctot,ctot,ctot)
#
xrtot<-sex/xrtot
xctot<-sex/xctot
#
rdot<-rtot/n
cdot<-ctot/n
dcols<-matrix(0,ncol,ncol)
for(i in 1:ncol){
	 for(j in 1:ncol){d<-0
	      for(k in 1:nrow) d<-d+(xctot[k,i]-xctot[k,j])^2/rdot[k]
	      dcols[i,j]<-sqrt(d)}}
	#
drows<-matrix(0,nrow,nrow)
for(i in 1:nrow){
	 for(j in 1:nrow){d<-0
	      for(k in 1:ncol) d<-d+(xrtot[i,k]-xrtot[j,k])^2/cdot[k]
	      drows[i,j]<-sqrt(d)}}

#
#
r1<-cmdscale(dcols,eig=TRUE)
r1$points
r1$eig
c1<-cmdscale(drows,eig=TRUE)
c1$points
c1$eig
xrtot
par(pty="s")
plot(r1$points,xlim=range(r1$points[,1],c1$points[,1]),ylim=range(r1$points[,1],c1$points[,1]),type="n",
xlab="Coordinate 1",ylab="Coordinate 2",lwd=2)
text(r1$points,labels=c("AG1","AG2","AG3","AG4","AG5"),lwd=2)
text(c1$points,labels=c("nobf","bfns","bfs"),lwd=4)
#
abline(h=0,lty=2)
abline(v=0,lty=2)
#
#
#
skulls<-source("c:\\allwork\\rsplus\\chap5skulls.dat")$value
#
#
#
#Mahalanobis distances 
#
#
labs<-rep(1:5,rep(30,5))
#
centres<-matrix(0,5,4)
S<-matrix(0,4,4)
#
for(i in 1:5) {
	           centres[i,]<-apply(skulls[labs==i,-1],2,mean)
	          S<-S+29*var(skulls[,-1])
}
#
S<-S/145
#
mahal<-matrix(0,5,5)
#
for(i in 1:5) {
	mahal[i,]<-mahalanobis(centres,centres[i,],S)
}
#
par(pty="s")
coords<-cmdscale(mahal)
xlim<-c(-1.5,1.5)
plot(coords,xlab="C1",ylab="C2",type="n",xlim=xlim,ylim=xlim,lwd=2)
text(coords,labels=c("c4000BC","c3300BC","c1850BC","c200BC","cAD150"),lwd=3)
#
#


