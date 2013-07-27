d.music<-read.csv("music-plusnew-sub-full.csv",row.names=1)
apply(d.music[,-c(1,2)],2,mean)
apply(d.music[,-c(1,2)],2,sd)
d.music.std<-cbind(d.music[,c(1,2)],apply(d.music[,-c(1,2)],2,f.std.data))

# Summary statistics
apply(d.music[,-c(1,2)],2,mean)
apply(d.music[,-c(1,2)],2,sd)

apply(d.music[d.music[,1]=="Abba",-c(1,2)],2,mean)
apply(d.music[d.music[,1]=="Beatles",-c(1,2)],2,mean)
apply(d.music[d.music[,1]=="Eels",-c(1,2)],2,mean)

apply(d.music[d.music[,1]=="Beethoven",-c(1,2)],2,mean)
apply(d.music[d.music[,1]=="Mozart",-c(1,2)],2,mean)
apply(d.music[d.music[,1]=="Vivaldi",-c(1,2)],2,mean)

apply(d.music[d.music[,1]=="Enya",-c(1,2)],2,mean)

# Plots
library(lattice)
d.music.df<-data.frame(Artist=factor(rep(d.music[,1],5)),
  y=as.vector(as.matrix(d.music[,3:7])),
  meas=factor(rep(1:5, rep(62,5)), labels=names(d.music[,-c(1,2)])))
postscript("music-dotplot.ps",width=8.0,height=8.0,horizontal=FALSE,
  paper="special",family="URWHelvetica")
par(pty="s",mar=c(2,1,1,1))
plt.bg<-trellis.par.get("background")
plt.bg$col<-"grey90"
trellis.par.set("background",plt.bg)
stripplot(Artist~y|meas, data=d.music.df, scales=list(x="free"),
  strip=function(...) strip.default(style=1,...), 
  panel=function(x,y){panel.grid(h=-1,v=5,col="white")
    panel.stripplot(x,y,col=1,pch=16)},
  xlab="",pch=16,col=1,layout=c(3,2),aspect=1,
  as.table=T)
dev.off()

# Hierarchical clustering
music.dist<-dist(d.music[,-c(1:2)])
music.dist<-dist(d.music.std[,-c(1:2)])
music.hc1<-hclust(music.dist,method="ward")
music.hc2<-hclust(music.dist,method="single")
music.hc3<-hclust(music.dist,method="complete")
postscript("music-hclust.ps",width=5.0,height=10.0,horizontal=FALSE,
  paper="special",family="URWHelvetica")
par(mfrow=c(3,1),mar=c(1,2,2,2))
plot(music.hc1,main="Ward",xlab=" ")
text(music.hc1)
plot(music.hc2,main="Single",ylab=" ")
text(music.hc2)
plot(music.hc3,main="Complete",ylab=" ")
text(music.hc3)
dev.off()

cl.12<-cutree(music.hc1,2)
cl.22<-cutree(music.hc2,2)
cl.32<-cutree(music.hc3,2)

cl.13<-cutree(music.hc1,3)
cl.23<-cutree(music.hc2,3)
cl.33<-cutree(music.hc3,3)

cl.14<-cutree(music.hc1,4)
cl.24<-cutree(music.hc2,4)
cl.34<-cutree(music.hc3,4)

cl.15<-cutree(music.hc1,5)
cl.25<-cutree(music.hc2,5)
cl.35<-cutree(music.hc3,5)

cl.16<-cutree(music.hc1,6)
cl.26<-cutree(music.hc2,6)
cl.36<-cutree(music.hc3,6)

cl.17<-cutree(music.hc1,7)
cl.27<-cutree(music.hc2,7)
cl.37<-cutree(music.hc3,7)

cl.18<-cutree(music.hc1,8)
cl.28<-cutree(music.hc2,8)
cl.38<-cutree(music.hc3,8)

cl.19<-cutree(music.hc1,9)
cl.29<-cutree(music.hc2,9)
cl.39<-cutree(music.hc3,9)

cl.110<-cutree(music.hc1,10)
cl.210<-cutree(music.hc2,10)
cl.310<-cutree(music.hc3,10)

cl.111<-cutree(music.hc1,11)
cl.211<-cutree(music.hc2,11)
cl.311<-cutree(music.hc3,11)

cl.112<-cutree(music.hc1,12)
cl.212<-cutree(music.hc2,12)
cl.312<-cutree(music.hc3,12)

cl.113<-cutree(music.hc1,13)
cl.213<-cutree(music.hc2,13)
cl.313<-cutree(music.hc3,13)

cl.114<-cutree(music.hc1,14)
cl.214<-cutree(music.hc2,14)
cl.314<-cutree(music.hc3,14)

table(cl.12,cl.22)
table(cl.12,cl.32)
table(cl.32,cl.22)
table(cl.13,cl.23)
table(cl.13,cl.33)
table(cl.33,cl.23)
table(cl.14,cl.24)
table(cl.14,cl.34)
table(cl.34,cl.24)
table(cl.15,cl.25)
table(cl.15,cl.35)
table(cl.35,cl.25)
table(cl.16,cl.26)
table(cl.16,cl.36)
table(cl.36,cl.26)
table(cl.17,cl.27)
table(cl.17,cl.37)
table(cl.37,cl.27)
table(cl.18,cl.28)
table(cl.18,cl.38)
table(cl.38,cl.28)
table(cl.19,cl.29)
table(cl.19,cl.39)
table(cl.39,cl.29)
table(cl.110,cl.210)
table(cl.110,cl.310)
table(cl.310,cl.210)
table(cl.111,cl.211)
table(cl.111,cl.311)
table(cl.311,cl.211)
table(cl.112,cl.212)
table(cl.112,cl.312)
table(cl.312,cl.212)
table(cl.113,cl.213)
table(cl.113,cl.313)
table(cl.313,cl.213)
table(cl.114,cl.214)
table(cl.114,cl.314)
table(cl.314,cl.214)


for (i in 1:5) 
  cat(i,",",dimnames(d.music)[[1]][cl.15==i],"\n")
for (i in 1:5) 
  cat(i,",",dimnames(d.music)[[1]][music.km5$cluster==i],"\n")
dimnames(d.music)[[1]][music.km5$cluster==3&cl.15==2]


library(genegobitree)
library(Rggobi)
ggobi()
setup.gobidend(music.hc1,d.music)
color.click.dn(music.hc1,d.music)

library(mva)
music.km2<-kmeans(d.music.std[,-c(1,2)],2)
music.km3<-kmeans(d.music.std[,-c(1,2)],3)
music.km4<-kmeans(d.music.std[,-c(1,2)],4)
music.km5<-kmeans(d.music.std[,-c(1,2)],5)
music.km6<-kmeans(d.music.std[,-c(1,2)],6)
music.km7<-kmeans(d.music.std[,-c(1,2)],7)
music.km8<-kmeans(d.music.std[,-c(1,2)],8)
music.km9<-kmeans(d.music.std[,-c(1,2)],9)
music.km10<-kmeans(d.music.std[,-c(1,2)],10)
music.km11<-kmeans(d.music.std[,-c(1,2)],11)
music.km12<-kmeans(d.music.std[,-c(1,2)],12)
music.km13<-kmeans(d.music.std[,-c(1,2)],13)
music.km14<-kmeans(d.music.std[,-c(1,2)],14)

table(cl.15,music.km5$cluster)

d.music.clust1<-cbind(d.music,cl.12,cl.22,cl.32,cl.13,cl.23,cl.33,
  cl.14,cl.24,cl.34,cl.15,cl.25,cl.35,cl.16,cl.26,cl.36,cl.17,cl.27,cl.37,
  cl.18,cl.28,cl.38,
  music.km2$cluster,music.km3$cluster,music.km4$cluster,music.km5$cluster,
  music.km6$cluster,music.km7$cluster,music.km8$cluster)
dimnames(d.music.clust1)[[2]][8]<-"HC-W2"
dimnames(d.music.clust1)[[2]][9]<-"HC-S2"
dimnames(d.music.clust1)[[2]][10]<-"HC-C2"
dimnames(d.music.clust1)[[2]][11]<-"HC-W3"
dimnames(d.music.clust1)[[2]][12]<-"HC-S3"
dimnames(d.music.clust1)[[2]][13]<-"HC-C3"
dimnames(d.music.clust1)[[2]][14]<-"HC-W4"
dimnames(d.music.clust1)[[2]][15]<-"HC-S4"
dimnames(d.music.clust1)[[2]][16]<-"HC-C4"
dimnames(d.music.clust1)[[2]][17]<-"HC-W5"
dimnames(d.music.clust1)[[2]][18]<-"HC-S5"
dimnames(d.music.clust1)[[2]][19]<-"HC-C5"
dimnames(d.music.clust1)[[2]][20]<-"HC-W6"
dimnames(d.music.clust1)[[2]][21]<-"HC-S6"
dimnames(d.music.clust1)[[2]][22]<-"HC-C6"
dimnames(d.music.clust1)[[2]][23]<-"HC-W7"
dimnames(d.music.clust1)[[2]][24]<-"HC-S7"
dimnames(d.music.clust1)[[2]][25]<-"HC-C7"
dimnames(d.music.clust1)[[2]][26]<-"HC-W8"
dimnames(d.music.clust1)[[2]][27]<-"HC-S8"
dimnames(d.music.clust1)[[2]][28]<-"HC-C8"
dimnames(d.music.clust1)[[2]][29]<-"KM-2"
dimnames(d.music.clust1)[[2]][30]<-"KM-3"
dimnames(d.music.clust1)[[2]][31]<-"KM-4"
dimnames(d.music.clust1)[[2]][32]<-"KM-5"
dimnames(d.music.clust1)[[2]][33]<-"KM-6"
dimnames(d.music.clust1)[[2]][34]<-"KM-7"
dimnames(d.music.clust1)[[2]][35]<-"KM-8"
f.writeXML(d.music.clust1,"music-clust1.xml",data.num=1)

for (i in 1:6) 
  cat(i,",",dimnames(d.music)[[1]][music.km6$cluster==i],"\n")

library(som)
music.som<-som(d.music.std[,-c(1:2)],6,6,rlen=100)
music.som<-som(d.music.std[,-c(1:2)],6,6,rlen=200)
music.som<-som(d.music.std[,-c(1:2)],6,6,rlen=300)
music.som<-som(d.music.std[,-c(1:2)],6,6,rlen=400)
music.som<-som(d.music.std[,-c(1:2)],6,6,rlen=1000)
music.som<-som(d.music.std[,-c(1:2)],6,6,neigh="bubble",rlen=100)
music.som<-som(d.music.std[,-c(1:2)],6,6,neigh="bubble",rlen=200)
music.som<-som(d.music.std[,-c(1:2)],6,6,neigh="bubble",rlen=300)
music.som<-som(d.music.std[,-c(1:2)],6,6,neigh="bubble",rlen=400)
music.som<-som(d.music.std[,-c(1:2)],6,6,neigh="bubble",rlen=1000)
music.som<-som(d.music.std[,-c(1:2)],6,6,init="random",neigh="bubble",rlen=100)
music.som<-som(d.music.std[,-c(1:2)],6,6,init="random",neigh="bubble",
  rlen=1000)

xmx<-jitter(music.som$visual$x,factor=3)
xmy<-jitter(music.som$visual$y,factor=3)
par(mfrow=c(1,1),pty="s")
plot(xmx,xmy,type="n",pch=16,xlab="x",ylab="y",main="SOM Map",
  xlim=c(-0.5,6),ylim=c(-0.5,6))
text(xmx,xmy,dimnames(d.music)[[1]])
dimnames(music.som$code)<-list(NULL,names(d.music[,-c(1,2)]))
d.music.clust<-cbind(d.music.std,xmx,xmy)
dimnames(d.music.clust)[[2]][8]<-"Map 1"
dimnames(d.music.clust)[[2]][9]<-"Map 2"
d.music.grid<-cbind(rep("0",36),rep("0",36),music.som$code,
  music.som$code.sum[,1:2])
dimnames(d.music.grid)[[2]][1]<-"Artist"
dimnames(d.music.grid)[[2]][2]<-"Type"
dimnames(d.music.grid)[[2]][8]<-"Map 1"
dimnames(d.music.grid)[[2]][9]<-"Map 2"
d.music.clust<-rbind(d.music.grid,d.music.clust)
f.writeXML(d.music.clust,
  "music-SOM.xml",data.num=2,dat1.id<-c(1:dim(d.music.clust)[1]),
  dat2=cbind(c(1:60),c(1:60)),
  dat2.source=x33.l[,1],
  dat2.destination=x33.l[,2],
  dat2.name="SOM",dat2.id=paste(rep("l",60),c(1:60)))

# Favorite model
music.som<-som(d.music[,-c(1:2)],6,6,neigh="bubble",rlen=1000)
music.som<-som(d.music.std[,-c(1:2)],6,6,neigh="bubble",rlen=1000)
xmx<-jitter(music.som$visual$x,factor=3)
xmy<-jitter(music.som$visual$y,factor=3)
postscript("music-som.ps",width=8.0,height=8.0,horizontal=FALSE,
  paper="special",family="URWHelvetica")
par(mfrow=c(1,1),pty="s")
plot(xmx,xmy,type="n",pch=16,xlab="x",ylab="y",main="SOM Map",
  xlim=c(-0.5,6),ylim=c(-0.5,6))
text(xmx,xmy,dimnames(d.music)[[1]])
dev.off()

# Setting up the net lines
n.nodes<-6
x33.l<-NULL
for (i in 1:n.nodes) {
  for (j in 1:n.nodes) {
    if (j<n.nodes) x33.l<-rbind(x33.l,c((i-1)*n.nodes+j,(i-1)*n.nodes+j+1))
    if (i<n.nodes) x33.l<-rbind(x33.l,c((i-1)*n.nodes+j,i*n.nodes+j))
}}

# Model-based
library(mclust)
postscript("music-mc1.ps",width=8.0,height=8.0,horizontal=FALSE,
  paper="special",family="URWHelvetica")
music.mc<-EMclust(d.music[,-c(1:2)],1:36,c("EII","VII","EEE","EEV","VVV"))
par(pty="m",mfrow=c(2,1))
plot(music.mc)
legend(1,-6300,col=c(1:5),lty=c(1:5),
  legend=c("1 EII","2 VII","3 EEE","4 EEV","5 VVV"),bg="white")
music.mc<-EMclust(d.music[,-c(1:2)],1:15,c("EEE","EEV"))
plot(music.mc)
abline(h=seq(-5610,-5400,by=10),col="gray80")
legend(1,-5550,col=c(1:2),lty=c(1:2),
  legend=c("1 EEE","2 EEV"),bg="white")
box()
dev.off()
smry<-summary(music.mc,d.music[,-c(1:2)])
cl<-smry$classification
cl.mat<-matrix(0,62,2)
cl.mat[cl==1,1]<-1
cl.mat[cl==2,2]<-1
prm<-mstepEEV(d.music[,-c(1:2)],cl.mat)
d.music[cl==1,1:2]
d.music[cl==2,1:2]

for (i in 1:14) 
  cat(i,",",dimnames(d.music)[[1]][cl==i],"\n")

smry<-summary(music.mc,d.music[,-c(1:2)])
smry
t(smry$mu)
smry$sigma

mc.clust.dist<-dist(t(smry$mu))
mc.clust.mean<-hclust(mc.clust.dist,method="single")
plot(mc.clust.mean)

music.mc<-EMclust(d.music[,-c(1:2)],11,"EEV")

music.mc2<-EMclust(d.music[,-c(1:2)],2,"EEE")
summary(music.mc2,d.music[,-c(1:2)])
music.mc3<-EMclust(d.music[,-c(1:2)],7,"EEE")
summary(music.mc3,d.music[,-c(1:2)])
mccl<-summary(music.mc3,d.music[,-c(1:2)])$classification
for (i in 1:7) 
  cat(i,",",dimnames(d.music)[[1]][mccl==i],"\n")

# Generate the ellipses in 5D
vc<-smry$sigma[,,1]
evc<-eigen(vc)
vc2<-(evc$vectors)%*%diag(sqrt(evc$values))%*%t(evc$vectors)
y1<-f.gen.sphere(500,5)
y1<-y1%*%vc2
y1[,1]<-y1[,1]+smry$mu[1,1]
y1[,2]<-y1[,2]+smry$mu[2,1]
y1[,3]<-y1[,3]+smry$mu[3,1]
y1[,4]<-y1[,4]+smry$mu[4,1]
y1[,5]<-y1[,5]+smry$mu[5,1]
vc<-smry$sigma[,,2]
evc<-eigen(vc)
vc2<-(evc$vectors)%*%diag(sqrt(evc$values))%*%t(evc$vectors)
y2<-f.gen.sphere(500,5)
y2<-y2%*%vc2
y2[,1]<-y2[,1]+smry$mu[1,2]
y2[,2]<-y2[,2]+smry$mu[2,2]
y2[,3]<-y2[,3]+smry$mu[3,2]
y2[,4]<-y2[,4]+smry$mu[4,2]
y2[,5]<-y2[,5]+smry$mu[5,2]

y<-cbind(rep(0,1000),c(rep(1,500),rep(2,500)),
  rbind(y1,y2))
y[,1]<-factor(y[,1])
y[,2]<-factor(y[,2])
dimnames(y)<-list(NULL,names(d.music))

d.music.mc<-rbind(d.music,y)

f.writeXML(d.music.mc,"music-mclust.xml",data.num=1)


# Set up a full data set
kmcl6<-music.km6$cluster
dimnames(music.som$code)<-list(NULL,names(d.music[,-c(1,2)]))
d.music.clust<-cbind(d.music,cl6,kmcl6,xmx,xmy)
dimnames(d.music.clust)[[2]][8]<-"HC-W6"
dimnames(d.music.clust)[[2]][9]<-"KM-6"
dimnames(d.music.clust)[[2]][10]<-"Map 1"
dimnames(d.music.clust)[[2]][11]<-"Map 2"
d.music.grid<-cbind(rep("0",36),rep("0",36),music.som$code,rep(0,36),rep(0,36),
  music.som$code.sum[,1:2])
dimnames(d.music.grid)[[2]][1]<-"Artist"
dimnames(d.music.grid)[[2]][2]<-"Type"
dimnames(d.music.grid)[[2]][8]<-"HC-W6"
dimnames(d.music.grid)[[2]][9]<-"KM-6"
dimnames(d.music.grid)[[2]][10]<-"Map 1"
dimnames(d.music.grid)[[2]][11]<-"Map 2"
d.music.clust<-rbind(d.music.grid,d.music.clust)
f.writeXML(d.music.clust,
  "SOM-music.xml",data.num=2,dat1.id<-c(1:dim(d.music.clust)[1]),
  dat2=cbind(c(1:60),c(1:60)),
  dat2.source=x33.l[,1],
  dat2.destination=x33.l[,2],
  dat2.name="SOM",dat2.id=paste(rep("l",60),c(1:60)))


# Utility functions
f.gen.sphere<-function(n=100,p=5) {
 x<-matrix(rnorm(n*p),ncol=p)
 xnew<-t(apply(x,1,norm.vec))
 xnew
 }

norm.vec<-function(x) {
 x<-x/norm(x)
 x
 }

norm<-function(x) { sqrt(sum(x^2))}

# Out put data for ggvis
edges<-NULL
k<-1
for (i in 1:61)
  for (j in (i+1):62) {
    edges<-rbind(edges,c(i,j,music.dist[k]))
    k<-k+1
  }
f.writeXML(d.music.std,"music-MDS.xml",data.num=2,dat1.id<-c(1:62),
  dat1.name="Music",
  dat2=cbind(1:1891,edges[,3]),
  dat2.source=edges[,1],dat2.destination=edges[,2],
  dat2.name="dist",dat2.id=c(1:1891))

# Comparison of clusters
for (i in 1:14) 
  cat(i,",",dimnames(d.music)[[1]][music.km14$cluster==i],"\n")

# Check smaller number of clusters
for (i in 1:9) 
  cat(i,",",dimnames(d.music)[[1]][music.km9$cluster==i],"\n")

for (i in 1:10) 
  cat(i,",",dimnames(d.music)[[1]][music.km10$cluster==i],"\n")

for (i in 1:11) 
  cat(i,",",dimnames(d.music)[[1]][music.km11$cluster==i],"\n")

options(digits=2)
for (i in 1:10) 
  cat(i,",",apply(d.music[music.km10$cluster==i,-c(1,2)],2,mean),"\n")

km10.mn<-NULL
for (i in 1:10) 
  km10.mn<-rbind(km10.mn,apply(d.music[music.km10$cluster==i,-c(1,2)],2,mean))


km10.mn.std<-apply(km10.mn,2,f.std.data)
range(km10.mn.std)
postscript("music-means.ps",width=5.0,height=8.0,horizontal=FALSE,
  paper="special",family="URWHelvetica")
plot(c(1,5),c(-2.5,2.8),type="n",axes=F,xlab="Variables",ylab="")
rect(0.8,-2.8,5.2,3.1,col="gray80")
abline(v=c(1:5),col="white")
abline(h=seq(-2.5,2.5,by=0.5),col="white")
clrs<-c(1:7,9:11)
for (i in 1:10) {
  lines(c(1:5),km10.mn.std[i,],lty=i,col=clrs[i])
  points(c(1:5),km10.mn.std[i,],pch=i,col=clrs[i])
}
axis(side=1,at=c(1:5),labels=names(d.music[,-c(1,2)]))
legend(1,-0.8,lty=c(1:10),pch=c(1:10),col=clrs,
  legend=c(1:10),bg="gray80")
title("Cluster means")
box()
dev.off()

library(lattice)
d.music.df<-data.frame(cluster=factor(rep(music.km10$cluster,5)),
  y=as.vector(as.matrix(d.music.std[,3:7])),
  meas=factor(rep(1:5, rep(62,5)), labels=names(d.music[,-c(1,2)])))
postscript("music-means2.ps",width=3.0,height=10.0,horizontal=FALSE,
  paper="special",family="URWHelvetica")
plt.bg<-trellis.par.get("background")
plt.bg$col<-"grey90"
trellis.par.set("background",plt.bg)
xyplot(y~meas|cluster,data=d.music.df,xlab="",ylab="",
  box.ratio=1,layout=c(1,10),col=1,pch=16,
  panel=function(x,y){panel.grid(h=-1,v=5,col="white")
  panel.stripplot(x,y,col=1,pch=16)})
dev.off()
